00001  IDENTIFICATION DIVISION.                                         02/18/05
00002  PROGRAM-ID. GAS1UPD.                                             GAS1UPD 
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV005
00004  AUTHOR. N ELBAZ.                                                 GAS1UPD 
00005  DATE-WRITTEN.   05/07/87.                                        GAS1UPD 
00006  DATE-COMPILED.                                                   GAS1UPD 
00007      SKIP3                                                        GAS1UPD 
00008 ******************************************************************GAS1UPD 
00009 *   GAS1UPD         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *GAS1UPD 
00010 *                        BENEFIT AGGREGATE MAXIMUMS     -   GAS1 *GAS1UPD 
00011 *                                                                *GAS1UPD 
00012 *     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *GAS1UPD 
00013 *   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *GAS1UPD 
00014 *   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *GAS1UPD 
00015 *   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *GAS1UPD 
00016 *   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *GAS1UPD 
00017 *   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*GAS1UPD 
00018 *                                                                *GAS1UPD 
00019 *     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *GAS1UPD 
00020 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *GAS1UPD 
00021 *   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*GAS1UPD 
00022 *   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *GAS1UPD 
00023 *   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *GAS1UPD 
00024 *   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *GAS1UPD 
00025 *                                                                *GAS1UPD 
00026 *    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*GAS1UPD 
00027 *   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*GAS1UPD 
00028 *   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *GAS1UPD 
00029 *   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *GAS1UPD 
00030 *   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *GAS1UPD 
00031 *   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *GAS1UPD 
00032 *                                                                *GAS1UPD 
00033 *   FUNC CODE: GAS1                                              *GAS1UPD 
00034 *                                                                *GAS1UPD 
00035 *                          ********************************      *GAS1UPD 
00036 *                          *   THIS MAPSET IS SHARED BY   *      *GAS1UPD 
00037 *                          *   THE FOLLOWING MODULES:     *      *GAS1UPD 
00038 *                          *   1. GA1BPGM                 *      *GAS1UPD 
00039 *                          *   2. GA1CPGM                 *      *GAS1UPD 
00040 *                          *   3. GA1DPGM                 *      *GAS1UPD 
00041 *   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *GAS1UPD 
00042 *                          *   5. GASEDIT1                *      *GAS1UPD 
00043 *                          *   6. GACDEPGM                *      *GAS1UPD 
00044 *                          *   7. GK1BPGM                 *      *GAS1UPD 
00045 *                          *   8. GK1CPGM                 *      *GAS1UPD 
00046 *                          *   9. GK1DPGM                 *      *GAS1UPD 
00047 *                          *  10. GK1EPGM                 *      *GAS1UPD 
00048 *                          *  11. GAS1UPD                 *      *GAS1UPD 
00049 *                          *  12. GAS2UPD                 *      *GAS1UPD 
00050 *                          *  11. GAS3UPD                 *      *GAS1UPD 
00051 *                          *  11. GAS4UPD                 *      *GAS1UPD 
00052 *                          ********************************      *GAS1UPD 
00053 *                                                                *GAS1UPD 
00054 *   FILES:     GCPSWORK           GCGRPSPC                       *GAS1UPD 
00055 *              GCTABULR           GCSTABLR                       *GAS1UPD 
00056 *              GCCONTR            GCSPROVN                       *GAS1UPD 
00057 *                                                                *GAS1UPD 
00058 ******************************************************************GAS1UPD 
00059                                                                   GAS1UPD 
00060 /    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS1UPD 
00061 *    *-*         U P D A T E   H I S T O R Y         *-*          GAS1UPD 
00062 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS1UPD 
00063                                                                   GAS1UPD 
00064 *NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*GAS1UPD 
00065 *                                                                *GAS1UPD 
00066 *                                                                *GAS1UPD 
00067 * D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *GAS1UPD 
00068 * D200 06/07/87  NGE  SEPERATE CHANGE/DELETE FUNCTION TO         *GAS1UPD 
00069 *                     ANOTHER MODULE.                            *GAS1UPD 
00070 * N121 08/10/87  NGE  ADD NEW FIELD -DEFINITION-                 *GAS1UPD 
00071 * N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT                  *GAS1UPD 
00072 *                                                                *GAS1UPD 
00073 *  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *GAS1UPD 
00074 *                             4600- SECTION THAT CAUSED THE CDE  *GAS1UPD 
00075 *                             MODIFIED STATUS TO BE SET.         *GAS1UPD 
00076 *                                                                *GAS1UPD 
00077 * D143 02/01/88  DES  CDE/NON-CDE CHANGES                        *GAS1UPD 
00078 *                                                                *GAS1UPD 
00079 *  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *GAS1UPD 
00080 *                              TO 'A'.                           *GAS1UPD 
00081 *                                                                *GAS1UPD 
00082 *  R1218     06/02/88  NGE  1. CHANGE ACCUM LOGIC FOR MAPING     *GAS1UPD 
00083 *                                                                *GAS1UPD 
00084 * ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC TO FLAG  THE    *GAS1UPD 
00085 *                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *GAS1UPD 
00086 *                            TAB LOGIC TO FLAG ITS ACCUM WITH CDE*GAS1UPD 
00087 *                            WHEN THE INTERNL FLAGED CDE.        *GAS1UPD 
00088 *                                                                *GAS1UPD 
00089 * ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *GAS1UPD 
00090 *                        UPDATING CDE COUNTERS DEPENDING ON THE  *GAS1UPD 
00091 *                        INTRNL TAB RECORD NOT THE CDE STATUS    *GAS1UPD 
00092 *                        IN THE ACCUM RECORD ATTACHED.           *GAS1UPD 
00093 *                                                                *GAS1UPD 
00094 *D????  01/13/89 ENW   DARKENED THE BISCENDING IND FIELD.         GAS1UPD 
00095 *                                                                *GAS1UPD 
00096 * D200 05/18/89  NGE  ADD TWO NEW COND-BITS TMJ AND INF,         *GAS1UPD 
00097 *                     TEMPROMAND-JOINT AND INFERTILITY-COND.     *GAS1UPD 
00098 *                                                                *GAS1UPD 
00099 * 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GAS1UPD 
00100 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GAS1UPD 
00101 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GAS1UPD 
00102 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GAS1UPD 
00103 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GAS1UPD 
00104 *                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*GAS1UPD 
00105 *                       7. >>> CONVERT TO COBOL/2 <<<.           *GAS1UPD 
00106 *                                                                *GAS1UPD 
00107 * 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *GAS1UPD 
00108 *                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *GAS1UPD 
00109 *                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*GAS1UPD 
00110 *                                                                *GAS1UPD 
00111 * 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *GAS1UPD 
00112 *                                                                *GAS1UPD 
00113 * D12009 9/11/91  GDM  INCREASE GRP-SPEC-FAM-REL-LVL             *GAS1UPD 
00114 *                               CONTRACT-FAM-REL-LVL             *GAS1UPD 
00115 *                               BEN-PROV-FAM-REL-LVL             *GAS1UPD 
00116 *                             TO 2 POSITIONS                     *GAS1UPD 
00117 *                                                                *GAS1UPD 
00118 * P-034  09/17/91  ENW  FIXED PROGRAM ERROR LEFT OVER FROM THE   *GAS1UPD 
00119 *                       11154 ACCUM EXPANSION. MOVED HIGH VALUES *GAS1UPD 
00120 *                       TO THE LAST OCCURS AFTER A DELETE OF AN  *GAS1UPD 
00121 *                       INTERNAL TABULAR.                        *GAS1UPD 
00122 *                                                                *GAS1UPD 
00123 * 12262  02/28/92 TPM ADD NEW COND-BIT LIF (LIFE-THREATING)      *GAS1UPD 
00124 *                     COND-LIFE-THREAT-BIT                       *GAS1UPD 
00125 *                                                                *GAS1UPD 
00126 *  D303  02/03/97 DAU ADD FEAK INDICATOR                         *GAS1UPD 
00127 *                                                                *GAS1UPD 
00128 * 14726/ 10/27/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *GAS1UPD 
00129 * 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *GAS1UPD 
00130 *                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *GAS1UPD 
00131 *                                                                *GAS1UPD 
00132 *  D341  10/07/98 GDM HIDE TIME/DOLLAR FIELD FROM SCREEN         *GAS1UPD 
00133 *                                                                *GAS1UPD 
00134 *        07/07/00 GSP ADDED #IPGS LOGIC.                         *GAS1UPD 
00135 *                                                                *GAS1UPD 
00136 *D352  09/21/00  GDM  ADD ACCUM IDENTIFIER                       *GAS1UPD 
00137 *                                                                *GAS1UPD 
00138 *        11/29/00 GSP ADD LOGIC TO DISPLAY MESSAGE IF A 6TH      *GAS1UPD 
00139 *                     INTERNAL TABULAR IS ATTEMPTED TO BE        *GAS1UPD 
00140 *                     ADDED.                                     *GAS1UPD 
00141 *                                                                *GAS1UPD 
00142 *        01/12/01 GSP ADD LOGIC TO PREVENT INTERNAL TABULAR      *GAS1UPD 
00143 *                     COUNT FROM BEING INCREASED TO GREATER      *GAS1UPD 
00144 *                     THAN 5.                                    *GAS1UPD 
00145 *                                                                *GAS1UPD 
00146 *        11/16/01 AKK ADD 4 NEW BITS, 2 FOR EMER AND 2 FOR       *GAS1UPD 
00147 *                     SERIOUS MENTAL ILLNESS.                    *GAS1UPD 
00148 *                                                                *GAS1UPD 
00149 *D365B 06/03/02   JP  ADD COMBINATION APPLIED INDICATOR (CAPI)   *GAS1UPD 
00150 *                                                                *GAS1UPD 
00151 *  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *GAS1UPD 
00152 *                         DETERMINATION (SABD)                   *GAS1UPD 
00153 *                                                                *GAS1UPD 
00154 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GAS1UPD 
00155 *                                                                *GAS1UPD 
00156 *            06-13-03   DAF   CORRECTED PROGRAM SO THAT THE      *GAS1UPD 
00157 *                             TABULAR ID IS ON THE 'C6' RECORD   *GAS1UPD 
00158 *                             INSTEAD OF THE BENEFIT PROVISION ID*GAS1UPD 
00159 *                             AND THE TABULAR SLOT NUMBER IS ON  *GAS1UPD 
00160 *                             THE 'C6' RECORD INSTEAD OF THE     *GAS1UPD 
00161 *                             BENEFIT PROVISION SLOT NUMBER      *GAS1UPD 
00162 *                             USE COPYBOOK GCTIPGPC INSTEAD OF   *GAS1UPD 
00163 *                             GCTIPGTC                           *GAS1UPD 
00164 *                                                                *GAS1UPD 
00156 * P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *GAS1UPD 
00164 *                             INDICATORS                         *GAS1UPD 
00164 *                                                                *GAS1UPD 
00156 *            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *GAS1UPD 
00156 *            04-14-25   CJB   FIXED ISSUE WITH OCCURS ENTRIES    *GAS1UPD 
00165 ******************************************************************GAS1UPD 
00168 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                     *GAS1UPD 
00169 * - DEAD CODE ELIMINATION.                                       *GAS1UPD 
      *                                                                *        
      * P21595  09/19/16  HSB       CHANGES FOR GCPS NEW FIELDS,BENEFIT*        
      *                             TYPE CODE,TIER CODE,TIER LEVEL.    *        
SI0724* P56703  05/08/24  SI        RECOMPILE - PEAQ COPYBOOK EXPANSION*        
SI0724*                             COPY ABM, ACP, ACL, ADL, AOL,      *        
SI0724*                             GCCDRLEN                           *        
00170 ******************************************************************GAS1UPD 
00171                                                                   GAS1UPD 
00172      SKIP3                                                        GAS1UPD 
00173  ENVIRONMENT DIVISION.                                            GAS1UPD 
00174 /    D A T A   D I V I S I O N                                    GAS1UPD 
00175  DATA DIVISION.                                                   GAS1UPD 
00176  WORKING-STORAGE SECTION.                                         GAS1UPD 
00177  01  WS-BEGIN                    PIC X(24)  VALUE                 GAS1UPD 
00178      '***GAS1UPD WS BEGINS***'.                                   GAS1UPD 
00179                                                                   GAS1UPD 
00180 *    T I T L E   L I N E S                                        GAS1UPD 
00181  01  WS-TITLE-LINES.                                              GAS1UPD 
00182  COPY GCMHLINE.                                                   GAS1UPD 
00183 *****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                GAS1UPD 
00184 *      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        GAS1UPD 
00185 *    05  GROUP-SPECIFIC-ID-LINE.                                  GAS1UPD 
00186 *      10  FILLER                        PIC X(20)                GAS1UPD 
00187 *        VALUE 'GROUP SPECIFIC ID= '.                             GAS1UPD 
00188 *      10  FILLER                        PIC X(5) VALUE 'GRP= '.  GAS1UPD 
00189 *      10  GRP-SPEC-GROUP-NO             PIC X(6).                GAS1UPD 
00190 *      10  FILLER                        PIC X(6) VALUE ' SEC= '. GAS1UPD 
00191 *      10  GRP-SPEC-SECTION-NO           PIC X(4).                GAS1UPD 
00192 *      10  FILLER                        PIC X(5) VALUE ' FR= '.  GAS1UPD 
00193 *      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  GAS1UPD 
00194 *      10  FILLER                        PIC X(7) VALUE ' EFDT= '.GAS1UPD 
00195 *      10  GRP-SPEC-EFF-DATE             PIC X(6).                GAS1UPD 
00196 *    05  CONTRACT-TITLE-LINE             PIC X(42)                GAS1UPD 
00197 *      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         GAS1UPD 
00198 *    05  CONTRACT-ID-LINE.                                        GAS1UPD 
00199 *      10  FILLER                      PIC X(14)                  GAS1UPD 
00200 *        VALUE 'CONTRACT ID= '.                                   GAS1UPD 
00201 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS1UPD 
00202 *      10  CONTRACT-GROUP-NO           PIC X(6).                  GAS1UPD 
00203 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS1UPD 
00204 *      10  CONTRACT-SECTION-NO         PIC X(4).                  GAS1UPD 
00205 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS1UPD 
00206 *      10  CONTRACT-LOB                PIC X.                     GAS1UPD 
00207 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS1UPD 
00208 *      10  CONTRACT-PROV-CTL           PIC XX.                    GAS1UPD 
00209 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS1UPD 
00210 *      10  CONTRACT-FAM-REL-LVL        PIC XX.                    GAS1UPD 
00211 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS1UPD 
00212 *      10  CONTRACT-EFF-DATE               PIC X(6).              GAS1UPD 
00213 *    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                GAS1UPD 
00214 *      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        GAS1UPD 
00215 *    05  BENEFIT-PROVISION-ID-LINE.                               GAS1UPD 
00216 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS1UPD 
00217 *      10  BEN-PROV-GROUP-NO           PIC X(6).                  GAS1UPD 
00218 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS1UPD 
00219 *      10  BEN-PROV-SECTION-NO         PIC X(4).                  GAS1UPD 
00220 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS1UPD 
00221 *      10  BEN-PROV-LOB                PIC X.                     GAS1UPD 
00222 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS1UPD 
00223 *      10  BEN-PROV-PROV-CTL           PIC XX.                    GAS1UPD 
00224 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS1UPD 
00225 *      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    GAS1UPD 
00226 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS1UPD 
00227 *      10  BEN-PROV-EFF-DATE           PIC X(6).                  GAS1UPD 
00228 *      10  FILLER                      PIC X(8) VALUE ' BPVID= '. GAS1UPD 
00229 *      10  BEN-PROV-ID-NO              PIC X(6).                  GAS1UPD 
00230 *    05  ABM-TITLE-LINE                PIC X(26)                  GAS1UPD 
00231 *********VALUE 'BENEFIT AGGREGATE MAXIMUMS'.                      GAS1UPD 
00232 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GAS1UPD 
00233  01  FILLER                      PIC X(32)  VALUE                 GAS1UPD 
00234      '*** ALTERNATIVE WORKFILE KEY ***'.                          GAS1UPD 
00235  01  SAVE-WS-ALT-WORKFILE-KEYS.                                   GAS1UPD 
00236      05 FILLER                   PIC X(63) VALUE SPACES.          GAS1UPD 
00237                                                                   GAS1UPD 
00238  01  SAVE-RESTORE-KEY.                                            GAS1UPD 
00239      05 WS-SV-RESTO-KY           PIC X(63) VALUE SPACES.          GAS1UPD 
00240                                                                   GAS1UPD 
00241  01  WS-ALT-WORKFILE-KEYS.                                        GAS1UPD 
00242  COPY GCWRKKEY.                                                   GAS1UPD 
00243                                                                   GAS1UPD 
00244                                                                   GAS1UPD 
00245 *   D A T E   F O R M A T T I N G   C O M M A R E A               GAS1UPD 
00246  01  HGADATES-COMMAREA.                                           GAS1UPD 
00247  COPY HGCDAT01.                                                   GAS1UPD 
00248                                                                   GAS1UPD 
00249                                                                   GAS1UPD 
00250 *    W O R K F I E L D S ,   A N D   S W I T C H E S              GAS1UPD 
00251  01  WS-WORK-FIELDS.                                              GAS1UPD 
00252                                                                   GAS1UPD 
00253      05  WS-SPACES-ZEROS.                                         GAS1UPD 
00254        10  WS-SPACES-ZEROS-SPACES       PIC X(6)  VALUE SPACES.   GAS1UPD 
00255        10  WS-SPACES-ZEROS-ZEROS        PIC S9(7) COMP-3          GAS1UPD 
00256                                                   VALUE ZEROS.    GAS1UPD 
00257                                                                   GAS1UPD 
00258      05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    GAS1UPD 
00259                                                                   GAS1UPD 
00260      05  WS-TAB-PROV-COPY-SLOT          PIC S9(7) COMP-3.         GAS1UPD 
00261      05  WS-INTL-TAB-ID.                                          GAS1UPD 
00262        10  WS-INTL-TAB-TAB-ID           PIC X(6).                 GAS1UPD 
00263        10  WS-INTL-TAB-TAB-SLOT         PIC S9(7) COMP-3.         GAS1UPD 
00264      05  WS-SAVE-INTL-TAB.                                        GAS1UPD 
00265        10  WS-SAVE-INTL-TAB-ID          PIC X(6).                 GAS1UPD 
00266        10  WS-SAVE-INTL-TAB-SLOT        PIC S9(7) COMP-3.         GAS1UPD 
00267                                                                   GAS1UPD 
00268      05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  GAS1UPD 
00269                                                                   GAS1UPD 
00270      05  WS-CDE-REQUEST-CODES.                                    GAS1UPD 
00271          10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.GAS1UPD 
00272          10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.GAS1UPD 
00273          10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.GAS1UPD 
00274          10  WS-REQUEST-4900-CNTL-UPDATE    PIC X(4) VALUE '4900'.GAS1UPD 
00275                                                                   GAS1UPD 
00276      05  WS-INTRNL-TABS-TO-CHG-CNT          PIC S9   COMP-3.      GAS1UPD 
00277      05  WS-INT-TAB-CHANGE-INDICATOR        PIC XX   VALUE SPACE. GAS1UPD 
00278        88  WS-INT-DESCRP-CHG-TO-NON-PROD        VALUE 'PN'.       GAS1UPD 
00279        88  WS-INT-DESCRP-CHG-BACK-TO-PROD       VALUE 'NP'.       GAS1UPD 
00280        88  WS-INT-DESCRP-NOCHG-AT-PROD          VALUE '  '.       GAS1UPD 
00281        88  WS-INT-DESCRP-NOCHG-AT-NONPROD       VALUE 'NN'.       GAS1UPD 
00282                                                                   GAS1UPD 
00283 *    I N T E R N A L   T A B U L A R   P R O G R A M   N A M E    GAS1UPD 
00284  01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  GAS1UPD 
00285                                                                   GAS1UPD 
00286                                                                   GAS1UPD 
00287 ** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          GAS1UPD 
00288  01  WS-ENTRY                          PIC X(176).                GAS1UPD 
00289      SKIP3                                                        GAS1UPD 
00290 /   A T T R I B U T E S                                           GAS1UPD 
00291  COPY DFHBMSCA.                                                   GAS1UPD 
00292      02  DFHBMABF                PIC X VALUE '9'.                 GAS1UPD 
00293 /   A T T E N T I O N   I D E N T I F I E R S                     GAS1UPD 
00294  COPY DFHAID.                                                     GAS1UPD 
00295 /   R E C O R D   L E N G T H S                                   GAS1UPD 
00296                                                                   GAS1UPD 
00297  01  WS-RECORD-LENGTHS.                                           GAS1UPD 
00298 *   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   GAS1UPD 
00299     05 WS-COMMON-WORKAREA-LEN         PIC S9(4) COMP  VALUE +550. GAS1UPD 
00300     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. GAS1UPD 
00301 ******** ACCUM MAX VARIABLE AREA 176 X 44 = 7744 ***********      GAS1UPD 
00301 ******** ACCUM MAX VARIABLE AREA 176 X 175 = 30800 **********     GAS1UPD 
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +7744. GAS1UPD 
SI0724    05 WS-COPY-TABLE-LEN             PIC S9(4) COMP VALUE +30800. GAS1UPD 
00303     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00304     05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00305     05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00306     05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00307     05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00308     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00309 *                                                                 GAS1UPD 
00310     05 WS-WF-INTR-TAB-LEN             PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00311     05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00312     05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00313     05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   GAS1UPD 
00314     05 WS-NEW-OCCR-ON-WF              PIC X     VALUE SPACE.      GAS1UPD 
00315                                                                   GAS1UPD 
00316 ******************************************************************GAS1UPD 
00317 ** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **GAS1UPD 
00318 ******************************************************************GAS1UPD 
00319  01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  GAS1UPD 
00320  01  CURNT-OCURS-PKD             PIC 9(4).                        GAS1UPD 
00321  01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            GAS1UPD 
00322      05  FILLER                  PIC XX.                          GAS1UPD 
00323      05  CURNT-OCCURS-OUT        PIC XX.                          GAS1UPD 
00324                                                                   GAS1UPD 
00325  01  TOTAL-OCURS-UNK             PIC 9(5).                        GAS1UPD 
00326  01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            GAS1UPD 
00327      05  FILLER                  PIC XXX.                         GAS1UPD 
00328      05  TOTAL-OCCURS-OUT        PIC XX.                          GAS1UPD 
00329 /    G . C .   R E C O R D S   L E N G T H S                      GAS1UPD 
00330  01  WS-GC-RECORD-LENGTHS.                                        GAS1UPD 
00331      COPY GCCDRLEN.                                               GAS1UPD 
00332                                                                   GAS1UPD 
00333 /    A B E N D   A R E A                                          GAS1UPD 
00334  01  WS-01-ABEND-AREA.                                            GAS1UPD 
00335      05  FILLER                   PIC X(16)  VALUE                GAS1UPD 
00336          '** ABEND AREA **'.                                      GAS1UPD 
00337                                                                   GAS1UPD 
00338      05  WS-ABCODE-CODES-AND-MSG.                                 GAS1UPD 
00339          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GAS1UPD 
00340          10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. GAS1UPD 
00341          10  WS-ABCODE-1BC1             PIC X(04)  VALUE  '1BC1'. GAS1UPD 
00342          10  WS-ABCODE-1BC1-MSG         PIC X(79)  VALUE          GAS1UPD 
00343              '*** INVALID PARAMETER LENGTH FOUND ***              GAS1UPD 
00344 -            '                           '.                       GAS1UPD 
00345          10  WS-ABCODE-1BC2             PIC X(04)  VALUE  '1BC2'. GAS1UPD 
00346          10  WS-ABCODE-1BC2-MSG         PIC X(79)  VALUE          GAS1UPD 
00347              '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      GAS1UPD 
00348 -            '                           '.                       GAS1UPD 
00349          10  WS-ABCODE-1BC3             PIC X(04)  VALUE  '1BC3'. GAS1UPD 
00350          10  WS-ABCODE-1BC3-MSG         PIC X(79)  VALUE          GAS1UPD 
00351              '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        GAS1UPD 
00352 -            '                           '.                       GAS1UPD 
00353          10  WS-ABCODE-1BF1             PIC X(04)  VALUE  '1BF1'. GAS1UPD 
00354          10  WS-ABCODE-1BF1-MSG         PIC X(79)  VALUE          GAS1UPD 
00355              '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABUGAS1UPD 
00356 -            'LAR.  CONTACT SYSTEMS ***  '.                       GAS1UPD 
00357          10  WS-ABCODE-1BF2             PIC X(04)  VALUE  '1BF2'. GAS1UPD 
00358          10  WS-ABCODE-1BF2-MSG         PIC X(79)  VALUE          GAS1UPD 
00359              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS1UPD 
00360 -            ' CONTACT SYSTEMS ***       '.                       GAS1UPD 
00361          10  WS-ABCODE-1BF3             PIC X(04)  VALUE  '1BF3'. GAS1UPD 
00362          10  WS-ABCODE-1BF3-MSG         PIC X(79)  VALUE          GAS1UPD 
00363              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS1UPD 
00364 -            ' CONTACT SYSTEMS ***       '.                       GAS1UPD 
00365          10  WS-ABCODE-1BF4             PIC X(04)  VALUE  '1BF4'. GAS1UPD 
00366          10  WS-ABCODE-1BF4-MSG         PIC X(79)  VALUE          GAS1UPD 
00367              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS1UPD 
00368 -            'CT SYSTEMS ***             '.                       GAS1UPD 
00369          10  WS-ABCODE-1BF5             PIC X(04)  VALUE  '1BF5'. GAS1UPD 
00370          10  WS-ABCODE-1BF5-MSG         PIC X(79)  VALUE          GAS1UPD 
00371              'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFIGAS1UPD 
00372 -            'LE.  PLEASE CONTACT SYSTEMS'.                       GAS1UPD 
00373          10  WS-ABCODE-1BF6             PIC X(04)  VALUE  '1BF6'. GAS1UPD 
00374          10  WS-ABCODE-1BF6-MSG         PIC X(79)  VALUE          GAS1UPD 
00375              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFILGAS1UPD 
00376 -            'E.  PLEASE CONTACT SYSTEMS '.                       GAS1UPD 
00377          10  WS-ABCODE-1BF7             PIC X(04)  VALUE  '1BF7'. GAS1UPD 
00378          10  WS-ABCODE-1BF7-MSG         PIC X(79)  VALUE          GAS1UPD 
00379              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS1UPD 
00380 -            ' SYSTEMS ***               '.                       GAS1UPD 
00381          10  WS-ABCODE-1BF9             PIC X(04)  VALUE  '1BF9'. GAS1UPD 
00382          10  WS-ABCODE-1BF9-MSG         PIC X(79)  VALUE          GAS1UPD 
00383              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILEGAS1UPD 
00384 -            '.  PLEASE CONTACT SYSTEMS  '.                       GAS1UPD 
00385          10  WS-ABCODE-1BFA             PIC X(04)  VALUE  '1BFA'. GAS1UPD 
00386          10  WS-ABCODE-1BFA-MSG         PIC X(79)  VALUE          GAS1UPD 
00387              '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE GAS1UPD 
00388 -            'CONTACT SYSTEMS ***        '.                       GAS1UPD 
00389          10  WS-ABCODE-1BFB             PIC X(04)  VALUE  '1BFB'. GAS1UPD 
00390          10  WS-ABCODE-1BFB-MSG         PIC X(79)  VALUE          GAS1UPD 
00391              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS1UPD 
00392 -            ' SYSTEMS ***               '.                       GAS1UPD 
00393          10  WS-ABCODE-1BFC             PIC X(04)  VALUE  '1BFC'. GAS1UPD 
00394          10  WS-ABCODE-1BFC-MSG         PIC X(79)  VALUE          GAS1UPD 
00395              '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTACGAS1UPD 
00396 -            'T SYSTEMS ***              '.                       GAS1UPD 
00397          10  WS-ABCODE-1BFJ             PIC X(04)  VALUE  '1BFJ'. GAS1UPD 
00398          10  WS-ABCODE-1BFJ-MSG         PIC X(79)  VALUE          GAS1UPD 
00399              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS1UPD 
00400 -            ' SYSTEMS ***               '.                       GAS1UPD 
00401          10  WS-ABCODE-1BFK             PIC X(04)  VALUE  '1BFK'. GAS1UPD 
00402          10  WS-ABCODE-1BFK-MSG         PIC X(79)  VALUE          GAS1UPD 
00403              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS1UPD 
00404 -            ' SYSTEMS ***               '.                       GAS1UPD 
00405          10  WS-ABCODE-1BFL             PIC X(04)  VALUE  '1BFL'. GAS1UPD 
00406          10  WS-ABCODE-1BFL-MSG         PIC X(79)  VALUE          GAS1UPD 
00407              '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TOGAS1UPD 
00408 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS1UPD 
00409          10  WS-ABCODE-1BFM             PIC X(04)  VALUE  '1BFM'. GAS1UPD 
00410          10  WS-ABCODE-1BFM-MSG         PIC X(79)  VALUE          GAS1UPD 
00411              '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  GAS1UPD 
00412 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS1UPD 
00413          10  WS-ABCODE-1BFN             PIC X(04)  VALUE  '1BFN'. GAS1UPD 
00414          10  WS-ABCODE-1BFN-MSG         PIC X(79)  VALUE          GAS1UPD 
00415              '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO MGAS1UPD 
00416 -            'ENU.  CONTACT SYSTEMS ***  '.                       GAS1UPD 
00417          10  WS-ABCODE-1BFO             PIC X(04)  VALUE  '1BFO'. GAS1UPD 
00418          10  WS-ABCODE-1BFO-MSG         PIC X(79)  VALUE          GAS1UPD 
00419              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS1UPD 
00420 -            ' SYSTEMS ***               '.                       GAS1UPD 
00421          10  WS-ABCODE-1BFP             PIC X(04)  VALUE  '1BFP'. GAS1UPD 
00422          10  WS-ABCODE-1BFP-MSG         PIC X(79)  VALUE          GAS1UPD 
00423              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS1UPD 
00424 -            ' SYSTEMS ***               '.                       GAS1UPD 
00425          10  WS-ABCODE-1BFQ             PIC X(04)  VALUE  '1BFQ'. GAS1UPD 
00426          10  WS-ABCODE-1BFQ-MSG         PIC X(79)  VALUE          GAS1UPD 
00427              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS1UPD 
00428 -            'CT SYSTEMS ***             '.                       GAS1UPD 
00429          10  WS-ABCODE-1BFR             PIC X(04)  VALUE  '1BFR'. GAS1UPD 
00430          10  WS-ABCODE-1BFR-MSG         PIC X(79)  VALUE          GAS1UPD 
00431              '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACTGAS1UPD 
00432 -            ' SYSTEMS ***               '.                       GAS1UPD 
00433          10  WS-ABCODE-1BFS             PIC X(04)  VALUE  '1BFS'. GAS1UPD 
00434          10  WS-ABCODE-1BFS-MSG         PIC X(79)  VALUE          GAS1UPD 
00435              '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACTGAS1UPD 
00436 -            ' SYSTEMS ***               '.                       GAS1UPD 
00437          10  WS-ABCODE-1BFT             PIC X(04)  VALUE  '1BFT'. GAS1UPD 
00438          10  WS-ABCODE-1BFT-MSG         PIC X(79)  VALUE          GAS1UPD 
00439              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS1UPD 
00440 -            ' SYSTEMS ***               '.                       GAS1UPD 
00441          10  WS-ABCODE-1BFU             PIC X(04)  VALUE  '1BFU'. GAS1UPD 
00442          10  WS-ABCODE-1BFU-MSG         PIC X(79)  VALUE          GAS1UPD 
00443              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS1UPD 
00444 -            'CT SYSTEMS ***             '.                       GAS1UPD 
00445          10  WS-ABCODE-1BL1             PIC X(04)  VALUE  '1BL1'. GAS1UPD 
00446          10  WS-ABCODE-1BL1-MSG         PIC X(79)  VALUE          GAS1UPD 
00447              '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***GAS1UPD 
00448 -            '                           '.                       GAS1UPD 
00449          10  WS-ABCODE-1BL2             PIC X(04)  VALUE  '1BL2'. GAS1UPD 
00450          10  WS-ABCODE-1BL2-MSG         PIC X(79)  VALUE          GAS1UPD 
00451              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS1UPD 
00452 -            'MS ***                     '.                       GAS1UPD 
00453          10  WS-ABCODE-1BL3             PIC X(04)  VALUE  '1BL3'. GAS1UPD 
00454          10  WS-ABCODE-1BL3-MSG         PIC X(79)  VALUE          GAS1UPD 
00455              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS1UPD 
00456 -            'EMS ***                    '.                       GAS1UPD 
00457          10  WS-ABCODE-1BL4             PIC X(04)  VALUE  '1BL4'. GAS1UPD 
00458          10  WS-ABCODE-1BL4-MSG         PIC X(79)  VALUE          GAS1UPD 
00459              '*** THE OCCURS WE ARE TO DISPLAY HAS BEEN DELETED   GAS1UPD 
00460 -            '                           '.                       GAS1UPD 
00461          10  WS-ABCODE-1BLX             PIC X(04)  VALUE  '1BLX'. GAS1UPD 
00462          10  WS-ABCODE-1BLX-MSG         PIC X(79)  VALUE          GAS1UPD 
00463              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS1UPD 
00464 -            'MS ***                     '.                       GAS1UPD 
00465          10  WS-ABCODE-1BP1             PIC X(04)  VALUE  '1BP1'. GAS1UPD 
00466          10  WS-ABCODE-1BP1-MSG         PIC X(79)  VALUE          GAS1UPD 
00467              '????????????????????????????????????????????????????GAS1UPD 
00468 -            '???????????????????????????'.                       GAS1UPD 
00469                                                                   GAS1UPD 
00470 /*****************************************************************GAS1UPD 
00471 *    WT-01   M E S S A G E   T A B L E                            GAS1UPD 
00472 ******************************************************************GAS1UPD 
00473  01  WT-01-TABLE.                                                 GAS1UPD 
00474      05  FILLER                  PIC X(16) VALUE                  GAS1UPD 
00475          '* WT-01-TABLE  *'.                                      GAS1UPD 
00476  01  FILLER.                                                      GAS1UPD 
00477      05  WT-01-MESSAGE-VALUES.                                    GAS1UPD 
00478                                                                   GAS1UPD 
00479 *----------------------------------------------------------------*GAS1UPD 
00480          10  WT-01-ENTRY-001.                                     GAS1UPD 
00481              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00482              15  WT-01-MESSAGE-TEXT-001.                          GAS1UPD 
00483                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00484                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00485                  20  FILLER          PIC X(3)  VALUE  '001'.      GAS1UPD 
00486                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00487                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00488                      '#IBGR HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00489 -                    '                         '.                 GAS1UPD 
00490              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00491 *----------------------------------------------------------------*GAS1UPD 
00492          10  WT-01-ENTRY-002.                                     GAS1UPD 
00493              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00494              15  WT-01-MESSAGE-TEXT-002.                          GAS1UPD 
00495                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00496                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00497                  20  FILLER          PIC X(3)  VALUE  '002'.      GAS1UPD 
00498                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00499                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00500                      '#IPGN HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00501 -                    '                         '.                 GAS1UPD 
00502              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00503 *----------------------------------------------------------------*GAS1UPD 
00504          10  WT-01-ENTRY-003.                                     GAS1UPD 
00505              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00506              15  WT-01-MESSAGE-TEXT-003.                          GAS1UPD 
00507                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00508                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00509                  20  FILLER          PIC X(3)  VALUE  '003'.      GAS1UPD 
00510                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00511                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00512                      '#IPGT HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00513 -                    '                         '.                 GAS1UPD 
00514              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00515 *----------------------------------------------------------------*GAS1UPD 
00516          10  WT-01-ENTRY-004.                                     GAS1UPD 
00517              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00518              15  WT-01-MESSAGE-TEXT-004.                          GAS1UPD 
00519                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00520                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00521                  20  FILLER          PIC X(3)  VALUE  '004'.      GAS1UPD 
00522                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00523                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00524                      ' BAD EFFECTIVE DATE CONVERSION - BEFOR READIGAS1UPD 
00525 -                    'NG PRODUCTIN FILE        '.                 GAS1UPD 
00526              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00527 *----------------------------------------------------------------*GAS1UPD 
00528          10  WT-01-ENTRY-005.                                     GAS1UPD 
00529              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00530              15  WT-01-MESSAGE-TEXT-005.                          GAS1UPD 
00531                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00532                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00533                  20  FILLER          PIC X(3)  VALUE  '005'.      GAS1UPD 
00534                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00535                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00536                      '******************* F U T U R E   U S E ****GAS1UPD 
00537 -                    '*************************'.                 GAS1UPD 
00538              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00539 *----------------------------------------------------------------*GAS1UPD 
00540          10  WT-01-ENTRY-006.                                     GAS1UPD 
00541              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00542              15  WT-01-MESSAGE-TEXT-006.                          GAS1UPD 
00543                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00544                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00545                  20  FILLER          PIC X(3)  VALUE  '006'.      GAS1UPD 
00546                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00547                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00548                      'DELETE OPTION MUST BE \
00549 -                    'VALID                    '.                 GAS1UPD 
00550              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00551 *----------------------------------------------------------------*GAS1UPD 
00552          10  WT-01-ENTRY-007.                                     GAS1UPD 
00553              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00554              15  WT-01-MESSAGE-TEXT-007.                          GAS1UPD 
00555                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00556                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00557                  20  FILLER          PIC X(3)  VALUE  '007'.      GAS1UPD 
00558                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00559                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00560                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSGAS1UPD 
00561 -                    ' PF4/PF16 TO CONTINUE    '.                 GAS1UPD 
00562              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00563 *----------------------------------------------------------------*GAS1UPD 
00564          10  WT-01-ENTRY-008.                                     GAS1UPD 
00565              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00566              15  WT-01-MESSAGE-TEXT-008.                          GAS1UPD 
00567                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00568                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00569                  20  FILLER          PIC X(3)  VALUE  '008'.      GAS1UPD 
00570                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00571                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00572                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGAS1UPD 
00573 -                    'GH-LIGHTED ELEMENTS      '.                 GAS1UPD 
00574              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00575 *----------------------------------------------------------------*GAS1UPD 
00576          10  WT-01-ENTRY-009.                                     GAS1UPD 
00577              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00578              15  WT-01-MESSAGE-TEXT-009.                          GAS1UPD 
00579                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00580                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00581                  20  FILLER          PIC X(3)  VALUE  '009'.      GAS1UPD 
00582                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00583                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00584                      'INVALID PFKEY SELECTION                     GAS1UPD 
00585 -                    '                         '.                 GAS1UPD 
00586              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00587 *----------------------------------------------------------------*GAS1UPD 
00588          10  WT-01-ENTRY-010.                                     GAS1UPD 
00589              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00590              15  WT-01-MESSAGE-TEXT-010.                          GAS1UPD 
00591                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00592                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00593                  20  FILLER          PIC X(3)  VALUE  '010'.      GAS1UPD 
00594                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00595                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00596                      'INVALID REQUEST.  THAT PF KEY HAS NO MEANINGGAS1UPD 
00597 -                    ' TO THIS PROGRAM         '.                 GAS1UPD 
00598              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00599 *----------------------------------------------------------------*GAS1UPD 
00600          10  WT-01-ENTRY-011.                                     GAS1UPD 
00601              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00602              15  WT-01-MESSAGE-TEXT-011.                          GAS1UPD 
00603                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00604                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00605                  20  FILLER          PIC X(3)  VALUE  '011'.      GAS1UPD 
00606                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00607                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00608                      'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT GAS1UPD 
00609 -                    '                         '.                 GAS1UPD 
00610              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00611 *----------------------------------------------------------------*GAS1UPD 
00612          10  WT-01-ENTRY-012.                                     GAS1UPD 
00613              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00614              15  WT-01-MESSAGE-TEXT-012.                          GAS1UPD 
00615                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00616                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00617                  20  FILLER          PIC X(3)  VALUE  '012'.      GAS1UPD 
00618                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00619                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00620                      'NO ENTRIES TO DISPLAY                       GAS1UPD 
00621 -                    '                         '.                 GAS1UPD 
00622              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00623 *----------------------------------------------------------------*GAS1UPD 
00624          10  WT-01-ENTRY-013.                                     GAS1UPD 
00625              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00626              15  WT-01-MESSAGE-TEXT-013.                          GAS1UPD 
00627                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00628                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00629                  20  FILLER          PIC X(3)  VALUE  '013'.      GAS1UPD 
00630                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00631                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00632                      'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HIGAS1UPD 
00633 -                    'T ENTER FOR ERR MSG      '.                 GAS1UPD 
00634              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00635 *----------------------------------------------------------------*GAS1UPD 
00636          10  WT-01-ENTRY-014.                                     GAS1UPD 
00637              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00638              15  WT-01-MESSAGE-TEXT-014.                          GAS1UPD 
00639                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00640                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00641                  20  FILLER          PIC X(3)  VALUE  '014'.      GAS1UPD 
00642                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00643                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00644                      'PROCESSING FROM THE TOP OF THE LIST         GAS1UPD 
00645 -                    '                         '.                 GAS1UPD 
00646              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00647 *----------------------------------------------------------------*GAS1UPD 
00648          10  WT-01-ENTRY-015.                                     GAS1UPD 
00649              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00650              15  WT-01-MESSAGE-TEXT-015.                          GAS1UPD 
00651                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00652                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00653                  20  FILLER          PIC X(3)  VALUE  '015'.      GAS1UPD 
00654                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00655                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00656                      'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDEGAS1UPD 
00657 -                    'D                        '.                 GAS1UPD 
00658              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00659 *----------------------------------------------------------------*GAS1UPD 
00660          10  WT-01-ENTRY-016.                                     GAS1UPD 
00661              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00662              15  WT-01-MESSAGE-TEXT-016.                          GAS1UPD 
00663                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00664                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00665                  20  FILLER          PIC X(3)  VALUE  '016'.      GAS1UPD 
00666                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00667                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00668                      'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUMGAS1UPD 
00669 -                    'BER OF OCCURANCES        '.                 GAS1UPD 
00670              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00671 *----------------------------------------------------------------*GAS1UPD 
00672          10  WT-01-ENTRY-017.                                     GAS1UPD 
00673              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00674              15  WT-01-MESSAGE-TEXT-017.                          GAS1UPD 
00675                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00676                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00677                  20  FILLER          PIC X(3)  VALUE  '017'.      GAS1UPD 
00678                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00679                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00680                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS1UPD 
00681 -                    'T BE CHANGED             '.                 GAS1UPD 
00682              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00683 *----------------------------------------------------------------*GAS1UPD 
00684          10  WT-01-ENTRY-018.                                     GAS1UPD 
00685              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00686              15  WT-01-MESSAGE-TEXT-018.                          GAS1UPD 
00687                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00688                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00689                  20  FILLER          PIC X(3)  VALUE  '018'.      GAS1UPD 
00690                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00691                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00692                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS1UPD 
00693 -                    'T BE MAPPED              '.                 GAS1UPD 
00694              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00695 *----------------------------------------------------------------*GAS1UPD 
00696          10  WT-01-ENTRY-019.                                     GAS1UPD 
00697              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00698              15  WT-01-MESSAGE-TEXT-019.                          GAS1UPD 
00699                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00700                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00701                  20  FILLER          PIC X(3)  VALUE  '019'.      GAS1UPD 
00702                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00703                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00704                      'THERE ARE NO MORE ENTRIES TO DISPLAY        GAS1UPD 
00705 -                    '                         '.                 GAS1UPD 
00706              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00707 *----------------------------------------------------------------*GAS1UPD 
00708          10  WT-01-ENTRY-020.                                     GAS1UPD 
00709              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00710              15  WT-01-MESSAGE-TEXT-020.                          GAS1UPD 
00711                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00712                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00713                  20  FILLER          PIC X(3)  VALUE  '020'.      GAS1UPD 
00714                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00715                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00716                      'THIS IS THE FIRST ON THE TABLE              GAS1UPD 
00717 -                    '                         '.                 GAS1UPD 
00718              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00719 *----------------------------------------------------------------*GAS1UPD 
00720          10  WT-01-ENTRY-021.                                     GAS1UPD 
00721              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00722              15  WT-01-MESSAGE-TEXT-021.                          GAS1UPD 
00723                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00724                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00725                  20  FILLER          PIC X(3)  VALUE  '021'.      GAS1UPD 
00726                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00727                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00728                      'THIS IS THE LAST ON THE TABLE               GAS1UPD 
00729 -                    '                         '.                 GAS1UPD 
00730              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00731 *----------------------------------------------------------------*GAS1UPD 
00732          10  WT-01-ENTRY-022.                                     GAS1UPD 
00733              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00734              15  WT-01-MESSAGE-TEXT-022.                          GAS1UPD 
00735                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00736                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00737                  20  FILLER          PIC X(3)  VALUE  '022'.      GAS1UPD 
00738                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00739                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00740                      'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  GAS1UPD 
00741 -                    '                         '.                 GAS1UPD 
00742              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00743 *----------------------------------------------------------------*GAS1UPD 
00744          10  WT-01-ENTRY-023.                                     GAS1UPD 
00745              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00746              15  WT-01-MESSAGE-TEXT-023.                          GAS1UPD 
00747                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00748                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00749                  20  FILLER          PIC X(3)  VALUE  '023'.      GAS1UPD 
00750                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00751                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00752                      '#IDGD HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00753 -                    '                         '.                 GAS1UPD 
00754              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00755 *----------------------------------------------------------------*GAS1UPD 
00756          10  WT-01-ENTRY-024.                                     GAS1UPD 
00757              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00758              15  WT-01-MESSAGE-TEXT-024.                          GAS1UPD 
00759                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00760                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00761                  20  FILLER          PIC X(3)  VALUE  '024'.      GAS1UPD 
00762                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00763                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00764                      '#IPGP HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00765 -                    '                         '.                 GAS1UPD 
00766              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00767 *----------------------------------------------------------------*GAS1UPD 
00768          10  WT-01-ENTRY-025.                                     GAS1UPD 
00769              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00770              15  WT-01-MESSAGE-TEXT-003.                          GAS1UPD 
00771                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00772                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00773                  20  FILLER          PIC X(3)  VALUE  '025'.      GAS1UPD 
00774                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00775                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00776                      '#IPGS HAS BEEN SUCCESSFULLY MAPPED          GAS1UPD 
00777 -                    '                         '.                 GAS1UPD 
00778              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00779 *----------------------------------------------------------------*GAS1UPD 
00780          10  WT-01-ENTRY-026.                                     GAS1UPD 
00781              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00782              15  WT-01-MESSAGE-TEXT-025.                          GAS1UPD 
00783                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00784                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00785                  20  FILLER          PIC X(3)  VALUE  '026'.      GAS1UPD 
00786                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00787                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00788                      'MAXIMUM OF 5 INTERNAL TABULARS HAS ALREADY BGAS1UPD 
00789 -                    'EEN REACHED              '.                 GAS1UPD 
00790              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00791 *----------------------------------------------------------------*GAS1UPD 
00792          10  WT-01-ENTRY-027.                                     GAS1UPD 
00793              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS1UPD 
00794              15  WT-01-MESSAGE-TEXT-025.                          GAS1UPD 
00795                  20  FILLER          PIC X(4)  VALUE  'GAS1'.     GAS1UPD 
00796                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS1UPD 
00797                  20  FILLER          PIC X(3)  VALUE  '027'.      GAS1UPD 
00798                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS1UPD 
00799                  20  FILLER          PIC X(70) VALUE              GAS1UPD 
00800                      '********** F U T U R E   U S E *************GAS1UPD 
00801 -                    '*************************'.                 GAS1UPD 
00802              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS1UPD 
00803 *----------------------------------------------------------------*GAS1UPD 
00804                                                                   GAS1UPD 
00805      05  WT-01-MESSAGE-TABLE         REDEFINES                    GAS1UPD 
00806          WT-01-MESSAGE-VALUES         OCCURS 027 TIMES            GAS1UPD 
00807                                      INDEXED BY WT-01-INDEX.      GAS1UPD 
00808          10  WT-01-ENTRY.                                         GAS1UPD 
00809              15  FILLER              PIC X(02).                   GAS1UPD 
00810              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GAS1UPD 
00811              15  FILLER              PIC X(02).                   GAS1UPD 
00812                                                                   GAS1UPD 
00813  01  WS-END                      PIC X(16)  VALUE                 GAS1UPD 
00814      '*** W/S ENDS ***'.                                          GAS1UPD 
00815 /    L I N K A G E   S E C T I O N                                GAS1UPD 
00816  LINKAGE SECTION.                                                 GAS1UPD 
00817  01  DFHCOMMAREA.                                                 GAS1UPD 
00818  COPY  G2ALCKEC.                                                  GAS1UPD 
00819  COPY  GACDACWA.                                                  GAS1UPD 
00820      05  GAS1UPD-PASSED-AREA.                                     GAS1UPD 
00821          07  LVL2-B-SW                PIC X.                      GAS1UPD 
00822          07  LVL2-F-SW                PIC X.                      GAS1UPD 
00823          07  LVL2-G-SW                PIC X.                      GAS1UPD 
00824          07  INTR-TAB-PGM-ID          PIC X(8).                   GAS1UPD 
00825          07  FILLER                   PIC X(09).                  GAS1UPD 
00826      05  DELADD-OPTION                PIC X(7).                   GAS1UPD 
00827                                                                   GAS1UPD 
00828 /*****************************************************************GAS1UPD 
00829 * W O R K F I L E   -   A L L   L E V E L   T A B U L A R   R E C GAS1UPD 
00830 ******************************************************************GAS1UPD 
00831  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS1UPD 
00832  COPY GCIOPRM1.                                                   GAS1UPD 
00833 /                                                                 GAS1UPD 
00834  COPY GCWRKDCC.                                                   GAS1UPD 
00835 /                                                                 GAS1UPD 
00836  COPY GCTABMC.                                                    GAS1UPD 
00837 /*****************************************************************GAS1UPD 
00838 *    C O M M U N I C A T I O N   K E Y   A R E A                  GAS1UPD 
00839 ******************************************************************GAS1UPD 
00840 *01  COMMUNICATION-KEY-AREA.                                      GAS1UPD 
00841 *COPY G2ALCKEC.                                                   GAS1UPD 
00842 *                                                                 GAS1UPD 
00843 /*****************************************************************GAS1UPD 
00844 *    C O P Y   T A B U L A R   T A B L E   A R E A                GAS1UPD 
00845 ******************************************************************GAS1UPD 
00846  01  COPY-TABULAR-TABLE-AREA.                                     GAS1UPD 
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          GAS1UPD 
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          GAS1UPD 
00848          COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               GAS1UPD 
00849        10  COPY-SORTABLE-FLDS              PIC X(169).            GAS1UPD 
00850        10  COPY-SORT-FYI                   PIC X(003).            GAS1UPD 
00851        10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      GAS1UPD 
00852                                                                   GAS1UPD 
00853 /*****************************************************************GAS1UPD 
00854 * W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   GAS1UPD 
00855 ******************************************************************GAS1UPD 
00856  01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              GAS1UPD 
00857  COPY GCIOPRM2.                                                   GAS1UPD 
00858 /                                                                 GAS1UPD 
00859  COPY GCWRKDC2.                                                   GAS1UPD 
00860 /                                                                 GAS1UPD 
00861  COPY GCTIPGPC.                                                   GAS1UPD 
00862 /*****************************************************************GAS1UPD 
00863 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     GAS1UPD 
00864 ******************************************************************GAS1UPD 
00865  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS1UPD 
00866  COPY GCIOPRMA.                                                   GAS1UPD 
00867                                                                   GAS1UPD 
00868  COPY GCWRKDCA.                                                   GAS1UPD 
00869                                                                   GAS1UPD 
00870  COPY GCTABM2.                                                    GAS1UPD 
00871 /*****************************************************************GAS1UPD 
00872 *    M A P S E T   A R E A                                        GAS1UPD 
00873 ******************************************************************GAS1UPD 
00874      COPY GA1XSETC.                                               GAS1UPD 
00875 /    P R O C E D U R E   D I V I S I O N                          GAS1UPD 
00876  PROCEDURE DIVISION.                                              GAS1UPD 
00877                                                                   GAS1UPD 
00878 ******************************************************************GAS1UPD 
00879 * 0000  HOUSEKEEPING                                             *GAS1UPD 
00880 ******************************************************************GAS1UPD 
00881  0000-000-HOUSEKEEPING          SECTION.                          GAS1UPD 
00882  0000-010.                                                        GAS1UPD 
00883                                                                   GAS1UPD 
00884 *                                                                 GAS1UPD 
00885      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD TO            GAS1UPD 
00886                      ACWA-WF-INTERNAL-TAB-PNTR.                   GAS1UPD 
00887                                                                   GAS1UPD 
00888      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS1UPD 
00889                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GAS1UPD 
00890                                                                   GAS1UPD 
00891      SET ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS1UPD 
00892                      ACWA-PR-ALL-LEVEL-TAB-PNTR.                  GAS1UPD 
00893                                                                   GAS1UPD 
00894      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GAS1UPD 
00895                                                                   GAS1UPD 
00896      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GAS1UPD 
00897                                                                   GAS1UPD 
00898 ***  MOVE GCA-FROM-MENU-ID  TO FRMNUIDO.                          GAS1UPD 
00899                                                                   GAS1UPD 
00900      IF FRMNUIDI  =  'GS3A'                                       GAS1UPD 
00901         MOVE IDLINEI  TO  GROUP-SPECIFIC-ID-LINE.                 GAS1UPD 
00902                                                                   GAS1UPD 
00903      IF FRMNUIDI  =  'GC4A' OR 'GTM1'                             GAS1UPD 
00904         MOVE IDLINEI  TO  CONTRACT-ID-LINE.                       GAS1UPD 
00905                                                                   GAS1UPD 
00906      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
00907         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE.              GAS1UPD 
00908                                                                   GAS1UPD 
00909      MOVE ABM-TITLE-LINE   TO  TITLEO.                            GAS1UPD 
00910                                                                   GAS1UPD 
00911      PERFORM 1000-000-MAIN-PROCESS.                               GAS1UPD 
00912                                                                   GAS1UPD 
00913      EXEC CICS  RETURN    END-EXEC.                               GAS1UPD 
00914      GOBACK.                                                      GAS1UPD 
00915                                                                   GAS1UPD 
00916  0000-900-EXIT.                                                   GAS1UPD 
00917        EXIT.                                                      GAS1UPD 
00918 /*****************************************************************GAS1UPD 
00919 * 1000  MAIN PROCESS                                             *GAS1UPD 
00920 ******************************************************************GAS1UPD 
00921  1000-000-MAIN-PROCESS          SECTION.                          GAS1UPD 
00922  1000-010.                                                        GAS1UPD 
00923                                                                   GAS1UPD 
00924      EXEC CICS  HANDLE CONDITION                                  GAS1UPD 
00925                 MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)   END-EXEC.   GAS1UPD 
00926                                                                   GAS1UPD 
00927      MOVE  INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.      GAS1UPD 
00928                                                                   GAS1UPD 
00929      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS1UPD 
00930         DELADDI   =     'CHG/ADD'                     AND         GAS1UPD 
00931         OENTCTRI  NOT = '0000000'                                 GAS1UPD 
00932         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS1UPD 
00933                                                                   GAS1UPD 
00934      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS1UPD 
00935         DELADDI   =    'CHG/DEL'                      AND         GAS1UPD 
00936         DELOPTNI  =    'D'                                        GAS1UPD 
00937         PERFORM  2400-000-DELETE-THIS-OCCURANCE.                  GAS1UPD 
00938                                                                   GAS1UPD 
00939      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS1UPD 
00940         DELADDI   =     'CHG/DEL'                      AND        GAS1UPD 
00941         DELOPTNI  NOT = 'D'                                       GAS1UPD 
00942         MOVE SPACES  TO  ERRMSGO                                  GAS1UPD 
00943         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS1UPD 
00944                                                                   GAS1UPD 
00945      IF (EIBAID   =    DFHPF4 OR DFHPF7 OR DFHPF8 OR              GAS1UPD 
00946                        DFHPF19 OR DFHPF20 OR DFHPF16) AND         GAS1UPD 
00947         DELADDI   =    'CHG/DEL'                      AND         GAS1UPD 
00948         DELOPTNI  NOT = 'D'                                       GAS1UPD 
00949         MOVE SPACES  TO  ERRMSGO                                  GAS1UPD 
00950         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS1UPD 
00951                                                                   GAS1UPD 
00952      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS1UPD 
00953         DELADDI   =    'CHG/DEL'                      AND         GAS1UPD 
00954         DELOPTNI  NOT = 'D'                                       GAS1UPD 
00955         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS1UPD 
00956                                                                   GAS1UPD 
00957  1000-900-EXIT.                                                   GAS1UPD 
00958        EXIT.                                                      GAS1UPD 
00959 /*****************************************************************GAS1UPD 
00960 * 2200  UPDATE THIS OCCURANCE                                    *GAS1UPD 
00961 *                                                                *GAS1UPD 
00962 *    THIS ROUTINE WILL CHANGE ANY FIELD THAT THE OPERATOR HAS    *GAS1UPD 
00963 *  CHANGED, AND HAS CODE FOR THE MAINTENANCE OF THE INTERNAL     *GAS1UPD 
00964 *  TABULAR ENTRIES.                                              *GAS1UPD 
00965 ******************************************************************GAS1UPD 
00966  2200-000-UPDATE-THIS-OCCURANCE SECTION.                          GAS1UPD 
00967  2200-010.                                                        GAS1UPD 
00968                                                                   GAS1UPD 
00969      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS1UPD 
00970                                                                   GAS1UPD 
00971      IF NOT GCIO-GOOD-RETURN                                      GAS1UPD 
00972         MOVE WS-ABCODE-1BF7        TO WS-ABCODE                   GAS1UPD 
00973         MOVE WS-ABCODE-1BF7-MSG    TO WS-ABCODE-MSG               GAS1UPD 
00974         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
00975                                                                   GAS1UPD 
00976      MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   GAS1UPD 
00977      SET GAA-INDEX         TO  1.                                 GAS1UPD 
00978      MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                GAS1UPD 
00979                                                                   GAS1UPD 
00980  2200-210-FIND-RIGHT-OCCURS.                                      GAS1UPD 
00981                                                                   GAS1UPD 
00982      IF  GAA-BAMA-BENEFIT-PERIOD(GAA-INDEX)  NOT = HIGH-VALUES ANDGAS1UPD 
00983          GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) NOT =                GAS1UPD 
00984                                                 ACWA-DISPLAY-LEN-7GAS1UPD 
00985      THEN                                                         GAS1UPD 
00986          IF  GAA-INDEX  <  GAA-ENTRY-COUNT                        GAS1UPD 
00987          THEN                                                     GAS1UPD 
00988              SET GAA-INDEX  UP BY  1                              GAS1UPD 
00989              GO TO 2200-210-FIND-RIGHT-OCCURS                     GAS1UPD 
00990          ELSE                                                     GAS1UPD 
00991              MOVE WS-ABCODE-1BL1        TO WS-ABCODE              GAS1UPD 
00992              MOVE WS-ABCODE-1BL1-MSG    TO WS-ABCODE-MSG          GAS1UPD 
00993              MOVE -1                    TO  MFRMSLTL              GAS1UPD 
00994              PERFORM 9800-000-ERROR-MSG-THEN-ABEND                GAS1UPD 
00995      ELSE                                                         GAS1UPD 
00996          NEXT SENTENCE.                                           GAS1UPD 
00997 *                                                                 GAS1UPD 
00998      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS1UPD 
00999          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01000          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01001          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01002          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01003          (IPGSOPTI  =  'MT' OR 'A')                               GAS1UPD 
01004      THEN                                                         GAS1UPD 
01005          ADD 1 TO ACWA-FIELD-CHG-CNT.                             GAS1UPD 
01006                                                                   GAS1UPD 
01007                                                                   GAS1UPD 
01008      IF DAYFACII NOT =   GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)      GAS1UPD 
01009         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01010         MOVE DAYFACII TO GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX).     GAS1UPD 
01011                                                                   GAS1UPD 
01012      IF COPAYINI  NOT =  GAA-BAMA-CO-PAY-IND (GAA-INDEX)          GAS1UPD 
01013         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01014         MOVE COPAYINI TO GAA-BAMA-CO-PAY-IND (GAA-INDEX).         GAS1UPD 
01015                                                                   GAS1UPD 
01016      IF CSTCONTI  NOT =   GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)   GAS1UPD 
01017         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01018         MOVE CSTCONTI  TO GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX).  GAS1UPD 
01019                                                                   GAS1UPD 
01020      IF PERIODI  NOT =    GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)     GAS1UPD 
01021         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01022         MOVE PERIODI   TO GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX).    GAS1UPD 
01023                                                                   GAS1UPD 
01024      IF DEFINTNI NOT =    GAA-BAMA-DEFINITION (GAA-INDEX)         GAS1UPD 
01025         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01026         MOVE DEFINTNI  TO GAA-BAMA-DEFINITION (GAA-INDEX).        GAS1UPD 
01027                                                                   GAS1UPD 
01028      IF PERTQALI  NOT =   GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)  GAS1UPD 
01029         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01030         MOVE PERTQALI  TO GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX). GAS1UPD 
01031                                                                   GAS1UPD 
01032      IF FAMINDII  NOT =   GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)       GAS1UPD 
01033         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01034         MOVE FAMINDII  TO GAA-BAMA-FAM-OR-INDIV (GAA-INDEX).      GAS1UPD 
01035                                                                   GAS1UPD 
01036      IF PLCTRMTI  NOT =   GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX) GAS1UPD 
01037         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01038         MOVE PLCTRMTI  TO GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX).GAS1UPD 
01039                                                                   GAS1UPD 
01040      IF SRVGRUPI  NOT =   GAA-BAMA-SERVICE-GROUP (GAA-INDEX)      GAS1UPD 
01041         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01042         MOVE SRVGRUPI  TO GAA-BAMA-SERVICE-GROUP (GAA-INDEX).     GAS1UPD 
01043                                                                   GAS1UPD 
01044        MOVE PRTIMEFI   TO ACWA-DISPLAY-LEN-3-X.                   GAS1UPD 
01045        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS1UPD 
01046                           GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)  GAS1UPD 
01047         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01048        MOVE ACWA-DISPLAY-LEN-3                                    GAS1UPD 
01049                        TO GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX). GAS1UPD 
01050                                                                   GAS1UPD 
01051        MOVE AGELIMLI   TO ACWA-DISPLAY-LEN-3-X.                   GAS1UPD 
01052        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS1UPD 
01053                           GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)     GAS1UPD 
01054         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01055        MOVE ACWA-DISPLAY-LEN-3                                    GAS1UPD 
01056                        TO GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX).    GAS1UPD 
01057                                                                   GAS1UPD 
01058        MOVE AGELIMHI   TO ACWA-DISPLAY-LEN-3-X.                   GAS1UPD 
01059        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS1UPD 
01060                           GAA-BAMA-AGE-LIMIT-TO   (GAA-INDEX)     GAS1UPD 
01061         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01062        MOVE ACWA-DISPLAY-LEN-3                                    GAS1UPD 
01063                        TO GAA-BAMA-AGE-LIMIT-TO   (GAA-INDEX).    GAS1UPD 
01064                                                                   GAS1UPD 
01065      IF FEAKINDI  NOT =  GAA-BAMA-FEAK-IND        (GAA-INDEX)     GAS1UPD 
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE FEAKINDI TO GAA-BAMA-FEAK-IND        (GAA-INDEX).    GAS1UPD 
01068                                                                   GAS1UPD 
01065      IF ASCDSCDI  NOT =                                           GAS1UPD 
                        GAA-BAMA-ASCEND-DESCEND-IND     (GAA-INDEX)             
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE ASCDSCDI TO                                          GAS1UPD 
                        GAA-BAMA-ASCEND-DESCEND-IND     (GAA-INDEX).            
01068                                                                   GAS1UPD 
      **P21595 CHANGES STARTS                                                   
01065      IF BENTYPI   NOT =                                           GAS1UPD 
                        GAA-BAMA-BEN-TYPE               (GAA-INDEX)             
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE BENTYPI  TO                                          GAS1UPD 
                        GAA-BAMA-BEN-TYPE               (GAA-INDEX).            
01068                                                                   GAS1UPD 
01065      IF TIERCDI   NOT =                                           GAS1UPD 
                        GAA-BAMA-TIER-CODE              (GAA-INDEX)             
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE TIERCDI  TO                                          GAS1UPD 
                        GAA-BAMA-TIER-CODE              (GAA-INDEX).            
01068                                                                   GAS1UPD 
01065      IF TIERLVI   NOT =                                           GAS1UPD 
                        GAA-BAMA-TIER-LVL               (GAA-INDEX)             
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE TIERLVI  TO                                          GAS1UPD 
                        GAA-BAMA-TIER-LVL               (GAA-INDEX).            
      **P21595 CHANGES ENDS                                                     
                                                                                
01065      IF BISNDINI  NOT =                                           GAS1UPD 
                        GAA-BAMA-BISCENDING-IND-RSV     (GAA-INDEX)             
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01067         MOVE BISNDINI TO                                          GAS1UPD 
                        GAA-BAMA-BISCENDING-IND-RSV     (GAA-INDEX).            
                                                                                
01069      IF ACCUMIDI  NOT =  GAA-BAMA-ACCUMID         (GAA-INDEX)     GAS1UPD 
01070         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01071         MOVE ACCUMIDI TO GAA-BAMA-ACCUMID         (GAA-INDEX).    GAS1UPD 
01072                                                                   GAS1UPD 
01073      IF CAPINDI   NOT =  GAA-BAMA-COMB-APPLIED-IND (GAA-INDEX)    GAS1UPD 
01074         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01075         MOVE CAPINDI  TO GAA-BAMA-COMB-APPLIED-IND (GAA-INDEX).   GAS1UPD 
01076                                                                   GAS1UPD 
01077      IF SABDINDI  NOT =  GAA-BAMA-SEL-ADDL-BEN-DET (GAA-INDEX)    GAS1UPD 
01078         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01079         MOVE SABDINDI TO GAA-BAMA-SEL-ADDL-BEN-DET (GAA-INDEX).   GAS1UPD 
01080                                                                   GAS1UPD 
01081      IF AGEQLLI   NOT =  GAA-BAMA-AGE-QUAL-IND-FROM(GAA-INDEX)    GAS1UPD 
01082         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01083         MOVE AGEQLLI  TO                                          GAS1UPD 
01084                         GAA-BAMA-AGE-QUAL-IND-FROM(GAA-INDEX).    GAS1UPD 
01085                                                                   GAS1UPD 
01086      IF AGEQLHI   NOT =  GAA-BAMA-AGE-QUAL-IND-TO  (GAA-INDEX)    GAS1UPD 
01087         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01088         MOVE AGEQLHI  TO                                          GAS1UPD 
01089                         GAA-BAMA-AGE-QUAL-IND-TO  (GAA-INDEX).    GAS1UPD 
01090                                                                   GAS1UPD 
01091      IF RELPINDI  NOT =  GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX)    GAS1UPD 
01092         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01093         MOVE RELPINDI TO                                          GAS1UPD 
01094                         GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX).    GAS1UPD 
01095                                                                   GAS1UPD 
01096      IF MAXOVRDI  NOT =  GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX)GAS1UPD 
01097         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01098         MOVE MAXOVRDI TO                                          GAS1UPD 
01099                         GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX).GAS1UPD 
01100                                                                   GAS1UPD 
01101      IF REININDI  NOT =   GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)  GAS1UPD 
01102         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01103         MOVE REININDI  TO GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX). GAS1UPD 
01104                                                                   GAS1UPD 
01105      IF CLMLVLII NOT =   GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX) GAS1UPD 
01106         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01107         MOVE CLMLVLII TO GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX).GAS1UPD 
01108                                                                   GAS1UPD 
01109         MOVE INTRVALI TO ACWA-DISPLAY-LEN-3-X.                    GAS1UPD 
01110      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS1UPD 
01111                          GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)  GAS1UPD 
01112         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01113         MOVE ACWA-DISPLAY-LEN-3 TO                                GAS1UPD 
01114                         GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX).  GAS1UPD 
01115                                                                   GAS1UPD 
01116      IF INTTYPEI  NOT =  GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)       GAS1UPD 
01117         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01118         MOVE INTTYPEI TO GAA-BAMA-INTERVAL-TYPE (GAA-INDEX).      GAS1UPD 
01119                                                                   GAS1UPD 
01120      IF LOBI NOT =       GAA-BAMA-L-O-B   (GAA-INDEX)             GAS1UPD 
01121         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01122         MOVE LOBI     TO GAA-BAMA-L-O-B (GAA-INDEX).              GAS1UPD 
01123                                                                   GAS1UPD 
01124      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS1UPD 
01125      THEN                                                         GAS1UPD 
01126          MOVE ACWA-VAL-LIM-SCREEN TO ACWA-VALUE-LIMIT-9-9         GAS1UPD 
01127      ELSE                                                         GAS1UPD 
01128          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                GAS1UPD 
01129              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'                   GAS1UPD 
01130          THEN                                                     GAS1UPD 
01131              MOVE -1                  TO ACWA-VALUE-LIMIT-9-9     GAS1UPD 
               ELSE                                                             
01128          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                GAS1UPD 
01129              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'                   GAS1UPD 
01130          THEN                                                     GAS1UPD 
01131              MOVE -2                  TO ACWA-VALUE-LIMIT-9-9     GAS1UPD 
01132          ELSE                                                     GAS1UPD 
01133              MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7     GAS1UPD 
01134              MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2.    GAS1UPD 
01135                                                                   GAS1UPD 
01136      IF ACWA-VALUE-LIMIT-9 NOT = GAA-BAMA-VALUE-LIMIT (GAA-INDEX) GAS1UPD 
01137         PERFORM 2600-000-PROCESS-VAL-LIMIT.                       GAS1UPD 
01138                                                                   GAS1UPD 
01139      IF ACWA-VALUE-LIMIT-9 NOT = GAA-BAMA-VALUE-LIMIT (GAA-INDEX) GAS1UPD 
01140         ADD 1                  TO ACWA-FIELD-CHG-CNT              GAS1UPD 
01141        MOVE ACWA-VALUE-LIMIT-9 TO GAA-BAMA-VALUE-LIMIT(GAA-INDEX).GAS1UPD 
01142                                                                   GAS1UPD 
01143      IF BENVLQLI  NOT =   GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)    GAS1UPD 
01144         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01145         MOVE BENVLQLI  TO GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX).   GAS1UPD 
01146                                                                   GAS1UPD 
01147      MOVE NEWVALUI     TO ACWA-DISPLAY-LEN-5-X.                   GAS1UPD 
01148      IF  ACWA-DISPLAY-LEN-5 NOT =                                 GAS1UPD 
01149                       GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)    GAS1UPD 
01150      THEN                                                         GAS1UPD 
01151          ADD 1     TO ACWA-FIELD-CHG-CNT                          GAS1UPD 
01152          MOVE ACWA-DISPLAY-LEN-5                                  GAS1UPD 
01153                    TO GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX).   GAS1UPD 
01154                                                                   GAS1UPD 
01155      IF OVRDINDI  NOT =   GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)  GAS1UPD 
01156         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01157         MOVE OVRDINDI  TO GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX). GAS1UPD 
01158                                                                   GAS1UPD 
01159                                                                   GAS1UPD 
01160      IF FYIVALI   NOT =  GAA-BAMA-FYI-VALUE (GAA-INDEX)           GAS1UPD 
01161         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01162         MOVE FYIVALI  TO GAA-BAMA-FYI-VALUE (GAA-INDEX).          GAS1UPD 
01163                                                                   GAS1UPD 
01164      IF CONDALLI  NOT =   GAA-COND-ALL-BIT (GAA-INDEX)            GAS1UPD 
01165         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01166         MOVE CONDALLI  TO GAA-COND-ALL-BIT (GAA-INDEX).           GAS1UPD 
01167                                                                   GAS1UPD 
01168      IF INTDESKI  =  GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)     GAS1UPD 
01169         MOVE SPACE  TO  WS-INT-TAB-CHANGE-INDICATOR               GAS1UPD 
01170      ELSE                                                         GAS1UPD 
01171         ADD 1   TO  ACWA-FIELD-CHG-CNT                            GAS1UPD 
01172         IF INTDESKI  =  IDPRODI                                   GAS1UPD 
01173            MOVE 'NP'   TO  WS-INT-TAB-CHANGE-INDICATOR            GAS1UPD 
01174            MOVE INTDESKI  TO                                      GAS1UPD 
01175                            GAA-BAMA-INTERNAL-DESCRIPTOR(GAA-INDEX)GAS1UPD 
01176         ELSE                                                      GAS1UPD 
01177            IF IDPRODI  =   GAA-BAMA-INTERNAL-DESCRIPTOR(GAA-INDEX)GAS1UPD 
01178               MOVE 'PN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS1UPD 
01179               MOVE INTDESKI  TO                                   GAS1UPD 
01180                            GAA-BAMA-INTERNAL-DESCRIPTOR(GAA-INDEX)GAS1UPD 
01181            ELSE                                                   GAS1UPD 
01182               MOVE 'NN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS1UPD 
01183               MOVE INTDESKI  TO                                   GAS1UPD 
01184                           GAA-BAMA-INTERNAL-DESCRIPTOR(GAA-INDEX).GAS1UPD 
01185                                                                   GAS1UPD 
01186      IF CONDEXCI  NOT =   GAA-COND-EXCLUSION-BIT (GAA-INDEX)      GAS1UPD 
01187         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01188         MOVE CONDEXCI  TO GAA-COND-EXCLUSION-BIT (GAA-INDEX).     GAS1UPD 
01189                                                                   GAS1UPD 
01190      IF CONDICDI  NOT =   GAA-COND-ICD-BIT (GAA-INDEX)            GAS1UPD 
01191         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01192         MOVE CONDICDI  TO GAA-COND-ICD-BIT (GAA-INDEX).           GAS1UPD 
01193                                                                   GAS1UPD 
01194      IF CONDTABI  NOT =   GAA-COND-TB-BIT (GAA-INDEX)             GAS1UPD 
01195         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01196         MOVE CONDTABI  TO GAA-COND-TB-BIT (GAA-INDEX).            GAS1UPD 
01197                                                                   GAS1UPD 
01198      IF CONDMENI  NOT =   GAA-COND-MENTAL-BIT (GAA-INDEX)         GAS1UPD 
01199         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01200         MOVE CONDMENI  TO GAA-COND-MENTAL-BIT (GAA-INDEX).        GAS1UPD 
01201                                                                   GAS1UPD 
01202      IF CONDDRGI  NOT =   GAA-COND-DRUG-BIT (GAA-INDEX)           GAS1UPD 
01203         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01204         MOVE CONDDRGI  TO GAA-COND-DRUG-BIT (GAA-INDEX).          GAS1UPD 
01205                                                                   GAS1UPD 
01206      IF CONDALCI  NOT =   GAA-COND-ALCOHOL-BIT (GAA-INDEX)        GAS1UPD 
01207         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01208         MOVE CONDALCI  TO GAA-COND-ALCOHOL-BIT (GAA-INDEX).       GAS1UPD 
01209                                                                   GAS1UPD 
01210      IF CONDOBCI  NOT =   GAA-COND-OB-COMP-BIT (GAA-INDEX)        GAS1UPD 
01211         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01212         MOVE CONDOBCI  TO GAA-COND-OB-COMP-BIT (GAA-INDEX).       GAS1UPD 
01213                                                                   GAS1UPD 
01214      IF CONDOBNI  NOT =   GAA-COND-OB-NORM-BIT (GAA-INDEX)        GAS1UPD 
01215         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01216         MOVE CONDOBNI  TO GAA-COND-OB-NORM-BIT (GAA-INDEX).       GAS1UPD 
01217                                                                   GAS1UPD 
01218      IF CONDMALI  NOT =   GAA-COND-MALIGNANCY-BIT (GAA-INDEX)     GAS1UPD 
01219         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS1UPD 
01220         MOVE CONDMALI  TO GAA-COND-MALIGNANCY-BIT (GAA-INDEX).    GAS1UPD 
01221                                                                   GAS1UPD 
01222      IF CONDCARI  NOT =  GAA-COND-CARDIAC-DISEASE-BIT (GAA-INDEX) GAS1UPD 
01223         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01224        MOVE CONDCARI  TO GAA-COND-CARDIAC-DISEASE-BIT (GAA-INDEX).GAS1UPD 
01225                                                                   GAS1UPD 
01226      IF CONDOBSI  NOT =  GAA-COND-OBESITY-BIT (GAA-INDEX)         GAS1UPD 
01227         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01228         MOVE CONDOBSI TO GAA-COND-OBESITY-BIT (GAA-INDEX).        GAS1UPD 
01229                                                                   GAS1UPD 
01230      IF CONDKDYI  NOT =  GAA-COND-KIDNEY-DISEASE-BIT (GAA-INDEX)  GAS1UPD 
01231         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS1UPD 
01232         MOVE CONDKDYI TO GAA-COND-KIDNEY-DISEASE-BIT (GAA-INDEX). GAS1UPD 
01233                                                                   GAS1UPD 
01234      IF CONDACCI  NOT  =   GAA-COND-ACCIDENT-BIT (GAA-INDEX)      GAS1UPD 
01235         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01236         MOVE CONDACCI  TO  GAA-COND-ACCIDENT-BIT (GAA-INDEX).     GAS1UPD 
01237                                                                   GAS1UPD 
01238      IF CONDPECI  NOT  =  GAA-COND-PRE-EXIST-BIT (GAA-INDEX)      GAS1UPD 
01239         ADD  1        TO  ACWA-FIELD-CHG-CNT                      GAS1UPD 
01240         MOVE CONDPECI TO  GAA-COND-PRE-EXIST-BIT (GAA-INDEX).     GAS1UPD 
01241                                                                   GAS1UPD 
01242      IF CONDNEMI  NOT  =   GAA-COND-NON-EMER-BIT (GAA-INDEX)      GAS1UPD 
01243         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01244         MOVE CONDNEMI  TO  GAA-COND-NON-EMER-BIT (GAA-INDEX).     GAS1UPD 
01245                                                                   GAS1UPD 
01246      IF CONDSUII  NOT  =   GAA-COND-SUICIDE-BIT  (GAA-INDEX)      GAS1UPD 
01247         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01248         MOVE CONDSUII  TO  GAA-COND-SUICIDE-BIT  (GAA-INDEX).     GAS1UPD 
01249                                                                   GAS1UPD 
01250      IF CONDTMJI  NOT  =   GAA-COND-TMJ-BIT      (GAA-INDEX)      GAS1UPD 
01251         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01252         MOVE CONDTMJI  TO  GAA-COND-TMJ-BIT      (GAA-INDEX).     GAS1UPD 
01253                                                                   GAS1UPD 
01254      IF CONDINFI  NOT  =   GAA-COND-INF-BIT      (GAA-INDEX)      GAS1UPD 
01255         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01256         MOVE CONDINFI  TO  GAA-COND-INF-BIT      (GAA-INDEX).     GAS1UPD 
01257                                                                   GAS1UPD 
01258      IF CONDLIFI  NOT  =   GAA-COND-LIFE-THREAT-BIT  (GAA-INDEX)  GAS1UPD 
01259         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01260         MOVE CONDLIFI  TO  GAA-COND-LIFE-THREAT-BIT  (GAA-INDEX). GAS1UPD 
01261                                                                   GAS1UPD 
01262      IF CONDEMCI  NOT  =   GAA-COND-EMER-MED-BIT     (GAA-INDEX)  GAS1UPD 
01263         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01264         MOVE CONDEMCI  TO  GAA-COND-EMER-MED-BIT     (GAA-INDEX). GAS1UPD 
01265                                                                   GAS1UPD 
01266      IF CONDEACI  NOT  =   GAA-COND-EMER-ACC-BIT     (GAA-INDEX)  GAS1UPD 
01267         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01268         MOVE CONDEACI  TO  GAA-COND-EMER-ACC-BIT     (GAA-INDEX). GAS1UPD 
01269                                                                   GAS1UPD 
01270      IF CONDSMII  NOT  =   GAA-COND-SER-MEN-ILL-BIT  (GAA-INDEX)  GAS1UPD 
01271         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01272         MOVE CONDSMII  TO  GAA-COND-SER-MEN-ILL-BIT  (GAA-INDEX). GAS1UPD 
01273                                                                   GAS1UPD 
01274      IF CONDNSMI  NOT  = GAA-COND-NON-SER-MEN-ILL-BIT (GAA-INDEX) GAS1UPD 
01275         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS1UPD 
01276         MOVE CONDNSMI  TO GAA-COND-NON-SER-MEN-ILL-BIT (GAA-INDEX)GAS1UPD 
01277                                                                   GAS1UPD 
01278      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
01279         MOVE GCIO-WRK-TABULAR-PROVISION  TO                       GAS1UPD 
01280                                     GCIO-WRK-BENEFIT-PROVISION.   GAS1UPD 
01281                                                                   GAS1UPD 
01282 ******* IF THE OCCUR IS A NEW ADDED ONE THEN IT IS FLAGED 1U      GAS1UPD 
01283 *** IN GA1BPGM ALL ATTACHED INTERNAL TABS TO THIS ADDED OCCUR     GAS1UPD 
01284 *** ALSO WILL BE FLAGED 1U IN THIS ROUTINE    NE 08/03/88         GAS1UPD 
01285                                                                   GAS1UPD 
01286      PERFORM  5000-000-READ-PROD-ALL-LVL-TAB.                     GAS1UPD 
01287         SEARCH GAA2-ENTRY                                         GAS1UPD 
01288            VARYING GAA2-INDEX                                     GAS1UPD 
01289            WHEN                                                   GAS1UPD 
01290               GAA2-INDEX NOT <  GAA2-ENTRY-COUNT  OR              GAS1UPD 
01291               GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)  =            GAS1UPD 
01292                              GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  GAS1UPD 
01293               NEXT SENTENCE.                                      GAS1UPD 
01294                                                                   GAS1UPD 
01295         IF GAA2-INDEX <  GAA2-ENTRY-COUNT  AND                    GAS1UPD 
01296            GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)  =               GAS1UPD 
01297                              GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  GAS1UPD 
01298            MOVE 'N'      TO  WS-NEW-OCCR-ON-WF                    GAS1UPD 
01299         ELSE                                                      GAS1UPD 
01300            MOVE 'Y'      TO WS-NEW-OCCR-ON-WF.                    GAS1UPD 
01301 ******************************************************** 8/3/88   GAS1UPD 
01302      IF ACWA-INTERNAL-TAB-CHANGE-ONLY                             GAS1UPD 
01303         GO TO 2200-260-CHANGE-INTERNAL-TAB.                       GAS1UPD 
01304                                                                   GAS1UPD 
01305 *** CHECK LVL2-B-SWITCH                                           GAS1UPD 
01306      IF EIBAID    =       DFHENTER  AND                           GAS1UPD 
01307         DELADDI   =      'CHG/ADD'  AND                           GAS1UPD 
01308         OENTCTRI  NOT =  '0000000'  AND                           GAS1UPD 
01309         ACWA-NO-CHANGE-FOUND                                      GAS1UPD 
01310         MOVE 'Y'   TO  LVL2-B-SW                                  GAS1UPD 
01311         PERFORM 3100-RLSE-RU-GAA-REC                              GAS1UPD 
01312         GO  TO  2200-900-EXIT.                                    GAS1UPD 
01313                                                                   GAS1UPD 
01314      IF  EIBAID  =  DFHENTER       AND                            GAS1UPD 
01315          ACWA-SCREEN-HAS-NO-ERRORS AND                            GAS1UPD 
01316          GCVI-TABLE-SW = 'N'                                      GAS1UPD 
01317      THEN                                                         GAS1UPD 
01318          IF  ACWA-NO-CHANGE-FOUND                                 GAS1UPD 
01319          THEN                                                     GAS1UPD 
01320              SET  WT-01-INDEX                     TO +11          GAS1UPD 
01321              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS1UPD 
01322              PERFORM 7900-000-RESET-ATTRIBUTES                    GAS1UPD 
01323              MOVE -1 TO PERIODL                                   GAS1UPD 
01324              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS1UPD 
01325          ELSE                                                     GAS1UPD 
01326              SET  WT-01-INDEX                     TO +07          GAS1UPD 
01327              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS1UPD 
01328              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS1UPD 
01329      ELSE                                                         GAS1UPD 
01330          NEXT SENTENCE.                                           GAS1UPD 
01331                                                                   GAS1UPD 
01332      IF (EIBAID  =  DFHPF4 OR  DFHPF16) AND                       GAS1UPD 
01333          ACWA-SCREEN-HAS-NO-ERRORS      AND                       GAS1UPD 
01334          GCVI-TABLE-SW = 'N'            AND                       GAS1UPD 
01335          ACWA-NO-CHANGE-FOUND                                     GAS1UPD 
01336      THEN                                                         GAS1UPD 
01337          SET  WT-01-INDEX                     TO +09              GAS1UPD 
01338          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS1UPD 
01339          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS1UPD 
01340          MOVE -1 TO PERIODL                                       GAS1UPD 
01341          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS1UPD 
01342                                                                   GAS1UPD 
01343      IF  EIBAID   =   DFHENTER AND                                GAS1UPD 
01344          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS1UPD 
01345          ACWA-NO-CHANGE-FOUND                                     GAS1UPD 
01346      THEN                                                         GAS1UPD 
01347          SET  WT-01-INDEX                     TO +11              GAS1UPD 
01348          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS1UPD 
01349          MOVE -1 TO PERIODL                                       GAS1UPD 
01350          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS1UPD 
01351                                                                   GAS1UPD 
01352      PERFORM 7900-000-RESET-ATTRIBUTES.                           GAS1UPD 
01353                                                                   GAS1UPD 
01354 *** LVL2-F-SWITCH                                                 GAS1UPD 
01355      IF  (EIBAID  =   DFHPF7 OR DFHPF19) AND                      GAS1UPD 
01356          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS1UPD 
01357          ACWA-NO-CHANGE-FOUND                                     GAS1UPD 
01358      THEN                                                         GAS1UPD 
01359          MOVE  'Y'   TO  LVL2-F-SW                                GAS1UPD 
01360         PERFORM 3100-RLSE-RU-GAA-REC                              GAS1UPD 
01361          GO TO  2200-900-EXIT.                                    GAS1UPD 
01362                                                                   GAS1UPD 
01363 *** LVL2-G-SWITCH                                                 GAS1UPD 
01364      IF  (EIBAID  =  DFHPF8 OR DFHPF20) AND                       GAS1UPD 
01365          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS1UPD 
01366          ACWA-NO-CHANGE-FOUND                                     GAS1UPD 
01367      THEN                                                         GAS1UPD 
01368          MOVE  'Y'   TO  LVL2-G-SW                                GAS1UPD 
01369         PERFORM 3100-RLSE-RU-GAA-REC                              GAS1UPD 
01370          GO TO  2200-900-EXIT.                                    GAS1UPD 
01371                                                                   GAS1UPD 
01372      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS1UPD 
01373         DELADDI  =  'CHG/DEL'                                     GAS1UPD 
01374         PERFORM 4600-000-UPDATE-CDE-STATUS.                       GAS1UPD 
01375                                                                   GAS1UPD 
01376 *                                                                 GAS1UPD 
01377      IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE OR             GAS1UPD 
01378          (IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999')) AND      GAS1UPD 
01379          (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE OR             GAS1UPD 
01380          (IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999')) AND      GAS1UPD 
01381          (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE OR             GAS1UPD 
01382          (IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999')) AND      GAS1UPD 
01383          (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE OR             GAS1UPD 
01384          (IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999')) AND      GAS1UPD 
01385          (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE OR             GAS1UPD 
01386          (IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999')) AND      GAS1UPD 
01387          (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE OR             GAS1UPD 
01388          (IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'))          GAS1UPD 
01389      THEN                                                         GAS1UPD 
01390          GO TO 2200-250-UPDATE-ALL-LVL-TAB.                       GAS1UPD 
01391 *                                                                 GAS1UPD 
01392      IF  IBGROPTI  =  'C' OR                                      GAS1UPD 
01393          IDGDOPTI  =  'C' OR                                      GAS1UPD 
01394          IPGNOPTI  =  'C' OR                                      GAS1UPD 
01395          IPGPOPTI  =  'C' OR                                      GAS1UPD 
01396          IPGTOPTI  =  'C' OR                                      GAS1UPD 
01397          IPGSOPTI  =  'C'                                         GAS1UPD 
01398      THEN                                                         GAS1UPD 
01399          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS1UPD 
01400                                                                   GAS1UPD 
01401      IF  (IBGROPTI  =  'MT' OR 'A') AND IBGRSLTI  =  '0000000' OR GAS1UPD 
01402          (IDGDOPTI  =  'MT' OR 'A') AND IDGDSLTI  =  '0000000' OR GAS1UPD 
01403          (IPGNOPTI  =  'MT' OR 'A') AND IPGNSLTI  =  '0000000' OR GAS1UPD 
01404          (IPGPOPTI  =  'MT' OR 'A') AND IPGPSLTI  =  '0000000' OR GAS1UPD 
01405          (IPGTOPTI  =  'MT' OR 'A') AND IPGTSLTI  =  '0000000' OR GAS1UPD 
01406          (IPGSOPTI  =  'MT' OR 'A') AND IPGSSLTI  =  '0000000'    GAS1UPD 
01407      THEN                                                         GAS1UPD 
01408          GO TO 2200-220-ADD-INTERNAL-OCCURS.                      GAS1UPD 
01409 *                                                                 GAS1UPD 
01410      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01411          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01412          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01413          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01414          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS1UPD 
01415          (IPGSOPTI  =  'MT' OR 'A')                               GAS1UPD 
01416      THEN                                                         GAS1UPD 
01417          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS1UPD 
01418 *                                                                 GAS1UPD 
01419      IF  IBGROPTI  =  'D'                                         GAS1UPD 
01420      THEN                                                         GAS1UPD 
01421          MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID             GAS1UPD 
01422          MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO             GAS1UPD 
01423      ELSE                                                         GAS1UPD 
01424          IF  IPGNOPTI  =  'D'                                     GAS1UPD 
01425          THEN                                                     GAS1UPD 
01426              MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID         GAS1UPD 
01427              MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO         GAS1UPD 
01428          ELSE                                                     GAS1UPD 
01429              IF  IPGTOPTI  =  'D'                                 GAS1UPD 
01430              THEN                                                 GAS1UPD 
01431                  MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS1UPD 
01432                  MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS1UPD 
01433          ELSE                                                     GAS1UPD 
01434              IF  IPGSOPTI  =  'D'                                 GAS1UPD 
01435              THEN                                                 GAS1UPD 
01436                  MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS1UPD 
01437                  MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS1UPD 
01438              ELSE                                                 GAS1UPD 
01439              IF  IDGDOPTI  =  'D'                                 GAS1UPD 
01440              THEN                                                 GAS1UPD 
01441                  MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS1UPD 
01442                  MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS1UPD 
01443              ELSE                                                 GAS1UPD 
01444              IF  IPGPOPTI  =  'D'                                 GAS1UPD 
01445              THEN                                                 GAS1UPD 
01446                  MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS1UPD 
01447                  MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS1UPD 
01448              ELSE                                                 GAS1UPD 
01449                  NEXT SENTENCE.                                   GAS1UPD 
01450                                                                   GAS1UPD 
01451                                                                   GAS1UPD 
01452      IF    GAA-BAMA-INTL-TAB-1 (GAA-INDEX)                        GAS1UPD 
01453          = GCIO-WRK-TABULAR-PROVISION                             GAS1UPD 
01454      THEN                                                         GAS1UPD 
01455          MOVE GAA-BAMA-INTL-TAB-2 (GAA-INDEX)                     GAS1UPD 
01456            TO GAA-BAMA-INTL-TAB-1 (GAA-INDEX)                     GAS1UPD 
01457          MOVE GAA-BAMA-INTL-TAB-3 (GAA-INDEX)                     GAS1UPD 
01458            TO GAA-BAMA-INTL-TAB-2 (GAA-INDEX)                     GAS1UPD 
01459          MOVE GAA-BAMA-INTL-TAB-4 (GAA-INDEX)                     GAS1UPD 
01460            TO GAA-BAMA-INTL-TAB-3 (GAA-INDEX)                     GAS1UPD 
01461          MOVE GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                     GAS1UPD 
01462            TO GAA-BAMA-INTL-TAB-4 (GAA-INDEX)                     GAS1UPD 
01463 ******** MOVE HIGH-VALUES                                         GAS1UPD 
01464 ********   TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                     GAS1UPD 
01465          IF GAA-BAMA-INTL-TAB-4 (GAA-INDEX) = HIGH-VALUES         GAS1UPD 
01466                                                 OR WS-SPACES-ZEROSGAS1UPD 
01467             MOVE WS-SPACES-ZEROS                                  GAS1UPD 
01468               TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                  GAS1UPD 
01469          ELSE                                                     GAS1UPD 
01470             MOVE HIGH-VALUES                                      GAS1UPD 
01471               TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                  GAS1UPD 
01472          END-IF                                                   GAS1UPD 
01473      ELSE                                                         GAS1UPD 
01474          IF   GAA-BAMA-INTL-TAB-2 (GAA-INDEX)                     GAS1UPD 
01475             = GCIO-WRK-TABULAR-PROVISION                          GAS1UPD 
01476         THEN                                                      GAS1UPD 
01477             MOVE GAA-BAMA-INTL-TAB-3 (GAA-INDEX)                  GAS1UPD 
01478               TO GAA-BAMA-INTL-TAB-2 (GAA-INDEX)                  GAS1UPD 
01479             MOVE GAA-BAMA-INTL-TAB-4 (GAA-INDEX)                  GAS1UPD 
01480               TO GAA-BAMA-INTL-TAB-3 (GAA-INDEX)                  GAS1UPD 
01481             MOVE GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                  GAS1UPD 
01482               TO GAA-BAMA-INTL-TAB-4 (GAA-INDEX)                  GAS1UPD 
01483 *********** MOVE HIGH-VALUES                                      GAS1UPD 
01484 ***********   TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)                  GAS1UPD 
01485             IF GAA-BAMA-INTL-TAB-4 (GAA-INDEX) = HIGH-VALUES      GAS1UPD 
01486                                                 OR WS-SPACES-ZEROSGAS1UPD 
01487                MOVE WS-SPACES-ZEROS                               GAS1UPD 
01488                  TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)               GAS1UPD 
01489             ELSE                                                  GAS1UPD 
01490                MOVE HIGH-VALUES                                   GAS1UPD 
01491                  TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)               GAS1UPD 
01492             END-IF                                                GAS1UPD 
01493         ELSE                                                      GAS1UPD 
01494             IF    GAA-BAMA-INTL-TAB-3 (GAA-INDEX)                 GAS1UPD 
01495                 = GCIO-WRK-TABULAR-PROVISION                      GAS1UPD 
01496             THEN                                                  GAS1UPD 
01497                 MOVE GAA-BAMA-INTL-TAB-4 (GAA-INDEX)              GAS1UPD 
01498                   TO GAA-BAMA-INTL-TAB-3 (GAA-INDEX)              GAS1UPD 
01499                 MOVE GAA-BAMA-INTL-TAB-5 (GAA-INDEX)              GAS1UPD 
01500                   TO GAA-BAMA-INTL-TAB-4 (GAA-INDEX)              GAS1UPD 
01501 *************** MOVE HIGH-VALUES                                  GAS1UPD 
01502 ***************   TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)              GAS1UPD 
01503                 IF GAA-BAMA-INTL-TAB-4 (GAA-INDEX)                GAS1UPD 
01504                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS1UPD 
01505                    MOVE WS-SPACES-ZEROS                           GAS1UPD 
01506                      TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)           GAS1UPD 
01507                 ELSE                                              GAS1UPD 
01508                    MOVE HIGH-VALUES                               GAS1UPD 
01509                      TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)           GAS1UPD 
01510                 END-IF                                            GAS1UPD 
01511             ELSE                                                  GAS1UPD 
01512                 IF    GAA-BAMA-INTL-TAB-4 (GAA-INDEX)             GAS1UPD 
01513                     = GCIO-WRK-TABULAR-PROVISION                  GAS1UPD 
01514                 THEN                                              GAS1UPD 
01515                     MOVE GAA-BAMA-INTL-TAB-5 (GAA-INDEX)          GAS1UPD 
01516                       TO GAA-BAMA-INTL-TAB-4 (GAA-INDEX)          GAS1UPD 
01517 ******************* MOVE HIGH-VALUES                              GAS1UPD 
01518 *******************   TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)          GAS1UPD 
01519                     IF GAA-BAMA-INTL-TAB-4 (GAA-INDEX)            GAS1UPD 
01520                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS1UPD 
01521                        MOVE WS-SPACES-ZEROS                       GAS1UPD 
01522                          TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)       GAS1UPD 
01523                     ELSE                                          GAS1UPD 
01524                        MOVE HIGH-VALUES                           GAS1UPD 
01525                          TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)       GAS1UPD 
01526                     END-IF                                        GAS1UPD 
01527                 ELSE                                              GAS1UPD 
01528                 IF    GAA-BAMA-INTL-TAB-5 (GAA-INDEX)             GAS1UPD 
01529                     = GCIO-WRK-TABULAR-PROVISION                  GAS1UPD 
01530                 THEN                                              GAS1UPD 
01531 ******************* MOVE HIGH-VALUES                              GAS1UPD 
01532 *******************   TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)          GAS1UPD 
01533                     IF GAA-BAMA-INTL-TAB-4 (GAA-INDEX)            GAS1UPD 
01534                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS1UPD 
01535                        MOVE WS-SPACES-ZEROS                       GAS1UPD 
01536                          TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)       GAS1UPD 
01537                     ELSE                                          GAS1UPD 
01538                        MOVE HIGH-VALUES                           GAS1UPD 
01539                          TO GAA-BAMA-INTL-TAB-5 (GAA-INDEX)       GAS1UPD 
01540                     END-IF                                        GAS1UPD 
01541                 ELSE                                              GAS1UPD 
01542                     MOVE WS-ABCODE-1BL2        TO WS-ABCODE       GAS1UPD 
01543                     MOVE WS-ABCODE-1BL2-MSG    TO WS-ABCODE-MSG   GAS1UPD 
01544                     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.        GAS1UPD 
01545                                                                   GAS1UPD 
01546      IF GAA-BAMA-INTL-TAB-5 (GAA-INDEX) = HIGH-VALUES             GAS1UPD 
01547         NEXT SENTENCE                                             GAS1UPD 
01548      ELSE                                                         GAS1UPD 
01549         SUBTRACT  1  FROM  GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX).GAS1UPD 
01550                                                                   GAS1UPD 
01551      GO TO 2200-250-UPDATE-ALL-LVL-TAB.                           GAS1UPD 
01552                                                                   GAS1UPD 
01553                                                                   GAS1UPD 
01554  2200-220-ADD-INTERNAL-OCCURS.                                    GAS1UPD 
01555                                                                   GAS1UPD 
01556 *****IF GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX) > 5                GAS1UPD 
01557      IF GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX) = 5 AND            GAS1UPD 
01558**       GAA-INT-TS (GAA-INDEX 5) NOT = HIGH-VALUES                GAS1UPD 
01558         GAA-INT-ID (GAA-INDEX 5) NOT = HIGH-VALUES                GAS1UPD 
01559         SET  WT-01-INDEX                     TO +26               GAS1UPD 
01560         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           GAS1UPD 
01561         MOVE SPACES TO  IBGROPTO, IPGPOPTO, IDGDOPTO,             GAS1UPD 
01562                         IPGTOPTO, IPGNOPTO, IPGSOPTO              GAS1UPD 
01563         MOVE SPACES TO  MFRMSLTO                                  GAS1UPD 
01564         MOVE -1 TO PERIODL                                        GAS1UPD 
01565         PERFORM 9010-000-SEND-DATAONLY-RETURN.                    GAS1UPD 
01566                                                                   GAS1UPD 
01567      MOVE GXA-PROVISION-ID          TO     WS-SAVE-INTL-TAB-ID.   GAS1UPD 
01568      MOVE GXA-PROVISION-SLOT-NO     TO     WS-TAB-PROV-COPY-SLOT. GAS1UPD 
01569      MOVE GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  TO                 GAS1UPD 
01570                                            WS-SAVE-INTL-TAB-SLOT. GAS1UPD 
01571                                                                   GAS1UPD 
01572      SET GAA-INT-INDEX  TO  1.                                    GAS1UPD 
01573      SEARCH GAA-INT-TS                                            GAS1UPD 
01574         VARYING GAA-INT-INDEX                                     GAS1UPD 
01575         AT END                                                    GAS1UPD 
01576            MOVE WS-ABCODE-1BL3      TO  WS-ABCODE                 GAS1UPD 
01577            MOVE WS-ABCODE-1BL3-MSG  TO  WS-ABCODE-MSG             GAS1UPD 
01578            PERFORM 9800-000-ERROR-MSG-THEN-ABEND                  GAS1UPD 
01579         WHEN                                                      GAS1UPD 
01580            GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  NOT <             GAS1UPD 
01581                                                 WS-SAVE-INTL-TAB  GAS1UPD 
01582            NEXT SENTENCE.                                         GAS1UPD 
01583                                                                   GAS1UPD 
01584      IF GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  NOT =                GAS1UPD 
01585                                                  WS-SAVE-INTL-TAB GAS1UPD 
01586         PERFORM 2200-225-SHIFT-OCCURS-UP                          GAS1UPD 
01587            VARYING GAA-INT-INDEX  FROM  GAA-INT-INDEX  BY  1      GAS1UPD 
01588            UNTIL GAA-INT-INDEX  >  5                              GAS1UPD 
01589         IF GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX) < 5             GAS1UPD 
01590            ADD  1  TO  GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)     GAS1UPD 
01591         END-IF                                                    GAS1UPD 
01592      ELSE                                                         GAS1UPD 
01593         MOVE WS-SAVE-INTL-TAB  TO                                 GAS1UPD 
01594                               GAA-INT-TS(GAA-INDEX GAA-INT-INDEX).GAS1UPD 
01595                                                                   GAS1UPD 
01596      GO TO 2200-240-SETUP-GCIO-PARMS.                             GAS1UPD 
01597                                                                   GAS1UPD 
01598                                                                   GAS1UPD 
01599  2200-225-SHIFT-OCCURS-UP.                                        GAS1UPD 
01600      MOVE GAA-INT-TS(GAA-INDEX GAA-INT-INDEX)  TO  WS-INTL-TAB-ID.GAS1UPD 
01601      MOVE WS-SAVE-INTL-TAB  TO                                    GAS1UPD 
01602                             GAA-INT-TS(GAA-INDEX GAA-INT-INDEX).  GAS1UPD 
01603                                                                   GAS1UPD 
01604      MOVE WS-INTL-TAB-ID  TO  WS-SAVE-INTL-TAB.                   GAS1UPD 
01605                                                                   GAS1UPD 
01606                                                                   GAS1UPD 
01607  2200-230-CHANGE-PROD-SLOT-NO.                                    GAS1UPD 
01608                                                                   GAS1UPD 
01609      MOVE GXA-PROVISION-SLOT-NO  TO  WS-TAB-PROV-COPY-SLOT.       GAS1UPD 
01610      SET  GAA-INT-INDEX TO      1.                                GAS1UPD 
01611      SET  GAA-INT-INDEX DOWN BY 1.                                GAS1UPD 
01612                                                                   GAS1UPD 
01613  2200-240-CHANGE-LOOP.                                            GAS1UPD 
01614                                                                   GAS1UPD 
01615      SET GAA-INT-INDEX UP BY 1.                                   GAS1UPD 
01616      IF  GAA-INT-INDEX > 5                                        GAS1UPD 
01617          GO TO 2200-240-SETUP-GCIO-PARMS.                         GAS1UPD 
01618                                                                   GAS1UPD 
01619      IF  GAA-INT-ID (GAA-INDEX GAA-INT-INDEX) = GXA-PROVISION-ID  GAS1UPD 
01620      THEN                                                         GAS1UPD 
01621          MOVE GAA-OCCURS-ENTRY-COUNTER (GAA-INDEX)                GAS1UPD 
01622            TO GXA-PROVISION-SLOT-NO                               GAS1UPD 
01623               GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)              GAS1UPD 
01624          GO TO 2200-240-SETUP-GCIO-PARMS                          GAS1UPD 
01625      ELSE                                                         GAS1UPD 
01626          GO TO 2200-240-CHANGE-LOOP.                              GAS1UPD 
01627                                                                   GAS1UPD 
01628                                                                   GAS1UPD 
01629  2200-240-SETUP-GCIO-PARMS.                                       GAS1UPD 
01630                                                                   GAS1UPD 
01631      IF FRMNUIDI  =  'GS3A'                                       GAS1UPD 
01632         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
01633      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS1UPD 
01634         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
01635      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
01636         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS1UPD 
01637         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS1UPD 
01638         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS1UPD 
01639                                                                   GAS1UPD 
01640      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GAS1UPD 
01641      MOVE GC-GCIO-AREA-1       TO  GCIO2-IO-AREA-TO-USE.          GAS1UPD 
01642      MOVE GXA-PROVISION-ID     TO  GCIO-WRK-TAB-PROVISION-ID.     GAS1UPD 
01643      MOVE GAA-OCCURS-ENTRY-COUNTER (GAA-INDEX)                    GAS1UPD 
01644                                  TO  GCIO-WRK-TAB-PROV-SLOT-NO    GAS1UPD 
01645                                      GXA-PROVISION-SLOT-NO.       GAS1UPD 
01646      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY                 GAS1UPD 
01647                                    WORK-RECORD-2.                 GAS1UPD 
01648      MOVE WS-TAB-PROV-COPY-SLOT  TO  WRK2-PROV-POOL-COPY-SLOT.    GAS1UPD 
01649                                                                   GAS1UPD 
01650      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
01651         MOVE GCA-BEN-PROV-ID  TO  WRK2-ALL-LEV-BEN-PROV.          GAS1UPD 
01652                                                                   GAS1UPD 
01653      MOVE GC-GCIO-ACCESS-CODE-WR  TO  GCIO2-FILE-ACCESS-CODE.     GAS1UPD 
01654                                                                   GAS1UPD 
01655      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                    GAS1UPD 
01656               GC-GCIOPARM-LEN  +   GCIO2-RECORD-LENGTH.           GAS1UPD 
01657                                                                   GAS1UPD 
01658                                                                   GAS1UPD 
01659  2200-250-UPDATE-ALL-LVL-TAB.                                     GAS1UPD 
01660                                                                   GAS1UPD 
01661 *******                                                           GAS1UPD 
01662 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS1UPD 
01663 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLE UPDATES THEGAS1UPD 
01664 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS1UPD 
01665 *******                                                           GAS1UPD 
01666 *                                                                 GAS1UPD 
01667      IF  IBGROPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS1UPD 
01668      THEN                                                         GAS1UPD 
01669          MOVE MFRMSLTI     TO  IBGRSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01670          SET  WT-01-INDEX  TO  +01                                GAS1UPD 
01671          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01672          MOVE -1           TO  IBGROPTL                           GAS1UPD 
01673          MOVE DFHBMABF     TO  IBGRSLTA,   IBGRIDA                GAS1UPD 
01674          MOVE SPACES       TO  IBGROPTI                           GAS1UPD 
01675          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS1UPD 
01676          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS1UPD 
01677          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS1UPD 
01678      IF  IDGDOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS1UPD 
01679      THEN                                                         GAS1UPD 
01680          MOVE MFRMSLTI     TO  IDGDSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01681          SET  WT-01-INDEX  TO  +23                                GAS1UPD 
01682          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01683          MOVE -1           TO  IDGDOPTL                           GAS1UPD 
01684          MOVE DFHBMABF     TO  IDGDSLTA,   IDGDIDA                GAS1UPD 
01685          MOVE SPACES       TO  IDGDOPTI                           GAS1UPD 
01686          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS1UPD 
01687          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS1UPD 
01688          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS1UPD 
01689      IF  IPGNOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS1UPD 
01690      THEN                                                         GAS1UPD 
01691          MOVE MFRMSLTI     TO  IPGNSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01692          SET  WT-01-INDEX  TO  +02                                GAS1UPD 
01693          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01694          MOVE -1           TO  IPGNOPTL                           GAS1UPD 
01695          MOVE DFHBMABF     TO  IPGNSLTA,   IPGNIDA                GAS1UPD 
01696          MOVE SPACES       TO  IPGNOPTI                           GAS1UPD 
01697          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS1UPD 
01698          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS1UPD 
01699          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS1UPD 
01700      IF  IPGPOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS1UPD 
01701      THEN                                                         GAS1UPD 
01702          MOVE MFRMSLTI     TO  IPGPSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01703          SET  WT-01-INDEX  TO  +24                                GAS1UPD 
01704          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01705          MOVE -1           TO  IPGPOPTL                           GAS1UPD 
01706          MOVE DFHBMABF     TO  IPGPSLTA,   IPGPIDA                GAS1UPD 
01707          MOVE SPACES       TO  IPGPOPTI                           GAS1UPD 
01708          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS1UPD 
01709          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS1UPD 
01710          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS1UPD 
01711      IF  IPGSOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS1UPD 
01712      THEN                                                         GAS1UPD 
01713          MOVE MFRMSLTI     TO  IPGSSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01714          SET  WT-01-INDEX  TO  +03                                GAS1UPD 
01715          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01716          MOVE -1           TO  IPGSOPTL                           GAS1UPD 
01717          MOVE DFHBMABF     TO IPGSSLTA,    IPGSIDA                GAS1UPD 
01718          MOVE SPACES       TO IPGSOPTI                            GAS1UPD 
01719          MOVE DFHBMUNP     TO MFRMSLTA                            GAS1UPD 
01720          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS1UPD 
01721          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS1UPD 
01722      IF  IPGTOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS1UPD 
01723      THEN                                                         GAS1UPD 
01724          MOVE MFRMSLTI     TO  IPGTSLTI,   ACWA-DISPLAY-LEN-7     GAS1UPD 
01725          SET  WT-01-INDEX  TO  +03                                GAS1UPD 
01726          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
01727          MOVE -1           TO  IPGTOPTL                           GAS1UPD 
01728          MOVE DFHBMABF     TO IPGTSLTA,    IPGTIDA                GAS1UPD 
01729          MOVE SPACES       TO IPGTOPTI                            GAS1UPD 
01730          MOVE DFHBMUNP     TO MFRMSLTA                            GAS1UPD 
01731          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS1UPD 
01732          GO TO 2200-252-REPLACE-INT-TAB-SLOT                      GAS1UPD 
01733      ELSE                                                         GAS1UPD 
01734          GO TO 2200-253-BYPASS-INT-TAB-SLOT.                      GAS1UPD 
01735                                                                   GAS1UPD 
01736                                                                   GAS1UPD 
01737  2200-252-REPLACE-INT-TAB-SLOT.                                   GAS1UPD 
01738      SET  GAA-INT-INDEX  TO       1.                              GAS1UPD 
01739      SET  GAA-INT-INDEX  DOWN BY  1.                              GAS1UPD 
01740                                                                   GAS1UPD 
01741  2200-252-REPLACE-LOOP.                                           GAS1UPD 
01742                                                                   GAS1UPD 
01743      SET GAA-INT-INDEX  UP BY  1.                                 GAS1UPD 
01744 *                                                                 GAS1UPD 
01745      IF  GAA-INT-INDEX  >  5                                      GAS1UPD 
01746          MOVE WS-ABCODE-1BLX       TO  WS-ABCODE                  GAS1UPD 
01747          MOVE WS-ABCODE-1BLX-MSG   TO  WS-ABCODE-MSG              GAS1UPD 
01748          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GAS1UPD 
01749                                                                   GAS1UPD 
01750      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  =  GXA-PROVISION-ID GAS1UPD 
01751      THEN                                                         GAS1UPD 
01752          MOVE ACWA-DISPLAY-LEN-7  TO                              GAS1UPD 
01753                             GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX) GAS1UPD 
01754          GO TO 2200-252-REPLACE-LOOP-END                          GAS1UPD 
01755      ELSE                                                         GAS1UPD 
01756          GO TO 2200-252-REPLACE-LOOP.                             GAS1UPD 
01757                                                                   GAS1UPD 
01758  2200-252-REPLACE-LOOP-END.                                       GAS1UPD 
01759                                                                   GAS1UPD 
01760  2200-253-BYPASS-INT-TAB-SLOT.                                    GAS1UPD 
01761 *******                                                          |GAS1UPD 
01762 * STS *----------------------------------------------------------*GAS1UPD 
01763 *******                                                           GAS1UPD 
01764                                                                   GAS1UPD 
01765                                                                   GAS1UPD 
01766      PERFORM 3000-000-UPDATE-GAA-RECORD.                          GAS1UPD 
01767                                                                   GAS1UPD 
01768 *******                                                           GAS1UPD 
01769 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS1UPD 
01770 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS1UPD 
01771 *******     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.| GAS1UPD 
01772 *                                                                 GAS1UPD 
01773      IF  DELADDI   =  'CHG/ADD'  AND  FRMNUIDI  =  'GTM1'  AND    GAS1UPD 
01774          OENTCTRI  NOT =  '0000000' AND                           GAS1UPD 
01775         (IBGROPTI  =  'MT'  OR   IPGNOPTI  =  'MT' OR             GAS1UPD 
01776          IDGDOPTI  =  'MT'  OR   IPGPOPTI  =  'MT' OR             GAS1UPD 
01777          IPGTOPTI  =  'MT'  OR   IPGSOPTI  =  'MT')               GAS1UPD 
01778      THEN                                                         GAS1UPD 
01779          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS1UPD 
01780          MOVE 'CHG/ADD'   TO  DELADDO                             GAS1UPD 
01781          MOVE SPACES      TO  COCURANO                            GAS1UPD 
01782          MOVE DFHBMASD    TO  DLOPTLTA,   DELOPTNA                GAS1UPD 
01783          PERFORM 4100-000-DISPLAY-SKELETON                        GAS1UPD 
01784      ELSE                                                         GAS1UPD 
01785          NEXT SENTENCE.                                           GAS1UPD 
01786 *******                                                         | GAS1UPD 
01787 * STS *---------------------------------------------------------* GAS1UPD 
01788 *******                                                           GAS1UPD 
01789 *                                                                 GAS1UPD 
01790      IF (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND            GAS1UPD 
01791         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND            GAS1UPD 
01792         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND            GAS1UPD 
01793         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND            GAS1UPD 
01794         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND            GAS1UPD 
01795         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)                GAS1UPD 
01796         GO TO 2200-800.                                           GAS1UPD 
01797                                                                   GAS1UPD 
01798 *******                                                           GAS1UPD 
01799 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS1UPD 
01800 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS1UPD 
01801 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS1UPD 
01802 *******                                                           GAS1UPD 
01803 *                                                                 GAS1UPD 
01804      IF  DELADDI   =  'CHG/DEL'  AND  FRMNUIDI  =  'GTM1'  AND    GAS1UPD 
01805         (IBGROPTI  =  'MT'  OR  IPGNOPTI  =  'MT'  OR             GAS1UPD 
01806          IDGDOPTI  =  'MT'  OR  IPGPOPTI  =  'MT'  OR             GAS1UPD 
01807          IPGTOPTI  =  'MT'  OR  IPGSOPTI  =  'MT')                GAS1UPD 
01808      THEN                                                         GAS1UPD 
01809          GO TO 2200-800                                           GAS1UPD 
01810      ELSE                                                         GAS1UPD 
01811          NEXT SENTENCE.                                           GAS1UPD 
01812                                                                   GAS1UPD 
01813 *******                                                           GAS1UPD 
01814 * STS *==> DELETES DURING SINGLE TABULAR SUPPORT, THERE IS NEVER  GAS1UPD 
01815 *     *     A WORKFILE INTERNAL TABULAR TO DELETE                 GAS1UPD 
01816 *******                                                           GAS1UPD 
01817 *                                                                 GAS1UPD 
01818      IF  IBGROPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01819          MOVE SPACE      TO  IBGROPTO                             GAS1UPD 
01820          MOVE '0000000'  TO  IBGRSLTO                             GAS1UPD 
01821          GO TO 2200-800.                                          GAS1UPD 
01822      IF  IDGDOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01823          MOVE SPACE      TO  IDGDOPTO                             GAS1UPD 
01824          MOVE '0000000'  TO  IDGDSLTO                             GAS1UPD 
01825          GO TO 2200-800.                                          GAS1UPD 
01826      IF  IPGNOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01827          MOVE SPACE      TO  IPGNOPTO                             GAS1UPD 
01828          MOVE '0000000'  TO  IPGNSLTO                             GAS1UPD 
01829          GO TO 2200-800.                                          GAS1UPD 
01830      IF  IPGPOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01831          MOVE SPACE      TO  IPGPOPTO                             GAS1UPD 
01832          MOVE '0000000'  TO  IPGPSLTO                             GAS1UPD 
01833          GO TO 2200-800.                                          GAS1UPD 
01834      IF  IPGTOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01835          MOVE SPACE      TO  IPGTOPTO                             GAS1UPD 
01836          MOVE '0000000'  TO  IPGTSLTO                             GAS1UPD 
01837          GO TO 2200-800.                                          GAS1UPD 
01838      IF  IPGSOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS1UPD 
01839          MOVE SPACE      TO  IPGSOPTO                             GAS1UPD 
01840          MOVE '0000000'  TO  IPGSSLTO                             GAS1UPD 
01841          GO TO 2200-800.                                          GAS1UPD 
01842                                                                   GAS1UPD 
01843 *******                                                          |GAS1UPD 
01844 * STS *----------------------------------------------------------*GAS1UPD 
01845 *******                                                           GAS1UPD 
01846 *                                                                 GAS1UPD 
01847      IF IBGROPTI  =  'D' AND  IBGRSLTI  <  '9000000'              GAS1UPD 
01848         MOVE SPACE      TO  IBGROPTO                              GAS1UPD 
01849         MOVE '0000000'  TO  IBGRSLTO                              GAS1UPD 
01850         GO TO 2200-800.                                           GAS1UPD 
01851      IF IDGDOPTI  =  'D' AND  IDGDSLTI  <  '9000000'              GAS1UPD 
01852         MOVE SPACE      TO  IDGDOPTO                              GAS1UPD 
01853         MOVE '0000000'  TO  IDGDSLTO                              GAS1UPD 
01854         GO TO 2200-800.                                           GAS1UPD 
01855      IF IPGNOPTI  =  'D' AND  IPGNSLTI  <  '9000000'              GAS1UPD 
01856         MOVE SPACE      TO  IPGNOPTO                              GAS1UPD 
01857         MOVE '0000000'  TO  IPGNSLTO                              GAS1UPD 
01858         GO TO 2200-800.                                           GAS1UPD 
01859      IF IPGPOPTI  =  'D' AND  IPGPSLTI  <  '9000000'              GAS1UPD 
01860         MOVE SPACE      TO  IPGPOPTO                              GAS1UPD 
01861         MOVE '0000000'  TO  IPGPSLTO                              GAS1UPD 
01862         GO TO 2200-800.                                           GAS1UPD 
01863      IF IPGTOPTI  =  'D' AND  IPGTSLTI  <  '9000000'              GAS1UPD 
01864         MOVE SPACE      TO  IPGTOPTO                              GAS1UPD 
01865         MOVE '0000000'  TO  IPGTSLTO                              GAS1UPD 
01866         GO TO 2200-800.                                           GAS1UPD 
01867      IF IPGSOPTI  =  'D' AND  IPGSSLTI  <  '9000000'              GAS1UPD 
01868         MOVE SPACE      TO  IPGSOPTO                              GAS1UPD 
01869         MOVE '0000000'  TO  IPGSSLTO                              GAS1UPD 
01870         GO TO 2200-800.                                           GAS1UPD 
01871 *                                                                 GAS1UPD 
01872      IF IBGROPTI  =  'D'                                          GAS1UPD 
01873         MOVE '0000000'  TO  IBGRSLTO                              GAS1UPD 
01874         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01875      IF IDGDOPTI  =  'D'                                          GAS1UPD 
01876         MOVE '0000000'  TO  IDGDSLTO                              GAS1UPD 
01877         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01878      IF IPGNOPTI  =  'D'                                          GAS1UPD 
01879         MOVE '0000000'  TO  IPGNSLTO                              GAS1UPD 
01880         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01881      IF IPGPOPTI  =  'D'                                          GAS1UPD 
01882         MOVE '0000000'  TO  IPGPSLTO                              GAS1UPD 
01883         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01884      IF IPGTOPTI  =  'D'                                          GAS1UPD 
01885         MOVE '0000000'  TO  IPGTSLTO                              GAS1UPD 
01886         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01887      IF IPGSOPTI  =  'D'                                          GAS1UPD 
01888         MOVE '0000000'  TO  IPGSSLTO                              GAS1UPD 
01889         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS1UPD 
01890                                                                   GAS1UPD 
01891                                                                   GAS1UPD 
01892  2200-260-CHANGE-INTERNAL-TAB.                                    GAS1UPD 
01893 *                                                                 GAS1UPD 
01894 *    EXEC CICS GETMAIN                                            GAS1UPD 
01895 *              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             GAS1UPD 
01896 *              INITIMG(WS-HEX-00)                                 GAS1UPD 
01897 *              LENGTH(WS-COMMUNICATION-KEY-LEN)                   GAS1UPD 
01898 *              END-EXEC.                                          GAS1UPD 
01899                                                                   GAS1UPD 
01900 *    SET ACWA-COMM-KEY-PNTR  TO                                   GAS1UPD 
01901 *                      ADDRESS OF COMMUNICATION-KEY-AREA.         GAS1UPD 
01902                                                                   GAS1UPD 
01903      SET GCA-RECORD-POINTER TO                                    GAS1UPD 
01904                     ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    GAS1UPD 
01905 *                                                                 GAS1UPD 
01906      MOVE GCIO-WRK-PLAN-CODE         TO  GCA-PLAN-CODE.           GAS1UPD 
01907      MOVE GCIO-WRK-GROUP-NUM         TO  GCA-GROUP-NUM.           GAS1UPD 
01908      MOVE GCIO-WRK-SECTION-NUM       TO  GCA-SECTION-NUM.         GAS1UPD 
01909      MOVE GCIO-WRK-PKG-CODE          TO  GCA-PKG-CODE.            GAS1UPD 
01910      MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B.               GAS1UPD 
01911      MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL.            GAS1UPD 
01912      MOVE GCIO-WRK-FAMILY-RELATION-LVL  TO                        GAS1UPD 
01913                                          GCA-FAM-REL-LVL.         GAS1UPD 
01914      MOVE GCIO-WRK-EFFDT-CEN         TO  GCA-EFFDT-CEN.           GAS1UPD 
01915 *    IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
01916 *       MOVE BEN-PROV-ID-NO          TO  GCA-BEN-PROV-ID.         GAS1UPD 
01917      MOVE TABIDI                     TO  GCA-ALL-LEVEL-TAB-ID.    GAS1UPD 
01918      MOVE TABSLTNI                   TO  ACWA-DISPLAY-LEN-7.      GAS1UPD 
01919      MOVE ACWA-DISPLAY-LEN-7         TO  GCA-ALL-LEVEL-TAB-SLOT.  GAS1UPD 
01920      MOVE FUNCTONI          TO   GCA-ALL-LEVEL-TAB-FUNC-CODE.     GAS1UPD 
01921      MOVE GXA-PROVISION-ID           TO  GCA-INTERNAL-TAB-ID.     GAS1UPD 
01922      MOVE OENTCTRI                   TO  GCA-INTERNAL-TAB-SLOT    GAS1UPD 
01923                                          GCA-OCCURS-ENTRY-COUNTER.GAS1UPD 
01924                                                                   GAS1UPD 
01925      IF  DELADDI  =  'CHG/ADD'                                    GAS1UPD 
01926          MOVE 'A'  TO  GCA-ADD-DEL-IND                            GAS1UPD 
01927      ELSE                                                         GAS1UPD 
01928          MOVE 'D'  TO  GCA-ADD-DEL-IND.                           GAS1UPD 
01929 *                                                                 GAS1UPD 
01930      MOVE GXA-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GAS1UPD 
01931      MOVE FRMNUIDI                 TO  GCA-FROM-MENU-ID.          GAS1UPD 
01932                                                                   GAS1UPD 
01933      IF IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999' OR           GAS1UPD 
01934         IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999' OR           GAS1UPD 
01935         IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999' OR           GAS1UPD 
01936         IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999' OR           GAS1UPD 
01937         IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999' OR           GAS1UPD 
01938         IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'              GAS1UPD 
01939         GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                       GAS1UPD 
01940                                                                   GAS1UPD 
01941      IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            GAS1UPD 
01942         MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                GAS1UPD 
01943         MOVE '1U'      TO  WRK2-CDE-SP                            GAS1UPD 
01944         ADD   1        TO  ACWA-CDE-1U-COUNT                      GAS1UPD 
01945      ELSE                                                         GAS1UPD 
01946         IF CDEINDO   =  ('+CDE+' OR  '+CDE-') AND                 GAS1UPD 
01947            DELADDI   =  'CHG/DEL'                                 GAS1UPD 
01948            IF INTDESKI  =  IDPRODI  AND                           GAS1UPD 
01949               WS-NEW-OCCR-ON-WF  = 'N'        THEN                GAS1UPD 
01950               MOVE '2 '   TO  WRK2-CDE-SP                         GAS1UPD 
01951               ADD   1     TO  ACWA-CDE-2B-COUNT                   GAS1UPD 
01952            ELSE                                                   GAS1UPD 
01953               MOVE '1U'   TO  WRK2-CDE-SP                         GAS1UPD 
01954               ADD   1     TO  ACWA-CDE-1U-COUNT                   GAS1UPD 
01955         ELSE                                                      GAS1UPD 
01956            IF  WS-NEW-OCCR-ON-WF = 'Y'   AND                      GAS1UPD 
01957                CDEINDO  = ('+CDE+'  OR '+CDE-')                   GAS1UPD 
01958                MOVE '1U'     TO WRK2-CDE-SP                       GAS1UPD 
01959                ADD   1       TO ACWA-CDE-1U-COUNT                 GAS1UPD 
01960            ELSE                                                   GAS1UPD 
01961                MOVE '2 '   TO  WRK2-CDE-SP                        GAS1UPD 
01962                ADD   1     TO  ACWA-CDE-2B-COUNT.                 GAS1UPD 
01963                                                                   GAS1UPD 
01964                                                                   GAS1UPD 
01965 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS1UPD 
01966      MOVE '1'          TO  GCIO2-OPER-ID-IND.                     GAS1UPD 
01967 *                                                                 GAS1UPD 
01968      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
01969                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS1UPD 
01970                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS1UPD 
01971                                                                   GAS1UPD 
01972      IF NOT GCIO2-GOOD-RETURN                                     GAS1UPD 
01973         MOVE WS-ABCODE-1BF9       TO  WS-ABCODE                   GAS1UPD 
01974         MOVE WS-ABCODE-1BF9-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
01975         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
01976                                                                   GAS1UPD 
01977      GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                          GAS1UPD 
01978                                                                   GAS1UPD 
01979                                                                   GAS1UPD 
01980  2200-270-DELETE-INTERNAL-TAB.                                    GAS1UPD 
01981 *                                                                 GAS1UPD 
01982      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS1UPD 
01983         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS1UPD 
01984           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS1UPD 
01985           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS1UPD 
01986                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS1UPD 
01987         EXEC CICS GETMAIN                                         GAS1UPD 
01988                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS1UPD 
01989                INITIMG(WS-HEX-00)                                 GAS1UPD 
01990                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS1UPD 
01991                END-EXEC                                           GAS1UPD 
01992         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS1UPD 
01993                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS1UPD 
01994                                                                   GAS1UPD 
01995      IF FRMNUIDI  =  'GS3A'                                       GAS1UPD 
01996         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
01997      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS1UPD 
01998         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
01999      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
02000         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS1UPD 
02001         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS1UPD 
02002         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS1UPD 
02003                                                                   GAS1UPD 
02004      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS1UPD 
02005      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS1UPD 
02006      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS1UPD 
02007      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS1UPD 
02008 *                                                                 GAS1UPD 
02009      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS1UPD 
02010           TO  GXA-ENTRY-COUNT.                                    GAS1UPD 
02011                                                                   GAS1UPD 
02012      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02013                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS1UPD 
02014                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS1UPD 
02015                                                                   GAS1UPD 
02016      IF NOT GCIO2-GOOD-RETURN                                     GAS1UPD 
02017         MOVE WS-ABCODE-1BF5       TO  WS-ABCODE                   GAS1UPD 
02018         MOVE WS-ABCODE-1BF5-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02019         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02020                                                                   GAS1UPD 
02021      MOVE GC-GCIO-ACCESS-CODE-DL TO  GCIO2-FILE-ACCESS-CODE.      GAS1UPD 
02022      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02023                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS1UPD 
02024                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS1UPD 
02025                                                                   GAS1UPD 
02026      IF NOT GCIO2-GOOD-RETURN                                     GAS1UPD 
02027         MOVE WS-ABCODE-1BFA       TO  WS-ABCODE                   GAS1UPD 
02028         MOVE WS-ABCODE-1BFA-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02029         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02030                                                                   GAS1UPD 
02031      IF WRK2-CDE-SP =  '1U'                                       GAS1UPD 
02032         SUBTRACT  1  FROM  ACWA-CDE-1U-COUNT.                     GAS1UPD 
02033                                                                   GAS1UPD 
02034      IF WRK2-CDE-SP  =  '2 '                                      GAS1UPD 
02035         SUBTRACT  1  FROM  ACWA-CDE-2B-COUNT.                     GAS1UPD 
02036 *                                                                 GAS1UPD 
02037      MOVE SPACES  TO  IBGROPTO,  IPGNOPTO,  IPGTOPTO              GAS1UPD 
02038                       IDGDOPTO,  IPGPOPTO   IPGSOPTO.             GAS1UPD 
02039      GO TO 2200-800.                                              GAS1UPD 
02040                                                                   GAS1UPD 
02041                                                                   GAS1UPD 
02042  2200-280-XCTL-TO-INT-TAB-PGM.                                    GAS1UPD 
02043                                                                   GAS1UPD 
02044      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS1UPD 
02045         DELADDI  =  'CHG/DEL' AND                                 GAS1UPD 
02046         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS1UPD 
02047         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS1UPD 
02048         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS1UPD 
02049                                                                   GAS1UPD 
02050      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS1UPD 
02051         ACWA-CDE-2B-COUNT  =  ZERO                                GAS1UPD 
02052         NEXT SENTENCE                                             GAS1UPD 
02053      ELSE                                                         GAS1UPD 
02054         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS1UPD 
02055                                                                   GAS1UPD 
02056      MOVE INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.       GAS1UPD 
02057      MOVE 'GAS1UPD' TO DELADD-OPTION.                             GAS1UPD 
02058 *                                                                 GAS1UPD 
02059      EXEC CICS  XCTL  PROGRAM (WS-INTERNAL-TABULAR-PGM-ID)        GAS1UPD 
02060                 COMMAREA(DFHCOMMAREA)                             GAS1UPD 
02061                 LENGTH  (LENGTH OF DFHCOMMAREA)                   GAS1UPD 
02062                 END-EXEC.                                         GAS1UPD 
02063  2200-800.                                                        GAS1UPD 
02064                                                                   GAS1UPD 
02065      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS1UPD 
02066         DELADDI  =  'CHG/DEL' AND                                 GAS1UPD 
02067         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS1UPD 
02068         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS1UPD 
02069         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS1UPD 
02070                                                                   GAS1UPD 
02071      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS1UPD 
02072         ACWA-CDE-2B-COUNT  =  ZERO                                GAS1UPD 
02073         NEXT SENTENCE                                             GAS1UPD 
02074      ELSE                                                         GAS1UPD 
02075         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS1UPD 
02076                                                                   GAS1UPD 
02077      MOVE -1  TO  PERIODL.                                        GAS1UPD 
02078      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS1UPD 
02079                                                                   GAS1UPD 
02080  2200-900-EXIT. EXIT.                                             GAS1UPD 
02081 /*****************************************************************GAS1UPD 
02082 *           D E L E T E   T H I S   O C C U R A N C E            *GAS1UPD 
02083 *    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *GAS1UPD 
02084 *  SCREEN'S DISPLAY AND REMOVE THAT ENTRY FROM THE ALL LEVEL     *GAS1UPD 
02085 *  TABULAR RECORD, INCLUDED WITH THAT IS CODE TO DELETE ANY      *GAS1UPD 
02086 *  INTERNAL TABULAR ENTRIES THAT MIGHT BE SPECIFIED BY THAT ENTRY*GAS1UPD 
02087 ******************************************************************GAS1UPD 
02088  2400-000-DELETE-THIS-OCCURANCE SECTION.                          GAS1UPD 
02089  2400-010.                                                        GAS1UPD 
02090                                                                   GAS1UPD 
02091      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS1UPD 
02092                                                                   GAS1UPD 
02093      IF NOT GCIO-GOOD-RETURN                                      GAS1UPD 
02094         MOVE WS-ABCODE-1BFB       TO  WS-ABCODE                   GAS1UPD 
02095         MOVE WS-ABCODE-1BFB-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02096         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02097                                                                   GAS1UPD 
02098      IF FRMNUIDI  =  'GS3A'                                       GAS1UPD 
02099         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
02100      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS1UPD 
02101         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS1UPD 
02102      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
02103         MOVE 'C6'  TO  GCIO-WRK-RECORD-TYPE                       GAS1UPD 
02104         MOVE GCIO-WRK-TABULAR-PROVISION                           GAS1UPD 
02105                    TO  GCIO-WRK-BENEFIT-PROVISION.                GAS1UPD 
02106                                                                   GAS1UPD 
02107      MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   GAS1UPD 
02108      MOVE OENTCTRI         TO  ACWA-DISPLAY-LEN-7.                GAS1UPD 
02109 *                                                                 GAS1UPD 
02110         EXEC CICS GETMAIN                                         GAS1UPD 
02111                SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            GAS1UPD 
02112                INITIMG(WS-HEX-00)                                 GAS1UPD 
02113                LENGTH(WS-COPY-TABLE-LEN)                          GAS1UPD 
02114                END-EXEC.                                          GAS1UPD 
02115                                                                   GAS1UPD 
02116         SET ACWA-COPY-TAB-PNTR        TO                          GAS1UPD 
02117                  ADDRESS OF COPY-TABULAR-TABLE-AREA.              GAS1UPD 
02118                                                                   GAS1UPD 
02119      SET GAA-INDEX,  COPY-IDX  TO  1.                             GAS1UPD 
02120                                                                   GAS1UPD 
02121                                                                   GAS1UPD 
02122  2400-100-COPY-SAVED-AND-DELETE.                                  GAS1UPD 
02123                                                                   GAS1UPD 
02124      IF  GAA-INDEX  <  GAA-ENTRY-COUNT                            GAS1UPD 
02125      THEN                                                         GAS1UPD 
02126          IF  GAA-OCCURS-ENTRY-COUNTER (GAA-INDEX)  NOT =          GAS1UPD 
02127              ACWA-DISPLAY-LEN-7                                   GAS1UPD 
02128          THEN                                                     GAS1UPD 
02129              MOVE GAA-ENTRY          (GAA-INDEX)                  GAS1UPD 
02130                TO COPY-TABULAR-TABLE (COPY-IDX)                   GAS1UPD 
02131              SET  GAA-INDEX,  COPY-IDX  UP BY  1                  GAS1UPD 
02132              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS1UPD 
02133          ELSE                                                     GAS1UPD 
02134 *---- WE ARE NOT GOING TO COPY THE ENTRY THAT IS BEING DELETED.   GAS1UPD 
02135 *---- BUT WE SAVE IT SINCE WE HAVE TO DELETE ANY INTERNAL TABULARSGAS1UPD 
02136              MOVE GAA-ENTRY (GAA-INDEX)  TO  WS-ENTRY             GAS1UPD 
02137              SET  COPY-IDX3  TO  GAA-INDEX                        GAS1UPD 
02138              SET  GAA-INDEX  UP BY  1                             GAS1UPD 
02139              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS1UPD 
02140      ELSE                                                         GAS1UPD 
02141          MOVE GAA-ENTRY          (GAA-INDEX)                      GAS1UPD 
02142            TO COPY-TABULAR-TABLE (COPY-IDX).                      GAS1UPD 
02143                                                                   GAS1UPD 
02144      SET  GAA-ENTRY-COUNT  TO  COPY-IDX.                          GAS1UPD 
02145      MOVE GAA-ENTRY-COUNT  TO  GAA-ENTRY-COUNT.                   GAS1UPD 
02146      SET COPY-IDX,  GAA-INDEX  TO  1.                             GAS1UPD 
02147                                                                   GAS1UPD 
02148                                                                   GAS1UPD 
02149  2400-200-MOVE-UPDATED-TABLE.                                     GAS1UPD 
02150                                                                   GAS1UPD 
02151      IF GAA-INDEX  NOT >  GAA-ENTRY-COUNT                         GAS1UPD 
02152         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GAS1UPD 
02153           TO GAA-ENTRY          (GAA-INDEX)                       GAS1UPD 
02154         SET GAA-INDEX,  COPY-IDX  UP BY  1                        GAS1UPD 
02155         GO TO 2400-200-MOVE-UPDATED-TABLE.                        GAS1UPD 
02156                                                                   GAS1UPD 
02157 *=== LOGICAL FIX FOR NON-CDE CONTRACT, GROUP SPC AND BENEFIT      GAS1UPD 
02158 *=== PROVISION AS A RESULT OF MAPING OR OTHERWISE- ANY ACCUM      GAS1UPD 
02159 *=== ATTACHED SHOULD NOT PASS THROUGH CDE LOGIC.  06/02/88 NE     GAS1UPD 
02160                                                                   GAS1UPD 
02161      IF CDEINDO  =  '+CDE+'   OR  '+CDE-'                         GAS1UPD 
02162         PERFORM   4600-000-UPDATE-CDE-STATUS.                     GAS1UPD 
02163                                                                   GAS1UPD 
02164 *============ NGE 6/02/88  D1218 ===============================  GAS1UPD 
02165                                                                   GAS1UPD 
02166      PERFORM 3000-000-UPDATE-GAA-RECORD.                          GAS1UPD 
02167                                                                   GAS1UPD 
02168      SET GAA-INDEX  TO  GAA-ENTRY-COUNT.                          GAS1UPD 
02169      MOVE WS-ENTRY  TO  GAA-ENTRY (GAA-INDEX).                    GAS1UPD 
02170                                                                   GAS1UPD 
02171      IF GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)  =  1              GAS1UPD 
02172         GO TO 2400-340-DELETE-LOOP-END.                           GAS1UPD 
02173                                                                   GAS1UPD 
02174                                                                   GAS1UPD 
02175 *---- DELETE INTERNAL TABULARS FROM W/F THAT ARE ATTACHED TO      GAS1UPD 
02176 *       ALL LVL TAB OCCURANCE BEING DELETED                       GAS1UPD 
02177  2400-300-DELETE-INTERNAL-TABS.                                   GAS1UPD 
02178      SET GAA-INT-INDEX  TO       1.                               GAS1UPD 
02179      SET GAA-INT-INDEX  DOWN BY  1.                               GAS1UPD 
02180                                                                   GAS1UPD 
02181  2400-320-DELETE-LOOP.                                            GAS1UPD 
02182                                                                   GAS1UPD 
02183      SET GAA-INT-INDEX UP BY 1.                                   GAS1UPD 
02184      IF  GAA-INT-INDEX >  5                                       GAS1UPD 
02185          GO TO 2400-340-DELETE-LOOP-END.                          GAS1UPD 
02186                                                                   GAS1UPD 
02187      IF GAA-INT-ID (GAA-INDEX GAA-INT-INDEX)  =  HIGH-VALUES      GAS1UPD 
02188         GO TO 2400-340-DELETE-LOOP-END.                           GAS1UPD 
02189                                                                   GAS1UPD 
02190 ************   09/14/88  NE                                       GAS1UPD 
02191      IF  GAA-INT-SLOT (GAA-INDEX GAA-INT-INDEX)  >  8999999       GAS1UPD 
02192          NEXT SENTENCE                                            GAS1UPD 
02193      ELSE                                                         GAS1UPD 
02194          GO TO  2400-320-DELETE-LOOP.                             GAS1UPD 
02195                                                                   GAS1UPD 
02196 *                                                                 GAS1UPD 
02197      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS1UPD 
02198         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS1UPD 
02199           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS1UPD 
02200           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS1UPD 
02201                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS1UPD 
02202         EXEC CICS GETMAIN                                         GAS1UPD 
02203                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS1UPD 
02204                INITIMG(WS-HEX-00)                                 GAS1UPD 
02205                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS1UPD 
02206                END-EXEC                                           GAS1UPD 
02207         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS1UPD 
02208                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS1UPD 
02209                                                                   GAS1UPD 
02210      MOVE GAA-INT-TS (GAA-INDEX GAA-INT-INDEX)                    GAS1UPD 
02211                                  TO GCIO-WRK-TABULAR-PROVISION.   GAS1UPD 
02212      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS1UPD 
02213      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS1UPD 
02214      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS1UPD 
02215      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS1UPD 
02216 *                                                                 GAS1UPD 
02217      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS1UPD 
02218           TO  GXA-ENTRY-COUNT.                                    GAS1UPD 
02219                                                                   GAS1UPD 
02220      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02221                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS1UPD 
02222                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS1UPD 
02223      IF NOT GCIO2-GOOD-RETURN                                     GAS1UPD 
02224         MOVE WS-ABCODE-1BF5       TO  WS-ABCODE                   GAS1UPD 
02225         MOVE WS-ABCODE-1BF5-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02226         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02227                                                                   GAS1UPD 
02228                                                                   GAS1UPD 
02229      MOVE GC-GCIO-ACCESS-CODE-DL TO GCIO2-FILE-ACCESS-CODE.       GAS1UPD 
02230      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02231                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS1UPD 
02232                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS1UPD 
02233                                                                   GAS1UPD 
02234      IF NOT GCIO2-GOOD-RETURN                                     GAS1UPD 
02235         MOVE WS-ABCODE-1BFC       TO  WS-ABCODE                   GAS1UPD 
02236         MOVE WS-ABCODE-1BFC-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02237         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02238                                                                   GAS1UPD 
02239      IF WRK2-CDE-SP  =  '1U'                                      GAS1UPD 
02240         SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT.                      GAS1UPD 
02241      IF WRK2-CDE-SP  =  '2 '                                      GAS1UPD 
02242         SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT.                      GAS1UPD 
02243                                                                   GAS1UPD 
02244      GO TO 2400-320-DELETE-LOOP.                                  GAS1UPD 
02245                                                                   GAS1UPD 
02246                                                                   GAS1UPD 
02247  2400-340-DELETE-LOOP-END.                                        GAS1UPD 
02248      PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      GAS1UPD 
02249                                                                   GAS1UPD 
02250  2400-400-DISPLAY-SCREEN.                                         GAS1UPD 
02251                                                                   GAS1UPD 
02252      IF COPY-IDX3  =  GAA-ENTRY-COUNT AND  =  1                   GAS1UPD 
02253         MOVE 'CHG/ADD'     TO  DELADDO                            GAS1UPD 
02254         MOVE SPACES        TO  DELOPTNO,   DELOLITO               GAS1UPD 
02255         MOVE DFHBMASD      TO  DLOPTLTA,   DELOPTNA               GAS1UPD 
02256         SET  WT-01-INDEX   TO  +19                                GAS1UPD 
02257         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO         GAS1UPD 
02258         PERFORM 4100-000-DISPLAY-SKELETON.                        GAS1UPD 
02259                                                                   GAS1UPD 
02260      IF  COPY-IDX3  <  GAA-ENTRY-COUNT                            GAS1UPD 
02261      THEN                                                         GAS1UPD 
02262          SET GAA-INDEX  TO  COPY-IDX3                             GAS1UPD 
02263          MOVE SPACES    TO  ERRMSGO                               GAS1UPD 
02264          PERFORM 4400-000-BUILD-DISPLAY                           GAS1UPD 
02265      ELSE                                                         GAS1UPD 
02266          SET  GAA-INDEX     TO  1                                 GAS1UPD 
02267          SET  WT-01-INDEX   TO  +14                               GAS1UPD 
02268          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS1UPD 
02269          PERFORM 4400-000-BUILD-DISPLAY.                          GAS1UPD 
02270                                                                   GAS1UPD 
02271      MOVE SPACES  TO  DELOPTNO.                                   GAS1UPD 
02272      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS1UPD 
02273                                                                   GAS1UPD 
02274  2400-900-EXIT. EXIT.                                             GAS1UPD 
02275 /     P R O C E S S   V A L   L I M I T                           GAS1UPD 
02276  2600-000-PROCESS-VAL-LIMIT     SECTION.                          GAS1UPD 
02277  2600-010.                                                        GAS1UPD 
02278                                                                   GAS1UPD 
02279      IF (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'NEG' OR                   GAS1UPD 
02280          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 GAS1UPD 
02279         (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'UNL' OR                   GAS1UPD 
02280          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    GAS1UPD 
02281          GO TO 2600-900-EXIT.                                     GAS1UPD 
02282                                                                   GAS1UPD 
02283      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS1UPD 
02284      THEN                                                         GAS1UPD 
02285          IF  BENVLQLI  =  '5'                                     GAS1UPD 
02286          THEN                                                     GAS1UPD 
02287              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         GAS1UPD 
02288              MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         GAS1UPD 
02289              GO TO 2600-900-EXIT                                  GAS1UPD 
02290          ELSE                                                     GAS1UPD 
02291              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       GAS1UPD 
02292              GO TO 2600-900-EXIT                                  GAS1UPD 
02293      ELSE                                                         GAS1UPD 
02294          NEXT SENTENCE.                                           GAS1UPD 
02295                                                                   GAS1UPD 
02296      IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            GAS1UPD 
02297          MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       GAS1UPD 
02298          MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       GAS1UPD 
02299          GO TO 2600-900-EXIT.                                     GAS1UPD 
02300                                                                   GAS1UPD 
02301  2600-900-EXIT. EXIT.                                             GAS1UPD 
02302                                                                   GAS1UPD 
02303 /*****************************************************************GAS1UPD 
02304 * 3000 UPDATE GAA RECORD                                         *GAS1UPD 
02305 *                                                                *GAS1UPD 
02306 *    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *GAS1UPD 
02307 ******************************************************************GAS1UPD 
02308  3000-000-UPDATE-GAA-RECORD     SECTION.                          GAS1UPD 
02309  3000-010.                                                        GAS1UPD 
02310                                                                   GAS1UPD 
02311      COMPUTE  GCIO-RECORD-LENGTH   =                              GAS1UPD 
02312          GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ABM-FIXED-LEN  +     GAS1UPD 
02313          (GC-GCTABULR-ABM-VARY-LEN * GAA-ENTRY-COUNT).            GAS1UPD 
02314                                                                   GAS1UPD 
02315 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS1UPD 
02316      MOVE '1'                      TO  GCIO-OPER-ID-IND.          GAS1UPD 
02317      MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     GAS1UPD 
02318                                                                   GAS1UPD 
02319      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02320                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS1UPD 
02321                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS1UPD 
02322                                                                   GAS1UPD 
02323      IF NOT GCIO-GOOD-RETURN                                      GAS1UPD 
02324         MOVE WS-ABCODE-1BF4       TO  WS-ABCODE                   GAS1UPD 
02325         MOVE WS-ABCODE-1BF4-MSG   TO  WS-ABCODE-MSG               GAS1UPD 
02326         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02327                                                                   GAS1UPD 
02328  3000-900-EXIT. EXIT.                                             GAS1UPD 
02329                                                                   GAS1UPD 
02330 ******************************************************************GAS1UPD 
02331 * 3100  UNLOCK THE GAA ACCUM TAB RECORD READ EARLIER FOR UPDATE   GAS1UPD 
02332 ******************************************************************GAS1UPD 
02333  3100-RLSE-RU-GAA-REC SECTION.                                    GAS1UPD 
02334                                                                   GAS1UPD 
02335      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO-FILE-ACCESS-CODE.     GAS1UPD 
02336                                                                   GAS1UPD 
02337      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GAS1UPD 
02338                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS1UPD 
02339                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS1UPD 
02340                                                                   GAS1UPD 
02341      IF NOT GCIO-GOOD-RETURN                                      GAS1UPD 
02342         MOVE WS-ABCODE-1BF4        TO  WS-ABCODE                  GAS1UPD 
02343         MOVE WS-ABCODE-1BF4-MSG    TO  WS-ABCODE-MSG              GAS1UPD 
02344         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02345  3199-EXIT.     EXIT.                                             GAS1UPD 
02346                                                                   GAS1UPD 
02347 /*****************************************************************GAS1UPD 
02348 * 3200  READ REC FOR UPDATE                                      *GAS1UPD 
02349 *                                                                *GAS1UPD 
02350 *    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *GAS1UPD 
02351 *  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *GAS1UPD 
02352 ******************************************************************GAS1UPD 
02353  3200-000-READ-REC-FOR-UPDATE   SECTION.                          GAS1UPD 
02354  3200-010.                                                        GAS1UPD 
02355                                                                   GAS1UPD 
02356      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN   +  GAS1UPD 
02357            GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ABM-FIXED-LEN   +  GAS1UPD 
02358            (GC-GCTABULR-ABM-VARY-LEN   *                          GAS1UPD 
02359                                  GC-GCTABULR-ABM-VARY-MAX-OCUR).  GAS1UPD 
02360 *                                                                 GAS1UPD 
02361      IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS1UPD 
02362         NEXT SENTENCE                                             GAS1UPD 
02363      ELSE                                                         GAS1UPD 
02364         EXEC CICS GETMAIN                                         GAS1UPD 
02365                SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      GAS1UPD 
02366                INITIMG(WS-HEX-00)                                 GAS1UPD 
02367                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS1UPD 
02368                END-EXEC                                           GAS1UPD 
02369         SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     GAS1UPD 
02370                  ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        GAS1UPD 
02371                                                                   GAS1UPD 
02372                                                                   GAS1UPD 
02373      IF FRMNUIDI  =  'GS3A'                                       GAS1UPD 
02374         PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    GAS1UPD 
02375      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS1UPD 
02376         PERFORM 6100-000-BUILD-CONTRACT-KEY.                      GAS1UPD 
02377      IF FRMNUIDI  =  'GC8A'                                       GAS1UPD 
02378         PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      GAS1UPD 
02379                                                                   GAS1UPD 
02380      MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           GAS1UPD 
02381      MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        GAS1UPD 
02382      MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              GAS1UPD 
02383      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      GAS1UPD 
02384 *                                                                 GAS1UPD 
02385      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO  GAA-ENTRY-COUNT.     GAS1UPD 
02386                                                                   GAS1UPD 
02387      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS1UPD 
02388                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS1UPD 
02389                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS1UPD 
02390                                                                   GAS1UPD 
02391  3200-900-EXIT. EXIT.                                             GAS1UPD 
02392 /*****************************************************************GAS1UPD 
02393 * 4100  DISPLAY SKELETON                                         *GAS1UPD 
02394 *                                                                *GAS1UPD 
02395 *    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *GAS1UPD 
02396 *  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *GAS1UPD 
02397 *  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *GAS1UPD 
02398 ******************************************************************GAS1UPD 
02399  4100-000-DISPLAY-SKELETON      SECTION.                          GAS1UPD 
02400  4100-010.                                                        GAS1UPD 
02401                                                                   GAS1UPD 
02402      MOVE SPACES    TO  ERRMSGO.                                  GAS1UPD 
02403                                                                   GAS1UPD 
02404      MOVE DFHBMFSE  TO  PERIODA.                                  GAS1UPD 
02405 *                                                                 GAS1UPD 
02406      MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         GAS1UPD 
02407                         IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     GAS1UPD 
02408                         IDGDOPTA IPGPOPTA  IPGSOPTA.              GAS1UPD 
02409                                                                   GAS1UPD 
02410      MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   GAS1UPD 
02411                        FAMINDIO  PLCTRMTO.                        GAS1UPD 
02412 *                                                                 GAS1UPD 
02413      MOVE LOW-VALUES  TO  INTDESKO  IBGROPTO  IPGNOPTO  IDGDOPTO  GAS1UPD 
02414                           IPGTOPTO  MFRMSLTO  IPGPOPTO  IPGSOPTO. GAS1UPD 
02415                                                                   GAS1UPD 
02416      MOVE ZEROS  TO  COPAYINO  CSTCONTO  PERTQALO                 GAS1UPD 
02417            DAYFACIO  SRVGRUPO  PRTIMEFO  MAXOVRDO   CONDTMJO      GAS1UPD 
02418            REININDO  CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO       GAS1UPD 
02419            FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO   CONDINFO      GAS1UPD 
02420            CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       GAS1UPD 
02421            CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO       GAS1UPD 
02422            CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO       GAS1UPD 
02423            CONDEMCO  CONDEACO  CONDSMIO  CONDNSMO                 GAS1UPD 
02424            PRTIMEFO  INTRVALO  CONDPECO  CONDNEMO  NEWVALUO       GAS1UPD 
02425            OENTCTRO  IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO       GAS1UPD 
02426            AGEQLLO   AGEQLHO   CONDLIFO  IPGSSLTO                 GAS1UPD 
02427            RELPINDO  AGELIMLO  AGELIMHO IDGDSLTO  IPGPSLTO        GAS1UPD 
02428 *          FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO.                 GAS1UPD 
02428            FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO                  GAS1UPD 
                 BISNDINO  BENTYPO   TIERCDO  TIERLVO.                          
02429                                                                   GAS1UPD 
02430      MOVE '01'    TO  COCURANO.                                   GAS1UPD 
02431      MOVE -1      TO  PERIODL.                                    GAS1UPD 
02432                                                                   GAS1UPD 
02433      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS1UPD 
02434                                                                   GAS1UPD 
02435  4100-900-EXIT. EXIT.                                             GAS1UPD 
02436 /*****************************************************************GAS1UPD 
02437 * 4400 BUILD DISPLAY                                             *GAS1UPD 
02438 *                                                                *GAS1UPD 
02439 *    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *GAS1UPD 
02440 *  SPECIFIED BY INDEX GAA-INDEX TO THE SCREEN.                   *GAS1UPD 
02441 ******************************************************************GAS1UPD 
02442  4400-000-BUILD-DISPLAY         SECTION.                          GAS1UPD 
02443  4400-010.                                                        GAS1UPD 
02444                                                                   GAS1UPD 
02445      IF  DELADDI  =  'CHG/DEL'                                    GAS1UPD 
02446      THEN                                                         GAS1UPD 
02447          MOVE 'D'  TO  DELOLITO                                   GAS1UPD 
02448      ELSE                                                         GAS1UPD 
02449          MOVE SPACE  TO  DELOLITO.                                GAS1UPD 
02450                                                                   GAS1UPD 
02451      MOVE SPACE                                       TO DELOPTNO.GAS1UPD 
02452      MOVE GAA-OCCURS-ENTRY-COUNTER     (GAA-INDEX)  TO            GAS1UPD 
02453                                                ACWA-DISPLAY-LEN-7.GAS1UPD 
02454      MOVE ACWA-DISPLAY-LEN-7                        TO  OENTCTRO. GAS1UPD 
02455      MOVE GAA-BAMA-DAY-FACTOR-IND      (GAA-INDEX)  TO  DAYFACIO. GAS1UPD 
02456      MOVE GAA-BAMA-CO-PAY-IND          (GAA-INDEX)  TO  COPAYINO. GAS1UPD 
02457      MOVE GAA-BAMA-COST-CONTAIN-IND    (GAA-INDEX)  TO  CSTCONTO. GAS1UPD 
02458      MOVE GAA-BAMA-BENEFIT-PERIOD      (GAA-INDEX)  TO  PERIODO.  GAS1UPD 
02459      MOVE GAA-BAMA-DEFINITION          (GAA-INDEX)  TO  DEFINTNO. GAS1UPD 
02460      MOVE GAA-BAMA-BEN-PER-TIME-QUAL   (GAA-INDEX)  TO  PERTQALO. GAS1UPD 
02461      MOVE GAA-BAMA-FAM-OR-INDIV        (GAA-INDEX)  TO  FAMINDIO. GAS1UPD 
02462      MOVE GAA-BAMA-PLACE-OF-TREATMENT  (GAA-INDEX)  TO  PLCTRMTO. GAS1UPD 
02463      MOVE GAA-BAMA-SERVICE-GROUP       (GAA-INDEX)  TO  SRVGRUPO. GAS1UPD 
02464                                                                   GAS1UPD 
02465      MOVE GAA-BAMA-AGE-QUAL-IND-FROM   (GAA-INDEX)  TO  AGEQLLO.  GAS1UPD 
02466      MOVE GAA-BAMA-AGE-QUAL-IND-TO     (GAA-INDEX)  TO  AGEQLHO.  GAS1UPD 
02467      MOVE GAA-BAMA-RELATIONSHIP-IND    (GAA-INDEX)  TO  RELPINDO. GAS1UPD 
02468      MOVE GAA-BAMA-AGE-LIMIT-FROM      (GAA-INDEX)  TO            GAS1UPD 
02469                                                ACWA-DISPLAY-LEN-3.GAS1UPD 
02470      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMLO. GAS1UPD 
02471      MOVE GAA-BAMA-AGE-LIMIT-TO        (GAA-INDEX)  TO            GAS1UPD 
02472                                                ACWA-DISPLAY-LEN-3.GAS1UPD 
02473      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMHO. GAS1UPD 
02474      MOVE GAA-BAMA-BEN-PER-TIME-FCTR   (GAA-INDEX)  TO            GAS1UPD 
02475                                                ACWA-DISPLAY-LEN-3.GAS1UPD 
02476      MOVE ACWA-DISPLAY-LEN-3                        TO  PRTIMEFO. GAS1UPD 
02477      MOVE GAA-BAMA-FEAK-IND            (GAA-INDEX)  TO  FEAKINDO. GAS1UPD 
02477      MOVE GAA-BAMA-ASCEND-DESCEND-IND  (GAA-INDEX)  TO  ASCDSCDO. GAS1UPD 
      **P21595 CHANGES STARTS                                                   
02477      MOVE GAA-BAMA-BEN-TYPE            (GAA-INDEX)  TO  BENTYPO.  GAS1UPD 
02477      MOVE GAA-BAMA-TIER-CODE           (GAA-INDEX)  TO  TIERCDO.  GAS1UPD 
02477      MOVE GAA-BAMA-TIER-LVL            (GAA-INDEX)  TO  TIERLVO.  GAS1UPD 
      **P21595 CHANGES ENDS                                                     
02477      MOVE GAA-BAMA-BISCENDING-IND-RSV  (GAA-INDEX)  TO  BISNDINO. GAS1UPD 
02478      MOVE GAA-BAMA-ACCUMID             (GAA-INDEX)  TO  ACCUMIDO. GAS1UPD 
02479      MOVE GAA-BAMA-COMB-APPLIED-IND    (GAA-INDEX)  TO  CAPINDO.  GAS1UPD 
02480      MOVE GAA-BAMA-SEL-ADDL-BEN-DET    (GAA-INDEX)  TO  SABDINDO. GAS1UPD 
02481      MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND(GAA-INDEX)  TO  MAXOVRDO. GAS1UPD 
02482      MOVE GAA-BAMA-REINSTATEMENT-IND   (GAA-INDEX)  TO  REININDO. GAS1UPD 
02483      MOVE GAA-BAMA-CLAIM-LVL-ACCUM-IND (GAA-INDEX)  TO  CLMLVLIO. GAS1UPD 
02484      MOVE GAA-BAMA-INTERVAL-TIME-FCTR  (GAA-INDEX)  TO            GAS1UPD 
02485                                                ACWA-DISPLAY-LEN-3.GAS1UPD 
02486      MOVE ACWA-DISPLAY-LEN-3                        TO  INTRVALO. GAS1UPD 
02487      MOVE GAA-BAMA-INTERVAL-TYPE       (GAA-INDEX)  TO  INTTYPEO. GAS1UPD 
02488      MOVE GAA-BAMA-L-O-B               (GAA-INDEX)  TO  LOBO.     GAS1UPD 
02489      MOVE GAA-BAMA-VALUE-LIMIT         (GAA-INDEX)  TO            GAS1UPD 
02490                                                ACWA-VALUE-LIMIT-9.GAS1UPD 
02491      IF  ACWA-VALUE-LIMIT-9-9 = -1                                GAS1UPD 
02492      THEN                                                         GAS1UPD 
02493          MOVE 'NEG' TO BNMXVALO                                   GAS1UPD 
02494      ELSE                                                         GAS1UPD 
02491      IF  ACWA-VALUE-LIMIT-9-9 = -2                                GAS1UPD 
02492      THEN                                                         GAS1UPD 
02493          MOVE 'UNL' TO BNMXVALO                                   GAS1UPD 
02494      ELSE                                                         GAS1UPD 
02495          IF  GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'           GAS1UPD 
02496          THEN                                                     GAS1UPD 
02497              MOVE ACWA-VALUE-LIMIT-9     TO  ACWA-EDIT-VALUE-LIMITGAS1UPD 
02498              MOVE ACWA-EDIT-VALUE-LIMIT  TO  BNMXVALO             GAS1UPD 
02499          ELSE                                                     GAS1UPD 
02500              MOVE GAA-BAMA-VALUE-LIMIT(GAA-INDEX)  TO             GAS1UPD 
02501                                               ACWA-DISPLAY-LEN-9-2GAS1UPD 
02502              MOVE ACWA-DISPLAY-LEN-9-X   TO  ACWA-DISPLAY-9       GAS1UPD 
02503              MOVE SPACES                 TO  ACWA-DISPLAY-1       GAS1UPD 
02504              MOVE ACWA-DISPLAY-VALUE-LIMIT  TO  BNMXVALO.         GAS1UPD 
02505                                                                   GAS1UPD 
02506      MOVE GAA-BAMA-VALUE-QUALIFIER    (GAA-INDEX)  TO  BENVLQLO.  GAS1UPD 
02507      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE(GAA-INDEX)  TO             GAS1UPD 
02508                                                ACWA-DISPLAY-LEN-5.GAS1UPD 
02509      MOVE ACWA-DISPLAY-LEN-5                       TO  NEWVALUO.  GAS1UPD 
02510      MOVE GAA-BAMA-INTERVAL-OVRD-IND  (GAA-INDEX)  TO  OVRDINDO.  GAS1UPD 
02511      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR(GAA-INDEX)  TO  INTDESKO.  GAS1UPD 
02512      MOVE GAA-COND-ALL-BIT            (GAA-INDEX)  TO  CONDALLO.  GAS1UPD 
02513      MOVE GAA-COND-EXCLUSION-BIT      (GAA-INDEX)  TO  CONDEXCO.  GAS1UPD 
02514      MOVE GAA-COND-ICD-BIT            (GAA-INDEX)  TO  CONDICDO.  GAS1UPD 
02515      MOVE GAA-COND-TB-BIT             (GAA-INDEX)  TO  CONDTABO.  GAS1UPD 
02516      MOVE GAA-COND-MENTAL-BIT         (GAA-INDEX)  TO  CONDMENO.  GAS1UPD 
02517      MOVE GAA-COND-DRUG-BIT           (GAA-INDEX)  TO  CONDDRGO.  GAS1UPD 
02518      MOVE GAA-COND-ALCOHOL-BIT        (GAA-INDEX)  TO  CONDALCO.  GAS1UPD 
02519      MOVE GAA-COND-OB-COMP-BIT        (GAA-INDEX)  TO  CONDOBCO.  GAS1UPD 
02520      MOVE GAA-COND-OB-NORM-BIT        (GAA-INDEX)  TO  CONDOBNO.  GAS1UPD 
02521      MOVE GAA-COND-MALIGNANCY-BIT     (GAA-INDEX)  TO  CONDMALO.  GAS1UPD 
02522      MOVE GAA-COND-CARDIAC-DISEASE-BIT(GAA-INDEX)  TO  CONDCARO.  GAS1UPD 
02523      MOVE GAA-COND-OBESITY-BIT        (GAA-INDEX)  TO  CONDOBSO.  GAS1UPD 
02524      MOVE GAA-COND-KIDNEY-DISEASE-BIT (GAA-INDEX)  TO  CONDKDYO.  GAS1UPD 
02525      MOVE GAA-COND-ACCIDENT-BIT       (GAA-INDEX)  TO  CONDACCO.  GAS1UPD 
02526      MOVE GAA-COND-PRE-EXIST-BIT      (GAA-INDEX)  TO  CONDPECO.  GAS1UPD 
02527      MOVE GAA-COND-NON-EMER-BIT       (GAA-INDEX)  TO  CONDNEMO.  GAS1UPD 
02528      MOVE GAA-COND-SUICIDE-BIT        (GAA-INDEX)  TO  CONDSUIO.  GAS1UPD 
02529      MOVE GAA-COND-TMJ-BIT            (GAA-INDEX)  TO  CONDTMJO.  GAS1UPD 
02530      MOVE GAA-COND-INF-BIT            (GAA-INDEX)  TO  CONDINFO.  GAS1UPD 
02531      MOVE GAA-COND-LIFE-THREAT-BIT    (GAA-INDEX)  TO  CONDLIFO.  GAS1UPD 
02532      MOVE GAA-COND-EMER-MED-BIT       (GAA-INDEX)  TO  CONDEMCO.  GAS1UPD 
02533      MOVE GAA-COND-EMER-ACC-BIT       (GAA-INDEX)  TO  CONDEACO.  GAS1UPD 
02534      MOVE GAA-COND-SER-MEN-ILL-BIT    (GAA-INDEX)  TO  CONDSMIO.  GAS1UPD 
02535      MOVE GAA-COND-NON-SER-MEN-ILL-BIT (GAA-INDEX)  TO  CONDNSMO. GAS1UPD 
02536                                                                   GAS1UPD 
02537      MOVE -1  TO  PERIODL.                                        GAS1UPD 
02538                                                                   GAS1UPD 
02539      MOVE GAA-BAMA-FYI-VALUE(GAA-INDEX)  TO  FYIVALO.             GAS1UPD 
02540      SET  CURNT-OCURS-BIN        TO  GAA-INDEX.                   GAS1UPD 
02541      MOVE CURNT-OCURS-BIN        TO  CURNT-OCURS-PKD.             GAS1UPD 
02542      MOVE CURNT-OCCURS-OUT       TO  COCURANO.                    GAS1UPD 
02543                                                                   GAS1UPD 
02544      IF GAA-ENTRY-COUNT  >  1                                     GAS1UPD 
02545      THEN                                                         GAS1UPD 
02546          COMPUTE  TOTAL-OCURS-UNK  =  GAA-ENTRY-COUNT  - 1        GAS1UPD 
02547          MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                     GAS1UPD 
02548      ELSE                                                         GAS1UPD 
02549          MOVE  '01'    TO  TOCURANO.                              GAS1UPD 
02550                                                                   GAS1UPD 
02551 *                                                                 GAS1UPD 
02552      MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              GAS1UPD 
02553                       IDGDSLTO,  IPGPSLTO   IPGSSLTO.             GAS1UPD 
02554                                                                   GAS1UPD 
02555      SET GAA-INT-INDEX  TO       1.                               GAS1UPD 
02556      SET GAA-INT-INDEX  DOWN BY  1.                               GAS1UPD 
02557                                                                   GAS1UPD 
02558  4400-300-DISPLAY-LOOP.                                           GAS1UPD 
02559                                                                   GAS1UPD 
02560      SET GAA-INT-INDEX  UP BY  1.                                 GAS1UPD 
02561      IF  GAA-INT-INDEX  >  5                                      GAS1UPD 
02562          GO TO 4400-800-SEND.                                     GAS1UPD 
02563                                                                   GAS1UPD 
02564      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  =  HIGH-VALUES      GAS1UPD 
02565          GO TO 4400-800-SEND.                                     GAS1UPD 
02566                                                                   GAS1UPD 
02567      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IBGR '       GAS1UPD 
02568          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02569                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02570          MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    GAS1UPD 
02571          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02572                                                                   GAS1UPD 
02573      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IDGD '       GAS1UPD 
02574          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02575                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02576          MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    GAS1UPD 
02577          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02578                                                                   GAS1UPD 
02579      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IPGN '       GAS1UPD 
02580          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02581                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02582          MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    GAS1UPD 
02583          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02584                                                                   GAS1UPD 
02585      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IPGP '       GAS1UPD 
02586          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02587                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02588          MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    GAS1UPD 
02589          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02590                                                                   GAS1UPD 
02591      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IPGT '       GAS1UPD 
02592          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02593                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02594          MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    GAS1UPD 
02595          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02596                                                                   GAS1UPD 
02597      IF  GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)   =   '#IPGS '       GAS1UPD 
02598          MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)               GAS1UPD 
02599                                   TO  ACWA-DISPLAY-LEN-7          GAS1UPD 
02600          MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    GAS1UPD 
02601          GO TO 4400-300-DISPLAY-LOOP.                             GAS1UPD 
02602                                                                   GAS1UPD 
02603      MOVE WS-ABCODE-1BF3       TO  WS-ABCODE                      GAS1UPD 
02604      MOVE WS-ABCODE-1BF3-MSG   TO  WS-ABCODE-MSG                  GAS1UPD 
02605      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       GAS1UPD 
02606                                                                   GAS1UPD 
02607                                                                   GAS1UPD 
02608  4400-800-SEND.                                                   GAS1UPD 
02609                                                                   GAS1UPD 
02610      PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      GAS1UPD 
02611                                                                   GAS1UPD 
02612      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS1UPD 
02613                                                                   GAS1UPD 
02614  4400-900-EXIT. EXIT.                                             GAS1UPD 
02615 /*****************************************************************GAS1UPD 
02616 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GAS1UPD 
02617 *                                                                *GAS1UPD 
02618 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS1UPD 
02619 *                                                                *GAS1UPD 
02620 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GAS1UPD 
02621 *          2. IF GROUP IS CRITICAL:                              *GAS1UPD 
02622 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GAS1UPD 
02623 *                BENEFIT PROVISION.                              *GAS1UPD 
02624 *                - IF ON DATA BASE:                              *GAS1UPD 
02625 *                  - SCAN FOR #ABM TABULAR                       *GAS1UPD 
02626 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GAS1UPD 
02627 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GAS1UPD 
02628 *                      ON SCREEN AND ISSUE MESSAGE.              *GAS1UPD 
02629 ******************************************************************GAS1UPD 
02630  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GAS1UPD 
02631  4500-010.                                                        GAS1UPD 
02632                                                                   GAS1UPD 
02633      IF DELADDI  =  'CHG/DEL'    OR                               GAS1UPD 
02634         DELOLITI =  SPACES                                        GAS1UPD 
02635      THEN                                                         GAS1UPD 
02636         NEXT SENTENCE                                             GAS1UPD 
02637      ELSE                                                         GAS1UPD 
02638         GO TO 4500-900-EXIT.                                      GAS1UPD 
02639                                                                   GAS1UPD 
02640                                                                   GAS1UPD 
02641      MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. GAS1UPD 
02642                                                                   GAS1UPD 
02643                                                                   GAS1UPD 
02644      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS1UPD 
02645                 COMMAREA (DFHCOMMAREA)                            GAS1UPD 
02646                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS1UPD 
02647                                                                   GAS1UPD 
02648      GO TO 4500-900-EXIT.                                         GAS1UPD 
02649                                                                   GAS1UPD 
02650                                                                   GAS1UPD 
02651  4500-900-EXIT.   EXIT.                                           GAS1UPD 
02652                                                                   GAS1UPD 
02653 /*****************************************************************GAS1UPD 
02654 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GAS1UPD 
02655 *                                                                *GAS1UPD 
02656 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS1UPD 
02657 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GAS1UPD 
02658 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GAS1UPD 
02659 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GAS1UPD 
02660 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GAS1UPD 
02661 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GAS1UPD 
02662 *               +CDE+ INDICATOR (POSITION=8).                    *GAS1UPD 
02663 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GAS1UPD 
02664 *               ENTER, CONTINUE PROCESSING.                      *GAS1UPD 
02665 ******************************************************************GAS1UPD 
02666  4600-000-UPDATE-CDE-STATUS     SECTION.                          GAS1UPD 
02667  4600-010.                                                        GAS1UPD 
02668                                                                   GAS1UPD 
02669                                                                   GAS1UPD 
02670      SET  ACWA-INDEX-1    TO  GAA-INDEX.                          GAS1UPD 
02671      MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  GAS1UPD 
02672                                                                   GAS1UPD 
02673      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS1UPD 
02674                 COMMAREA (DFHCOMMAREA)                            GAS1UPD 
02675                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS1UPD 
02676                                                                   GAS1UPD 
02677      IF  ACWA-CDE-RETURN-DONT-SEND                                GAS1UPD 
02678          EXEC CICS  RETURN   END-EXEC.                            GAS1UPD 
02679                                                                   GAS1UPD 
02680  4600-900-EXIT.   EXIT.                                           GAS1UPD 
02681                                                                   GAS1UPD 
02682 /*****************************************************************GAS1UPD 
02683 *  4700  -  UPDATE W/F CONTROL RECORD                            *GAS1UPD 
02684 *                                                                *GAS1UPD 
02685 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS1UPD 
02686 *                                                                *GAS1UPD 
02687 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GAS1UPD 
02688 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GAS1UPD 
02689 *             RECORD AND THE INTERNAL TABULAR RECORDS; IF THE    *GAS1UPD 
02690 *             CDE STATUS HAS CHANGED.                            *GAS1UPD 
02691 *          2. REWRITE W/F CONTROL RECORD                         *GAS1UPD 
02692 ******************************************************************GAS1UPD 
02693  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GAS1UPD 
02694  4700-010.                                                        GAS1UPD 
02695                                                                   GAS1UPD 
02696      SET  ACWA-INDEX-1     TO  GAA-INDEX.                         GAS1UPD 
02697      MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. GAS1UPD 
02698                                                                   GAS1UPD 
02699      EXEC CICS  LINK   PROGRAM ('GACDEPGM')                       GAS1UPD 
02700                 COMMAREA (DFHCOMMAREA)                            GAS1UPD 
02701                 LENGTH(LENGTH OF DFHCOMMAREA)      END-EXEC.      GAS1UPD 
02702                                                                   GAS1UPD 
02703  4700-900-EXIT.  EXIT.                                            GAS1UPD 
02704                                                                   GAS1UPD 
02705 ******************************************************************GAS1UPD 
02706 *  4900  -  R E S E T   O T H E R   I N T E R N A L   T A B S     GAS1UPD 
02707 *                                                                 GAS1UPD 
02708 *    FUNCTION  (VIA CDE MODULE GACDEPGM)                          GAS1UPD 
02709 *          READ W/F ACCUM'S INTERNAL TABULAR RECORDS, THOSE ON    GAS1UPD 
02710 *          W/F ONLY.  RESET THE CDE STATUS INDICATOR ON THIS      GAS1UPD 
02711 *          INTERNAL TO EITHER 1U OR 2 BASED ON THE INTERNAL       GAS1UPD 
02712 *          DESCRIPTOR, THEN REWRITE THIS RECORD.                  GAS1UPD 
02713 ******************************************************************GAS1UPD 
02714  4900-RESET-OTHER-INT-TABS      SECTION.                          GAS1UPD 
02715                                                                   GAS1UPD 
02716      MOVE ZERO  TO  WS-INTRNL-TABS-TO-CHG-CNT.                    GAS1UPD 
02717      PERFORM 4900-COUNT-INT-TAB                                   GAS1UPD 
02718         VARYING GAA-INT-INDEX  FROM  1  BY  1                     GAS1UPD 
02719         UNTIL GAA-INT-INDEX  NOT <                                GAS1UPD 
02720                          GAA-INTERNAL-TABULAR-COUNT(GAA-INDEX)  ORGAS1UPD 
02721               GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  =  HIGH-VALUES.GAS1UPD 
02722                                                                   GAS1UPD 
02723      IF WS-INTRNL-TABS-TO-CHG-CNT  >  ZERO                        GAS1UPD 
02724         MOVE WS-REQUEST-4900-CNTL-UPDATE  TO                      GAS1UPD 
02725                                            ACWA-CDE-REQUEST-CODE  GAS1UPD 
02726         EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                   GAS1UPD 
02727                    COMMAREA (DFHCOMMAREA)                         GAS1UPD 
02728                    LENGTH(LENGTH OF DFHCOMMAREA)        END-EXEC. GAS1UPD 
02729                                                                   GAS1UPD 
02730      GO TO 4999-EXIT.                                             GAS1UPD 
02731                                                                   GAS1UPD 
02732  4900-COUNT-INT-TAB.                                              GAS1UPD 
02733      IF GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)  >  +8999999        GAS1UPD 
02734         ADD 1  TO  WS-INTRNL-TABS-TO-CHG-CNT.                     GAS1UPD 
02735                                                                   GAS1UPD 
02736  4999-EXIT.       EXIT.                                           GAS1UPD 
02737 /*****************************************************************GAS1UPD 
02738 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GAS1UPD 
02739 *                                                                 GAS1UPD 
02740 ******************************************************************GAS1UPD 
02741  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GAS1UPD 
02742                                                                   GAS1UPD 
02743      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GAS1UPD 
02744               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ABM-FIXED-LEN  +GAS1UPD 
02745              (GC-GCTABULR-ABM-VARY-LEN  *                         GAS1UPD 
02746                                    GC-GCTABULR-ABM-VARY-MAX-OCUR).GAS1UPD 
02747                                                                   GAS1UPD 
02748      IF ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS1UPD 
02749         NEXT SENTENCE                                             GAS1UPD 
02750      ELSE                                                         GAS1UPD 
02751         EXEC CICS GETMAIN                                         GAS1UPD 
02752                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GAS1UPD 
02753                INITIMG(WS-HEX-00)                                 GAS1UPD 
02754                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS1UPD 
02755                END-EXEC                                           GAS1UPD 
02756         SET ACWA-PR-ALL-LEVEL-TAB-PNTR     TO                     GAS1UPD 
02757                  ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.        GAS1UPD 
02758                                                                   GAS1UPD 
02759      MOVE GCIO-WORKFILE-KEY       TO  WS-SV-RESTO-KY.             GAS1UPD 
02760      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GAS1UPD 
02761      MOVE WRK-TAB-PROV-COPY-SLOT  TO  GCIO-TAB-SLOT-NO.           GAS1UPD 
02762      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GAS1UPD 
02763      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GAS1UPD 
02764      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GAS1UPD 
02765      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GAS1UPD 
02766                                                                   GAS1UPD 
02767      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GAS1UPD 
02768           TO  GAA2-ENTRY-COUNT.                                   GAS1UPD 
02769                                                                   GAS1UPD 
02770         MOVE WS-SV-RESTO-KY     TO GCIO-WORKFILE-KEY.             GAS1UPD 
02771                                                                   GAS1UPD 
02772      IF GCIO-TAB-TABULAR-ID  =  GAA2-PROVISION-ID AND             GAS1UPD 
02773         GAA2-PROVISION-SLOT-NO  NUMERIC AND                       GAS1UPD 
02774         GCIO-TAB-SLOT-NO     =  GAA2-PROVISION-SLOT-NO            GAS1UPD 
02775         GO TO 5000-900-EXIT.                                      GAS1UPD 
02776                                                                   GAS1UPD 
02777      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GAS1UPD 
02778                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GAS1UPD 
02779                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GAS1UPD 
02780                                                                   GAS1UPD 
02781      IF NOT GCIOA-GOOD-RETURN                                     GAS1UPD 
02782         MOVE WS-ABCODE-1BF7        TO  WS-ABCODE                  GAS1UPD 
02783         MOVE WS-ABCODE-1BF7-MSG    TO  WS-ABCODE-MSG              GAS1UPD 
02784         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS1UPD 
02785                                                                   GAS1UPD 
02786  5000-900-EXIT.     EXIT.                                         GAS1UPD 
02787                                                                   GAS1UPD 
02788 /*****************************************************************GAS1UPD 
02789 * 6000  BUILD GROUP SPEC KEY                                     *GAS1UPD 
02790 *                                                                *GAS1UPD 
02791 *    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *GAS1UPD 
02792 ******************************************************************GAS1UPD 
02793  6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          GAS1UPD 
02794  6000-010.                                                        GAS1UPD 
02795                                                                   GAS1UPD 
02796      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS1UPD 
02797      MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE.          GAS1UPD 
02798      MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE.          GAS1UPD 
02799      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS1UPD 
02800      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS1UPD 
02801      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS1UPD 
02802      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS1UPD 
02803      MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,          GAS1UPD 
02804                                    GCIO-WRK-PROVIDER-CONTROL.     GAS1UPD 
02805      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS1UPD 
02806      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS1UPD 
02807      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS1UPD 
02808      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS1UPD 
02809      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS1UPD 
02810      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS1UPD 
02811      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS1UPD 
02812  6000-900-EXIT. EXIT.                                             GAS1UPD 
02813                                                                   GAS1UPD 
02814                                                                   GAS1UPD 
02815 ******************************************************************GAS1UPD 
02816 * 6100  BUILD CONTRACT KEY                                       *GAS1UPD 
02817 *                                                                *GAS1UPD 
02818 *    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *GAS1UPD 
02819 ******************************************************************GAS1UPD 
02820  6100-000-BUILD-CONTRACT-KEY    SECTION.                          GAS1UPD 
02821  6100-010.                                                        GAS1UPD 
02822                                                                   GAS1UPD 
02823      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS1UPD 
02824      MOVE  'C'                 TO  GCIO-WRK-STATUS-CODE.          GAS1UPD 
02825      MOVE  'C3'                TO  GCIO-WRK-RECORD-TYPE.          GAS1UPD 
02826      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS1UPD 
02827      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS1UPD 
02828      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS1UPD 
02829      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS1UPD 
02830      MOVE GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS.          GAS1UPD 
02831      MOVE GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL.     GAS1UPD 
02832      MOVE GCA-FAM-REL-LVL      TO  GCIO-WRK-FAMILY-RELATION-LVL.  GAS1UPD 
02833      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS1UPD 
02834      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID          GAS1UPD 
02835      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS1UPD 
02836      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS1UPD 
02837      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS1UPD 
02838      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS1UPD 
02839  6100-900-EXIT. EXIT.                                             GAS1UPD 
02840                                                                   GAS1UPD 
02841 /*****************************************************************GAS1UPD 
02842 * 6200  BUILD BEN PROV KEY                                       *GAS1UPD 
02843 *                                                                *GAS1UPD 
02844 *    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *GAS1UPD 
02845 ******************************************************************GAS1UPD 
02846  6200-000-BUILD-BEN-PROV-KEY    SECTION.                          GAS1UPD 
02847  6200-010.                                                        GAS1UPD 
02848                                                                   GAS1UPD 
02849      MOVE SPACES                TO  GCIO-WORKFILE-KEY.            GAS1UPD 
02850      MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE.         GAS1UPD 
02851      MOVE  'C5'                 TO  GCIO-WRK-RECORD-TYPE.         GAS1UPD 
02852      MOVE GCA-PLAN-CODE         TO  GCIO-WRK-PLAN-CODE.           GAS1UPD 
02853      MOVE GCA-GROUP-NUM         TO  GCIO-WRK-GROUP-NUM.           GAS1UPD 
02854      MOVE GCA-SECTION-NUM       TO  GCIO-WRK-SECTION-NUM.         GAS1UPD 
02855      MOVE GCA-PKG-CODE          TO  GCIO-WRK-PKG-CODE.            GAS1UPD 
02856      MOVE GCA-L-O-B             TO  GCIO-WRK-LINE-OF-BUS.         GAS1UPD 
02857      MOVE GCA-PROV-CTL          TO  GCIO-WRK-PROVIDER-CONTROL.    GAS1UPD 
02858      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS1UPD 
02859      MOVE GCA-EFFDT-CEN         TO  GCIO-WRK-EFFDT-CEN.           GAS1UPD 
02860      MOVE GCA-BEN-PROV-ID       TO  GCIO-WRK-PROVISION-ID.        GAS1UPD 
02861      MOVE +9999999              TO  GCIO-WRK-PROVISION-SLOT-NO.   GAS1UPD 
02862      MOVE TABIDI                TO  GCIO-WRK-TAB-PROVISION-ID.    GAS1UPD 
02863      MOVE TABSLTNI              TO  ACWA-DISPLAY-LEN-7.           GAS1UPD 
02864      MOVE ACWA-DISPLAY-LEN-7    TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS1UPD 
02865                                                                   GAS1UPD 
02866  6200-900-EXIT. EXIT.                                             GAS1UPD 
02867                                                                   GAS1UPD 
02868 /*****************************************************************GAS1UPD 
02869 *  XCTL TO MAIN MENU                                             *GAS1UPD 
02870 *                                                                *GAS1UPD 
02871 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *GAS1UPD 
02872 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*GAS1UPD 
02873 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUSGAS1UPD 
02874 *  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GETGAS1UPD 
02875 *  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *GAS1UPD 
02876 ******************************************************************GAS1UPD 
02877  6400-000-XCTL-TO-MAIN-MENU     SECTION.                          GAS1UPD 
02878  6400-010.                                                        GAS1UPD 
02879                                                                   GAS1UPD 
02880      MOVE WS-ABCODE-1BP1       TO  WS-ABCODE.                     GAS1UPD 
02881      MOVE WS-ABCODE-1BP1-MSG   TO  WS-ABCODE-MSG.                 GAS1UPD 
02882                                                                   GAS1UPD 
02883      EXEC CICS  XCTL   PROGRAM('GCPSPGM')   END-EXEC.             GAS1UPD 
02884                                                                   GAS1UPD 
02885  6400-900-EXIT. EXIT.                                             GAS1UPD 
02886                                                                   GAS1UPD 
02887 **************************************************************    GAS1UPD 
02888 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS1UPD 
02889 * - DEAD CODE ELIMINATION.                                        GAS1UPD 
02890 * - REMOVED: 7000-000-PRINT-HARDCOPY        SECTION.              GAS1UPD 
02891 **************************************************************    GAS1UPD 
02892                                                                   GAS1UPD 
02893 /*****************************************************************GAS1UPD 
02894 * 7900  RESET ATTRIBUTES                                         *GAS1UPD 
02895 ******************************************************************GAS1UPD 
02896  7900-000-RESET-ATTRIBUTES      SECTION.                          GAS1UPD 
02897  7900-010.                                                        GAS1UPD 
02898                                                                   GAS1UPD 
02899      MOVE DFHBMUNF  TO  BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA    GAS1UPD 
02900               LOBA      PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA    GAS1UPD 
02901               MAXOVRDA  REININDA  INTRVALA  INTTYPEA  CLMLVLIA    GAS1UPD 
02902               BNMXVALA  DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA    GAS1UPD 
02903               FYIVALA   CONDALLA  CONDEXCA  CONDICDA  CONDTABA    GAS1UPD 
02904               CONDMENA  CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA    GAS1UPD 
02905               CONDMALA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA    GAS1UPD 
02906         CONDPECA  CONDNEMA  DEFINTNA  CONDSUIA CONDTMJA CONDINFA  GAS1UPD 
02907               IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA    GAS1UPD 
02908               AGEQLLA   AGEQLHA   CONDLIFA  IPGSOPTA              GAS1UPD 
02909               CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA              GAS1UPD 
02910               IDGDOPTA  IPGPOPTA  RELPINDA AGELIMLA AGELIMHA      GAS1UPD 
02911               FEAKINDA  ACCUMIDA  CAPINDA  SABDINDA               GAS1UPD 
02912               BISNDINA  ASCDSCDA  BENTYPA   TIERCDA   TIERLVA.    GAS1UPD 
02913                                                                   GAS1UPD 
02914      IF  DELADDO  =  'CHG/DEL'                                    GAS1UPD 
02915      THEN                                                         GAS1UPD 
02916          NEXT SENTENCE                                            GAS1UPD 
02917      ELSE                                                         GAS1UPD 
02918          GO TO 7900-900-EXIT.                                     GAS1UPD 
02919                                                                   GAS1UPD 
02920                                                                   GAS1UPD 
02921      IF  CDEINDO  =  '+CDE+'                                      GAS1UPD 
02922      THEN                                                         GAS1UPD 
02923 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS1UPD 
02924          MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       GAS1UPD 
02925                AGELIMA    PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   GAS1UPD 
02926                AGEQLTA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   GAS1UPD 
                     BISNDITA                                                   
02927      ELSE                                                         GAS1UPD 
02928          NEXT SENTENCE.                                           GAS1UPD 
02929                                                                   GAS1UPD 
02930                                                                   GAS1UPD 
02931      IF  CDEINDO  =  '+CDE-'                                      GAS1UPD 
02932      THEN                                                         GAS1UPD 
02933 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS1UPD 
02934          MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       GAS1UPD 
02935                AGELIMA    PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   GAS1UPD 
02936                AGEQLTA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   GAS1UPD 
                     BISNDITA                                                   
02937 *---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        GAS1UPD 
02938          MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       GAS1UPD 
02939        AGELIMLA AGELIMHA  PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   GAS1UPD 
02940         AGEQLLA AGEQLHA   COPAYINA  INTDESKA CONDALLA  CONDEXCA   GAS1UPD 
02941                           CONDICDA  CONDTABA CONDMENA  CONDDRGA   GAS1UPD 
02942                           CONDALCA  CONDOBCA CONDOBNA  CONDMALA   GAS1UPD 
02943                 CONDTMJA  CONDCARA  CONDOBSA CONDKDYA  CONDACCA   GAS1UPD 
02944                 CONDINFA  CONDPECA  CONDNEMA CONDSUIA  CONDLIFA   GAS1UPD 
02945                 CONDEMCA  CONDEACA CONDSMIA  CONDNSMA  BISNDINA   GAS1UPD 
02946          IF  ERRMSGO  >  SPACES                                   GAS1UPD 
02947          THEN                                                     GAS1UPD 
02948              NEXT SENTENCE                                        GAS1UPD 
02949          ELSE                                                     GAS1UPD 
02950              SET  WT-01-INDEX  TO  +08                            GAS1UPD 
02951              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO    GAS1UPD 
02952      ELSE                                                         GAS1UPD 
02953          NEXT SENTENCE.                                           GAS1UPD 
02954                                                                   GAS1UPD 
02955  7900-900-EXIT. EXIT.                                             GAS1UPD 
02956                                                                   GAS1UPD 
02957 /*****************************************************************GAS1UPD 
02958 * 9000  SEND ERASE THEN RETURN                                   *GAS1UPD 
02959 ******************************************************************GAS1UPD 
02960  9000-000-SEND-ERASE-RETURN     SECTION.                          GAS1UPD 
02961  9000-010.                                                        GAS1UPD 
02962                                                                   GAS1UPD 
02963      MOVE DFHBMASD  TO                                            GAS1UPD 
02964                    PERLITTA MANAPLTA CARYOVTA FDLRCLTA            GAS1UPD 
02965                    PERLIMTA MANAPLIA CARYOVRA FDLRCLIA            GAS1UPD 
02966                    TIMEDLRA TIMEDOLA.                             GAS1UPD 
02967                                                                   GAS1UPD 
02968      MOVE -1  TO  ERRMSGL.                                        GAS1UPD 
02969                                                                   GAS1UPD 
02970      EXEC CICS  SEND   MAP ('GA1XI01')    ERASE  CURSOR           GAS1UPD 
02971                 MAPSET('GA1XSET')    END-EXEC.                    GAS1UPD 
02972                                                                   GAS1UPD 
02973      EXEC CICS  RETURN   END-EXEC.                                GAS1UPD 
02974                                                                   GAS1UPD 
02975  9000-900-EXIT.    EXIT.                                          GAS1UPD 
02976                                                                   GAS1UPD 
02977 /*****************************************************************GAS1UPD 
02978 * 9010  SEND DATAONLY AND RETURN                                 *GAS1UPD 
02979 ******************************************************************GAS1UPD 
02980  9010-000-SEND-DATAONLY-RETURN  SECTION.                          GAS1UPD 
02981  9010-010.                                                        GAS1UPD 
02982                                                                   GAS1UPD 
02983      MOVE -1  TO  ERRMSGL.                                        GAS1UPD 
02984                                                                   GAS1UPD 
02985      EXEC CICS  SEND   MAP('GA1XI01')  DATAONLY  CURSOR           GAS1UPD 
02986                 MAPSET('GA1XSET')       END-EXEC.                 GAS1UPD 
02987                                                                   GAS1UPD 
02988      EXEC CICS  RETURN   END-EXEC.                                GAS1UPD 
02989                                                                   GAS1UPD 
02990  9010-900-EXIT.     EXIT.                                         GAS1UPD 
02991                                                                   GAS1UPD 
02992 **************************************************************    GAS1UPD 
02993 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS1UPD 
02994 * - DEAD CODE ELIMINATION.                                        GAS1UPD 
02995 * - REMOVED: 9200-000-GREGORIAN-TO-JULIAN   SECTION.              GAS1UPD 
02996 *            9300-000-JULIAN-TO-GREGORIAN   SECTION.              GAS1UPD 
02997 **************************************************************    GAS1UPD 
02998                                                                   GAS1UPD 
02999 /*****************************************************************GAS1UPD 
03000 * 9800  ERROR MSG THEN ABEND                                     *GAS1UPD 
03001 *                                                                *GAS1UPD 
03002 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GAS1UPD 
03003 *  AND THEN ABENDS USING THE ABEND CODE EARLIER MEFINED.         *GAS1UPD 
03004 ******************************************************************GAS1UPD 
03005  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GAS1UPD 
03006  9800-010.                                                        GAS1UPD 
03007                                                                   GAS1UPD 
03008      MOVE -1              TO  MFRMSLTL.                           GAS1UPD 
03009      MOVE WS-ABCODE-MSG   TO  ERRMSGO.                            GAS1UPD 
03010                                                                   GAS1UPD 
03011      EXEC CICS  SEND   MAP ('GA1XI01') ERASE  CURSOR   WAIT       GAS1UPD 
03012                 MAPSET('GA1XSET')      END-EXEC.                  GAS1UPD 
03013                                                                   GAS1UPD 
03014      EXEC CICS  ABEND   ABCODE(WS-ABCODE)   END-EXEC.             GAS1UPD 
03015                                                                   GAS1UPD 
03016  9800-900-EXIT. EXIT.                                             GAS1UPD 
