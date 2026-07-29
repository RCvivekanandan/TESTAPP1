00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTTHERP.                                            ELTTHERP
00003  AUTHOR. D SECOR  -  A C I.                                          LV002
00004  DATE-WRITTEN.   5/19/86.                                         ELTTHERP
00005  DATE-COMPILED.                                                   ELTTHERP
00006      SKIP3                                                        ELTTHERP
00007 ******************************************************************ELTTHERP
00008 *@>ELTTHERP                                                       ELTTHERP
00009 *@¬                                                               ELTTHERP
00010 *                        PROGRAM ABSTRACT                         ELTTHERP
00011 *                                                                 ELTTHERP
00012 *@¬ PROGRAM NAME:   E.L.S. THERAPY BENEFITS                       ELTTHERP
00013 *@¬                                                               ELTTHERP
00014 *@¬ PROGRAM I.D.:   ELTTHERP                                      ELTTHERP
00015 *@¬                                                               ELTTHERP
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTTHERP
00017 *@¬            THERAPY COVERAGE GIVEN A MEMBER.                   ELTTHERP
00018 *@¬                                                               ELTTHERP
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF THERAPY COVERAGE  ELTTHERP
00020 *@¬            AFFORDED A MEMBER BY HIS GROUP.  THIS INFORMATION  ELTTHERP
00021 *@¬            IS GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS   ELTTHERP
00022 *@¬            FOR THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR ELTTHERP
00023 *@¬            RANGE OF DATES.                                    ELTTHERP
00024 *              THIS PROGRAM WORKS WITH ALL THE SUB-TOPICS OF      ELTTHERP
00025 *              THERAPIES.                                         ELTTHERP
00026 *                                                                 ELTTHERP
00027 *@¬ RECORDS                                                       ELTTHERP
00028 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT, BENEFIT PROVISION FORMAT ELTTHERP
00029 *@¬          A, B, AND E.                                         ELTTHERP
00030 *@¬                                                               ELTTHERP
00031 *@¬ PROCESSING                                                    ELTTHERP
00032 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTTHERP
00033 *@¬                                                               ELTTHERP
00034 *@¬                                                               ELTTHERP
00035 ***************************************************************** ELTTHERP
00036 *                                                                 ELTTHERP
00037 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTTHERP
00038 *          *-*       U P D A T E   H I S T O R Y       *-*        ELTTHERP
00039 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTTHERP
00040 *                                                                 ELTTHERP
00041 **-CHG NUM-* *-DATE-* *WHO* *------DESCRIPTION------------------- ELTTHERP
00042 *    XXXX    05/19/86  DES   ORIGINAL IMPLEMENTATION              ELTTHERP
00043 *    0001    07/03/86  LET   DISCREPANCY #179.  ADDED PROFESSIONALELTTHERP
00044 *                            CHARGES ON A HOSPITAL CLAIM ELEMENT  ELTTHERP
00045 *                            TO TOPIC.                            ELTTHERP
00046 *    0002    07/14/86  LET   DISCREPANCY #181.  ADDED 'CRPI B'    ELTTHERP
00047 *                            AND 'CRPO B' BENEFIT PROVISION.      ELTTHERP
00048 *    0003    07/24/86  JTC   CHANGED THE PICTURE OF WS-BASIC-AMOUNELTTHERP
00049 *                            FROM PIC $$$9 TO PIC ZZ9.99-, AND    ELTTHERP
00050 *                            THE ASSOCIATED REDEFINES.            ELTTHERP
00051 *                            CHANGED THE PICTURE OF WS-SUPP-AMOUNTELTTHERP
00052 *                            FROM PIC $$$9 TO PIC ZZ9.99-, AND    ELTTHERP
00053 *                            THE ASSOCIATED REDEFINES.            ELTTHERP
00054 *                                                                 ELTTHERP
00055 *    0004    08/01/86  LET   MADE CORRECTION TO THE PRIOR         ELTTHERP
00056 *                            ADMISSION REQUIREMENT FOR 'CRPO B'.  ELTTHERP
00057 *                                                                 ELTTHERP
00058 *    0005    08/14/86  LET   USING 1ST HEADER LINE FROM THE       ELTTHERP
00059 *                            PROLOG ON THE TOPIC SCREENS.         ELTTHERP
00060 *                                                                 ELTTHERP
00061 *    XXXX    10/09/86  NAC   VS COBOL II CONVERSION.              ELTTHERP
00062 *                                                                 ELTTHERP
00063 *    XXXX    03/12/87  NAC   CHANGE KEYWORDS.                     ELTTHERP
00064 *                                                                 ELTTHERP
00065 *    XXXX    04/29/87  NAC   CLEANUP OUTPUT ROUTINE AND INCLUDE   ELTTHERP
00066 *                            SET ADDRESS OF GROUP SPECIFIC.       ELTTHERP
00067 *    XXXX    10/19/87  NAC   REWORD PHRASE FOR COVERED BENEFITS.  ELTTHERP
00068 *                                                                 ELTTHERP
00069 *            04/06/88  REB   INITIALIZE WS-TEMP2-CHARS TO LOW-VALUELTTHERP
00070 *                            TO AVOID OCCASION JUNK BEING DISPLAYEELTTHERP
00071 *                            FROM TIME TO TIME.                   ELTTHERP
00072 *                                                                 ELTTHERP
00073 *            04/10/89  GEM   STORAGE MANAGEMENT ENHANCEMENTS.     ELTTHERP
00074 *            10/10/89  RKH   ADDED TRANS TO OTHER RESP            ELTTHERP
00075 *            10/16/90  GEM   ADDED BENEFIT PROVISIONS IDS.        ELTTHERP
00076 *            11/16/90  GEM   CHANGED PLP-TRANSF-OTHER-RESP-IND    ELTTHERP
00077 *                            COMPARE TO THE LITERAL ZERO.         ELTTHERP
00078 *            01/28/92  AKK   ADD TEST FOR BENEFIT PROVISION       ELTTHERP
00079 *                      BAK   FORMATS TO ASSURE DATA EXISTS.       ELTTHERP
00080 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTTHERP
00081 *                                                                 ELTTHERP
00082 *  DECEMBER, 1995.  RGO.  RE-STRUCTURE THE THERAPIES BP PROGRAM,  ELTTHERP
00083 *            AND CHANGE WHICH BP'S ARE DISPLAYED IN THE SUBTOPICS.ELTTHERP
00084 *         1) THE FORMAT OF THE DISPLAY IS JUST LIKE THAT DONE IN  ELTTHERP
00085 *            OB RELATED (PROGRAM ELTOBREL).                       ELTTHERP
00086 *         2) SUB-TOPICS WERE ADDED AND EXISTING ONES RE-ARRANGED. ELTTHERP
00087 *         3) 2 FIELDS, ADDITIONAL-PRICING-PERCENT AND             ELTTHERP
00088 *            VARIABLE-INDEMNITY-PERCENT, ARE NO LONGER BEING      ELTTHERP
00089 *            DISPLAYED.                                           ELTTHERP
00090 *         4) THE USE OF 'SECTIONS' WAS REMOVED, BECAUSE IT'S POOR ELTTHERP
00091 *            CODING TECHNIQUE AND UNNECCESARILY COMPLICATES THE   ELTTHERP
00092 *            LOGIC AND DEBUGGING.                                 ELTTHERP
00093 *         5) FIXED AN ERROR IN PARA 3250-. IT WAS MOVING          ELTTHERP
00094 *            PLP-PLACE-TREAT-ELIG-IND TO CODES VALUE, INSTEAD OF  ELTTHERP
00095 *            TRANSF-OTHER-RESPON-IND. OUCH!                       ELTTHERP
00096 *                                                                 ELTTHERP
00097 ***************************************************************** ELTTHERP
00098                                                                   ELTTHERP
00099 /                                                                 ELTTHERP
00100  ENVIRONMENT DIVISION.                                            ELTTHERP
00101      SKIP3                                                        ELTTHERP
00102  DATA DIVISION.                                                   ELTTHERP
00103  WORKING-STORAGE SECTION.                                         ELTTHERP
00104  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTTHERP
00105      '***ELTTHERP WS BEGINS***'.                                  ELTTHERP
00106 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTTHERP
00107  01  WS-WORK-FIELDS.                                              ELTTHERP
00108      05  WS-HEX-00                     PIC X.                     ELTTHERP
00109      05  WS-CHAR-0                     PIC X.                     ELTTHERP
00110      05  WS-YES                        PIC X(01) VALUE 'Y'.       ELTTHERP
00111      05  WS-NO                         PIC X(01) VALUE 'N'.       ELTTHERP
00112      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTTHERP
00113      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTTHERP
00114      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTTHERP
00115      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTTHERP
00116      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTTHERP
00117      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTTHERP
00118      05  WS-FIRSTTIME-IND              PIC X.                     ELTTHERP
00119        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTTHERP
00120      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTTHERP
00121        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTTHERP
00122      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTTHERP
00123                                                                   ELTTHERP
00124                                                                   ELTTHERP
00125  01  SWITCHES.                                                    ELTTHERP
00126      05  WS-SUB-CMF                    PIC S999  COMP-3 VALUE +0. ELTTHERP
00127      05  DISPLAY-BAS-SUP         PIC X(01) VALUE 'N'.             ELTTHERP
00128      05  CALL-ELUOUTPT-IND       PIC X(01).                       ELTTHERP
00129          88  YES-CALL-ELUOUTPT              VALUE 'Y'.            ELTTHERP
00130      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTTHERP
00131          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTTHERP
00132                                                                   ELTTHERP
00133  01  FIRST-CODE-LINE.                                             ELTTHERP
00134      05  FIRST-CODE-CHAR79       PIC X(79).                       ELTTHERP
00135      05  FIRST-CODE-BROKE-UP   REDEFINES FIRST-CODE-CHAR79.       ELTTHERP
00136          10  FIRST-CHAR          PIC X(1).                        ELTTHERP
00137              88  FIRST-CHAR-SHOW-AS-IS  VALUE QUOTE.              ELTTHERP
00138          10  FIRST-THE-REST      PIC X(78).                       ELTTHERP
00139                                                                   ELTTHERP
00140 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTTHERP
00141  01  TABLE-MAX                   PIC S9(03) VALUE +15 COMP.       ELTTHERP
00142 * 9  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTTHERP
00143                                                                   ELTTHERP
00144  01  WS-BEN-PROV-ID.                                              ELTTHERP
00145 ***************************************************************** ELTTHERP
00146 *   PHYSICAL THERAPY                                              ELTTHERP
00147 ***************************************************************** ELTTHERP
00148      05  WS-PHYS-IP-INST-CNT           PIC S999 COMP-3  VALUE +5. ELTTHERP
00149      05  WS-PHYS-IP-INST-TAB.                                     ELTTHERP
00150        10  FILLER                      PIC X(6)  VALUE 'PMTI B'.  ELTTHERP
00151        10  FILLER                      PIC X(6)  VALUE 'WTI  B'.  ELTTHERP
00152        10  FILLER                      PIC X(6)  VALUE 'FOTI B'.  ELTTHERP
00153        10  FILLER                      PIC X(6)  VALUE 'MMI  B'.  ELTTHERP
00154        10  FILLER                      PIC X(6)  VALUE 'PTRI B'.  ELTTHERP
00155      05  WS-PHYS-IP-INST-LIST   REDEFINES   WS-PHYS-IP-INST-TAB   ELTTHERP
00156                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00157                                                                   ELTTHERP
00158      05  WS-PHYS-OP-INST-CNT           PIC S999 COMP-3  VALUE +5. ELTTHERP
00159      05  WS-PHYS-OP-INST-TAB.                                     ELTTHERP
00160        10  FILLER                      PIC X(6)  VALUE 'PMTO B'.  ELTTHERP
00161        10  FILLER                      PIC X(6)  VALUE 'WTO  B'.  ELTTHERP
00162        10  FILLER                      PIC X(6)  VALUE 'FOTO B'.  ELTTHERP
00163        10  FILLER                      PIC X(6)  VALUE 'MMO  B'.  ELTTHERP
00164        10  FILLER                      PIC X(6)  VALUE 'PTRO B'.  ELTTHERP
00165      05  WS-PHYS-OP-INST-LIST   REDEFINES   WS-PHYS-OP-INST-TAB   ELTTHERP
00166                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00167                                                                   ELTTHERP
00168      05  WS-PHYS-IP-PROF-CNT           PIC S999 COMP-3  VALUE +5. ELTTHERP
00169      05  WS-PHYS-IP-PROF-TAB.                                     ELTTHERP
00170        10  FILLER                      PIC X(6)  VALUE 'PMTI E'.  ELTTHERP
00171        10  FILLER                      PIC X(6)  VALUE 'WTI  E'.  ELTTHERP
00172        10  FILLER                      PIC X(6)  VALUE 'FOTI E'.  ELTTHERP
00173        10  FILLER                      PIC X(6)  VALUE 'MMI  E'.  ELTTHERP
00174        10  FILLER                      PIC X(6)  VALUE 'PTRI W'.  ELTTHERP
00175      05  WS-PHYS-IP-PROF-LIST   REDEFINES   WS-PHYS-IP-PROF-TAB   ELTTHERP
00176                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00177                                                                   ELTTHERP
00178      05  WS-PHYS-OP-PROF-CNT           PIC S999 COMP-3  VALUE +5. ELTTHERP
00179      05  WS-PHYS-OP-PROF-TAB.                                     ELTTHERP
00180        10  FILLER                      PIC X(6)  VALUE 'PMTO E'.  ELTTHERP
00181        10  FILLER                      PIC X(6)  VALUE 'WTO  E'.  ELTTHERP
00182        10  FILLER                      PIC X(6)  VALUE 'FOTO E'.  ELTTHERP
00183        10  FILLER                      PIC X(6)  VALUE 'MMO  E'.  ELTTHERP
00184        10  FILLER                      PIC X(6)  VALUE 'PTRO W'.  ELTTHERP
00185      05  WS-PHYS-OP-PROF-LIST   REDEFINES   WS-PHYS-OP-PROF-TAB   ELTTHERP
00186                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00187                                                                   ELTTHERP
00188 ***************************************************************** ELTTHERP
00189 *   CHEMO    THERAPY                                              ELTTHERP
00190 ***************************************************************** ELTTHERP
00191      05  WS-CHEMO-IP-INST-CNT          PIC S999 COMP-3  VALUE +4. ELTTHERP
00192      05  WS-CHEMO-IP-INST-TAB.                                    ELTTHERP
00193        10  FILLER                      PIC X(6)  VALUE 'BCHI B'.  ELTTHERP
00194        10  FILLER                      PIC X(6)  VALUE 'BRXI B'.  ELTTHERP
00195        10  FILLER                      PIC X(6)  VALUE 'MCHI B'.  ELTTHERP
00196        10  FILLER                      PIC X(6)  VALUE 'MRXI B'.  ELTTHERP
00197      05  WS-CHEMO-IP-INST-LIST   REDEFINES   WS-CHEMO-IP-INST-TAB ELTTHERP
00198                                        PIC X(6)  OCCURS 4 TIMES.  ELTTHERP
00199                                                                   ELTTHERP
00200      05  WS-CHEMO-OP-INST-CNT          PIC S999 COMP-3  VALUE +5. ELTTHERP
00201      05  WS-CHEMO-OP-INST-TAB.                                    ELTTHERP
00202        10  FILLER                      PIC X(6)  VALUE 'BCHO B'.  ELTTHERP
00203        10  FILLER                      PIC X(6)  VALUE 'BRXO B'.  ELTTHERP
00204        10  FILLER                      PIC X(6)  VALUE 'CFOV B'.  ELTTHERP
00205        10  FILLER                      PIC X(6)  VALUE 'MCHO B'.  ELTTHERP
00206        10  FILLER                      PIC X(6)  VALUE 'MRXO B'.  ELTTHERP
00207      05  WS-CHEMO-OP-INST-LIST   REDEFINES   WS-CHEMO-OP-INST-TAB ELTTHERP
00208                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00209                                                                   ELTTHERP
00210      05  WS-CHEMO-IP-PROF-CNT          PIC S999 COMP-3  VALUE +4. ELTTHERP
00211      05  WS-CHEMO-IP-PROF-TAB.                                    ELTTHERP
00212        10  FILLER                      PIC X(6)  VALUE 'BCHI E'.  ELTTHERP
00213        10  FILLER                      PIC X(6)  VALUE 'BRXI E'.  ELTTHERP
00214        10  FILLER                      PIC X(6)  VALUE 'MCHI E'.  ELTTHERP
00215        10  FILLER                      PIC X(6)  VALUE 'MRXI E'.  ELTTHERP
00216      05  WS-CHEMO-IP-PROF-LIST   REDEFINES   WS-CHEMO-IP-PROF-TAB ELTTHERP
00217                                        PIC X(6)  OCCURS 4 TIMES.  ELTTHERP
00218                                                                   ELTTHERP
00219      05  WS-CHEMO-OP-PROF-CNT          PIC S999 COMP-3  VALUE +5. ELTTHERP
00220      05  WS-CHEMO-OP-PROF-TAB.                                    ELTTHERP
00221        10  FILLER                      PIC X(6)  VALUE 'BCHO E'.  ELTTHERP
00222        10  FILLER                      PIC X(6)  VALUE 'BRXO E'.  ELTTHERP
00223        10  FILLER                      PIC X(6)  VALUE 'CFOV E'.  ELTTHERP
00224        10  FILLER                      PIC X(6)  VALUE 'MCHO E'.  ELTTHERP
00225        10  FILLER                      PIC X(6)  VALUE 'MRXO E'.  ELTTHERP
00226      05  WS-CHEMO-OP-PROF-LIST   REDEFINES   WS-CHEMO-OP-PROF-TAB ELTTHERP
00227                                        PIC X(6)  OCCURS 5 TIMES.  ELTTHERP
00228                                                                   ELTTHERP
00229 ***************************************************************** ELTTHERP
00230 *   SHOCK    THERAPY                                              ELTTHERP
00231 ***************************************************************** ELTTHERP
00232      05  WS-SHOCK-IP-INST-CNT          PIC S999 COMP-3  VALUE +1. ELTTHERP
00233      05  WS-SHOCK-IP-INST-TAB.                                    ELTTHERP
00234        10  FILLER                      PIC X(6)  VALUE 'STI  B'.  ELTTHERP
00235      05  WS-SHOCK-IP-INST-LIST  REDEFINES   WS-SHOCK-IP-INST-TAB  ELTTHERP
00236                                        PIC X(6)  OCCURS  1 TIMES. ELTTHERP
00237                                                                   ELTTHERP
00238      05  WS-SHOCK-OP-INST-CNT          PIC S999 COMP-3  VALUE +1. ELTTHERP
00239      05  WS-SHOCK-OP-INST-TAB.                                    ELTTHERP
00240        10  FILLER                      PIC X(6)  VALUE 'STO  A'.  ELTTHERP
00241      05  WS-SHOCK-OP-INST-LIST  REDEFINES   WS-SHOCK-OP-INST-TAB  ELTTHERP
00242                                        PIC X(6)  OCCURS  1 TIMES. ELTTHERP
00243                                                                   ELTTHERP
00244      05  WS-SHOCK-IP-PROF-CNT          PIC S999 COMP-3  VALUE +3. ELTTHERP
00245      05  WS-SHOCK-IP-PROF-TAB.                                    ELTTHERP
00246        10  FILLER                      PIC X(6)  VALUE 'ETAI C'.  ELTTHERP
00247        10  FILLER                      PIC X(6)  VALUE 'ESTI E'.  ELTTHERP
00248        10  FILLER                      PIC X(6)  VALUE 'ISTI E'.  ELTTHERP
00249      05  WS-SHOCK-IP-PROF-LIST  REDEFINES   WS-SHOCK-IP-PROF-TAB  ELTTHERP
00250                                        PIC X(6)  OCCURS  3 TIMES. ELTTHERP
00251                                                                   ELTTHERP
00252      05  WS-SHOCK-OP-PROF-CNT          PIC S999 COMP-3  VALUE +3. ELTTHERP
00253      05  WS-SHOCK-OP-PROF-TAB.                                    ELTTHERP
00254        10  FILLER                      PIC X(6)  VALUE 'ETAO C'.  ELTTHERP
00255        10  FILLER                      PIC X(6)  VALUE 'ESTO E'.  ELTTHERP
00256        10  FILLER                      PIC X(6)  VALUE 'ISTO E'.  ELTTHERP
00257      05  WS-SHOCK-OP-PROF-LIST  REDEFINES   WS-SHOCK-OP-PROF-TAB  ELTTHERP
00258                                        PIC X(6)  OCCURS  3 TIMES. ELTTHERP
00259                                                                   ELTTHERP
00260 ***************************************************************** ELTTHERP
00261 *   OTHER    THERAPY                                              ELTTHERP
00262 ***************************************************************** ELTTHERP
00263      05  WS-MISC-IP-INST-CNT           PIC S999 COMP-3  VALUE +4. ELTTHERP
00264      05  WS-MISC-IP-INST-TAB.                                     ELTTHERP
00265        10  FILLER                      PIC X(6)  VALUE 'DRTI B'.  ELTTHERP
00266        10  FILLER                      PIC X(6)  VALUE 'DTI  B'.  ELTTHERP
00267        10  FILLER                      PIC X(6)  VALUE 'IHI  B'.  ELTTHERP
00268        10  FILLER                      PIC X(6)  VALUE 'MPTI B'.  ELTTHERP
00269      05  WS-MISC-IP-INST-LIST   REDEFINES   WS-MISC-IP-INST-TAB   ELTTHERP
00270                                        PIC X(6)  OCCURS  4 TIMES. ELTTHERP
00271                                                                   ELTTHERP
00272      05  WS-MISC-OP-INST-CNT           PIC S999 COMP-3  VALUE +6. ELTTHERP
00273      05  WS-MISC-OP-INST-TAB.                                     ELTTHERP
00274        10  FILLER                      PIC X(6)  VALUE 'DRTO B'.  ELTTHERP
00275        10  FILLER                      PIC X(6)  VALUE 'DTO  B'.  ELTTHERP
00276        10  FILLER                      PIC X(6)  VALUE 'EAIH B'.  ELTTHERP
00277        10  FILLER                      PIC X(6)  VALUE 'EMIH B'.  ELTTHERP
00278        10  FILLER                      PIC X(6)  VALUE 'IHO  B'.  ELTTHERP
00279        10  FILLER                      PIC X(6)  VALUE 'MPTO B'.  ELTTHERP
00280      05  WS-MISC-OP-INST-LIST   REDEFINES   WS-MISC-OP-INST-TAB   ELTTHERP
00281                                        PIC X(6)  OCCURS  6 TIMES. ELTTHERP
00282                                                                   ELTTHERP
00283      05  WS-MISC-IP-PROF-CNT           PIC S999 COMP-3  VALUE +4. ELTTHERP
00284      05  WS-MISC-IP-PROF-TAB.                                     ELTTHERP
00285        10  FILLER                      PIC X(6)  VALUE 'DRTI B'.  ELTTHERP
00286        10  FILLER                      PIC X(6)  VALUE 'DTI  B'.  ELTTHERP
00287        10  FILLER                      PIC X(6)  VALUE 'IHI  B'.  ELTTHERP
00288        10  FILLER                      PIC X(6)  VALUE 'MPTI D'.  ELTTHERP
00289      05  WS-MISC-IP-PROF-LIST   REDEFINES   WS-MISC-IP-PROF-TAB   ELTTHERP
00290                                        PIC X(6)  OCCURS  4 TIMES. ELTTHERP
00291                                                                   ELTTHERP
00292      05  WS-MISC-OP-PROF-CNT           PIC S999 COMP-3  VALUE +6. ELTTHERP
00293      05  WS-MISC-OP-PROF-TAB.                                     ELTTHERP
00294        10  FILLER                      PIC X(6)  VALUE 'DRTO E'.  ELTTHERP
00295        10  FILLER                      PIC X(6)  VALUE 'DTO  E'.  ELTTHERP
00296        10  FILLER                      PIC X(6)  VALUE 'EAIH E'.  ELTTHERP
00297        10  FILLER                      PIC X(6)  VALUE 'EMIH E'.  ELTTHERP
00298        10  FILLER                      PIC X(6)  VALUE 'IHO  E'.  ELTTHERP
00299        10  FILLER                      PIC X(6)  VALUE 'MPTO E'.  ELTTHERP
00300      05  WS-MISC-OP-PROF-LIST   REDEFINES   WS-MISC-OP-PROF-TAB   ELTTHERP
00301                                        PIC X(6)  OCCURS  6 TIMES. ELTTHERP
00302 /          D I S P L A Y   L I N E S                              ELTTHERP
00303  01  WS-ELS-DISPLAY-LINES.                                        ELTTHERP
00304    05  WS-HDR-2-PHYS-IP-INST.                                     ELTTHERP
00305      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHERP
00306      10  FILLER                    PIC X(49)                      ELTTHERP
00307         VALUE 'PHYSICAL THERAPY SERVICES INPATIENT INSTITUTIONAL'.ELTTHERP
00308      10  FILLER                    PIC X(15) VALUE LOW-VALUES.    ELTTHERP
00309                                                                   ELTTHERP
00310    05  WS-HDR-2-PHYS-OP-INST.                                     ELTTHERP
00311      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHERP
00312      10  FILLER                    PIC X(50)                      ELTTHERP
00313        VALUE 'PHYSICAL THERAPY SERVICES OUTPATIENT INSTITUTIONAL'.ELTTHERP
00314      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTTHERP
00315                                                                   ELTTHERP
00316    05  WS-HDR-2-PHYS-IP-PROF.                                     ELTTHERP
00317      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHERP
00318      10  FILLER                    PIC X(48)                      ELTTHERP
00319         VALUE 'PHYSICAL THERAPY SERVICES INPATIENT PROFESSIONAL'. ELTTHERP
00320      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTTHERP
00321                                                                   ELTTHERP
00322    05  WS-HDR-2-PHYS-OP-PROF.                                     ELTTHERP
00323      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHERP
00324      10  FILLER                    PIC X(49)                      ELTTHERP
00325        VALUE 'PHYSICAL THERAPY SERVICES OUTPATIENT PROFESSIONAL'. ELTTHERP
00326      10  FILLER                    PIC X(15) VALUE LOW-VALUES.    ELTTHERP
00327                                                                   ELTTHERP
00328    05  WS-HDR-2-CHEMO-IP-INST.                                    ELTTHERP
00329      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHERP
00330      10  FILLER                    PIC X(46)                      ELTTHERP
00331            VALUE 'CHEMOTHERAPY INPATIENT INSTITUTIONAL'.          ELTTHERP
00332      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00333                                                                   ELTTHERP
00334    05  WS-HDR-2-CHEMO-OP-INST.                                    ELTTHERP
00335      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHERP
00336      10  FILLER                    PIC X(47)                      ELTTHERP
00337           VALUE 'CHEMOTHERAPY OUTPATIENT INSTITUTIONAL'.          ELTTHERP
00338      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTTHERP
00339                                                                   ELTTHERP
00340    05  WS-HDR-2-CHEMO-IP-PROF.                                    ELTTHERP
00341      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHERP
00342      10  FILLER                    PIC X(45)                      ELTTHERP
00343             VALUE 'CHEMOTHERAPY INPATIENT PROFESSIONAL'.          ELTTHERP
00344      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00345                                                                   ELTTHERP
00346    05  WS-HDR-2-CHEMO-OP-PROF.                                    ELTTHERP
00347      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHERP
00348      10  FILLER                    PIC X(46)                      ELTTHERP
00349            VALUE 'CHEMOTHERAPY OUTPATIENT PROFESSIONAL'.          ELTTHERP
00350      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00351                                                                   ELTTHERP
00352    05  WS-HDR-2-SHOCK-IP-INST.                                    ELTTHERP
00353      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHERP
00354      10  FILLER                    PIC X(37)                      ELTTHERP
00355                     VALUE 'SHOCK THERAPY INPATIENT INSTITUTIONAL'.ELTTHERP
00356      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTTHERP
00357                                                                   ELTTHERP
00358    05  WS-HDR-2-SHOCK-OP-INST.                                    ELTTHERP
00359      10  FILLER                    PIC X(20) VALUE SPACES.        ELTTHERP
00360      10  FILLER                    PIC X(38)                      ELTTHERP
00361                    VALUE 'SHOCK THERAPY OUTPATIENT INSTITUTIONAL'.ELTTHERP
00362      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTTHERP
00363                                                                   ELTTHERP
00364    05  WS-HDR-2-SHOCK-IP-PROF.                                    ELTTHERP
00365      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHERP
00366      10  FILLER                    PIC X(36)                      ELTTHERP
00367                     VALUE 'SHOCK THERAPY INPATIENT PROFESSIONAL'. ELTTHERP
00368      10  FILLER                    PIC X(22) VALUE LOW-VALUES.    ELTTHERP
00369                                                                   ELTTHERP
00370    05  WS-HDR-2-SHOCK-OP-PROF.                                    ELTTHERP
00371      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHERP
00372      10  FILLER                    PIC X(37)                      ELTTHERP
00373                     VALUE 'SHOCK THERAPY OUTPATIENT PROFESSIONAL'.ELTTHERP
00374      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTTHERP
00375                                                                   ELTTHERP
00376    05  WS-HDR-2-MISC-IP-INST.                                     ELTTHERP
00377      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHERP
00378      10  FILLER                    PIC X(45)                      ELTTHERP
00379             VALUE 'MISCELLANEOUS THERAPY INPATIENT INSTITUTIONAL'.ELTTHERP
00380      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00381                                                                   ELTTHERP
00382    05  WS-HDR-2-MISC-OP-INST.                                     ELTTHERP
00383      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHERP
00384      10  FILLER                    PIC X(46)                      ELTTHERP
00385            VALUE 'MISCELLANEOUS THERAPY OUTPATIENT INSTITUTIONAL'.ELTTHERP
00386      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00387                                                                   ELTTHERP
00388    05  WS-HDR-2-MISC-IP-PROF.                                     ELTTHERP
00389      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHERP
00390      10  FILLER                    PIC X(44)                      ELTTHERP
00391             VALUE 'MISCELLANEOUS THERAPY INPATIENT PROFESSIONAL'. ELTTHERP
00392      10  FILLER                    PIC X(18) VALUE LOW-VALUES.    ELTTHERP
00393                                                                   ELTTHERP
00394    05  WS-HDR-2-MISC-OP-PROF.                                     ELTTHERP
00395      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHERP
00396      10  FILLER                    PIC X(45)                      ELTTHERP
00397             VALUE 'MISCELLANEOUS THERAPY OUTPATIENT PROFESSIONAL'.ELTTHERP
00398      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHERP
00399                                                                   ELTTHERP
00400    05  WS-PHYS-THERAPY-SERVICES    PIC X(26)                      ELTTHERP
00401                            VALUE 'PHYSICAL THERAPY SERVICES:'.    ELTTHERP
00402                                                                   ELTTHERP
00403    05  WS-CHEMO-IP-SERVICES        PIC X(42)                      ELTTHERP
00404            VALUE 'CHEMOTHERAPY INPATIENT SERVICES:'.              ELTTHERP
00405                                                                   ELTTHERP
00406    05  WS-CHEMO-OP-SERVICES        PIC X(43)                      ELTTHERP
00407           VALUE 'CHEMOTHERAPY OUTPATIENT SERVICES:'.              ELTTHERP
00408                                                                   ELTTHERP
00409    05  WS-SHOCK-IP-SERVICES        PIC X(33)                      ELTTHERP
00410                     VALUE 'SHOCK THERAPY INPATIENT SERVICES:'.    ELTTHERP
00411                                                                   ELTTHERP
00412    05  WS-SHOCK-OP-SERVICES        PIC X(34)                      ELTTHERP
00413                    VALUE 'SHOCK THERAPY OUTPATIENT SERVICES:'.    ELTTHERP
00414                                                                   ELTTHERP
00415    05  WS-MISC-SERVICES            PIC X(31)                      ELTTHERP
00416                       VALUE 'MISCELLANEOUS THERAPY SERVICES:'.    ELTTHERP
00417                                                                   ELTTHERP
00418    05  WS-COMPARISON-INJURY.                                      ELTTHERP
00419      10  FILLER                    PIC X(48)                      ELTTHERP
00420          VALUE 'A COMPARISON OF THE INJURY TO THE EFFECTIVE DATE'.ELTTHERP
00421      10  FILLER                    PIC X(11)                      ELTTHERP
00422          VALUE ' INDICATES:'.                                     ELTTHERP
00423      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTTHERP
00424                                                                   ELTTHERP
00425                                                                   ELTTHERP
00426    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTTHERP
00427          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTTHERP
00428                                                                   ELTTHERP
00429    05  WS-FOLLOWING-BEN.                                          ELTTHERP
00430      10  FILLER                  PIC  X(21) VALUE                 ELTTHERP
00431            'COVERED SERVICES ARE:'.                               ELTTHERP
00432                                                                   ELTTHERP
00433    05  WS-PAYABLE-AS               PIC X(40) VALUE                ELTTHERP
00434          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTTHERP
00435                                                                   ELTTHERP
00436    05  WS-PAYMNT-BASED             PIC X(20)                      ELTTHERP
00437          VALUE 'PAYMENT IS BASED ON:'.                            ELTTHERP
00438                                                                   ELTTHERP
00439    05  WS-ELIG-METHOD-OF-TREAT     PIC X(35)                      ELTTHERP
00440                       VALUE 'THE ELIGIBLE METHOD OF TREATMENT IS'.ELTTHERP
00441                                                                   ELTTHERP
00442    05  WS-MAX-NUM-OF-VISITS        PIC X(32)                      ELTTHERP
00443                          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE'.ELTTHERP
00444                                                                   ELTTHERP
00445    05  WS-MAX-AMT-PER-VISIT        PIC X(28)                      ELTTHERP
00446                             VALUE 'THE MAXIMUM AMOUNT PER VISIT'. ELTTHERP
00447                                                                   ELTTHERP
00448    05  WS-PRIOR-ADM.                                              ELTTHERP
00449        10  FILLER                  PIC X(55) VALUE                ELTTHERP
00450            'PRIOR INPATIENT ADMISSION REQUIRED FOR THIS SERVICE ANELTTHERP
00451 -          'D'.                                                   ELTTHERP
00452                                                                   ELTTHERP
00453    05  WS-PROF-INPT-CHRGES.                                       ELTTHERP
00454        10  FILLER                  PIC  X(61) VALUE               ELTTHERP
00455            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTTHERP
00456 -          'EPORT: '.                                             ELTTHERP
00457        10  FILLER                  PIC  X(18) VALUE LOW-VALUES.   ELTTHERP
00458                                                                   ELTTHERP
00459    05  WS-PROF-OUTPT-CHRGES.                                      ELTTHERP
00460        10  FILLER                  PIC  X(62) VALUE               ELTTHERP
00461            'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE ELTTHERP
00462 -          'REPORT: '.                                            ELTTHERP
00463        10  FILLER                  PIC  X(17) VALUE LOW-VALUES.   ELTTHERP
00464                                                                   ELTTHERP
00465    05  WS-HOLD-PROF-CHRG-MSG       PIC  X(79) VALUE LOW-VALUES.   ELTTHERP
00466                                                                   ELTTHERP
00467    05  WS-SAME-PROVIDER-RADIATN    PIC X(57)  VALUE               ELTTHERP
00468       'IF THE SAME PROVIDER IS BILLING RADIATION THERAPY/MEDICAL'.ELTTHERP
00469 *   |9876+4321|9876+4321|9876+4321|9876+4321|9876+4321|9876+4321| ELTTHERP
00470 *   6         5         4         3         2         1           ELTTHERP
00471 *  WS-MAX-DAYS CREATED BY RGO, SO THAT THIS TOPIC LOOKS LIKE      ELTTHERP
00472 *    ELTOBREL.                                                    ELTTHERP
00473    05  WS-MAX-DAYS.                                               ELTTHERP
00474      10  FILLER                PIC X(01).                         ELTTHERP
00475      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTTHERP
00476      10  FILLER                PIC X(01).                         ELTTHERP
00477      10  WS-DAYS-LITERAL       PIC X(07) VALUE 'VISITS '.         ELTTHERP
00478      10  FILLER                PIC X(01).                         ELTTHERP
00479      10  WS-DTL-MAX-IND        PIC X(66).                         ELTTHERP
00480                                                                   ELTTHERP
00481                                                                   ELTTHERP
00482    05  WS-BASIC.                                                  ELTTHERP
00483      10  WS-BASIC-LIT              PIC X(07)                      ELTTHERP
00484         VALUE 'BASIC: '.                                          ELTTHERP
00485      10  WS-DTL-BASIC-LONG.                                       ELTTHERP
00486        15  WS-BASIC-AMOUNT         PIC ZZ9.99-.                   ELTTHERP
00487        15  FILLER                  PIC X(5) VALUE SPACES.         ELTTHERP
00488      10  FILLER       REDEFINES    WS-DTL-BASIC-LONG.             ELTTHERP
00489        15  WS-BASIC-DAYS           PIC ZZ9.                       ELTTHERP
00490        15  FILLER                  PIC X(9).                      ELTTHERP
00491                                                                   ELTTHERP
00492    05  WS-SUPPLEMENTAL.                                           ELTTHERP
00493      10  WS-SUPP-LIT               PIC X(16)                      ELTTHERP
00494          VALUE 'SUPPLEMENTAL: '.                                  ELTTHERP
00495      10  WS-DTL-SUPP-LONG.                                        ELTTHERP
00496        15  WS-SUPP-AMOUNT          PIC ZZ9.99-.                   ELTTHERP
00497        15  FILLER                  PIC X(5) VALUE SPACES.         ELTTHERP
00498      10  FILLER       REDEFINES    WS-DTL-SUPP-LONG.              ELTTHERP
00499        15  WS-SUPP-DAYS            PIC ZZ9.                       ELTTHERP
00500        15  FILLER                  PIC X(9).                      ELTTHERP
00501                                                                   ELTTHERP
00502    05  WS-BASIC-DAYS-REDUCT-1ST.                                  ELTTHERP
00503      10  FILLER                    PIC X(2) VALUE SPACES.         ELTTHERP
00504      10  FILLER                    PIC X(32)                      ELTTHERP
00505                          VALUE 'BASIC DAY REDUCTION RATIO BASIC '.ELTTHERP
00506      10  WS-BASIC-DAYS-1ST         PIC ZZ9.                       ELTTHERP
00507      10  FILLER                    PIC X(5)  VALUE ' FOR '.       ELTTHERP
00508      10  WS-BASIC-DAYS-FOR-1ST     PIC ZZ9.                       ELTTHERP
00509      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTTHERP
00510                                                                   ELTTHERP
00511    05  WS-BASIC-DAYS-REDUCT-2ND.                                  ELTTHERP
00512      10  FILLER                    PIC X(2) VALUE SPACES.         ELTTHERP
00513      10  FILLER                    PIC X(36)                      ELTTHERP
00514                      VALUE 'BASIC DAY REDUCTION RATIO SECONDARY '.ELTTHERP
00515      10  WS-BASIC-DAYS-2ND         PIC ZZ9.                       ELTTHERP
00516      10  FILLER                    PIC X(5)  VALUE ' FOR '.       ELTTHERP
00517      10  WS-BASIC-DAYS-FOR-2ND     PIC ZZ9.                       ELTTHERP
00518      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTTHERP
00519                                                                   ELTTHERP
00520    05  WS-FOR-BASIC                PIC X(13)                      ELTTHERP
00521         VALUE '  FOR BASIC: '.                                    ELTTHERP
00522                                                                   ELTTHERP
00523    05  WS-FOR-SUPP                 PIC X(20)                      ELTTHERP
00524         VALUE '  FOR SUPPLEMENTAL: '.                             ELTTHERP
00525                                                                   ELTTHERP
00526    05  WS-AND-MUST-BEGIN.                                         ELTTHERP
00527      10  FILLER                    PIC X(40)                      ELTTHERP
00528                  VALUE ' FOR THIS SERVICE AND MUST BEGIN WITHIN '.ELTTHERP
00529      10  WS-MUST-BEGIN-DAYS        PIC ZZ9.                       ELTTHERP
00530      10  FILLER                    PIC X(20)                      ELTTHERP
00531                                      VALUE ' DAYS FROM DISCHARGE'.ELTTHERP
00532                                                                   ELTTHERP
00533    05  WS-THE                PIC X(04)                            ELTTHERP
00534          VALUE 'THE:'.                                            ELTTHERP
00535    05  WS-BASIC-THE                PIC X(20)                      ELTTHERP
00536          VALUE '         BASIC: THE '.                            ELTTHERP
00537                                                                   ELTTHERP
00538    05  WS-SUPP-THE                 PIC X(20)                      ELTTHERP
00539          VALUE '  SUPPLEMENTAL: THE '.                            ELTTHERP
00540                                                                   ELTTHERP
00541    05  WS-BASIC-MAX.                                              ELTTHERP
00542      10  FILLER                    PIC X(09) VALUE SPACES.        ELTTHERP
00543      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTTHERP
00544      10  WS-BASIC-MAX-AMT          PIC $$$9.                      ELTTHERP
00545      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTTHERP
00546                                                                   ELTTHERP
00547    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTTHERP
00548        VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTTHERP
00549                                                                   ELTTHERP
00550    05  WS-SUPP-MAX.                                               ELTTHERP
00551      10  FILLER                    PIC X(16)                      ELTTHERP
00552          VALUE '  SUPPLEMENTAL: '.                                ELTTHERP
00553      10  WS-SUPP-MAX-AMT           PIC ZZ9.                       ELTTHERP
00554      10  FILLER                    PIC X(60) VALUE LOW-VALUES.    ELTTHERP
00555                                                                   ELTTHERP
00556    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER '. ELTTHERP
00557                                                                   ELTTHERP
00558    05  WS-MAXIMUM-AMT-PER.                                        ELTTHERP
00559      10  FILLER                    PIC X(29)                      ELTTHERP
00560        VALUE 'THE MAXIMUM AMOUNT PER VISIT '.                     ELTTHERP
00561      10  FILLER                    PIC X(50) VALUE LOW-VALUES.    ELTTHERP
00562                                                                   ELTTHERP
00563    05  WS-FOR-SUPP-ACCIDENT.                                      ELTTHERP
00564      10  FILLER                    PIC X(26)                      ELTTHERP
00565        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTTHERP
00566      10  WS-NO-DAYS                PIC ZZ9.                       ELTTHERP
00567      10  WS-FOR-SUPP-ACC-DESC      PIC X(50).                     ELTTHERP
00568                                                                   ELTTHERP
00569    05  WS-CONTRACT-RELATED.                                       ELTTHERP
00570      10  FILLER                    PIC X(49)   VALUE              ELTTHERP
00571           'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS.'.    ELTTHERP
00572                                                                   ELTTHERP
00573    05  WS-PROVIDER-ELIGIBILITY.                                   ELTTHERP
00574      10  FILLER                    PIC X(44)   VALUE              ELTTHERP
00575        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTTHERP
00576                                                                   ELTTHERP
00577    05  WS-ACCUM-MSG1.                                             ELTTHERP
00578      10  FILLER                  PIC  X(79) VALUE                 ELTTHERP
00579      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTTHERP
00580 -    'CONSIDERATIONS.'.                                           ELTTHERP
00581                                                                   ELTTHERP
00582    05  WS-NO-TABULAR1.                                            ELTTHERP
00583      10  FILLER                    PIC X(51)  VALUE               ELTTHERP
00584         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTTHERP
00585      10  FILLER                    PIC X(22)  VALUE               ELTTHERP
00586         'GOING FROM BENEFIT ***'.                                 ELTTHERP
00587                                                                   ELTTHERP
00588    05  WS-NO-TABULAR2.                                            ELTTHERP
00589      10  FILLER                    PIC X(15)  VALUE               ELTTHERP
00590         '*** PROVISION: '.                                        ELTTHERP
00591      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTTHERP
00592      10  FILLER                    PIC X VALUE SPACE.             ELTTHERP
00593      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTTHERP
00594      10  FILLER                    PIC X(13)  VALUE               ELTTHERP
00595         ' TO TABULAR: '.                                          ELTTHERP
00596      10  WS-NO-TAB-ID              PIC X(6).                      ELTTHERP
00597      10  FILLER                    PIC X VALUE SPACE.             ELTTHERP
00598      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTTHERP
00599      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTTHERP
00600                                                                   ELTTHERP
00601    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTTHERP
00602    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTTHERP
00603      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTTHERP
00604    05  WS-TEMP-TEXT-AREA2.                                        ELTTHERP
00605      10  WS-TEMP2-CHARS            PIC X(5)    VALUE LOW-VALUES.  ELTTHERP
00606      10  FILLER                    PIC X(74).                     ELTTHERP
00607                                                                   ELTTHERP
00608  01  WS-END                            PIC X(16)  VALUE           ELTTHERP
00609      '*** W/S ENDS ***'.                                          ELTTHERP
00610 /             L I N K A G E   S E C T I O N                       ELTTHERP
00611  LINKAGE SECTION.                                                 ELTTHERP
00612  01  DFHCOMMAREA.                                                 ELTTHERP
00613      COPY ELSCOMMC.                                               ELTTHERP
00614 /                                                                 ELTTHERP
00615      COPY ELSCIA2C.                                               ELTTHERP
00616 /                                                                 ELTTHERP
00617      COPY ELSIOPMC.                                               ELTTHERP
00618 /                                                                 ELTTHERP
00619      COPY ELSKEYSC.                                               ELTTHERP
00620 /                                                                 ELTTHERP
00621      COPY ELSOUTPC.                                               ELTTHERP
00622 /                                                                 ELTTHERP
00623      COPY ELSSSCBC.                                               ELTTHERP
00624 /                                                                 ELTTHERP
00625      COPY ELSCMIFC.                                               ELTTHERP
00626 /                                                                 ELTTHERP
00627      COPY ELSCMDSC.                                               ELTTHERP
00628 /                                                                 ELTTHERP
00629      COPY ELSPRVNC.                                               ELTTHERP
00630 /                                                                 ELTTHERP
00631 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTTHERP
00632      COPY ELSPLGSW.                                               ELTTHERP
00633 *** BENEFIT PROVISION TABLE OF FLDS                               ELTTHERP
00634      COPY ELSPLGTB.                                               ELTTHERP
00635 /                                                                 ELTTHERP
00636      COPY ELSTCWAC.                                               ELTTHERP
00637 /                                                                 ELTTHERP
00638 /                                                                 ELTTHERP
00639 /        G R O U P   S P E C I F I C   R E C O R D                ELTTHERP
00640  01  GROUP-SPECIFIC-RECORD.                                       ELTTHERP
00641      COPY GCGROUPC.                                               ELTTHERP
00642 /        C O N T R A C T   R E C O R D                            ELTTHERP
00643  01  CONTRACT-RECORD.                                             ELTTHERP
00644      COPY GCCONTRC.                                               ELTTHERP
00645 /                  M A I N L I N E                                ELTTHERP
00646  PROCEDURE DIVISION.                                              ELTTHERP
00647                                                                   ELTTHERP
00648 ******************************************************************ELTTHERP
00649 *                                                                 ELTTHERP
00650 *   PERFORM THE MAINLINE OPERATIONS.                              ELTTHERP
00651 *                                                                 ELTTHERP
00652 ******************************************************************ELTTHERP
00653  0000-MAINLINE.                                                   ELTTHERP
00654 ****************************************************************  ELTTHERP
00655 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTTHERP
00656 ****************************************************************  ELTTHERP
00657                                                                   ELTTHERP
00658      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTTHERP
00659          EXEC CICS ABEND                                          ELTTHERP
00660                    ABCODE ('EL01')                                ELTTHERP
00661          END-EXEC                                                 ELTTHERP
00662      END-IF.                                                      ELTTHERP
00663                                                                   ELTTHERP
00664 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTTHERP
00665                                                                   ELTTHERP
00666                                                                   ELTTHERP
00667      PERFORM 0002-SET-ADR-OF-CIA-ELS-COM.                         ELTTHERP
00668      PERFORM 0004-SET-ADR-OF-SSB-SEL-STAT.                        ELTTHERP
00669      PERFORM 0006-SET-ADR-OF-COF-OUT-INT.                         ELTTHERP
00670      PERFORM 0008-SET-ADR-OF-KWA-FILE-KEY.                        ELTTHERP
00671      PERFORM 0010-SET-ADR-OF-CMF-COD-MAN.                         ELTTHERP
00672      PERFORM 0012-SET-ADR-OF-TCAR-COMP-WRK.                       ELTTHERP
00673      PERFORM 0014-SET-ADR-OF-GRP-SPEC-REC.                        ELTTHERP
00674      PERFORM 0016-SET-ADR-OF-PLS-PAY-LVL.                         ELTTHERP
00675                                                                   ELTTHERP
00676      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTTHERP
00677                                                                   ELTTHERP
00678      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTTHERP
00679              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTTHERP
00680                                                                   ELTTHERP
00681      SET CIA-STG-GETMAIN TO TRUE.                                 ELTTHERP
00682      EXEC CICS LINK                                               ELTTHERP
00683                PROGRAM('ELUSTGMG')                                ELTTHERP
00684                COMMAREA(DFHCOMMAREA)                              ELTTHERP
00685      END-EXEC.                                                    ELTTHERP
00686                                                                   ELTTHERP
00687      PERFORM 0018-SET-ADR-OF-PVN-BEN-PROV.                        ELTTHERP
00688      PERFORM 0100-CHECK-LOB.                                      ELTTHERP
00689                                                                   ELTTHERP
00690      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTTHERP
00691                                                                   ELTTHERP
00692                                                                   ELTTHERP
00693      IF SSB-SUB-TOPIC = 'CARDIAC         '                        ELTTHERP
00694         OR SSB-SUB-TOPIC = 'RADIATION       '                     ELTTHERP
00695         OR SSB-SUB-TOPIC = 'SPEECH          '                     ELTTHERP
00696             EXEC CICS LINK                                        ELTTHERP
00697                       PROGRAM('ELTTHER2')                         ELTTHERP
00698                       COMMAREA(DFHCOMMAREA)                       ELTTHERP
00699             END-EXEC                                              ELTTHERP
00700             GOBACK                                                ELTTHERP
00701      END-IF.                                                      ELTTHERP
00702      IF SSB-SUB-TOPIC = 'PT              '                        ELTTHERP
00703          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00704             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00705                 PERFORM 1000-PHYS-THERP-IP-INST-RTNE              ELTTHERP
00706                         THRU 1099-EXIT                            ELTTHERP
00707             END-IF                                                ELTTHERP
00708             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00709                 PERFORM 1200-PHYS-THERP-OP-INST-RTNE              ELTTHERP
00710                         THRU 1299-EXIT                            ELTTHERP
00711             END-IF                                                ELTTHERP
00712          END-IF                                                   ELTTHERP
00713          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00714             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00715                 PERFORM 1400-PHYS-THERP-IP-PROF-RTNE              ELTTHERP
00716                         THRU 1499-EXIT                            ELTTHERP
00717             END-IF                                                ELTTHERP
00718             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00719                 PERFORM 1600-PHYS-THERP-OP-PROF-RTNE              ELTTHERP
00720                         THRU 1699-EXIT                            ELTTHERP
00721             END-IF                                                ELTTHERP
00722          END-IF                                                   ELTTHERP
00723      END-IF.                                                      ELTTHERP
00724                                                                   ELTTHERP
00725      IF SSB-SUB-TOPIC = 'CHEMO           '                        ELTTHERP
00726          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00727             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00728                 PERFORM 2000-CHEMO-THRP-IP-INST-RTNE              ELTTHERP
00729                         THRU 2099-EXIT                            ELTTHERP
00730             END-IF                                                ELTTHERP
00731             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00732                 PERFORM 2200-CHEMO-THRP-OP-INST-RTNE              ELTTHERP
00733                         THRU 2299-EXIT                            ELTTHERP
00734             END-IF                                                ELTTHERP
00735          END-IF                                                   ELTTHERP
00736          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00737             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00738                 PERFORM 2400-CHEMO-THRP-IP-PROF-RTNE              ELTTHERP
00739                         THRU 2499-EXIT                            ELTTHERP
00740             END-IF                                                ELTTHERP
00741             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00742                 PERFORM 2600-CHEMO-THRP-OP-PROF-RTNE              ELTTHERP
00743                         THRU 2699-EXIT                            ELTTHERP
00744             END-IF                                                ELTTHERP
00745          END-IF                                                   ELTTHERP
00746      END-IF.                                                      ELTTHERP
00747                                                                   ELTTHERP
00748                                                                   ELTTHERP
00749      IF SSB-SUB-TOPIC = 'SHOCK           '                        ELTTHERP
00750          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00751             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00752                 PERFORM 5000-SHOCK-THRP-IP-INST-RTNE              ELTTHERP
00753                         THRU 5099-EXIT                            ELTTHERP
00754             END-IF                                                ELTTHERP
00755             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00756                 PERFORM 5200-SHOCK-THRP-OP-INST-RTNE              ELTTHERP
00757                         THRU 5299-EXIT                            ELTTHERP
00758             END-IF                                                ELTTHERP
00759          END-IF                                                   ELTTHERP
00760          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00761             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00762                 PERFORM 5400-SHOCK-THRP-IP-PROF-RTNE              ELTTHERP
00763                         THRU 5499-EXIT                            ELTTHERP
00764             END-IF                                                ELTTHERP
00765             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00766                 PERFORM 5600-SHOCK-THRP-OP-PROF-RTNE              ELTTHERP
00767                         THRU 5699-EXIT                            ELTTHERP
00768             END-IF                                                ELTTHERP
00769          END-IF                                                   ELTTHERP
00770      END-IF.                                                      ELTTHERP
00771                                                                   ELTTHERP
00772                                                                   ELTTHERP
00773      IF SSB-SUB-TOPIC = 'OTHER           '                        ELTTHERP
00774          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00775             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00776                 PERFORM 6000-MISC-THERP-IP-INST-RTNE              ELTTHERP
00777                         THRU 6099-EXIT                            ELTTHERP
00778             END-IF                                                ELTTHERP
00779             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00780                 PERFORM 6200-MISC-THERP-OP-INST-RTNE              ELTTHERP
00781                         THRU 6299-EXIT                            ELTTHERP
00782             END-IF                                                ELTTHERP
00783          END-IF                                                   ELTTHERP
00784          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHERP
00785             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00786                 PERFORM 6400-MISC-THERP-IP-PROF-RTNE              ELTTHERP
00787                         THRU 6499-EXIT                            ELTTHERP
00788             END-IF                                                ELTTHERP
00789             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHERP
00790                 PERFORM 6600-MISC-THERP-OP-PROF-RTNE              ELTTHERP
00791                         THRU 6699-EXIT                            ELTTHERP
00792             END-IF                                                ELTTHERP
00793          END-IF                                                   ELTTHERP
00794      END-IF.                                                      ELTTHERP
00795                                                                   ELTTHERP
00796      IF (SSB-SUB-TOPIC = 'PT              '  OR                   ELTTHERP
00797                          'CHEMO           '  OR                   ELTTHERP
00798                          'SHOCK           '  OR                   ELTTHERP
00799                          'OTHER           ')            AND       ELTTHERP
00800          (SSB-PROV-CLASS-INST   OR                                ELTTHERP
00801           SSB-PROV-CLASS-PROF   OR                                ELTTHERP
00802           SSB-PROV-CLASS-BOTH)                          AND       ELTTHERP
00803          (SSB-SERV-CLASS-IP     OR                                ELTTHERP
00804           SSB-SERV-CLASS-OP     OR                                ELTTHERP
00805           SSB-SERV-CLASS-BOTH)                                    ELTTHERP
00806            CONTINUE                                               ELTTHERP
00807      ELSE                                                         ELTTHERP
00808          SET CIA-AB-UNDEF TO TRUE                                 ELTTHERP
00809          EXEC CICS ABEND                                          ELTTHERP
00810                    ABCODE(CIA-ABCODE)                             ELTTHERP
00811          END-EXEC                                                 ELTTHERP
00812      END-IF.                                                      ELTTHERP
00813                                                                   ELTTHERP
00814      MOVE 'E'  TO  COF-FUNCTION.                                  ELTTHERP
00815      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTTHERP
00816                     COF-NBR-DTL-LINES.                            ELTTHERP
00817      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
00818             END-EXEC.                                             ELTTHERP
00819                                                                   ELTTHERP
00820      EXEC CICS RETURN   END-EXEC.                                 ELTTHERP
00821                                                                   ELTTHERP
00822      GOBACK.                                                      ELTTHERP
00823  0002-SET-ADR-OF-CIA-ELS-COM.                                     ELTTHERP
00824      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTTHERP
00825          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTTHERP
00826                                                                   ELTTHERP
00827  0004-SET-ADR-OF-SSB-SEL-STAT.                                    ELTTHERP
00828      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTTHERP
00829      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00830          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTTHERP
00831                                                                   ELTTHERP
00832  0006-SET-ADR-OF-COF-OUT-INT.                                     ELTTHERP
00833      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTTHERP
00834      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00835          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTTHERP
00836                                                                   ELTTHERP
00837  0008-SET-ADR-OF-KWA-FILE-KEY.                                    ELTTHERP
00838      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTTHERP
00839      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00840          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTTHERP
00841                                                                   ELTTHERP
00842  0010-SET-ADR-OF-CMF-COD-MAN.                                     ELTTHERP
00843      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTTHERP
00844      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00845          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTTHERP
00846                                                                   ELTTHERP
00847  0012-SET-ADR-OF-TCAR-COMP-WRK.                                   ELTTHERP
00848      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTTHERP
00849      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00850          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTTHERP
00851                                                                   ELTTHERP
00852  0014-SET-ADR-OF-GRP-SPEC-REC.                                    ELTTHERP
00853      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTTHERP
00854      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00855          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTTHERP
00856                                                                   ELTTHERP
00857  0016-SET-ADR-OF-PLS-PAY-LVL.                                     ELTTHERP
00858      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTTHERP
00859      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00860          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTTHERP
00861                                                                   ELTTHERP
00862  0018-SET-ADR-OF-PVN-BEN-PROV.                                    ELTTHERP
00863      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTTHERP
00864      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00865          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTTHERP
00866                                                                   ELTTHERP
00867  0020-SET-ADR-OF-PLT-PAY-LVL.                                     ELTTHERP
00868      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTTHERP
00869      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00870          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTTHERP
00871                                                                   ELTTHERP
00872                                                                   ELTTHERP
00873 ****************************************************************  ELTTHERP
00874 * 0100-CHECK-LOB   CHECK ANY OF THE CONTRACT RECORDS TO SEE    *  ELTTHERP
00875 *      IF THE LOB EQUALS '1,','2' OR '3'.                      *  ELTTHERP
00876 *      IF SO, THEN WE WILL DISPLAY 'BASIC:' AND/OR             *  ELTTHERP
00877 *         'SUPPLEMENTAL:' ON THE SCREEN LATER ON.              *  ELTTHERP
00878 ****************************************************************  ELTTHERP
00879  0100-CHECK-LOB.                                                  ELTTHERP
00880                                                                   ELTTHERP
00881      MOVE 'N' TO DISPLAY-BAS-SUP.                                 ELTTHERP
00882                                                                   ELTTHERP
00883      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTTHERP
00884      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00885                      ADDRESS OF CONTRACT-RECORD.                  ELTTHERP
00886                                                                   ELTTHERP
00887      IF CIA-RC-OK                                                 ELTTHERP
00888         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHERP
00889             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHERP
00890                                                                   ELTTHERP
00891      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTTHERP
00892      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00893                      ADDRESS OF CONTRACT-RECORD.                  ELTTHERP
00894                                                                   ELTTHERP
00895      IF CIA-RC-OK                                                 ELTTHERP
00896         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHERP
00897             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHERP
00898                                                                   ELTTHERP
00899      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTTHERP
00900      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00901                      ADDRESS OF CONTRACT-RECORD.                  ELTTHERP
00902                                                                   ELTTHERP
00903      IF CIA-RC-OK                                                 ELTTHERP
00904         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHERP
00905             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHERP
00906                                                                   ELTTHERP
00907      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTTHERP
00908      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
00909                      ADDRESS OF CONTRACT-RECORD.                  ELTTHERP
00910                                                                   ELTTHERP
00911      IF CIA-RC-OK                                                 ELTTHERP
00912         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHERP
00913             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHERP
00914 /     P H Y S    T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHERP
00915 ***************************************************************** ELTTHERP
00916 *     P H Y S    T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHERP
00917 *                                                                 ELTTHERP
00918 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHERP
00919 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHERP
00920 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHERP
00921 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHERP
00922 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHERP
00923 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHERP
00924 *  MODULE.                                                        ELTTHERP
00925 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHERP
00926 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHERP
00927 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHERP
00928 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHERP
00929 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHERP
00930 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHERP
00931 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHERP
00932 *                                                                 ELTTHERP
00933 ***************************************************************** ELTTHERP
00934  1000-PHYS-THERP-IP-INST-RTNE.                                    ELTTHERP
00935      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
00936                                                                   ELTTHERP
00937      MOVE WS-HDR-2-PHYS-IP-INST TO COF-HDR-LINE (2)               ELTTHERP
00938      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
00939      PERFORM WITH TEST BEFORE                                     ELTTHERP
00940              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
00941              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
00942         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
00943         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
00944         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
00945         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
00946      END-PERFORM.                                                 ELTTHERP
00947      MOVE WS-PHYS-IP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
00948                                                                   ELTTHERP
00949                                                                   ELTTHERP
00950      PERFORM WITH TEST BEFORE                                     ELTTHERP
00951         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
00952         UNTIL   WS-SUB  >     WS-PHYS-IP-INST-CNT                 ELTTHERP
00953           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
00954           MOVE WS-PHYS-IP-INST-LIST (WS-SUB)                      ELTTHERP
00955                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
00956            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
00957                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
00958      END-PERFORM.                                                 ELTTHERP
00959                                                                   ELTTHERP
00960      MOVE WS-PHYS-THERAPY-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTTHERP
00961                                                                   ELTTHERP
00962      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
00963      END-EXEC.                                                    ELTTHERP
00964                                                                   ELTTHERP
00965      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
00966      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
00967      END-EXEC.                                                    ELTTHERP
00968                                                                   ELTTHERP
00969      IF PVN-COVG-NONE                                             ELTTHERP
00970         GO TO 1099-EXIT.                                          ELTTHERP
00971                                                                   ELTTHERP
00972      MOVE +1  TO  WS-CIA.                                         ELTTHERP
00973                                                                   ELTTHERP
00974      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
00975            PSP-PROVN-PRICING-METHD,                               ELTTHERP
00976            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
00977            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
00978            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
00979            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
00980            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
00981            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
00982            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
00983            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
00984            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
00985            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
00986            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
00987            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
00988                                                                   ELTTHERP
00989                                                                   ELTTHERP
00990      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
00991                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
00992                     END-EXEC.                                     ELTTHERP
00993                                                                   ELTTHERP
00994      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
00995                                                                   ELTTHERP
00996      PERFORM WITH TEST BEFORE                                     ELTTHERP
00997         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
00998         UNTIL WS-SUB  >  WS-PHYS-IP-INST-CNT                      ELTTHERP
00999              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTTHERP
01000              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTTHERP
01001                   PERFORM 1040-BUILD-SCREEN-LINES THRU 1040-EXIT  ELTTHERP
01002              END-IF                                               ELTTHERP
01003      END-PERFORM.                                                 ELTTHERP
01004                                                                   ELTTHERP
01005      GO TO 1099-EXIT.                                             ELTTHERP
01006                                                                   ELTTHERP
01007 /                                                                 ELTTHERP
01008  1040-BUILD-SCREEN-LINES.                                         ELTTHERP
01009                                                                   ELTTHERP
01010      SET PLT-INDEX1  TO                                           ELTTHERP
01011                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
01012      IF WS-NOT-FIRST-TIME                                         ELTTHERP
01013         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
01014         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
01015         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
01016             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
01017         END-EXEC                                                  ELTTHERP
01018      ELSE                                                         ELTTHERP
01019         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
01020                                                                   ELTTHERP
01021      MOVE 1  TO  WS-CIA.                                          ELTTHERP
01022      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
01023         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
01024            SET PLT-INDEX2  TO  2                                  ELTTHERP
01025         ELSE                                                      ELTTHERP
01026            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
01027            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
01028            GO TO 1040-EXIT                                        ELTTHERP
01029      ELSE                                                         ELTTHERP
01030         SET PLT-INDEX2  TO  1.                                    ELTTHERP
01031                                                                   ELTTHERP
01032 **---------------------------------------------------------------+ELTTHERP
01033 **                                                               |ELTTHERP
01034 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
01035      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
01036      ADD  +1  TO  WS-CIA.                                         ELTTHERP
01037      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
01038                                                                   ELTTHERP
01039      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
01040                                                                   ELTTHERP
01041      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
01042         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
01043         UNTIL  PVN-BEN-PROVN-IDX > WS-PHYS-IP-INST-CNT.           ELTTHERP
01044      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01045                                                                   ELTTHERP
01046      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
01047      MOVE +1  TO  WS-CIA                                          ELTTHERP
01048      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01049              END-EXEC.                                            ELTTHERP
01050 **                                                               |ELTTHERP
01051 **---------------------------------------------------------------+ELTTHERP
01052                                                                   ELTTHERP
01053 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
01054      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
01055 **                                                               |ELTTHERP
01056 **---------------------------------------------------------------+ELTTHERP
01057                                                                   ELTTHERP
01058 **---------------------------------------------------------------+ELTTHERP
01059 **                                                               |ELTTHERP
01060 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
01061 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
01062 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
01063      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
01064 **                                                               |ELTTHERP
01065 **---------------------------------------------------------------+ELTTHERP
01066                                                                   ELTTHERP
01067 **---------------------------------------------------------------+ELTTHERP
01068 **                                                               |ELTTHERP
01069 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHERP
01070 **                  H O S P I T A L   B I L L                    |ELTTHERP
01071      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHERP
01072      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
01073 **                                                               |ELTTHERP
01074 **---------------------------------------------------------------+ELTTHERP
01075                                                                   ELTTHERP
01076 **---------------------------------------------------------------+ELTTHERP
01077 **                                                               |ELTTHERP
01078 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
01079      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
01080 **                                                               |ELTTHERP
01081 **---------------------------------------------------------------+ELTTHERP
01082                                                                   ELTTHERP
01083 **---------------------------------------------------------------+ELTTHERP
01084 **                                                               |ELTTHERP
01085 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
01086      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
01087         THRU 3250-EXIT.                                           ELTTHERP
01088 **                                                               |ELTTHERP
01089 **---------------------------------------------------------------+ELTTHERP
01090                                                                   ELTTHERP
01091 **---------------------------------------------------------------+ELTTHERP
01092 **                                                               |ELTTHERP
01093 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
01094      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
01095 **                                                               |ELTTHERP
01096 **---------------------------------------------------------------+ELTTHERP
01097                                                                   ELTTHERP
01098 **---------------------------------------------------------------+ELTTHERP
01099 **                                                               |ELTTHERP
01100 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
01101 **                         A N D                                 |ELTTHERP
01102 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
01103      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
01104 **                                                               |ELTTHERP
01105 **---------------------------------------------------------------+ELTTHERP
01106                                                                   ELTTHERP
01107 **---------------------------------------------------------------+ELTTHERP
01108 **                                                               |ELTTHERP
01109 **               P E R F O R M   T A B U L A R                   |ELTTHERP
01110      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
01111 **                                                               |ELTTHERP
01112 **---------------------------------------------------------------+ELTTHERP
01113  1040-EXIT.  EXIT.                                                ELTTHERP
01114 /                                                                 ELTTHERP
01115  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTTHERP
01116                                                                   ELTTHERP
01117      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB    ELTTHERP
01118          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX)           =  'B'      ELTTHERP
01119              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTTHERP
01120                                                                   ELTTHERP
01121      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTTHERP
01122         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
01123         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTTHERP
01124         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTTHERP
01125                                                   CMF-CODE-VALUE  ELTTHERP
01126         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTTHERP
01127         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTTHERP
01128         PERFORM 4000-CODES-MANUAL-LONG                            ELTTHERP
01129         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTTHERP
01130         IF WS-CIA  >  20 OR  =  20                                ELTTHERP
01131            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTTHERP
01132            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTTHERP
01133                COMMAREA(DFHCOMMAREA)                              ELTTHERP
01134                 END-EXEC                                          ELTTHERP
01135            MOVE +1  TO  WS-CIA.                                   ELTTHERP
01136                                                                   ELTTHERP
01137  1090-PROBLEM-WITH-INDICES.                                       ELTTHERP
01138                                                                   ELTTHERP
01139      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTTHERP
01140      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
01141      MOVE 'P'  TO  COF-FUNCTION.                                  ELTTHERP
01142                                                                   ELTTHERP
01143      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01144              END-EXEC.                                            ELTTHERP
01145                                                                   ELTTHERP
01146  1099-EXIT.            EXIT.                                      ELTTHERP
01147 /     P H Y S    T H E R A P Y   O P   I N S T I T U T I O N A L  ELTTHERP
01148  1200-PHYS-THERP-OP-INST-RTNE.                                    ELTTHERP
01149      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
01150                                                                   ELTTHERP
01151      MOVE WS-HDR-2-PHYS-OP-INST TO COF-HDR-LINE (2)               ELTTHERP
01152                                                                   ELTTHERP
01153      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
01154      PERFORM WITH TEST BEFORE                                     ELTTHERP
01155              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
01156              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
01157         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
01158         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01159         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01160         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
01161      END-PERFORM.                                                 ELTTHERP
01162      MOVE WS-PHYS-OP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
01163                                                                   ELTTHERP
01164                                                                   ELTTHERP
01165      PERFORM WITH TEST BEFORE                                     ELTTHERP
01166         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
01167         UNTIL   WS-SUB  >     WS-PHYS-OP-INST-CNT                 ELTTHERP
01168           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
01169           MOVE WS-PHYS-OP-INST-LIST (WS-SUB)                      ELTTHERP
01170                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
01171            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
01172                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
01173      END-PERFORM.                                                 ELTTHERP
01174                                                                   ELTTHERP
01175      MOVE WS-PHYS-THERAPY-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTTHERP
01176                                                                   ELTTHERP
01177      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
01178      END-EXEC.                                                    ELTTHERP
01179                                                                   ELTTHERP
01180      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
01181      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01182      END-EXEC.                                                    ELTTHERP
01183                                                                   ELTTHERP
01184      IF PVN-COVG-NONE                                             ELTTHERP
01185         GO TO 1299-EXIT.                                          ELTTHERP
01186                                                                   ELTTHERP
01187      MOVE +1  TO  WS-CIA.                                         ELTTHERP
01188      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
01189            PSP-PROVN-PRICING-METHD,                               ELTTHERP
01190            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
01191            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
01192            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
01193            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
01194            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
01195            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
01196            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
01197            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
01198            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
01199            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
01200            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
01201            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHERP
01202            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHERP
01203            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
01204                                                                   ELTTHERP
01205      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
01206                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
01207                     END-EXEC.                                     ELTTHERP
01208                                                                   ELTTHERP
01209      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
01210                                                                   ELTTHERP
01211      PERFORM 1230-FIND-FIRST-NONZERO                              ELTTHERP
01212         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
01213         UNTIL WS-SUB  >  WS-PHYS-OP-INST-CNT.                     ELTTHERP
01214                                                                   ELTTHERP
01215      GO TO 1299-EXIT.                                             ELTTHERP
01216  1230-FIND-FIRST-NONZERO.                                         ELTTHERP
01217      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01218      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
01219         CONTINUE                                                  ELTTHERP
01220      ELSE                                                         ELTTHERP
01221         PERFORM 1240-BUILD-SCREEN-LINES THRU 1240-EXIT.           ELTTHERP
01222                                                                   ELTTHERP
01223  1240-BUILD-SCREEN-LINES.                                         ELTTHERP
01224                                                                   ELTTHERP
01225      SET PLT-INDEX1  TO                                           ELTTHERP
01226                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
01227      IF WS-NOT-FIRST-TIME                                         ELTTHERP
01228         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
01229         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
01230         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
01231             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
01232              END-EXEC                                             ELTTHERP
01233      ELSE                                                         ELTTHERP
01234         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
01235                                                                   ELTTHERP
01236      MOVE 1  TO  WS-CIA.                                          ELTTHERP
01237      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
01238         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
01239            SET PLT-INDEX2  TO  2                                  ELTTHERP
01240         ELSE                                                      ELTTHERP
01241            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
01242            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
01243            GO TO 1240-EXIT                                        ELTTHERP
01244      ELSE                                                         ELTTHERP
01245         SET PLT-INDEX2  TO  1.                                    ELTTHERP
01246                                                                   ELTTHERP
01247 **---------------------------------------------------------------+ELTTHERP
01248 **                                                               |ELTTHERP
01249 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
01250      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
01251      ADD  +1  TO  WS-CIA.                                         ELTTHERP
01252      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
01253                                                                   ELTTHERP
01254      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
01255                                                                   ELTTHERP
01256      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
01257         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
01258         UNTIL  PVN-BEN-PROVN-IDX > WS-PHYS-OP-INST-CNT.           ELTTHERP
01259      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01260                                                                   ELTTHERP
01261      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
01262      MOVE +1  TO  WS-CIA                                          ELTTHERP
01263      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01264              END-EXEC.                                            ELTTHERP
01265 **                                                               |ELTTHERP
01266 **---------------------------------------------------------------+ELTTHERP
01267                                                                   ELTTHERP
01268 **---------------------------------------------------------------+ELTTHERP
01269 **                                                               |ELTTHERP
01270 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
01271      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
01272 **                                                               |ELTTHERP
01273 **---------------------------------------------------------------+ELTTHERP
01274                                                                   ELTTHERP
01275 **---------------------------------------------------------------+ELTTHERP
01276 **                                                               |ELTTHERP
01277 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
01278 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
01279 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
01280      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
01281 **                                                               |ELTTHERP
01282 **---------------------------------------------------------------+ELTTHERP
01283                                                                   ELTTHERP
01284 **---------------------------------------------------------------+ELTTHERP
01285 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHERP
01286 **                    H O S P I T A L    B I L L                 |ELTTHERP
01287      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHERP
01288      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
01289 **                                                               |ELTTHERP
01290 **---------------------------------------------------------------+ELTTHERP
01291                                                                   ELTTHERP
01292 **---------------------------------------------------------------+ELTTHERP
01293 **                                                               |ELTTHERP
01294 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
01295      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
01296 **                                                               |ELTTHERP
01297 **---------------------------------------------------------------+ELTTHERP
01298                                                                   ELTTHERP
01299 **---------------------------------------------------------------+ELTTHERP
01300 **                                                               |ELTTHERP
01301 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
01302      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
01303         THRU 3250-EXIT.                                           ELTTHERP
01304 **                                                               |ELTTHERP
01305 **---------------------------------------------------------------+ELTTHERP
01306                                                                   ELTTHERP
01307 **---------------------------------------------------------------+ELTTHERP
01308 **                                                               |ELTTHERP
01309 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
01310      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
01311 **                                                               |ELTTHERP
01312 **---------------------------------------------------------------+ELTTHERP
01313                                                                   ELTTHERP
01314 **---------------------------------------------------------------+ELTTHERP
01315 **                                                               |ELTTHERP
01316 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
01317      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
01318 **                                                               |ELTTHERP
01319 **---------------------------------------------------------------+ELTTHERP
01320                                                                   ELTTHERP
01321 **---------------------------------------------------------------+ELTTHERP
01322 **                                                               |ELTTHERP
01323 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
01324 **                         A N D                                 |ELTTHERP
01325 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
01326      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
01327 **                                                               |ELTTHERP
01328 **---------------------------------------------------------------+ELTTHERP
01329                                                                   ELTTHERP
01330 **---------------------------------------------------------------+ELTTHERP
01331 **                                                               |ELTTHERP
01332 **               P E R F O R M   T A B U L A R                   |ELTTHERP
01333      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
01334 **                                                               |ELTTHERP
01335 **---------------------------------------------------------------+ELTTHERP
01336                                                                   ELTTHERP
01337  1240-EXIT.  EXIT.                                                ELTTHERP
01338                                                                   ELTTHERP
01339  1299-EXIT.            EXIT.                                      ELTTHERP
01340 /     P H Y S    T H E R A P Y   I P   P R O F E S S I O N A L    ELTTHERP
01341  1400-PHYS-THERP-IP-PROF-RTNE.                                    ELTTHERP
01342      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
01343                                                                   ELTTHERP
01344      MOVE WS-HDR-2-PHYS-IP-PROF TO COF-HDR-LINE (2)               ELTTHERP
01345                                                                   ELTTHERP
01346      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
01347      PERFORM WITH TEST BEFORE                                     ELTTHERP
01348              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
01349              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
01350         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
01351         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01352         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01353         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
01354      END-PERFORM.                                                 ELTTHERP
01355      MOVE WS-PHYS-IP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
01356                                                                   ELTTHERP
01357                                                                   ELTTHERP
01358      PERFORM WITH TEST BEFORE                                     ELTTHERP
01359         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
01360         UNTIL   WS-SUB  >     WS-PHYS-IP-PROF-CNT                 ELTTHERP
01361           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
01362           MOVE WS-PHYS-IP-PROF-LIST (WS-SUB)                      ELTTHERP
01363                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
01364            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
01365                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
01366      END-PERFORM.                                                 ELTTHERP
01367                                                                   ELTTHERP
01368      MOVE WS-PHYS-THERAPY-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTTHERP
01369                                                                   ELTTHERP
01370      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
01371      END-EXEC.                                                    ELTTHERP
01372                                                                   ELTTHERP
01373      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
01374      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01375      END-EXEC.                                                    ELTTHERP
01376                                                                   ELTTHERP
01377      IF PVN-COVG-NONE                                             ELTTHERP
01378         GO TO 1499-EXIT.                                          ELTTHERP
01379                                                                   ELTTHERP
01380      MOVE +1  TO  WS-CIA.                                         ELTTHERP
01381      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
01382            PSP-PROVN-PRICING-METHD,                               ELTTHERP
01383            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
01384            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
01385            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
01386            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
01387            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
01388            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
01389            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
01390            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
01391            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
01392            PSE-BEN-SCOPE-ID,                                      ELTTHERP
01393            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
01394            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
01395            PSE-MAX-AMT-PER-VISIT.                                 ELTTHERP
01396                                                                   ELTTHERP
01397                                                                   ELTTHERP
01398      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
01399                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
01400                     END-EXEC.                                     ELTTHERP
01401                                                                   ELTTHERP
01402      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
01403                                                                   ELTTHERP
01404      PERFORM 1430-FIND-FIRST-NONZERO                              ELTTHERP
01405         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
01406         UNTIL WS-SUB  >  WS-PHYS-IP-PROF-CNT.                     ELTTHERP
01407                                                                   ELTTHERP
01408      GO TO 1499-EXIT.                                             ELTTHERP
01409                                                                   ELTTHERP
01410  1430-FIND-FIRST-NONZERO.                                         ELTTHERP
01411      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01412      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
01413         CONTINUE                                                  ELTTHERP
01414      ELSE                                                         ELTTHERP
01415         PERFORM 1440-BUILD-SCREEN-LINES THRU 1440-EXIT.           ELTTHERP
01416                                                                   ELTTHERP
01417  1440-BUILD-SCREEN-LINES.                                         ELTTHERP
01418                                                                   ELTTHERP
01419      SET PLT-INDEX1  TO                                           ELTTHERP
01420                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
01421      IF WS-NOT-FIRST-TIME                                         ELTTHERP
01422         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
01423         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
01424         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
01425             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
01426             END-EXEC                                              ELTTHERP
01427      ELSE                                                         ELTTHERP
01428         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
01429                                                                   ELTTHERP
01430      MOVE 1  TO  WS-CIA.                                          ELTTHERP
01431      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
01432         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
01433            SET PLT-INDEX2  TO  2                                  ELTTHERP
01434         ELSE                                                      ELTTHERP
01435            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
01436            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
01437            GO TO 1440-EXIT                                        ELTTHERP
01438      ELSE                                                         ELTTHERP
01439         SET PLT-INDEX2  TO  1.                                    ELTTHERP
01440                                                                   ELTTHERP
01441 **---------------------------------------------------------------+ELTTHERP
01442 **                                                               |ELTTHERP
01443 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
01444      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
01445      ADD  +1  TO  WS-CIA.                                         ELTTHERP
01446      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
01447      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
01448         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
01449         UNTIL  PVN-BEN-PROVN-IDX > WS-PHYS-IP-PROF-CNT.           ELTTHERP
01450      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01451                                                                   ELTTHERP
01452      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
01453      MOVE +1  TO  WS-CIA                                          ELTTHERP
01454      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01455               END-EXEC.                                           ELTTHERP
01456 **                                                               |ELTTHERP
01457 **---------------------------------------------------------------+ELTTHERP
01458                                                                   ELTTHERP
01459 **---------------------------------------------------------------+ELTTHERP
01460 **                                                               |ELTTHERP
01461 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
01462      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
01463 **                                                               |ELTTHERP
01464 **---------------------------------------------------------------+ELTTHERP
01465                                                                   ELTTHERP
01466 **---------------------------------------------------------------+ELTTHERP
01467 **                                                               |ELTTHERP
01468 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
01469      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
01470 **                                                               |ELTTHERP
01471 **---------------------------------------------------------------+ELTTHERP
01472                                                                   ELTTHERP
01473 **---------------------------------------------------------------+ELTTHERP
01474 **                                                               |ELTTHERP
01475 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
01476 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
01477 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
01478      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
01479 **                                                               |ELTTHERP
01480 **---------------------------------------------------------------+ELTTHERP
01481                                                                   ELTTHERP
01482 **---------------------------------------------------------------+ELTTHERP
01483 **                                                               |ELTTHERP
01484 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
01485      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
01486         THRU 3250-EXIT.                                           ELTTHERP
01487 **                                                               |ELTTHERP
01488 **---------------------------------------------------------------+ELTTHERP
01489                                                                   ELTTHERP
01490 **---------------------------------------------------------------+ELTTHERP
01491 **                                                               |ELTTHERP
01492 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
01493      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
01494 **                                                               |ELTTHERP
01495 **---------------------------------------------------------------+ELTTHERP
01496                                                                   ELTTHERP
01497 **---------------------------------------------------------------+ELTTHERP
01498 **                                                               |ELTTHERP
01499 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
01500      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
01501 **                                                               |ELTTHERP
01502 **---------------------------------------------------------------+ELTTHERP
01503                                                                   ELTTHERP
01504 **---------------------------------------------------------------+ELTTHERP
01505 **                                                               |ELTTHERP
01506 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
01507 **                         A N D                                 |ELTTHERP
01508 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
01509      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
01510 **                                                               |ELTTHERP
01511 **---------------------------------------------------------------+ELTTHERP
01512                                                                   ELTTHERP
01513 **---------------------------------------------------------------+ELTTHERP
01514 **                                                               |ELTTHERP
01515 **               P E R F O R M   T A B U L A R                   |ELTTHERP
01516      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
01517 **                                                               |ELTTHERP
01518 **---------------------------------------------------------------+ELTTHERP
01519  1440-EXIT.  EXIT.                                                ELTTHERP
01520                                                                   ELTTHERP
01521  1499-EXIT.            EXIT.                                      ELTTHERP
01522                                                                   ELTTHERP
01523 /     P H Y S    T H E R A P Y   O P   P R O F E S S I O N A L    ELTTHERP
01524  1600-PHYS-THERP-OP-PROF-RTNE.                                    ELTTHERP
01525      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
01526                                                                   ELTTHERP
01527      MOVE WS-HDR-2-PHYS-OP-PROF TO COF-HDR-LINE (2)               ELTTHERP
01528                                                                   ELTTHERP
01529      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
01530      PERFORM WITH TEST BEFORE                                     ELTTHERP
01531              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
01532              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
01533         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
01534         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01535         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01536         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
01537      END-PERFORM.                                                 ELTTHERP
01538      MOVE WS-PHYS-OP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
01539                                                                   ELTTHERP
01540                                                                   ELTTHERP
01541      PERFORM WITH TEST BEFORE                                     ELTTHERP
01542         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
01543         UNTIL   WS-SUB  >     WS-PHYS-OP-PROF-CNT                 ELTTHERP
01544           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
01545           MOVE WS-PHYS-OP-PROF-LIST (WS-SUB)                      ELTTHERP
01546                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
01547            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
01548                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
01549      END-PERFORM.                                                 ELTTHERP
01550                                                                   ELTTHERP
01551      MOVE WS-PHYS-THERAPY-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTTHERP
01552                                                                   ELTTHERP
01553      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
01554      END-EXEC.                                                    ELTTHERP
01555                                                                   ELTTHERP
01556      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
01557      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01558      END-EXEC.                                                    ELTTHERP
01559                                                                   ELTTHERP
01560      IF PVN-COVG-NONE                                             ELTTHERP
01561         GO TO 1699-EXIT.                                          ELTTHERP
01562                                                                   ELTTHERP
01563      MOVE +1  TO  WS-CIA.                                         ELTTHERP
01564      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
01565            PSP-PROVN-PRICING-METHD,                               ELTTHERP
01566            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
01567            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
01568            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
01569            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
01570            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
01571            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
01572            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
01573            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
01574            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
01575            PSE-BEN-SCOPE-ID,                                      ELTTHERP
01576            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
01577            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
01578            PSE-MAX-AMT-PER-VISIT,                                 ELTTHERP
01579            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHERP
01580            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHERP
01581                                                                   ELTTHERP
01582                                                                   ELTTHERP
01583      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
01584                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
01585                     END-EXEC.                                     ELTTHERP
01586                                                                   ELTTHERP
01587      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
01588                                                                   ELTTHERP
01589      PERFORM 1630-FIND-FIRST-NONZERO                              ELTTHERP
01590         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
01591         UNTIL WS-SUB  >  WS-PHYS-OP-PROF-CNT.                     ELTTHERP
01592                                                                   ELTTHERP
01593      GO TO 1699-EXIT.                                             ELTTHERP
01594                                                                   ELTTHERP
01595  1630-FIND-FIRST-NONZERO.                                         ELTTHERP
01596      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01597      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
01598         CONTINUE                                                  ELTTHERP
01599      ELSE                                                         ELTTHERP
01600         PERFORM 1640-BUILD-SCREEN-LINES THRU 1640-EXIT.           ELTTHERP
01601                                                                   ELTTHERP
01602  1640-BUILD-SCREEN-LINES.                                         ELTTHERP
01603                                                                   ELTTHERP
01604      SET PLT-INDEX1  TO                                           ELTTHERP
01605                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
01606      IF WS-NOT-FIRST-TIME                                         ELTTHERP
01607         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
01608         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
01609         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
01610             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
01611              END-EXEC                                             ELTTHERP
01612      ELSE                                                         ELTTHERP
01613         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
01614                                                                   ELTTHERP
01615      MOVE 1  TO  WS-CIA.                                          ELTTHERP
01616      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
01617         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
01618            SET PLT-INDEX2  TO  2                                  ELTTHERP
01619         ELSE                                                      ELTTHERP
01620            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
01621            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
01622            GO TO 1640-EXIT                                        ELTTHERP
01623      ELSE                                                         ELTTHERP
01624         SET PLT-INDEX2  TO  1.                                    ELTTHERP
01625                                                                   ELTTHERP
01626 **---------------------------------------------------------------+ELTTHERP
01627 **                                                               |ELTTHERP
01628 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
01629      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
01630      ADD  +1  TO  WS-CIA.                                         ELTTHERP
01631      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
01632      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
01633         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
01634         UNTIL  PVN-BEN-PROVN-IDX > WS-PHYS-OP-PROF-CNT.           ELTTHERP
01635      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01636                                                                   ELTTHERP
01637      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
01638      MOVE +1  TO  WS-CIA                                          ELTTHERP
01639      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01640              END-EXEC.                                            ELTTHERP
01641 **                                                               |ELTTHERP
01642 **---------------------------------------------------------------+ELTTHERP
01643                                                                   ELTTHERP
01644 **---------------------------------------------------------------+ELTTHERP
01645 **                                                               |ELTTHERP
01646 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
01647      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
01648 **                                                               |ELTTHERP
01649 **---------------------------------------------------------------+ELTTHERP
01650                                                                   ELTTHERP
01651 **---------------------------------------------------------------+ELTTHERP
01652 **                                                               |ELTTHERP
01653 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
01654      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
01655 **                                                               |ELTTHERP
01656 **---------------------------------------------------------------+ELTTHERP
01657                                                                   ELTTHERP
01658 **---------------------------------------------------------------+ELTTHERP
01659 **                                                               |ELTTHERP
01660 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
01661 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
01662 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
01663      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
01664 **                                                               |ELTTHERP
01665 **---------------------------------------------------------------+ELTTHERP
01666                                                                   ELTTHERP
01667 **---------------------------------------------------------------+ELTTHERP
01668 **                                                               |ELTTHERP
01669 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
01670      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
01671         THRU 3250-EXIT.                                           ELTTHERP
01672 **                                                               |ELTTHERP
01673 **---------------------------------------------------------------+ELTTHERP
01674                                                                   ELTTHERP
01675 **---------------------------------------------------------------+ELTTHERP
01676 **                                                               |ELTTHERP
01677 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
01678      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
01679 **                                                               |ELTTHERP
01680 **---------------------------------------------------------------+ELTTHERP
01681                                                                   ELTTHERP
01682 **---------------------------------------------------------------+ELTTHERP
01683 **                                                               |ELTTHERP
01684 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
01685      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
01686 **                                                               |ELTTHERP
01687 **---------------------------------------------------------------+ELTTHERP
01688                                                                   ELTTHERP
01689 **---------------------------------------------------------------+ELTTHERP
01690 **                                                               |ELTTHERP
01691 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
01692      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
01693 **                                                               |ELTTHERP
01694 **---------------------------------------------------------------+ELTTHERP
01695                                                                   ELTTHERP
01696 **---------------------------------------------------------------+ELTTHERP
01697 **                                                               |ELTTHERP
01698 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
01699 **                         A N D                                 |ELTTHERP
01700 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
01701      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
01702 **                                                               |ELTTHERP
01703 **---------------------------------------------------------------+ELTTHERP
01704                                                                   ELTTHERP
01705 **---------------------------------------------------------------+ELTTHERP
01706 **                                                               |ELTTHERP
01707 **               P E R F O R M   T A B U L A R                   |ELTTHERP
01708      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
01709 **                                                               |ELTTHERP
01710 **---------------------------------------------------------------+ELTTHERP
01711  1640-EXIT.  EXIT.                                                ELTTHERP
01712                                                                   ELTTHERP
01713  1699-EXIT.            EXIT.                                      ELTTHERP
01714                                                                   ELTTHERP
01715 /  R A D I A T I O N / C H E M O T H E R A P Y   I P   I N S T N LELTTHERP
01716 ***************************************************************** ELTTHERP
01717 *  R A D I A T I O N / C H E M O T H E R A P Y   I P   I N S T N LELTTHERP
01718 *                                                                 ELTTHERP
01719 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHERP
01720 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHERP
01721 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHERP
01722 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHERP
01723 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHERP
01724 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHERP
01725 *  MODULE.                                                        ELTTHERP
01726 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHERP
01727 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHERP
01728 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHERP
01729 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHERP
01730 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHERP
01731 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHERP
01732 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHERP
01733 *                                                                 ELTTHERP
01734 ***************************************************************** ELTTHERP
01735  2000-CHEMO-THRP-IP-INST-RTNE.                                    ELTTHERP
01736      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
01737                                                                   ELTTHERP
01738      MOVE WS-HDR-2-CHEMO-IP-INST  TO  COF-HDR-LINE(2).            ELTTHERP
01739                                                                   ELTTHERP
01740      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
01741      PERFORM WITH TEST BEFORE                                     ELTTHERP
01742              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
01743              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
01744         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
01745         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01746         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01747         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
01748      END-PERFORM.                                                 ELTTHERP
01749      MOVE WS-CHEMO-IP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
01750                                                                   ELTTHERP
01751                                                                   ELTTHERP
01752      PERFORM WITH TEST BEFORE                                     ELTTHERP
01753         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
01754         UNTIL   WS-SUB  >     WS-CHEMO-IP-INST-CNT                ELTTHERP
01755           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
01756           MOVE WS-CHEMO-IP-INST-LIST (WS-SUB)                     ELTTHERP
01757                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
01758            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
01759                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
01760      END-PERFORM.                                                 ELTTHERP
01761                                                                   ELTTHERP
01762      MOVE WS-CHEMO-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
01763                                                                   ELTTHERP
01764      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
01765      END-EXEC.                                                    ELTTHERP
01766                                                                   ELTTHERP
01767      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
01768      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01769      END-EXEC.                                                    ELTTHERP
01770                                                                   ELTTHERP
01771      IF PVN-COVG-NONE                                             ELTTHERP
01772         GO TO 2099-EXIT.                                          ELTTHERP
01773                                                                   ELTTHERP
01774      MOVE +1  TO  WS-CIA.                                         ELTTHERP
01775      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
01776            PSP-PROVN-PRICING-METHD,                               ELTTHERP
01777            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
01778            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
01779            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
01780            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
01781            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
01782            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
01783            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
01784            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
01785            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
01786            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
01787            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
01788            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
01789                                                                   ELTTHERP
01790                                                                   ELTTHERP
01791      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
01792                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
01793                     END-EXEC.                                     ELTTHERP
01794                                                                   ELTTHERP
01795      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
01796                                                                   ELTTHERP
01797                                                                   ELTTHERP
01798      PERFORM 2030-FIND-FIRST-NONZERO                              ELTTHERP
01799         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
01800         UNTIL WS-SUB  >  WS-CHEMO-IP-INST-CNT.                    ELTTHERP
01801                                                                   ELTTHERP
01802      GO TO 2099-EXIT.                                             ELTTHERP
01803  2030-FIND-FIRST-NONZERO.                                         ELTTHERP
01804      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01805      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
01806         CONTINUE                                                  ELTTHERP
01807      ELSE                                                         ELTTHERP
01808         PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT.           ELTTHERP
01809                                                                   ELTTHERP
01810  2040-BUILD-SCREEN-LINES.                                         ELTTHERP
01811                                                                   ELTTHERP
01812      SET PLT-INDEX1  TO                                           ELTTHERP
01813                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
01814      IF WS-NOT-FIRST-TIME                                         ELTTHERP
01815         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
01816         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
01817         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
01818             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
01819             END-EXEC                                              ELTTHERP
01820      ELSE                                                         ELTTHERP
01821         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
01822                                                                   ELTTHERP
01823      MOVE 1  TO  WS-CIA.                                          ELTTHERP
01824      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
01825         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
01826            SET PLT-INDEX2  TO  2                                  ELTTHERP
01827         ELSE                                                      ELTTHERP
01828            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
01829            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
01830            GO TO 2040-EXIT                                        ELTTHERP
01831      ELSE                                                         ELTTHERP
01832         SET PLT-INDEX2  TO  1.                                    ELTTHERP
01833                                                                   ELTTHERP
01834 **---------------------------------------------------------------+ELTTHERP
01835 **                                                               |ELTTHERP
01836 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
01837      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
01838      ADD  +1  TO  WS-CIA.                                         ELTTHERP
01839      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
01840                                                                   ELTTHERP
01841      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
01842                                                                   ELTTHERP
01843      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
01844         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
01845         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-IP-INST-CNT.          ELTTHERP
01846      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01847                                                                   ELTTHERP
01848      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
01849      MOVE +1  TO  WS-CIA                                          ELTTHERP
01850      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01851               END-EXEC.                                           ELTTHERP
01852 **                                                               |ELTTHERP
01853 **---------------------------------------------------------------+ELTTHERP
01854                                                                   ELTTHERP
01855 **---------------------------------------------------------------+ELTTHERP
01856 **                                                               |ELTTHERP
01857 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
01858      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
01859 **                                                               |ELTTHERP
01860 **---------------------------------------------------------------+ELTTHERP
01861                                                                   ELTTHERP
01862 **---------------------------------------------------------------+ELTTHERP
01863 **                                                               |ELTTHERP
01864 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
01865 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
01866 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
01867      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
01868 **                                                               |ELTTHERP
01869 **---------------------------------------------------------------+ELTTHERP
01870                                                                   ELTTHERP
01871 **---------------------------------------------------------------+ELTTHERP
01872 **                                                               |ELTTHERP
01873 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHERP
01874 **                  H O S P I T A L   B I L L                    |ELTTHERP
01875      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHERP
01876      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
01877 **                                                               |ELTTHERP
01878 **---------------------------------------------------------------+ELTTHERP
01879                                                                   ELTTHERP
01880 **---------------------------------------------------------------+ELTTHERP
01881 **                                                               |ELTTHERP
01882 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
01883      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
01884 **                                                               |ELTTHERP
01885 **---------------------------------------------------------------+ELTTHERP
01886                                                                   ELTTHERP
01887 **---------------------------------------------------------------+ELTTHERP
01888 **                                                               |ELTTHERP
01889 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
01890      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
01891         THRU 3250-EXIT.                                           ELTTHERP
01892 **                                                               |ELTTHERP
01893 **---------------------------------------------------------------+ELTTHERP
01894                                                                   ELTTHERP
01895 **---------------------------------------------------------------+ELTTHERP
01896 **                                                               |ELTTHERP
01897 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
01898      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
01899 **                                                               |ELTTHERP
01900 **---------------------------------------------------------------+ELTTHERP
01901                                                                   ELTTHERP
01902 **---------------------------------------------------------------+ELTTHERP
01903 **                                                               |ELTTHERP
01904 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
01905 **                         A N D                                 |ELTTHERP
01906 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
01907      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
01908 **                                                               |ELTTHERP
01909 **---------------------------------------------------------------+ELTTHERP
01910                                                                   ELTTHERP
01911 **---------------------------------------------------------------+ELTTHERP
01912 **                                                               |ELTTHERP
01913 **               P E R F O R M   T A B U L A R                   |ELTTHERP
01914      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
01915 **                                                               |ELTTHERP
01916 **---------------------------------------------------------------+ELTTHERP
01917                                                                   ELTTHERP
01918  2040-EXIT.  EXIT.                                                ELTTHERP
01919                                                                   ELTTHERP
01920  2099-EXIT.            EXIT.                                      ELTTHERP
01921 /  R A D I A T I O N / C H E M O T H E R A P Y   O P   I N S T N LELTTHERP
01922  2200-CHEMO-THRP-OP-INST-RTNE.                                    ELTTHERP
01923      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
01924                                                                   ELTTHERP
01925      MOVE WS-HDR-2-CHEMO-OP-INST  TO  COF-HDR-LINE(2).            ELTTHERP
01926                                                                   ELTTHERP
01927      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
01928      PERFORM WITH TEST BEFORE                                     ELTTHERP
01929              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
01930              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
01931         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
01932         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01933         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
01934         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
01935      END-PERFORM.                                                 ELTTHERP
01936      MOVE WS-CHEMO-OP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
01937                                                                   ELTTHERP
01938                                                                   ELTTHERP
01939      PERFORM WITH TEST BEFORE                                     ELTTHERP
01940         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
01941         UNTIL   WS-SUB  >     WS-CHEMO-OP-INST-CNT                ELTTHERP
01942           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
01943           MOVE WS-CHEMO-OP-INST-LIST(WS-SUB)                      ELTTHERP
01944                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
01945            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
01946                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
01947      END-PERFORM.                                                 ELTTHERP
01948                                                                   ELTTHERP
01949      MOVE WS-CHEMO-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
01950                                                                   ELTTHERP
01951      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
01952      END-EXEC.                                                    ELTTHERP
01953                                                                   ELTTHERP
01954      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
01955      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
01956      END-EXEC.                                                    ELTTHERP
01957                                                                   ELTTHERP
01958                                                                   ELTTHERP
01959      IF PVN-COVG-NONE                                             ELTTHERP
01960         GO TO 2299-EXIT.                                          ELTTHERP
01961                                                                   ELTTHERP
01962      MOVE +1  TO  WS-CIA.                                         ELTTHERP
01963      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
01964            PSP-PROVN-PRICING-METHD,                               ELTTHERP
01965            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
01966            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
01967            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
01968            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
01969            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
01970            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
01971            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
01972            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
01973            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
01974            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
01975            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
01976            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHERP
01977            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHERP
01978            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
01979                                                                   ELTTHERP
01980                                                                   ELTTHERP
01981      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
01982                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
01983                     END-EXEC.                                     ELTTHERP
01984                                                                   ELTTHERP
01985      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
01986                                                                   ELTTHERP
01987      PERFORM 2230-FIND-FIRST-NONZERO                              ELTTHERP
01988         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
01989         UNTIL WS-SUB  >  WS-CHEMO-OP-INST-CNT.                    ELTTHERP
01990                                                                   ELTTHERP
01991      GO TO 2299-EXIT.                                             ELTTHERP
01992  2230-FIND-FIRST-NONZERO.                                         ELTTHERP
01993      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
01994      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
01995         CONTINUE                                                  ELTTHERP
01996      ELSE                                                         ELTTHERP
01997         PERFORM 2240-BUILD-SCREEN-LINES THRU 2240-EXIT.           ELTTHERP
01998                                                                   ELTTHERP
01999  2240-BUILD-SCREEN-LINES.                                         ELTTHERP
02000                                                                   ELTTHERP
02001      SET PLT-INDEX1  TO                                           ELTTHERP
02002                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
02003      IF WS-NOT-FIRST-TIME                                         ELTTHERP
02004         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
02005         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
02006         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
02007             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
02008              END-EXEC                                             ELTTHERP
02009      ELSE                                                         ELTTHERP
02010         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
02011                                                                   ELTTHERP
02012      MOVE 1  TO  WS-CIA.                                          ELTTHERP
02013      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
02014         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
02015            SET PLT-INDEX2  TO  2                                  ELTTHERP
02016         ELSE                                                      ELTTHERP
02017            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
02018            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
02019            GO TO 2240-EXIT                                        ELTTHERP
02020      ELSE                                                         ELTTHERP
02021         SET PLT-INDEX2  TO  1.                                    ELTTHERP
02022                                                                   ELTTHERP
02023 **---------------------------------------------------------------+ELTTHERP
02024 **                                                               |ELTTHERP
02025 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
02026      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
02027      ADD  +1  TO  WS-CIA.                                         ELTTHERP
02028      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
02029                                                                   ELTTHERP
02030      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
02031                                                                   ELTTHERP
02032      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
02033         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
02034         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-OP-INST-CNT.          ELTTHERP
02035      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
02036                                                                   ELTTHERP
02037      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
02038      MOVE +1  TO  WS-CIA                                          ELTTHERP
02039      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
02040               END-EXEC.                                           ELTTHERP
02041 **                                                               |ELTTHERP
02042 **---------------------------------------------------------------+ELTTHERP
02043                                                                   ELTTHERP
02044 **---------------------------------------------------------------+ELTTHERP
02045 **                                                               |ELTTHERP
02046 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
02047      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
02048 **                                                               |ELTTHERP
02049 **---------------------------------------------------------------+ELTTHERP
02050                                                                   ELTTHERP
02051 **---------------------------------------------------------------+ELTTHERP
02052 **                                                               |ELTTHERP
02053 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
02054 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
02055 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
02056      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
02057 **                                                               |ELTTHERP
02058 **---------------------------------------------------------------+ELTTHERP
02059                                                                   ELTTHERP
02060 **---------------------------------------------------------------+ELTTHERP
02061 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHERP
02062 **                    H O S P I T A L    B I L L                 |ELTTHERP
02063      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHERP
02064      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
02065 **                                                               |ELTTHERP
02066 **---------------------------------------------------------------+ELTTHERP
02067                                                                   ELTTHERP
02068 **---------------------------------------------------------------+ELTTHERP
02069 **                                                               |ELTTHERP
02070 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
02071      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
02072 **                                                               |ELTTHERP
02073 **---------------------------------------------------------------+ELTTHERP
02074                                                                   ELTTHERP
02075 **---------------------------------------------------------------+ELTTHERP
02076 **                                                               |ELTTHERP
02077 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
02078      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
02079         THRU 3250-EXIT.                                           ELTTHERP
02080 **                                                               |ELTTHERP
02081 **---------------------------------------------------------------+ELTTHERP
02082                                                                   ELTTHERP
02083 **---------------------------------------------------------------+ELTTHERP
02084 **                                                               |ELTTHERP
02085 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
02086      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
02087 **                                                               |ELTTHERP
02088 **---------------------------------------------------------------+ELTTHERP
02089                                                                   ELTTHERP
02090 **---------------------------------------------------------------+ELTTHERP
02091 **                                                               |ELTTHERP
02092 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
02093      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
02094 **                                                               |ELTTHERP
02095 **---------------------------------------------------------------+ELTTHERP
02096                                                                   ELTTHERP
02097 **---------------------------------------------------------------+ELTTHERP
02098 **                                                               |ELTTHERP
02099 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
02100 **                         A N D                                 |ELTTHERP
02101 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
02102      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
02103 **                                                               |ELTTHERP
02104 **---------------------------------------------------------------+ELTTHERP
02105                                                                   ELTTHERP
02106 **---------------------------------------------------------------+ELTTHERP
02107 **                                                               |ELTTHERP
02108 **               P E R F O R M   T A B U L A R                   |ELTTHERP
02109      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
02110 **                                                               |ELTTHERP
02111 **---------------------------------------------------------------+ELTTHERP
02112  2240-EXIT.  EXIT.                                                ELTTHERP
02113                                                                   ELTTHERP
02114                                                                   ELTTHERP
02115  2299-EXIT.            EXIT.                                      ELTTHERP
02116 /  R A D I A T I O N / C H E M O T H E R A P Y   I P   P R O F S LELTTHERP
02117  2400-CHEMO-THRP-IP-PROF-RTNE.                                    ELTTHERP
02118      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
02119                                                                   ELTTHERP
02120      MOVE WS-HDR-2-CHEMO-IP-PROF  TO  COF-HDR-LINE(2).            ELTTHERP
02121                                                                   ELTTHERP
02122      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
02123      PERFORM WITH TEST BEFORE                                     ELTTHERP
02124              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
02125              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
02126         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
02127         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
02128         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
02129         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
02130      END-PERFORM.                                                 ELTTHERP
02131      MOVE WS-CHEMO-IP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
02132                                                                   ELTTHERP
02133                                                                   ELTTHERP
02134      PERFORM WITH TEST BEFORE                                     ELTTHERP
02135         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
02136         UNTIL   WS-SUB  >     WS-CHEMO-IP-PROF-CNT                ELTTHERP
02137           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
02138           MOVE WS-CHEMO-IP-PROF-LIST(WS-SUB)                      ELTTHERP
02139                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
02140            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
02141                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
02142      END-PERFORM.                                                 ELTTHERP
02143                                                                   ELTTHERP
02144      MOVE WS-CHEMO-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
02145                                                                   ELTTHERP
02146      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
02147      END-EXEC.                                                    ELTTHERP
02148                                                                   ELTTHERP
02149      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
02150      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
02151      END-EXEC.                                                    ELTTHERP
02152                                                                   ELTTHERP
02153      IF PVN-COVG-NONE                                             ELTTHERP
02154         GO TO 2499-EXIT.                                          ELTTHERP
02155                                                                   ELTTHERP
02156      MOVE +1  TO  WS-CIA.                                         ELTTHERP
02157      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
02158            PSP-PROVN-PRICING-METHD,                               ELTTHERP
02159            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
02160            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
02161            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
02162            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
02163            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
02164            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
02165            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
02166            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
02167            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
02168            PSE-BEN-SCOPE-ID,                                      ELTTHERP
02169            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
02170            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
02171            PSE-MAX-AMT-PER-VISIT.                                 ELTTHERP
02172                                                                   ELTTHERP
02173                                                                   ELTTHERP
02174      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
02175                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
02176                     END-EXEC.                                     ELTTHERP
02177                                                                   ELTTHERP
02178      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
02179                                                                   ELTTHERP
02180      PERFORM 2430-FIND-FIRST-NONZERO                              ELTTHERP
02181         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
02182         UNTIL WS-SUB  >  WS-CHEMO-IP-PROF-CNT.                    ELTTHERP
02183                                                                   ELTTHERP
02184      GO TO 2499-EXIT.                                             ELTTHERP
02185  2430-FIND-FIRST-NONZERO.                                         ELTTHERP
02186      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
02187      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
02188         CONTINUE                                                  ELTTHERP
02189      ELSE                                                         ELTTHERP
02190         PERFORM 2440-BUILD-SCREEN-LINES THRU 2440-EXIT.           ELTTHERP
02191                                                                   ELTTHERP
02192  2440-BUILD-SCREEN-LINES.                                         ELTTHERP
02193                                                                   ELTTHERP
02194      SET PLT-INDEX1  TO                                           ELTTHERP
02195                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
02196      IF WS-NOT-FIRST-TIME                                         ELTTHERP
02197         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
02198         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
02199         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
02200             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
02201                                         END-EXEC                  ELTTHERP
02202      ELSE                                                         ELTTHERP
02203         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
02204                                                                   ELTTHERP
02205      MOVE 1  TO  WS-CIA.                                          ELTTHERP
02206      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
02207         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
02208            SET PLT-INDEX2  TO  2                                  ELTTHERP
02209         ELSE                                                      ELTTHERP
02210            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
02211            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
02212            GO TO 2440-EXIT                                        ELTTHERP
02213      ELSE                                                         ELTTHERP
02214         SET PLT-INDEX2  TO  1.                                    ELTTHERP
02215                                                                   ELTTHERP
02216 **---------------------------------------------------------------+ELTTHERP
02217 **                                                               |ELTTHERP
02218 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
02219      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
02220      ADD  +1  TO  WS-CIA.                                         ELTTHERP
02221      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
02222      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
02223         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
02224         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-IP-PROF-CNT.          ELTTHERP
02225      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
02226                                                                   ELTTHERP
02227      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
02228      MOVE +1  TO  WS-CIA                                          ELTTHERP
02229      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
02230                                         END-EXEC.                 ELTTHERP
02231 **                                                               |ELTTHERP
02232 **---------------------------------------------------------------+ELTTHERP
02233 **---------------------------------------------------------------+ELTTHERP
02234 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
02235      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
02236 **                                                               |ELTTHERP
02237 **---------------------------------------------------------------+ELTTHERP
02238                                                                   ELTTHERP
02239 **---------------------------------------------------------------+ELTTHERP
02240 **                                                               |ELTTHERP
02241 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
02242      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
02243 **                                                               |ELTTHERP
02244 **---------------------------------------------------------------+ELTTHERP
02245                                                                   ELTTHERP
02246 **---------------------------------------------------------------+ELTTHERP
02247 **                                                               |ELTTHERP
02248 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
02249 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
02250 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
02251      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
02252 **                                                               |ELTTHERP
02253 **---------------------------------------------------------------+ELTTHERP
02254                                                                   ELTTHERP
02255 **---------------------------------------------------------------+ELTTHERP
02256 **                                                               |ELTTHERP
02257 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
02258      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
02259         THRU 3250-EXIT.                                           ELTTHERP
02260 **                                                               |ELTTHERP
02261 **---------------------------------------------------------------+ELTTHERP
02262                                                                   ELTTHERP
02263 **---------------------------------------------------------------+ELTTHERP
02264 **                                                               |ELTTHERP
02265 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
02266      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
02267 **                                                               |ELTTHERP
02268 **---------------------------------------------------------------+ELTTHERP
02269                                                                   ELTTHERP
02270 **---------------------------------------------------------------+ELTTHERP
02271 **                                                               |ELTTHERP
02272 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
02273      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
02274 **                                                               |ELTTHERP
02275 **---------------------------------------------------------------+ELTTHERP
02276                                                                   ELTTHERP
02277 **---------------------------------------------------------------+ELTTHERP
02278 **                                                               |ELTTHERP
02279 **          S A M E   P R O V I D E R   B I L L I N G            |ELTTHERP
02280 **    I P   R A D I A T I O N   T H E R A P Y / M E D I C A L    |ELTTHERP
02281                                                                   ELTTHERP
02282      PERFORM 2450-SET-ADR-OF-CON-REC-B.                           ELTTHERP
02283      IF CIA-RC-PTR-NULL                                           ELTTHERP
02284         PERFORM 2460-SET-ADR-OF-CON-REC-S                         ELTTHERP
02285         IF CIA-RC-PTR-NULL                                        ELTTHERP
02286            CONTINUE                                               ELTTHERP
02287      ELSE                                                         ELTTHERP
02288          MOVE 'Y' TO CALL-ELUOUTPT-IND                            ELTTHERP
02289          MOVE WS-SAME-PROVIDER-RADIATN TO COF-DTL-LINE (WS-CIA)   ELTTHERP
02290          ADD +1                   TO WS-CIA                       ELTTHERP
02291          PERFORM 2450-SET-ADR-OF-CON-REC-B                        ELTTHERP
02292          IF CIA-RC-PTR-NULL                                       ELTTHERP
02293             PERFORM 4200-SAME-PROV-BILL-THRP-BAS                  ELTTHERP
02294          END-IF                                                   ELTTHERP
02295          PERFORM 2460-SET-ADR-OF-CON-REC-S                        ELTTHERP
02296          IF CIA-RC-PTR-NULL                                       ELTTHERP
02297             PERFORM 4250-SAME-PROV-BILL-THRP-SUP                  ELTTHERP
02298          END-IF                                                   ELTTHERP
02299          IF YES-CALL-ELUOUTPT                                     ELTTHERP
02300              MOVE 'N' TO CALL-ELUOUTPT-IND                        ELTTHERP
02301              ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES          ELTTHERP
02302              EXEC CICS  LINK  PROGRAM('ELUOUTPT')                 ELTTHERP
02303                               COMMAREA(DFHCOMMAREA)               ELTTHERP
02304              END-EXEC                                             ELTTHERP
02305              MOVE 1  TO  WS-CIA                                   ELTTHERP
02306          ELSE                                                     ELTTHERP
02307              ADD -1 TO WS-CIA                                     ELTTHERP
02308          END-IF                                                   ELTTHERP
02309      END-IF.                                                      ELTTHERP
02310                                                                   ELTTHERP
02311 **                                                               |ELTTHERP
02312 **---------------------------------------------------------------+ELTTHERP
02313                                                                   ELTTHERP
02314 **---------------------------------------------------------------+ELTTHERP
02315 **                                                               |ELTTHERP
02316 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
02317 **                         A N D                                 |ELTTHERP
02318 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
02319      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
02320 **                                                               |ELTTHERP
02321 **---------------------------------------------------------------+ELTTHERP
02322                                                                   ELTTHERP
02323 **---------------------------------------------------------------+ELTTHERP
02324 **                                                               |ELTTHERP
02325 **               P E R F O R M   T A B U L A R                   |ELTTHERP
02326      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
02327 **                                                               |ELTTHERP
02328 **---------------------------------------------------------------+ELTTHERP
02329                                                                   ELTTHERP
02330  2440-EXIT.  EXIT.                                                ELTTHERP
02331                                                                   ELTTHERP
02332  2450-SET-ADR-OF-CON-REC-B.                                       ELTTHERP
02333      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTTHERP
02334      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
02335          ADDRESS OF CONTRACT-RECORD.                              ELTTHERP
02336                                                                   ELTTHERP
02337  2460-SET-ADR-OF-CON-REC-S.                                       ELTTHERP
02338      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTTHERP
02339      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
02340          ADDRESS OF CONTRACT-RECORD.                              ELTTHERP
02341                                                                   ELTTHERP
02342  2499-EXIT.            EXIT.                                      ELTTHERP
02343 /  R A D I A T I O N / C H E M O T H E R A P Y   O P   P R O F S LELTTHERP
02344  2600-CHEMO-THRP-OP-PROF-RTNE.                                    ELTTHERP
02345      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
02346                                                                   ELTTHERP
02347      MOVE WS-HDR-2-CHEMO-OP-PROF  TO  COF-HDR-LINE(2).            ELTTHERP
02348                                                                   ELTTHERP
02349      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
02350      PERFORM WITH TEST BEFORE                                     ELTTHERP
02351              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
02352              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
02353         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
02354         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
02355         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
02356         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
02357      END-PERFORM.                                                 ELTTHERP
02358      MOVE WS-CHEMO-OP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
02359                                                                   ELTTHERP
02360                                                                   ELTTHERP
02361      PERFORM WITH TEST BEFORE                                     ELTTHERP
02362         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
02363         UNTIL   WS-SUB  >     WS-CHEMO-OP-PROF-CNT                ELTTHERP
02364           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
02365           MOVE WS-CHEMO-OP-PROF-LIST(WS-SUB)                      ELTTHERP
02366                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
02367            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
02368                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
02369      END-PERFORM.                                                 ELTTHERP
02370                                                                   ELTTHERP
02371      MOVE WS-CHEMO-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
02372                                                                   ELTTHERP
02373      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
02374      END-EXEC.                                                    ELTTHERP
02375                                                                   ELTTHERP
02376      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
02377      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
02378      END-EXEC.                                                    ELTTHERP
02379                                                                   ELTTHERP
02380      IF PVN-COVG-NONE                                             ELTTHERP
02381         GO TO 2699-EXIT.                                          ELTTHERP
02382                                                                   ELTTHERP
02383      MOVE +1  TO  WS-CIA.                                         ELTTHERP
02384      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
02385            PSP-PROVN-PRICING-METHD,                               ELTTHERP
02386            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
02387            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
02388            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
02389            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
02390            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
02391            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
02392            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
02393            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
02394            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
02395            PSE-BEN-SCOPE-ID,                                      ELTTHERP
02396            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
02397            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
02398            PSE-MAX-AMT-PER-VISIT,                                 ELTTHERP
02399            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHERP
02400            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHERP
02401                                                                   ELTTHERP
02402                                                                   ELTTHERP
02403      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
02404                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
02405                     END-EXEC.                                     ELTTHERP
02406                                                                   ELTTHERP
02407      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
02408                                                                   ELTTHERP
02409      PERFORM 2630-FIND-FIRST-NONZERO                              ELTTHERP
02410         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
02411         UNTIL WS-SUB  >  WS-CHEMO-OP-PROF-CNT.                    ELTTHERP
02412                                                                   ELTTHERP
02413      GO TO 2699-EXIT.                                             ELTTHERP
02414  2630-FIND-FIRST-NONZERO.                                         ELTTHERP
02415      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
02416      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
02417         CONTINUE                                                  ELTTHERP
02418      ELSE                                                         ELTTHERP
02419         PERFORM 2640-BUILD-SCREEN-LINES THRU 2640-EXIT.           ELTTHERP
02420                                                                   ELTTHERP
02421  2640-BUILD-SCREEN-LINES.                                         ELTTHERP
02422                                                                   ELTTHERP
02423      SET PLT-INDEX1  TO                                           ELTTHERP
02424                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
02425      IF WS-NOT-FIRST-TIME                                         ELTTHERP
02426         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
02427         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
02428         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
02429             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
02430                                         END-EXEC                  ELTTHERP
02431      ELSE                                                         ELTTHERP
02432         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
02433                                                                   ELTTHERP
02434      MOVE 1  TO  WS-CIA.                                          ELTTHERP
02435      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
02436         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
02437            SET PLT-INDEX2  TO  2                                  ELTTHERP
02438         ELSE                                                      ELTTHERP
02439            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
02440            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
02441            GO TO 2640-EXIT                                        ELTTHERP
02442      ELSE                                                         ELTTHERP
02443         SET PLT-INDEX2  TO  1.                                    ELTTHERP
02444                                                                   ELTTHERP
02445 **---------------------------------------------------------------+ELTTHERP
02446 **                                                               |ELTTHERP
02447 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
02448      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
02449      ADD  +1  TO  WS-CIA.                                         ELTTHERP
02450      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
02451      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
02452         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
02453         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-OP-PROF-CNT.          ELTTHERP
02454      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
02455                                                                   ELTTHERP
02456      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
02457      MOVE +1  TO  WS-CIA                                          ELTTHERP
02458      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
02459                                         END-EXEC.                 ELTTHERP
02460 **                                                               |ELTTHERP
02461 **---------------------------------------------------------------+ELTTHERP
02462 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
02463      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
02464 **                                                               |ELTTHERP
02465 **---------------------------------------------------------------+ELTTHERP
02466                                                                   ELTTHERP
02467 **---------------------------------------------------------------+ELTTHERP
02468 **                                                               |ELTTHERP
02469 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
02470      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
02471 **                                                               |ELTTHERP
02472 **---------------------------------------------------------------+ELTTHERP
02473                                                                   ELTTHERP
02474 **---------------------------------------------------------------+ELTTHERP
02475 **                                                               |ELTTHERP
02476 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
02477 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
02478 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
02479      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
02480 **                                                               |ELTTHERP
02481 **---------------------------------------------------------------+ELTTHERP
02482                                                                   ELTTHERP
02483 **---------------------------------------------------------------+ELTTHERP
02484 **                                                               |ELTTHERP
02485 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
02486      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
02487         THRU 3250-EXIT.                                           ELTTHERP
02488 **                                                               |ELTTHERP
02489 **---------------------------------------------------------------+ELTTHERP
02490                                                                   ELTTHERP
02491 **---------------------------------------------------------------+ELTTHERP
02492 **                                                               |ELTTHERP
02493 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
02494      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
02495 **                                                               |ELTTHERP
02496 **---------------------------------------------------------------+ELTTHERP
02497                                                                   ELTTHERP
02498 **---------------------------------------------------------------+ELTTHERP
02499 **                                                               |ELTTHERP
02500 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
02501      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
02502 **                                                               |ELTTHERP
02503 **---------------------------------------------------------------+ELTTHERP
02504                                                                   ELTTHERP
02505 **---------------------------------------------------------------+ELTTHERP
02506 **                                                               |ELTTHERP
02507 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
02508      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
02509 **                                                               |ELTTHERP
02510 **---------------------------------------------------------------+ELTTHERP
02511                                                                   ELTTHERP
02512 **---------------------------------------------------------------+ELTTHERP
02513 **                                                               |ELTTHERP
02514 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
02515 **                         A N D                                 |ELTTHERP
02516 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
02517      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
02518 **                                                               |ELTTHERP
02519 **---------------------------------------------------------------+ELTTHERP
02520                                                                   ELTTHERP
02521 **---------------------------------------------------------------+ELTTHERP
02522 **                                                               |ELTTHERP
02523 **               P E R F O R M   T A B U L A R                   |ELTTHERP
02524      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
02525 **                                                               |ELTTHERP
02526 **---------------------------------------------------------------+ELTTHERP
02527  2640-EXIT.  EXIT.                                                ELTTHERP
02528                                                                   ELTTHERP
02529  2699-EXIT.            EXIT.                                      ELTTHERP
02530                                                                   ELTTHERP
02531                                                                   ELTTHERP
02532                                                                   ELTTHERP
02533 /*****************************************************************ELTTHERP
02534 **                                                               |ELTTHERP
02535 *     C O M M O N   H E A D E R   R O U T I N E                   ELTTHERP
02536 ** 3000-                                                         |ELTTHERP
02537 ******************************************************************ELTTHERP
02538  3000-COMMON-HEADER-RTNE.                                         ELTTHERP
02539                                                                   ELTTHERP
02540      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTTHERP
02541      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTTHERP
02542      MOVE 'P'            TO  COF-FUNCTION.                        ELTTHERP
02543      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTTHERP
02544                     COF-NBR-DTL-LINES.                            ELTTHERP
02545                                                                   ELTTHERP
02546      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTTHERP
02547                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
02548                     END-EXEC.                                     ELTTHERP
02549                                                                   ELTTHERP
02550      MOVE SPACE  TO  COF-FUNCTION.                                ELTTHERP
02551      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTTHERP
02552                                                                   ELTTHERP
02553  3099-EXIT.  EXIT.                                                ELTTHERP
02554 ******************************************************************ELTTHERP
02555 **                                                               |ELTTHERP
02556 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
02557 ** 3050-   RGO                                                   |ELTTHERP
02558 ******************************************************************ELTTHERP
02559  3050-PLACE-TREAT.                                                ELTTHERP
02560      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
02561                                                              ZERO ELTTHERP
02562         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
02563         MOVE 2 TO WS-CIA                                          ELTTHERP
02564         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)      ELTTHERP
02565                                                                   ELTTHERP
02566         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
02567         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTTHERP
02568         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02569                                               TO  CMF-CODE-VALUE  ELTTHERP
02570                                                                   ELTTHERP
02571         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02572         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02573                                                                   ELTTHERP
02574         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02575             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02576             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02577                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02578                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02579         ELSE                                                      ELTTHERP
02580             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02581         END-IF                                                    ELTTHERP
02582      END-IF.                                                      ELTTHERP
02583                                                                   ELTTHERP
02584      IF YES-CALL-ELUOUTPT                                         ELTTHERP
02585         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
02586         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
02587      END-IF.                                                      ELTTHERP
02588                                                                   ELTTHERP
02589 /*****************************************************************ELTTHERP
02590 *      P R O V I S I O N   P R I C I N G   M E T H O D            ELTTHERP
02591 * 3100-                                                           ELTTHERP
02592 ******************************************************************ELTTHERP
02593  3100-PROV-PRICING-METHD.                                         ELTTHERP
02594                                                                   ELTTHERP
02595                                                                   ELTTHERP
02596 * START CHECK FOR POSSIBLE ERROR                                  ELTTHERP
02597      SET  PLT-INDEX2  TO  1.                                      ELTTHERP
02598      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
02599         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTTHERP
02600                          AND                                      ELTTHERP
02601         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHERP
02602         SET  PLT-INDEX2  TO  2                                    ELTTHERP
02603         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTTHERP
02604                                                              ZERO ELTTHERP
02605            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG          ELTTHERP
02606            MOVE 1  TO  WS-CIA                                     ELTTHERP
02607            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTTHERP
02608                                                                   ELTTHERP
02609      SET  PLT-INDEX2  TO  1.                                      ELTTHERP
02610      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
02611         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTTHERP
02612                               AND                                 ELTTHERP
02613         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTTHERP
02614         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG             ELTTHERP
02615         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTTHERP
02616         ADD +1  TO  WS-CIA.                                       ELTTHERP
02617                                                                   ELTTHERP
02618      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTTHERP
02619         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHERP
02620         SET  PLT-INDEX2  TO  2                                    ELTTHERP
02621         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTTHERP
02622                                                              ZERO ELTTHERP
02623            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG          ELTTHERP
02624            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHERP
02625            ADD +1  TO  WS-CIA.                                    ELTTHERP
02626                                                                   ELTTHERP
02627 * END   CHECK FOR POSSIBLE ERROR                                  ELTTHERP
02628                                                                   ELTTHERP
02629      SET  PLT-INDEX2  TO  1.                                      ELTTHERP
02630      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
02631         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTTHERP
02632                                             ZERO AND  NOT = '19'  ELTTHERP
02633         MOVE 2 TO WS-CIA                                          ELTTHERP
02634         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTTHERP
02635         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
02636                                                                   ELTTHERP
02637         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02638             ADD 1 TO WS-CIA                                       ELTTHERP
02639             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHERP
02640         END-IF                                                    ELTTHERP
02641                                                                   ELTTHERP
02642         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
02643         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
02644         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTTHERP
02645                                               TO   CMF-CODE-VALUE ELTTHERP
02646         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02647         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02648                                                                   ELTTHERP
02649         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02650             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02651             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02652                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02653                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02654         ELSE                                                      ELTTHERP
02655             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02656         END-IF                                                    ELTTHERP
02657      END-IF.                                                      ELTTHERP
02658                                                                   ELTTHERP
02659      SET  PLT-INDEX2  TO  2.                                      ELTTHERP
02660      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTTHERP
02661         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTTHERP
02662               ZERO AND  NOT = '19'                                ELTTHERP
02663                                                                   ELTTHERP
02664         IF NOT YES-CALL-ELUOUTPT                                  ELTTHERP
02665             MOVE 2 TO WS-CIA                                      ELTTHERP
02666             MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)          ELTTHERP
02667             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHERP
02668                                                                   ELTTHERP
02669         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02670             ADD 1 TO WS-CIA                                       ELTTHERP
02671             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
02672         END-IF                                                    ELTTHERP
02673                                                                   ELTTHERP
02674         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
02675         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
02676         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
02677                                                    CMF-CODE-VALUE ELTTHERP
02678         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02679         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02680                                                                   ELTTHERP
02681         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02682             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02683             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02684                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02685                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02686         ELSE                                                      ELTTHERP
02687             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02688         END-IF                                                    ELTTHERP
02689      END-IF.                                                      ELTTHERP
02690                                                                   ELTTHERP
02691      IF YES-CALL-ELUOUTPT                                         ELTTHERP
02692         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
02693         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
02694      END-IF.                                                      ELTTHERP
02695                                                                   ELTTHERP
02696      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
02697         SET PLT-INDEX2  TO  2                                     ELTTHERP
02698      ELSE                                                         ELTTHERP
02699         SET PLT-INDEX2  TO  1.                                    ELTTHERP
02700                                                                   ELTTHERP
02701                                                                   ELTTHERP
02702 /*****************************************************************ELTTHERP
02703 *  P R O F.   C H A R G E S   O N   H O S P I T A L   B I L L     ELTTHERP
02704 *  3150-                                                          ELTTHERP
02705 ******************************************************************ELTTHERP
02706  3150-PROF-CHGR-HSP-CLM.                                          ELTTHERP
02707                                                                   ELTTHERP
02708      SET PLT-INDEX2  TO  1.                                       ELTTHERP
02709                                                                   ELTTHERP
02710      IF (PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)  NOT  =  ZERO       ELTTHERP
02711         AND (WS-DISPLAY-B-FORMAT-TEXT  =  'Y')                    ELTTHERP
02712         AND (PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHERP
02713                   NOT  =  '0'  AND  NOT  =  LOW-VALUES))          ELTTHERP
02714                MOVE 2 TO  WS-CIA                                  ELTTHERP
02715                MOVE WS-HOLD-PROF-CHRG-MSG                         ELTTHERP
02716                               TO COF-DTL-LINE (WS-CIA)            ELTTHERP
02717                MOVE 'Y'         TO  CALL-ELUOUTPT-IND             ELTTHERP
02718                                                                   ELTTHERP
02719                IF DISPLAY-BAS-SUP = 'Y'                           ELTTHERP
02720                    ADD 1 TO WS-CIA                                ELTTHERP
02721                    MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)     ELTTHERP
02722                END-IF                                             ELTTHERP
02723                                                                   ELTTHERP
02724                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTTHERP
02725                MOVE 'PROF-CHRG-HSP-CLM'                           ELTTHERP
02726                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTTHERP
02727                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTTHERP
02728                                   TO  CMF-CODE-VALUE              ELTTHERP
02729                                                                   ELTTHERP
02730                PERFORM 9300-CALL-CODES-MANUAL                     ELTTHERP
02731                                                                   ELTTHERP
02732                MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79        ELTTHERP
02733                IF FIRST-CHAR-SHOW-AS-IS                           ELTTHERP
02734                    MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)       ELTTHERP
02735                    PERFORM 9400-MOVE-TO-COFDTL                    ELTTHERP
02736                         VARYING WS-SUB-CMF FROM 1 BY 1            ELTTHERP
02737                         UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES    ELTTHERP
02738                ELSE                                               ELTTHERP
02739                    PERFORM 9400-COMPRESS-STRING-MOVE              ELTTHERP
02740                END-IF                                             ELTTHERP
02741      END-IF.                                                      ELTTHERP
02742                                                                   ELTTHERP
02743      SET PLT-INDEX2  TO  2.                                       ELTTHERP
02744                                                                   ELTTHERP
02745      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTTHERP
02746         AND WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTTHERP
02747         AND (PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHERP
02748                         NOT  =  '0'  AND  NOT  =  LOW-VALUES)     ELTTHERP
02749         IF NOT YES-CALL-ELUOUTPT                                  ELTTHERP
02750             MOVE 2 TO WS-CIA                                      ELTTHERP
02751             MOVE WS-HOLD-PROF-CHRG-MSG TO COF-DTL-LINE (WS-CIA)   ELTTHERP
02752             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHERP
02753                                                                   ELTTHERP
02754         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02755             ADD 1 TO WS-CIA                                       ELTTHERP
02756             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
02757         END-IF                                                    ELTTHERP
02758         MOVE 'BPB'                TO  CMF-RECORD-PREFIX           ELTTHERP
02759         MOVE 'PROF-CHRG-HSP-CLM'                                  ELTTHERP
02760                            TO         CMF-ELEMENT-SYSTEM-NAME     ELTTHERP
02761         MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHERP
02762                            TO         CMF-CODE-VALUE              ELTTHERP
02763                                                                   ELTTHERP
02764         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02765         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02766                                                                   ELTTHERP
02767         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02768             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02769             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02770                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02771                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02772         ELSE                                                      ELTTHERP
02773             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02774         END-IF                                                    ELTTHERP
02775      END-IF.                                                      ELTTHERP
02776                                                                   ELTTHERP
02777      IF YES-CALL-ELUOUTPT                                         ELTTHERP
02778         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
02779         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
02780      END-IF.                                                      ELTTHERP
02781                                                                   ELTTHERP
02782 /*****************************************************************ELTTHERP
02783 *     E L I G I B L E   M E T H O D   T R E A T M E N T           ELTTHERP
02784 * 3200- (V)                                                       ELTTHERP
02785 ******************************************************************ELTTHERP
02786  3200-ELIG-METHOD-TREAT.                                          ELTTHERP
02787                                                                   ELTTHERP
02788      SET PLT-INDEX2  TO  1.                                       ELTTHERP
02789      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                      ELTTHERP
02790         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
02791         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHERP
02792                                                       NOT = '0'   ELTTHERP
02793         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHERP
02794                                                NOT = LOW-VALUES   ELTTHERP
02795         MOVE 2 TO WS-CIA                                          ELTTHERP
02796         MOVE WS-ELIG-METHOD-OF-TREAT TO COF-DTL-LINE(WS-CIA)      ELTTHERP
02797         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
02798                                                                   ELTTHERP
02799         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02800             ADD 1 TO WS-CIA                                       ELTTHERP
02801             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHERP
02802         END-IF                                                    ELTTHERP
02803                                                                   ELTTHERP
02804         MOVE 'BPB' TO CMF-RECORD-PREFIX                           ELTTHERP
02805         MOVE 'ELIG-METHD-OF-TREAT-IND'                            ELTTHERP
02806                          TO CMF-ELEMENT-SYSTEM-NAME               ELTTHERP
02807         MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHERP
02808                          TO CMF-CODE-VALUE                        ELTTHERP
02809                                                                   ELTTHERP
02810         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02811         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02812                                                                   ELTTHERP
02813         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02814             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02815             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02816                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02817                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02818         ELSE                                                      ELTTHERP
02819             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02820         END-IF                                                    ELTTHERP
02821      END-IF.                                                      ELTTHERP
02822                                                                   ELTTHERP
02823      SET PLT-INDEX2  TO  2.                                       ELTTHERP
02824      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                      ELTTHERP
02825         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
02826         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHERP
02827                                          NOT = '0'                ELTTHERP
02828         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHERP
02829                                          NOT = LOW-VALUES         ELTTHERP
02830                                                                   ELTTHERP
02831         IF NOT YES-CALL-ELUOUTPT                                  ELTTHERP
02832             MOVE 2 TO WS-CIA                                      ELTTHERP
02833             MOVE WS-ELIG-METHOD-OF-TREAT TO COF-DTL-LINE(WS-CIA)  ELTTHERP
02834             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHERP
02835         END-IF                                                    ELTTHERP
02836                                                                   ELTTHERP
02837         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02838             ADD 1 TO WS-CIA                                       ELTTHERP
02839             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
02840         END-IF                                                    ELTTHERP
02841                                                                   ELTTHERP
02842         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHERP
02843         MOVE 'ELIG-METHD-OF-TREAT-IND' TO  CMF-ELEMENT-SYSTEM-NAMEELTTHERP
02844         MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHERP
02845                                                TO   CMF-CODE-VALUEELTTHERP
02846                                                                   ELTTHERP
02847         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02848         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02849                                                                   ELTTHERP
02850         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02851             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02852             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02853                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02854                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02855         ELSE                                                      ELTTHERP
02856             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02857         END-IF                                                    ELTTHERP
02858      END-IF.                                                      ELTTHERP
02859                                                                   ELTTHERP
02860      IF YES-CALL-ELUOUTPT                                         ELTTHERP
02861         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
02862         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
02863      END-IF.                                                      ELTTHERP
02864                                                                   ELTTHERP
02865                                                                   ELTTHERP
02866 /*****************************************************************ELTTHERP
02867 *   TRANSFER TO OTHER RESPONSIBILITY INDICATOR                    ELTTHERP
02868 * 3250- (V)                                                       ELTTHERP
02869 ******************************************************************ELTTHERP
02870  3250-TRANSFER-OTHER-RESPON-IND.                                  ELTTHERP
02871                                                                   ELTTHERP
02872      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)          =  ZEROES    ELTTHERP
02873         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZEROES    ELTTHERP
02874            SET PLT-INDEX2  TO  2                                  ELTTHERP
02875         ELSE                                                      ELTTHERP
02876            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
02877            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
02878            GO TO 3250-EXIT                                        ELTTHERP
02879      ELSE                                                         ELTTHERP
02880         SET PLT-INDEX2  TO  1.                                    ELTTHERP
02881                                                                   ELTTHERP
02882      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZERO ELTTHERP
02883         OR PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02884                                                   = LOW-VALUES    ELTTHERP
02885         GO TO 3250-EXIT.                                          ELTTHERP
02886                                                                   ELTTHERP
02887      MOVE 2 TO WS-CIA.                                            ELTTHERP
02888      MOVE 'BP'                     TO  CMF-RECORD-PREFIX.         ELTTHERP
02889      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTTHERP
02890      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTTHERP
02891           TO CMF-CODE-VALUE.                                      ELTTHERP
02892      PERFORM 9300-CALL-CODES-MANUAL                               ELTTHERP
02893      MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79.                 ELTTHERP
02894                                                                   ELTTHERP
02895      IF FIRST-CHAR-SHOW-AS-IS                                     ELTTHERP
02896          MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)                 ELTTHERP
02897          PERFORM 9400-MOVE-TO-COFDTL                              ELTTHERP
02898               VARYING WS-SUB-CMF FROM 1 BY 1                      ELTTHERP
02899               UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES              ELTTHERP
02900      ELSE                                                         ELTTHERP
02901          PERFORM 9400-COMPRESS-STRING-MOVE                        ELTTHERP
02902      END-IF.                                                      ELTTHERP
02903                                                                   ELTTHERP
02904      MOVE 'N' TO  CALL-ELUOUTPT-IND.                              ELTTHERP
02905      PERFORM 9200-TEXT-OUTPUT-REQUEST.                            ELTTHERP
02906  3250-EXIT.  EXIT.                                                ELTTHERP
02907 /*****************************************************************ELTTHERP
02908 *      H O S P I T A L   C O N D I T I O N   R E L .   I N D .    ELTTHERP
02909 * 3300-   (V2)                                                    ELTTHERP
02910 ******************************************************************ELTTHERP
02911  3300-HOSP-COND-REL-IND.                                          ELTTHERP
02912                                                                   ELTTHERP
02913      SET PLT-INDEX2  TO  1.                                       ELTTHERP
02914      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHERP
02915         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
02916         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02917                                 NOT = '0'                         ELTTHERP
02918         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02919                                 NOT = LOW-VALUES                  ELTTHERP
02920         MOVE 2 TO WS-CIA                                          ELTTHERP
02921         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHERP
02922         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
02923                                                                   ELTTHERP
02924         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02925             ADD 1 TO WS-CIA                                       ELTTHERP
02926             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHERP
02927         END-IF                                                    ELTTHERP
02928                                                                   ELTTHERP
02929         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHERP
02930         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHERP
02931         MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
02932                                                     CMF-CODE-VALUEELTTHERP
02933                                                                   ELTTHERP
02934         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02935         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02936                                                                   ELTTHERP
02937         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02938             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02939             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02940                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02941                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02942         ELSE                                                      ELTTHERP
02943             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02944         END-IF                                                    ELTTHERP
02945      END-IF.                                                      ELTTHERP
02946                                                                   ELTTHERP
02947      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHERP
02948         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
02949         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02950                                 NOT = '0'                         ELTTHERP
02951         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02952                                 NOT = LOW-VALUES                  ELTTHERP
02953         MOVE 2 TO WS-CIA                                          ELTTHERP
02954         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHERP
02955         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
02956                                                                   ELTTHERP
02957         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02958             ADD 1 TO WS-CIA                                       ELTTHERP
02959             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHERP
02960         END-IF                                                    ELTTHERP
02961                                                                   ELTTHERP
02962         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHERP
02963         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHERP
02964         MOVE PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
02965                                                     CMF-CODE-VALUEELTTHERP
02966                                                                   ELTTHERP
02967         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
02968         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
02969                                                                   ELTTHERP
02970         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
02971             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
02972             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
02973                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
02974                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
02975         ELSE                                                      ELTTHERP
02976             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
02977         END-IF                                                    ELTTHERP
02978      END-IF.                                                      ELTTHERP
02979                                                                   ELTTHERP
02980      SET PLT-INDEX2  TO  2.                                       ELTTHERP
02981                                                                   ELTTHERP
02982      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHERP
02983         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
02984         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02985                                 NOT = '0'                         ELTTHERP
02986         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
02987                                 NOT = LOW-VALUES                  ELTTHERP
02988         MOVE 2 TO WS-CIA                                          ELTTHERP
02989         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHERP
02990         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
02991                                                                   ELTTHERP
02992         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
02993             ADD 1 TO WS-CIA                                       ELTTHERP
02994             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
02995         END-IF                                                    ELTTHERP
02996                                                                   ELTTHERP
02997         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHERP
02998         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHERP
02999         MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03000                                                     CMF-CODE-VALUEELTTHERP
03001                                                                   ELTTHERP
03002         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03003         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03004                                                                   ELTTHERP
03005         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03006             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03007             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03008                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03009                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03010         ELSE                                                      ELTTHERP
03011             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03012         END-IF                                                    ELTTHERP
03013      END-IF.                                                      ELTTHERP
03014                                                                   ELTTHERP
03015      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHERP
03016         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03017         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
03018                                 NOT = '0'                         ELTTHERP
03019         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHERP
03020                                 NOT = LOW-VALUES                  ELTTHERP
03021         MOVE 2 TO WS-CIA                                          ELTTHERP
03022         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHERP
03023         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHERP
03024                                                                   ELTTHERP
03025         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03026             ADD 1 TO WS-CIA                                       ELTTHERP
03027             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
03028         END-IF                                                    ELTTHERP
03029                                                                   ELTTHERP
03030         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03031         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHERP
03032         MOVE PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03033                                                     CMF-CODE-VALUEELTTHERP
03034                                                                   ELTTHERP
03035         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03036         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03037                                                                   ELTTHERP
03038         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03039             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03040             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03041                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03042                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03043         ELSE                                                      ELTTHERP
03044             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03045         END-IF                                                    ELTTHERP
03046      END-IF.                                                      ELTTHERP
03047                                                                   ELTTHERP
03048      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03049         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03050         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03051      END-IF.                                                      ELTTHERP
03052                                                                   ELTTHERP
03053                                                                   ELTTHERP
03054 /*****************************************************************ELTTHERP
03055 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C    ELTTHERP
03056 **   AND                                                          ELTTHERP
03057 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C      ELTTHERP
03058 **   3400- (V)                                                    ELTTHERP
03059 ******************************************************************ELTTHERP
03060  3400-SPILLOVER-COINS-N-DEDBL.                                    ELTTHERP
03061                                                                   ELTTHERP
03062      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03063      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTTHERP
03064         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTTHERP
03065                                                       NOT =  '0'  ELTTHERP
03066         AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHERP
03067                                         NOT = LOW-VALUES          ELTTHERP
03068         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03069         MOVE 2 TO WS-CIA                                          ELTTHERP
03070         MOVE WS-SPILLOVER TO COF-DTL-LINE (WS-CIA)                ELTTHERP
03071         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
03072         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTTHERP
03073                                           CMF-ELEMENT-SYSTEM-NAME ELTTHERP
03074         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTTHERP
03075                                           TO   CMF-CODE-VALUE     ELTTHERP
03076                                                                   ELTTHERP
03077         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03078         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03079                                                                   ELTTHERP
03080         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03081             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03082             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03083                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03084                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03085         ELSE                                                      ELTTHERP
03086             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03087         END-IF                                                    ELTTHERP
03088      END-IF.                                                      ELTTHERP
03089 **                                                               |ELTTHERP
03090 **---------------------------------------------------------------+ELTTHERP
03091 **                                                               |ELTTHERP
03092                                                                   ELTTHERP
03093      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTTHERP
03094         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTTHERP
03095                                                       NOT =  '0'  ELTTHERP
03096         AND PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTTHERP
03097              NOT = LOW-VALUES                                     ELTTHERP
03098         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03099         ADD 1 TO WS-CIA                                           ELTTHERP
03100         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTTHERP
03101         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHERP
03102         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTTHERP
03103         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTTHERP
03104                                                     CMF-CODE-VALUEELTTHERP
03105                                                                   ELTTHERP
03106         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03107         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03108                                                                   ELTTHERP
03109         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03110             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03111             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03112                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03113                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03114         ELSE                                                      ELTTHERP
03115             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03116         END-IF                                                    ELTTHERP
03117      END-IF.                                                      ELTTHERP
03118                                                                   ELTTHERP
03119      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03120         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03121         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03122      END-IF.                                                      ELTTHERP
03123                                                                   ELTTHERP
03124 /*****************************************************************ELTTHERP
03125 *      H O S P I T A L   A D M I S S I O N   R E S T R I C T I O NELTTHERP
03126 **   3500- (V)                                                    ELTTHERP
03127 ******************************************************************ELTTHERP
03128  3500-HOSP-ADM-RESTRN.                                            ELTTHERP
03129                                                                   ELTTHERP
03130      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03131      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHERP
03132         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03133         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03134                              NOT = '0'                            ELTTHERP
03135         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03136                              NOT = LOW-VALUES                     ELTTHERP
03137                                                                   ELTTHERP
03138         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03139         MOVE 2 TO WS-CIA                                          ELTTHERP
03140                                                                   ELTTHERP
03141         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03142         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03143         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03144                                                     CMF-CODE-VALUEELTTHERP
03145         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03146                                                                   ELTTHERP
03147         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03148         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03149             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03150             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03151                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03152                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03153         ELSE                                                      ELTTHERP
03154             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03155         END-IF                                                    ELTTHERP
03156                                                                   ELTTHERP
03157         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03158                                                 WS-MUST-BEGIN-DAYSELTTHERP
03159         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03160             ADD 1 TO WS-CIA                                       ELTTHERP
03161             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03162         END-IF                                                    ELTTHERP
03163         ADD 1 TO WS-CIA                                           ELTTHERP
03164         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03165                                                                   ELTTHERP
03166      END-IF.                                                      ELTTHERP
03167 *   *****************                                             ELTTHERP
03168                                                                   ELTTHERP
03169      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHERP
03170         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03171         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03172                              NOT = '0'                            ELTTHERP
03173         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03174                              NOT = LOW-VALUES                     ELTTHERP
03175                                                                   ELTTHERP
03176         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03177         MOVE 2 TO WS-CIA                                          ELTTHERP
03178                                                                   ELTTHERP
03179         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03180         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03181         MOVE PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03182                                                     CMF-CODE-VALUEELTTHERP
03183         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03184                                                                   ELTTHERP
03185         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03186         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03187             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03188             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03189                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03190                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03191         ELSE                                                      ELTTHERP
03192             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03193         END-IF                                                    ELTTHERP
03194                                                                   ELTTHERP
03195         MOVE PLA-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03196                                                 WS-MUST-BEGIN-DAYSELTTHERP
03197         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03198             ADD 1 TO WS-CIA                                       ELTTHERP
03199             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03200         END-IF                                                    ELTTHERP
03201         ADD 1 TO WS-CIA                                           ELTTHERP
03202         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03203                                                                   ELTTHERP
03204      END-IF.                                                      ELTTHERP
03205 *   *****************                                             ELTTHERP
03206                                                                   ELTTHERP
03207      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'E'                    ELTTHERP
03208         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03209         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03210                              NOT = '0'                            ELTTHERP
03211         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03212                              NOT = LOW-VALUES                     ELTTHERP
03213                                                                   ELTTHERP
03214         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03215         MOVE 2 TO WS-CIA                                          ELTTHERP
03216                                                                   ELTTHERP
03217         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03218         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03219         MOVE PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03220                                                     CMF-CODE-VALUEELTTHERP
03221         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03222                                                                   ELTTHERP
03223         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03224         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03225             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03226             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03227                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03228                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03229         ELSE                                                      ELTTHERP
03230             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03231         END-IF                                                    ELTTHERP
03232                                                                   ELTTHERP
03233         MOVE PLE-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03234                                                 WS-MUST-BEGIN-DAYSELTTHERP
03235         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03236             ADD 1 TO WS-CIA                                       ELTTHERP
03237             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03238         END-IF                                                    ELTTHERP
03239         ADD 1 TO WS-CIA                                           ELTTHERP
03240         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03241                                                                   ELTTHERP
03242      END-IF.                                                      ELTTHERP
03243 *   *****************                                             ELTTHERP
03244      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03245                                                                   ELTTHERP
03246      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHERP
03247         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03248         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03249                              NOT = '0'                            ELTTHERP
03250         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03251                              NOT = LOW-VALUES                     ELTTHERP
03252                                                                   ELTTHERP
03253         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03254         MOVE 2 TO WS-CIA                                          ELTTHERP
03255                                                                   ELTTHERP
03256         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03257         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03258         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03259                                                     CMF-CODE-VALUEELTTHERP
03260         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03261                                                                   ELTTHERP
03262         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03263         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03264             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03265             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03266                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03267                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03268         ELSE                                                      ELTTHERP
03269             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03270         END-IF                                                    ELTTHERP
03271                                                                   ELTTHERP
03272         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03273                                                 WS-MUST-BEGIN-DAYSELTTHERP
03274         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03275             ADD 1 TO WS-CIA                                       ELTTHERP
03276             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03277         END-IF                                                    ELTTHERP
03278         ADD 1 TO WS-CIA                                           ELTTHERP
03279         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03280                                                                   ELTTHERP
03281      END-IF.                                                      ELTTHERP
03282 *   *****************                                             ELTTHERP
03283                                                                   ELTTHERP
03284      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHERP
03285         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03286         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03287                              NOT = '0'                            ELTTHERP
03288         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03289                              NOT = LOW-VALUES                     ELTTHERP
03290                                                                   ELTTHERP
03291         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03292         MOVE 2 TO WS-CIA                                          ELTTHERP
03293                                                                   ELTTHERP
03294         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03295         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03296         MOVE PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03297                                                     CMF-CODE-VALUEELTTHERP
03298         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03299                                                                   ELTTHERP
03300         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03301         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03302             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03303             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03304                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03305                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03306         ELSE                                                      ELTTHERP
03307             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03308         END-IF                                                    ELTTHERP
03309                                                                   ELTTHERP
03310         MOVE PLA-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03311                                                 WS-MUST-BEGIN-DAYSELTTHERP
03312         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03313             ADD 1 TO WS-CIA                                       ELTTHERP
03314             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03315         END-IF                                                    ELTTHERP
03316         ADD 1 TO WS-CIA                                           ELTTHERP
03317         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03318                                                                   ELTTHERP
03319      END-IF.                                                      ELTTHERP
03320 *   *****************                                             ELTTHERP
03321                                                                   ELTTHERP
03322      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'E'                    ELTTHERP
03323         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHERP
03324         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03325                              NOT = '0'                            ELTTHERP
03326         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHERP
03327                              NOT = LOW-VALUES                     ELTTHERP
03328                                                                   ELTTHERP
03329         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03330         MOVE 2 TO WS-CIA                                          ELTTHERP
03331                                                                   ELTTHERP
03332         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03333         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHERP
03334         MOVE PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03335                                                     CMF-CODE-VALUEELTTHERP
03336         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03337                                                                   ELTTHERP
03338         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03339         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03340             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03341             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03342                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03343                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03344         ELSE                                                      ELTTHERP
03345             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03346         END-IF                                                    ELTTHERP
03347                                                                   ELTTHERP
03348         MOVE PLE-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHERP
03349                                                 WS-MUST-BEGIN-DAYSELTTHERP
03350         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03351             ADD 1 TO WS-CIA                                       ELTTHERP
03352             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03353         END-IF                                                    ELTTHERP
03354         ADD 1 TO WS-CIA                                           ELTTHERP
03355         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHERP
03356                                                                   ELTTHERP
03357      END-IF.                                                      ELTTHERP
03358      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03359         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03360         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03361      END-IF.                                                      ELTTHERP
03362                                                                   ELTTHERP
03363 /*****************************************************************ELTTHERP
03364 *      H O S P I T A L   A D M I S S I O N   R E S T R I C T I O NELTTHERP
03365 *                F O R    'C R P O  B'  P R O V I S I O N         ELTTHERP
03366 **   3501-                                                        ELTTHERP
03367 ******************************************************************ELTTHERP
03368  3501-PRIOR-ADMIS-REQ.                                            ELTTHERP
03369                                                                   ELTTHERP
03370      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03371      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZEROES    ELTTHERP
03372                         AND                                       ELTTHERP
03373         GCG-BC-CARD-REH-PRIOR-ADM  NOT  =  '0'                    ELTTHERP
03374         MOVE 'Y'            TO  CALL-ELUOUTPT-IND                 ELTTHERP
03375         MOVE 2 TO WS-CIA                                          ELTTHERP
03376         MOVE WS-PRIOR-ADM TO COF-DTL-LINE (WS-CIA)                ELTTHERP
03377                                                                   ELTTHERP
03378         MOVE 'GROUP'        TO  CMF-RECORD-PREFIX                 ELTTHERP
03379         MOVE 'BC-CARD-REH-PRIOR-ADM'                              ELTTHERP
03380                             TO  CMF-ELEMENT-SYSTEM-NAME           ELTTHERP
03381         MOVE GCG-BC-CARD-REH-PRIOR-ADM  TO  CMF-CODE-VALUE        ELTTHERP
03382                                                                   ELTTHERP
03383         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03384             ADD 1 TO WS-CIA                                       ELTTHERP
03385             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03386         END-IF                                                    ELTTHERP
03387                                                                   ELTTHERP
03388         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03389                                                                   ELTTHERP
03390         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03391         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03392             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03393             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03394                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03395                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03396         ELSE                                                      ELTTHERP
03397             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03398         END-IF                                                    ELTTHERP
03399                                                                   ELTTHERP
03400      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03401      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROS      ELTTHERP
03402                         AND                                       ELTTHERP
03403         GCG-MM-CARD-REH-PRIOR-ADM  NOT  =  '0'                    ELTTHERP
03404                         AND                                       ELTTHERP
03405         DISPLAY-BAS-SUP = 'Y'                                     ELTTHERP
03406         IF CALL-ELUOUTPT-IND = 'N'                                ELTTHERP
03407             MOVE 'Y' TO CALL-ELUOUTPT-IND                         ELTTHERP
03408             MOVE 2 TO WS-CIA                                      ELTTHERP
03409             MOVE WS-PRIOR-ADM TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03410         END-IF                                                    ELTTHERP
03411         MOVE 'Y'            TO  CALL-ELUOUTPT-IND                 ELTTHERP
03412         MOVE 'GROUP'        TO  CMF-RECORD-PREFIX                 ELTTHERP
03413         MOVE 'MM-CARD-REH-PRIOR-ADM'                              ELTTHERP
03414                             TO  CMF-ELEMENT-SYSTEM-NAME           ELTTHERP
03415         MOVE GCG-MM-CARD-REH-PRIOR-ADM  TO  CMF-CODE-VALUE        ELTTHERP
03416                                                                   ELTTHERP
03417         ADD 1 TO WS-CIA                                           ELTTHERP
03418         MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)                ELTTHERP
03419                                                                   ELTTHERP
03420         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03421                                                                   ELTTHERP
03422         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03423         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03424             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03425             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03426                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03427                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03428         ELSE                                                      ELTTHERP
03429             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03430         END-IF                                                    ELTTHERP
03431                                                                   ELTTHERP
03432      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03433         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03434         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03435      END-IF.                                                      ELTTHERP
03436                                                                   ELTTHERP
03437 /*****************************************************************ELTTHERP
03438 *        B E N E F I T   S C O P E   I D E N T I F I E R          ELTTHERP
03439 **   3600-                                                        ELTTHERP
03440 ******************************************************************ELTTHERP
03441  3600-BENEFIT-SCOPE-ID.                                           ELTTHERP
03442                                                                   ELTTHERP
03443      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03444      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03445         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E' AND                  ELTTHERP
03446         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTTHERP
03447                                       '0000' AND  NOT =  '00  '   ELTTHERP
03448         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03449         MOVE 2 TO  WS-CIA                                         ELTTHERP
03450         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTTHERP
03451                                                                   ELTTHERP
03452         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03453             ADD 1 TO WS-CIA                                       ELTTHERP
03454             MOVE WS-BASIC  TO COF-DTL-LINE (WS-CIA)               ELTTHERP
03455         END-IF                                                    ELTTHERP
03456                                                                   ELTTHERP
03457         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03458         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTTHERP
03459         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTTHERP
03460                                                    CMF-CODE-VALUE ELTTHERP
03461                                                                   ELTTHERP
03462         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03463                                                                   ELTTHERP
03464         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03465         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03466             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03467             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03468                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03469                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03470         ELSE                                                      ELTTHERP
03471             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03472         END-IF                                                    ELTTHERP
03473                                                                   ELTTHERP
03474      END-IF.                                                      ELTTHERP
03475                                                                   ELTTHERP
03476      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03477      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03478         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E' AND                  ELTTHERP
03479         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTTHERP
03480                                       '0000' AND  NOT =  '00  '   ELTTHERP
03481         IF CALL-ELUOUTPT-IND = 'N'                                ELTTHERP
03482             MOVE 'Y' TO CALL-ELUOUTPT-IND                         ELTTHERP
03483             MOVE 2 TO WS-CIA                                      ELTTHERP
03484             MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)          ELTTHERP
03485         END-IF                                                    ELTTHERP
03486                                                                   ELTTHERP
03487         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03488             ADD 1 TO WS-CIA                                       ELTTHERP
03489             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
03490         END-IF                                                    ELTTHERP
03491                                                                   ELTTHERP
03492         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHERP
03493         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTTHERP
03494         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTTHERP
03495                                                    CMF-CODE-VALUE ELTTHERP
03496                                                                   ELTTHERP
03497         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHERP
03498                                                                   ELTTHERP
03499         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHERP
03500         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHERP
03501             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHERP
03502             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHERP
03503                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHERP
03504                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHERP
03505         ELSE                                                      ELTTHERP
03506             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHERP
03507         END-IF                                                    ELTTHERP
03508                                                                   ELTTHERP
03509      END-IF.                                                      ELTTHERP
03510                                                                   ELTTHERP
03511      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03512         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03513         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03514      END-IF.                                                      ELTTHERP
03515                                                                   ELTTHERP
03516 /*****************************************************************ELTTHERP
03517 *     G E N E R A L   G E T   T A B U L A R   R T N E             ELTTHERP
03518  3700-GENERAL-TABULAR-RTNE.                                       ELTTHERP
03519 ****************************************************************  ELTTHERP
03520 *                  A A R   T A B U L A R                       *  ELTTHERP
03521 ****************************************************************  ELTTHERP
03522      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03523      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03524         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03525                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03526         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03527         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTTHERP
03528         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTTHERP
03529      ELSE                                                         ELTTHERP
03530         SET PLT-INDEX2  TO  2                                     ELTTHERP
03531         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03532            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03533                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03534            MOVE 'Y'  TO  CALL-ELUOUTPT-IND                        ELTTHERP
03535            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTTHERP
03536            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)          ELTTHERP
03537         END-IF                                                    ELTTHERP
03538      END-IF.                                                      ELTTHERP
03539                                                                   ELTTHERP
03540      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03541         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03542         MOVE 1  TO  WS-CIA                                        ELTTHERP
03543         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
03544             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
03545         END-EXEC                                                  ELTTHERP
03546      END-IF.                                                      ELTTHERP
03547 *--------------------------------------------------------------*  ELTTHERP
03548 *                  P P F   T A B U L A R                       *  ELTTHERP
03549 *--------------------------------------------------------------*  ELTTHERP
03550      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03551      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03552         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03553                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03554         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTTHERP
03555                                             KWA-GCTABULR-KEY      ELTTHERP
03556         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHERP
03557         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTTHERP
03558               COMMAREA(DFHCOMMAREA)                               ELTTHERP
03559         END-EXEC                                                  ELTTHERP
03560      ELSE                                                         ELTTHERP
03561         SET PLT-INDEX2  TO  2                                     ELTTHERP
03562         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03563            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03564                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03565            MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  ELTTHERP
03566                                          TO KWA-GCTABULR-KEY      ELTTHERP
03567            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHERP
03568            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTTHERP
03569                  COMMAREA(DFHCOMMAREA)                            ELTTHERP
03570            END-EXEC                                               ELTTHERP
03571         END-IF                                                    ELTTHERP
03572      END-IF.                                                      ELTTHERP
03573 *--------------------------------------------------------------*  ELTTHERP
03574 *                  P V E   T A B U L A R                       *  ELTTHERP
03575 *--------------------------------------------------------------*  ELTTHERP
03576                                                                   ELTTHERP
03577      MOVE  +2     TO  WS-CIA.                                     ELTTHERP
03578      MOVE WS-PROVIDER-ELIGIBILITY TO COF-DTL-LINE (2).            ELTTHERP
03579      ADD +1  WS-CIA GIVING COF-NBR-DTL-LINES.                     ELTTHERP
03580      EXEC CICS LINK                                               ELTTHERP
03581                PROGRAM('ELUOUTPT')                                ELTTHERP
03582                COMMAREA (DFHCOMMAREA)                             ELTTHERP
03583      END-EXEC.                                                    ELTTHERP
03584                                                                   ELTTHERP
03585                                                                   ELTTHERP
03586 *--------------------------------------------------------------*  ELTTHERP
03587 *                  A B M   T A B U L A R                       *  ELTTHERP
03588 *--------------------------------------------------------------*  ELTTHERP
03589      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03590      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTTHERP
03591                              AND                                  ELTTHERP
03592         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03593                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03594         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTTHERP
03595                                              KWA-GCTABULR-KEY     ELTTHERP
03596         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHERP
03597         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTTHERP
03598               COMMAREA(DFHCOMMAREA)                               ELTTHERP
03599         END-EXEC                                                  ELTTHERP
03600      ELSE                                                         ELTTHERP
03601         SET PLT-INDEX2  TO  2                                     ELTTHERP
03602         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03603            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03604                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03605          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03606                                             KWA-GCTABULR-KEY      ELTTHERP
03607            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHERP
03608            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTTHERP
03609                  COMMAREA(DFHCOMMAREA)                            ELTTHERP
03610            END-EXEC                                               ELTTHERP
03611         END-IF                                                    ELTTHERP
03612      END-IF.                                                      ELTTHERP
03613 *--------------------------------------------------------------*  ELTTHERP
03614 *                  A C L   T A B U L A R                       *  ELTTHERP
03615 *--------------------------------------------------------------*  ELTTHERP
03616      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03617      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03618         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03619                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03620         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHERP
03621                                              KWA-GCTABULR-KEY     ELTTHERP
03622         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHERP
03623         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTTHERP
03624               COMMAREA(DFHCOMMAREA)                               ELTTHERP
03625         END-EXEC                                                  ELTTHERP
03626      ELSE                                                         ELTTHERP
03627         SET PLT-INDEX2  TO  2                                     ELTTHERP
03628         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03629            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03630                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03631          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03632                                              KWA-GCTABULR-KEY     ELTTHERP
03633            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHERP
03634            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTTHERP
03635                  COMMAREA(DFHCOMMAREA)                            ELTTHERP
03636            END-EXEC                                               ELTTHERP
03637         END-IF                                                    ELTTHERP
03638      END-IF.                                                      ELTTHERP
03639 *--------------------------------------------------------------*  ELTTHERP
03640 *                  A D L   T A B U L A R                       *  ELTTHERP
03641 *--------------------------------------------------------------*  ELTTHERP
03642      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03643      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03644         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03645                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03646         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHERP
03647                                             KWA-GCTABULR-KEY      ELTTHERP
03648         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHERP
03649         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTTHERP
03650               COMMAREA(DFHCOMMAREA)                               ELTTHERP
03651         END-EXEC                                                  ELTTHERP
03652      ELSE                                                         ELTTHERP
03653         SET PLT-INDEX2  TO  2                                     ELTTHERP
03654         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03655            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03656                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03657          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03658                                               KWA-GCTABULR-KEY    ELTTHERP
03659            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHERP
03660            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTTHERP
03661                  COMMAREA(DFHCOMMAREA)                            ELTTHERP
03662            END-EXEC                                               ELTTHERP
03663         END-IF                                                    ELTTHERP
03664      END-IF.                                                      ELTTHERP
03665 *--------------------------------------------------------------*  ELTTHERP
03666 *                  A O L   T A B U L A R                       *  ELTTHERP
03667 *--------------------------------------------------------------*  ELTTHERP
03668      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03669      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
03670         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHERP
03671                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03672         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHERP
03673                                             KWA-GCTABULR-KEY      ELTTHERP
03674         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHERP
03675         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTTHERP
03676               COMMAREA(DFHCOMMAREA)                               ELTTHERP
03677         END-EXEC                                                  ELTTHERP
03678      ELSE                                                         ELTTHERP
03679         SET PLT-INDEX2  TO  2                                     ELTTHERP
03680         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHERP
03681            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
03682                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHERP
03683          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTTHERP
03684                                              KWA-GCTABULR-KEY     ELTTHERP
03685            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHERP
03686            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTTHERP
03687                  COMMAREA(DFHCOMMAREA)                            ELTTHERP
03688            END-EXEC                                               ELTTHERP
03689         END-IF                                                    ELTTHERP
03690      END-IF.                                                      ELTTHERP
03691                                                                   ELTTHERP
03692 *--------------------------------------------------------------*  ELTTHERP
03693 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTTHERP
03694 *--------------------------------------------------------------*  ELTTHERP
03695                                                                   ELTTHERP
03696      MOVE  +1     TO  WS-CIA.                                     ELTTHERP
03697      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTTHERP
03698      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTTHERP
03699      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTTHERP
03700             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
03701      END-EXEC.                                                    ELTTHERP
03702                                                                   ELTTHERP
03703                                                                   ELTTHERP
03704 *--------------------------------------------------------------*  ELTTHERP
03705 * 3750-GET-TABULAR RECORD                                      *  ELTTHERP
03706 *--------------------------------------------------------------*  ELTTHERP
03707  3750-GET-TABULAR-RECORD.                                         ELTTHERP
03708                                                                   ELTTHERP
03709      SET CIA-GCTABULR-DDN TO TRUE.                                ELTTHERP
03710      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
03711          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTTHERP
03712      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTTHERP
03713      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTTHERP
03714      SET IOP-RD                          TO TRUE.                 ELTTHERP
03715      SET IOP-FCQ-NONE                    TO TRUE.                 ELTTHERP
03716      SET IOP-KVQ-NONE                    TO TRUE.                 ELTTHERP
03717      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTTHERP
03718                                                                   ELTTHERP
03719      EXEC CICS LINK                                               ELTTHERP
03720                PROGRAM ('ELUIOPGM')                               ELTTHERP
03721                COMMAREA (DFHCOMMAREA)                             ELTTHERP
03722      END-EXEC.                                                    ELTTHERP
03723                                                                   ELTTHERP
03724      IF IOP-RC-NOTFND                                             ELTTHERP
03725         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTTHERP
03726         EXEC CICS ABEND                                           ELTTHERP
03727                   ABCODE(CIA-ABCODE)                              ELTTHERP
03728         END-EXEC                                                  ELTTHERP
03729      ELSE                                                         ELTTHERP
03730          IF NOT IOP-RC-OK                                         ELTTHERP
03731             SET CIA-AB-CRITIO TO TRUE                             ELTTHERP
03732             EXEC CICS ABEND                                       ELTTHERP
03733                       ABCODE(CIA-ABCODE)                          ELTTHERP
03734             END-EXEC                                              ELTTHERP
03735          END-IF                                                   ELTTHERP
03736      END-IF.                                                      ELTTHERP
03737                                                                   ELTTHERP
03738 /*****************************************************************ELTTHERP
03739 *        B E N E F I T   M A X I M U M   V I S I T S              ELTTHERP
03740 * 3800-                                                           ELTTHERP
03741 ******************************************************************ELTTHERP
03742  3800-BEN-MAXIMUM-VISITS.                                         ELTTHERP
03743                                                                   ELTTHERP
03744      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03745      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZERO           ELTTHERP
03746         AND                                                       ELTTHERP
03747         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHERP
03748         AND                                                       ELTTHERP
03749         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  '0'ELTTHERP
03750         AND                                                       ELTTHERP
03751         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)            ELTTHERP
03752                                     NOT = LOW-VALUES              ELTTHERP
03753                                                                   ELTTHERP
03754         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03755         MOVE 2 TO WS-CIA                                          ELTTHERP
03756         MOVE WS-MAX-NUM-OF-VISITS  TO  COF-DTL-LINE(WS-CIA)       ELTTHERP
03757                                                                   ELTTHERP
03758         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03759             ADD 1 TO WS-CIA                                       ELTTHERP
03760             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHERP
03761         END-IF                                                    ELTTHERP
03762         PERFORM 3810-CK-N-MOVE                                    ELTTHERP
03763      END-IF.                                                      ELTTHERP
03764                                                                   ELTTHERP
03765      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03766      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZERO           ELTTHERP
03767         AND                                                       ELTTHERP
03768         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHERP
03769         AND                                                       ELTTHERP
03770         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  '0'ELTTHERP
03771         AND                                                       ELTTHERP
03772         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)            ELTTHERP
03773                                     NOT = LOW-VALUES              ELTTHERP
03774                                                                   ELTTHERP
03775         IF NOT YES-CALL-ELUOUTPT                                  ELTTHERP
03776            MOVE 'Y' TO CALL-ELUOUTPT-IND                          ELTTHERP
03777            ADD 2 TO WS-CIA                                        ELTTHERP
03778            MOVE WS-MAX-NUM-OF-VISITS  TO  COF-DTL-LINE(WS-CIA)    ELTTHERP
03779         END-IF                                                    ELTTHERP
03780                                                                   ELTTHERP
03781                                                                   ELTTHERP
03782         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03783             ADD 1 TO WS-CIA                                       ELTTHERP
03784             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHERP
03785         END-IF                                                    ELTTHERP
03786         PERFORM 3810-CK-N-MOVE                                    ELTTHERP
03787      END-IF.                                                      ELTTHERP
03788                                                                   ELTTHERP
03789      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03790         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03791         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03792      END-IF.                                                      ELTTHERP
03793                                                                   ELTTHERP
03794                                                                   ELTTHERP
03795 /*****************************************************************ELTTHERP
03796  3810-CK-N-MOVE.                                                  ELTTHERP
03797                                                                   ELTTHERP
03798      MOVE PLE-BEN-MAX-VISITS-IND (PLT-INDEX1, PLT-INDEX2)         ELTTHERP
03799                           TO CMF-CODE-VALUE.                      ELTTHERP
03800      MOVE 'BPE'                     TO  CMF-RECORD-PREFIX.        ELTTHERP
03801      MOVE 'BEN-MAX-VISITS-IND' TO       CMF-ELEMENT-SYSTEM-NAME.  ELTTHERP
03802      PERFORM 9300-CALL-CODES-MANUAL.                              ELTTHERP
03803      ADD 1 TO WS-CIA.                                             ELTTHERP
03804                                                                   ELTTHERP
03805      IF PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1, PLT-INDEX2)          ELTTHERP
03806                                                    = ZEROS        ELTTHERP
03807         MOVE CMF-DESCR-LINE(1) TO COF-DTL-LINE(WS-CIA)            ELTTHERP
03808      ELSE                                                         ELTTHERP
03809         MOVE PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1, PLT-INDEX2)     ELTTHERP
03810              TO WS-DTL-MAX-DAYS                                   ELTTHERP
03811         MOVE CMF-DESCR-LINE(1) TO WS-DTL-MAX-IND                  ELTTHERP
03812         MOVE WS-MAX-DAYS TO COF-DTL-LINE(WS-CIA)                  ELTTHERP
03813      END-IF.                                                      ELTTHERP
03814 /*****************************************************************ELTTHERP
03815 *3900-                                                            ELTTHERP
03816 **************************************************************    ELTTHERP
03817  3900-MAX-AMOUNT-PER-VISIT.                                       ELTTHERP
03818                                                                   ELTTHERP
03819      MOVE SPACES  TO  WS-DTL-BASIC-LONG,  WS-DTL-SUPP-LONG.       ELTTHERP
03820                                                                   ELTTHERP
03821      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03822      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHERP
03823         AND                                                       ELTTHERP
03824         PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTTHERP
03825         AND                                                       ELTTHERP
03826         PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2) NOT = ZERO  ELTTHERP
03827                                                                   ELTTHERP
03828         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03829         MOVE 2 TO WS-CIA                                          ELTTHERP
03830         MOVE WS-MAX-AMT-PER-VISIT  TO  COF-DTL-LINE(WS-CIA)       ELTTHERP
03831                                                                   ELTTHERP
03832         ADD  +1  TO  WS-CIA                                       ELTTHERP
03833         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO    ELTTHERP
03834                                                    WS-BASIC-AMOUNTELTTHERP
03835         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03836            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHERP
03837         ELSE                                                      ELTTHERP
03838            MOVE WS-DTL-BASIC-LONG TO COF-DTL-LINE(WS-CIA)         ELTTHERP
03839         END-IF                                                    ELTTHERP
03840      END-IF.                                                      ELTTHERP
03841                                                                   ELTTHERP
03842      SET PLT-INDEX2  TO  2.                                       ELTTHERP
03843                                                                   ELTTHERP
03844      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHERP
03845         AND                                                       ELTTHERP
03846         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTTHERP
03847         AND                                                       ELTTHERP
03848         PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2) NOT = ZERO  ELTTHERP
03849                                                                   ELTTHERP
03850         IF NOT YES-CALL-ELUOUTPT                                  ELTTHERP
03851             MOVE 'Y' TO CALL-ELUOUTPT-IND                         ELTTHERP
03852             MOVE 2 TO WS-CIA                                      ELTTHERP
03853             MOVE WS-MAX-AMT-PER-VISIT TO COF-DTL-LINE(WS-CIA)     ELTTHERP
03854         END-IF                                                    ELTTHERP
03855                                                                   ELTTHERP
03856         ADD  +1  TO  WS-CIA                                       ELTTHERP
03857         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO    ELTTHERP
03858                                                WS-SUPP-AMOUNT     ELTTHERP
03859         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHERP
03860            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHERP
03861         ELSE                                                      ELTTHERP
03862            MOVE WS-DTL-SUPP-LONG TO COF-DTL-LINE(WS-CIA)          ELTTHERP
03863         END-IF                                                    ELTTHERP
03864      END-IF.                                                      ELTTHERP
03865                                                                   ELTTHERP
03866      IF YES-CALL-ELUOUTPT                                         ELTTHERP
03867         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
03868         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHERP
03869      END-IF.                                                      ELTTHERP
03870                                                                   ELTTHERP
03871 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTTHERP
03872  4000-CODES-MANUAL-LONG.                                          ELTTHERP
03873                                                                   ELTTHERP
03874      INITIALIZE CMF-RETURN-CODE                                   ELTTHERP
03875                 TCAR-FROM-AREA                                    ELTTHERP
03876                 TCAR-AREA-LENGTH.                                 ELTTHERP
03877                                                                   ELTTHERP
03878      MOVE 1 TO WS-SUB2.                                           ELTTHERP
03879                                                                   ELTTHERP
03880      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTTHERP
03881                       COMMAREA(DFHCOMMAREA)                       ELTTHERP
03882      END-EXEC.                                                    ELTTHERP
03883                                                                   ELTTHERP
03884      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTTHERP
03885      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
03886          ADDRESS OF CMF-DESCR.                                    ELTTHERP
03887                                                                   ELTTHERP
03888      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTTHERP
03889         MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN                        ELTTHERP
03890         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-LINE (WS-SUB2)        ELTTHERP
03891         PERFORM 4010-MOVE-DESCRIPTION-LINES THRU 4010-EXIT        ELTTHERP
03892                VARYING CMF-DESCR-IDX FROM 1 BY 1                  ELTTHERP
03893                UNTIL CMF-DESCR-IDX GREATER THAN                   ELTTHERP
03894                               CMF-NBR-DESCR-LINES                 ELTTHERP
03895         IF  WS-TEMP2-CHARS  NOT =  LOW-VALUES                     ELTTHERP
03896            ADD 1 TO WS-SUB2                                       ELTTHERP
03897            MOVE WS-TEMP-TEXT-AREA2 TO TCAR-FROM-LINE (WS-SUB2)    ELTTHERP
03898            MOVE LOW-VALUES  TO  WS-TEMP-TEXT-AREA2                ELTTHERP
03899         END-IF                                                    ELTTHERP
03900      ELSE                                                         ELTTHERP
03901         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTTHERP
03902         PERFORM 4010-MOVE-DESCRIPTION-LINES THRU 4010-EXIT        ELTTHERP
03903                VARYING CMF-DESCR-IDX FROM 1 BY 1                  ELTTHERP
03904                UNTIL CMF-DESCR-IDX GREATER THAN                   ELTTHERP
03905                               CMF-NBR-DESCR-LINES                 ELTTHERP
03906         IF  WS-TEMP2-CHARS  NOT =  LOW-VALUES                     ELTTHERP
03907            ADD 1 TO WS-SUB2                                       ELTTHERP
03908            MOVE WS-TEMP-TEXT-AREA2 TO TCAR-FROM-LINE (WS-SUB2)    ELTTHERP
03909            MOVE LOW-VALUES  TO  WS-TEMP-TEXT-AREA2                ELTTHERP
03910         END-IF                                                    ELTTHERP
03911      END-IF.                                                      ELTTHERP
03912                                                                   ELTTHERP
03913      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTTHERP
03914                                                                   ELTTHERP
03915      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTTHERP
03916      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTTHERP
03917      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTTHERP
03918      MOVE +79  TO  TCAR-OUTPUT-FIELD-3-LEN.                       ELTTHERP
03919      MOVE +79  TO  TCAR-OUTPUT-FIELD-4-LEN.                       ELTTHERP
03920      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTTHERP
03921                                                                   ELTTHERP
03922      IF WS-MOVE-LINES-TO-CIA                                      ELTTHERP
03923         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTTHERP
03924            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTTHERP
03925                                             WS-TEMP-NOT-USED-CNT  ELTTHERP
03926            PERFORM 4050-CONCATENATE-TO-TEMP-TEXT                  ELTTHERP
03927              VARYING WS-SUB1  FROM  1  BY  1                      ELTTHERP
03928              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTTHERP
03929            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTTHERP
03930            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTTHERP
03931            ADD +1  TO  WS-CIA                                     ELTTHERP
03932         ELSE                                                      ELTTHERP
03933            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTTHERP
03934            ADD +1  TO  WS-CIA.                                    ELTTHERP
03935                                                                   ELTTHERP
03936      IF WS-MOVE-LINES-TO-CIA                                      ELTTHERP
03937         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTTHERP
03938            PERFORM 4020-MOVE-FORMATTED-TEXT THRU 4020-EXIT        ELTTHERP
03939                 VARYING WS-SUB2 FROM 2 BY 1                       ELTTHERP
03940                 UNTIL WS-SUB2 > TCAR-OUTPUT-FIELDS-USED           ELTTHERP
03941         ELSE                                                      ELTTHERP
03942            CONTINUE                                               ELTTHERP
03943      ELSE                                                         ELTTHERP
03944         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTTHERP
03945 /                                                                 ELTTHERP
03946  4010-MOVE-DESCRIPTION-LINES.                                     ELTTHERP
03947      ADD 1 TO WS-SUB2.                                            ELTTHERP
03948      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO TCAR-FROM-LINE        ELTTHERP
03949                                              (WS-SUB2).           ELTTHERP
03950  4010-EXIT.  EXIT.                                                ELTTHERP
03951  4020-MOVE-FORMATTED-TEXT.                                        ELTTHERP
03952      MOVE TCAR-OPF-DATA(WS-SUB2) TO  COF-DTL-LINE(WS-CIA).        ELTTHERP
03953      ADD +1  TO  WS-CIA.                                          ELTTHERP
03954  4020-EXIT.  EXIT.                                                ELTTHERP
03955  4050-CONCATENATE-TO-TEMP-TEXT.                                   ELTTHERP
03956      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTTHERP
03957      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTTHERP
03958                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTTHERP
03959                                                                   ELTTHERP
03960                                                                   ELTTHERP
03961                                                                   ELTTHERP
03962                                                                   ELTTHERP
03963                                                                   ELTTHERP
03964 /**************************************************************** ELTTHERP
03965 *           S A M E   P R O V I D E R   B I L L I N G             ELTTHERP
03966 *     I P   R A D I A T I O N   T H E R A P Y / M E D I C A L     ELTTHERP
03967 *                                                                 ELTTHERP
03968 ***************************************************************** ELTTHERP
03969  4200-SAME-PROV-BILL-THRP-BAS.                                    ELTTHERP
03970                                                                   ELTTHERP
03971      IF GCT-SAME-PROV-IP-RAD-THRPY-MED  NOT =  ZERO               ELTTHERP
03972            MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX                 ELTTHERP
03973            MOVE 'SAME-PROV-IP-RAD-THRPY-MED'  TO                  ELTTHERP
03974                                           CMF-ELEMENT-SYSTEM-NAME ELTTHERP
03975            MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED  TO                ELTTHERP
03976                                                    CMF-CODE-VALUE ELTTHERP
03977            MOVE WS-BASIC  TO  WS-TEMP-TEXT-AREA                   ELTTHERP
03978            MOVE +63  TO  WS-TEMP-NOT-USED-CNT                     ELTTHERP
03979            PERFORM 4000-CODES-MANUAL-LONG                         ELTTHERP
03980      END-IF.                                                      ELTTHERP
03981                                                                   ELTTHERP
03982  4250-SAME-PROV-BILL-THRP-SUP.                                    ELTTHERP
03983      IF GCT-SAME-PROV-IP-RAD-THRPY-MED  NOT =  ZERO               ELTTHERP
03984            MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX                 ELTTHERP
03985            MOVE 'SAME-PROV-IP-RAD-THRPY-MED'  TO                  ELTTHERP
03986                                           CMF-ELEMENT-SYSTEM-NAME ELTTHERP
03987            MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED  TO                ELTTHERP
03988                                                    CMF-CODE-VALUE ELTTHERP
03989            MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                ELTTHERP
03990            MOVE +63  TO  WS-TEMP-NOT-USED-CNT                     ELTTHERP
03991            PERFORM 4000-CODES-MANUAL-LONG                         ELTTHERP
03992      END-IF.                                                      ELTTHERP
03993                                                                   ELTTHERP
03994                                                                   ELTTHERP
03995 /  D A Y S   R E D U C T I O N   B A S I C   &   S E C O N D A R YELTTHERP
03996  4300-DAYS-REDCN-BASIC-SEC.                                       ELTTHERP
03997                                                                   ELTTHERP
03998      SET PLT-INDEX2  TO  1.                                       ELTTHERP
03999      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)      NOT EQUAL 'A'         ELTTHERP
04000            NEXT SENTENCE                                          ELTTHERP
04001                ELSE                                               ELTTHERP
04002      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
04003         PVN-BEN-FMT(PVN-BEN-PROVN-IDX)           =  'A' AND       ELTTHERP
04004         PLA-DAYS-RDCN-RAT-BASIC-APL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHERP
04005                                                         ZERO AND  ELTTHERP
04006         PLA-DAYS-RDCN-RAT-BASIC-BASE(PLT-INDEX1, PLT-INDEX2)      ELTTHERP
04007                                                      NOT =  ZERO  ELTTHERP
04008         MOVE PLA-DAYS-RDCN-RAT-BASIC-APL(PLT-INDEX1, PLT-INDEX2)  ELTTHERP
04009                                             TO  WS-BASIC-DAYS-1ST ELTTHERP
04010         MOVE PLA-DAYS-RDCN-RAT-BASIC-BASE(PLT-INDEX1, PLT-INDEX2) ELTTHERP
04011                                         TO  WS-BASIC-DAYS-FOR-1ST ELTTHERP
04012         MOVE WS-BASIC-DAYS-REDUCT-1ST  TO  COF-DTL-LINE(WS-CIA)   ELTTHERP
04013         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
04014         ADD  +1  TO  WS-CIA.                                      ELTTHERP
04015                                                                   ELTTHERP
04016      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)      NOT EQUAL 'A'         ELTTHERP
04017            NEXT SENTENCE                                          ELTTHERP
04018                ELSE                                               ELTTHERP
04019      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHERP
04020         PVN-BEN-FMT(PVN-BEN-PROVN-IDX)           =  'A' AND       ELTTHERP
04021         PLA-DAYS-RDCN-RAT-SEC-APL(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTTHERP
04022                                                         ZERO AND  ELTTHERP
04023         PLA-DAYS-RDCN-RAT-SEC-BASE(PLT-INDEX1, PLT-INDEX2)        ELTTHERP
04024                                                      NOT =  ZERO  ELTTHERP
04025         MOVE PLA-DAYS-RDCN-RAT-SEC-APL(PLT-INDEX1, PLT-INDEX2)    ELTTHERP
04026                                             TO  WS-BASIC-DAYS-2ND ELTTHERP
04027         MOVE PLA-DAYS-RDCN-RAT-SEC-BASE(PLT-INDEX1, PLT-INDEX2)   ELTTHERP
04028                                         TO  WS-BASIC-DAYS-FOR-2ND ELTTHERP
04029         MOVE WS-BASIC-DAYS-REDUCT-2ND  TO  COF-DTL-LINE(WS-CIA)   ELTTHERP
04030         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
04031         ADD  +1  TO  WS-CIA.                                      ELTTHERP
04032                                                                   ELTTHERP
04033      IF YES-CALL-ELUOUTPT                                         ELTTHERP
04034         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHERP
04035         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTTHERP
04036         MOVE 1  TO  WS-CIA                                        ELTTHERP
04037         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04038             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04039                                         END-EXEC.                 ELTTHERP
04040                                                                   ELTTHERP
04041      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04042         SET PLT-INDEX2  TO  2                                     ELTTHERP
04043      ELSE                                                         ELTTHERP
04044         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04045                                                                   ELTTHERP
04046  4399-EXIT.             EXIT.                                     ELTTHERP
04047                                                                   ELTTHERP
04048 /   S H O C K   T H E R A P Y   I P   I N S T I T U T I O N A L   ELTTHERP
04049 ***************************************************************** ELTTHERP
04050 *   S H O C K   T H E R A P Y   I P   I N S T I T U T I O N A L   ELTTHERP
04051 *                                                                 ELTTHERP
04052 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHERP
04053 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHERP
04054 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHERP
04055 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHERP
04056 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHERP
04057 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHERP
04058 *  MODULE.                                                        ELTTHERP
04059 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHERP
04060 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHERP
04061 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHERP
04062 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHERP
04063 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHERP
04064 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHERP
04065 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHERP
04066 *                                                                 ELTTHERP
04067 ***************************************************************** ELTTHERP
04068  5000-SHOCK-THRP-IP-INST-RTNE.                                    ELTTHERP
04069      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
04070                                                                   ELTTHERP
04071      MOVE WS-HDR-2-SHOCK-IP-INST  TO  COF-HDR-LINE(2).            ELTTHERP
04072                                                                   ELTTHERP
04073      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
04074      PERFORM WITH TEST BEFORE                                     ELTTHERP
04075              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
04076              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
04077         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
04078         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04079         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04080         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
04081      END-PERFORM.                                                 ELTTHERP
04082      MOVE WS-SHOCK-IP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
04083                                                                   ELTTHERP
04084                                                                   ELTTHERP
04085      PERFORM WITH TEST BEFORE                                     ELTTHERP
04086         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
04087         UNTIL   WS-SUB  >     WS-SHOCK-IP-INST-CNT                ELTTHERP
04088           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
04089           MOVE WS-SHOCK-IP-INST-LIST(WS-SUB)                      ELTTHERP
04090                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
04091            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
04092                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
04093      END-PERFORM.                                                 ELTTHERP
04094                                                                   ELTTHERP
04095      MOVE WS-SHOCK-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
04096                                                                   ELTTHERP
04097      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
04098      END-EXEC.                                                    ELTTHERP
04099                                                                   ELTTHERP
04100      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
04101      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04102      END-EXEC.                                                    ELTTHERP
04103                                                                   ELTTHERP
04104      IF PVN-COVG-NONE                                             ELTTHERP
04105         GO TO 5099-EXIT.                                          ELTTHERP
04106                                                                   ELTTHERP
04107      MOVE +1  TO  WS-CIA.                                         ELTTHERP
04108      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
04109            PSP-PROVN-PRICING-METHD,                               ELTTHERP
04110            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
04111            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
04112            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
04113            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
04114            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
04115            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
04116            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
04117            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
04118            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
04119            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
04120            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
04121            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
04122                                                                   ELTTHERP
04123      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
04124                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
04125                     END-EXEC.                                     ELTTHERP
04126                                                                   ELTTHERP
04127      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
04128                                                                   ELTTHERP
04129      PERFORM 5030-FIND-FIRST-NONZERO                              ELTTHERP
04130         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
04131         UNTIL WS-SUB  >  WS-SHOCK-IP-INST-CNT.                    ELTTHERP
04132                                                                   ELTTHERP
04133      GO TO 5099-EXIT.                                             ELTTHERP
04134  5030-FIND-FIRST-NONZERO.                                         ELTTHERP
04135      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04136      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
04137         CONTINUE                                                  ELTTHERP
04138      ELSE                                                         ELTTHERP
04139         PERFORM 5040-BUILD-SCREEN-LINES THRU 5040-EXIT.           ELTTHERP
04140                                                                   ELTTHERP
04141  5040-BUILD-SCREEN-LINES.                                         ELTTHERP
04142                                                                   ELTTHERP
04143      SET PLT-INDEX1  TO                                           ELTTHERP
04144                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
04145      IF WS-NOT-FIRST-TIME                                         ELTTHERP
04146         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
04147         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
04148         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04149             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04150                                         END-EXEC                  ELTTHERP
04151      ELSE                                                         ELTTHERP
04152         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
04153                                                                   ELTTHERP
04154      MOVE 1  TO  WS-CIA.                                          ELTTHERP
04155      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04156         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
04157            SET PLT-INDEX2  TO  2                                  ELTTHERP
04158         ELSE                                                      ELTTHERP
04159            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
04160            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
04161            GO TO 5040-EXIT                                        ELTTHERP
04162      ELSE                                                         ELTTHERP
04163         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04164                                                                   ELTTHERP
04165 **---------------------------------------------------------------+ELTTHERP
04166 **                                                               |ELTTHERP
04167 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
04168      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
04169      ADD  +1  TO  WS-CIA.                                         ELTTHERP
04170      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
04171                                                                   ELTTHERP
04172      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
04173                                                                   ELTTHERP
04174      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
04175         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
04176         UNTIL  PVN-BEN-PROVN-IDX > WS-SHOCK-IP-INST-CNT.          ELTTHERP
04177      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04178                                                                   ELTTHERP
04179      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
04180      MOVE +1  TO  WS-CIA                                          ELTTHERP
04181      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04182                                         END-EXEC.                 ELTTHERP
04183 **                                                               |ELTTHERP
04184 **---------------------------------------------------------------+ELTTHERP
04185 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
04186      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
04187 **                                                               |ELTTHERP
04188 **---------------------------------------------------------------+ELTTHERP
04189                                                                   ELTTHERP
04190 **---------------------------------------------------------------+ELTTHERP
04191 **                                                               |ELTTHERP
04192 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
04193 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
04194 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
04195      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
04196 **                                                               |ELTTHERP
04197 **---------------------------------------------------------------+ELTTHERP
04198                                                                   ELTTHERP
04199 **---------------------------------------------------------------+ELTTHERP
04200 **                                                               |ELTTHERP
04201 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHERP
04202 **                  H O S P I T A L   B I L L                    |ELTTHERP
04203      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHERP
04204      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
04205 **                                                               |ELTTHERP
04206 **---------------------------------------------------------------+ELTTHERP
04207                                                                   ELTTHERP
04208 **---------------------------------------------------------------+ELTTHERP
04209 **                                                               |ELTTHERP
04210 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
04211      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
04212 **                                                               |ELTTHERP
04213 **---------------------------------------------------------------+ELTTHERP
04214                                                                   ELTTHERP
04215 **---------------------------------------------------------------+ELTTHERP
04216 **                                                               |ELTTHERP
04217 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
04218      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
04219         THRU 3250-EXIT.                                           ELTTHERP
04220 **                                                               |ELTTHERP
04221 **---------------------------------------------------------------+ELTTHERP
04222                                                                   ELTTHERP
04223 **---------------------------------------------------------------+ELTTHERP
04224 **                                                               |ELTTHERP
04225 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
04226      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
04227 **                                                               |ELTTHERP
04228 **---------------------------------------------------------------+ELTTHERP
04229                                                                   ELTTHERP
04230 **---------------------------------------------------------------+ELTTHERP
04231 **                                                               |ELTTHERP
04232 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
04233 **                         A N D                                 |ELTTHERP
04234 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
04235      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
04236 **                                                               |ELTTHERP
04237 **---------------------------------------------------------------+ELTTHERP
04238                                                                   ELTTHERP
04239 **---------------------------------------------------------------+ELTTHERP
04240 **                                                               |ELTTHERP
04241 **               P E R F O R M   T A B U L A R                   |ELTTHERP
04242      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
04243 **                                                               |ELTTHERP
04244 **---------------------------------------------------------------+ELTTHERP
04245                                                                   ELTTHERP
04246  5040-EXIT.  EXIT.                                                ELTTHERP
04247                                                                   ELTTHERP
04248  5099-EXIT.            EXIT.                                      ELTTHERP
04249                                                                   ELTTHERP
04250 /   S H O C K   T H E R A P Y   O P   I N S T I T U T I O N A L   ELTTHERP
04251  5200-SHOCK-THRP-OP-INST-RTNE.                                    ELTTHERP
04252      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
04253                                                                   ELTTHERP
04254      MOVE WS-HDR-2-SHOCK-OP-INST  TO  COF-HDR-LINE(2).            ELTTHERP
04255                                                                   ELTTHERP
04256      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
04257      PERFORM WITH TEST BEFORE                                     ELTTHERP
04258              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
04259              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
04260         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
04261         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04262         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04263         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
04264      END-PERFORM.                                                 ELTTHERP
04265      MOVE WS-SHOCK-OP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
04266                                                                   ELTTHERP
04267                                                                   ELTTHERP
04268      PERFORM WITH TEST BEFORE                                     ELTTHERP
04269         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
04270         UNTIL   WS-SUB  >     WS-SHOCK-OP-INST-CNT                ELTTHERP
04271           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
04272           MOVE WS-SHOCK-OP-INST-LIST(WS-SUB)                      ELTTHERP
04273                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
04274            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
04275                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
04276      END-PERFORM.                                                 ELTTHERP
04277                                                                   ELTTHERP
04278      MOVE WS-SHOCK-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
04279                                                                   ELTTHERP
04280      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
04281      END-EXEC.                                                    ELTTHERP
04282                                                                   ELTTHERP
04283      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
04284      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04285      END-EXEC.                                                    ELTTHERP
04286                                                                   ELTTHERP
04287                                                                   ELTTHERP
04288      IF PVN-COVG-NONE                                             ELTTHERP
04289         GO TO 5299-EXIT.                                          ELTTHERP
04290                                                                   ELTTHERP
04291      MOVE +1  TO  WS-CIA.                                         ELTTHERP
04292      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
04293            PSP-PROVN-PRICING-METHD,                               ELTTHERP
04294            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
04295            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
04296            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
04297            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
04298            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
04299            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
04300            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
04301            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
04302            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
04303            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTTHERP
04304            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTTHERP
04305            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTTHERP
04306            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTTHERP
04307            PSA-HOSP-COND-RELATSP-IND,                             ELTTHERP
04308            PSA-HOSP-ADM-RESTRN-IND,                               ELTTHERP
04309            PSA-HSP-ADM-RESTRN-DAYS,                               ELTTHERP
04310            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
04311                                                                   ELTTHERP
04312                                                                   ELTTHERP
04313      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
04314                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
04315                     END-EXEC.                                     ELTTHERP
04316                                                                   ELTTHERP
04317      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
04318                                                                   ELTTHERP
04319      PERFORM 5230-FIND-FIRST-NONZERO                              ELTTHERP
04320         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
04321         UNTIL WS-SUB  >  WS-SHOCK-OP-INST-CNT.                    ELTTHERP
04322                                                                   ELTTHERP
04323      GO TO 5299-EXIT.                                             ELTTHERP
04324  5230-FIND-FIRST-NONZERO.                                         ELTTHERP
04325      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04326      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
04327         CONTINUE                                                  ELTTHERP
04328      ELSE                                                         ELTTHERP
04329         PERFORM 5240-BUILD-SCREEN-LINES THRU 5240-EXIT.           ELTTHERP
04330                                                                   ELTTHERP
04331  5240-BUILD-SCREEN-LINES.                                         ELTTHERP
04332                                                                   ELTTHERP
04333      SET PLT-INDEX1  TO                                           ELTTHERP
04334                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
04335      IF WS-NOT-FIRST-TIME                                         ELTTHERP
04336         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
04337         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
04338         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04339             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04340                                         END-EXEC                  ELTTHERP
04341      ELSE                                                         ELTTHERP
04342         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
04343                                                                   ELTTHERP
04344      MOVE 1  TO  WS-CIA.                                          ELTTHERP
04345      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04346         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
04347            SET PLT-INDEX2  TO  2                                  ELTTHERP
04348         ELSE                                                      ELTTHERP
04349            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
04350            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
04351            GO TO 5240-EXIT                                        ELTTHERP
04352      ELSE                                                         ELTTHERP
04353         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04354                                                                   ELTTHERP
04355 **---------------------------------------------------------------+ELTTHERP
04356 **                                                               |ELTTHERP
04357 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
04358      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
04359      ADD  +1  TO  WS-CIA.                                         ELTTHERP
04360      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
04361                                                                   ELTTHERP
04362      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
04363                                                                   ELTTHERP
04364      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
04365         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
04366         UNTIL  PVN-BEN-PROVN-IDX > WS-SHOCK-OP-INST-CNT.          ELTTHERP
04367      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04368                                                                   ELTTHERP
04369      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
04370      MOVE +1  TO  WS-CIA                                          ELTTHERP
04371      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04372                                         END-EXEC.                 ELTTHERP
04373 **                                                               |ELTTHERP
04374 **---------------------------------------------------------------+ELTTHERP
04375 **                                                               |ELTTHERP
04376 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
04377      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
04378 **                                                               |ELTTHERP
04379 **---------------------------------------------------------------+ELTTHERP
04380                                                                   ELTTHERP
04381 **---------------------------------------------------------------+ELTTHERP
04382 **                                                               |ELTTHERP
04383 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
04384 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
04385 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
04386      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
04387 **                                                               |ELTTHERP
04388 **---------------------------------------------------------------+ELTTHERP
04389                                                                   ELTTHERP
04390 **---------------------------------------------------------------+ELTTHERP
04391 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHERP
04392 **                    H O S P I T A L    B I L L                 |ELTTHERP
04393      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHERP
04394      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
04395 **                                                               |ELTTHERP
04396 **---------------------------------------------------------------+ELTTHERP
04397                                                                   ELTTHERP
04398 **---------------------------------------------------------------+ELTTHERP
04399 **                                                               |ELTTHERP
04400 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
04401      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
04402         THRU 3250-EXIT.                                           ELTTHERP
04403 **                                                               |ELTTHERP
04404 **---------------------------------------------------------------+ELTTHERP
04405                                                                   ELTTHERP
04406 **---------------------------------------------------------------+ELTTHERP
04407 **                                                               |ELTTHERP
04408 **          D A Y S   R E D U C T I O N   R A T I O              |ELTTHERP
04409 **           B A S I C   A N D   S E C O N D A R Y               |ELTTHERP
04410      PERFORM 4300-DAYS-REDCN-BASIC-SEC.                           ELTTHERP
04411 **                                                               |ELTTHERP
04412 **---------------------------------------------------------------+ELTTHERP
04413                                                                   ELTTHERP
04414 **---------------------------------------------------------------+ELTTHERP
04415 **                                                               |ELTTHERP
04416 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
04417      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
04418 **                                                               |ELTTHERP
04419 **---------------------------------------------------------------+ELTTHERP
04420                                                                   ELTTHERP
04421 **---------------------------------------------------------------+ELTTHERP
04422 **                                                               |ELTTHERP
04423 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
04424      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
04425 **                                                               |ELTTHERP
04426 **---------------------------------------------------------------+ELTTHERP
04427                                                                   ELTTHERP
04428 **---------------------------------------------------------------+ELTTHERP
04429 **                                                               |ELTTHERP
04430 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
04431 **                         A N D                                 |ELTTHERP
04432 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
04433      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
04434 **                                                               |ELTTHERP
04435 **---------------------------------------------------------------+ELTTHERP
04436                                                                   ELTTHERP
04437 **---------------------------------------------------------------+ELTTHERP
04438 **                                                               |ELTTHERP
04439 **               P E R F O R M   T A B U L A R                   |ELTTHERP
04440      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
04441 **                                                               |ELTTHERP
04442 **---------------------------------------------------------------+ELTTHERP
04443  5240-EXIT.  EXIT.                                                ELTTHERP
04444                                                                   ELTTHERP
04445  5299-EXIT.            EXIT.                                      ELTTHERP
04446                                                                   ELTTHERP
04447 /   S H O C K   T H E R A P Y   I P   P R O F E S S I O N A L     ELTTHERP
04448  5400-SHOCK-THRP-IP-PROF-RTNE.                                    ELTTHERP
04449      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
04450                                                                   ELTTHERP
04451      MOVE WS-HDR-2-SHOCK-IP-PROF  TO  COF-HDR-LINE(2).            ELTTHERP
04452                                                                   ELTTHERP
04453      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
04454      PERFORM WITH TEST BEFORE                                     ELTTHERP
04455              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
04456              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
04457         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
04458         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04459         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04460         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
04461      END-PERFORM.                                                 ELTTHERP
04462      MOVE WS-SHOCK-IP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
04463                                                                   ELTTHERP
04464                                                                   ELTTHERP
04465      PERFORM WITH TEST BEFORE                                     ELTTHERP
04466         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
04467         UNTIL   WS-SUB  >     WS-SHOCK-IP-PROF-CNT                ELTTHERP
04468           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
04469           MOVE WS-SHOCK-IP-PROF-LIST(WS-SUB)                      ELTTHERP
04470                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
04471            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
04472                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
04473      END-PERFORM.                                                 ELTTHERP
04474                                                                   ELTTHERP
04475      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
04476      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04477                                         END-EXEC.                 ELTTHERP
04478                                                                   ELTTHERP
04479      MOVE WS-SHOCK-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
04480                                                                   ELTTHERP
04481      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
04482                             END-EXEC.                             ELTTHERP
04483                                                                   ELTTHERP
04484      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
04485      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04486                                         END-EXEC.                 ELTTHERP
04487                                                                   ELTTHERP
04488      IF PVN-COVG-NONE                                             ELTTHERP
04489         GO TO 5499-EXIT.                                          ELTTHERP
04490                                                                   ELTTHERP
04491      MOVE +1  TO  WS-CIA.                                         ELTTHERP
04492      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
04493            PSP-PROVN-PRICING-METHD,                               ELTTHERP
04494            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
04495            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
04496            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
04497            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
04498            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
04499            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
04500            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
04501            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
04502            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
04503            PSE-BEN-SCOPE-ID,                                      ELTTHERP
04504            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
04505            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
04506            PSE-MAX-AMT-PER-VISIT.                                 ELTTHERP
04507                                                                   ELTTHERP
04508                                                                   ELTTHERP
04509      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
04510                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
04511                     END-EXEC.                                     ELTTHERP
04512                                                                   ELTTHERP
04513      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
04514                                                                   ELTTHERP
04515      PERFORM 5430-FIND-FIRST-NONZERO                              ELTTHERP
04516         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
04517         UNTIL WS-SUB  >  WS-SHOCK-IP-PROF-CNT.                    ELTTHERP
04518                                                                   ELTTHERP
04519      GO TO 5499-EXIT.                                             ELTTHERP
04520  5430-FIND-FIRST-NONZERO.                                         ELTTHERP
04521      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04522      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
04523         CONTINUE                                                  ELTTHERP
04524      ELSE                                                         ELTTHERP
04525         PERFORM 5440-BUILD-SCREEN-LINES THRU 5440-EXIT.           ELTTHERP
04526                                                                   ELTTHERP
04527  5440-BUILD-SCREEN-LINES.                                         ELTTHERP
04528                                                                   ELTTHERP
04529      SET PLT-INDEX1  TO                                           ELTTHERP
04530                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
04531      IF WS-NOT-FIRST-TIME                                         ELTTHERP
04532         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
04533         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
04534         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04535             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04536                                         END-EXEC                  ELTTHERP
04537      ELSE                                                         ELTTHERP
04538         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
04539                                                                   ELTTHERP
04540      MOVE 1  TO  WS-CIA.                                          ELTTHERP
04541      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04542         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
04543            SET PLT-INDEX2  TO  2                                  ELTTHERP
04544         ELSE                                                      ELTTHERP
04545            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
04546            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
04547            GO TO 5440-EXIT                                        ELTTHERP
04548      ELSE                                                         ELTTHERP
04549         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04550                                                                   ELTTHERP
04551 **---------------------------------------------------------------+ELTTHERP
04552 **                                                               |ELTTHERP
04553 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
04554      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
04555      ADD  +1  TO  WS-CIA.                                         ELTTHERP
04556      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
04557      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
04558         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
04559         UNTIL  PVN-BEN-PROVN-IDX > WS-SHOCK-IP-PROF-CNT.          ELTTHERP
04560      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04561                                                                   ELTTHERP
04562      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
04563      MOVE +1  TO  WS-CIA                                          ELTTHERP
04564      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04565                                         END-EXEC.                 ELTTHERP
04566 **                                                               |ELTTHERP
04567 **---------------------------------------------------------------+ELTTHERP
04568 **                                                               |ELTTHERP
04569 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
04570      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
04571 **                                                               |ELTTHERP
04572 **---------------------------------------------------------------+ELTTHERP
04573                                                                   ELTTHERP
04574 **---------------------------------------------------------------+ELTTHERP
04575 **                                                               |ELTTHERP
04576 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
04577      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
04578 **                                                               |ELTTHERP
04579 **---------------------------------------------------------------+ELTTHERP
04580                                                                   ELTTHERP
04581 **---------------------------------------------------------------+ELTTHERP
04582 **                                                               |ELTTHERP
04583 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
04584 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
04585 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
04586      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
04587 **                                                               |ELTTHERP
04588 **---------------------------------------------------------------+ELTTHERP
04589                                                                   ELTTHERP
04590 **---------------------------------------------------------------+ELTTHERP
04591 **                                                               |ELTTHERP
04592 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
04593      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
04594         THRU 3250-EXIT.                                           ELTTHERP
04595 **                                                               |ELTTHERP
04596 **---------------------------------------------------------------+ELTTHERP
04597                                                                   ELTTHERP
04598 **---------------------------------------------------------------+ELTTHERP
04599 **                                                               |ELTTHERP
04600 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
04601      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
04602 **                                                               |ELTTHERP
04603 **---------------------------------------------------------------+ELTTHERP
04604                                                                   ELTTHERP
04605 **---------------------------------------------------------------+ELTTHERP
04606 **                                                               |ELTTHERP
04607 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
04608      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
04609 **                                                               |ELTTHERP
04610 **---------------------------------------------------------------+ELTTHERP
04611                                                                   ELTTHERP
04612 **---------------------------------------------------------------+ELTTHERP
04613 **                                                               |ELTTHERP
04614 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
04615 **                         A N D                                 |ELTTHERP
04616 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
04617      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
04618 **                                                               |ELTTHERP
04619 **---------------------------------------------------------------+ELTTHERP
04620                                                                   ELTTHERP
04621 **---------------------------------------------------------------+ELTTHERP
04622 **                                                               |ELTTHERP
04623 **               P E R F O R M   T A B U L A R                   |ELTTHERP
04624      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
04625 **                                                               |ELTTHERP
04626 **---------------------------------------------------------------+ELTTHERP
04627                                                                   ELTTHERP
04628  5440-EXIT.  EXIT.                                                ELTTHERP
04629                                                                   ELTTHERP
04630  5499-EXIT.            EXIT.                                      ELTTHERP
04631                                                                   ELTTHERP
04632 /   S H O C K   T H E R A P Y   O P   P R O F E S S I O N A L     ELTTHERP
04633  5600-SHOCK-THRP-OP-PROF-RTNE.                                    ELTTHERP
04634      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
04635                                                                   ELTTHERP
04636      MOVE WS-HDR-2-SHOCK-OP-PROF  TO  COF-HDR-LINE(2).            ELTTHERP
04637                                                                   ELTTHERP
04638      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
04639      PERFORM WITH TEST BEFORE                                     ELTTHERP
04640              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
04641              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
04642         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
04643         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04644         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04645         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
04646      END-PERFORM.                                                 ELTTHERP
04647      MOVE WS-SHOCK-OP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHERP
04648                                                                   ELTTHERP
04649                                                                   ELTTHERP
04650      PERFORM WITH TEST BEFORE                                     ELTTHERP
04651         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
04652         UNTIL   WS-SUB  >     WS-SHOCK-OP-PROF-CNT                ELTTHERP
04653           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
04654           MOVE WS-SHOCK-OP-PROF-LIST(WS-SUB)                      ELTTHERP
04655                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
04656            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
04657                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
04658      END-PERFORM.                                                 ELTTHERP
04659                                                                   ELTTHERP
04660      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
04661      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04662                                         END-EXEC.                 ELTTHERP
04663                                                                   ELTTHERP
04664      MOVE WS-SHOCK-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHERP
04665                                                                   ELTTHERP
04666      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
04667           END-EXEC.                                               ELTTHERP
04668                                                                   ELTTHERP
04669      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
04670      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04671                                         END-EXEC.                 ELTTHERP
04672                                                                   ELTTHERP
04673      IF PVN-COVG-NONE                                             ELTTHERP
04674         GO TO 5699-EXIT.                                          ELTTHERP
04675                                                                   ELTTHERP
04676      MOVE +1  TO  WS-CIA.                                         ELTTHERP
04677      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
04678            PSP-PROVN-PRICING-METHD,                               ELTTHERP
04679            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
04680            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
04681            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
04682            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
04683            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
04684            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
04685            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
04686            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
04687            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
04688            PSE-BEN-SCOPE-ID,                                      ELTTHERP
04689            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
04690            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
04691            PSE-MAX-AMT-PER-VISIT,                                 ELTTHERP
04692            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHERP
04693            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHERP
04694                                                                   ELTTHERP
04695                                                                   ELTTHERP
04696      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
04697                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
04698                     END-EXEC.                                     ELTTHERP
04699                                                                   ELTTHERP
04700      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
04701                                                                   ELTTHERP
04702      PERFORM 5630-FIND-FIRST-NONZERO                              ELTTHERP
04703         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
04704         UNTIL WS-SUB  >  WS-SHOCK-OP-PROF-CNT.                    ELTTHERP
04705                                                                   ELTTHERP
04706      GO TO 5699-EXIT.                                             ELTTHERP
04707  5630-FIND-FIRST-NONZERO.                                         ELTTHERP
04708      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04709      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
04710         CONTINUE                                                  ELTTHERP
04711      ELSE                                                         ELTTHERP
04712         PERFORM 5640-BUILD-SCREEN-LINES THRU 5640-EXIT.           ELTTHERP
04713                                                                   ELTTHERP
04714  5640-BUILD-SCREEN-LINES.                                         ELTTHERP
04715                                                                   ELTTHERP
04716      SET PLT-INDEX1  TO                                           ELTTHERP
04717                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
04718      IF WS-NOT-FIRST-TIME                                         ELTTHERP
04719         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
04720         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
04721         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04722             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04723                                         END-EXEC                  ELTTHERP
04724      ELSE                                                         ELTTHERP
04725         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
04726                                                                   ELTTHERP
04727      MOVE 1  TO  WS-CIA.                                          ELTTHERP
04728      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04729         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
04730            SET PLT-INDEX2  TO  2                                  ELTTHERP
04731         ELSE                                                      ELTTHERP
04732            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
04733            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
04734            GO TO 5640-EXIT                                        ELTTHERP
04735      ELSE                                                         ELTTHERP
04736         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04737                                                                   ELTTHERP
04738 **---------------------------------------------------------------+ELTTHERP
04739 **                                                               |ELTTHERP
04740 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
04741      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
04742      ADD  +1  TO  WS-CIA.                                         ELTTHERP
04743      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
04744      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
04745         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
04746         UNTIL  PVN-BEN-PROVN-IDX > WS-SHOCK-OP-PROF-CNT.          ELTTHERP
04747      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04748                                                                   ELTTHERP
04749      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
04750      MOVE +1  TO  WS-CIA                                          ELTTHERP
04751      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04752                                         END-EXEC.                 ELTTHERP
04753 **                                                               |ELTTHERP
04754 **---------------------------------------------------------------+ELTTHERP
04755 **                                                               |ELTTHERP
04756 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
04757      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
04758 **                                                               |ELTTHERP
04759 **---------------------------------------------------------------+ELTTHERP
04760                                                                   ELTTHERP
04761 **---------------------------------------------------------------+ELTTHERP
04762 **                                                               |ELTTHERP
04763 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
04764      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
04765 **                                                               |ELTTHERP
04766 **---------------------------------------------------------------+ELTTHERP
04767                                                                   ELTTHERP
04768 **---------------------------------------------------------------+ELTTHERP
04769 **                                                               |ELTTHERP
04770 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
04771 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
04772 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
04773      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
04774 **                                                               |ELTTHERP
04775 **---------------------------------------------------------------+ELTTHERP
04776                                                                   ELTTHERP
04777 **---------------------------------------------------------------+ELTTHERP
04778 **                                                               |ELTTHERP
04779 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
04780      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
04781 **                                                               |ELTTHERP
04782 **---------------------------------------------------------------+ELTTHERP
04783                                                                   ELTTHERP
04784 **---------------------------------------------------------------+ELTTHERP
04785 **                                                               |ELTTHERP
04786 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
04787      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
04788 **                                                               |ELTTHERP
04789 **---------------------------------------------------------------+ELTTHERP
04790                                                                   ELTTHERP
04791 **---------------------------------------------------------------+ELTTHERP
04792 **                                                               |ELTTHERP
04793 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
04794      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
04795         THRU 3250-EXIT.                                           ELTTHERP
04796 **                                                               |ELTTHERP
04797 **---------------------------------------------------------------+ELTTHERP
04798                                                                   ELTTHERP
04799 **---------------------------------------------------------------+ELTTHERP
04800 **                                                               |ELTTHERP
04801 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
04802      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
04803 **                                                               |ELTTHERP
04804 **---------------------------------------------------------------+ELTTHERP
04805                                                                   ELTTHERP
04806 **---------------------------------------------------------------+ELTTHERP
04807 **                                                               |ELTTHERP
04808 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
04809 **                         A N D                                 |ELTTHERP
04810 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
04811      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
04812 **                                                               |ELTTHERP
04813 **---------------------------------------------------------------+ELTTHERP
04814                                                                   ELTTHERP
04815 **---------------------------------------------------------------+ELTTHERP
04816 **                                                               |ELTTHERP
04817 **               P E R F O R M   T A B U L A R                   |ELTTHERP
04818      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
04819 **                                                               |ELTTHERP
04820 **---------------------------------------------------------------+ELTTHERP
04821                                                                   ELTTHERP
04822  5640-EXIT.  EXIT.                                                ELTTHERP
04823                                                                   ELTTHERP
04824  5699-EXIT.            EXIT.                                      ELTTHERP
04825                                                                   ELTTHERP
04826 /    M I S C .   T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHERP
04827 ***************************************************************** ELTTHERP
04828 *    M I S C .   T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHERP
04829 *                                                                 ELTTHERP
04830 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHERP
04831 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHERP
04832 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHERP
04833 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHERP
04834 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHERP
04835 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHERP
04836 *  MODULE.                                                        ELTTHERP
04837 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHERP
04838 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHERP
04839 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHERP
04840 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHERP
04841 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHERP
04842 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHERP
04843 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHERP
04844 *                                                                 ELTTHERP
04845 ***************************************************************** ELTTHERP
04846  6000-MISC-THERP-IP-INST-RTNE.                                    ELTTHERP
04847      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
04848                                                                   ELTTHERP
04849      MOVE WS-HDR-2-MISC-IP-INST  TO  COF-HDR-LINE(2).             ELTTHERP
04850                                                                   ELTTHERP
04851      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
04852      PERFORM WITH TEST BEFORE                                     ELTTHERP
04853              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
04854              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
04855         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
04856         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04857         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
04858         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
04859      END-PERFORM.                                                 ELTTHERP
04860      MOVE WS-MISC-IP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
04861                                                                   ELTTHERP
04862                                                                   ELTTHERP
04863      PERFORM WITH TEST BEFORE                                     ELTTHERP
04864         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
04865         UNTIL   WS-SUB  >     WS-MISC-IP-INST-CNT                 ELTTHERP
04866           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
04867           MOVE WS-MISC-IP-INST-LIST (WS-SUB)                      ELTTHERP
04868                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
04869            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
04870                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
04871      END-PERFORM.                                                 ELTTHERP
04872                                                                   ELTTHERP
04873                                                                   ELTTHERP
04874      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
04875      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04876                                         END-EXEC.                 ELTTHERP
04877                                                                   ELTTHERP
04878      MOVE WS-MISC-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHERP
04879                                                                   ELTTHERP
04880      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
04881                               END-EXEC.                           ELTTHERP
04882                                                                   ELTTHERP
04883      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
04884      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04885                                         END-EXEC.                 ELTTHERP
04886                                                                   ELTTHERP
04887      IF PVN-COVG-NONE                                             ELTTHERP
04888         GO TO 6099-EXIT.                                          ELTTHERP
04889                                                                   ELTTHERP
04890      MOVE +1  TO  WS-CIA.                                         ELTTHERP
04891      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
04892            PSP-PROVN-PRICING-METHD,                               ELTTHERP
04893            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
04894            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
04895            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
04896            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
04897            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
04898            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
04899            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
04900            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
04901            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
04902            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
04903            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
04904            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
04905                                                                   ELTTHERP
04906                                                                   ELTTHERP
04907      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
04908                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
04909                     END-EXEC.                                     ELTTHERP
04910                                                                   ELTTHERP
04911      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
04912                                                                   ELTTHERP
04913      PERFORM 6030-FIND-FIRST-NONZERO                              ELTTHERP
04914         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
04915         UNTIL WS-SUB  >  WS-MISC-IP-INST-CNT.                     ELTTHERP
04916                                                                   ELTTHERP
04917      GO TO 6099-EXIT.                                             ELTTHERP
04918  6030-FIND-FIRST-NONZERO.                                         ELTTHERP
04919      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04920      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
04921         CONTINUE                                                  ELTTHERP
04922      ELSE                                                         ELTTHERP
04923         PERFORM 6040-BUILD-SCREEN-LINES THRU 6040-EXIT.           ELTTHERP
04924                                                                   ELTTHERP
04925  6040-BUILD-SCREEN-LINES.                                         ELTTHERP
04926                                                                   ELTTHERP
04927      SET PLT-INDEX1  TO                                           ELTTHERP
04928                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
04929      IF WS-NOT-FIRST-TIME                                         ELTTHERP
04930         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
04931         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
04932         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
04933             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
04934                                         END-EXEC                  ELTTHERP
04935      ELSE                                                         ELTTHERP
04936         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
04937                                                                   ELTTHERP
04938      MOVE 1  TO  WS-CIA.                                          ELTTHERP
04939      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
04940         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
04941            SET PLT-INDEX2  TO  2                                  ELTTHERP
04942         ELSE                                                      ELTTHERP
04943            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
04944            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
04945            GO TO 6040-EXIT                                        ELTTHERP
04946      ELSE                                                         ELTTHERP
04947         SET PLT-INDEX2  TO  1.                                    ELTTHERP
04948                                                                   ELTTHERP
04949 **---------------------------------------------------------------+ELTTHERP
04950 **                                                               |ELTTHERP
04951 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
04952      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
04953      ADD  +1  TO  WS-CIA.                                         ELTTHERP
04954      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
04955                                                                   ELTTHERP
04956      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
04957                                                                   ELTTHERP
04958      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
04959         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
04960         UNTIL  PVN-BEN-PROVN-IDX > WS-MISC-IP-INST-CNT.           ELTTHERP
04961      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
04962                                                                   ELTTHERP
04963      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
04964      MOVE +1  TO  WS-CIA                                          ELTTHERP
04965      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
04966                                         END-EXEC.                 ELTTHERP
04967 **                                                               |ELTTHERP
04968 **---------------------------------------------------------------+ELTTHERP
04969 **                                                               |ELTTHERP
04970 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
04971      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
04972 **                                                               |ELTTHERP
04973 **---------------------------------------------------------------+ELTTHERP
04974                                                                   ELTTHERP
04975 **---------------------------------------------------------------+ELTTHERP
04976 **                                                               |ELTTHERP
04977 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
04978 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
04979 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
04980      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
04981 **                                                               |ELTTHERP
04982 **---------------------------------------------------------------+ELTTHERP
04983                                                                   ELTTHERP
04984 **---------------------------------------------------------------+ELTTHERP
04985 **                                                               |ELTTHERP
04986 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHERP
04987 **                  H O S P I T A L   B I L L                    |ELTTHERP
04988      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHERP
04989      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
04990 **                                                               |ELTTHERP
04991 **---------------------------------------------------------------+ELTTHERP
04992                                                                   ELTTHERP
04993 **---------------------------------------------------------------+ELTTHERP
04994 **                                                               |ELTTHERP
04995 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
04996      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
04997 **                                                               |ELTTHERP
04998 **---------------------------------------------------------------+ELTTHERP
04999                                                                   ELTTHERP
05000 **---------------------------------------------------------------+ELTTHERP
05001 **                                                               |ELTTHERP
05002 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
05003      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
05004         THRU 3250-EXIT.                                           ELTTHERP
05005 **                                                               |ELTTHERP
05006 **---------------------------------------------------------------+ELTTHERP
05007                                                                   ELTTHERP
05008 **---------------------------------------------------------------+ELTTHERP
05009 **                                                               |ELTTHERP
05010 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
05011      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
05012 **                                                               |ELTTHERP
05013 **---------------------------------------------------------------+ELTTHERP
05014                                                                   ELTTHERP
05015 **---------------------------------------------------------------+ELTTHERP
05016 **                                                               |ELTTHERP
05017 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
05018 **                         A N D                                 |ELTTHERP
05019 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
05020      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
05021 **                                                               |ELTTHERP
05022 **---------------------------------------------------------------+ELTTHERP
05023                                                                   ELTTHERP
05024 **---------------------------------------------------------------+ELTTHERP
05025 **                                                               |ELTTHERP
05026 **               P E R F O R M   T A B U L A R                   |ELTTHERP
05027      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
05028 **                                                               |ELTTHERP
05029 **---------------------------------------------------------------+ELTTHERP
05030                                                                   ELTTHERP
05031  6040-EXIT.  EXIT.                                                ELTTHERP
05032  6099-EXIT.            EXIT.                                      ELTTHERP
05033                                                                   ELTTHERP
05034 /    M I S C .   T H E R A P Y   O P   I N S T I T U T I O N A L  ELTTHERP
05035  6200-MISC-THERP-OP-INST-RTNE.                                    ELTTHERP
05036      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
05037                                                                   ELTTHERP
05038      MOVE WS-HDR-2-MISC-OP-INST  TO  COF-HDR-LINE(2).             ELTTHERP
05039                                                                   ELTTHERP
05040      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
05041      PERFORM WITH TEST BEFORE                                     ELTTHERP
05042              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
05043              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
05044         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
05045         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05046         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05047         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
05048      END-PERFORM.                                                 ELTTHERP
05049      MOVE WS-MISC-OP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
05050                                                                   ELTTHERP
05051                                                                   ELTTHERP
05052      PERFORM WITH TEST BEFORE                                     ELTTHERP
05053         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
05054         UNTIL   WS-SUB  >     WS-MISC-OP-INST-CNT                 ELTTHERP
05055           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
05056           MOVE WS-MISC-OP-INST-LIST (WS-SUB)                      ELTTHERP
05057                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
05058            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
05059                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
05060      END-PERFORM.                                                 ELTTHERP
05061                                                                   ELTTHERP
05062                                                                   ELTTHERP
05063      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
05064      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05065                                         END-EXEC.                 ELTTHERP
05066                                                                   ELTTHERP
05067      MOVE WS-MISC-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHERP
05068                                                                   ELTTHERP
05069      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
05070                                         END-EXEC.                 ELTTHERP
05071                                                                   ELTTHERP
05072      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
05073      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05074                                         END-EXEC.                 ELTTHERP
05075                                                                   ELTTHERP
05076      IF PVN-COVG-NONE                                             ELTTHERP
05077         GO TO 6299-EXIT.                                          ELTTHERP
05078                                                                   ELTTHERP
05079      MOVE +1  TO  WS-CIA.                                         ELTTHERP
05080      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
05081            PSP-PROVN-PRICING-METHD,                               ELTTHERP
05082            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
05083            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
05084            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
05085            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
05086            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
05087            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
05088            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
05089            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
05090            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
05091            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHERP
05092            PSB-HOSP-COND-RELATSP-IND,                             ELTTHERP
05093            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHERP
05094            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHERP
05095            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHERP
05096                                                                   ELTTHERP
05097                                                                   ELTTHERP
05098      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
05099                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
05100                     END-EXEC.                                     ELTTHERP
05101                                                                   ELTTHERP
05102      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
05103                                                                   ELTTHERP
05104      PERFORM 6230-FIND-FIRST-NONZERO                              ELTTHERP
05105         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
05106         UNTIL WS-SUB  >  WS-MISC-OP-INST-CNT.                     ELTTHERP
05107                                                                   ELTTHERP
05108      GO TO 6299-EXIT.                                             ELTTHERP
05109  6230-FIND-FIRST-NONZERO.                                         ELTTHERP
05110      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05111      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
05112         CONTINUE                                                  ELTTHERP
05113      ELSE                                                         ELTTHERP
05114         PERFORM 6240-BUILD-SCREEN-LINES THRU 6240-EXIT.           ELTTHERP
05115                                                                   ELTTHERP
05116  6240-BUILD-SCREEN-LINES.                                         ELTTHERP
05117                                                                   ELTTHERP
05118      SET PLT-INDEX1  TO                                           ELTTHERP
05119                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
05120      IF WS-NOT-FIRST-TIME                                         ELTTHERP
05121         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
05122         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
05123         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
05124             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
05125                                         END-EXEC                  ELTTHERP
05126      ELSE                                                         ELTTHERP
05127         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
05128                                                                   ELTTHERP
05129      MOVE 1  TO  WS-CIA.                                          ELTTHERP
05130      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
05131         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
05132            SET PLT-INDEX2  TO  2                                  ELTTHERP
05133         ELSE                                                      ELTTHERP
05134            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
05135            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
05136            GO TO 6240-EXIT                                        ELTTHERP
05137      ELSE                                                         ELTTHERP
05138         SET PLT-INDEX2  TO  1.                                    ELTTHERP
05139                                                                   ELTTHERP
05140 **---------------------------------------------------------------+ELTTHERP
05141 **                                                               |ELTTHERP
05142 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
05143      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
05144      ADD  +1  TO  WS-CIA.                                         ELTTHERP
05145      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
05146                                                                   ELTTHERP
05147      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHERP
05148      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
05149         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
05150         UNTIL  PVN-BEN-PROVN-IDX > WS-MISC-OP-INST-CNT.           ELTTHERP
05151      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05152                                                                   ELTTHERP
05153      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
05154      MOVE +1  TO  WS-CIA                                          ELTTHERP
05155      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05156                                         END-EXEC.                 ELTTHERP
05157 **---------------------------------------------------------------+ELTTHERP
05158 **                                                               |ELTTHERP
05159 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
05160      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
05161 **                                                               |ELTTHERP
05162 **---------------------------------------------------------------+ELTTHERP
05163                                                                   ELTTHERP
05164 **---------------------------------------------------------------+ELTTHERP
05165 **                                                               |ELTTHERP
05166 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
05167 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
05168 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
05169      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
05170 **                                                               |ELTTHERP
05171 **---------------------------------------------------------------+ELTTHERP
05172                                                                   ELTTHERP
05173 **---------------------------------------------------------------+ELTTHERP
05174 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHERP
05175 **                    H O S P I T A L    B I L L                 |ELTTHERP
05176      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHERP
05177      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHERP
05178 **                                                               |ELTTHERP
05179 **---------------------------------------------------------------+ELTTHERP
05180                                                                   ELTTHERP
05181 **---------------------------------------------------------------+ELTTHERP
05182 **                                                               |ELTTHERP
05183 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHERP
05184      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHERP
05185 **                                                               |ELTTHERP
05186 **---------------------------------------------------------------+ELTTHERP
05187                                                                   ELTTHERP
05188 **---------------------------------------------------------------+ELTTHERP
05189 **                                                               |ELTTHERP
05190 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
05191      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
05192         THRU 3250-EXIT.                                           ELTTHERP
05193 **                                                               |ELTTHERP
05194 **---------------------------------------------------------------+ELTTHERP
05195                                                                   ELTTHERP
05196 **---------------------------------------------------------------+ELTTHERP
05197 **                                                               |ELTTHERP
05198 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
05199      IF PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)   =  'CRPO B'        ELTTHERP
05200          PERFORM 3501-PRIOR-ADMIS-REQ                             ELTTHERP
05201      ELSE                                                         ELTTHERP
05202          PERFORM 3500-HOSP-ADM-RESTRN.                            ELTTHERP
05203 **                                                               |ELTTHERP
05204 **---------------------------------------------------------------+ELTTHERP
05205                                                                   ELTTHERP
05206 **---------------------------------------------------------------+ELTTHERP
05207 **                                                               |ELTTHERP
05208 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHERP
05209      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHERP
05210 **                                                               |ELTTHERP
05211 **---------------------------------------------------------------+ELTTHERP
05212                                                                   ELTTHERP
05213 **---------------------------------------------------------------+ELTTHERP
05214 **                                                               |ELTTHERP
05215 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
05216 **                         A N D                                 |ELTTHERP
05217 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
05218      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
05219 **                                                               |ELTTHERP
05220 **---------------------------------------------------------------+ELTTHERP
05221                                                                   ELTTHERP
05222 **---------------------------------------------------------------+ELTTHERP
05223 **                                                               |ELTTHERP
05224 **               P E R F O R M   T A B U L A R                   |ELTTHERP
05225      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
05226 **                                                               |ELTTHERP
05227 **---------------------------------------------------------------+ELTTHERP
05228                                                                   ELTTHERP
05229  6240-EXIT.  EXIT.                                                ELTTHERP
05230                                                                   ELTTHERP
05231  6299-EXIT.            EXIT.                                      ELTTHERP
05232                                                                   ELTTHERP
05233 /    M I S C .   T H E R A P Y   I P   P R O F E S S I O N A L    ELTTHERP
05234  6400-MISC-THERP-IP-PROF-RTNE.                                    ELTTHERP
05235      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
05236                                                                   ELTTHERP
05237      MOVE WS-HDR-2-MISC-IP-PROF  TO  COF-HDR-LINE(2).             ELTTHERP
05238                                                                   ELTTHERP
05239      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
05240      PERFORM WITH TEST BEFORE                                     ELTTHERP
05241              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
05242              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
05243         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
05244         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05245         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05246         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
05247      END-PERFORM.                                                 ELTTHERP
05248      MOVE WS-MISC-IP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
05249                                                                   ELTTHERP
05250                                                                   ELTTHERP
05251      PERFORM WITH TEST BEFORE                                     ELTTHERP
05252         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
05253         UNTIL   WS-SUB  >     WS-MISC-IP-PROF-CNT                 ELTTHERP
05254           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
05255           MOVE WS-MISC-IP-PROF-LIST (WS-SUB)                      ELTTHERP
05256                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
05257            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
05258                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
05259      END-PERFORM.                                                 ELTTHERP
05260                                                                   ELTTHERP
05261                                                                   ELTTHERP
05262      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
05263      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05264                                         END-EXEC.                 ELTTHERP
05265                                                                   ELTTHERP
05266      MOVE WS-MISC-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHERP
05267                                                                   ELTTHERP
05268      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
05269                                         END-EXEC.                 ELTTHERP
05270                                                                   ELTTHERP
05271      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
05272      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05273                                         END-EXEC.                 ELTTHERP
05274                                                                   ELTTHERP
05275      IF PVN-COVG-NONE                                             ELTTHERP
05276         GO TO 6499-EXIT.                                          ELTTHERP
05277                                                                   ELTTHERP
05278      MOVE +1  TO  WS-CIA.                                         ELTTHERP
05279      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
05280            PSP-PROVN-PRICING-METHD,                               ELTTHERP
05281            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
05282            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
05283            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
05284            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
05285            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
05286            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
05287            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
05288            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
05289            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
05290            PSE-BEN-SCOPE-ID,                                      ELTTHERP
05291            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
05292            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
05293            PSE-MAX-AMT-PER-VISIT.                                 ELTTHERP
05294                                                                   ELTTHERP
05295                                                                   ELTTHERP
05296      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
05297                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
05298                     END-EXEC.                                     ELTTHERP
05299                                                                   ELTTHERP
05300      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
05301                                                                   ELTTHERP
05302      PERFORM 6430-FIND-FIRST-NONZERO                              ELTTHERP
05303         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
05304         UNTIL WS-SUB  >  WS-MISC-IP-PROF-CNT.                     ELTTHERP
05305                                                                   ELTTHERP
05306      GO TO 6499-EXIT.                                             ELTTHERP
05307  6430-FIND-FIRST-NONZERO.                                         ELTTHERP
05308      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05309      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
05310         CONTINUE                                                  ELTTHERP
05311      ELSE                                                         ELTTHERP
05312         PERFORM 6440-BUILD-SCREEN-LINES THRU 6440-EXIT.           ELTTHERP
05313                                                                   ELTTHERP
05314  6440-BUILD-SCREEN-LINES.                                         ELTTHERP
05315                                                                   ELTTHERP
05316      SET PLT-INDEX1  TO                                           ELTTHERP
05317                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
05318      IF WS-NOT-FIRST-TIME                                         ELTTHERP
05319         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
05320         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
05321         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
05322             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
05323                                         END-EXEC                  ELTTHERP
05324      ELSE                                                         ELTTHERP
05325         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
05326                                                                   ELTTHERP
05327      MOVE 1  TO  WS-CIA.                                          ELTTHERP
05328      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
05329         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
05330            SET PLT-INDEX2  TO  2                                  ELTTHERP
05331         ELSE                                                      ELTTHERP
05332            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
05333            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
05334            GO TO 6440-EXIT                                        ELTTHERP
05335      ELSE                                                         ELTTHERP
05336         SET PLT-INDEX2  TO  1.                                    ELTTHERP
05337                                                                   ELTTHERP
05338 **---------------------------------------------------------------+ELTTHERP
05339 **                                                               |ELTTHERP
05340 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
05341      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
05342      ADD  +1  TO  WS-CIA.                                         ELTTHERP
05343      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
05344      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
05345         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
05346         UNTIL  PVN-BEN-PROVN-IDX > WS-MISC-IP-PROF-CNT.           ELTTHERP
05347      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05348                                                                   ELTTHERP
05349      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
05350      MOVE +1  TO  WS-CIA                                          ELTTHERP
05351      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05352                                         END-EXEC.                 ELTTHERP
05353 **                                                               |ELTTHERP
05354 **---------------------------------------------------------------+ELTTHERP
05355 **                                                               |ELTTHERP
05356 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
05357      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
05358 **                                                               |ELTTHERP
05359 **---------------------------------------------------------------+ELTTHERP
05360                                                                   ELTTHERP
05361 **---------------------------------------------------------------+ELTTHERP
05362 **                                                               |ELTTHERP
05363 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
05364      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
05365 **                                                               |ELTTHERP
05366 **---------------------------------------------------------------+ELTTHERP
05367                                                                   ELTTHERP
05368 **---------------------------------------------------------------+ELTTHERP
05369 **                                                               |ELTTHERP
05370 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
05371 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
05372 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
05373      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
05374 **                                                               |ELTTHERP
05375 **---------------------------------------------------------------+ELTTHERP
05376                                                                   ELTTHERP
05377 **---------------------------------------------------------------+ELTTHERP
05378 **                                                               |ELTTHERP
05379 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
05380      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
05381         THRU 3250-EXIT.                                           ELTTHERP
05382 **                                                               |ELTTHERP
05383 **---------------------------------------------------------------+ELTTHERP
05384                                                                   ELTTHERP
05385 **---------------------------------------------------------------+ELTTHERP
05386 **                                                               |ELTTHERP
05387 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
05388      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
05389 **                                                               |ELTTHERP
05390 **---------------------------------------------------------------+ELTTHERP
05391                                                                   ELTTHERP
05392 **---------------------------------------------------------------+ELTTHERP
05393 **                                                               |ELTTHERP
05394 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
05395      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
05396 **                                                               |ELTTHERP
05397 **---------------------------------------------------------------+ELTTHERP
05398                                                                   ELTTHERP
05399 **---------------------------------------------------------------+ELTTHERP
05400 **                                                               |ELTTHERP
05401 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
05402 **                         A N D                                 |ELTTHERP
05403 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
05404      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
05405 **                                                               |ELTTHERP
05406 **---------------------------------------------------------------+ELTTHERP
05407                                                                   ELTTHERP
05408 **---------------------------------------------------------------+ELTTHERP
05409 **                                                               |ELTTHERP
05410 **               P E R F O R M   T A B U L A R                   |ELTTHERP
05411      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
05412 **                                                               |ELTTHERP
05413 **---------------------------------------------------------------+ELTTHERP
05414  6440-EXIT.  EXIT.                                                ELTTHERP
05415                                                                   ELTTHERP
05416                                                                   ELTTHERP
05417  6499-EXIT.            EXIT.                                      ELTTHERP
05418                                                                   ELTTHERP
05419 /    M I S C .   T H E R A P Y   O P   P R O F E S S I O N A L    ELTTHERP
05420  6600-MISC-THERP-OP-PROF-RTNE.                                    ELTTHERP
05421                                                                   ELTTHERP
05422      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHERP
05423                                                                   ELTTHERP
05424      MOVE WS-HDR-2-MISC-OP-PROF  TO  COF-HDR-LINE(2).             ELTTHERP
05425                                                                   ELTTHERP
05426      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHERP
05427      PERFORM WITH TEST BEFORE                                     ELTTHERP
05428              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHERP
05429              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHERP
05430         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHERP
05431         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05432         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHERP
05433         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHERP
05434      END-PERFORM.                                                 ELTTHERP
05435      MOVE WS-MISC-OP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHERP
05436                                                                   ELTTHERP
05437                                                                   ELTTHERP
05438      PERFORM WITH TEST BEFORE                                     ELTTHERP
05439         VARYING WS-SUB FROM +1 BY +1                              ELTTHERP
05440         UNTIL   WS-SUB  >     WS-MISC-OP-PROF-CNT                 ELTTHERP
05441           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHERP
05442           MOVE WS-MISC-OP-PROF-LIST (WS-SUB)                      ELTTHERP
05443                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHERP
05444            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHERP
05445                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHERP
05446      END-PERFORM.                                                 ELTTHERP
05447                                                                   ELTTHERP
05448      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHERP
05449      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05450                                         END-EXEC.                 ELTTHERP
05451                                                                   ELTTHERP
05452      MOVE WS-MISC-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHERP
05453                                                                   ELTTHERP
05454      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHERP
05455                                         END-EXEC.                 ELTTHERP
05456                                                                   ELTTHERP
05457      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHERP
05458      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05459                                         END-EXEC.                 ELTTHERP
05460                                                                   ELTTHERP
05461      IF PVN-COVG-NONE                                             ELTTHERP
05462         GO TO 6699-EXIT.                                          ELTTHERP
05463                                                                   ELTTHERP
05464      MOVE +1  TO  WS-CIA.                                         ELTTHERP
05465      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHERP
05466            PSP-PROVN-PRICING-METHD,                               ELTTHERP
05467            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHERP
05468            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHERP
05469            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHERP
05470            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHERP
05471            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHERP
05472            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHERP
05473            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHERP
05474            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHERP
05475            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHERP
05476            PSE-BEN-SCOPE-ID,                                      ELTTHERP
05477            PSE-BEN-MAX-VISITS-IND,                                ELTTHERP
05478            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHERP
05479            PSE-MAX-AMT-PER-VISIT,                                 ELTTHERP
05480            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHERP
05481            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHERP
05482                                                                   ELTTHERP
05483                                                                   ELTTHERP
05484      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHERP
05485                     COMMAREA (DFHCOMMAREA)                        ELTTHERP
05486                     END-EXEC.                                     ELTTHERP
05487                                                                   ELTTHERP
05488      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHERP
05489                                                                   ELTTHERP
05490      PERFORM 6630-FIND-FIRST-NONZERO                              ELTTHERP
05491         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHERP
05492         UNTIL WS-SUB  >  WS-MISC-OP-PROF-CNT.                     ELTTHERP
05493                                                                   ELTTHERP
05494      GO TO 6699-EXIT.                                             ELTTHERP
05495  6630-FIND-FIRST-NONZERO.                                         ELTTHERP
05496      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05497      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHERP
05498         CONTINUE                                                  ELTTHERP
05499      ELSE                                                         ELTTHERP
05500         PERFORM 6640-BUILD-SCREEN-LINES THRU 6640-EXIT.           ELTTHERP
05501                                                                   ELTTHERP
05502  6640-BUILD-SCREEN-LINES.                                         ELTTHERP
05503                                                                   ELTTHERP
05504      SET PLT-INDEX1  TO                                           ELTTHERP
05505                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHERP
05506      IF WS-NOT-FIRST-TIME                                         ELTTHERP
05507         MOVE 'P'  TO  COF-FUNCTION                                ELTTHERP
05508         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHERP
05509         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHERP
05510             COMMAREA(DFHCOMMAREA)                                 ELTTHERP
05511                                         END-EXEC                  ELTTHERP
05512      ELSE                                                         ELTTHERP
05513         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHERP
05514                                                                   ELTTHERP
05515      MOVE 1  TO  WS-CIA.                                          ELTTHERP
05516      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHERP
05517         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHERP
05518            SET PLT-INDEX2  TO  2                                  ELTTHERP
05519         ELSE                                                      ELTTHERP
05520            MOVE TABLE-MAX TO WS-SUB                               ELTTHERP
05521            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHERP
05522            GO TO 6640-EXIT                                        ELTTHERP
05523      ELSE                                                         ELTTHERP
05524         SET PLT-INDEX2  TO  1.                                    ELTTHERP
05525                                                                   ELTTHERP
05526 **---------------------------------------------------------------+ELTTHERP
05527 **                                                               |ELTTHERP
05528 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHERP
05529      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHERP
05530      ADD  +1  TO  WS-CIA.                                         ELTTHERP
05531      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHERP
05532      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHERP
05533         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHERP
05534         UNTIL  PVN-BEN-PROVN-IDX > WS-MISC-OP-PROF-CNT.           ELTTHERP
05535      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHERP
05536                                                                   ELTTHERP
05537      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHERP
05538      MOVE +1  TO  WS-CIA                                          ELTTHERP
05539      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHERP
05540                                         END-EXEC.                 ELTTHERP
05541 **---------------------------------------------------------------+ELTTHERP
05542 **                                                               |ELTTHERP
05543 **        P L A C E   O F   T R E A T M E N T                    |ELTTHERP
05544      PERFORM 3050-PLACE-TREAT.                                    ELTTHERP
05545 **                                                               |ELTTHERP
05546 **---------------------------------------------------------------+ELTTHERP
05547                                                                   ELTTHERP
05548 **---------------------------------------------------------------+ELTTHERP
05549 **                                                               |ELTTHERP
05550 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHERP
05551      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHERP
05552 **                                                               |ELTTHERP
05553 **---------------------------------------------------------------+ELTTHERP
05554                                                                   ELTTHERP
05555 **---------------------------------------------------------------+ELTTHERP
05556 **                                                               |ELTTHERP
05557 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHERP
05558 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHERP
05559 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHERP
05560      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHERP
05561 **                                                               |ELTTHERP
05562 **---------------------------------------------------------------+ELTTHERP
05563                                                                   ELTTHERP
05564 **---------------------------------------------------------------+ELTTHERP
05565 **                                                               |ELTTHERP
05566 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHERP
05567      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHERP
05568 **                                                               |ELTTHERP
05569 **---------------------------------------------------------------+ELTTHERP
05570                                                                   ELTTHERP
05571 **---------------------------------------------------------------+ELTTHERP
05572 **                                                               |ELTTHERP
05573 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHERP
05574      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHERP
05575         THRU 3250-EXIT.                                           ELTTHERP
05576 **                                                               |ELTTHERP
05577 **---------------------------------------------------------------+ELTTHERP
05578                                                                   ELTTHERP
05579 **---------------------------------------------------------------+ELTTHERP
05580 **                                                               |ELTTHERP
05581 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHERP
05582      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHERP
05583 **                                                               |ELTTHERP
05584 **---------------------------------------------------------------+ELTTHERP
05585                                                                   ELTTHERP
05586 **---------------------------------------------------------------+ELTTHERP
05587 **                                                               |ELTTHERP
05588 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHERP
05589      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHERP
05590 **                                                               |ELTTHERP
05591 **---------------------------------------------------------------+ELTTHERP
05592                                                                   ELTTHERP
05593 **---------------------------------------------------------------+ELTTHERP
05594 **                                                               |ELTTHERP
05595 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHERP
05596 **                         A N D                                 |ELTTHERP
05597 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHERP
05598      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHERP
05599 **                                                               |ELTTHERP
05600 **---------------------------------------------------------------+ELTTHERP
05601                                                                   ELTTHERP
05602 **---------------------------------------------------------------+ELTTHERP
05603 **                                                               |ELTTHERP
05604 **               P E R F O R M   T A B U L A R                   |ELTTHERP
05605      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHERP
05606 **                                                               |ELTTHERP
05607 **---------------------------------------------------------------+ELTTHERP
05608                                                                   ELTTHERP
05609  6640-EXIT.  EXIT.                                                ELTTHERP
05610                                                                   ELTTHERP
05611  6699-EXIT.           EXIT.                                       ELTTHERP
05612                                                                   ELTTHERP
05613      COPY ELSTCOMP.                                               ELTTHERP
05614 ***************************************************************** ELTTHERP
05615 **          PRINT THE TEXT INFORMATION FOR THE SCREEN           * ELTTHERP
05616 ***************************************************************** ELTTHERP
05617  9200-TEXT-OUTPUT-REQUEST.                                        ELTTHERP
05618                                                                   ELTTHERP
05619       MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                         ELTTHERP
05620       MOVE +0      TO  COF-NBR-HDR-LINES.                         ELTTHERP
05621       MOVE ' '     TO  COF-FUNCTION.                              ELTTHERP
05622                                                                   ELTTHERP
05623       EXEC CICS LINK PROGRAM ('ELUOUTPT')                         ELTTHERP
05624                      COMMAREA (DFHCOMMAREA)                       ELTTHERP
05625                      END-EXEC.                                    ELTTHERP
05626                                                                   ELTTHERP
05627                                                                   ELTTHERP
05628 ****************************************************************  ELTTHERP
05629 *          C A L L   C O D E S   M A N U A L                   *  ELTTHERP
05630 ****************************************************************  ELTTHERP
05631  9300-CALL-CODES-MANUAL.                                          ELTTHERP
05632      INITIALIZE CMF-RETURN-CODE,                                  ELTTHERP
05633                 TCAR-FROM-AREA.                                   ELTTHERP
05634                                                                   ELTTHERP
05635      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTTHERP
05636                       COMMAREA(DFHCOMMAREA)                       ELTTHERP
05637      END-EXEC.                                                    ELTTHERP
05638                                                                   ELTTHERP
05639      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTTHERP
05640      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHERP
05641          ADDRESS OF CMF-DESCR.                                    ELTTHERP
05642                                                                   ELTTHERP
05643 ****************************************************************  ELTTHERP
05644 * 9400-MOVE-TO-COFDTL                                          *  ELTTHERP
05645 * THE PURPOSE OF THIS PARAGRAPH IS TO MOVE EACH LINE RETURNED  *  ELTTHERP
05646 * FROM THE CODES MANUAL TO THE ARRAY INTERFACE FOR PROGRAM     *  ELTTHERP
05647 * ELUOUTPT.  WS-CIA IS THE SUBSRIPT.                           *  ELTTHERP
05648 * RGO. 10/12/95.                                               *  ELTTHERP
05649 ****************************************************************  ELTTHERP
05650  9400-MOVE-TO-COFDTL.                                             ELTTHERP
05651                                                                   ELTTHERP
05652                                                                   ELTTHERP
05653      ADD +1 TO WS-CIA.                                            ELTTHERP
05654      MOVE CMF-DESCR-LINE (WS-SUB-CMF) TO                          ELTTHERP
05655           COF-DTL-LINE (WS-CIA).                                  ELTTHERP
05656                                                                   ELTTHERP
05657      IF WS-CIA > 20 OR = 20                                       ELTTHERP
05658          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTTHERP
05659          MOVE ' ' TO COF-FUNCTION                                 ELTTHERP
05660          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTTHERP
05661                         COMMAREA (DFHCOMMAREA)                    ELTTHERP
05662                         END-EXEC                                  ELTTHERP
05663          MOVE +1 TO WS-CIA                                        ELTTHERP
05664      END-IF.                                                      ELTTHERP
05665                                                                   ELTTHERP
05666 ****************************************************************  ELTTHERP
05667 * 9400-COMPRESS-STRING-MOVE                                    *  ELTTHERP
05668 * THE PURPOSE OF THIS PARAGRAPH IS TO COMPRESS WHAT IS RETURNED*  ELTTHERP
05669 * FROM THE CODES MANUAL, STRING IT INTO 79 CHARACTER LINES,    *  ELTTHERP
05670 * AND DISPLAY IT STARTING ON A NEW LINE.                       *  ELTTHERP
05671 * RGO. 11/09/95.                                               *  ELTTHERP
05672 ****************************************************************  ELTTHERP
05673  9400-COMPRESS-STRING-MOVE.                                       ELTTHERP
05674      MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN.                          ELTTHERP
05675      STRING CMF-DESCR-LINE(1), ' '                                ELTTHERP
05676         CMF-DESCR-LINE(2), ' '                                    ELTTHERP
05677         CMF-DESCR-LINE(3), ' '                                    ELTTHERP
05678         CMF-DESCR-LINE(4), ' '                                    ELTTHERP
05679         CMF-DESCR-LINE(5), ' '                                    ELTTHERP
05680         CMF-DESCR-LINE(6), ' '                                    ELTTHERP
05681         CMF-DESCR-LINE(7), ' '                                    ELTTHERP
05682         CMF-DESCR-LINE(8), ' '                                    ELTTHERP
05683         CMF-DESCR-LINE(9), ' '                                    ELTTHERP
05684         CMF-DESCR-LINE(10), ' '                                   ELTTHERP
05685         CMF-DESCR-LINE(11), ' '                                   ELTTHERP
05686         CMF-DESCR-LINE(12), ' '                                   ELTTHERP
05687         CMF-DESCR-LINE(13), ' '                                   ELTTHERP
05688         CMF-DESCR-LINE(14), ' '                                   ELTTHERP
05689         CMF-DESCR-LINE(15), ' '                                   ELTTHERP
05690         DELIMITED BY SIZE INTO TCAR-FROM-AREA.                    ELTTHERP
05691                                                                   ELTTHERP
05692      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTTHERP
05693      MOVE +15              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTTHERP
05694      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN,           ELTTHERP
05695                                TCAR-OUTPUT-FIELD-3-LEN,           ELTTHERP
05696                                TCAR-OUTPUT-FIELD-4-LEN,           ELTTHERP
05697                                TCAR-OUTPUT-FIELD-5-LEN,           ELTTHERP
05698                                TCAR-OUTPUT-FIELD-6-LEN,           ELTTHERP
05699                                TCAR-OUTPUT-FIELD-7-LEN,           ELTTHERP
05700                                TCAR-OUTPUT-FIELD-8-LEN,           ELTTHERP
05701                                TCAR-OUTPUT-FIELD-9-LEN,           ELTTHERP
05702                                TCAR-OUTPUT-FIELD-10-LEN,          ELTTHERP
05703                                TCAR-OUTPUT-FIELD-11-LEN,          ELTTHERP
05704                                TCAR-OUTPUT-FIELD-12-LEN,          ELTTHERP
05705                                TCAR-OUTPUT-FIELD-13-LEN,          ELTTHERP
05706                                TCAR-OUTPUT-FIELD-14-LEN,          ELTTHERP
05707                                TCAR-OUTPUT-FIELD-15-LEN.          ELTTHERP
05708      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTTHERP
05709                                                                   ELTTHERP
05710      IF WS-MOVE-LINES-TO-CIA                                      ELTTHERP
05711         PERFORM 9450-MOVE-STRUNG                                  ELTTHERP
05712            VARYING WS-SUB1 FROM 1 BY 1                            ELTTHERP
05713            UNTIL WS-SUB1 > TCAR-OUTPUT-FIELDS-USED                ELTTHERP
05714      END-IF.                                                      ELTTHERP
05715                                                                   ELTTHERP
05716  9450-MOVE-STRUNG.                                                ELTTHERP
05717      ADD 1 TO WS-CIA.                                             ELTTHERP
05718      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTTHERP
05719                                                                   ELTTHERP
05720      IF WS-CIA > 20 OR = 20                                       ELTTHERP
05721          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTTHERP
05722          MOVE ' ' TO COF-FUNCTION                                 ELTTHERP
05723          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTTHERP
05724                         COMMAREA (DFHCOMMAREA)                    ELTTHERP
05725                         END-EXEC                                  ELTTHERP
05726          MOVE +1 TO WS-CIA                                        ELTTHERP
05727      END-IF.                                                      ELTTHERP
05728                                                                   ELTTHERP
