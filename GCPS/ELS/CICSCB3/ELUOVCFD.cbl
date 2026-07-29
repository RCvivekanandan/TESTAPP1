00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUOVCFD
00003  PROGRAM-ID.           ELUOVCFD.                                     LV001
00004                                                                   ELUOVCFD
00005  AUTHOR.               NINA A CERVANTES.                          ELUOVCFD
00006                        R. LUKETICH (MAJOR REVISIONS SEP 1989).    ELUOVCFD
00007                                                                   ELUOVCFD
00008  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUOVCFD
00009                        A MUTUAL LEGAL RESERVE COMPANY             ELUOVCFD
00010                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUOVCFD
00011                        233 N. MICHIGAN AVE                        ELUOVCFD
00012                        CHICAGO, ILLINOIS 60601                    ELUOVCFD
00013                                                                   ELUOVCFD
00014  DATE-WRITTEN.         01-SEP-1988.                               ELUOVCFD
00015                                                                   ELUOVCFD
00016  ENVIRONMENT DIVISION.                                            ELUOVCFD
00017  CONFIGURATION SECTION.                                           ELUOVCFD
00018  SOURCE-COMPUTER. IBM-3090.                                       ELUOVCFD
00019  OBJECT-COMPUTER. IBM-3090.                                       ELUOVCFD
00020                                                                   ELUOVCFD
00021 ****************************************************************  ELUOVCFD
00022 *                                                              *  ELUOVCFD
00023 *  ELUOVCFD :  THIS MODULE IS CALLED BY 'ELTCSMRY'.  THE MODULE*  ELUOVCFD
00024 *              IS BROKEN DOWN INTO THREE PHASES:               *  ELUOVCFD
00025 *            1) CREATE THE RANKING REQUEST BLOCKS;             *  ELUOVCFD
00026 *            2) ASSIGN CONFIDENCE FACTORS TO INDIVIDUAL        *  ELUOVCFD
00027 *               ATTRIBUTES PER ACCUM TABULAR;                  *  ELUOVCFD
00028 *            3) RANK THE ACCUMS ACCORDING TO THEIR AGGREGATE   *  ELUOVCFD
00029 *               CONFIDENCE FACTORS THAT WAS ASSIGNED DURING    *  ELUOVCFD
00030 *               PHASE 2.                                       *  ELUOVCFD
00031 *                                                              *  ELUOVCFD
00032 *              ALSO THIS MODULE WILL ISSUE SUCCESSIVE LINKS TO *  ELUOVCFD
00033 *              ELUIBGR, ELUIPGN AND ELUIPGT, WHICH ARE REPOND- *  ELUOVCFD
00034 *              SIBLE FOR TABLE LOOKUPS.  THEY WILL RETURN      *  ELUOVCFD
00035 *              VALUES WHICH ARE USED TO CALCULATE CONFIDENCE   *  ELUOVCFD
00036 *              FACTORS IN PHASE 2.                             *  ELUOVCFD
00037 ****************************************************************  ELUOVCFD
00038 *                      MAINTENANCE HISTORY                     *  ELUOVCFD
00039 *                                                              *  ELUOVCFD
00040 *  MOD     DATE      BY  DRPT              ACTION              *  ELUOVCFD
00041 * ----- ----------- --- ----- ---------------------------------*  ELUOVCFD
00042 * 01.00 01-SEP-1988 NAC       CREATED                          *  ELUOVCFD
00043 * 02.00 22-SEP-1989 RJL       MAJOR REVISIONS TO IMPROVE       *  ELUOVCFD
00044 *                             PERFORMANCE AND CORRECT MAJOR    *  ELUOVCFD
00045 *                             LOGIC ERRORS.  ALSO DESTRUCT.    *  ELUOVCFD
00046 * 02.01 02-OCT-1989 RJL       CONVERTED FUZZY AND'S, OR'S AND  *  ELUOVCFD
00047 *                             COMBINES TO USE NEW SUBROUTINES  *  ELUOVCFD
00048 *                                                              *  ELUOVCFD
00049 ****************************************************************  ELUOVCFD
00050  DATA DIVISION.                                                   ELUOVCFD
00051  WORKING-STORAGE SECTION.                                         ELUOVCFD
00052  77  FILLER                      PIC X(42)   VALUE                ELUOVCFD
00053      '***ELUOVCFD WORKING STORAGE BEGINS HERE***'.                ELUOVCFD
00054  77  HOLD-IDX                    USAGE INDEX.                     ELUOVCFD
00055                                                                   ELUOVCFD
00056  01  WS-SWITCHES.                                                 ELUOVCFD
00057      02 WS-CONTRACT-BC           PICTURE  X(01)                   ELUOVCFD
00058                                  VALUE SPACES.                    ELUOVCFD
00059         88 LOAD-FOR-BC           VALUE '1'.                       ELUOVCFD
00060      02 WS-CONTRACT-BS           PICTURE  X(01)                   ELUOVCFD
00061                                  VALUE SPACES.                    ELUOVCFD
00062         88 LOAD-FOR-BS           VALUE '2'.                       ELUOVCFD
00063      02 WS-CONTRACT-SMM          PICTURE  X(01)                   ELUOVCFD
00064                                  VALUE SPACES.                    ELUOVCFD
00065         88 LOAD-FOR-SMM          VALUE '3'.                       ELUOVCFD
00066      02 WS-CONTRACT-CMM          PICTURE  X(01)                   ELUOVCFD
00067                                  VALUE SPACES.                    ELUOVCFD
00068         88 LOAD-FOR-CMM          VALUE '4'.                       ELUOVCFD
00069      02 WS-PROCESS-SW            PICTURE  X(01)                   ELUOVCFD
00070                                  VALUE SPACE.                     ELUOVCFD
00071         88 PROCESSING-ABM        VALUE 'M'.                       ELUOVCFD
00072         88 PROCESSING-ACL        VALUE 'C'.                       ELUOVCFD
00073         88 PROCESSING-ADL        VALUE 'D'.                       ELUOVCFD
00074         88 PROCESSING-AOL        VALUE 'O'.                       ELUOVCFD
00075      02 WS-QUALIFY-SW            PICTURE  X(01)                   ELUOVCFD
00076                                  VALUE SPACE.                     ELUOVCFD
00077         88 TABULAR-QUALIFIES     VALUE 'Y'.                       ELUOVCFD
00078         88 TABULAR-DISQUALIFIES  VALUE 'N'.                       ELUOVCFD
00079      02 WS-IBGR-SW               PICTURE  X(01)                   ELUOVCFD
00080                                  VALUE SPACE.                     ELUOVCFD
00081         88 ELUIBGR-PROCESSED     VALUE 'Y'.                       ELUOVCFD
00082      02 WS-IPGT-SW               PICTURE  X(01)                   ELUOVCFD
00083                                  VALUE SPACE.                     ELUOVCFD
00084         88 ELUIPGT-PROCESSED     VALUE 'Y'.                       ELUOVCFD
00085      02 WS-IPGN-SW               PICTURE  X(01)                   ELUOVCFD
00086                                  VALUE SPACE.                     ELUOVCFD
00087         88 ELUIPGN-PROCESSED     VALUE 'Y'.                       ELUOVCFD
00088      02 WS-ARGUMENT-SW           PICTURE  X(01)                   ELUOVCFD
00089                                  VALUE SPACE.                     ELUOVCFD
00090         88 ARGUMENT-FOUND        VALUE 'Y'.                       ELUOVCFD
00091         88 ARGUMENT-NOTFND       VALUE 'N'.                       ELUOVCFD
00092 *                                                                 ELUOVCFD
00093  01  WS-COUNTS.                                                   ELUOVCFD
00094      02 WS-ELSRRBLC-MVO          PICTURE S9(04)          COMP     ELUOVCFD
00095                                  VALUE ZERO.                      ELUOVCFD
00096      02 WS-ELSATBLC-MVO          PICTURE S9(04)          COMP     ELUOVCFD
00097                                  VALUE ZERO.                      ELUOVCFD
00098      02 WS-ABM-COMBINED-TTL      PICTURE S9(04)          COMP     ELUOVCFD
00099                                  VALUE ZERO.                      ELUOVCFD
00100      02 WS-ACL-COMBINED-TTL      PICTURE S9(04)          COMP     ELUOVCFD
00101                                  VALUE ZERO.                      ELUOVCFD
00102      02 WS-ADL-COMBINED-TTL      PICTURE S9(04)          COMP     ELUOVCFD
00103                                  VALUE ZERO.                      ELUOVCFD
00104      02 WS-AOL-COMBINED-TTL      PICTURE S9(04)          COMP     ELUOVCFD
00105                                  VALUE ZERO.                      ELUOVCFD
00106      02 WS-SUBA                  PICTURE S9(04)          COMP     ELUOVCFD
00107                                  VALUE ZERO.                      ELUOVCFD
00108      02 WS-SUBB                  PICTURE S9(04)          COMP     ELUOVCFD
00109                                  VALUE ZERO.                      ELUOVCFD
00110 *                                                                 ELUOVCFD
00111  01  WS-CONFIDENCE-FACTORS.                                       ELUOVCFD
00112      02 WS-CF-OV                 COMP-1.                          ELUOVCFD
00113      02 WS-CF-BEN-PERD           COMP-1.                          ELUOVCFD
00114      02 WS-CF-FAM                COMP-1.                          ELUOVCFD
00115      02 WS-CF-INDIV              COMP-1.                          ELUOVCFD
00116      02 WS-CF-INST               COMP-1.                          ELUOVCFD
00117      02 WS-CF-INST-LOB           COMP-1.                          ELUOVCFD
00118      02 WS-CF-INST-IPGT          COMP-1.                          ELUOVCFD
00119      02 WS-CF-INST-IBGR          COMP-1.                          ELUOVCFD
00120      02 WS-CF-PROF               COMP-1.                          ELUOVCFD
00121      02 WS-CF-PROF-LOB           COMP-1.                          ELUOVCFD
00122      02 WS-CF-PROF-IPGT          COMP-1.                          ELUOVCFD
00123      02 WS-CF-PROF-IBGR          COMP-1.                          ELUOVCFD
00124      02 WS-CF-BAS                COMP-1.                          ELUOVCFD
00125      02 WS-CF-SUP                COMP-1.                          ELUOVCFD
00126      02 WS-CF-IP                 COMP-1.                          ELUOVCFD
00127      02 WS-CF-IP-POT             COMP-1.                          ELUOVCFD
00128      02 WS-CF-IP-IBGR            COMP-1.                          ELUOVCFD
00129      02 WS-CF-IP-BOTH-IBGR       COMP-1.                          ELUOVCFD
00130      02 WS-CF-OP                 COMP-1.                          ELUOVCFD
00131      02 WS-CF-OP-POT             COMP-1.                          ELUOVCFD
00132      02 WS-CF-OP-IBGR            COMP-1.                          ELUOVCFD
00133      02 WS-CF-OP-BOTH-IBGR       COMP-1.                          ELUOVCFD
00134      02 WS-CF-PLAN               COMP-1.                          ELUOVCFD
00135      02 WS-CF-NON-PLAN           COMP-1.                          ELUOVCFD
00136      02 WS-CF-PPO                COMP-1.                          ELUOVCFD
00137      02 WS-CF-NON-PPO            COMP-1.                          ELUOVCFD
00138      02 WS-CF-OV-COND-BIT        COMP-1.                          ELUOVCFD
00139      02 WS-CF-OV-CC-IND          COMP-1.                          ELUOVCFD
00140      02 WS-CF-OV-SERV-GRP        COMP-1.                          ELUOVCFD
00141      02 WS-CF-OV-POT             COMP-1.                          ELUOVCFD
00142      02 WS-CF-OV-INT-DESC        COMP-1.                          ELUOVCFD
00143      02 WS-CF-OV-INT-DESC-INST   COMP-1.                          ELUOVCFD
00144      02 WS-CF-OV-INT-DESC-PROF   COMP-1.                          ELUOVCFD
00145      02 WS-CF-OV-VAL-QUAL        COMP-1.                          ELUOVCFD
00146      02 WS-CF-OV-VAL-QUAL-INST   COMP-1.                          ELUOVCFD
00147      02 WS-CF-OV-VAL-QUAL-PROF   COMP-1.                          ELUOVCFD
00148      02 WS-CF-OV-IBGR            COMP-1.                          ELUOVCFD
00149      02 WS-CF-OV-IPGN            COMP-1.                          ELUOVCFD
00150      02 WS-CF-OV-IPGT            COMP-1.                          ELUOVCFD
00151 *                                                                 ELUOVCFD
00152  01  WS-CONFIDENCE-WEIGHTS.                                       ELUOVCFD
00153      02 WS-WT-OV                 COMP-1      VALUE +1.000000E+00. ELUOVCFD
00154      02 WS-WT-BEN-PERD           COMP-1      VALUE +1.000000E+00. ELUOVCFD
00155      02 WS-WT-FAM                COMP-1      VALUE +1.000000E+00. ELUOVCFD
00156      02 WS-WT-INDIV              COMP-1      VALUE +1.000000E+00. ELUOVCFD
00157      02 WS-WT-INST               COMP-1      VALUE +1.000000E+00. ELUOVCFD
00158      02 WS-WT-INST-LOB           COMP-1      VALUE +0.950000E+00. ELUOVCFD
00159      02 WS-WT-INST-IPGT          COMP-1      VALUE +0.900000E+00. ELUOVCFD
00160      02 WS-WT-INST-IBGR          COMP-1      VALUE +0.700000E+00. ELUOVCFD
00161      02 WS-WT-PROF               COMP-1      VALUE +1.000000E+00. ELUOVCFD
00162      02 WS-WT-PROF-LOB           COMP-1      VALUE +0.950000E+00. ELUOVCFD
00163      02 WS-WT-PROF-IPGT          COMP-1      VALUE +0.900000E+00. ELUOVCFD
00164      02 WS-WT-PROF-IBGR          COMP-1      VALUE +0.700000E+00. ELUOVCFD
00165      02 WS-WT-BAS                COMP-1      VALUE +1.000000E+00. ELUOVCFD
00166      02 WS-WT-SUP                COMP-1      VALUE +1.000000E+00. ELUOVCFD
00167      02 WS-WT-IP                 COMP-1      VALUE +1.000000E+00. ELUOVCFD
00168      02 WS-WT-IP-POT             COMP-1      VALUE +0.950000E+00. ELUOVCFD
00169      02 WS-WT-IP-IBGR            COMP-1      VALUE +0.800000E+00. ELUOVCFD
00170      02 WS-WT-OP                 COMP-1      VALUE +1.000000E+00. ELUOVCFD
00171      02 WS-WT-OP-POT             COMP-1      VALUE +0.950000E+00. ELUOVCFD
00172      02 WS-WT-OP-IBGR            COMP-1      VALUE +0.800000E+00. ELUOVCFD
00173      02 WS-WT-PLAN               COMP-1      VALUE +1.000000E+00. ELUOVCFD
00174      02 WS-WT-NON-PLAN           COMP-1      VALUE +1.000000E+00. ELUOVCFD
00175      02 WS-WT-PPO                COMP-1      VALUE +1.000000E+00. ELUOVCFD
00176      02 WS-WT-NON-PPO            COMP-1      VALUE +1.000000E+00. ELUOVCFD
00177      02 WS-WT-OV-COND-BIT        COMP-1      VALUE +1.000000E+00. ELUOVCFD
00178      02 WS-WT-OV-CC-IND          COMP-1      VALUE +1.000000E+00. ELUOVCFD
00179      02 WS-WT-OV-SERV-GRP        COMP-1      VALUE +1.000000E+00. ELUOVCFD
00180      02 WS-WT-OV-POT             COMP-1      VALUE +1.000000E+00. ELUOVCFD
00181      02 WS-WT-OV-INT-DESC        COMP-1      VALUE +1.000000E+00. ELUOVCFD
00182      02 WS-WT-OV-INT-DESC-INST   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00183      02 WS-WT-OV-INST-INT-DESC   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00184      02 WS-WT-OV-INT-DESC-PROF   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00185      02 WS-WT-OV-PROF-INT-DESC   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00186      02 WS-WT-OV-VAL-QUAL        COMP-1      VALUE +1.000000E+00. ELUOVCFD
00187      02 WS-WT-OV-VAL-QUAL-INST   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00188      02 WS-WT-OV-INST-VAL-QUAL   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00189      02 WS-WT-OV-VAL-QUAL-PROF   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00190      02 WS-WT-OV-PROF-VAL-QUAL   COMP-1      VALUE +1.000000E+00. ELUOVCFD
00191      02 WS-WT-OV-IBGR            COMP-1      VALUE +1.000000E+00. ELUOVCFD
00192      02 WS-WT-OV-IPGN            COMP-1      VALUE +1.000000E+00. ELUOVCFD
00193      02 WS-WT-OV-IPGT            COMP-1      VALUE +1.000000E+00. ELUOVCFD
00194 *                                                                 ELUOVCFD
00195  01  WS-WEIGHTED-CONFIDENCE-FACTORS.                              ELUOVCFD
00196      02 WS-CW-OV                 COMP-1.                          ELUOVCFD
00197      02 WS-CW-BEN-PERD           COMP-1.                          ELUOVCFD
00198      02 WS-CW-FAM                COMP-1.                          ELUOVCFD
00199      02 WS-CW-INDIV              COMP-1.                          ELUOVCFD
00200      02 WS-CW-INST               COMP-1.                          ELUOVCFD
00201      02 WS-CW-INST-LOB           COMP-1.                          ELUOVCFD
00202      02 WS-CW-INST-IPGT          COMP-1.                          ELUOVCFD
00203      02 WS-CW-INST-IBGR          COMP-1.                          ELUOVCFD
00204      02 WS-CW-PROF               COMP-1.                          ELUOVCFD
00205      02 WS-CW-PROF-LOB           COMP-1.                          ELUOVCFD
00206      02 WS-CW-PROF-IPGT          COMP-1.                          ELUOVCFD
00207      02 WS-CW-PROF-IBGR          COMP-1.                          ELUOVCFD
00208      02 WS-CW-BAS                COMP-1.                          ELUOVCFD
00209      02 WS-CW-SUP                COMP-1.                          ELUOVCFD
00210      02 WS-CW-IP                 COMP-1.                          ELUOVCFD
00211      02 WS-CW-IP-POT             COMP-1.                          ELUOVCFD
00212      02 WS-CW-IP-IBGR            COMP-1.                          ELUOVCFD
00213      02 WS-CW-OP                 COMP-1.                          ELUOVCFD
00214      02 WS-CW-OP-POT             COMP-1.                          ELUOVCFD
00215      02 WS-CW-OP-IBGR            COMP-1.                          ELUOVCFD
00216      02 WS-CW-PLAN               COMP-1.                          ELUOVCFD
00217      02 WS-CW-NON-PLAN           COMP-1.                          ELUOVCFD
00218      02 WS-CW-PPO                COMP-1.                          ELUOVCFD
00219      02 WS-CW-NON-PPO            COMP-1.                          ELUOVCFD
00220      02 WS-CW-OV-COND-BIT        COMP-1.                          ELUOVCFD
00221      02 WS-CW-OV-CC-IND          COMP-1.                          ELUOVCFD
00222      02 WS-CW-OV-SERV-GRP        COMP-1.                          ELUOVCFD
00223      02 WS-CW-OV-POT             COMP-1.                          ELUOVCFD
00224      02 WS-CW-OV-INT-DESC        COMP-1.                          ELUOVCFD
00225      02 WS-CW-OV-INT-DESC-INST   COMP-1.                          ELUOVCFD
00226      02 WS-CW-OV-INST-INT-DESC   COMP-1.                          ELUOVCFD
00227      02 WS-CW-OV-INT-DESC-PROF   COMP-1.                          ELUOVCFD
00228      02 WS-CW-OV-PROF-INT-DESC   COMP-1.                          ELUOVCFD
00229      02 WS-CW-OV-VAL-QUAL        COMP-1.                          ELUOVCFD
00230      02 WS-CW-OV-VAL-QUAL-INST   COMP-1.                          ELUOVCFD
00231      02 WS-CW-OV-INST-VAL-QUAL   COMP-1.                          ELUOVCFD
00232      02 WS-CW-OV-VAL-QUAL-PROF   COMP-1.                          ELUOVCFD
00233      02 WS-CW-OV-PROF-VAL-QUAL   COMP-1.                          ELUOVCFD
00234      02 WS-CW-OV-IBGR-INST       COMP-1.                          ELUOVCFD
00235      02 WS-CW-OV-IBGR-PROF       COMP-1.                          ELUOVCFD
00236      02 WS-CW-OV-IPGN            COMP-1.                          ELUOVCFD
00237      02 WS-CW-OV-IPGT-INST       COMP-1.                          ELUOVCFD
00238      02 WS-CW-OV-IPGT-PROF       COMP-1.                          ELUOVCFD
00239 *                                                                 ELUOVCFD
00240  01  WS-DEFAULT-CONFIDENCE-FACTORS.                               ELUOVCFD
00241      02 WS-DV-IP-POT             COMP-1      VALUE +0.444444E+00. ELUOVCFD
00242      02 WS-DV-OP-POT             COMP-1      VALUE +0.176471E+00. ELUOVCFD
00243      02 WS-DV-OV-POT             COMP-1      VALUE +0.120000E+00. ELUOVCFD
00244      02 WS-DV-OV-INT-DESC-INST   COMP-1      VALUE +0.200000E+00. ELUOVCFD
00245      02 WS-DV-OV-INT-DESC-PROF   COMP-1      VALUE +0.200000E+00. ELUOVCFD
00246      02 WS-DV-OV-VAL-QUAL-INST   COMP-1      VALUE +0.700000E+00. ELUOVCFD
00247      02 WS-DV-OV-VAL-QUAL-PROF   COMP-1      VALUE +0.700000E+00. ELUOVCFD
00248 *                                                                 ELUOVCFD
00249  01  WS-CF-STUFF.                                                 ELUOVCFD
00250      02 WS-CF-OV-COND-BIT-VTRUE  COMP-1      VALUE +0.950000E+00. ELUOVCFD
00251      02 WS-CF-OV-COND-BIT-MTRUE  COMP-1      VALUE +0.850000E+00. ELUOVCFD
00252      02 WS-CF-OV-CC-IND-TRUE     COMP-1      VALUE +0.950000E+00. ELUOVCFD
00253      02 WS-CF-OV-SERV-GRP-TRUE   COMP-1      VALUE +0.950000E+00. ELUOVCFD
00254      02 WS-CF-THRESHHOLD         COMP-1      VALUE +0.200000E+00. ELUOVCFD
00255      02 WS-CF-TRUE               COMP-1      VALUE +1.000000E+00. ELUOVCFD
00256      02 WS-CF-FALSE              COMP-1      VALUE -1.000000E+00. ELUOVCFD
00257      02 WS-CF-POS                COMP-1      VALUE +1.000000E+00. ELUOVCFD
00258      02 WS-CF-NEG                COMP-1      VALUE -1.000000E+00. ELUOVCFD
00259      02 WS-CF-NEG-20             COMP-1      VALUE -0.200000E+00. ELUOVCFD
00260      02 WS-CF-95                 COMP-1      VALUE +0.950000E+00. ELUOVCFD
00261      02 WS-CF-90                 COMP-1      VALUE +0.900000E+00. ELUOVCFD
00262      02 WS-CF-70                 COMP-1      VALUE +0.700000E+00. ELUOVCFD
00263      02 WS-CF-80                 COMP-1      VALUE +0.800000E+00. ELUOVCFD
00264      02 WS-CF-85                 COMP-1      VALUE +0.850000E+00. ELUOVCFD
00265 *                                                                 ELUOVCFD
00266  01  WS-WORKAREAS.                                                ELUOVCFD
00267      02 CF1                      COMP-1.                          ELUOVCFD
00268      02 CF2                      COMP-1.                          ELUOVCFD
00269      02 WS-CF-WORKAREA           COMP-1.                          ELUOVCFD
00270      02 MIN                      COMP-1.                          ELUOVCFD
00271      02 MIN1                     COMP-1.                          ELUOVCFD
00272      02 MIN2                     COMP-1.                          ELUOVCFD
00273 *                                                                 ELUOVCFD
00274  01  WS-PROVIDER-CONTROL.                                         ELUOVCFD
00275      03  FILLER                  PICTURE  X(01).                  ELUOVCFD
00276         88 PROVIDER-PLAN         VALUE '1'.                       ELUOVCFD
00277         88 PROVIDER-NON-PLAN     VALUE '2'.                       ELUOVCFD
00278      03 FILLER                   PICTURE  X(01).                  ELUOVCFD
00279 /                                                                 ELUOVCFD
00280      COPY ELSOARRC.                                               ELUOVCFD
00281 /                                                                 ELUOVCFD
00282      COPY ELSCFTB1.                                               ELUOVCFD
00283 /                                                                 ELUOVCFD
00284      COPY ELSCFTB2.                                               ELUOVCFD
00285 /                                                                 ELUOVCFD
00286      COPY ELSCFTB3.                                               ELUOVCFD
00287 /                                                                 ELUOVCFD
00288      COPY ELSCFTB5.                                               ELUOVCFD
00289 /                                                                 ELUOVCFD
00290      COPY ELSCFTB6.                                               ELUOVCFD
00291 /                                                                 ELUOVCFD
00292  LINKAGE SECTION.                                                 ELUOVCFD
00293  01  DFHCOMMAREA.                                                 ELUOVCFD
00294      COPY ELSCOMMC.                                               ELUOVCFD
00295 /                                                                 ELUOVCFD
00296      COPY ELSCIA2C.                                               ELUOVCFD
00297 /                                                                 ELUOVCFD
00298      COPY ELSSSCBC.                                               ELUOVCFD
00299 /                                                                 ELUOVCFD
00300      COPY ELSCSACC.                                               ELUOVCFD
00301 /                                                                 ELUOVCFD
00302      COPY ELSRRBLC.                                               ELUOVCFD
00303 /                                                                 ELUOVCFD
00304      COPY ELSATBLC.                                               ELUOVCFD
00305 /                                                                 ELUOVCFD
00306      COPY ELSIBGRC.                                               ELUOVCFD
00307 /                                                                 ELUOVCFD
00308      COPY ELSIPGNC.                                               ELUOVCFD
00309 /                                                                 ELUOVCFD
00310      COPY ELSIPGTC.                                               ELUOVCFD
00311 /***********************************************************      ELUOVCFD
00312 *                                                          *      ELUOVCFD
00313 *    OVERALL ACCUMULATOR CONFIDENCE FACTOR DETERMINATION   *      ELUOVCFD
00314 *                                                          *      ELUOVCFD
00315 *    PROCEDURE DIVISION                                    *      ELUOVCFD
00316 *                                                          *      ELUOVCFD
00317 ************************************************************      ELUOVCFD
00318                                                                   ELUOVCFD
00319  PROCEDURE DIVISION.                                              ELUOVCFD
00320                                                                   ELUOVCFD
00321  OVERALL-ACCUM-CF-DETERMINATION.                                  ELUOVCFD
00322      PERFORM INITIALIZATION.                                      ELUOVCFD
00323      PERFORM PROCESS.                                             ELUOVCFD
00324      GOBACK.                                                      ELUOVCFD
00325                                                                   ELUOVCFD
00326 /***********************************************************      ELUOVCFD
00327 *                                                          *      ELUOVCFD
00328 *        INITIALIZATION.                                   *      ELUOVCFD
00329 *                                                          *      ELUOVCFD
00330 ************************************************************      ELUOVCFD
00331                                                                   ELUOVCFD
00332  INITIALIZATION.                                                  ELUOVCFD
00333      PERFORM ESTAB-ADDR-CTRL-BLOCKS.                              ELUOVCFD
00334      PERFORM ESTAB-ADDR-CSAC.                                     ELUOVCFD
00335                                                                   ELUOVCFD
00336 ************************************************************      ELUOVCFD
00337 *                                                          *      ELUOVCFD
00338 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUOVCFD
00339 *                                                          *      ELUOVCFD
00340 ************************************************************      ELUOVCFD
00341                                                                   ELUOVCFD
00342  ESTAB-ADDR-CTRL-BLOCKS.                                          ELUOVCFD
00343      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELUOVCFD
00344      PERFORM ESTAB-ADDR-CIA.                                      ELUOVCFD
00345      PERFORM ESTAB-ADDR-SSB.                                      ELUOVCFD
00346                                                                   ELUOVCFD
00347 ************************************************************      ELUOVCFD
00348 *                                                          *      ELUOVCFD
00349 *        CHECK FOR VALID COMMAREA                          *      ELUOVCFD
00350 *                                                          *      ELUOVCFD
00351 ************************************************************      ELUOVCFD
00352                                                                   ELUOVCFD
00353  CHECK-FOR-VALID-COMMAREA.                                        ELUOVCFD
00354      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUOVCFD
00355      THEN                                                         ELUOVCFD
00356         EXEC CICS ABEND ABCODE ('EL01') END-EXEC.                 ELUOVCFD
00357                                                                   ELUOVCFD
00358 ************************************************************      ELUOVCFD
00359 *                                                          *      ELUOVCFD
00360 *        ESTABLISH ADDRESSABILITY OF CIA                   *      ELUOVCFD
00361 *                                                          *      ELUOVCFD
00362 ************************************************************      ELUOVCFD
00363                                                                   ELUOVCFD
00364  ESTAB-ADDR-CIA.                                                  ELUOVCFD
00365      IF ECA-CIA-PTR = NULL                                        ELUOVCFD
00366      THEN                                                         ELUOVCFD
00367         PERFORM SIGNAL-INVALID-CIA                                ELUOVCFD
00368      ELSE                                                         ELUOVCFD
00369         CALL 'ELUINISM'                                           ELUOVCFD
00370            USING DFHCOMMAREA                                      ELUOVCFD
00371                  ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.        ELUOVCFD
00372                                                                   ELUOVCFD
00373 ************************************************************      ELUOVCFD
00374 *                                                          *      ELUOVCFD
00375 *        SIGNAL INVALID CIA                                *      ELUOVCFD
00376 *                                                          *      ELUOVCFD
00377 ************************************************************      ELUOVCFD
00378                                                                   ELUOVCFD
00379  SIGNAL-INVALID-CIA.                                              ELUOVCFD
00380      EXEC CICS ABEND ABCODE ('EL02') END-EXEC.                    ELUOVCFD
00381                                                                   ELUOVCFD
00382 ************************************************************      ELUOVCFD
00383 *                                                          *      ELUOVCFD
00384 *        ESTABLISH ADDRESSABILITY OF ELSSSCB               *      ELUOVCFD
00385 *                                                          *      ELUOVCFD
00386 ************************************************************      ELUOVCFD
00387                                                                   ELUOVCFD
00388  ESTAB-ADDR-SSB.                                                  ELUOVCFD
00389      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUOVCFD
00390      CALL 'ELUSETAD'                                              ELUOVCFD
00391         USING DFHCOMMAREA                                         ELUOVCFD
00392               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.             ELUOVCFD
00393      IF CIA-RC-PTR-NULL                                           ELUOVCFD
00394          PERFORM SIGNAL-UNALLOC-AREA.                             ELUOVCFD
00395                                                                   ELUOVCFD
00396 ************************************************************      ELUOVCFD
00397 *                                                          *      ELUOVCFD
00398 *        ESTABLISH ADDRESSABILITY OF ELSCSAC               *      ELUOVCFD
00399 *                                                          *      ELUOVCFD
00400 ************************************************************      ELUOVCFD
00401                                                                   ELUOVCFD
00402  ESTAB-ADDR-CSAC.                                                 ELUOVCFD
00403      SET  CIA-ELSCSAC-DDN   TO TRUE.                              ELUOVCFD
00404      CALL 'ELUSETAD'                                              ELUOVCFD
00405         USING DFHCOMMAREA                                         ELUOVCFD
00406               ADDRESS OF CSAC-ACCUMULATOR-TABLE.                  ELUOVCFD
00407      IF CIA-RC-PTR-NULL                                           ELUOVCFD
00408          PERFORM SIGNAL-UNALLOC-AREA.                             ELUOVCFD
00409                                                                   ELUOVCFD
00410 ************************************************************      ELUOVCFD
00411 *                                                          *      ELUOVCFD
00412 *        SIGNAL UNALLOCATED AREA ERROR                     *      ELUOVCFD
00413 *                                                          *      ELUOVCFD
00414 ************************************************************      ELUOVCFD
00415                                                                   ELUOVCFD
00416  SIGNAL-UNALLOC-AREA.                                             ELUOVCFD
00417      SET CIA-AB-UNALLOC-AREA  TO TRUE.                            ELUOVCFD
00418      PERFORM SIGNAL-ABEND.                                        ELUOVCFD
00419                                                                   ELUOVCFD
00420 ************************************************************      ELUOVCFD
00421 *                                                          *      ELUOVCFD
00422 *        SIGNAL ABEND                                      *      ELUOVCFD
00423 *                                                          *      ELUOVCFD
00424 ************************************************************      ELUOVCFD
00425                                                                   ELUOVCFD
00426  SIGNAL-ABEND.                                                    ELUOVCFD
00427      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUOVCFD
00428                                                                   ELUOVCFD
00429 /***********************************************************      ELUOVCFD
00430 *                                                          *      ELUOVCFD
00431 *        PROCESS                                           *      ELUOVCFD
00432 *                                                          *      ELUOVCFD
00433 ************************************************************      ELUOVCFD
00434                                                                   ELUOVCFD
00435  PROCESS.                                                         ELUOVCFD
00436      PERFORM CREATE-RANKING-REQUEST-BLOCKS.                       ELUOVCFD
00437      PERFORM ASSIGN-CONFIDENCE-FACTORS.                           ELUOVCFD
00438      PERFORM RANK-TABULARS.                                       ELUOVCFD
00439                                                                   ELUOVCFD
00440 ************************************************************      ELUOVCFD
00441 *                                                          *      ELUOVCFD
00442 *        CREATE RANKING REQUEST BLOCKS                     *      ELUOVCFD
00443 *                                                          *      ELUOVCFD
00444 ************************************************************      ELUOVCFD
00445                                                                   ELUOVCFD
00446  CREATE-RANKING-REQUEST-BLOCKS.                                   ELUOVCFD
00447      PERFORM DETERMINE-TYPE-OF-CONTRACT-BEI.                      ELUOVCFD
00448      IF CSAC-ABM-GC-TBL-PTR NOT = NULLS                           ELUOVCFD
00449          PERFORM CREATE-RANKING-REQUEST-ABM.                      ELUOVCFD
00450      IF CSAC-ACL-GC-TBL-PTR NOT = NULLS                           ELUOVCFD
00451          PERFORM CREATE-RANKING-REQUEST-ACL.                      ELUOVCFD
00452      IF CSAC-ADL-GC-TBL-PTR NOT = NULLS                           ELUOVCFD
00453          PERFORM CREATE-RANKING-REQUEST-ADL.                      ELUOVCFD
00454      IF CSAC-AOL-GC-TBL-PTR NOT = NULLS                           ELUOVCFD
00455          PERFORM CREATE-RANKING-REQUEST-AOL.                      ELUOVCFD
00456                                                                   ELUOVCFD
00457 ************************************************************      ELUOVCFD
00458 *                                                          *      ELUOVCFD
00459 *        DETERMINE TYPE OF CONTRACT BEING PROCESSED        *      ELUOVCFD
00460 *                                                          *      ELUOVCFD
00461 ************************************************************      ELUOVCFD
00462                                                                   ELUOVCFD
00463  DETERMINE-TYPE-OF-CONTRACT-BEI.                                  ELUOVCFD
00464      IF    SSB-CONT-L-O-B (1) = 4                                 ELUOVCFD
00465         OR SSB-CONT-L-O-B (3) = 4                                 ELUOVCFD
00466      THEN                                                         ELUOVCFD
00467         SET LOAD-FOR-CMM TO TRUE                                  ELUOVCFD
00468         MOVE SSB-CONT-PROVDR-CONTROL (1)                          ELUOVCFD
00469           TO WS-PROVIDER-CONTROL                                  ELUOVCFD
00470      ELSE                                                         ELUOVCFD
00471          PERFORM SET-BASE-PLUS-SWITCH.                            ELUOVCFD
00472                                                                   ELUOVCFD
00473 ************************************************************      ELUOVCFD
00474 *                                                          *      ELUOVCFD
00475 *        SET BASE PLUS SWITCH                              *      ELUOVCFD
00476 *                                                          *      ELUOVCFD
00477 ************************************************************      ELUOVCFD
00478                                                                   ELUOVCFD
00479  SET-BASE-PLUS-SWITCH.                                            ELUOVCFD
00480      IF SSB-CONT-L-O-B (1) = 1                                    ELUOVCFD
00481      THEN                                                         ELUOVCFD
00482         SET LOAD-FOR-BC TO TRUE                                   ELUOVCFD
00483         MOVE SSB-CONT-PROVDR-CONTROL (1) TO WS-PROVIDER-CONTROL.  ELUOVCFD
00484      IF SSB-CONT-L-O-B (3) = 2                                    ELUOVCFD
00485      THEN                                                         ELUOVCFD
00486         SET LOAD-FOR-BS TO TRUE.                                  ELUOVCFD
00487      IF    SSB-CONT-L-O-B (2) = 3                                 ELUOVCFD
00488         OR SSB-CONT-L-O-B (4) = 3                                 ELUOVCFD
00489      THEN                                                         ELUOVCFD
00490         SET LOAD-FOR-SMM TO TRUE.                                 ELUOVCFD
00491                                                                   ELUOVCFD
00492 ************************************************************      ELUOVCFD
00493 *                                                          *      ELUOVCFD
00494 *        CREATE RANKING REQUEST BLOCK FOR ABM              *      ELUOVCFD
00495 *                                                          *      ELUOVCFD
00496 ************************************************************      ELUOVCFD
00497                                                                   ELUOVCFD
00498  CREATE-RANKING-REQUEST-ABM.                                      ELUOVCFD
00499      SET PROCESSING-ABM TO TRUE.                                  ELUOVCFD
00500      IF LOAD-FOR-CMM                                              ELUOVCFD
00501      THEN                                                         ELUOVCFD
00502          PERFORM COMPUTE-ABM-CMM-LENGTH                           ELUOVCFD
00503      ELSE                                                         ELUOVCFD
00504          PERFORM COMPUTE-ABM-BASE-PLUS-LENGTH.                    ELUOVCFD
00505      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00506      PERFORM LINK-TO-MODULE-ELUSTGMG.                             ELUOVCFD
00507      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00508      CALL 'ELUSETAD'                                              ELUOVCFD
00509         USING DFHCOMMAREA                                         ELUOVCFD
00510               ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.                ELUOVCFD
00511      SET CSAC-ABM-RR-PTR TO ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.  ELUOVCFD
00512      PERFORM LOAD-ABM-RANKING-REQUESTS.                           ELUOVCFD
00513                                                                   ELUOVCFD
00514 ************************************************************      ELUOVCFD
00515 *                                                          *      ELUOVCFD
00516 *        LOAD ABM RANKING REQUESTS                         *      ELUOVCFD
00517 *                                                          *      ELUOVCFD
00518 ************************************************************      ELUOVCFD
00519                                                                   ELUOVCFD
00520  LOAD-ABM-RANKING-REQUESTS.                                       ELUOVCFD
00521      IF LOAD-FOR-CMM                                              ELUOVCFD
00522          PERFORM MOVE-ABM-CMM-TABLE-COUNT                         ELUOVCFD
00523      ELSE                                                         ELUOVCFD
00524          PERFORM MOVE-ABM-BASE-PLUS-TABLE-COUNT.                  ELUOVCFD
00525      SET RRBL-ABM TO TRUE.                                        ELUOVCFD
00526      IF LOAD-FOR-CMM                                              ELUOVCFD
00527          PERFORM LOAD-ABM-CMM-RANKING-REQUESTS                    ELUOVCFD
00528      ELSE                                                         ELUOVCFD
00529          PERFORM LOAD-ABM-BASE-PLUS-RANKING-REQ.                  ELUOVCFD
00530                                                                   ELUOVCFD
00531 ************************************************************      ELUOVCFD
00532 *                                                          *      ELUOVCFD
00533 *        COMPUTE ABM CMM LENGTH                            *      ELUOVCFD
00534 *                                                          *      ELUOVCFD
00535 ************************************************************      ELUOVCFD
00536                                                                   ELUOVCFD
00537  COMPUTE-ABM-CMM-LENGTH.                                          ELUOVCFD
00538      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
00539            LENGTH OF RRBL-ACCUMULATOR-TYPE                        ELUOVCFD
00540          + LENGTH OF RRBL-TBL-CNT                                 ELUOVCFD
00541          + (  WS-ABM-CMM-TBL-CNT                                  ELUOVCFD
00542             * LENGTH OF RRBL-RANK-REQ-BLOCK).                     ELUOVCFD
00543                                                                   ELUOVCFD
00544 ************************************************************      ELUOVCFD
00545 *                                                          *      ELUOVCFD
00546 *        COMPUTE ABM BASE PLUS LENGTH                      *      ELUOVCFD
00547 *                                                          *      ELUOVCFD
00548 ************************************************************      ELUOVCFD
00549                                                                   ELUOVCFD
00550  COMPUTE-ABM-BASE-PLUS-LENGTH.                                    ELUOVCFD
00551      IF LOAD-FOR-BC                                               ELUOVCFD
00552         ADD WS-ABM-BC-TBL-CNT TO WS-ABM-COMBINED-TTL.             ELUOVCFD
00553      IF LOAD-FOR-BS                                               ELUOVCFD
00554         ADD WS-ABM-BS-TBL-CNT TO WS-ABM-COMBINED-TTL.             ELUOVCFD
00555      IF LOAD-FOR-SMM                                              ELUOVCFD
00556         ADD WS-ABM-SMM-TBL-CNT TO WS-ABM-COMBINED-TTL.            ELUOVCFD
00557      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
00558            LENGTH OF RRBL-ACCUMULATOR-TYPE                        ELUOVCFD
00559          + LENGTH OF RRBL-TBL-CNT                                 ELUOVCFD
00560          + (  WS-ABM-COMBINED-TTL                                 ELUOVCFD
00561             * LENGTH OF RRBL-RANK-REQ-BLOCK).                     ELUOVCFD
00562                                                                   ELUOVCFD
00563 ************************************************************      ELUOVCFD
00564 *                                                          *      ELUOVCFD
00565 *        MOVE ABM CMM TABLE COUNT                          *      ELUOVCFD
00566 *                                                          *      ELUOVCFD
00567 ************************************************************      ELUOVCFD
00568                                                                   ELUOVCFD
00569  MOVE-ABM-CMM-TABLE-COUNT.                                        ELUOVCFD
00570      MOVE WS-ABM-CMM-TBL-CNT TO RRBL-TBL-CNT.                     ELUOVCFD
00571                                                                   ELUOVCFD
00572 ************************************************************      ELUOVCFD
00573 *                                                          *      ELUOVCFD
00574 *        MOVE ABM BASE PLUS TABLE COUNT                    *      ELUOVCFD
00575 *                                                          *      ELUOVCFD
00576 ************************************************************      ELUOVCFD
00577                                                                   ELUOVCFD
00578  MOVE-ABM-BASE-PLUS-TABLE-COUNT.                                  ELUOVCFD
00579      MOVE WS-ABM-COMBINED-TTL TO RRBL-TBL-CNT.                    ELUOVCFD
00580                                                                   ELUOVCFD
00581 ************************************************************      ELUOVCFD
00582 *                                                          *      ELUOVCFD
00583 *        LOAD ABM CMM RANKING REQUESTS                     *      ELUOVCFD
00584 *                                                          *      ELUOVCFD
00585 ************************************************************      ELUOVCFD
00586                                                                   ELUOVCFD
00587  LOAD-ABM-CMM-RANKING-REQUESTS.                                   ELUOVCFD
00588      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
00589      PERFORM LOAD-ABM-CMM-LIST                                    ELUOVCFD
00590          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00591            UNTIL WS-SUBA > WS-ABM-CMM-TBL-CNT.                    ELUOVCFD
00592                                                                   ELUOVCFD
00593 ************************************************************      ELUOVCFD
00594 *                                                          *      ELUOVCFD
00595 *        LOAD ABM CMM LIST                                 *      ELUOVCFD
00596 *                                                          *      ELUOVCFD
00597 ************************************************************      ELUOVCFD
00598                                                                   ELUOVCFD
00599  LOAD-ABM-CMM-LIST.                                               ELUOVCFD
00600      MOVE WS-ABM-CMM-ENTRY (WS-SUBA)                              ELUOVCFD
00601        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00602      IF PROVIDER-PLAN                                             ELUOVCFD
00603      THEN                                                         ELUOVCFD
00604         PERFORM SET-PLAN-SW                                       ELUOVCFD
00605      ELSE                                                         ELUOVCFD
00606         IF PROVIDER-NON-PLAN                                      ELUOVCFD
00607         THEN                                                      ELUOVCFD
00608            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
00609      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00610                                                                   ELUOVCFD
00611 ************************************************************      ELUOVCFD
00612 *                                                          *      ELUOVCFD
00613 *        SET PLAN SW                                       *      ELUOVCFD
00614 *                                                          *      ELUOVCFD
00615 ************************************************************      ELUOVCFD
00616                                                                   ELUOVCFD
00617  SET-PLAN-SW.                                                     ELUOVCFD
00618      SET RRBL-REGARD-PLAN (RRBL-X-IDX) TO TRUE.                   ELUOVCFD
00619                                                                   ELUOVCFD
00620 ************************************************************      ELUOVCFD
00621 *                                                          *      ELUOVCFD
00622 *        SET NON-PLAN SW                                   *      ELUOVCFD
00623 *                                                          *      ELUOVCFD
00624 ************************************************************      ELUOVCFD
00625                                                                   ELUOVCFD
00626  SET-NON-PLAN-SW.                                                 ELUOVCFD
00627      SET RRBL-REGARD-NON-PLAN (RRBL-X-IDX) TO TRUE.               ELUOVCFD
00628                                                                   ELUOVCFD
00629 ************************************************************      ELUOVCFD
00630 *                                                          *      ELUOVCFD
00631 *        LOAD ABM BASE PLUS RANKING REQUESTS               *      ELUOVCFD
00632 *                                                          *      ELUOVCFD
00633 ************************************************************      ELUOVCFD
00634                                                                   ELUOVCFD
00635  LOAD-ABM-BASE-PLUS-RANKING-REQ.                                  ELUOVCFD
00636      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
00637      IF LOAD-FOR-BC                                               ELUOVCFD
00638          PERFORM LOAD-ABM-BC-RANKING-REQUESTS.                    ELUOVCFD
00639      IF LOAD-FOR-BS                                               ELUOVCFD
00640          PERFORM LOAD-ABM-BS-RANKING-REQUESTS.                    ELUOVCFD
00641      IF LOAD-FOR-SMM                                              ELUOVCFD
00642          PERFORM LOAD-ABM-SMM-RANKING-REQUESTS.                   ELUOVCFD
00643                                                                   ELUOVCFD
00644 ************************************************************      ELUOVCFD
00645 *                                                          *      ELUOVCFD
00646 *        LOAD ABM BC RANKING REQUESTS                      *      ELUOVCFD
00647 *                                                          *      ELUOVCFD
00648 ************************************************************      ELUOVCFD
00649                                                                   ELUOVCFD
00650  LOAD-ABM-BC-RANKING-REQUESTS.                                    ELUOVCFD
00651      PERFORM LOAD-ABM-BC-LIST                                     ELUOVCFD
00652          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00653            UNTIL WS-SUBA > WS-ABM-BC-TBL-CNT.                     ELUOVCFD
00654                                                                   ELUOVCFD
00655 ************************************************************      ELUOVCFD
00656 *                                                          *      ELUOVCFD
00657 *        LOAD ABM BC LIST                                  *      ELUOVCFD
00658 *                                                          *      ELUOVCFD
00659 ************************************************************      ELUOVCFD
00660                                                                   ELUOVCFD
00661  LOAD-ABM-BC-LIST.                                                ELUOVCFD
00662      MOVE WS-ABM-BC-ENTRY (WS-SUBA)                               ELUOVCFD
00663        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00664      IF PROVIDER-PLAN                                             ELUOVCFD
00665      THEN                                                         ELUOVCFD
00666         PERFORM SET-PLAN-SW                                       ELUOVCFD
00667      ELSE                                                         ELUOVCFD
00668         IF PROVIDER-NON-PLAN                                      ELUOVCFD
00669         THEN                                                      ELUOVCFD
00670            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
00671      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00672                                                                   ELUOVCFD
00673 ************************************************************      ELUOVCFD
00674 *                                                          *      ELUOVCFD
00675 *        LOAD ABM BS RANKING REQUESTS                      *      ELUOVCFD
00676 *                                                          *      ELUOVCFD
00677 ************************************************************      ELUOVCFD
00678                                                                   ELUOVCFD
00679  LOAD-ABM-BS-RANKING-REQUESTS.                                    ELUOVCFD
00680      PERFORM LOAD-ABM-BS-LIST                                     ELUOVCFD
00681          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00682            UNTIL WS-SUBA > WS-ABM-BS-TBL-CNT.                     ELUOVCFD
00683                                                                   ELUOVCFD
00684 ************************************************************      ELUOVCFD
00685 *                                                          *      ELUOVCFD
00686 *        LOAD ABM BS LIST                                  *      ELUOVCFD
00687 *                                                          *      ELUOVCFD
00688 ************************************************************      ELUOVCFD
00689                                                                   ELUOVCFD
00690  LOAD-ABM-BS-LIST.                                                ELUOVCFD
00691      MOVE WS-ABM-BS-ENTRY (WS-SUBA)                               ELUOVCFD
00692        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00693      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00694                                                                   ELUOVCFD
00695 ************************************************************      ELUOVCFD
00696 *                                                          *      ELUOVCFD
00697 *        LOAD ABM SMM RANKING REQUESTS                     *      ELUOVCFD
00698 *                                                          *      ELUOVCFD
00699 ************************************************************      ELUOVCFD
00700                                                                   ELUOVCFD
00701  LOAD-ABM-SMM-RANKING-REQUESTS.                                   ELUOVCFD
00702      PERFORM LOAD-ABM-SMM-LIST                                    ELUOVCFD
00703          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00704            UNTIL WS-SUBA > WS-ABM-SMM-TBL-CNT.                    ELUOVCFD
00705                                                                   ELUOVCFD
00706 ************************************************************      ELUOVCFD
00707 *                                                          *      ELUOVCFD
00708 *        LOAD ABM SMM LIST                                 *      ELUOVCFD
00709 *                                                          *      ELUOVCFD
00710 ************************************************************      ELUOVCFD
00711                                                                   ELUOVCFD
00712  LOAD-ABM-SMM-LIST.                                               ELUOVCFD
00713      MOVE WS-ABM-SMM-ENTRY (WS-SUBA)                              ELUOVCFD
00714        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00715      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00716                                                                   ELUOVCFD
00717 ************************************************************      ELUOVCFD
00718 *                                                          *      ELUOVCFD
00719 *        CREATE RANKING REQUEST BLOCK FOR ACL              *      ELUOVCFD
00720 *                                                          *      ELUOVCFD
00721 ************************************************************      ELUOVCFD
00722                                                                   ELUOVCFD
00723  CREATE-RANKING-REQUEST-ACL.                                      ELUOVCFD
00724      SET PROCESSING-ACL TO TRUE.                                  ELUOVCFD
00725      IF LOAD-FOR-CMM                                              ELUOVCFD
00726      THEN                                                         ELUOVCFD
00727         PERFORM COMPUTE-ACL-CMM-LENGTH                            ELUOVCFD
00728      ELSE                                                         ELUOVCFD
00729         PERFORM COMPUTE-ACL-BASE-PLUS-LENGTH.                     ELUOVCFD
00730      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00731      PERFORM LINK-TO-MODULE-ELUSTGMG.                             ELUOVCFD
00732      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00733      CALL 'ELUSETAD'                                              ELUOVCFD
00734         USING DFHCOMMAREA                                         ELUOVCFD
00735         ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.                      ELUOVCFD
00736      SET CSAC-ACL-RR-PTR TO ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.  ELUOVCFD
00737      PERFORM LOAD-ACL-RANKING-REQUESTS.                           ELUOVCFD
00738                                                                   ELUOVCFD
00739 ************************************************************      ELUOVCFD
00740 *                                                          *      ELUOVCFD
00741 *        LOAD ACL RANKING REQUESTS                         *      ELUOVCFD
00742 *                                                          *      ELUOVCFD
00743 ************************************************************      ELUOVCFD
00744                                                                   ELUOVCFD
00745  LOAD-ACL-RANKING-REQUESTS.                                       ELUOVCFD
00746      IF LOAD-FOR-CMM                                              ELUOVCFD
00747      THEN                                                         ELUOVCFD
00748         PERFORM MOVE-ACL-CMM-TABLE-COUNT                          ELUOVCFD
00749      ELSE                                                         ELUOVCFD
00750         PERFORM MOVE-ACL-BASE-PLUS-TABLE-COUNT.                   ELUOVCFD
00751      SET RRBL-ACL TO TRUE.                                        ELUOVCFD
00752      IF LOAD-FOR-CMM                                              ELUOVCFD
00753      THEN                                                         ELUOVCFD
00754         PERFORM LOAD-ACL-CMM-RANKING-REQUESTS                     ELUOVCFD
00755      ELSE                                                         ELUOVCFD
00756         PERFORM LOAD-ACL-BASE-PLUS-RANKING-REQ.                   ELUOVCFD
00757                                                                   ELUOVCFD
00758 ************************************************************      ELUOVCFD
00759 *                                                          *      ELUOVCFD
00760 *        COMPUTE ACL CMM LENGTH                            *      ELUOVCFD
00761 *                                                          *      ELUOVCFD
00762 ************************************************************      ELUOVCFD
00763                                                                   ELUOVCFD
00764  COMPUTE-ACL-CMM-LENGTH.                                          ELUOVCFD
00765      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
00766           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
00767         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
00768         + (  WS-ACL-CMM-TBL-CNT                                   ELUOVCFD
00769            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
00770                                                                   ELUOVCFD
00771 ************************************************************      ELUOVCFD
00772 *                                                          *      ELUOVCFD
00773 *        COMPUTE ACL BASE PLUS LENGTH                      *      ELUOVCFD
00774 *                                                          *      ELUOVCFD
00775 ************************************************************      ELUOVCFD
00776                                                                   ELUOVCFD
00777  COMPUTE-ACL-BASE-PLUS-LENGTH.                                    ELUOVCFD
00778      IF LOAD-FOR-BC                                               ELUOVCFD
00779          PERFORM ADD-ACL-BC-LENGTH.                               ELUOVCFD
00780      IF LOAD-FOR-BS                                               ELUOVCFD
00781          PERFORM ADD-ACL-BS-LENGTH.                               ELUOVCFD
00782      IF LOAD-FOR-SMM                                              ELUOVCFD
00783          PERFORM ADD-ACL-SMM-LENGTH.                              ELUOVCFD
00784      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
00785           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
00786         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
00787         + (  WS-ACL-COMBINED-TTL                                  ELUOVCFD
00788            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
00789                                                                   ELUOVCFD
00790 ************************************************************      ELUOVCFD
00791 *                                                          *      ELUOVCFD
00792 *        ADD ACL BC LENGTH                                 *      ELUOVCFD
00793 *                                                          *      ELUOVCFD
00794 ************************************************************      ELUOVCFD
00795                                                                   ELUOVCFD
00796  ADD-ACL-BC-LENGTH.                                               ELUOVCFD
00797      ADD WS-ACL-BC-TBL-CNT TO WS-ACL-COMBINED-TTL.                ELUOVCFD
00798                                                                   ELUOVCFD
00799 ************************************************************      ELUOVCFD
00800 *                                                          *      ELUOVCFD
00801 *        ADD ACL BS LENGTH                                 *      ELUOVCFD
00802 *                                                          *      ELUOVCFD
00803 ************************************************************      ELUOVCFD
00804                                                                   ELUOVCFD
00805  ADD-ACL-BS-LENGTH.                                               ELUOVCFD
00806      ADD WS-ACL-BS-TBL-CNT TO WS-ACL-COMBINED-TTL.                ELUOVCFD
00807                                                                   ELUOVCFD
00808 ************************************************************      ELUOVCFD
00809 *                                                          *      ELUOVCFD
00810 *        ADD ACL SMM LENGTH                                *      ELUOVCFD
00811 *                                                          *      ELUOVCFD
00812 ************************************************************      ELUOVCFD
00813                                                                   ELUOVCFD
00814  ADD-ACL-SMM-LENGTH.                                              ELUOVCFD
00815      ADD WS-ACL-SMM-TBL-CNT TO WS-ACL-COMBINED-TTL.               ELUOVCFD
00816                                                                   ELUOVCFD
00817 ************************************************************      ELUOVCFD
00818 *                                                          *      ELUOVCFD
00819 *        MOVE ACL CMM TABLE COUNT                          *      ELUOVCFD
00820 *                                                          *      ELUOVCFD
00821 ************************************************************      ELUOVCFD
00822                                                                   ELUOVCFD
00823  MOVE-ACL-CMM-TABLE-COUNT.                                        ELUOVCFD
00824      MOVE WS-ACL-CMM-TBL-CNT TO RRBL-TBL-CNT.                     ELUOVCFD
00825                                                                   ELUOVCFD
00826 ************************************************************      ELUOVCFD
00827 *                                                          *      ELUOVCFD
00828 *        MOVE ACL BASE PLUS TABLE COUNT                    *      ELUOVCFD
00829 *                                                          *      ELUOVCFD
00830 ************************************************************      ELUOVCFD
00831                                                                   ELUOVCFD
00832  MOVE-ACL-BASE-PLUS-TABLE-COUNT.                                  ELUOVCFD
00833      MOVE WS-ACL-COMBINED-TTL TO RRBL-TBL-CNT.                    ELUOVCFD
00834                                                                   ELUOVCFD
00835 ************************************************************      ELUOVCFD
00836 *                                                          *      ELUOVCFD
00837 *        LOAD ACL CMM RANKING REQUESTS                     *      ELUOVCFD
00838 *                                                          *      ELUOVCFD
00839 ************************************************************      ELUOVCFD
00840                                                                   ELUOVCFD
00841  LOAD-ACL-CMM-RANKING-REQUESTS.                                   ELUOVCFD
00842      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
00843      PERFORM LOAD-ACL-CMM-LIST                                    ELUOVCFD
00844          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00845            UNTIL WS-SUBA > WS-ACL-CMM-TBL-CNT.                    ELUOVCFD
00846                                                                   ELUOVCFD
00847 ************************************************************      ELUOVCFD
00848 *                                                          *      ELUOVCFD
00849 *        LOAD ACL CMM LIST                                 *      ELUOVCFD
00850 *                                                          *      ELUOVCFD
00851 ************************************************************      ELUOVCFD
00852                                                                   ELUOVCFD
00853  LOAD-ACL-CMM-LIST.                                               ELUOVCFD
00854      MOVE WS-ACL-CMM-ENTRY (WS-SUBA)                              ELUOVCFD
00855        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00856      IF PROVIDER-PLAN                                             ELUOVCFD
00857      THEN                                                         ELUOVCFD
00858         PERFORM SET-PLAN-SW                                       ELUOVCFD
00859      ELSE                                                         ELUOVCFD
00860         IF PROVIDER-NON-PLAN                                      ELUOVCFD
00861         THEN                                                      ELUOVCFD
00862            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
00863      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00864                                                                   ELUOVCFD
00865 ************************************************************      ELUOVCFD
00866 *                                                          *      ELUOVCFD
00867 *        LOAD ACL BASE PLUS RANKING REQUESTS               *      ELUOVCFD
00868 *                                                          *      ELUOVCFD
00869 ************************************************************      ELUOVCFD
00870                                                                   ELUOVCFD
00871  LOAD-ACL-BASE-PLUS-RANKING-REQ.                                  ELUOVCFD
00872      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
00873      IF LOAD-FOR-BC                                               ELUOVCFD
00874          PERFORM LOAD-ACL-BC-RANKING-REQUESTS.                    ELUOVCFD
00875      IF LOAD-FOR-BS                                               ELUOVCFD
00876          PERFORM LOAD-ACL-BS-RANKING-REQUESTS.                    ELUOVCFD
00877      IF LOAD-FOR-SMM                                              ELUOVCFD
00878          PERFORM LOAD-ACL-SMM-RANKING-REQUESTS.                   ELUOVCFD
00879                                                                   ELUOVCFD
00880 ************************************************************      ELUOVCFD
00881 *                                                          *      ELUOVCFD
00882 *        LOAD ACL BC RANKING REQUESTS                      *      ELUOVCFD
00883 *                                                          *      ELUOVCFD
00884 ************************************************************      ELUOVCFD
00885                                                                   ELUOVCFD
00886  LOAD-ACL-BC-RANKING-REQUESTS.                                    ELUOVCFD
00887      PERFORM LOAD-ACL-BC-LIST                                     ELUOVCFD
00888          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00889            UNTIL WS-SUBA > WS-ACL-BC-TBL-CNT.                     ELUOVCFD
00890                                                                   ELUOVCFD
00891 ************************************************************      ELUOVCFD
00892 *                                                          *      ELUOVCFD
00893 *        LOAD ACL BC LIST                                  *      ELUOVCFD
00894 *                                                          *      ELUOVCFD
00895 ************************************************************      ELUOVCFD
00896                                                                   ELUOVCFD
00897  LOAD-ACL-BC-LIST.                                                ELUOVCFD
00898      MOVE WS-ACL-BC-ENTRY (WS-SUBA)                               ELUOVCFD
00899        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00900      IF PROVIDER-PLAN                                             ELUOVCFD
00901      THEN                                                         ELUOVCFD
00902         PERFORM SET-PLAN-SW                                       ELUOVCFD
00903      ELSE                                                         ELUOVCFD
00904         IF PROVIDER-NON-PLAN                                      ELUOVCFD
00905         THEN                                                      ELUOVCFD
00906            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
00907      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00908                                                                   ELUOVCFD
00909 ************************************************************      ELUOVCFD
00910 *                                                          *      ELUOVCFD
00911 *        LOAD ACL BS RANKING REQUESTS                      *      ELUOVCFD
00912 *                                                          *      ELUOVCFD
00913 ************************************************************      ELUOVCFD
00914                                                                   ELUOVCFD
00915  LOAD-ACL-BS-RANKING-REQUESTS.                                    ELUOVCFD
00916      PERFORM LOAD-ACL-BS-LIST                                     ELUOVCFD
00917          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00918            UNTIL WS-SUBA > WS-ACL-BS-TBL-CNT.                     ELUOVCFD
00919                                                                   ELUOVCFD
00920 ************************************************************      ELUOVCFD
00921 *                                                          *      ELUOVCFD
00922 *        LOAD ACL BS LIST                                  *      ELUOVCFD
00923 *                                                          *      ELUOVCFD
00924 ************************************************************      ELUOVCFD
00925                                                                   ELUOVCFD
00926  LOAD-ACL-BS-LIST.                                                ELUOVCFD
00927      MOVE WS-ACL-BS-ENTRY (WS-SUBA)                               ELUOVCFD
00928        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00929      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00930                                                                   ELUOVCFD
00931 ************************************************************      ELUOVCFD
00932 *                                                          *      ELUOVCFD
00933 *        LOAD ACL SMM RANKING REQUESTS                     *      ELUOVCFD
00934 *                                                          *      ELUOVCFD
00935 ************************************************************      ELUOVCFD
00936                                                                   ELUOVCFD
00937  LOAD-ACL-SMM-RANKING-REQUESTS.                                   ELUOVCFD
00938      PERFORM LOAD-ACL-SMM-LIST                                    ELUOVCFD
00939          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
00940            UNTIL WS-SUBA > WS-ACL-SMM-TBL-CNT.                    ELUOVCFD
00941                                                                   ELUOVCFD
00942 ************************************************************      ELUOVCFD
00943 *                                                          *      ELUOVCFD
00944 *        LOAD ACL SMM LIST                                 *      ELUOVCFD
00945 *                                                          *      ELUOVCFD
00946 ************************************************************      ELUOVCFD
00947                                                                   ELUOVCFD
00948  LOAD-ACL-SMM-LIST.                                               ELUOVCFD
00949      MOVE WS-ACL-SMM-ENTRY (WS-SUBA)                              ELUOVCFD
00950        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
00951      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
00952                                                                   ELUOVCFD
00953 ************************************************************      ELUOVCFD
00954 *                                                          *      ELUOVCFD
00955 *        CREATE RANKING REQUEST BLOCK FOR ADL              *      ELUOVCFD
00956 *                                                          *      ELUOVCFD
00957 ************************************************************      ELUOVCFD
00958                                                                   ELUOVCFD
00959  CREATE-RANKING-REQUEST-ADL.                                      ELUOVCFD
00960      SET PROCESSING-ADL TO TRUE.                                  ELUOVCFD
00961      IF LOAD-FOR-CMM                                              ELUOVCFD
00962      THEN                                                         ELUOVCFD
00963         PERFORM COMPUTE-ADL-CMM-LENGTH                            ELUOVCFD
00964      ELSE                                                         ELUOVCFD
00965         PERFORM COMPUTE-ADL-BASE-PLUS-LENGTH.                     ELUOVCFD
00966      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00967      PERFORM LINK-TO-MODULE-ELUSTGMG.                             ELUOVCFD
00968      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
00969      CALL 'ELUSETAD'                                              ELUOVCFD
00970         USING DFHCOMMAREA                                         ELUOVCFD
00971               ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.                ELUOVCFD
00972      SET CSAC-ADL-RR-PTR TO ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.  ELUOVCFD
00973      PERFORM LOAD-ADL-RANKING-REQUESTS.                           ELUOVCFD
00974                                                                   ELUOVCFD
00975 ************************************************************      ELUOVCFD
00976 *                                                          *      ELUOVCFD
00977 *        LOAD ADL RANKING REQUESTS                         *      ELUOVCFD
00978 *                                                          *      ELUOVCFD
00979 ************************************************************      ELUOVCFD
00980                                                                   ELUOVCFD
00981  LOAD-ADL-RANKING-REQUESTS.                                       ELUOVCFD
00982      IF LOAD-FOR-CMM                                              ELUOVCFD
00983      THEN                                                         ELUOVCFD
00984         PERFORM MOVE-ADL-CMM-TABLE-COUNT                          ELUOVCFD
00985      ELSE                                                         ELUOVCFD
00986         PERFORM MOVE-ADL-BASE-PLUS-TABLE-COUNT.                   ELUOVCFD
00987      SET RRBL-ADL TO TRUE.                                        ELUOVCFD
00988      IF LOAD-FOR-CMM                                              ELUOVCFD
00989      THEN                                                         ELUOVCFD
00990         PERFORM LOAD-ADL-CMM-RANKING-REQUESTS                     ELUOVCFD
00991      ELSE                                                         ELUOVCFD
00992         PERFORM LOAD-ADL-BASE-PLUS-RANKING-REQ.                   ELUOVCFD
00993                                                                   ELUOVCFD
00994 ************************************************************      ELUOVCFD
00995 *                                                          *      ELUOVCFD
00996 *        COMPUTE ADL CMM LENGTH                            *      ELUOVCFD
00997 *                                                          *      ELUOVCFD
00998 ************************************************************      ELUOVCFD
00999                                                                   ELUOVCFD
01000  COMPUTE-ADL-CMM-LENGTH.                                          ELUOVCFD
01001      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
01002           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
01003         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
01004         + (  WS-ADL-CMM-TBL-CNT                                   ELUOVCFD
01005            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
01006                                                                   ELUOVCFD
01007 ************************************************************      ELUOVCFD
01008 *                                                          *      ELUOVCFD
01009 *        COMPUTE ADL BASE PLUS LENGTH                      *      ELUOVCFD
01010 *                                                          *      ELUOVCFD
01011 ************************************************************      ELUOVCFD
01012                                                                   ELUOVCFD
01013  COMPUTE-ADL-BASE-PLUS-LENGTH.                                    ELUOVCFD
01014      IF LOAD-FOR-BC                                               ELUOVCFD
01015          PERFORM ADD-ADL-BC-LENGTH.                               ELUOVCFD
01016      IF LOAD-FOR-BS                                               ELUOVCFD
01017          PERFORM ADD-ADL-BS-LENGTH.                               ELUOVCFD
01018      IF LOAD-FOR-SMM                                              ELUOVCFD
01019          PERFORM ADD-ADL-SMM-LENGTH.                              ELUOVCFD
01020      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
01021           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
01022         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
01023         + (  WS-ADL-COMBINED-TTL                                  ELUOVCFD
01024            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
01025                                                                   ELUOVCFD
01026 ************************************************************      ELUOVCFD
01027 *                                                          *      ELUOVCFD
01028 *        ADD ADL BC LENGTH                                 *      ELUOVCFD
01029 *                                                          *      ELUOVCFD
01030 ************************************************************      ELUOVCFD
01031                                                                   ELUOVCFD
01032  ADD-ADL-BC-LENGTH.                                               ELUOVCFD
01033      ADD WS-ADL-BC-TBL-CNT TO WS-ADL-COMBINED-TTL.                ELUOVCFD
01034                                                                   ELUOVCFD
01035 ************************************************************      ELUOVCFD
01036 *                                                          *      ELUOVCFD
01037 *        ADD ADL BS LENGTH                                 *      ELUOVCFD
01038 *                                                          *      ELUOVCFD
01039 ************************************************************      ELUOVCFD
01040                                                                   ELUOVCFD
01041  ADD-ADL-BS-LENGTH.                                               ELUOVCFD
01042      ADD WS-ADL-BS-TBL-CNT TO WS-ADL-COMBINED-TTL.                ELUOVCFD
01043                                                                   ELUOVCFD
01044 ************************************************************      ELUOVCFD
01045 *                                                          *      ELUOVCFD
01046 *        ADD ADL SMM LENGTH                                *      ELUOVCFD
01047 *                                                          *      ELUOVCFD
01048 ************************************************************      ELUOVCFD
01049                                                                   ELUOVCFD
01050  ADD-ADL-SMM-LENGTH.                                              ELUOVCFD
01051      ADD WS-ADL-SMM-TBL-CNT TO WS-ADL-COMBINED-TTL.               ELUOVCFD
01052                                                                   ELUOVCFD
01053 ************************************************************      ELUOVCFD
01054 *                                                          *      ELUOVCFD
01055 *        MOVE ADL CMM TABLE COUNT                          *      ELUOVCFD
01056 *                                                          *      ELUOVCFD
01057 ************************************************************      ELUOVCFD
01058                                                                   ELUOVCFD
01059  MOVE-ADL-CMM-TABLE-COUNT.                                        ELUOVCFD
01060      MOVE WS-ADL-CMM-TBL-CNT TO RRBL-TBL-CNT.                     ELUOVCFD
01061                                                                   ELUOVCFD
01062 ************************************************************      ELUOVCFD
01063 *                                                          *      ELUOVCFD
01064 *        MOVE ADL BASE PLUS TABLE COUNT                    *      ELUOVCFD
01065 *                                                          *      ELUOVCFD
01066 ************************************************************      ELUOVCFD
01067                                                                   ELUOVCFD
01068  MOVE-ADL-BASE-PLUS-TABLE-COUNT.                                  ELUOVCFD
01069      MOVE WS-ADL-COMBINED-TTL TO RRBL-TBL-CNT.                    ELUOVCFD
01070                                                                   ELUOVCFD
01071 ************************************************************      ELUOVCFD
01072 *                                                          *      ELUOVCFD
01073 *        LOAD ADL CMM RANKING REQUESTS                     *      ELUOVCFD
01074 *                                                          *      ELUOVCFD
01075 ************************************************************      ELUOVCFD
01076                                                                   ELUOVCFD
01077  LOAD-ADL-CMM-RANKING-REQUESTS.                                   ELUOVCFD
01078      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
01079      PERFORM LOAD-ADL-CMM-LIST                                    ELUOVCFD
01080          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
01081            UNTIL WS-SUBA > WS-ADL-CMM-TBL-CNT.                    ELUOVCFD
01082                                                                   ELUOVCFD
01083 ************************************************************      ELUOVCFD
01084 *                                                          *      ELUOVCFD
01085 *        LOAD ADL CMM LIST                                 *      ELUOVCFD
01086 *                                                          *      ELUOVCFD
01087 ************************************************************      ELUOVCFD
01088                                                                   ELUOVCFD
01089  LOAD-ADL-CMM-LIST.                                               ELUOVCFD
01090      MOVE WS-ADL-CMM-ENTRY (WS-SUBA)                              ELUOVCFD
01091        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01092      IF PROVIDER-PLAN                                             ELUOVCFD
01093      THEN                                                         ELUOVCFD
01094         PERFORM SET-PLAN-SW                                       ELUOVCFD
01095      ELSE                                                         ELUOVCFD
01096         IF PROVIDER-NON-PLAN                                      ELUOVCFD
01097         THEN                                                      ELUOVCFD
01098            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
01099      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01100                                                                   ELUOVCFD
01101 ************************************************************      ELUOVCFD
01102 *                                                          *      ELUOVCFD
01103 *        LOAD ADL BASE PLUS RANKING REQUESTS               *      ELUOVCFD
01104 *                                                          *      ELUOVCFD
01105 ************************************************************      ELUOVCFD
01106                                                                   ELUOVCFD
01107  LOAD-ADL-BASE-PLUS-RANKING-REQ.                                  ELUOVCFD
01108      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
01109      IF LOAD-FOR-BC                                               ELUOVCFD
01110          PERFORM LOAD-ADL-BC-RANKING-REQUESTS.                    ELUOVCFD
01111      IF LOAD-FOR-BS                                               ELUOVCFD
01112          PERFORM LOAD-ADL-BS-RANKING-REQUESTS.                    ELUOVCFD
01113      IF LOAD-FOR-SMM                                              ELUOVCFD
01114          PERFORM LOAD-ADL-SMM-RANKING-REQUESTS.                   ELUOVCFD
01115                                                                   ELUOVCFD
01116 ************************************************************      ELUOVCFD
01117 *                                                          *      ELUOVCFD
01118 *        LOAD ADL BC RANKING REQUESTS                      *      ELUOVCFD
01119 *                                                          *      ELUOVCFD
01120 ************************************************************      ELUOVCFD
01121                                                                   ELUOVCFD
01122  LOAD-ADL-BC-RANKING-REQUESTS.                                    ELUOVCFD
01123      PERFORM LOAD-ADL-BC-LIST                                     ELUOVCFD
01124          VARYING WS-SUBA FROM 1 BY 1                              ELUOVCFD
01125            UNTIL WS-SUBA > WS-ADL-BC-TBL-CNT.                     ELUOVCFD
01126                                                                   ELUOVCFD
01127 ************************************************************      ELUOVCFD
01128 *                                                          *      ELUOVCFD
01129 *        LOAD ADL BC LIST                                  *      ELUOVCFD
01130 *                                                          *      ELUOVCFD
01131 ************************************************************      ELUOVCFD
01132                                                                   ELUOVCFD
01133  LOAD-ADL-BC-LIST.                                                ELUOVCFD
01134      MOVE WS-ADL-BC-ENTRY (WS-SUBA)                               ELUOVCFD
01135        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01136      IF PROVIDER-PLAN                                             ELUOVCFD
01137      THEN                                                         ELUOVCFD
01138         PERFORM SET-PLAN-SW                                       ELUOVCFD
01139      ELSE                                                         ELUOVCFD
01140         IF PROVIDER-NON-PLAN                                      ELUOVCFD
01141         THEN                                                      ELUOVCFD
01142            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
01143      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01144                                                                   ELUOVCFD
01145 ************************************************************      ELUOVCFD
01146 *                                                          *      ELUOVCFD
01147 *        LOAD ADL BS RANKING REQUESTS                      *      ELUOVCFD
01148 *                                                          *      ELUOVCFD
01149 ************************************************************      ELUOVCFD
01150                                                                   ELUOVCFD
01151  LOAD-ADL-BS-RANKING-REQUESTS.                                    ELUOVCFD
01152      PERFORM LOAD-ADL-BS-LIST                                     ELUOVCFD
01153         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01154           UNTIL WS-SUBA > WS-ADL-BS-TBL-CNT.                      ELUOVCFD
01155                                                                   ELUOVCFD
01156 ************************************************************      ELUOVCFD
01157 *                                                          *      ELUOVCFD
01158 *        LOAD ADL BS LIST                                  *      ELUOVCFD
01159 *                                                          *      ELUOVCFD
01160 ************************************************************      ELUOVCFD
01161                                                                   ELUOVCFD
01162  LOAD-ADL-BS-LIST.                                                ELUOVCFD
01163      MOVE WS-ADL-BS-ENTRY (WS-SUBA)                               ELUOVCFD
01164        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01165      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01166                                                                   ELUOVCFD
01167 ************************************************************      ELUOVCFD
01168 *                                                          *      ELUOVCFD
01169 *        LOAD ADL SMM RANKING REQUESTS                     *      ELUOVCFD
01170 *                                                          *      ELUOVCFD
01171 ************************************************************      ELUOVCFD
01172                                                                   ELUOVCFD
01173  LOAD-ADL-SMM-RANKING-REQUESTS.                                   ELUOVCFD
01174      PERFORM LOAD-ADL-SMM-LIST                                    ELUOVCFD
01175         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01176           UNTIL WS-SUBA > WS-ADL-SMM-TBL-CNT.                     ELUOVCFD
01177                                                                   ELUOVCFD
01178 ************************************************************      ELUOVCFD
01179 *                                                          *      ELUOVCFD
01180 *        LOAD ADL SMM LIST                                 *      ELUOVCFD
01181 *                                                          *      ELUOVCFD
01182 ************************************************************      ELUOVCFD
01183                                                                   ELUOVCFD
01184  LOAD-ADL-SMM-LIST.                                               ELUOVCFD
01185      MOVE WS-ADL-SMM-ENTRY (WS-SUBA)                              ELUOVCFD
01186        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01187      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01188                                                                   ELUOVCFD
01189 ************************************************************      ELUOVCFD
01190 *                                                          *      ELUOVCFD
01191 *        CREATE RANKING REQUEST BLOCK FOR AOL              *      ELUOVCFD
01192 *                                                          *      ELUOVCFD
01193 ************************************************************      ELUOVCFD
01194                                                                   ELUOVCFD
01195  CREATE-RANKING-REQUEST-AOL.                                      ELUOVCFD
01196      SET PROCESSING-AOL TO TRUE.                                  ELUOVCFD
01197      IF LOAD-FOR-CMM                                              ELUOVCFD
01198      THEN                                                         ELUOVCFD
01199         PERFORM COMPUTE-AOL-CMM-LENGTH                            ELUOVCFD
01200      ELSE                                                         ELUOVCFD
01201         PERFORM COMPUTE-AOL-BASE-PLUS-LENGTH.                     ELUOVCFD
01202      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
01203      PERFORM LINK-TO-MODULE-ELUSTGMG.                             ELUOVCFD
01204      SET CIA-ELSRRBL-DDN TO TRUE.                                 ELUOVCFD
01205      CALL 'ELUSETAD'                                              ELUOVCFD
01206         USING DFHCOMMAREA                                         ELUOVCFD
01207               ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.                ELUOVCFD
01208      SET CSAC-AOL-RR-PTR TO ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST.  ELUOVCFD
01209      PERFORM LOAD-AOL-RANKING-REQUESTS.                           ELUOVCFD
01210                                                                   ELUOVCFD
01211 ************************************************************      ELUOVCFD
01212 *                                                          *      ELUOVCFD
01213 *        LOAD AOL RANKING REQUESTS                         *      ELUOVCFD
01214 *                                                          *      ELUOVCFD
01215 ************************************************************      ELUOVCFD
01216                                                                   ELUOVCFD
01217  LOAD-AOL-RANKING-REQUESTS.                                       ELUOVCFD
01218      IF LOAD-FOR-CMM                                              ELUOVCFD
01219      THEN                                                         ELUOVCFD
01220         PERFORM MOVE-AOL-CMM-TABLE-COUNT                          ELUOVCFD
01221      ELSE                                                         ELUOVCFD
01222         PERFORM MOVE-AOL-BASE-PLUS-TABLE-COUNT.                   ELUOVCFD
01223      SET RRBL-AOL TO TRUE.                                        ELUOVCFD
01224      IF LOAD-FOR-CMM                                              ELUOVCFD
01225      THEN                                                         ELUOVCFD
01226         PERFORM LOAD-AOL-CMM-RANKING-REQUESTS                     ELUOVCFD
01227      ELSE                                                         ELUOVCFD
01228         PERFORM LOAD-AOL-BASE-PLUS-RANKING-REQ.                   ELUOVCFD
01229                                                                   ELUOVCFD
01230 ************************************************************      ELUOVCFD
01231 *                                                          *      ELUOVCFD
01232 *        COMPUTE AOL CMM LENGTH                            *      ELUOVCFD
01233 *                                                          *      ELUOVCFD
01234 ************************************************************      ELUOVCFD
01235                                                                   ELUOVCFD
01236  COMPUTE-AOL-CMM-LENGTH.                                          ELUOVCFD
01237      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
01238           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
01239         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
01240         + (  WS-AOL-CMM-TBL-CNT                                   ELUOVCFD
01241            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
01242                                                                   ELUOVCFD
01243 ************************************************************      ELUOVCFD
01244 *                                                          *      ELUOVCFD
01245 *        COMPUTE AOL BASE PLUS LENGTH                      *      ELUOVCFD
01246 *                                                          *      ELUOVCFD
01247 ************************************************************      ELUOVCFD
01248                                                                   ELUOVCFD
01249  COMPUTE-AOL-BASE-PLUS-LENGTH.                                    ELUOVCFD
01250      IF LOAD-FOR-BC                                               ELUOVCFD
01251         ADD WS-AOL-BC-TBL-CNT TO WS-AOL-COMBINED-TTL.             ELUOVCFD
01252      IF LOAD-FOR-BS                                               ELUOVCFD
01253         ADD WS-AOL-BS-TBL-CNT TO WS-AOL-COMBINED-TTL.             ELUOVCFD
01254      IF LOAD-FOR-SMM                                              ELUOVCFD
01255         ADD WS-AOL-SMM-TBL-CNT TO WS-AOL-COMBINED-TTL.            ELUOVCFD
01256      COMPUTE CIA-AREA-LEN =                                       ELUOVCFD
01257           LENGTH OF RRBL-ACCUMULATOR-TYPE                         ELUOVCFD
01258         + LENGTH OF RRBL-TBL-CNT                                  ELUOVCFD
01259         + (  WS-AOL-COMBINED-TTL                                  ELUOVCFD
01260            * LENGTH OF RRBL-RANK-REQ-BLOCK).                      ELUOVCFD
01261                                                                   ELUOVCFD
01262 ************************************************************      ELUOVCFD
01263 *                                                          *      ELUOVCFD
01264 *        MOVE AOL CMM TABLE COUNT                          *      ELUOVCFD
01265 *                                                          *      ELUOVCFD
01266 ************************************************************      ELUOVCFD
01267                                                                   ELUOVCFD
01268  MOVE-AOL-CMM-TABLE-COUNT.                                        ELUOVCFD
01269      MOVE WS-AOL-CMM-TBL-CNT TO RRBL-TBL-CNT.                     ELUOVCFD
01270                                                                   ELUOVCFD
01271 ************************************************************      ELUOVCFD
01272 *                                                          *      ELUOVCFD
01273 *        MOVE AOL BASE PLUS TABLE COUNT                    *      ELUOVCFD
01274 *                                                          *      ELUOVCFD
01275 ************************************************************      ELUOVCFD
01276                                                                   ELUOVCFD
01277  MOVE-AOL-BASE-PLUS-TABLE-COUNT.                                  ELUOVCFD
01278      MOVE WS-AOL-COMBINED-TTL TO RRBL-TBL-CNT.                    ELUOVCFD
01279                                                                   ELUOVCFD
01280 ************************************************************      ELUOVCFD
01281 *                                                          *      ELUOVCFD
01282 *        LOAD AOL CMM RANKING REQUESTS                     *      ELUOVCFD
01283 *                                                          *      ELUOVCFD
01284 ************************************************************      ELUOVCFD
01285                                                                   ELUOVCFD
01286  LOAD-AOL-CMM-RANKING-REQUESTS.                                   ELUOVCFD
01287      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
01288      PERFORM LOAD-AOL-CMM-LIST                                    ELUOVCFD
01289         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01290           UNTIL WS-SUBA > WS-AOL-CMM-TBL-CNT.                     ELUOVCFD
01291                                                                   ELUOVCFD
01292 ************************************************************      ELUOVCFD
01293 *                                                          *      ELUOVCFD
01294 *        LOAD AOL CMM LIST                                 *      ELUOVCFD
01295 *                                                          *      ELUOVCFD
01296 ************************************************************      ELUOVCFD
01297                                                                   ELUOVCFD
01298  LOAD-AOL-CMM-LIST.                                               ELUOVCFD
01299      MOVE WS-AOL-CMM-ENTRY (WS-SUBA)                              ELUOVCFD
01300        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01301      IF PROVIDER-PLAN                                             ELUOVCFD
01302      THEN                                                         ELUOVCFD
01303         PERFORM SET-PLAN-SW                                       ELUOVCFD
01304      ELSE                                                         ELUOVCFD
01305         IF PROVIDER-NON-PLAN                                      ELUOVCFD
01306         THEN                                                      ELUOVCFD
01307            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
01308      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01309                                                                   ELUOVCFD
01310 ************************************************************      ELUOVCFD
01311 *                                                          *      ELUOVCFD
01312 *        LOAD AOL BASE PLUS RANKING REQUESTS               *      ELUOVCFD
01313 *                                                          *      ELUOVCFD
01314 ************************************************************      ELUOVCFD
01315                                                                   ELUOVCFD
01316  LOAD-AOL-BASE-PLUS-RANKING-REQ.                                  ELUOVCFD
01317      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
01318      IF LOAD-FOR-BC                                               ELUOVCFD
01319          PERFORM LOAD-AOL-BC-RANKING-REQUESTS.                    ELUOVCFD
01320      IF LOAD-FOR-BS                                               ELUOVCFD
01321          PERFORM LOAD-AOL-BS-RANKING-REQUESTS.                    ELUOVCFD
01322      IF LOAD-FOR-SMM                                              ELUOVCFD
01323          PERFORM LOAD-AOL-SMM-RANKING-REQUESTS.                   ELUOVCFD
01324                                                                   ELUOVCFD
01325 ************************************************************      ELUOVCFD
01326 *                                                          *      ELUOVCFD
01327 *        LOAD AOL BC RANKING REQUESTS                      *      ELUOVCFD
01328 *                                                          *      ELUOVCFD
01329 ************************************************************      ELUOVCFD
01330                                                                   ELUOVCFD
01331  LOAD-AOL-BC-RANKING-REQUESTS.                                    ELUOVCFD
01332      PERFORM LOAD-AOL-BC-LIST                                     ELUOVCFD
01333         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01334           UNTIL WS-SUBA > WS-AOL-BC-TBL-CNT.                      ELUOVCFD
01335                                                                   ELUOVCFD
01336 ************************************************************      ELUOVCFD
01337 *                                                          *      ELUOVCFD
01338 *        LOAD AOL BC LIST                                  *      ELUOVCFD
01339 *                                                          *      ELUOVCFD
01340 ************************************************************      ELUOVCFD
01341                                                                   ELUOVCFD
01342  LOAD-AOL-BC-LIST.                                                ELUOVCFD
01343      MOVE WS-AOL-BC-ENTRY (WS-SUBA)                               ELUOVCFD
01344        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01345      IF PROVIDER-PLAN                                             ELUOVCFD
01346      THEN                                                         ELUOVCFD
01347         PERFORM SET-PLAN-SW                                       ELUOVCFD
01348      ELSE                                                         ELUOVCFD
01349         IF PROVIDER-NON-PLAN                                      ELUOVCFD
01350         THEN                                                      ELUOVCFD
01351            PERFORM SET-NON-PLAN-SW.                               ELUOVCFD
01352      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01353                                                                   ELUOVCFD
01354 ************************************************************      ELUOVCFD
01355 *                                                          *      ELUOVCFD
01356 *        LOAD AOL BS RANKING REQUESTS                      *      ELUOVCFD
01357 *                                                          *      ELUOVCFD
01358 ************************************************************      ELUOVCFD
01359                                                                   ELUOVCFD
01360  LOAD-AOL-BS-RANKING-REQUESTS.                                    ELUOVCFD
01361      PERFORM LOAD-AOL-BS-LIST                                     ELUOVCFD
01362         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01363           UNTIL WS-SUBA > WS-AOL-BS-TBL-CNT.                      ELUOVCFD
01364                                                                   ELUOVCFD
01365 ************************************************************      ELUOVCFD
01366 *                                                          *      ELUOVCFD
01367 *        LOAD AOL BS LIST                                  *      ELUOVCFD
01368 *                                                          *      ELUOVCFD
01369 ************************************************************      ELUOVCFD
01370                                                                   ELUOVCFD
01371  LOAD-AOL-BS-LIST.                                                ELUOVCFD
01372      MOVE WS-AOL-BS-ENTRY (WS-SUBA)                               ELUOVCFD
01373        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01374      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01375                                                                   ELUOVCFD
01376 ************************************************************      ELUOVCFD
01377 *                                                          *      ELUOVCFD
01378 *        LOAD AOL SMM RANKING REQUESTS                     *      ELUOVCFD
01379 *                                                          *      ELUOVCFD
01380 ************************************************************      ELUOVCFD
01381                                                                   ELUOVCFD
01382  LOAD-AOL-SMM-RANKING-REQUESTS.                                   ELUOVCFD
01383      PERFORM LOAD-AOL-SMM-LIST                                    ELUOVCFD
01384         VARYING WS-SUBA FROM 1 BY 1                               ELUOVCFD
01385           UNTIL WS-SUBA > WS-AOL-SMM-TBL-CNT.                     ELUOVCFD
01386                                                                   ELUOVCFD
01387 ************************************************************      ELUOVCFD
01388 *                                                          *      ELUOVCFD
01389 *        LOAD AOL SMM LIST                                 *      ELUOVCFD
01390 *                                                          *      ELUOVCFD
01391 ************************************************************      ELUOVCFD
01392                                                                   ELUOVCFD
01393  LOAD-AOL-SMM-LIST.                                               ELUOVCFD
01394      MOVE WS-AOL-SMM-ENTRY (WS-SUBA)                              ELUOVCFD
01395        TO RRBL-ATTR-SELECTION-PARMS (RRBL-X-IDX).                 ELUOVCFD
01396      SET RRBL-X-IDX UP BY 1.                                      ELUOVCFD
01397                                                                   ELUOVCFD
01398 ************************************************************      ELUOVCFD
01399 *                                                          *      ELUOVCFD
01400 *        LINK TO MODULE ELUSTGMG                           *      ELUOVCFD
01401 *                                                          *      ELUOVCFD
01402 ************************************************************      ELUOVCFD
01403                                                                   ELUOVCFD
01404  LINK-TO-MODULE-ELUSTGMG.                                         ELUOVCFD
01405      SET CIA-STG-GETMAIN TO TRUE.                                 ELUOVCFD
01406      EXEC CICS LINK PROGRAM('ELUSTGMG') COMMAREA(DFHCOMMAREA)     ELUOVCFD
01407                END-EXEC.                                          ELUOVCFD
01408                                                                   ELUOVCFD
01409 /***********************************************************      ELUOVCFD
01410 *                                                          *      ELUOVCFD
01411 *        ASSIGN CONFIDENCE FACTORS TO TABULARS             *      ELUOVCFD
01412 *                                                          *      ELUOVCFD
01413 ************************************************************      ELUOVCFD
01414                                                                   ELUOVCFD
01415  ASSIGN-CONFIDENCE-FACTORS.                                       ELUOVCFD
01416      PERFORM PROCESS-INTERNAL-TABULAR-TABLE.                      ELUOVCFD
01417      IF CSAC-ABM-RR-PTR NOT = NULLS                               ELUOVCFD
01418          PERFORM ASSIGN-CONFIDENCE-FACTORS-ABM.                   ELUOVCFD
01419      IF CSAC-ACL-RR-PTR NOT = NULLS                               ELUOVCFD
01420          PERFORM ASSIGN-CONFIDENCE-FACTORS-ACL.                   ELUOVCFD
01421      IF CSAC-ADL-RR-PTR NOT = NULLS                               ELUOVCFD
01422          PERFORM ASSIGN-CONFIDENCE-FACTORS-ADL.                   ELUOVCFD
01423      IF CSAC-AOL-RR-PTR NOT = NULLS                               ELUOVCFD
01424          PERFORM ASSIGN-CONFIDENCE-FACTORS-AOL.                   ELUOVCFD
01425                                                                   ELUOVCFD
01426 /***********************************************************      ELUOVCFD
01427 *                                                          *      ELUOVCFD
01428 *        PROCESS INTERNAL TABULAR TABLES                   *      ELUOVCFD
01429 *                                                          *      ELUOVCFD
01430 ************************************************************      ELUOVCFD
01431                                                                   ELUOVCFD
01432  PROCESS-INTERNAL-TABULAR-TABLE.                                  ELUOVCFD
01433      PERFORM ESTABLISH-ADDRESSABILITY-IBGR.                       ELUOVCFD
01434      PERFORM ESTABLISH-ADDRESSABILITY-IPGN.                       ELUOVCFD
01435      PERFORM ESTABLISH-ADDRESSABILITY-IPGT.                       ELUOVCFD
01436                                                                   ELUOVCFD
01437 ************************************************************      ELUOVCFD
01438 *                                                          *      ELUOVCFD
01439 *        ESTABLISH ADDRESSABILITY OF ELSIBGRC              *      ELUOVCFD
01440 *                                                          *      ELUOVCFD
01441 ************************************************************      ELUOVCFD
01442                                                                   ELUOVCFD
01443  ESTABLISH-ADDRESSABILITY-IBGR.                                   ELUOVCFD
01444      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUOVCFD
01445      CALL 'ELUSETAD'                                              ELUOVCFD
01446         USING DFHCOMMAREA                                         ELUOVCFD
01447               ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                ELUOVCFD
01448      IF NOT CIA-RC-PTR-NULL                                       ELUOVCFD
01449         EXEC CICS LINK PROGRAM('ELUIBGR') COMMAREA(DFHCOMMAREA)   ELUOVCFD
01450                   END-EXEC.                                       ELUOVCFD
01451      SET ELUIBGR-PROCESSED TO TRUE.                               ELUOVCFD
01452                                                                   ELUOVCFD
01453 ************************************************************      ELUOVCFD
01454 *                                                          *      ELUOVCFD
01455 *        ESTABLISH ADDRESSABILITY OF ELSIPGNC              *      ELUOVCFD
01456 *                                                          *      ELUOVCFD
01457 ************************************************************      ELUOVCFD
01458                                                                   ELUOVCFD
01459  ESTABLISH-ADDRESSABILITY-IPGN.                                   ELUOVCFD
01460      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELUOVCFD
01461      CALL 'ELUSETAD'                                              ELUOVCFD
01462          USING DFHCOMMAREA                                        ELUOVCFD
01463                ADDRESS OF IPGN-INTERNAL-TABS-TABLE.               ELUOVCFD
01464      IF NOT CIA-RC-PTR-NULL                                       ELUOVCFD
01465         EXEC CICS LINK PROGRAM('ELUIPGN') COMMAREA(DFHCOMMAREA)   ELUOVCFD
01466                  END-EXEC.                                        ELUOVCFD
01467      SET ELUIPGN-PROCESSED TO TRUE.                               ELUOVCFD
01468                                                                   ELUOVCFD
01469 ************************************************************      ELUOVCFD
01470 *                                                          *      ELUOVCFD
01471 *        ESTABLISH ADDRESSABILITY OF ELSIPGTC              *      ELUOVCFD
01472 *                                                          *      ELUOVCFD
01473 ************************************************************      ELUOVCFD
01474                                                                   ELUOVCFD
01475  ESTABLISH-ADDRESSABILITY-IPGT.                                   ELUOVCFD
01476      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELUOVCFD
01477      CALL 'ELUSETAD'                                              ELUOVCFD
01478         USING DFHCOMMAREA                                         ELUOVCFD
01479               ADDRESS OF IPGT-INTERNAL-TABS-TABLE.                ELUOVCFD
01480      IF NOT CIA-RC-PTR-NULL                                       ELUOVCFD
01481         EXEC CICS LINK PROGRAM('ELUIPGT') COMMAREA(DFHCOMMAREA)   ELUOVCFD
01482                  END-EXEC.                                        ELUOVCFD
01483      SET ELUIPGT-PROCESSED TO TRUE.                               ELUOVCFD
01484                                                                   ELUOVCFD
01485 /***********************************************************      ELUOVCFD
01486 *                                                          *      ELUOVCFD
01487 *        ASSIGN CONFIDENCE FACTORS FOR ABM                 *      ELUOVCFD
01488 *                                                          *      ELUOVCFD
01489 ************************************************************      ELUOVCFD
01490                                                                   ELUOVCFD
01491  ASSIGN-CONFIDENCE-FACTORS-ABM.                                   ELUOVCFD
01492      SET PROCESSING-ABM TO TRUE.                                  ELUOVCFD
01493      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ABM-GC-TBL-PTR.ELUOVCFD
01494      PERFORM PROCESS-ALL-ATBL-ENTRIES.                            ELUOVCFD
01495                                                                   ELUOVCFD
01496 ************************************************************      ELUOVCFD
01497 *                                                          *      ELUOVCFD
01498 *        ASSIGN CONFIDENCE FACTORS FOR ACL                 *      ELUOVCFD
01499 *                                                          *      ELUOVCFD
01500 ************************************************************      ELUOVCFD
01501                                                                   ELUOVCFD
01502  ASSIGN-CONFIDENCE-FACTORS-ACL.                                   ELUOVCFD
01503      SET PROCESSING-ACL TO TRUE.                                  ELUOVCFD
01504      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ACL-GC-TBL-PTR.ELUOVCFD
01505      PERFORM PROCESS-ALL-ATBL-ENTRIES.                            ELUOVCFD
01506                                                                   ELUOVCFD
01507 ************************************************************      ELUOVCFD
01508 *                                                          *      ELUOVCFD
01509 *        ASSIGN CONFIDENCE FACTORS FOR ADL                 *      ELUOVCFD
01510 *                                                          *      ELUOVCFD
01511 ************************************************************      ELUOVCFD
01512                                                                   ELUOVCFD
01513  ASSIGN-CONFIDENCE-FACTORS-ADL.                                   ELUOVCFD
01514      SET PROCESSING-ADL TO TRUE.                                  ELUOVCFD
01515      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ADL-GC-TBL-PTR.ELUOVCFD
01516      PERFORM PROCESS-ALL-ATBL-ENTRIES.                            ELUOVCFD
01517                                                                   ELUOVCFD
01518 ************************************************************      ELUOVCFD
01519 *                                                          *      ELUOVCFD
01520 *        ASSIGN CONFIDENCE FACTORS FOR AOL                 *      ELUOVCFD
01521 *                                                          *      ELUOVCFD
01522 ************************************************************      ELUOVCFD
01523                                                                   ELUOVCFD
01524  ASSIGN-CONFIDENCE-FACTORS-AOL.                                   ELUOVCFD
01525      SET PROCESSING-AOL TO TRUE.                                  ELUOVCFD
01526      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-AOL-GC-TBL-PTR.ELUOVCFD
01527      PERFORM PROCESS-ALL-ATBL-ENTRIES.                            ELUOVCFD
01528                                                                   ELUOVCFD
01529 /***********************************************************      ELUOVCFD
01530 *                                                          *      ELUOVCFD
01531 *        PROCESS ALL ATBL ENTRIES                          *      ELUOVCFD
01532 *                                                          *      ELUOVCFD
01533 ************************************************************      ELUOVCFD
01534                                                                   ELUOVCFD
01535  PROCESS-ALL-ATBL-ENTRIES.                                        ELUOVCFD
01536      PERFORM LOOP-THRU-ELSATBL                                    ELUOVCFD
01537         VARYING ATBL-X-IDX FROM 1 BY 1                            ELUOVCFD
01538           UNTIL ATBL-X-IDX > ATBL-TBL-CNT.                        ELUOVCFD
01539                                                                   ELUOVCFD
01540 ************************************************************      ELUOVCFD
01541 *                                                          *      ELUOVCFD
01542 *        LOOP THRU ELSATBL                                 *      ELUOVCFD
01543 *                                                          *      ELUOVCFD
01544 ************************************************************      ELUOVCFD
01545                                                                   ELUOVCFD
01546  LOOP-THRU-ELSATBL.                                               ELUOVCFD
01547      INITIALIZE WS-WORKAREAS                                      ELUOVCFD
01548                 WS-CONFIDENCE-FACTORS                             ELUOVCFD
01549                 WS-WEIGHTED-CONFIDENCE-FACTORS.                   ELUOVCFD
01550      PERFORM DET-CF-ALL-LOB.                                      ELUOVCFD
01551      PERFORM DET-CF-ALL-POT.                                      ELUOVCFD
01552      PERFORM DET-CF-ALL-IBGR.                                     ELUOVCFD
01553      PERFORM DET-CF-ALL-IPGT.                                     ELUOVCFD
01554      PERFORM DET-CF-ALL-IPGN.                                     ELUOVCFD
01555      PERFORM DET-CF-FAM-INDIV.                                    ELUOVCFD
01556      PERFORM DET-CF-INST.                                         ELUOVCFD
01557      PERFORM DET-CF-PROF.                                         ELUOVCFD
01558      PERFORM DET-CF-IP.                                           ELUOVCFD
01559      PERFORM DET-CF-OP.                                           ELUOVCFD
01560      PERFORM DET-CF-OV.                                           ELUOVCFD
01561      PERFORM STORE-CF-RESULTS.                                    ELUOVCFD
01562                                                                   ELUOVCFD
01563 /***********************************************************      ELUOVCFD
01564 *                                                          *      ELUOVCFD
01565 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELUOVCFD
01566 *    LINE OF BUSINESS                                      *      ELUOVCFD
01567 *                                                          *      ELUOVCFD
01568 ************************************************************      ELUOVCFD
01569                                                                   ELUOVCFD
01570  DET-CF-ALL-LOB.                                                  ELUOVCFD
01571      SET CFT1-IDX TO 1.                                           ELUOVCFD
01572      SEARCH CFT1-TBL VARYING CFT1-IDX                             ELUOVCFD
01573         AT END                                                    ELUOVCFD
01574            SET CIA-AB-ARG-NOTFND TO TRUE                          ELUOVCFD
01575            PERFORM SIGNAL-ABEND                                   ELUOVCFD
01576         WHEN CFT1-LOB (CFT1-IDX) = ATBL-L-O-B (ATBL-X-IDX)        ELUOVCFD
01577            MOVE CFT1-CF-LOB-INST (CFT1-IDX) TO WS-CF-INST-LOB     ELUOVCFD
01578            MOVE CFT1-CF-LOB-PROF (CFT1-IDX) TO WS-CF-PROF-LOB     ELUOVCFD
01579            MOVE CFT1-CF-LOB-BAS (CFT1-IDX) TO WS-CF-BAS           ELUOVCFD
01580            MOVE CFT1-CF-LOB-SUP (CFT1-IDX) TO WS-CF-SUP           ELUOVCFD
01581         END-SEARCH.                                               ELUOVCFD
01582                                                                   ELUOVCFD
01583 /***********************************************************      ELUOVCFD
01584 *                                                          *      ELUOVCFD
01585 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM         *      ELUOVCFD
01586 *    PLACE OF TREATMENT                                    *      ELUOVCFD
01587 *                                                          *      ELUOVCFD
01588 ************************************************************      ELUOVCFD
01589                                                                   ELUOVCFD
01590  DET-CF-ALL-POT.                                                  ELUOVCFD
01591      SET CFT3-IDX TO 1.                                           ELUOVCFD
01592      SEARCH CFT3-TBL VARYING CFT3-IDX                             ELUOVCFD
01593         AT END                                                    ELUOVCFD
01594            MOVE WS-DV-IP-POT TO WS-CF-IP-POT                      ELUOVCFD
01595            MOVE WS-DV-OP-POT TO WS-CF-OP-POT                      ELUOVCFD
01596            MOVE WS-DV-OV-POT TO WS-CF-OV-POT                      ELUOVCFD
01597         WHEN CFT3-POT (CFT3-IDX) =                                ELUOVCFD
01598              ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)                 ELUOVCFD
01599            MOVE CFT3-CF-POT-INPT (CFT3-IDX) TO WS-CF-IP-POT       ELUOVCFD
01600            MOVE CFT3-CF-POT-OUTPT (CFT3-IDX) TO WS-CF-OP-POT      ELUOVCFD
01601            MOVE CFT3-CF-POT-OV   (CFT3-IDX) TO WS-CF-OV-POT       ELUOVCFD
01602         END-SEARCH.                                               ELUOVCFD
01603                                                                   ELUOVCFD
01604 /***********************************************************      ELUOVCFD
01605 *                                                          *      ELUOVCFD
01606 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELUOVCFD
01607 *    #IBGR TABULAR RECORD                                  *      ELUOVCFD
01608 *                                                          *      ELUOVCFD
01609 ************************************************************      ELUOVCFD
01610                                                                   ELUOVCFD
01611  DET-CF-ALL-IBGR.                                                 ELUOVCFD
01612      IF ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX) = ZERO                 ELUOVCFD
01613      THEN                                                         ELUOVCFD
01614         MOVE WS-CF-TRUE TO WS-CF-INST-IBGR                        ELUOVCFD
01615                            WS-CF-PROF-IBGR                        ELUOVCFD
01616                            WS-CF-IP-IBGR                          ELUOVCFD
01617                            WS-CF-IP-BOTH-IBGR                     ELUOVCFD
01618                            WS-CF-OP-IBGR                          ELUOVCFD
01619                            WS-CF-OP-BOTH-IBGR                     ELUOVCFD
01620                            WS-CF-OV-IBGR                          ELUOVCFD
01621      ELSE                                                         ELUOVCFD
01622         IF ELUIBGR-PROCESSED                                      ELUOVCFD
01623         THEN                                                      ELUOVCFD
01624            PERFORM SEARCH-CF-ALL-IBGR                             ELUOVCFD
01625         ELSE                                                      ELUOVCFD
01626            PERFORM SIGNAL-PROGRAM-LOGIC.                          ELUOVCFD
01627                                                                   ELUOVCFD
01628 ************************************************************      ELUOVCFD
01629 *                                                          *      ELUOVCFD
01630 *    SEARCH THE IBGR CONFIDENCE FACTORS TABLE AND          *      ELUOVCFD
01631 *    RETRIEVE VALUES                                       *      ELUOVCFD
01632 *                                                          *      ELUOVCFD
01633 ************************************************************      ELUOVCFD
01634                                                                   ELUOVCFD
01635  SEARCH-CF-ALL-IBGR.                                              ELUOVCFD
01636      SET IBGR-IDX TO 1.                                           ELUOVCFD
01637      SEARCH IBGR-INTERNAL-TABS VARYING IBGR-IDX                   ELUOVCFD
01638          AT END                                                   ELUOVCFD
01639             SET CIA-AB-ARG-NOTFND TO TRUE                         ELUOVCFD
01640             PERFORM SIGNAL-ABEND                                  ELUOVCFD
01641          WHEN IBGR-SLOT-NUMBER (IBGR-IDX) =                       ELUOVCFD
01642                  ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)               ELUOVCFD
01643             MOVE IBGR-CF-INST   (IBGR-IDX) TO WS-CF-INST-IBGR     ELUOVCFD
01644             MOVE IBGR-CF-PROF   (IBGR-IDX) TO WS-CF-PROF-IBGR     ELUOVCFD
01645             MOVE IBGR-CF-IP     (IBGR-IDX) TO WS-CF-IP-IBGR       ELUOVCFD
01646             MOVE IBGR-CF-IP-BOTH (IBGR-IDX) TO WS-CF-IP-BOTH-IBGR ELUOVCFD
01647             MOVE IBGR-CF-OP     (IBGR-IDX) TO WS-CF-OP-IBGR       ELUOVCFD
01648             MOVE IBGR-CF-OP-BOTH (IBGR-IDX) TO WS-CF-OP-BOTH-IBGR ELUOVCFD
01649          END-SEARCH.                                              ELUOVCFD
01650                                                                   ELUOVCFD
01651 /***********************************************************      ELUOVCFD
01652 *                                                          *      ELUOVCFD
01653 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELUOVCFD
01654 *    #IPGT TABULAR RECORD                                  *      ELUOVCFD
01655 *                                                          *      ELUOVCFD
01656 ************************************************************      ELUOVCFD
01657                                                                   ELUOVCFD
01658  DET-CF-ALL-IPGT.                                                 ELUOVCFD
01659      IF ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX) = ZERO                 ELUOVCFD
01660      THEN                                                         ELUOVCFD
01661         MOVE WS-CF-TRUE TO WS-CF-INST-IPGT                        ELUOVCFD
01662                            WS-CF-PROF-IPGT                        ELUOVCFD
01663                            WS-CF-PLAN                             ELUOVCFD
01664                            WS-CF-NON-PLAN                         ELUOVCFD
01665                            WS-CF-OV-IPGT                          ELUOVCFD
01666      ELSE                                                         ELUOVCFD
01667         IF ELUIPGT-PROCESSED                                      ELUOVCFD
01668         THEN                                                      ELUOVCFD
01669            PERFORM SEARCH-CF-ALL-IPGT                             ELUOVCFD
01670         ELSE                                                      ELUOVCFD
01671            PERFORM SIGNAL-PROGRAM-LOGIC.                          ELUOVCFD
01672                                                                   ELUOVCFD
01673 ************************************************************      ELUOVCFD
01674 *                                                          *      ELUOVCFD
01675 *    SEARCH THE IPGT CONFIDENCE FACTORS TABLE AND          *      ELUOVCFD
01676 *    RETRIEVE VALUES                                       *      ELUOVCFD
01677 *                                                          *      ELUOVCFD
01678 ************************************************************      ELUOVCFD
01679                                                                   ELUOVCFD
01680  SEARCH-CF-ALL-IPGT.                                              ELUOVCFD
01681      SET IPGT-X-IDX TO 1.                                         ELUOVCFD
01682      SEARCH IPGT-INTERNAL-TABS VARYING IPGT-X-IDX                 ELUOVCFD
01683         AT END                                                    ELUOVCFD
01684            SET CIA-AB-ARG-NOTFND TO TRUE                          ELUOVCFD
01685            PERFORM SIGNAL-ABEND                                   ELUOVCFD
01686         WHEN IPGT-SLOT-NUMBER (IPGT-X-IDX) =                      ELUOVCFD
01687                 ATBL-IPGT-SLOT-NUMBER (ATBL-X-IDX)                ELUOVCFD
01688            MOVE IPGT-CF-INST    (IPGT-X-IDX) TO WS-CF-INST-IPGT   ELUOVCFD
01689            MOVE IPGT-CF-PROF    (IPGT-X-IDX) TO WS-CF-PROF-IPGT   ELUOVCFD
01690            MOVE IPGT-CF-PLAN    (IPGT-X-IDX) TO WS-CF-PLAN        ELUOVCFD
01691            MOVE IPGT-CF-NON-PLAN (IPGT-X-IDX) TO WS-CF-NON-PLAN   ELUOVCFD
01692         END-SEARCH.                                               ELUOVCFD
01693                                                                   ELUOVCFD
01694 /***********************************************************      ELUOVCFD
01695 *                                                          *      ELUOVCFD
01696 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELUOVCFD
01697 *    #IPGN TABULAR RECORD                                  *      ELUOVCFD
01698 *                                                          *      ELUOVCFD
01699 ************************************************************      ELUOVCFD
01700                                                                   ELUOVCFD
01701  DET-CF-ALL-IPGN.                                                 ELUOVCFD
01702      IF ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX) = ZERO                 ELUOVCFD
01703      THEN                                                         ELUOVCFD
01704         MOVE WS-CF-TRUE TO WS-CF-OV-IPGN                          ELUOVCFD
01705      ELSE                                                         ELUOVCFD
01706         IF ELUIPGN-PROCESSED                                      ELUOVCFD
01707         THEN                                                      ELUOVCFD
01708            PERFORM SEARCH-CF-ALL-IPGN                             ELUOVCFD
01709         ELSE                                                      ELUOVCFD
01710            PERFORM SIGNAL-PROGRAM-LOGIC.                          ELUOVCFD
01711                                                                   ELUOVCFD
01712                                                                   ELUOVCFD
01713 ************************************************************      ELUOVCFD
01714 *                                                          *      ELUOVCFD
01715 *    SEARCH THE IPGT CONFIDENCE FACTORS TABLE AND          *      ELUOVCFD
01716 *    RETRIEVE VALUES                                       *      ELUOVCFD
01717 *                                                          *      ELUOVCFD
01718 ************************************************************      ELUOVCFD
01719                                                                   ELUOVCFD
01720  SEARCH-CF-ALL-IPGN.                                              ELUOVCFD
01721      SET IPGN-X-IDX TO 1.                                         ELUOVCFD
01722      SEARCH IPGN-INTERNAL-TABS VARYING IPGN-X-IDX                 ELUOVCFD
01723         AT END                                                    ELUOVCFD
01724            SET CIA-AB-ARG-NOTFND TO TRUE                          ELUOVCFD
01725            PERFORM SIGNAL-ABEND                                   ELUOVCFD
01726         WHEN IPGN-SLOT-NUMBER (IPGN-X-IDX) =                      ELUOVCFD
01727                 ATBL-IPGN-SLOT-NUMBER (ATBL-X-IDX)                ELUOVCFD
01728            MOVE IPGN-CF-OV (IPGN-X-IDX) TO WS-CF-OV-IPGN          ELUOVCFD
01729         END-SEARCH.                                               ELUOVCFD
01730                                                                   ELUOVCFD
01731 /***********************************************************      ELUOVCFD
01732 *                                                          *      ELUOVCFD
01733 *    DETERMINE FAMILY AND INDIVIDUAL CONFIDENCE FACTORS    *      ELUOVCFD
01734 *                                                          *      ELUOVCFD
01735 ************************************************************      ELUOVCFD
01736                                                                   ELUOVCFD
01737  DET-CF-FAM-INDIV.                                                ELUOVCFD
01738      IF ATBL-FAM-OR-INDIV (ATBL-X-IDX) = 'F'                      ELUOVCFD
01739      THEN                                                         ELUOVCFD
01740         MOVE WS-CF-TRUE  TO WS-CF-FAM                             ELUOVCFD
01741         MOVE WS-CF-FALSE TO WS-CF-INDIV                           ELUOVCFD
01742      ELSE                                                         ELUOVCFD
01743         MOVE WS-CF-FALSE TO WS-CF-FAM                             ELUOVCFD
01744         MOVE WS-CF-TRUE  TO WS-CF-INDIV                           ELUOVCFD
01745      END-IF.                                                      ELUOVCFD
01746                                                                   ELUOVCFD
01747 /***********************************************************      ELUOVCFD
01748 *                                                          *      ELUOVCFD
01749 *    DETERMINE INSTITUTIONAL CONFIDENCE FACTOR             *      ELUOVCFD
01750 *                                                          *      ELUOVCFD
01751 ************************************************************      ELUOVCFD
01752                                                                   ELUOVCFD
01753  DET-CF-INST.                                                     ELUOVCFD
01754                                                                   ELUOVCFD
01755 * AND THE IPGT AND IBGR COMPONENTS WITH THE LOB COMPONENT         ELUOVCFD
01756      CALL 'ELKFLAND'                                              ELUOVCFD
01757         USING WS-CW-INST-IPGT                                     ELUOVCFD
01758               WS-CF-INST-LOB                                      ELUOVCFD
01759               WS-CF-INST-IPGT.                                    ELUOVCFD
01760      CALL 'ELKFLAND'                                              ELUOVCFD
01761         USING WS-CW-INST-IBGR                                     ELUOVCFD
01762               WS-CF-INST-LOB                                      ELUOVCFD
01763               WS-CF-INST-IBGR.                                    ELUOVCFD
01764                                                                   ELUOVCFD
01765 * APPLY WEIGHTING FACTORS TO INSTITUTIONAL CF COMPONENTS          ELUOVCFD
01766      COMPUTE WS-CW-INST-IPGT = WS-CW-INST-IPGT * WS-WT-INST-IPGT. ELUOVCFD
01767      COMPUTE WS-CW-INST-IBGR = WS-CW-INST-IBGR * WS-WT-INST-IBGR. ELUOVCFD
01768                                                                   ELUOVCFD
01769 * USE COMBINATION FUNCTION TO COMBINE PARTIAL FACTORS FOR INST CF ELUOVCFD
01770      CALL 'ELKFLCMB'                                              ELUOVCFD
01771         USING WS-CF-INST                                          ELUOVCFD
01772               WS-CW-INST-IPGT                                     ELUOVCFD
01773               WS-CW-INST-IBGR.                                    ELUOVCFD
01774                                                                   ELUOVCFD
01775 ************************************************************      ELUOVCFD
01776 *                                                          *      ELUOVCFD
01777 *    DETERMINE PROFESSIONAL CONFIDENCE FACTOR              *      ELUOVCFD
01778 *                                                          *      ELUOVCFD
01779 ************************************************************      ELUOVCFD
01780                                                                   ELUOVCFD
01781  DET-CF-PROF.                                                     ELUOVCFD
01782                                                                   ELUOVCFD
01783 * AND THE IPGT AND IBGR COMPONENTS WITH THE LOB COMPONENT         ELUOVCFD
01784      CALL 'ELKFLAND'                                              ELUOVCFD
01785         USING WS-CW-PROF-IPGT                                     ELUOVCFD
01786               WS-CF-PROF-LOB                                      ELUOVCFD
01787               WS-CF-PROF-IPGT.                                    ELUOVCFD
01788      CALL 'ELKFLAND'                                              ELUOVCFD
01789         USING WS-CW-PROF-IBGR                                     ELUOVCFD
01790               WS-CF-PROF-LOB                                      ELUOVCFD
01791               WS-CF-PROF-IBGR.                                    ELUOVCFD
01792                                                                   ELUOVCFD
01793 * APPLY WEIGHTING FACTORS TO PROFESSIONAL CF COMPONENTS           ELUOVCFD
01794      COMPUTE WS-CW-PROF-IPGT = WS-CW-PROF-IPGT * WS-WT-PROF-IPGT. ELUOVCFD
01795      COMPUTE WS-CW-PROF-IBGR = WS-CW-PROF-IBGR * WS-WT-PROF-IBGR. ELUOVCFD
01796                                                                   ELUOVCFD
01797 * USE COMBINATION FUNCTION TO COMBINE PARTIAL FACTORS FOR PROF CF ELUOVCFD
01798      CALL 'ELKFLCMB'                                              ELUOVCFD
01799         USING WS-CF-PROF                                          ELUOVCFD
01800               WS-CW-PROF-IPGT                                     ELUOVCFD
01801               WS-CW-PROF-IBGR.                                    ELUOVCFD
01802                                                                   ELUOVCFD
01803 /***********************************************************      ELUOVCFD
01804 *                                                          *      ELUOVCFD
01805 *        DETERMINE INPATIENT CONFIDENCE FACTOR             *      ELUOVCFD
01806 *                                                          *      ELUOVCFD
01807 ************************************************************      ELUOVCFD
01808                                                                   ELUOVCFD
01809  DET-CF-IP.                                                       ELUOVCFD
01810                                                                   ELUOVCFD
01811 * APPLY WEIGHTING FACTORS                                         ELUOVCFD
01812      COMPUTE WS-CW-IP-POT  = WS-CF-IP-POT * WS-WT-IP-POT.         ELUOVCFD
01813      COMPUTE WS-CW-IP-IBGR = WS-CF-IP-POT * WS-WT-IP-IBGR.        ELUOVCFD
01814                                                                   ELUOVCFD
01815 * COMBINE FACTORS TO DETERMINE INPATIENT CONFIDENCE FACTOR        ELUOVCFD
01816      CALL 'ELKFLCMB'                                              ELUOVCFD
01817         USING WS-CF-IP                                            ELUOVCFD
01818               WS-CW-IP-POT                                        ELUOVCFD
01819               WS-CW-IP-IBGR.                                      ELUOVCFD
01820                                                                   ELUOVCFD
01821 ************************************************************      ELUOVCFD
01822 *                                                          *      ELUOVCFD
01823 *        DETERMINE OUTPATIENT CONFIDENCE FACTOR            *      ELUOVCFD
01824 *                                                          *      ELUOVCFD
01825 ************************************************************      ELUOVCFD
01826                                                                   ELUOVCFD
01827  DET-CF-OP.                                                       ELUOVCFD
01828                                                                   ELUOVCFD
01829 * APPLY WEIGHTING FACTORS                                         ELUOVCFD
01830      COMPUTE WS-CW-OP-POT  = WS-CF-OP-POT * WS-WT-OP-POT.         ELUOVCFD
01831      COMPUTE WS-CW-OP-IBGR = WS-CF-OP-POT * WS-WT-OP-IBGR.        ELUOVCFD
01832                                                                   ELUOVCFD
01833 * COMBINE FACTORS TO DETERMINE OUTPATIENT CONFIDENCE FACTOR       ELUOVCFD
01834      CALL 'ELKFLCMB'                                              ELUOVCFD
01835         USING WS-CF-OP                                            ELUOVCFD
01836               WS-CW-OP-POT                                        ELUOVCFD
01837               WS-CW-OP-IBGR.                                      ELUOVCFD
01838                                                                   ELUOVCFD
01839 /***********************************************************      ELUOVCFD
01840 *                                                          *      ELUOVCFD
01841 *    DETERMINE MISCELLANEOUS OVERALLNESS CONFIDENCE        *      ELUOVCFD
01842 *    FACTOR                                                *      ELUOVCFD
01843 *                                                          *      ELUOVCFD
01844 ************************************************************      ELUOVCFD
01845                                                                   ELUOVCFD
01846  DET-CF-OV.                                                       ELUOVCFD
01847                                                                   ELUOVCFD
01848 * DETERMINE REMAINING COMPONENTS OF OVERALLNESS                   ELUOVCFD
01849      PERFORM DET-CF-OV-COND-BIT.                                  ELUOVCFD
01850      PERFORM DET-CF-OV-CC-IND.                                    ELUOVCFD
01851      PERFORM DET-CF-OV-SERV-GRP.                                  ELUOVCFD
01852      PERFORM DET-CF-OV-INT-DESC.                                  ELUOVCFD
01853      PERFORM DET-CF-OV-VAL-QUAL.                                  ELUOVCFD
01854      PERFORM DET-CF-OV-IBGR.                                      ELUOVCFD
01855      PERFORM DET-CF-OV-IPGT.                                      ELUOVCFD
01856                                                                   ELUOVCFD
01857 * FUZZY AND ALL WEIGHTED COMPONENTS TO GET OVERALLNESS VALUE      ELUOVCFD
01858      CALL 'ELKFLAND'                                              ELUOVCFD
01859         USING WS-CF-OV                                            ELUOVCFD
01860               WS-CF-OV-COND-BIT                                   ELUOVCFD
01861               WS-CF-OV-CC-IND                                     ELUOVCFD
01862               WS-CF-OV-SERV-GRP                                   ELUOVCFD
01863 *             WS-CF-OV-POT                                        ELUOVCFD
01864               WS-CF-OV-INT-DESC                                   ELUOVCFD
01865               WS-CF-OV-VAL-QUAL                                   ELUOVCFD
01866               WS-CF-OV-IBGR                                       ELUOVCFD
01867               WS-CF-OV-IPGT                                       ELUOVCFD
01868               WS-CF-OV-IPGN.                                      ELUOVCFD
01869                                                                   ELUOVCFD
01870 /***********************************************************      ELUOVCFD
01871 *                                                          *      ELUOVCFD
01872 *        DETERMINE CONDITION BIT CF                        *      ELUOVCFD
01873 *                                                          *      ELUOVCFD
01874 ************************************************************      ELUOVCFD
01875                                                                   ELUOVCFD
01876  DET-CF-OV-COND-BIT.                                              ELUOVCFD
01877      IF    ATBL-COND-ALL-BIT (ATBL-X-IDX ) = '1'                  ELUOVCFD
01878         OR ATBL-COND-ICD-BIT (ATBL-X-IDX ) = '1'                  ELUOVCFD
01879      THEN                                                         ELUOVCFD
01880         IF ATBL-COND-EXCLUSION-BIT (ATBL-X-IDX) = '1'             ELUOVCFD
01881         THEN                                                      ELUOVCFD
01882            MOVE WS-CF-OV-COND-BIT-MTRUE TO WS-CF-OV-COND-BIT      ELUOVCFD
01883         ELSE                                                      ELUOVCFD
01884            MOVE WS-CF-OV-COND-BIT-VTRUE TO WS-CF-OV-COND-BIT      ELUOVCFD
01885         END-IF                                                    ELUOVCFD
01886      ELSE                                                         ELUOVCFD
01887         MOVE WS-CF-FALSE TO WS-CF-OV-COND-BIT                     ELUOVCFD
01888      END-IF.                                                      ELUOVCFD
01889                                                                   ELUOVCFD
01890 ************************************************************      ELUOVCFD
01891 *                                                          *      ELUOVCFD
01892 *        DETERMINE COST CONTAINMENT CF                     *      ELUOVCFD
01893 *                                                          *      ELUOVCFD
01894 ************************************************************      ELUOVCFD
01895                                                                   ELUOVCFD
01896  DET-CF-OV-CC-IND.                                                ELUOVCFD
01897      IF ATBL-COST-CONTAIN-IND (ATBL-X-IDX) = '00'                 ELUOVCFD
01898      THEN                                                         ELUOVCFD
01899         MOVE WS-CF-OV-CC-IND-TRUE TO WS-CF-OV-CC-IND              ELUOVCFD
01900      ELSE                                                         ELUOVCFD
01901         MOVE WS-CF-FALSE TO WS-CF-OV-CC-IND                       ELUOVCFD
01902      END-IF.                                                      ELUOVCFD
01903                                                                   ELUOVCFD
01904 ************************************************************      ELUOVCFD
01905 *                                                          *      ELUOVCFD
01906 *        DETERMINE SERVICE GROUP CF                        *      ELUOVCFD
01907 *                                                          *      ELUOVCFD
01908 ************************************************************      ELUOVCFD
01909                                                                   ELUOVCFD
01910  DET-CF-OV-SERV-GRP.                                              ELUOVCFD
01911      IF ATBL-SERVICE-GROUP (ATBL-X-IDX) = '00'                    ELUOVCFD
01912      THEN                                                         ELUOVCFD
01913         MOVE WS-CF-OV-SERV-GRP-TRUE TO WS-CF-OV-SERV-GRP          ELUOVCFD
01914      ELSE                                                         ELUOVCFD
01915         MOVE WS-CF-FALSE TO WS-CF-OV-SERV-GRP                     ELUOVCFD
01916      END-IF.                                                      ELUOVCFD
01917                                                                   ELUOVCFD
01918 /***********************************************************      ELUOVCFD
01919 *                                                          *      ELUOVCFD
01920 *        DETERMINE INTERNAL DESCRIPTOR CF                  *      ELUOVCFD
01921 *                                                          *      ELUOVCFD
01922 ************************************************************      ELUOVCFD
01923                                                                   ELUOVCFD
01924  DET-CF-OV-INT-DESC.                                              ELUOVCFD
01925                                                                   ELUOVCFD
01926 * LOOK UP RELATIVE INSTITUTIONAL AND PROFESSIONAL CONFIDENCE      ELUOVCFD
01927 * FACTORS PER INTERNAL DESCRIPTOR                                 ELUOVCFD
01928      SET CFT5-IDX TO 1.                                           ELUOVCFD
01929      SEARCH CFT5-TBL VARYING CFT5-IDX                             ELUOVCFD
01930         AT END                                                    ELUOVCFD
01931            MOVE WS-DV-OV-INT-DESC-INST TO WS-CF-OV-INT-DESC-INST  ELUOVCFD
01932            MOVE WS-DV-OV-INT-DESC-PROF TO WS-CF-OV-INT-DESC-PROF  ELUOVCFD
01933         WHEN CFT5-INTD (CFT5-IDX) =                               ELUOVCFD
01934              ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX)                ELUOVCFD
01935            MOVE CFT5-CF-INTD-INST (CFT5-IDX)                      ELUOVCFD
01936              TO WS-CF-OV-INT-DESC-INST                            ELUOVCFD
01937            MOVE CFT5-CF-INTD-PROF (CFT5-IDX)                      ELUOVCFD
01938              TO WS-CF-OV-INT-DESC-PROF                            ELUOVCFD
01939         END-SEARCH.                                               ELUOVCFD
01940                                                                   ELUOVCFD
01941 * WEIGHTING OF THE INSTITUTIONAL AND PROFESSIONAL COMPONENTS      ELUOVCFD
01942 * IS DONE ACCORDING TO THE RELATED GENERAL INSTITUTIONAL AND      ELUOVCFD
01943 * PROFESSIONAL CONFIDENCE FACTORS, RESPECTIVELY.                  ELUOVCFD
01944                                                                   ELUOVCFD
01945 * AND INSTITUTIONAL SUBCOMPONENTS                                 ELUOVCFD
01946      CALL 'ELKFLAND'                                              ELUOVCFD
01947         USING WS-CW-OV-INT-DESC-INST                              ELUOVCFD
01948               WS-CF-INST                                          ELUOVCFD
01949               WS-CF-OV-INT-DESC-INST.                             ELUOVCFD
01950                                                                   ELUOVCFD
01951 * AND PROFESSIONAL SUBCOMPONENTS                                  ELUOVCFD
01952      CALL 'ELKFLAND'                                              ELUOVCFD
01953         USING WS-CW-OV-INT-DESC-PROF                              ELUOVCFD
01954               WS-CF-PROF                                          ELUOVCFD
01955               WS-CF-OV-INT-DESC-PROF.                             ELUOVCFD
01956                                                                   ELUOVCFD
01957 * OR THE INSTITUTIONAL COMPONENT WITH THE PROFESSIONAL COMPONENT  ELUOVCFD
01958      CALL 'ELKFLOR'                                               ELUOVCFD
01959         USING WS-CF-OV-INT-DESC                                   ELUOVCFD
01960               WS-CW-OV-INT-DESC-INST                              ELUOVCFD
01961               WS-CW-OV-INT-DESC-PROF.                             ELUOVCFD
01962                                                                   ELUOVCFD
01963 /***********************************************************      ELUOVCFD
01964 *                                                          *      ELUOVCFD
01965 *        DETERMINE VALUE QUALIFIER CF                      *      ELUOVCFD
01966 *                                                          *      ELUOVCFD
01967 ************************************************************      ELUOVCFD
01968                                                                   ELUOVCFD
01969  DET-CF-OV-VAL-QUAL.                                              ELUOVCFD
01970                                                                   ELUOVCFD
01971 * LOOK UP RELATIVE INSTITUTIONAL AND PROFESSIONAL CONFIDENCE      ELUOVCFD
01972 * FACTORS PER VALUE QUALIFIER                                     ELUOVCFD
01973      SET CFT6-IDX TO 1.                                           ELUOVCFD
01974      SEARCH CFT6-TBL VARYING CFT6-IDX                             ELUOVCFD
01975         AT END                                                    ELUOVCFD
01976            MOVE WS-DV-OV-VAL-QUAL-INST TO WS-CF-OV-VAL-QUAL-INST  ELUOVCFD
01977            MOVE WS-DV-OV-VAL-QUAL-PROF TO WS-CF-OV-VAL-QUAL-PROF  ELUOVCFD
01978         WHEN CFT6-VALQL (CFT6-IDX) =                              ELUOVCFD
01979               ATBL-VALUE-QUALIFIER (ATBL-X-IDX)                   ELUOVCFD
01980            MOVE CFT6-CF-VALQL-INST (CFT6-IDX)                     ELUOVCFD
01981              TO WS-CF-OV-VAL-QUAL-INST                            ELUOVCFD
01982            MOVE CFT6-CF-VALQL-PROF (CFT6-IDX)                     ELUOVCFD
01983              TO WS-CF-OV-VAL-QUAL-PROF                            ELUOVCFD
01984         END-SEARCH.                                               ELUOVCFD
01985                                                                   ELUOVCFD
01986 * WEIGHTING OF THE INSTITUTIONAL AND PROFESSIONAL COMPONENTS      ELUOVCFD
01987 * IS DONE ACCORDING TO THE RELATED GENERAL INSTITUTIONAL AND      ELUOVCFD
01988 * PROFESSIONAL CONFIDENCE FACTORS, RESPECTIVELY.                  ELUOVCFD
01989                                                                   ELUOVCFD
01990 * AND INSTITUTIONAL SUBCOMPONENTS                                 ELUOVCFD
01991      CALL 'ELKFLAND'                                              ELUOVCFD
01992         USING WS-CW-OV-VAL-QUAL-INST                              ELUOVCFD
01993               WS-CF-INST                                          ELUOVCFD
01994               WS-CF-OV-VAL-QUAL-INST.                             ELUOVCFD
01995                                                                   ELUOVCFD
01996 * AND PROFESSIONAL SUBCOMPONENTS                                  ELUOVCFD
01997      CALL 'ELKFLAND'                                              ELUOVCFD
01998         USING WS-CW-OV-VAL-QUAL-PROF                              ELUOVCFD
01999               WS-CF-PROF                                          ELUOVCFD
02000               WS-CF-OV-VAL-QUAL-PROF.                             ELUOVCFD
02001                                                                   ELUOVCFD
02002 * OR THE INSTITUTIONAL COMPONENT WITH THE PROFESSIONAL COMPONENT  ELUOVCFD
02003      CALL 'ELKFLOR'                                               ELUOVCFD
02004         USING WS-CF-OV-VAL-QUAL                                   ELUOVCFD
02005               WS-CW-OV-VAL-QUAL-INST                              ELUOVCFD
02006               WS-CW-OV-VAL-QUAL-PROF.                             ELUOVCFD
02007                                                                   ELUOVCFD
02008 /***********************************************************      ELUOVCFD
02009 *                                                          *      ELUOVCFD
02010 *        DETERMINE IBGR OVERALLNESS CONFIDENCE FACTOR      *      ELUOVCFD
02011 *                                                          *      ELUOVCFD
02012 ************************************************************      ELUOVCFD
02013                                                                   ELUOVCFD
02014  DET-CF-OV-IBGR.                                                  ELUOVCFD
02015                                                                   ELUOVCFD
02016 * WEIGHTING OF THE INSTITUTIONAL AND PROFESSIONAL COMPONENTS      ELUOVCFD
02017 * IS DONE ACCORDING TO THE RELATED GENERAL INSTITUTIONAL AND      ELUOVCFD
02018 * PROFESSIONAL CONFIDENCE FACTORS, RESPECTIVELY.                  ELUOVCFD
02019                                                                   ELUOVCFD
02020 * AND INSTITUTIONAL SUBCOMPONENTS                                 ELUOVCFD
02021      CALL 'ELKFLAND'                                              ELUOVCFD
02022         USING WS-CW-OV-IBGR-INST                                  ELUOVCFD
02023               WS-CF-INST                                          ELUOVCFD
02024               WS-CF-INST-IBGR.                                    ELUOVCFD
02025                                                                   ELUOVCFD
02026 * AND PROFESSIONAL SUBCOMPONENTS                                  ELUOVCFD
02027      CALL 'ELKFLAND'                                              ELUOVCFD
02028         USING WS-CW-OV-IBGR-PROF                                  ELUOVCFD
02029               WS-CF-PROF                                          ELUOVCFD
02030               WS-CF-PROF-IBGR.                                    ELUOVCFD
02031                                                                   ELUOVCFD
02032 * OR THE INSTITUTIONAL COMPONENT WITH THE PROFESSIONAL COMPONENT  ELUOVCFD
02033      CALL 'ELKFLOR'                                               ELUOVCFD
02034         USING WS-CF-OV-IBGR                                       ELUOVCFD
02035               WS-CW-OV-IBGR-INST                                  ELUOVCFD
02036               WS-CW-OV-IBGR-PROF.                                 ELUOVCFD
02037                                                                   ELUOVCFD
02038 /***********************************************************      ELUOVCFD
02039 *                                                          *      ELUOVCFD
02040 *        DETERMINE IPGT OVERALLNESS CONFIDENCE FACTOR      *      ELUOVCFD
02041 *                                                          *      ELUOVCFD
02042 ************************************************************      ELUOVCFD
02043                                                                   ELUOVCFD
02044  DET-CF-OV-IPGT.                                                  ELUOVCFD
02045                                                                   ELUOVCFD
02046 * WEIGHTING OF THE INSTITUTIONAL AND PROFESSIONAL COMPONENTS      ELUOVCFD
02047 * IS DONE ACCORDING TO THE RELATED GENERAL INSTITUTIONAL AND      ELUOVCFD
02048 * PROFESSIONAL CONFIDENCE FACTORS, RESPECTIVELY.                  ELUOVCFD
02049                                                                   ELUOVCFD
02050 * AND INSTITUTIONAL SUBCOMPONENTS                                 ELUOVCFD
02051      CALL 'ELKFLAND'                                              ELUOVCFD
02052         USING WS-CW-OV-IPGT-INST                                  ELUOVCFD
02053               WS-CF-INST                                          ELUOVCFD
02054               WS-CF-INST-IPGT.                                    ELUOVCFD
02055                                                                   ELUOVCFD
02056 * AND PROFESSIONAL SUBCOMPONENTS                                  ELUOVCFD
02057      CALL 'ELKFLAND'                                              ELUOVCFD
02058         USING WS-CW-OV-IPGT-PROF                                  ELUOVCFD
02059               WS-CF-PROF                                          ELUOVCFD
02060               WS-CF-PROF-IPGT.                                    ELUOVCFD
02061                                                                   ELUOVCFD
02062 * OR THE INSTITUTIONAL COMPONENT WITH THE PROFESSIONAL COMPONENT  ELUOVCFD
02063      CALL 'ELKFLOR'                                               ELUOVCFD
02064         USING WS-CF-OV-IPGT                                       ELUOVCFD
02065               WS-CW-OV-IPGT-INST                                  ELUOVCFD
02066               WS-CW-OV-IPGT-PROF.                                 ELUOVCFD
02067                                                                   ELUOVCFD
02068 /***********************************************************      ELUOVCFD
02069 *                                                          *      ELUOVCFD
02070 *    STORE CONFIDENCE FACTORS CALCULATED                   *      ELUOVCFD
02071 *                                                          *      ELUOVCFD
02072 ************************************************************      ELUOVCFD
02073                                                                   ELUOVCFD
02074  STORE-CF-RESULTS.                                                ELUOVCFD
02075      MOVE WS-CF-BEN-PERD TO ATBL-CF-BENPERD (ATBL-X-IDX).         ELUOVCFD
02076      MOVE WS-CF-FAM      TO ATBL-CF-FAM     (ATBL-X-IDX).         ELUOVCFD
02077      MOVE WS-CF-INDIV    TO ATBL-CF-INDIV   (ATBL-X-IDX).         ELUOVCFD
02078      MOVE WS-CF-INST     TO ATBL-CF-INST    (ATBL-X-IDX).         ELUOVCFD
02079      MOVE WS-CF-PROF     TO ATBL-CF-PROF    (ATBL-X-IDX).         ELUOVCFD
02080      MOVE WS-CF-BAS      TO ATBL-CF-BAS     (ATBL-X-IDX).         ELUOVCFD
02081      MOVE WS-CF-SUP      TO ATBL-CF-SUP     (ATBL-X-IDX).         ELUOVCFD
02082      MOVE WS-CF-IP       TO ATBL-CF-IP      (ATBL-X-IDX).         ELUOVCFD
02083      MOVE WS-CF-OP       TO ATBL-CF-OP      (ATBL-X-IDX).         ELUOVCFD
02084      MOVE WS-CF-PLAN     TO ATBL-CF-PLAN    (ATBL-X-IDX).         ELUOVCFD
02085      MOVE WS-CF-NON-PLAN TO ATBL-CF-NON-PLAN (ATBL-X-IDX).        ELUOVCFD
02086      MOVE WS-CF-OV       TO ATBL-CF-OV      (ATBL-X-IDX).         ELUOVCFD
02087      MOVE WS-CF-OV-IBGR  TO ATBL-CF-OV-BEN-PROVN (ATBL-X-IDX).    ELUOVCFD
02088      MOVE WS-CF-OV-IPGN  TO ATBL-CF-OV-PROV-NBR (ATBL-X-IDX).     ELUOVCFD
02089      MOVE WS-CF-OV-IPGT  TO ATBL-CF-OV-PROV-TYPE (ATBL-X-IDX).    ELUOVCFD
02090                                                                   ELUOVCFD
02091 /***********************************************************      ELUOVCFD
02092 *                                                          *      ELUOVCFD
02093 *    RANK TABULARS                                         *      ELUOVCFD
02094 *                                                          *      ELUOVCFD
02095 ************************************************************      ELUOVCFD
02096                                                                   ELUOVCFD
02097  RANK-TABULARS.                                                   ELUOVCFD
02098      IF CSAC-ABM-RR-PTR NOT = NULLS                               ELUOVCFD
02099          PERFORM ASSIGN-WORK-CF-FOR-ABM.                          ELUOVCFD
02100      IF CSAC-ACL-RR-PTR NOT = NULLS                               ELUOVCFD
02101          PERFORM ASSIGN-WORK-CF-FOR-ACL.                          ELUOVCFD
02102      IF CSAC-ADL-RR-PTR NOT = NULLS                               ELUOVCFD
02103          PERFORM ASSIGN-WORK-CF-FOR-ADL.                          ELUOVCFD
02104      IF CSAC-AOL-RR-PTR NOT = NULLS                               ELUOVCFD
02105          PERFORM ASSIGN-WORK-CF-FOR-AOL.                          ELUOVCFD
02106                                                                   ELUOVCFD
02107 ************************************************************      ELUOVCFD
02108 *                                                          *      ELUOVCFD
02109 *    ASSIGN WORK CF FOR ABM                                *      ELUOVCFD
02110 *                                                          *      ELUOVCFD
02111 ************************************************************      ELUOVCFD
02112                                                                   ELUOVCFD
02113  ASSIGN-WORK-CF-FOR-ABM.                                          ELUOVCFD
02114      SET PROCESSING-ABM TO TRUE.                                  ELUOVCFD
02115      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ABM-RR-PTR.  ELUOVCFD
02116      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ABM-GC-TBL-PTR.ELUOVCFD
02117      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
02118      PERFORM LOOP-THRU-ELSRRBLC                                   ELUOVCFD
02119         VARYING RRBL-X-IDX FROM 1 BY 1                            ELUOVCFD
02120           UNTIL RRBL-X-IDX > RRBL-TBL-CNT.                        ELUOVCFD
02121                                                                   ELUOVCFD
02122 ************************************************************      ELUOVCFD
02123 *                                                          *      ELUOVCFD
02124 *    ASSIGN WORK CF FOR ACL                                *      ELUOVCFD
02125 *                                                          *      ELUOVCFD
02126 ************************************************************      ELUOVCFD
02127                                                                   ELUOVCFD
02128  ASSIGN-WORK-CF-FOR-ACL.                                          ELUOVCFD
02129      SET PROCESSING-ACL TO TRUE.                                  ELUOVCFD
02130      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ACL-RR-PTR.  ELUOVCFD
02131      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ACL-GC-TBL-PTR.ELUOVCFD
02132      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
02133      PERFORM LOOP-THRU-ELSRRBLC                                   ELUOVCFD
02134         VARYING RRBL-X-IDX FROM 1 BY 1                            ELUOVCFD
02135           UNTIL RRBL-X-IDX > RRBL-TBL-CNT.                        ELUOVCFD
02136                                                                   ELUOVCFD
02137 ************************************************************      ELUOVCFD
02138 *                                                          *      ELUOVCFD
02139 *    ASSIGN WORK CF FOR ADL                                *      ELUOVCFD
02140 *                                                          *      ELUOVCFD
02141 ************************************************************      ELUOVCFD
02142                                                                   ELUOVCFD
02143  ASSIGN-WORK-CF-FOR-ADL.                                          ELUOVCFD
02144      SET PROCESSING-ADL TO TRUE.                                  ELUOVCFD
02145      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ADL-RR-PTR.  ELUOVCFD
02146      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-ADL-GC-TBL-PTR.ELUOVCFD
02147      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
02148      PERFORM LOOP-THRU-ELSRRBLC                                   ELUOVCFD
02149         VARYING RRBL-X-IDX FROM 1 BY 1                            ELUOVCFD
02150           UNTIL RRBL-X-IDX > RRBL-TBL-CNT.                        ELUOVCFD
02151                                                                   ELUOVCFD
02152 ************************************************************      ELUOVCFD
02153 *                                                          *      ELUOVCFD
02154 *    ASSIGN WORK CF FOR AOL                                *      ELUOVCFD
02155 *                                                          *      ELUOVCFD
02156 ************************************************************      ELUOVCFD
02157                                                                   ELUOVCFD
02158  ASSIGN-WORK-CF-FOR-AOL.                                          ELUOVCFD
02159      SET PROCESSING-AOL TO TRUE.                                  ELUOVCFD
02160      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-AOL-RR-PTR.  ELUOVCFD
02161      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO CSAC-AOL-GC-TBL-PTR.ELUOVCFD
02162      SET RRBL-X-IDX TO 1.                                         ELUOVCFD
02163      PERFORM LOOP-THRU-ELSRRBLC                                   ELUOVCFD
02164         VARYING RRBL-X-IDX FROM 1 BY 1                            ELUOVCFD
02165           UNTIL RRBL-X-IDX > RRBL-TBL-CNT.                        ELUOVCFD
02166                                                                   ELUOVCFD
02167 ************************************************************      ELUOVCFD
02168 *                                                          *      ELUOVCFD
02169 *    LOOP THRU ELSRRBLC                                    *      ELUOVCFD
02170 *                                                          *      ELUOVCFD
02171 ************************************************************      ELUOVCFD
02172                                                                   ELUOVCFD
02173  LOOP-THRU-ELSRRBLC.                                              ELUOVCFD
02174      INITIALIZE RRBL-HIGH-CF-ENTRY (RRBL-X-IDX).                  ELUOVCFD
02175      SET ATBL-X-IDX TO 1.                                         ELUOVCFD
02176      PERFORM LOOP-THRU-ACCUM-TABULARS                             ELUOVCFD
02177          VARYING ATBL-X-IDX FROM 1 BY 1                           ELUOVCFD
02178            UNTIL ATBL-X-IDX > ATBL-TBL-CNT.                       ELUOVCFD
02179                                                                   ELUOVCFD
02180 ************************************************************      ELUOVCFD
02181 *                                                          *      ELUOVCFD
02182 *    LOOP THRU ACCUM TABULARS                              *      ELUOVCFD
02183 *                                                          *      ELUOVCFD
02184 ************************************************************      ELUOVCFD
02185                                                                   ELUOVCFD
02186  LOOP-THRU-ACCUM-TABULARS.                                        ELUOVCFD
02187      PERFORM DET-CF-BEN-PERD.                                     ELUOVCFD
02188      PERFORM INTERROGATE-CONFIDENCE-FACTORS.                      ELUOVCFD
02189      PERFORM COMPARE-FOR-BEST-OVERALL.                            ELUOVCFD
02190                                                                   ELUOVCFD
02191 ************************************************************      ELUOVCFD
02192 *                                                          *      ELUOVCFD
02193 *    DETERMINE BENEFIT PERIOD CONFIDENCE FACTOR            *      ELUOVCFD
02194 *                                                          *      ELUOVCFD
02195 ************************************************************      ELUOVCFD
02196                                                                   ELUOVCFD
02197  DET-CF-BEN-PERD.                                                 ELUOVCFD
02198      IF ATBL-BENEFIT-PERIOD (ATBL-X-IDX) =                        ELUOVCFD
02199         RRBL-BENEFIT-PERIOD (RRBL-X-IDX)                          ELUOVCFD
02200      THEN                                                         ELUOVCFD
02201         MOVE WS-CF-TRUE TO WS-CF-BEN-PERD                         ELUOVCFD
02202      ELSE                                                         ELUOVCFD
02203         MOVE WS-CF-FALSE TO WS-CF-BEN-PERD                        ELUOVCFD
02204      END-IF.                                                      ELUOVCFD
02205      MOVE WS-CF-BEN-PERD TO ATBL-CF-BENPERD (ATBL-X-IDX).         ELUOVCFD
02206                                                                   ELUOVCFD
02207 ************************************************************      ELUOVCFD
02208 *                                                          *      ELUOVCFD
02209 *        INTERROGATE CONFIDENCE FACTORS                    *      ELUOVCFD
02210 *                                                          *      ELUOVCFD
02211 ************************************************************      ELUOVCFD
02212                                                                   ELUOVCFD
02213  INTERROGATE-CONFIDENCE-FACTORS.                                  ELUOVCFD
02214      MOVE ATBL-CF-BENPERD (ATBL-X-IDX) TO WS-CF-WORKAREA.         ELUOVCFD
02215      IF RRBL-REGARD-FAMILY       (RRBL-X-IDX)                     ELUOVCFD
02216      THEN                                                         ELUOVCFD
02217         CALL 'ELKFLAND'                                           ELUOVCFD
02218            USING WS-CF-WORKAREA                                   ELUOVCFD
02219                  WS-CF-WORKAREA                                   ELUOVCFD
02220                  ATBL-CF-FAM (ATBL-X-IDX).                        ELUOVCFD
02221                                                                   ELUOVCFD
02222      IF RRBL-REGARD-INDIVIDUAL   (RRBL-X-IDX)                     ELUOVCFD
02223      THEN                                                         ELUOVCFD
02224         CALL 'ELKFLAND'                                           ELUOVCFD
02225            USING WS-CF-WORKAREA                                   ELUOVCFD
02226                  WS-CF-WORKAREA                                   ELUOVCFD
02227                  ATBL-CF-INDIV (ATBL-X-IDX).                      ELUOVCFD
02228                                                                   ELUOVCFD
02229      IF RRBL-REGARD-INSTITUTIONAL (RRBL-X-IDX)                    ELUOVCFD
02230      THEN                                                         ELUOVCFD
02231         CALL 'ELKFLAND'                                           ELUOVCFD
02232            USING WS-CF-WORKAREA                                   ELUOVCFD
02233                  WS-CF-WORKAREA                                   ELUOVCFD
02234                  ATBL-CF-INST (ATBL-X-IDX).                       ELUOVCFD
02235                                                                   ELUOVCFD
02236      IF RRBL-REGARD-PROFESSIONAL (RRBL-X-IDX)                     ELUOVCFD
02237      THEN                                                         ELUOVCFD
02238         CALL 'ELKFLAND'                                           ELUOVCFD
02239            USING WS-CF-WORKAREA                                   ELUOVCFD
02240                  WS-CF-WORKAREA                                   ELUOVCFD
02241                  ATBL-CF-PROF (ATBL-X-IDX).                       ELUOVCFD
02242                                                                   ELUOVCFD
02243      IF RRBL-REGARD-BASIC        (RRBL-X-IDX)                     ELUOVCFD
02244      THEN                                                         ELUOVCFD
02245         CALL 'ELKFLAND'                                           ELUOVCFD
02246            USING WS-CF-WORKAREA                                   ELUOVCFD
02247                  WS-CF-WORKAREA                                   ELUOVCFD
02248                  ATBL-CF-BAS (ATBL-X-IDX).                        ELUOVCFD
02249                                                                   ELUOVCFD
02250      IF RRBL-REGARD-SUPPLEMENTAL (RRBL-X-IDX)                     ELUOVCFD
02251      THEN                                                         ELUOVCFD
02252         CALL 'ELKFLAND'                                           ELUOVCFD
02253            USING WS-CF-WORKAREA                                   ELUOVCFD
02254                  WS-CF-WORKAREA                                   ELUOVCFD
02255                  ATBL-CF-SUP (ATBL-X-IDX).                        ELUOVCFD
02256                                                                   ELUOVCFD
02257      IF RRBL-REGARD-INPATIENT    (RRBL-X-IDX)                     ELUOVCFD
02258      THEN                                                         ELUOVCFD
02259         CALL 'ELKFLAND'                                           ELUOVCFD
02260            USING WS-CF-WORKAREA                                   ELUOVCFD
02261                  WS-CF-WORKAREA                                   ELUOVCFD
02262                  ATBL-CF-IP (ATBL-X-IDX).                         ELUOVCFD
02263                                                                   ELUOVCFD
02264      IF RRBL-REGARD-OUTPATIENT   (RRBL-X-IDX)                     ELUOVCFD
02265      THEN                                                         ELUOVCFD
02266         CALL 'ELKFLAND'                                           ELUOVCFD
02267            USING WS-CF-WORKAREA                                   ELUOVCFD
02268                  WS-CF-WORKAREA                                   ELUOVCFD
02269                  ATBL-CF-OP (ATBL-X-IDX).                         ELUOVCFD
02270                                                                   ELUOVCFD
02271      IF RRBL-REGARD-PLAN         (RRBL-X-IDX)                     ELUOVCFD
02272      THEN                                                         ELUOVCFD
02273         CALL 'ELKFLAND'                                           ELUOVCFD
02274            USING WS-CF-WORKAREA                                   ELUOVCFD
02275                  WS-CF-WORKAREA                                   ELUOVCFD
02276                  ATBL-CF-PLAN (ATBL-X-IDX).                       ELUOVCFD
02277                                                                   ELUOVCFD
02278      IF RRBL-REGARD-NON-PLAN     (RRBL-X-IDX)                     ELUOVCFD
02279      THEN                                                         ELUOVCFD
02280         CALL 'ELKFLAND'                                           ELUOVCFD
02281            USING WS-CF-WORKAREA                                   ELUOVCFD
02282                  WS-CF-WORKAREA                                   ELUOVCFD
02283                  ATBL-CF-NON-PLAN (ATBL-X-IDX).                   ELUOVCFD
02284                                                                   ELUOVCFD
02285      CALL 'ELKFLAND'                                              ELUOVCFD
02286         USING WS-CF-WORKAREA                                      ELUOVCFD
02287               WS-CF-WORKAREA                                      ELUOVCFD
02288               ATBL-CF-OV (ATBL-X-IDX).                            ELUOVCFD
02289                                                                   ELUOVCFD
02290      MOVE WS-CF-WORKAREA TO ATBL-CF-WORK-ENTRY (ATBL-X-IDX).      ELUOVCFD
02291      SET RRBL-Y-IDX TO ATBL-X-IDX.                                ELUOVCFD
02292      MOVE ATBL-CF-WORK-ENTRY (ATBL-X-IDX)                         ELUOVCFD
02293        TO RRBL-CF-ENTRIES (RRBL-X-IDX RRBL-Y-IDX).                ELUOVCFD
02294                                                                   ELUOVCFD
02295 ************************************************************      ELUOVCFD
02296 *                                                          *      ELUOVCFD
02297 *        COMPARE FOR BEST OVERALL                          *      ELUOVCFD
02298 *                                                          *      ELUOVCFD
02299 ************************************************************      ELUOVCFD
02300                                                                   ELUOVCFD
02301  COMPARE-FOR-BEST-OVERALL.                                        ELUOVCFD
02302      IF ATBL-CF-WORK-ENTRY (ATBL-X-IDX) > WS-CF-THRESHHOLD        ELUOVCFD
02303      THEN                                                         ELUOVCFD
02304         SET RRBL-Y-IDX TO RRBL-HIGH-CF-ENTRY (RRBL-X-IDX)         ELUOVCFD
02305         IF ATBL-CF-WORK-ENTRY (ATBL-X-IDX) >                      ELUOVCFD
02306            RRBL-CF-ENTRIES (RRBL-X-IDX RRBL-Y-IDX)                ELUOVCFD
02307         THEN                                                      ELUOVCFD
02308            SET RRBL-HIGH-CF-ENTRY (RRBL-X-IDX) TO ATBL-X-IDX      ELUOVCFD
02309         ELSE                                                      ELUOVCFD
02310            CONTINUE                                               ELUOVCFD
02311      ELSE                                                         ELUOVCFD
02312         CONTINUE                                                  ELUOVCFD
02313      END-IF.                                                      ELUOVCFD
02314                                                                   ELUOVCFD
02315 /***********************************************************      ELUOVCFD
02316 *                                                          *      ELUOVCFD
02317 *        SIGNAL PROGRAM LOGIC                              *      ELUOVCFD
02318 *                                                          *      ELUOVCFD
02319 ************************************************************      ELUOVCFD
02320  SIGNAL-PROGRAM-LOGIC.                                            ELUOVCFD
02321      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELUOVCFD
02322      PERFORM SIGNAL-ABEND.                                        ELUOVCFD
