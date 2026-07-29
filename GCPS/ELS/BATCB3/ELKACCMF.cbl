00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKACCMF
00003  PROGRAM-ID.           ELKACCMF.                                     LV004
00004                                                                   ELKACCMF
00005  AUTHOR.               BARBARA KEIB.                              ELKACCMF
00006                                                                   ELKACCMF
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKACCMF
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKACCMF
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKACCMF
00010                        233 N. MICHIGAN AVE                        ELKACCMF
00011                        CHICAGO, ILLINOIS 60601                    ELKACCMF
00012                                                                   ELKACCMF
00013  DATE-WRITTEN.         12-NOV-1992.                               ELKACCMF
00014                                                                   ELKACCMF
00015  ENVIRONMENT DIVISION.                                            ELKACCMF
00016  CONFIGURATION SECTION.                                           ELKACCMF
00017  SOURCE-COMPUTER. IBM-3090.                                       ELKACCMF
00018  OBJECT-COMPUTER. IBM-3090.                                       ELKACCMF
00019                                                                   ELKACCMF
00020 ****************************************************************  ELKACCMF
00021 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKACCMF
00022 *  ELKACCMF :  COMPUTE CONFIDENCE FACTORS FOR ACCUMULATORS.    *  ELKACCMF
00023 *                                                              *  ELKACCMF
00024 *              THIS PROGRAM COMPUTES THE PARTIAL CONFIDENCE    *  ELKACCMF
00025 *              FACTORS FOR EACH AND EVERY ACCUMULATOR LOADED   *  ELKACCMF
00026 *              INTO THE ACCUMULATOR TABLES WHICH THE POINTERS  *  ELKACCMF
00027 *              IN THE CONTRACT SUMMARY ACCUMULATOR POINTER     *  ELKACCMF
00028 *              TABLE ADDRESS.  ONLY THOSE FACTORS THAT ARE     *  ELKACCMF
00029 *              COMMON TO ALL APPLICATIONS ARE COMPUTED.  THE   *  ELKACCMF
00030 *              CONFIDENCE FACTORS DETERMINED ARE:              *  ELKACCMF
00031 *                                                              *  ELKACCMF
00032 *              -ANNUAL     -  LIFETIME                         *  ELKACCMF
00033 *              -FAMILY     -  INDIVIDUAL                       *  ELKACCMF
00034 *              -INSTITUTIONAL:                                 *  ELKACCMF
00035 *               BASIC      -  SUPPLEMENTAL                     *  ELKACCMF
00036 *              -PROFESSIONAL:                                  *  ELKACCMF
00037 *               BASIC      -  SUPPLEMENTAL                     *  ELKACCMF
00038 *              -INPATIENT  -  OUTPATIENT                       *  ELKACCMF
00039 *              -PLAN       -  NON-PLAN                         *  ELKACCMF
00040 *              -OVERALL PER:                                   *  ELKACCMF
00041 *               BENEFIT PROVISIONS                             *  ELKACCMF
00042 *               CONDITION BITS                                 *  ELKACCMF
00043 *               COST CONTAINMENT                               *  ELKACCMF
00044 *               DIAGNOSES                                      *  ELKACCMF
00045 *               INTERNAL DESCRIPTOR                            *  ELKACCMF
00046 *               PLACE OF TREATMENT                             *  ELKACCMF
00047 *               PROCEDURE CODES                                *  ELKACCMF
00048 *               PROVIDER NUMBERS                               *  ELKACCMF
00049 *               PROVIDER TYPES                                 *  ELKACCMF
00050 *               PROVIDER SPECIALTY                             *  ELKACCMF
00051 *               SERVICE GROUP                                  *  ELKACCMF
00052 *               VALUE QUALIFER                                 *  ELKACCMF
00053 *              -OVERALL CONFIDENCE FACTOR                      *  ELKACCMF
00054 *                                                              *  ELKACCMF
00055 *              THE PARTIAL CONFIDENCE FACTORS ARE RETURNED IN  *  ELKACCMF
00056 *              THEIR RESPECTIVE CONTRACT SUMMARY ACCUMULATOR   *  ELKACCMF
00057 *              TABLE ENTRIES, AND ARE AVAILABLE FOR USE BY     *  ELKACCMF
00058 *              OTHER PROCESSES IN DETERMINING OVERALL AND      *  ELKACCMF
00059 *              SPECIAL CASE ACCUMULATORS.                      *  ELKACCMF
00060 *                                                              *  ELKACCMF
00061 *   NOTE:      THE VALUE OF BENEFIT PERIOD CONFIDENCE FACTOR   *  ELKACCMF
00062 *              CANNOT BE DETERMINED IN THIS MODULE.  ALSO THIS *  ELKACCMF
00063 *              PROGRAM INITIALIZES ALL SITUATIONAL VERSIONS TO *  ELKACCMF
00064 *              +0.0E+00 TO INSURE PROPER INITIALIZATION.       *  ELKACCMF
00065 *                                                              *  ELKACCMF
00066 *              SEE ELKACCMF DOCUMENTATION FOR FURTHER DETAILS. *  ELKACCMF
00067 ****************************************************************  ELKACCMF
00068 *                      MAINTENANCE HISTORY                     *  ELKACCMF
00069 *                                                              *  ELKACCMF
00070 *  MOD     DATE      BY  DRPT              ACTION              *  ELKACCMF
00071 * ----- ----------- --- ----- ---------------------------------*  ELKACCMF
00072 * 01.00 12-NOV-1992 BAK       CREATED                          *  ELKACCMF
00073 * 01.01 15-DEC-1992 BAK       CORRECT CFT3-POT LABEL.          *  ELKACCMF
00074 * 01.02 23-DEC-1992 BAK       CHANGE IP/OP-CALCULATIONS.       *  ELKACCMF
00075 * 01.03 10-MAR-1993 CGL       CHANGE CONFIDENCE FACTORS.       *  ELKACCMF
00076 *                             REPLACE ELSCFTB3 WITH ELSCFTBB   *  ELKACCMF
00077 * 01.04 18-MAR-1993 BAK       CHANGED COMPARES AFTER ANDS TO   *  ELKACCMF
00078 *                             CHECK FOR TRUE OR LESS THAN ZERO.*  ELKACCMF
00079 *                                                              *  ELKACCMF
00080 * 01.05 28-AUG-2000 AKK       CHANGED COMPARES AFTER ANDS TO   *  ELKACCMF
00081 *                             CHECK FOR TRUE OR LESS THAN ZERO.*  ELKACCMF
00082 *                                                              *  ELKACCMF
00083 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX       *  ELKACCMF
00084 *                             ASM RECOMPILES                   *  ELKACCMF
00085 *                                                              *  ELKACCMF
00086 * 02.00 07-MAY-2003 AKK       REGEN'D FOR CHANGES PROC AND DIAG*  ELKACCMF
00087 *                             CODES.                           *  ELKACCMF
00088 * 02.01 24-JUN-2003 AKK       RECOMPILE FOR CHANGES IN CALLED  *  ELKACCMF
00089 *                             PGM.                             *  ELKACCMF
00090 *                                                              *  ELKACCMF
00091 * 02.02 03-OCT-2003 GTF       RECOMPILE FOR CHANGES IN COPYBKS *  ELKACCMF
00092 *                             ELSCFTB5 & ELSCFTB7.             *  ELKACCMF
00093 * 02.03 09-JAN-2004 AKK       S0C7 INTERTEST                   *  ELKACCMF
00094 *                                                              *  ELKACCMF
00095 * 02.04 21-JAN-2004 AKK       HAD PARA 4450- IN PLACE OF       *  ELKACCMF
00096 *                             4451-SEARCH-CF-ALL-IPGS          *  ELKACCMF
00097 * 02.05 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKACCMF
00098 *                             CICSCB3 COMPILER FIX             *  ELKACCMF
00099 ****************************************************************  ELKACCMF
00100                                                                   ELKACCMF
00101                                                                   ELKACCMF
00102  DATA DIVISION.                                                   ELKACCMF
00103  WORKING-STORAGE SECTION.                                         ELKACCMF
00104                                                                   ELKACCMF
00105                                                                   ELKACCMF
00106  77  FILLER                      PIC X(42)   VALUE                ELKACCMF
00107      '***ELKACCMF WORKING STORAGE BEGINS HERE***'.                ELKACCMF
00108                                                                   ELKACCMF
00109                                                                   ELKACCMF
00110  01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.      ELKACCMF
00111                                                                   ELKACCMF
00112      88 WS-SUCCESSFULL-CALL       VALUE ZERO.                     ELKACCMF
00113      88 WS-UNIDENT-PARM          VALUE +8.                        ELKACCMF
00114      88 WS-MISSING-PARM          VALUE +12.                       ELKACCMF
00115      88 WS-INTERNAL-ERROR        VALUE +16.                       ELKACCMF
00116                                                                   ELKACCMF
00117                                                                   ELKACCMF
00118                                                                   ELKACCMF
00119  01  WS-TEST-SWITCHES            PIC X(05) VALUE 'YYYYY'.         ELKACCMF
00120                                                                   ELKACCMF
00121  01  WS-OTHER-SWITCHES.                                           ELKACCMF
00122                                                                   ELKACCMF
00123                                                                   ELKACCMF
00124      02 WS-BPV-SW                PIC X(01) VALUE 'N'.             ELKACCMF
00125                                                                   ELKACCMF
00126         88 WS-BPV-FOUND                    VALUE 'Y'.             ELKACCMF
00127         88 WS-BPV-NOT-FOUND                VALUE 'N'.             ELKACCMF
00128                                                                   ELKACCMF
00129      02 WS-INT-DESC              PIC X(01) VALUE 'N'.             ELKACCMF
00130                                                                   ELKACCMF
00131         88 WS-INT-DSCRPT-FOUND             VALUE 'Y'.             ELKACCMF
00132         88 WS-INT-DSCRPT-NOT-FOUND         VALUE 'N'.             ELKACCMF
00133                                                                   ELKACCMF
00134      02 WS-LOB                   PIC X(01) VALUE 'N'.             ELKACCMF
00135                                                                   ELKACCMF
00136         88 WS-LOB-FOUND                    VALUE 'Y'.             ELKACCMF
00137         88 WS-LOB-NOT-FOUND                VALUE 'N'.             ELKACCMF
00138                                                                   ELKACCMF
00139      02 WS-POT                   PIC X(01) VALUE 'N'.             ELKACCMF
00140                                                                   ELKACCMF
00141         88 WS-POT-FOUND                    VALUE 'Y'.             ELKACCMF
00142         88 WS-POT-NOT-FOUND                VALUE 'N'.             ELKACCMF
00143                                                                   ELKACCMF
00144      02 WS-VAL-QUAL              PIC X(01) VALUE 'N'.             ELKACCMF
00145         88 WS-VL-QLFR-FOUND                VALUE 'Y'.             ELKACCMF
00146         88 WS-VL-QLFR-NOT-FOUND            VALUE 'N'.             ELKACCMF
00147 *                                                                 ELKACCMF
00148 *                                                                 ELKACCMF
00149 *                                                                 ELKACCMF
00150  01 WS-ENTRY-SWITCH          PIC X(01) VALUE 'N'.                 ELKACCMF
00151     88 WS-ENTRY-FOUND                  VALUE 'Y'.                 ELKACCMF
00152     88 WS-ENTRY-NOT-FOUND              VALUE 'N'.                 ELKACCMF
00153 *                                                                 ELKACCMF
00154 *                                                                 ELKACCMF
00155 *                                                                 ELKACCMF
00156  01  WS-DEFAULT-CONFIDENCE-FACTORS.                               ELKACCMF
00157                                                                   ELKACCMF
00158      02 WS-CF-TRUE               COMP-1      VALUE +1.000000E+00. ELKACCMF
00159      02 WS-CF-FALSE              COMP-1      VALUE -1.000000E+00. ELKACCMF
00160      02 WS-CF-ZERO               COMP-1      VALUE +0.000000E+00. ELKACCMF
00161                                                                   ELKACCMF
00162  01  WS-WEIGHT-FACTORS.                                           ELKACCMF
00163                                                                   ELKACCMF
00164      02 WS-CW-50                 COMP-1      VALUE +0.500000E+00. ELKACCMF
00165      02 WS-CW-80                 COMP-1      VALUE +0.800000E+00. ELKACCMF
00166      02 WS-CW-85                 COMP-1      VALUE +0.850000E+00. ELKACCMF
00167      02 WS-CW-90                 COMP-1      VALUE +0.900000E+00. ELKACCMF
00168      02 WS-CW-95                 COMP-1      VALUE +0.950000E+00. ELKACCMF
00169 *                                                                 ELKACCMF
00170 *                                                                 ELKACCMF
00171 *                                                                 ELKACCMF
00172 *                                                                 ELKACCMF
00173                                                                   ELKACCMF
00174                                                                   ELKACCMF
00175  01  WS-CONFIDENCE-FACTORS.                                       ELKACCMF
00176                                                                   ELKACCMF
00177      02 WS-CF-ANL                COMP-1.                          ELKACCMF
00178      02 WS-CF-BNFT-PRD           COMP-1.                          ELKACCMF
00179      02 WS-CF-FMLY               COMP-1.                          ELKACCMF
00180      02 WS-CF-LFTM               COMP-1.                          ELKACCMF
00181      02 WS-CF-INDVDL             COMP-1.                          ELKACCMF
00182      02 WS-CF-INST-BAS           COMP-1.                          ELKACCMF
00183      02 WS-CF-INST-BAS-LOB       COMP-1.                          ELKACCMF
00184      02 WS-CF-INST-IBGR          COMP-1.                          ELKACCMF
00185      02 WS-CF-INST-IPGP          COMP-1.                          ELKACCMF
00186      02 WS-CF-INST-IPGT          COMP-1.                          ELKACCMF
00187      02 WS-CF-INST-SUP           COMP-1.                          ELKACCMF
00188      02 WS-CF-INST-SUP-LOB       COMP-1.                          ELKACCMF
00189      02 WS-CF-IP                 COMP-1.                          ELKACCMF
00190      02 WS-CF-IP-BOTH-IBGR       COMP-1.                          ELKACCMF
00191      02 WS-CF-IP-IBGR            COMP-1.                          ELKACCMF
00192      02 WS-CF-IP-PLC-TRTMNT      COMP-1.                          ELKACCMF
00193      02 WS-CF-NON-PLAN           COMP-1.                          ELKACCMF
00194      02 WS-CF-PLAN               COMP-1.                          ELKACCMF
00195      02 WS-CF-PROF-BAS           COMP-1.                          ELKACCMF
00196      02 WS-CF-PROF-BAS-LOB       COMP-1.                          ELKACCMF
00197      02 WS-CF-PROF-IBGR          COMP-1.                          ELKACCMF
00198      02 WS-CF-PROF-IPGP          COMP-1.                          ELKACCMF
00199      02 WS-CF-PROF-IPGT          COMP-1.                          ELKACCMF
00200      02 WS-CF-PROF-IPGS          COMP-1.                          ELKACCMF
00201      02 WS-CF-PROF-SUP           COMP-1.                          ELKACCMF
00202      02 WS-CF-PROF-SUP-LOB       COMP-1.                          ELKACCMF
00203      02 WS-CF-OP                 COMP-1.                          ELKACCMF
00204      02 WS-CF-OP-BOTH-IBGR       COMP-1.                          ELKACCMF
00205      02 WS-CF-OP-IBGR            COMP-1.                          ELKACCMF
00206      02 WS-CF-OP-PLC-TRTMNT      COMP-1.                          ELKACCMF
00207      02 WS-CF-OV                 COMP-1.                          ELKACCMF
00208      02 WS-CF-OV-CNDTN-BTS       COMP-1.                          ELKACCMF
00209      02 WS-CF-OV-CST-CNTNMT      COMP-1.                          ELKACCMF
00210      02 WS-CF-OV-IBGR            COMP-1.                          ELKACCMF
00211      02 WS-CF-OV-IDGD            COMP-1.                          ELKACCMF
00212      02 WS-CF-OV-INT-DSCRPT      COMP-1.                          ELKACCMF
00213      02 WS-CF-OV-INT-DSCRPT-INST COMP-1.                          ELKACCMF
00214      02 WS-CF-OV-INT-DSCRPT-PROF COMP-1.                          ELKACCMF
00215      02 WS-CF-OV-IPGN            COMP-1.                          ELKACCMF
00216      02 WS-CF-OV-IPGP            COMP-1.                          ELKACCMF
00217      02 WS-CF-OV-IPGT            COMP-1.                          ELKACCMF
00218      02 WS-CF-OV-IPGS            COMP-1.                          ELKACCMF
00219      02 WS-CF-OV-PLC-TRTMNT      COMP-1.                          ELKACCMF
00220      02 WS-CF-OV-SRVC-GRP        COMP-1.                          ELKACCMF
00221      02 WS-CF-OV-VL-QLFR         COMP-1.                          ELKACCMF
00222      02 WS-CF-OV-VL-QLFR-INST    COMP-1.                          ELKACCMF
00223      02 WS-CF-OV-VL-QLFR-PROF    COMP-1.                          ELKACCMF
00224 *                                                                 ELKACCMF
00225 *                                                                 ELKACCMF
00226 *                                                                 ELKACCMF
00227 *                                                                 ELKACCMF
00228  01  WS-WEIGHTED-CONFIDENCE-FACTORS.                              ELKACCMF
00229                                                                   ELKACCMF
00230      02 WS-CW-INST-BAS-LOB       COMP-1.                          ELKACCMF
00231      02 WS-CW-INST-IBGR          COMP-1.                          ELKACCMF
00232      02 WS-CW-INST-IPGP          COMP-1.                          ELKACCMF
00233      02 WS-CW-INST-IPGT          COMP-1.                          ELKACCMF
00234      02 WS-CW-INST-IPGS          COMP-1.                          ELKACCMF
00235      02 WS-CW-INST-SUP-LOB       COMP-1.                          ELKACCMF
00236      02 WS-CW-OV-IBGR            COMP-1.                          ELKACCMF
00237      02 WS-CW-OV-IDGD            COMP-1.                          ELKACCMF
00238      02 WS-CW-OV-IPGN            COMP-1.                          ELKACCMF
00239      02 WS-CW-OV-IPGP            COMP-1.                          ELKACCMF
00240      02 WS-CW-OV-IPGT            COMP-1.                          ELKACCMF
00241      02 WS-CW-OV-IPGS            COMP-1.                          ELKACCMF
00242      02 WS-CW-IP-PLC-TRTMNT      COMP-1.                          ELKACCMF
00243      02 WS-CW-IP-BOTH-IBGR       COMP-1.                          ELKACCMF
00244      02 WS-CW-OP-PLC-TRTMNT      COMP-1.                          ELKACCMF
00245      02 WS-CW-OP-BOTH-IBGR       COMP-1.                          ELKACCMF
00246      02 WS-CW-OV-CNDTN-BTS       COMP-1.                          ELKACCMF
00247      02 WS-CW-OV-CST-CNTNMT      COMP-1.                          ELKACCMF
00248      02 WS-CW-OV-INT-DSCRPT      COMP-1.                          ELKACCMF
00249      02 WS-CW-OV-PLC-TRTMNT      COMP-1.                          ELKACCMF
00250      02 WS-CW-OV-SRVC-GRP        COMP-1.                          ELKACCMF
00251      02 WS-CW-OV-VL-QLFR         COMP-1.                          ELKACCMF
00252      02 WS-CW-PROF-BAS-LOB       COMP-1.                          ELKACCMF
00253      02 WS-CW-PROF-IBGR          COMP-1.                          ELKACCMF
00254      02 WS-CW-PROF-IPGP          COMP-1.                          ELKACCMF
00255      02 WS-CW-PROF-IPGT          COMP-1.                          ELKACCMF
00256      02 WS-CW-PROF-IPGS          COMP-1.                          ELKACCMF
00257      02 WS-CW-PROF-SUP-LOB       COMP-1.                          ELKACCMF
00258 *                                                                 ELKACCMF
00259 *                                                                 ELKACCMF
00260  01  WS-WORK-CONFIDENCE-FACTORS.                                  ELKACCMF
00261                                                                   ELKACCMF
00262      02 WS-CF-0                  COMP-1.                          ELKACCMF
00263      02 WS-CF-1                  COMP-1.                          ELKACCMF
00264      02 WS-CF-2                  COMP-1.                          ELKACCMF
00265      02 WS-CF-3                  COMP-1.                          ELKACCMF
00266      02 WS-CF-4                  COMP-1.                          ELKACCMF
00267      02 WS-CF-5                  COMP-1.                          ELKACCMF
00268      02 WS-CF-6                  COMP-1.                          ELKACCMF
00269      02 WS-CF-7                  COMP-1.                          ELKACCMF
00270      02 WS-CW-8                  COMP-1.                          ELKACCMF
00271      02 WS-CW-9                  COMP-1.                          ELKACCMF
00272      02 WS-CF-10                 COMP-1.                          ELKACCMF
00273      02 WS-CF-11                 COMP-1.                          ELKACCMF
00274      02 WS-CF-12                 COMP-1.                          ELKACCMF
00275 /                                                                 ELKACCMF
00276      COPY ELSCFTBB.                                               ELKACCMF
00277 *                                                                 ELKACCMF
00278      COPY ELSCFTB5.                                               ELKACCMF
00279 *                                                                 ELKACCMF
00280      COPY ELSCFTB6.                                               ELKACCMF
00281 *                                                                 ELKACCMF
00282      COPY ELSCFTB7.                                               ELKACCMF
00283 *                                                                 ELKACCMF
00284      COPY ELSCFTB8.                                               ELKACCMF
00285 *                                                                 ELKACCMF
00286  LINKAGE SECTION.                                                 ELKACCMF
00287  01  DFHCOMMAREA.                                                 ELKACCMF
00288 /                                                                 ELKACCMF
00289      COPY ELSCOMMC.                                               ELKACCMF
00290 *                                                                 ELKACCMF
00291      COPY ELSCIA2C.                                               ELKACCMF
00292 *                                                                 ELKACCMF
00293      COPY ELSCSACC.                                               ELKACCMF
00294 *                                                                 ELKACCMF
00295      COPY ELSATBLC.                                               ELKACCMF
00296 *                                                                 ELKACCMF
00297      COPY ELSIBGRC.                                               ELKACCMF
00298 *                                                                 ELKACCMF
00299      COPY ELSIDGDC.                                               ELKACCMF
00300 *                                                                 ELKACCMF
00301      COPY ELSIPGNC.                                               ELKACCMF
00302 *                                                                 ELKACCMF
00303      COPY ELSIPGPC.                                               ELKACCMF
00304 *                                                                 ELKACCMF
00305      COPY ELSIPGTC.                                               ELKACCMF
00306 *                                                                 ELKACCMF
00307      COPY ELSIPGSC.                                               ELKACCMF
00308 *                                                                 ELKACCMF
00309   01  LS-MATCH-FACTOR-LIST            PIC X.                      ELKACCMF
00310 /***********************************************************      ELKACCMF
00311 *                                                          *      ELKACCMF
00312 *    COMPUTE CONFIDENCE FACTORS FOR ACCUMULATORS           *      ELKACCMF
00313 *                                                          *      ELKACCMF
00314 ************************************************************      ELKACCMF
00315                                                                   ELKACCMF
00316  PROCEDURE DIVISION USING CSAC-ACCUMULATOR-TABLE                  ELKACCMF
00317                           IBGR-INTERNAL-TABS-TABLE                ELKACCMF
00318                           IDGD-INTERNAL-TABS-TABLE                ELKACCMF
00319                           IPGN-INTERNAL-TABS-TABLE                ELKACCMF
00320                           IPGP-INTERNAL-TABS-TABLE                ELKACCMF
00321                           IPGT-INTERNAL-TABS-TABLE                ELKACCMF
00322                           IPGS-INTERNAL-TABS-TABLE.               ELKACCMF
00323                                                                   ELKACCMF
00324      IF ADDRESS OF CSAC-ACCUMULATOR-TABLE = NULL                  ELKACCMF
00325         SET WS-MISSING-PARM TO TRUE                               ELKACCMF
00326      ELSE                                                         ELKACCMF
00327         MOVE ZERO TO RETURN-CODE                                  ELKACCMF
00328         MOVE ZERO TO WS-RETURN-CODE                               ELKACCMF
00329         PERFORM 1000-COMPUTE-INT-TABS.                            ELKACCMF
00330      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00331         PERFORM 2000-COMPUTE-ACCUM-CONF-FACT.                     ELKACCMF
00332      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKACCMF
00333      GOBACK.                                                      ELKACCMF
00334                                                                   ELKACCMF
00335 ************************************************************      ELKACCMF
00336 *                                                          *      ELKACCMF
00337 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS          *      ELKACCMF
00338 *                                                          *      ELKACCMF
00339 ************************************************************      ELKACCMF
00340                                                                   ELKACCMF
00341  1000-COMPUTE-INT-TABS.                                           ELKACCMF
00342                                                                   ELKACCMF
00343      SET ADDRESS OF LS-MATCH-FACTOR-LIST TO NULL.                 ELKACCMF
00344      PERFORM 1100-COMPUTE-IBGR.                                   ELKACCMF
00345      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00346         PERFORM 1200-COMPUTE-IDGD.                                ELKACCMF
00347      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00348         PERFORM 1300-COMPUTE-IPGN.                                ELKACCMF
00349      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00350         PERFORM 1400-COMPUTE-IPGP.                                ELKACCMF
00351      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00352         PERFORM 1500-COMPUTE-IPGT.                                ELKACCMF
00353      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00354         PERFORM 1600-COMPUTE-IPGS.                                ELKACCMF
00355                                                                   ELKACCMF
00356 ************************************************************      ELKACCMF
00357 *                                                          *      ELKACCMF
00358 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IBGR   *      ELKACCMF
00359 *                                                          *      ELKACCMF
00360 ************************************************************      ELKACCMF
00361                                                                   ELKACCMF
00362  1100-COMPUTE-IBGR.                                               ELKACCMF
00363                                                                   ELKACCMF
00364      SET ADDRESS OF LS-MATCH-FACTOR-LIST TO NULL.                 ELKACCMF
00365      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00366         SET IBGR-CF-CALC-OV TO TRUE                               ELKACCMF
00367         CALL 'ELKIBGRF' USING IBGR-INTERNAL-TABS-TABLE            ELKACCMF
00368                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00369         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00370                                                                   ELKACCMF
00371 ************************************************************      ELKACCMF
00372 *                                                          *      ELKACCMF
00373 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IDGD   *      ELKACCMF
00374 *                                                          *      ELKACCMF
00375 ************************************************************      ELKACCMF
00376                                                                   ELKACCMF
00377  1200-COMPUTE-IDGD.                                               ELKACCMF
00378                                                                   ELKACCMF
00379      IF ADDRESS OF IDGD-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00380         SET IDGD-CF-CALC-OV TO TRUE                               ELKACCMF
00381         CALL 'ELKIDGDF' USING IDGD-INTERNAL-TABS-TABLE            ELKACCMF
00382                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00383         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00384                                                                   ELKACCMF
00385 ************************************************************      ELKACCMF
00386 *                                                          *      ELKACCMF
00387 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IPGN   *      ELKACCMF
00388 *                                                          *      ELKACCMF
00389 ************************************************************      ELKACCMF
00390                                                                   ELKACCMF
00391  1300-COMPUTE-IPGN.                                               ELKACCMF
00392                                                                   ELKACCMF
00393      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00394         SET IPGN-CF-CALC-OV TO TRUE                               ELKACCMF
00395         CALL 'ELKIPGNF' USING IPGN-INTERNAL-TABS-TABLE            ELKACCMF
00396                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00397         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00398 ************************************************************      ELKACCMF
00399 *                                                          *      ELKACCMF
00400 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IPGP   *      ELKACCMF
00401 *                                                          *      ELKACCMF
00402 ************************************************************      ELKACCMF
00403                                                                   ELKACCMF
00404  1400-COMPUTE-IPGP.                                               ELKACCMF
00405                                                                   ELKACCMF
00406      IF ADDRESS OF IPGP-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00407         SET IPGP-CF-CALC-OV TO TRUE                               ELKACCMF
00408         CALL 'ELKIPGPF' USING IPGP-INTERNAL-TABS-TABLE            ELKACCMF
00409                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00410         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00411 ************************************************************      ELKACCMF
00412 *                                                          *      ELKACCMF
00413 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IPGT   *      ELKACCMF
00414 *                                                          *      ELKACCMF
00415 ************************************************************      ELKACCMF
00416                                                                   ELKACCMF
00417  1500-COMPUTE-IPGT.                                               ELKACCMF
00418                                                                   ELKACCMF
00419      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00420         SET IPGT-CF-CALC-OV TO TRUE                               ELKACCMF
00421         CALL 'ELKIPGTF' USING IPGT-INTERNAL-TABS-TABLE            ELKACCMF
00422                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00423         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00424 ************************************************************      ELKACCMF
00425 *                                                          *      ELKACCMF
00426 *     COMPUTE INTERNAL TABULAR CONFIDENCE FACTORS - IPGS   *      ELKACCMF
00427 *                                                          *      ELKACCMF
00428 ************************************************************      ELKACCMF
00429                                                                   ELKACCMF
00430  1600-COMPUTE-IPGS.                                               ELKACCMF
00431                                                                   ELKACCMF
00432      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE NOT EQUAL NULL        ELKACCMF
00433         SET IPGS-CF-CALC-OV TO TRUE                               ELKACCMF
00434         CALL 'ELKIPGSF' USING IPGS-INTERNAL-TABS-TABLE            ELKACCMF
00435                             LS-MATCH-FACTOR-LIST                  ELKACCMF
00436         MOVE RETURN-CODE TO WS-RETURN-CODE.                       ELKACCMF
00437 ************************************************************      ELKACCMF
00438 *                                                          *      ELKACCMF
00439 *     COMPUTE ACCUMULATOR CONFIDENCE FACTORS               *      ELKACCMF
00440 *                                                          *      ELKACCMF
00441 ************************************************************      ELKACCMF
00442                                                                   ELKACCMF
00443  2000-COMPUTE-ACCUM-CONF-FACT.                                    ELKACCMF
00444                                                                   ELKACCMF
00445      IF CSAC-ABM-GC-TBL-PTR NOT = NULLS                           ELKACCMF
00446         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00447                        CSAC-ABM-GC-TBL-PTR                        ELKACCMF
00448         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00449                                                                   ELKACCMF
00450      IF CSAC-ABM-BP-TBL-PTR NOT = NULLS                           ELKACCMF
00451         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00452                        CSAC-ABM-BP-TBL-PTR                        ELKACCMF
00453         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00454                                                                   ELKACCMF
00455      IF CSAC-ACL-GC-TBL-PTR NOT = NULLS                           ELKACCMF
00456         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00457                        CSAC-ACL-GC-TBL-PTR                        ELKACCMF
00458         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00459                                                                   ELKACCMF
00460      IF CSAC-ACL-BP-TBL-PTR NOT = NULLS                           ELKACCMF
00461         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00462                        CSAC-ACL-BP-TBL-PTR                        ELKACCMF
00463         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00464                                                                   ELKACCMF
00465      IF CSAC-ADL-GC-TBL-PTR NOT = NULLS                           ELKACCMF
00466         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00467                        CSAC-ADL-GC-TBL-PTR                        ELKACCMF
00468         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00469                                                                   ELKACCMF
00470      IF CSAC-ADL-BP-TBL-PTR NOT = NULLS                           ELKACCMF
00471         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00472                        CSAC-ADL-BP-TBL-PTR                        ELKACCMF
00473         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00474                                                                   ELKACCMF
00475      IF CSAC-AOL-GC-TBL-PTR NOT = NULLS                           ELKACCMF
00476         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00477                        CSAC-AOL-GC-TBL-PTR                        ELKACCMF
00478         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00479                                                                   ELKACCMF
00480      IF CSAC-AOL-BP-TBL-PTR NOT = NULLS                           ELKACCMF
00481         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00482                        CSAC-AOL-BP-TBL-PTR                        ELKACCMF
00483         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00484                                                                   ELKACCMF
00485      IF CSAC-ACP-GC-TBL-PTR NOT = NULLS                           ELKACCMF
00486         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00487                        CSAC-ACP-GC-TBL-PTR                        ELKACCMF
00488         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00489                                                                   ELKACCMF
00490      IF CSAC-ACP-BP-TBL-PTR NOT = NULLS                           ELKACCMF
00491         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                  ELKACCMF
00492                        CSAC-ACP-BP-TBL-PTR                        ELKACCMF
00493         PERFORM 3000-PROCESS-ATBL-ENTRIES.                        ELKACCMF
00494                                                                   ELKACCMF
00495 ************************************************************      ELKACCMF
00496 *                                                          *      ELKACCMF
00497 *        PROCESS ALL ATBL ENTRIES                          *      ELKACCMF
00498 *                                                          *      ELKACCMF
00499 ************************************************************      ELKACCMF
00500                                                                   ELKACCMF
00501  3000-PROCESS-ATBL-ENTRIES.                                       ELKACCMF
00502                                                                   ELKACCMF
00503      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELKACCMF
00504      PERFORM 3100-COMPUTE-ALL-ACCUM-CF                            ELKACCMF
00505         VARYING ATBL-IDX FROM 1 BY 1                              ELKACCMF
00506           UNTIL ATBL-IDX > ATBL-MAX-IDX.                          ELKACCMF
00507                                                                   ELKACCMF
00508 ************************************************************      ELKACCMF
00509 *                                                          *      ELKACCMF
00510 *     COMPUTE ALL ACCUMULATOR CONFIDENCE FACTORS           *      ELKACCMF
00511 *                                                          *      ELKACCMF
00512 ************************************************************      ELKACCMF
00513                                                                   ELKACCMF
00514  3100-COMPUTE-ALL-ACCUM-CF.                                       ELKACCMF
00515                                                                   ELKACCMF
00516      INITIALIZE WS-CONFIDENCE-FACTORS                             ELKACCMF
00517                 WS-WEIGHTED-CONFIDENCE-FACTORS                    ELKACCMF
00518                 ATBL-ATTR-CONFIDENCE-FACTORS (ATBL-IDX).          ELKACCMF
00519                                                                   ELKACCMF
00520      PERFORM 4000-DETER-CF-ALL-IBGR.                              ELKACCMF
00521      PERFORM 4100-DETER-CF-ALL-IDGD.                              ELKACCMF
00522      PERFORM 4200-DETER-CF-ALL-IPGN.                              ELKACCMF
00523      PERFORM 4300-DETER-CF-ALL-IPGP.                              ELKACCMF
00524      PERFORM 4400-DETER-CF-ALL-IPGT.                              ELKACCMF
00525      PERFORM 4500-DETER-CF-ALL-IPGS.                              ELKACCMF
00526                                                                   ELKACCMF
00527      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00528         PERFORM 3200-LOOK-ASSIGN-PRLIM.                           ELKACCMF
00529                                                                   ELKACCMF
00530 ************************************************************      ELKACCMF
00531 *                                                          *      ELKACCMF
00532 *     LOOKUP AND ASSIGN PRILIMINARY FACTORS                *      ELKACCMF
00533 *                                                          *      ELKACCMF
00534 ************************************************************      ELKACCMF
00535                                                                   ELKACCMF
00536  3200-LOOK-ASSIGN-PRLIM.                                          ELKACCMF
00537                                                                   ELKACCMF
00538      PERFORM 5000-DETER-CF-BNFT-PRD.                              ELKACCMF
00539      PERFORM 5100-DETER-CF-FMLY-INDIV.                            ELKACCMF
00540      PERFORM 5200-DETER-CF-ALL-PLC-TRTMNT.                        ELKACCMF
00541      PERFORM 5300-DETER-CF-ALL-LOB.                               ELKACCMF
00542      PERFORM 5400-DETER-CF-OV-VL-QLFR.                            ELKACCMF
00543      PERFORM 5500-DETER-CF-OV-INT-DSCRPT.                         ELKACCMF
00544      PERFORM 5600-DETER-CF-OV-CNDTN-BTS.                          ELKACCMF
00545      PERFORM 5700-ASSIGN-OV-CST-CNTMNT.                           ELKACCMF
00546      PERFORM 5800-ASSIGN-OV-SRVC-GRP.                             ELKACCMF
00547      IF WS-OTHER-SWITCHES NOT EQUAL TO WS-TEST-SWITCHES           ELKACCMF
00548         SET WS-UNIDENT-PARM TO TRUE.                              ELKACCMF
00549      IF WS-SUCCESSFULL-CALL                                       ELKACCMF
00550         PERFORM 3300-COMPUTE-FACTORS.                             ELKACCMF
00551                                                                   ELKACCMF
00552 ************************************************************      ELKACCMF
00553 *                                                          *      ELKACCMF
00554 *     COMPUTE CATEGORICAL AND OVERALL FACTORS              *      ELKACCMF
00555 *                                                          *      ELKACCMF
00556 ************************************************************      ELKACCMF
00557                                                                   ELKACCMF
00558  3300-COMPUTE-FACTORS.                                            ELKACCMF
00559                                                                   ELKACCMF
00560      PERFORM 6000-COMPUTE-INPATIENT-FACT.                         ELKACCMF
00561      PERFORM 6100-COMPUTE-OUTPATIENT-FACT.                        ELKACCMF
00562      PERFORM 6200-COMPUTE-INST-BASIC-FACT.                        ELKACCMF
00563      PERFORM 6300-COMPUTE-INST-SUP-FACT.                          ELKACCMF
00564      PERFORM 6400-COMPUTE-PROF-BASIC-FACT.                        ELKACCMF
00565      PERFORM 6500-COMPUTE-PROF-SUP-FACT.                          ELKACCMF
00566      PERFORM 6600-COMPUTE-OV-PER-VL-QLFR.                         ELKACCMF
00567      PERFORM 6700-COMPUTE-OV-INT-DSCRPT.                          ELKACCMF
00568      PERFORM 6800-COMPUTE-OV-CONF-FACTOR.                         ELKACCMF
00569                                                                   ELKACCMF
00570      PERFORM 7000-MOVE-CF-WORK-TO-ATBL.                           ELKACCMF
00571                                                                   ELKACCMF
00572 ************************************************************      ELKACCMF
00573 *                                                          *      ELKACCMF
00574 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00575 *    #IBGR TABULAR RECORD-BENEFIT PROVISION                *      ELKACCMF
00576 *                                                          *      ELKACCMF
00577 ************************************************************      ELKACCMF
00578                                                                   ELKACCMF
00579  4000-DETER-CF-ALL-IBGR.                                          ELKACCMF
00580                                                                   ELKACCMF
00581      IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00582      THEN                                                         ELKACCMF
00583         MOVE WS-CF-ZERO TO WS-CF-INST-IBGR                        ELKACCMF
00584                            WS-CF-PROF-IBGR                        ELKACCMF
00585                            WS-CF-IP-IBGR                          ELKACCMF
00586                            WS-CF-IP-BOTH-IBGR                     ELKACCMF
00587                            WS-CF-OP-IBGR                          ELKACCMF
00588                            WS-CF-OP-BOTH-IBGR                     ELKACCMF
00589                            WS-CF-OV-IBGR                          ELKACCMF
00590                            ATBL-CF-OV-BNFT-PRVSN (ATBL-IDX)       ELKACCMF
00591      ELSE                                                         ELKACCMF
00592         PERFORM 4050-SEARCH-CF-ALL-IBGR.                          ELKACCMF
00593                                                                   ELKACCMF
00594 ************************************************************      ELKACCMF
00595 *                                                          *      ELKACCMF
00596 *    SEARCH THE IBGR CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00597 *    RETRIEVE VALUES-BENEFIT PROVISION                     *      ELKACCMF
00598 *                                                          *      ELKACCMF
00599 ************************************************************      ELKACCMF
00600                                                                   ELKACCMF
00601  4050-SEARCH-CF-ALL-IBGR.                                         ELKACCMF
00602                                                                   ELKACCMF
00603      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00604      SET IBGR-MAX-IDX TO IBGR-TBL-CNT.                            ELKACCMF
00605      PERFORM VARYING IBGR-IDX FROM 1 BY 1                         ELKACCMF
00606          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00607               IBGR-IDX > IBGR-MAX-IDX                             ELKACCMF
00608         IF IBGR-SLOT-NUMBER (IBGR-IDX) =                          ELKACCMF
00609             ATBL-IBGR-SLOT-NUMBER (ATBL-IDX)                      ELKACCMF
00610         MOVE IBGR-CF-INST     (IBGR-IDX) TO WS-CF-INST-IBGR       ELKACCMF
00611         MOVE IBGR-CF-PROF     (IBGR-IDX) TO WS-CF-PROF-IBGR       ELKACCMF
00612         MOVE IBGR-CF-IP       (IBGR-IDX) TO WS-CF-IP-IBGR         ELKACCMF
00613         MOVE IBGR-CF-IP-BOTH  (IBGR-IDX) TO WS-CF-IP-BOTH-IBGR    ELKACCMF
00614         MOVE IBGR-CF-OP       (IBGR-IDX) TO WS-CF-OP-IBGR         ELKACCMF
00615         MOVE IBGR-CF-OP-BOTH  (IBGR-IDX) TO WS-CF-OP-BOTH-IBGR    ELKACCMF
00616         MOVE IBGR-CF-OV       (IBGR-IDX) TO WS-CF-OV-IBGR         ELKACCMF
00617         MOVE IBGR-CF-OV       (IBGR-IDX) TO                       ELKACCMF
00618                   ATBL-CF-OV-BNFT-PRVSN (ATBL-IDX)                ELKACCMF
00619         SET WS-ENTRY-FOUND TO TRUE                                ELKACCMF
00620         END-IF                                                    ELKACCMF
00621      END-PERFORM.                                                 ELKACCMF
00622      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00623            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00624                                                                   ELKACCMF
00625 ************************************************************      ELKACCMF
00626 *                                                          *      ELKACCMF
00627 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00628 *    #IDGD TABULAR RECORD-DIAGNOSIS                        *      ELKACCMF
00629 *                                                          *      ELKACCMF
00630 ************************************************************      ELKACCMF
00631                                                                   ELKACCMF
00632  4100-DETER-CF-ALL-IDGD.                                          ELKACCMF
00633                                                                   ELKACCMF
00634      IF ATBL-IDGD-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00635      THEN                                                         ELKACCMF
00636         MOVE WS-CF-ZERO TO ATBL-CF-OV-DGNSS (ATBL-IDX)            ELKACCMF
00637                            WS-CF-OV-IDGD                          ELKACCMF
00638      ELSE                                                         ELKACCMF
00639         PERFORM 4150-SEARCH-CF-ALL-IDGD.                          ELKACCMF
00640                                                                   ELKACCMF
00641                                                                   ELKACCMF
00642 ************************************************************      ELKACCMF
00643 *                                                          *      ELKACCMF
00644 *    SEARCH THE IDGD CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00645 *    RETRIEVE VALUES-DIAGNOSIS                             *      ELKACCMF
00646 *                                                          *      ELKACCMF
00647 ************************************************************      ELKACCMF
00648                                                                   ELKACCMF
00649  4150-SEARCH-CF-ALL-IDGD.                                         ELKACCMF
00650                                                                   ELKACCMF
00651      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00652      SET IDGD-MAX-IDX TO IDGD-TBL-CNT.                            ELKACCMF
00653      PERFORM VARYING IDGD-IDX FROM 1 BY 1                         ELKACCMF
00654          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00655               IDGD-IDX > IDGD-MAX-IDX                             ELKACCMF
00656         IF IDGD-SLOT-NUMBER (IDGD-IDX) =                          ELKACCMF
00657                ATBL-IDGD-SLOT-NUMBER (ATBL-IDX)                   ELKACCMF
00658            MOVE IDGD-CF-OV (IDGD-IDX) TO                          ELKACCMF
00659                           ATBL-CF-OV-DGNSS (ATBL-IDX)             ELKACCMF
00660            MOVE IDGD-CF-OV (IDGD-IDX) TO WS-CF-OV-IDGD            ELKACCMF
00661            SET WS-ENTRY-FOUND TO TRUE                             ELKACCMF
00662         END-IF                                                    ELKACCMF
00663      END-PERFORM.                                                 ELKACCMF
00664      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00665            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00666                                                                   ELKACCMF
00667 ************************************************************      ELKACCMF
00668 *                                                          *      ELKACCMF
00669 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00670 *    #IPGN TABULAR RECORD-PROVIDER NUMBER                  *      ELKACCMF
00671 *                                                          *      ELKACCMF
00672 ************************************************************      ELKACCMF
00673                                                                   ELKACCMF
00674  4200-DETER-CF-ALL-IPGN.                                          ELKACCMF
00675                                                                   ELKACCMF
00676      IF ATBL-IPGN-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00677      THEN                                                         ELKACCMF
00678         MOVE WS-CF-ZERO TO WS-CF-OV-IPGN                          ELKACCMF
00679                            ATBL-CF-OV-PRVDR-NBR (ATBL-IDX)        ELKACCMF
00680      ELSE                                                         ELKACCMF
00681         PERFORM 4250-SEARCH-CF-ALL-IPGN.                          ELKACCMF
00682                                                                   ELKACCMF
00683 ************************************************************      ELKACCMF
00684 *                                                          *      ELKACCMF
00685 *    SEARCH THE IPGN CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00686 *    RETRIEVE VALUES-PROVIDER NUMBER                       *      ELKACCMF
00687 *                                                          *      ELKACCMF
00688 ************************************************************      ELKACCMF
00689                                                                   ELKACCMF
00690  4250-SEARCH-CF-ALL-IPGN.                                         ELKACCMF
00691                                                                   ELKACCMF
00692      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00693      SET IPGN-MAX-IDX TO IPGN-TBL-CNT.                            ELKACCMF
00694      PERFORM VARYING IPGN-IDX FROM 1 BY 1                         ELKACCMF
00695          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00696               IPGN-IDX > IPGN-MAX-IDX                             ELKACCMF
00697         IF IPGN-SLOT-NUMBER (IPGN-IDX) =                          ELKACCMF
00698                    ATBL-IPGN-SLOT-NUMBER (ATBL-IDX)               ELKACCMF
00699            MOVE IPGN-CF-OV (IPGN-IDX) TO WS-CF-OV-IPGN            ELKACCMF
00700            MOVE IPGN-CF-OV (IPGN-IDX) TO                          ELKACCMF
00701                           ATBL-CF-OV-PRVDR-NBR (ATBL-IDX)         ELKACCMF
00702            SET WS-ENTRY-FOUND TO TRUE                             ELKACCMF
00703         END-IF                                                    ELKACCMF
00704      END-PERFORM.                                                 ELKACCMF
00705      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00706            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00707                                                                   ELKACCMF
00708 ************************************************************      ELKACCMF
00709 *                                                          *      ELKACCMF
00710 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00711 *    #IPGP TABULAR RECORD-PROCEDURE CODE                   *      ELKACCMF
00712 *                                                          *      ELKACCMF
00713 ************************************************************      ELKACCMF
00714                                                                   ELKACCMF
00715  4300-DETER-CF-ALL-IPGP.                                          ELKACCMF
00716                                                                   ELKACCMF
00717      IF ATBL-IPGP-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00718      THEN                                                         ELKACCMF
00719         MOVE WS-CF-ZERO TO ATBL-CF-OV-PRCDR (ATBL-IDX)            ELKACCMF
00720                            WS-CF-OV-IPGP                          ELKACCMF
00721                            WS-CF-INST-IPGP                        ELKACCMF
00722                            WS-CF-PROF-IPGP                        ELKACCMF
00723      ELSE                                                         ELKACCMF
00724         PERFORM 4350-SEARCH-CF-ALL-IPGP.                          ELKACCMF
00725                                                                   ELKACCMF
00726                                                                   ELKACCMF
00727 ************************************************************      ELKACCMF
00728 *                                                          *      ELKACCMF
00729 *    SEARCH THE IPGP CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00730 *    RETRIEVE VALUES-PROCEDURE CODES                       *      ELKACCMF
00731 *                                                          *      ELKACCMF
00732 ************************************************************      ELKACCMF
00733                                                                   ELKACCMF
00734  4350-SEARCH-CF-ALL-IPGP.                                         ELKACCMF
00735                                                                   ELKACCMF
00736      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00737      SET IPGP-MAX-IDX TO IPGP-TBL-CNT.                            ELKACCMF
00738      PERFORM VARYING IPGP-IDX FROM 1 BY 1                         ELKACCMF
00739          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00740               IPGP-IDX > IPGP-MAX-IDX                             ELKACCMF
00741         IF IPGP-SLOT-NUMBER (IPGP-IDX) =                          ELKACCMF
00742                    ATBL-IPGP-SLOT-NUMBER (ATBL-IDX)               ELKACCMF
00743            MOVE IPGP-CF-INST (IPGP-IDX) TO WS-CF-INST-IPGP        ELKACCMF
00744            MOVE IPGP-CF-PROF (IPGP-IDX) TO WS-CF-PROF-IPGP        ELKACCMF
00745            MOVE IPGP-CF-OV   (IPGP-IDX) TO WS-CF-OV-IPGP          ELKACCMF
00746            MOVE IPGP-CF-OV   (IPGN-IDX) TO                        ELKACCMF
00747                           ATBL-CF-OV-PRCDR (ATBL-IDX)             ELKACCMF
00748            SET WS-ENTRY-FOUND TO TRUE                             ELKACCMF
00749         END-IF                                                    ELKACCMF
00750      END-PERFORM.                                                 ELKACCMF
00751      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00752            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00753                                                                   ELKACCMF
00754 ************************************************************      ELKACCMF
00755 *                                                          *      ELKACCMF
00756 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00757 *    #IPGT TABULAR RECORD-PROVIDER TYPE                    *      ELKACCMF
00758 *                                                          *      ELKACCMF
00759 ************************************************************      ELKACCMF
00760                                                                   ELKACCMF
00761  4400-DETER-CF-ALL-IPGT.                                          ELKACCMF
00762                                                                   ELKACCMF
00763      IF ATBL-IPGT-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00764         MOVE WS-CF-ZERO TO WS-CF-INST-IPGT                        ELKACCMF
00765                            WS-CF-PROF-IPGT                        ELKACCMF
00766                            WS-CF-PLAN                             ELKACCMF
00767                            WS-CF-NON-PLAN                         ELKACCMF
00768                            WS-CF-OV-IPGT                          ELKACCMF
00769                            ATBL-CF-OV-PRVDR-TYP (ATBL-IDX)        ELKACCMF
00770      ELSE                                                         ELKACCMF
00771         PERFORM 4450-SEARCH-CF-ALL-IPGT.                          ELKACCMF
00772                                                                   ELKACCMF
00773 ************************************************************      ELKACCMF
00774 *                                                          *      ELKACCMF
00775 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00776 *    #IPGS TABULAR RECORD-PROVIDER SPECIALTY               *      ELKACCMF
00777 * THIS IS PROFESSIONAL ONLY                                *      ELKACCMF
00778 ************************************************************      ELKACCMF
00779                                                                   ELKACCMF
00780  4500-DETER-CF-ALL-IPGS.                                          ELKACCMF
00781                                                                   ELKACCMF
00782      IF ATBL-IPGS-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELKACCMF
00783         MOVE WS-CF-ZERO TO WS-CF-PROF-IPGS                        ELKACCMF
00784                            WS-CF-PLAN                             ELKACCMF
00785                            WS-CF-NON-PLAN                         ELKACCMF
00786                            WS-CF-OV-IPGS                          ELKACCMF
00787                            ATBL-CF-OV-PRVDR-SPC (ATBL-IDX)        ELKACCMF
00788      ELSE                                                         ELKACCMF
00789 *       PERFORM 4450-SEARCH-CF-ALL-IPGT.                          ELKACCMF
00790         PERFORM 4451-SEARCH-CF-ALL-IPGS.                          ELKACCMF
00791                                                                   ELKACCMF
00792 ************************************************************      ELKACCMF
00793 *                                                          *      ELKACCMF
00794 *    SEARCH THE IPGT CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00795 *    RETRIEVE VALUES-PROVIDER TYPE                         *      ELKACCMF
00796 *                                                          *      ELKACCMF
00797 ************************************************************      ELKACCMF
00798                                                                   ELKACCMF
00799  4450-SEARCH-CF-ALL-IPGT.                                         ELKACCMF
00800                                                                   ELKACCMF
00801      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00802      SET IPGT-MAX-IDX TO IPGT-TBL-CNT.                            ELKACCMF
00803      PERFORM VARYING IPGT-IDX FROM 1 BY 1                         ELKACCMF
00804          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00805               IPGT-IDX > IPGT-MAX-IDX                             ELKACCMF
00806         IF IPGT-SLOT-NUMBER (IPGT-IDX) =                          ELKACCMF
00807                    ATBL-IPGT-SLOT-NUMBER (ATBL-IDX)               ELKACCMF
00808            MOVE IPGT-CF-INST      (IPGT-IDX) TO WS-CF-INST-IPGT   ELKACCMF
00809            MOVE IPGT-CF-PROF      (IPGT-IDX) TO WS-CF-PROF-IPGT   ELKACCMF
00810            MOVE IPGT-CF-PLAN      (IPGT-IDX) TO WS-CF-PLAN        ELKACCMF
00811            MOVE IPGT-CF-NON-PLAN  (IPGT-IDX) TO WS-CF-NON-PLAN    ELKACCMF
00812            MOVE IPGT-CF-OV        (IPGT-IDX) TO WS-CF-OV-IPGT     ELKACCMF
00813            MOVE IPGT-CF-OV        (IPGT-IDX) TO                   ELKACCMF
00814                           ATBL-CF-OV-PRVDR-TYP (ATBL-IDX)         ELKACCMF
00815            SET WS-ENTRY-FOUND TO TRUE                             ELKACCMF
00816         END-IF                                                    ELKACCMF
00817      END-PERFORM.                                                 ELKACCMF
00818      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00819            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00820                                                                   ELKACCMF
00821 ************************************************************      ELKACCMF
00822 *                                                          *      ELKACCMF
00823 *    SEARCH THE IPGT CONFIDENCE FACTORS TABLE AND          *      ELKACCMF
00824 *    RETRIEVE VALUES-PROVIDER SPEC                         *      ELKACCMF
00825 *                                                          *      ELKACCMF
00826 ************************************************************      ELKACCMF
00827                                                                   ELKACCMF
00828  4451-SEARCH-CF-ALL-IPGS.                                         ELKACCMF
00829                                                                   ELKACCMF
00830      SET WS-ENTRY-NOT-FOUND TO TRUE.                              ELKACCMF
00831      SET IPGS-MAX-IDX TO IPGS-TBL-CNT.                            ELKACCMF
00832      PERFORM VARYING IPGS-IDX FROM 1 BY 1                         ELKACCMF
00833          UNTIL WS-ENTRY-FOUND OR                                  ELKACCMF
00834               IPGS-IDX > IPGS-MAX-IDX                             ELKACCMF
00835         IF IPGS-SLOT-NUMBER (IPGS-IDX) =                          ELKACCMF
00836                    ATBL-IPGS-SLOT-NUMBER (ATBL-IDX)               ELKACCMF
00837            MOVE IPGS-CF-PROF      (IPGS-IDX) TO WS-CF-PROF-IPGS   ELKACCMF
00838            MOVE IPGS-CF-PLAN      (IPGS-IDX) TO WS-CF-PLAN        ELKACCMF
00839            MOVE IPGS-CF-NON-PLAN  (IPGS-IDX) TO WS-CF-NON-PLAN    ELKACCMF
00840            MOVE IPGS-CF-OV        (IPGS-IDX) TO WS-CF-OV-IPGS     ELKACCMF
00841            MOVE IPGS-CF-OV        (IPGS-IDX) TO                   ELKACCMF
00842                           ATBL-CF-OV-PRVDR-SPC (ATBL-IDX)         ELKACCMF
00843            SET WS-ENTRY-FOUND TO TRUE                             ELKACCMF
00844         END-IF                                                    ELKACCMF
00845      END-PERFORM.                                                 ELKACCMF
00846      IF WS-ENTRY-NOT-FOUND                                        ELKACCMF
00847            SET WS-MISSING-PARM TO TRUE.                           ELKACCMF
00848                                                                   ELKACCMF
00849 ************************************************************      ELKACCMF
00850 *                                                          *      ELKACCMF
00851 *    DETERMINE BENEFIT PERIOD BASED  CONFIDENCE FACTORS    *      ELKACCMF
00852 *                                                          *      ELKACCMF
00853 ************************************************************      ELKACCMF
00854                                                                   ELKACCMF
00855  5000-DETER-CF-BNFT-PRD.                                          ELKACCMF
00856                                                                   ELKACCMF
00857      SET WS-BPV-NOT-FOUND TO TRUE.                                ELKACCMF
00858      MOVE WS-CF-FALSE TO                                          ELKACCMF
00859                         WS-CF-ANL,                                ELKACCMF
00860                         WS-CF-LFTM.                               ELKACCMF
00861      SET CFT7-MAX-IDX TO CFT7-NBR-ENTRS.                          ELKACCMF
00862      PERFORM VARYING CFT7-IDX FROM 1 BY 1                         ELKACCMF
00863          UNTIL WS-BPV-FOUND OR                                    ELKACCMF
00864               CFT7-IDX > CFT7-MAX-IDX                             ELKACCMF
00865         IF CFT7-BNFT-PRD (CFT7-IDX) =                             ELKACCMF
00866                  ATBL-BENEFIT-PERIOD (ATBL-IDX)                   ELKACCMF
00867            MOVE CFT7-CF-ANL (CFT7-IDX) TO WS-CF-ANL               ELKACCMF
00868            MOVE CFT7-CF-LFTM    (CFT7-IDX) TO WS-CF-LFTM          ELKACCMF
00869            SET WS-BPV-FOUND TO TRUE                               ELKACCMF
00870         END-IF                                                    ELKACCMF
00871      END-PERFORM.                                                 ELKACCMF
00872                                                                   ELKACCMF
00873 ************************************************************      ELKACCMF
00874 *                                                          *      ELKACCMF
00875 *    DETERMINE FAMILY AND INDIVIDUAL CONFIDENCE FACTORS    *      ELKACCMF
00876 *                                                          *      ELKACCMF
00877 ************************************************************      ELKACCMF
00878                                                                   ELKACCMF
00879  5100-DETER-CF-FMLY-INDIV.                                        ELKACCMF
00880                                                                   ELKACCMF
00881      IF ATBL-FAM-OR-INDIV (ATBL-IDX) = 'I'                        ELKACCMF
00882         MOVE WS-CF-FALSE TO WS-CF-FMLY                            ELKACCMF
00883         MOVE WS-CF-TRUE  TO WS-CF-INDVDL                          ELKACCMF
00884      ELSE                                                         ELKACCMF
00885         IF ATBL-FAM-OR-INDIV (ATBL-IDX) = 'F'                     ELKACCMF
00886            MOVE WS-CF-TRUE  TO WS-CF-FMLY                         ELKACCMF
00887            MOVE WS-CF-FALSE TO WS-CF-INDVDL                       ELKACCMF
00888      ELSE                                                         ELKACCMF
00889            SET WS-UNIDENT-PARM TO TRUE.                           ELKACCMF
00890                                                                   ELKACCMF
00891 ************************************************************      ELKACCMF
00892 *                                                          *      ELKACCMF
00893 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM         *      ELKACCMF
00894 *    PLACE OF TREATMENT                                    *      ELKACCMF
00895 *                                                          *      ELKACCMF
00896 ************************************************************      ELKACCMF
00897                                                                   ELKACCMF
00898  5200-DETER-CF-ALL-PLC-TRTMNT.                                    ELKACCMF
00899                                                                   ELKACCMF
00900      SET WS-POT-NOT-FOUND TO TRUE.                                ELKACCMF
00901      MOVE WS-CF-FALSE TO WS-CF-IP-PLC-TRTMNT,                     ELKACCMF
00902                          WS-CF-OP-PLC-TRTMNT,                     ELKACCMF
00903                          WS-CF-OV-PLC-TRTMNT.                     ELKACCMF
00904      SET CFTB-MAX-IDX TO CFTB-NBR-ENTRS.                          ELKACCMF
00905      PERFORM VARYING CFTB-IDX FROM 1 BY 1                         ELKACCMF
00906          UNTIL WS-POT-FOUND OR                                    ELKACCMF
00907               CFTB-IDX > CFTB-MAX-IDX                             ELKACCMF
00908         IF CFTB-CF-POT (CFTB-IDX) =                               ELKACCMF
00909              ATBL-PLACE-OF-TREATMENT (ATBL-IDX)                   ELKACCMF
00910         MOVE CFTB-CF-POT-INPT  (CFTB-IDX) TO WS-CF-IP-PLC-TRTMNT  ELKACCMF
00911         MOVE CFTB-CF-POT-OUTPT (CFTB-IDX) TO WS-CF-OP-PLC-TRTMNT  ELKACCMF
00912         MOVE CFTB-CF-POT-OV    (CFTB-IDX) TO WS-CF-OV-PLC-TRTMNT  ELKACCMF
00913         SET WS-POT-FOUND TO TRUE                                  ELKACCMF
00914         END-IF                                                    ELKACCMF
00915      END-PERFORM.                                                 ELKACCMF
00916                                                                   ELKACCMF
00917 ************************************************************      ELKACCMF
00918 *                                                          *      ELKACCMF
00919 *    DETERMINE ALL CONFIDENCE FACTORS DERIVED FROM THE     *      ELKACCMF
00920 *    LINE OF BUSINESS                                      *      ELKACCMF
00921 *                                                          *      ELKACCMF
00922 ************************************************************      ELKACCMF
00923                                                                   ELKACCMF
00924  5300-DETER-CF-ALL-LOB.                                           ELKACCMF
00925                                                                   ELKACCMF
00926      SET WS-LOB-NOT-FOUND TO TRUE.                                ELKACCMF
00927      MOVE WS-CF-FALSE TO WS-CF-INST-BAS-LOB                       ELKACCMF
00928                          WS-CF-PROF-BAS-LOB                       ELKACCMF
00929                          WS-CF-INST-SUP-LOB                       ELKACCMF
00930                          WS-CF-PROF-SUP-LOB.                      ELKACCMF
00931      SET CFT8-MAX-IDX TO CFT8-NBR-ENTRS.                          ELKACCMF
00932      PERFORM VARYING CFT8-IDX FROM 1 BY 1                         ELKACCMF
00933          UNTIL WS-LOB-FOUND OR                                    ELKACCMF
00934               CFT8-IDX > CFT8-MAX-IDX                             ELKACCMF
00935         IF CFT8-LOB (CFT8-IDX) = ATBL-L-O-B (ATBL-IDX)            ELKACCMF
00936            MOVE CFT8-CF-INST-BAS (CFT8-IDX) TO WS-CF-INST-BAS-LOB ELKACCMF
00937            MOVE CFT8-CF-PROF-BAS (CFT8-IDX) TO WS-CF-PROF-BAS-LOB ELKACCMF
00938            MOVE CFT8-CF-INST-SUP (CFT8-IDX) TO WS-CF-INST-SUP-LOB ELKACCMF
00939            MOVE CFT8-CF-PROF-SUP (CFT8-IDX) TO WS-CF-PROF-SUP-LOB ELKACCMF
00940            SET WS-LOB-FOUND TO TRUE                               ELKACCMF
00941         END-IF                                                    ELKACCMF
00942      END-PERFORM.                                                 ELKACCMF
00943                                                                   ELKACCMF
00944 ************************************************************      ELKACCMF
00945 *                                                          *      ELKACCMF
00946 *        DETERMINE VALUE QUALIFIER CF                      *      ELKACCMF
00947 *                                                          *      ELKACCMF
00948 ************************************************************      ELKACCMF
00949                                                                   ELKACCMF
00950  5400-DETER-CF-OV-VL-QLFR.                                        ELKACCMF
00951                                                                   ELKACCMF
00952      SET WS-VL-QLFR-NOT-FOUND TO TRUE.                            ELKACCMF
00953      MOVE WS-CF-FALSE TO WS-CF-OV-VL-QLFR-INST                    ELKACCMF
00954                          WS-CF-OV-VL-QLFR-PROF.                   ELKACCMF
00955      SET CFT6-MAX-IDX TO CFT6-NBR-ENTRS.                          ELKACCMF
00956      PERFORM VARYING CFT6-IDX FROM 1 BY 1                         ELKACCMF
00957          UNTIL WS-VL-QLFR-FOUND OR                                ELKACCMF
00958               CFT6-IDX > CFT6-MAX-IDX                             ELKACCMF
00959         IF CFT6-VALQL (CFT6-IDX) =                                ELKACCMF
00960               ATBL-VALUE-QUALIFIER (ATBL-IDX)                     ELKACCMF
00961            MOVE CFT6-CF-VALQL-INST (CFT6-IDX)                     ELKACCMF
00962              TO WS-CF-OV-VL-QLFR-INST                             ELKACCMF
00963            MOVE CFT6-CF-VALQL-PROF (CFT6-IDX)                     ELKACCMF
00964              TO WS-CF-OV-VL-QLFR-PROF                             ELKACCMF
00965            SET WS-VL-QLFR-FOUND TO TRUE                           ELKACCMF
00966         END-IF                                                    ELKACCMF
00967      END-PERFORM.                                                 ELKACCMF
00968                                                                   ELKACCMF
00969 ************************************************************      ELKACCMF
00970 *                                                          *      ELKACCMF
00971 *        DETERMINE INTERNAL DESCRIPTOR CF                  *      ELKACCMF
00972 *                                                          *      ELKACCMF
00973 ************************************************************      ELKACCMF
00974                                                                   ELKACCMF
00975  5500-DETER-CF-OV-INT-DSCRPT.                                     ELKACCMF
00976                                                                   ELKACCMF
00977      SET WS-INT-DSCRPT-NOT-FOUND TO TRUE.                         ELKACCMF
00978      MOVE WS-CF-FALSE TO WS-CF-OV-INT-DSCRPT-INST                 ELKACCMF
00979                          WS-CF-OV-INT-DSCRPT-PROF.                ELKACCMF
00980      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELKACCMF
00981      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELKACCMF
00982          UNTIL WS-INT-DSCRPT-FOUND OR                             ELKACCMF
00983               CFT5-IDX > CFT5-MAX-IDX                             ELKACCMF
00984         IF CFT5-INTD (CFT5-IDX) =                                 ELKACCMF
00985              ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX)                  ELKACCMF
00986            MOVE CFT5-CF-INTD-INST (CFT5-IDX)                      ELKACCMF
00987              TO WS-CF-OV-INT-DSCRPT-INST                          ELKACCMF
00988            MOVE CFT5-CF-INTD-PROF (CFT5-IDX)                      ELKACCMF
00989              TO WS-CF-OV-INT-DSCRPT-PROF                          ELKACCMF
00990            SET WS-INT-DSCRPT-FOUND TO TRUE                        ELKACCMF
00991         END-IF                                                    ELKACCMF
00992      END-PERFORM.                                                 ELKACCMF
00993                                                                   ELKACCMF
00994 ************************************************************      ELKACCMF
00995 *                                                          *      ELKACCMF
00996 *        DETERMINE CONDITION BIT CF                        *      ELKACCMF
00997 *                                                          *      ELKACCMF
00998 ************************************************************      ELKACCMF
00999                                                                   ELKACCMF
01000  5600-DETER-CF-OV-CNDTN-BTS.                                      ELKACCMF
01001                                                                   ELKACCMF
01002      MOVE WS-CW-95 TO WS-CF-OV-CNDTN-BTS.                         ELKACCMF
01003      IF    ATBL-COND-ALL-BIT (ATBL-IDX ) = '1'                    ELKACCMF
01004         OR ATBL-COND-ICD-BIT (ATBL-IDX ) = '1'                    ELKACCMF
01005      THEN                                                         ELKACCMF
01006         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELKACCMF
01007            AND ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1'            ELKACCMF
01008         THEN                                                      ELKACCMF
01009            MOVE WS-CF-FALSE TO WS-CF-OV-CNDTN-BTS                 ELKACCMF
01010         ELSE                                                      ELKACCMF
01011            MOVE WS-CW-85 TO WS-CF-OV-CNDTN-BTS                    ELKACCMF
01012         END-IF                                                    ELKACCMF
01013      ELSE                                                         ELKACCMF
01014         IF ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1'                ELKACCMF
01015         THEN                                                      ELKACCMF
01016            MOVE WS-CW-85 TO WS-CF-OV-CNDTN-BTS                    ELKACCMF
01017         ELSE                                                      ELKACCMF
01018            MOVE WS-CF-FALSE TO WS-CF-OV-CNDTN-BTS                 ELKACCMF
01019      END-IF.                                                      ELKACCMF
01020                                                                   ELKACCMF
01021 ************************************************************      ELKACCMF
01022 *                                                          *      ELKACCMF
01023 *        ASSIGN COST CONTAINMENT CONF FACTOR               *      ELKACCMF
01024 *                                                          *      ELKACCMF
01025 ************************************************************      ELKACCMF
01026                                                                   ELKACCMF
01027  5700-ASSIGN-OV-CST-CNTMNT.                                       ELKACCMF
01028                                                                   ELKACCMF
01029      IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'                   ELKACCMF
01030         MOVE WS-CF-TRUE TO WS-CF-OV-CST-CNTNMT                    ELKACCMF
01031      ELSE                                                         ELKACCMF
01032         MOVE WS-CF-FALSE TO WS-CF-OV-CST-CNTNMT                   ELKACCMF
01033      END-IF.                                                      ELKACCMF
01034                                                                   ELKACCMF
01035 ************************************************************      ELKACCMF
01036 *                                                          *      ELKACCMF
01037 *       ASSIGN OVERALL PER SERVICE GROUP CONF FACTOR       *      ELKACCMF
01038 *                                                          *      ELKACCMF
01039 ************************************************************      ELKACCMF
01040                                                                   ELKACCMF
01041  5800-ASSIGN-OV-SRVC-GRP.                                         ELKACCMF
01042                                                                   ELKACCMF
01043      IF ATBL-SERVICE-GROUP (ATBL-IDX) = '00'                      ELKACCMF
01044         MOVE WS-CF-TRUE TO WS-CF-OV-SRVC-GRP                      ELKACCMF
01045      ELSE                                                         ELKACCMF
01046         MOVE WS-CF-FALSE TO WS-CF-OV-SRVC-GRP                     ELKACCMF
01047      END-IF.                                                      ELKACCMF
01048                                                                   ELKACCMF
01049 ************************************************************      ELKACCMF
01050 *                                                          *      ELKACCMF
01051 *        COMPUTE   INPATIENT CONFIDENCE FACTOR             *      ELKACCMF
01052 *                                                          *      ELKACCMF
01053 ************************************************************      ELKACCMF
01054                                                                   ELKACCMF
01055  6000-COMPUTE-INPATIENT-FACT.                                     ELKACCMF
01056                                                                   ELKACCMF
01057      MOVE WS-CW-50 TO WS-CW-IP-BOTH-IBGR.                         ELKACCMF
01058      MOVE WS-CW-95 TO WS-CW-IP-PLC-TRTMNT.                        ELKACCMF
01059      CALL 'ELKFLAND'                                              ELKACCMF
01060         USING WS-CF-0                                             ELKACCMF
01061               WS-CF-IP-PLC-TRTMNT                                 ELKACCMF
01062               WS-CF-IP-BOTH-IBGR.                                 ELKACCMF
01063                                                                   ELKACCMF
01064      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01065          MOVE WS-CF-0  TO WS-CF-IP                                ELKACCMF
01066      ELSE                                                         ELKACCMF
01067          IF WS-CF-0 = WS-CF-TRUE                                  ELKACCMF
01068              MOVE WS-CF-0 TO WS-CF-IP                             ELKACCMF
01069          ELSE                                                     ELKACCMF
01070              COMPUTE WS-CF-1 = WS-CF-IP-PLC-TRTMNT *              ELKACCMF
01071                                WS-CW-IP-PLC-TRTMNT                ELKACCMF
01072              COMPUTE WS-CF-2 = WS-CF-IP-BOTH-IBGR  *              ELKACCMF
01073                                WS-CW-IP-BOTH-IBGR                 ELKACCMF
01074              CALL 'ELKFLCMB'                                      ELKACCMF
01075                 USING WS-CF-IP                                    ELKACCMF
01076                       WS-CF-1                                     ELKACCMF
01077                       WS-CF-2                                     ELKACCMF
01078          END-IF                                                   ELKACCMF
01079      END-IF.                                                      ELKACCMF
01080                                                                   ELKACCMF
01081 ************************************************************      ELKACCMF
01082 *                                                          *      ELKACCMF
01083 *        DETERMINE OUTPATIENT CONFIDENCE FACTOR            *      ELKACCMF
01084 *                                                          *      ELKACCMF
01085 ************************************************************      ELKACCMF
01086                                                                   ELKACCMF
01087  6100-COMPUTE-OUTPATIENT-FACT.                                    ELKACCMF
01088                                                                   ELKACCMF
01089      MOVE WS-CW-50 TO WS-CW-OP-BOTH-IBGR.                         ELKACCMF
01090      MOVE WS-CW-95 TO WS-CW-OP-PLC-TRTMNT.                        ELKACCMF
01091      CALL 'ELKFLAND'                                              ELKACCMF
01092         USING WS-CF-0                                             ELKACCMF
01093               WS-CF-OP-PLC-TRTMNT                                 ELKACCMF
01094               WS-CF-OP-BOTH-IBGR.                                 ELKACCMF
01095                                                                   ELKACCMF
01096      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01097          MOVE WS-CF-0  TO WS-CF-OP                                ELKACCMF
01098      ELSE                                                         ELKACCMF
01099          IF WS-CF-0 = WS-CF-TRUE                                  ELKACCMF
01100              MOVE WS-CF-0 TO WS-CF-OP                             ELKACCMF
01101          ELSE                                                     ELKACCMF
01102              COMPUTE WS-CF-1 = WS-CF-OP-PLC-TRTMNT *              ELKACCMF
01103                                WS-CW-OP-PLC-TRTMNT                ELKACCMF
01104              COMPUTE WS-CF-2 = WS-CF-OP-BOTH-IBGR  *              ELKACCMF
01105                                WS-CW-OP-BOTH-IBGR                 ELKACCMF
01106              CALL 'ELKFLCMB'                                      ELKACCMF
01107                 USING WS-CF-OP                                    ELKACCMF
01108                       WS-CF-1                                     ELKACCMF
01109                       WS-CF-2                                     ELKACCMF
01110          END-IF                                                   ELKACCMF
01111      END-IF.                                                      ELKACCMF
01112                                                                   ELKACCMF
01113 ************************************************************      ELKACCMF
01114 *                                                          *      ELKACCMF
01115 *    COMPUTE INSTITUTIONAL BASIC CONFIDENCE FACTOR         *      ELKACCMF
01116 *                                                          *      ELKACCMF
01117 ************************************************************      ELKACCMF
01118                                                                   ELKACCMF
01119  6200-COMPUTE-INST-BASIC-FACT.                                    ELKACCMF
01120                                                                   ELKACCMF
01121      COMPUTE WS-CW-INST-BAS-LOB = WS-CW-95.                       ELKACCMF
01122      COMPUTE WS-CW-INST-IPGT = WS-CW-95.                          ELKACCMF
01123      COMPUTE WS-CW-INST-IBGR = WS-CW-90.                          ELKACCMF
01124      COMPUTE WS-CW-INST-IPGP = WS-CW-85.                          ELKACCMF
01125                                                                   ELKACCMF
01126      CALL 'ELKFLAND'                                              ELKACCMF
01127         USING WS-CF-0                                             ELKACCMF
01128               WS-CF-INST-BAS-LOB                                  ELKACCMF
01129               WS-CF-INST-IBGR                                     ELKACCMF
01130               WS-CF-INST-IPGP.                                    ELKACCMF
01131                                                                   ELKACCMF
01132      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01133         MOVE WS-CF-0 TO WS-CF-INST-BAS                            ELKACCMF
01134      ELSE                                                         ELKACCMF
01135         IF WS-CF-0 = WS-CF-TRUE                                   ELKACCMF
01136            MOVE WS-CF-0 TO WS-CF-INST-BAS                         ELKACCMF
01137      ELSE                                                         ELKACCMF
01138                                                                   ELKACCMF
01139            COMPUTE WS-CF-1 =                                      ELKACCMF
01140                        WS-CF-INST-BAS-LOB * WS-CW-INST-BAS-LOB    ELKACCMF
01141            COMPUTE WS-CF-2 = WS-CF-INST-IPGT * WS-CW-INST-IPGT    ELKACCMF
01142            COMPUTE WS-CF-3 = WS-CF-INST-IBGR * WS-CW-INST-IBGR    ELKACCMF
01143            COMPUTE WS-CF-4 = WS-CF-INST-IPGP * WS-CW-INST-IPGP    ELKACCMF
01144            CALL 'ELKFLCMB'                                        ELKACCMF
01145              USING WS-CF-INST-BAS                                 ELKACCMF
01146                  WS-CF-1                                          ELKACCMF
01147                  WS-CF-2                                          ELKACCMF
01148                  WS-CF-3                                          ELKACCMF
01149                  WS-CF-4                                          ELKACCMF
01150         END-IF                                                    ELKACCMF
01151      END-IF.                                                      ELKACCMF
01152                                                                   ELKACCMF
01153 ************************************************************      ELKACCMF
01154 *                                                          *      ELKACCMF
01155 *    COMPUTE INSTITUTIONAL SUPPLEMENTAL CONFIDENCE FACTOR  *      ELKACCMF
01156 *                                                          *      ELKACCMF
01157 ************************************************************      ELKACCMF
01158                                                                   ELKACCMF
01159  6300-COMPUTE-INST-SUP-FACT.                                      ELKACCMF
01160                                                                   ELKACCMF
01161      COMPUTE WS-CW-INST-SUP-LOB = WS-CW-95.                       ELKACCMF
01162      COMPUTE WS-CW-INST-IPGT = WS-CW-95.                          ELKACCMF
01163      COMPUTE WS-CW-INST-IBGR = WS-CW-90.                          ELKACCMF
01164      COMPUTE WS-CW-INST-IPGP = WS-CW-85.                          ELKACCMF
01165                                                                   ELKACCMF
01166      CALL 'ELKFLAND'                                              ELKACCMF
01167         USING WS-CF-0                                             ELKACCMF
01168               WS-CF-INST-SUP-LOB                                  ELKACCMF
01169               WS-CF-INST-IPGT                                     ELKACCMF
01170               WS-CF-INST-IBGR                                     ELKACCMF
01171               WS-CF-INST-IPGP.                                    ELKACCMF
01172                                                                   ELKACCMF
01173      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01174         MOVE WS-CF-0 TO WS-CF-INST-SUP                            ELKACCMF
01175      ELSE                                                         ELKACCMF
01176         IF WS-CF-0 = WS-CF-TRUE                                   ELKACCMF
01177            MOVE WS-CF-0 TO WS-CF-INST-SUP                         ELKACCMF
01178      ELSE                                                         ELKACCMF
01179                                                                   ELKACCMF
01180            COMPUTE WS-CF-1 =                                      ELKACCMF
01181                        WS-CF-INST-SUP-LOB * WS-CW-INST-SUP-LOB    ELKACCMF
01182            COMPUTE WS-CF-2 = WS-CF-INST-IPGT * WS-CW-INST-IPGT    ELKACCMF
01183            COMPUTE WS-CF-3 = WS-CF-INST-IBGR * WS-CW-INST-IBGR    ELKACCMF
01184            COMPUTE WS-CF-4 = WS-CF-INST-IPGP * WS-CW-INST-IPGP    ELKACCMF
01185            CALL 'ELKFLCMB'                                        ELKACCMF
01186              USING WS-CF-INST-SUP                                 ELKACCMF
01187                  WS-CF-1                                          ELKACCMF
01188                  WS-CF-2                                          ELKACCMF
01189                  WS-CF-3                                          ELKACCMF
01190                  WS-CF-4                                          ELKACCMF
01191         END-IF                                                    ELKACCMF
01192      END-IF.                                                      ELKACCMF
01193                                                                   ELKACCMF
01194 ************************************************************      ELKACCMF
01195 *                                                          *      ELKACCMF
01196 *    COMPUTE PROFESSIONAL  BASIC CONFIDENCE FACTOR         *      ELKACCMF
01197 *                                                          *      ELKACCMF
01198 ************************************************************      ELKACCMF
01199                                                                   ELKACCMF
01200  6400-COMPUTE-PROF-BASIC-FACT.                                    ELKACCMF
01201                                                                   ELKACCMF
01202      COMPUTE WS-CW-PROF-BAS-LOB = WS-CW-95.                       ELKACCMF
01203      COMPUTE WS-CW-PROF-IPGT = WS-CW-95.                          ELKACCMF
01204      COMPUTE WS-CW-PROF-IPGS = WS-CW-95.                          ELKACCMF
01205      COMPUTE WS-CW-PROF-IBGR = WS-CW-90.                          ELKACCMF
01206      COMPUTE WS-CW-PROF-IPGP = WS-CW-85.                          ELKACCMF
01207                                                                   ELKACCMF
01208      CALL 'ELKFLAND'                                              ELKACCMF
01209         USING WS-CF-0                                             ELKACCMF
01210               WS-CF-PROF-BAS-LOB                                  ELKACCMF
01211               WS-CF-PROF-IPGT                                     ELKACCMF
01212               WS-CF-PROF-IPGS                                     ELKACCMF
01213               WS-CF-PROF-IBGR                                     ELKACCMF
01214               WS-CF-PROF-IPGP.                                    ELKACCMF
01215                                                                   ELKACCMF
01216      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01217         MOVE WS-CF-0 TO WS-CF-PROF-BAS                            ELKACCMF
01218      ELSE                                                         ELKACCMF
01219         IF WS-CF-0 = WS-CF-TRUE                                   ELKACCMF
01220            MOVE WS-CF-0 TO WS-CF-PROF-BAS                         ELKACCMF
01221      ELSE                                                         ELKACCMF
01222                                                                   ELKACCMF
01223            COMPUTE WS-CF-1 =                                      ELKACCMF
01224                        WS-CF-PROF-BAS-LOB * WS-CW-PROF-BAS-LOB    ELKACCMF
01225            COMPUTE WS-CF-2 = WS-CF-PROF-IPGT * WS-CW-PROF-IPGT    ELKACCMF
01226            COMPUTE WS-CF-3 = WS-CF-PROF-IBGR * WS-CW-PROF-IBGR    ELKACCMF
01227            COMPUTE WS-CF-4 = WS-CF-PROF-IPGP * WS-CW-PROF-IPGP    ELKACCMF
01228            COMPUTE WS-CF-5 = WS-CF-PROF-IPGS * WS-CW-PROF-IPGS    ELKACCMF
01229            CALL 'ELKFLCMB'                                        ELKACCMF
01230              USING WS-CF-PROF-BAS                                 ELKACCMF
01231                  WS-CF-1                                          ELKACCMF
01232                  WS-CF-2                                          ELKACCMF
01233                  WS-CF-3                                          ELKACCMF
01234                  WS-CF-4                                          ELKACCMF
01235                  WS-CF-5                                          ELKACCMF
01236         END-IF                                                    ELKACCMF
01237      END-IF.                                                      ELKACCMF
01238                                                                   ELKACCMF
01239 ************************************************************      ELKACCMF
01240 *                                                          *      ELKACCMF
01241 *    COMPUTE PROFESSIONAL  SUPPLEMENTAL CONFIDENCE FACTOR  *      ELKACCMF
01242 *                                                          *      ELKACCMF
01243 ************************************************************      ELKACCMF
01244                                                                   ELKACCMF
01245  6500-COMPUTE-PROF-SUP-FACT.                                      ELKACCMF
01246                                                                   ELKACCMF
01247      COMPUTE WS-CW-PROF-SUP-LOB = WS-CW-95.                       ELKACCMF
01248      COMPUTE WS-CW-PROF-IPGT = WS-CW-95.                          ELKACCMF
01249      COMPUTE WS-CW-PROF-IPGS = WS-CW-95.                          ELKACCMF
01250      COMPUTE WS-CW-PROF-IBGR = WS-CW-90.                          ELKACCMF
01251      COMPUTE WS-CW-PROF-IPGP = WS-CW-85.                          ELKACCMF
01252                                                                   ELKACCMF
01253      CALL 'ELKFLAND'                                              ELKACCMF
01254         USING WS-CF-0                                             ELKACCMF
01255               WS-CF-PROF-SUP-LOB                                  ELKACCMF
01256               WS-CF-PROF-IPGT                                     ELKACCMF
01257               WS-CF-PROF-IPGS                                     ELKACCMF
01258               WS-CF-PROF-IBGR                                     ELKACCMF
01259               WS-CF-PROF-IPGP.                                    ELKACCMF
01260                                                                   ELKACCMF
01261      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01262         MOVE WS-CF-0 TO WS-CF-PROF-SUP                            ELKACCMF
01263      ELSE                                                         ELKACCMF
01264         IF WS-CF-0 = WS-CF-TRUE                                   ELKACCMF
01265            MOVE WS-CF-0 TO WS-CF-PROF-SUP                         ELKACCMF
01266      ELSE                                                         ELKACCMF
01267                                                                   ELKACCMF
01268            COMPUTE WS-CF-1 =                                      ELKACCMF
01269                        WS-CF-PROF-SUP-LOB * WS-CW-PROF-SUP-LOB    ELKACCMF
01270            COMPUTE WS-CF-2 = WS-CF-PROF-IPGT * WS-CW-PROF-IPGT    ELKACCMF
01271            COMPUTE WS-CF-3 = WS-CF-PROF-IBGR * WS-CW-PROF-IBGR    ELKACCMF
01272            COMPUTE WS-CF-4 = WS-CF-PROF-IPGP * WS-CW-PROF-IPGP    ELKACCMF
01273            COMPUTE WS-CF-5 = WS-CF-PROF-IPGS * WS-CW-PROF-IPGS    ELKACCMF
01274            CALL 'ELKFLCMB'                                        ELKACCMF
01275              USING WS-CF-PROF-SUP                                 ELKACCMF
01276                  WS-CF-1                                          ELKACCMF
01277                  WS-CF-2                                          ELKACCMF
01278                  WS-CF-3                                          ELKACCMF
01279                  WS-CF-4                                          ELKACCMF
01280                  WS-CF-5                                          ELKACCMF
01281         END-IF                                                    ELKACCMF
01282      END-IF.                                                      ELKACCMF
01283                                                                   ELKACCMF
01284 ************************************************************      ELKACCMF
01285 *                                                          *      ELKACCMF
01286 *   COMPUTE OVERALL PER VALUE QUALIFER                     *      ELKACCMF
01287 *                                                          *      ELKACCMF
01288 ************************************************************      ELKACCMF
01289                                                                   ELKACCMF
01290  6600-COMPUTE-OV-PER-VL-QLFR.                                     ELKACCMF
01291                                                                   ELKACCMF
01292      CALL 'ELKFLOR'                                               ELKACCMF
01293         USING WS-CF-1                                             ELKACCMF
01294               WS-CF-INST-BAS                                      ELKACCMF
01295               WS-CF-INST-SUP.                                     ELKACCMF
01296                                                                   ELKACCMF
01297      CALL 'ELKFLAND'                                              ELKACCMF
01298         USING WS-CF-2                                             ELKACCMF
01299               WS-CF-OV-VL-QLFR-INST                               ELKACCMF
01300               WS-CF-1.                                            ELKACCMF
01301                                                                   ELKACCMF
01302      CALL 'ELKFLOR'                                               ELKACCMF
01303         USING WS-CF-3                                             ELKACCMF
01304               WS-CF-PROF-BAS                                      ELKACCMF
01305               WS-CF-PROF-SUP.                                     ELKACCMF
01306                                                                   ELKACCMF
01307      CALL 'ELKFLAND'                                              ELKACCMF
01308         USING WS-CF-4                                             ELKACCMF
01309               WS-CF-OV-VL-QLFR-PROF                               ELKACCMF
01310               WS-CF-3.                                            ELKACCMF
01311                                                                   ELKACCMF
01312      CALL 'ELKFLOR'                                               ELKACCMF
01313         USING WS-CF-OV-VL-QLFR                                    ELKACCMF
01314               WS-CF-2                                             ELKACCMF
01315               WS-CF-4.                                            ELKACCMF
01316                                                                   ELKACCMF
01317 ************************************************************      ELKACCMF
01318 *                                                          *      ELKACCMF
01319 *   COMPUTE OVERALL PER INTERNAL DESCRIPTOR                *      ELKACCMF
01320 *                                                          *      ELKACCMF
01321 ************************************************************      ELKACCMF
01322                                                                   ELKACCMF
01323  6700-COMPUTE-OV-INT-DSCRPT.                                      ELKACCMF
01324                                                                   ELKACCMF
01325      CALL 'ELKFLOR'                                               ELKACCMF
01326         USING WS-CF-1                                             ELKACCMF
01327               WS-CF-INST-BAS                                      ELKACCMF
01328               WS-CF-INST-SUP.                                     ELKACCMF
01329                                                                   ELKACCMF
01330      CALL 'ELKFLAND'                                              ELKACCMF
01331         USING WS-CF-2                                             ELKACCMF
01332               WS-CF-OV-INT-DSCRPT-INST                            ELKACCMF
01333               WS-CF-1.                                            ELKACCMF
01334                                                                   ELKACCMF
01335      CALL 'ELKFLOR'                                               ELKACCMF
01336         USING WS-CF-3                                             ELKACCMF
01337               WS-CF-PROF-BAS                                      ELKACCMF
01338               WS-CF-PROF-SUP.                                     ELKACCMF
01339                                                                   ELKACCMF
01340      CALL 'ELKFLAND'                                              ELKACCMF
01341         USING WS-CF-4                                             ELKACCMF
01342               WS-CF-OV-INT-DSCRPT-PROF                            ELKACCMF
01343               WS-CF-3.                                            ELKACCMF
01344                                                                   ELKACCMF
01345      CALL 'ELKFLOR'                                               ELKACCMF
01346         USING WS-CF-OV-INT-DSCRPT                                 ELKACCMF
01347               WS-CF-2                                             ELKACCMF
01348               WS-CF-4.                                            ELKACCMF
01349 ************************************************************      ELKACCMF
01350 *                                                          *      ELKACCMF
01351 *   COMPUTE OVERALL CONFIDENCE FACTOR                      *      ELKACCMF
01352 *                                                          *      ELKACCMF
01353 ************************************************************      ELKACCMF
01354                                                                   ELKACCMF
01355  6800-COMPUTE-OV-CONF-FACTOR.                                     ELKACCMF
01356                                                                   ELKACCMF
01357      MOVE WS-CW-90 TO WS-CW-OV-PLC-TRTMNT.                        ELKACCMF
01358      MOVE WS-CW-85 TO WS-CW-OV-VL-QLFR.                           ELKACCMF
01359      MOVE WS-CW-85 TO WS-CW-OV-INT-DSCRPT.                        ELKACCMF
01360      MOVE WS-CW-90 TO WS-CW-OV-CNDTN-BTS.                         ELKACCMF
01361      MOVE WS-CW-95 TO WS-CW-OV-CST-CNTNMT.                        ELKACCMF
01362      MOVE WS-CW-80 TO WS-CW-OV-SRVC-GRP.                          ELKACCMF
01363      MOVE WS-CW-85 TO WS-CW-OV-IBGR.                              ELKACCMF
01364      MOVE WS-CW-85 TO WS-CW-OV-IDGD.                              ELKACCMF
01365      MOVE WS-CW-85 TO WS-CW-OV-IPGN.                              ELKACCMF
01366      MOVE WS-CW-85 TO WS-CW-OV-IPGP.                              ELKACCMF
01367      MOVE WS-CW-85 TO WS-CW-OV-IPGT.                              ELKACCMF
01368      MOVE WS-CW-85 TO WS-CW-OV-IPGS.                              ELKACCMF
01369                                                                   ELKACCMF
01370      CALL 'ELKFLAND'                                              ELKACCMF
01371         USING WS-CF-0                                             ELKACCMF
01372               WS-CF-OV-PLC-TRTMNT                                 ELKACCMF
01373               WS-CF-OV-VL-QLFR                                    ELKACCMF
01374               WS-CF-OV-INT-DSCRPT                                 ELKACCMF
01375               WS-CF-OV-CNDTN-BTS                                  ELKACCMF
01376               WS-CF-OV-CST-CNTNMT                                 ELKACCMF
01377               WS-CF-OV-SRVC-GRP                                   ELKACCMF
01378               WS-CF-OV-IBGR                                       ELKACCMF
01379               WS-CF-OV-IDGD                                       ELKACCMF
01380               WS-CF-OV-IPGN                                       ELKACCMF
01381               WS-CF-OV-IPGP                                       ELKACCMF
01382               WS-CF-OV-IPGT                                       ELKACCMF
01383               WS-CF-OV-IPGS.                                      ELKACCMF
01384                                                                   ELKACCMF
01385      IF WS-CF-0 < WS-CF-ZERO                                      ELKACCMF
01386         MOVE WS-CF-0 TO WS-CF-OV                                  ELKACCMF
01387      ELSE                                                         ELKACCMF
01388         IF WS-CF-0 = WS-CF-TRUE                                   ELKACCMF
01389            MOVE WS-CF-0 TO WS-CF-OV                               ELKACCMF
01390      ELSE                                                         ELKACCMF
01391         COMPUTE WS-CF-1 =                                         ELKACCMF
01392                     WS-CF-OV-PLC-TRTMNT * WS-CW-OV-PLC-TRTMNT     ELKACCMF
01393         COMPUTE WS-CF-2 =                                         ELKACCMF
01394                     WS-CF-OV-VL-QLFR * WS-CW-OV-VL-QLFR           ELKACCMF
01395         COMPUTE WS-CF-3 =                                         ELKACCMF
01396                     WS-CF-OV-INT-DSCRPT * WS-CW-OV-INT-DSCRPT     ELKACCMF
01397         COMPUTE WS-CF-4 =                                         ELKACCMF
01398                     WS-CF-OV-CNDTN-BTS * WS-CW-OV-CNDTN-BTS       ELKACCMF
01399         COMPUTE WS-CF-5 =                                         ELKACCMF
01400                     WS-CF-OV-CST-CNTNMT * WS-CW-OV-CST-CNTNMT     ELKACCMF
01401         COMPUTE WS-CF-6 =                                         ELKACCMF
01402                     WS-CF-OV-SRVC-GRP * WS-CW-OV-SRVC-GRP         ELKACCMF
01403         COMPUTE WS-CF-7 =                                         ELKACCMF
01404                     WS-CF-OV-IBGR * WS-CW-OV-IBGR                 ELKACCMF
01405         COMPUTE WS-CW-8 =                                         ELKACCMF
01406                     WS-CF-OV-IDGD * WS-CW-OV-IDGD                 ELKACCMF
01407         COMPUTE WS-CW-9 =                                         ELKACCMF
01408                     WS-CF-OV-IPGN * WS-CW-OV-IPGN                 ELKACCMF
01409         COMPUTE WS-CF-10 =                                        ELKACCMF
01410                     WS-CF-OV-IPGP * WS-CW-OV-IPGP                 ELKACCMF
01411         COMPUTE WS-CF-11 =                                        ELKACCMF
01412                     WS-CF-OV-IPGT * WS-CW-OV-IPGT                 ELKACCMF
01413         COMPUTE WS-CF-12 =                                        ELKACCMF
01414                     WS-CF-OV-IPGS * WS-CW-OV-IPGS                 ELKACCMF
01415                                                                   ELKACCMF
01416         CALL 'ELKFLCMB'                                           ELKACCMF
01417            USING WS-CF-OV                                         ELKACCMF
01418                 WS-CF-1                                           ELKACCMF
01419                 WS-CF-2                                           ELKACCMF
01420                 WS-CF-3                                           ELKACCMF
01421                 WS-CF-4                                           ELKACCMF
01422                 WS-CF-5                                           ELKACCMF
01423                 WS-CF-6                                           ELKACCMF
01424                 WS-CF-7                                           ELKACCMF
01425                 WS-CW-8                                           ELKACCMF
01426                 WS-CW-9                                           ELKACCMF
01427                 WS-CF-10                                          ELKACCMF
01428                 WS-CF-11                                          ELKACCMF
01429                 WS-CF-12                                          ELKACCMF
01430         END-IF                                                    ELKACCMF
01431      END-IF.                                                      ELKACCMF
01432                                                                   ELKACCMF
01433 ************************************************************      ELKACCMF
01434 *                                                          *      ELKACCMF
01435 *    STORE CONFIDENCE FACTORS CALCULATED                   *      ELKACCMF
01436 *                                                          *      ELKACCMF
01437 ************************************************************      ELKACCMF
01438                                                                   ELKACCMF
01439  7000-MOVE-CF-WORK-TO-ATBL.                                       ELKACCMF
01440                                                                   ELKACCMF
01441      MOVE WS-CF-ANL               TO ATBL-CF-ANL      (ATBL-IDX). ELKACCMF
01442      MOVE WS-CF-FMLY              TO ATBL-CF-FMLY     (ATBL-IDX). ELKACCMF
01443      MOVE WS-CF-INDVDL            TO ATBL-CF-INDVDL   (ATBL-IDX). ELKACCMF
01444      MOVE WS-CF-LFTM              TO ATBL-CF-LFTM     (ATBL-IDX). ELKACCMF
01445      MOVE WS-CF-INST-BAS          TO ATBL-CF-INST-BAS (ATBL-IDX). ELKACCMF
01446      MOVE WS-CF-INST-SUP          TO ATBL-CF-INST-SUP (ATBL-IDX). ELKACCMF
01447      MOVE WS-CF-PROF-BAS          TO ATBL-CF-PROF-BAS (ATBL-IDX). ELKACCMF
01448      MOVE WS-CF-PROF-SUP          TO ATBL-CF-PROF-SUP (ATBL-IDX). ELKACCMF
01449      MOVE WS-CF-IP                TO ATBL-CF-IP       (ATBL-IDX). ELKACCMF
01450      MOVE WS-CF-OP                TO ATBL-CF-OP       (ATBL-IDX). ELKACCMF
01451      MOVE WS-CF-PLAN              TO ATBL-CF-PLAN     (ATBL-IDX). ELKACCMF
01452      MOVE WS-CF-NON-PLAN          TO ATBL-CF-NON-PLAN (ATBL-IDX). ELKACCMF
01453      MOVE WS-CF-OV                TO ATBL-CF-OV       (ATBL-IDX). ELKACCMF
01454      MOVE WS-CF-OV-CNDTN-BTS      TO                              ELKACCMF
01455                            ATBL-CF-OV-CNDTN-BTS       (ATBL-IDX). ELKACCMF
01456      MOVE WS-CF-OV-CST-CNTNMT     TO                              ELKACCMF
01457                            ATBL-CF-OV-CST-CNTNMT      (ATBL-IDX). ELKACCMF
01458      MOVE WS-CF-OV-INT-DSCRPT     TO                              ELKACCMF
01459                            ATBL-CF-OV-INTRNL-DSCRPTR  (ATBL-IDX). ELKACCMF
01460      MOVE WS-CF-OV-PLC-TRTMNT     TO                              ELKACCMF
01461                            ATBL-CF-OV-PLC-TRTMNT      (ATBL-IDX). ELKACCMF
01462      MOVE WS-CF-OV-SRVC-GRP       TO                              ELKACCMF
01463                            ATBL-CF-OV-SRVC-GRP        (ATBL-IDX). ELKACCMF
01464      MOVE WS-CF-OV-VL-QLFR        TO                              ELKACCMF
01465                            ATBL-CF-OV-VL-QLFR         (ATBL-IDX). ELKACCMF
01466                                                                   ELKACCMF
