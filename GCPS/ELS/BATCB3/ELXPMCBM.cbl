00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCBM
00003  PROGRAM-ID.         ELXPMCBM                                        LV004
00004                                                                   ELXPMCBM
00005  AUTHOR.             BARBARA KEIB                                 ELXPMCBM
00006                                                                   ELXPMCBM
00007                                                                   ELXPMCBM
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCBM
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCBM
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCBM
00011                      233 N. MICHIGAN AVE                          ELXPMCBM
00012                      CHICAGO, ILLINOIS 60601                      ELXPMCBM
00013                                                                   ELXPMCBM
00014  DATE-WRITTEN.       07-JAN-1993.                                 ELXPMCBM
00015                                                                   ELXPMCBM
00016  DATE-COMPILED.                                                   ELXPMCBM
00017                                                                   ELXPMCBM
00018  SECURITY.           COPYRIGHT 1993,                              ELXPMCBM
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCBM
00020      SKIP3                                                        ELXPMCBM
00021  ENVIRONMENT DIVISION.                                            ELXPMCBM
00022                                                                   ELXPMCBM
00023  CONFIGURATION SECTION.                                           ELXPMCBM
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCBM
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCBM
00026      EJECT                                                        ELXPMCBM
00027 ******************************************************************ELXPMCBM
00028 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCBM
00029 *      EXTRACT BENEFIT ACCUMULATOR DATA FOR ELIGIBILITY SUMMARY  *ELXPMCBM
00030 *                                                                *ELXPMCBM
00031 *     ****THIS PROGRAM MUST BE BATCH COMPILED********            *ELXPMCBM
00032 *                                                                *ELXPMCBM
00033 ******************************************************************ELXPMCBM
00034 *                      MAINTENANCE HISTORY                       *ELXPMCBM
00035 *                                                                *ELXPMCBM
00036 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCBM
00037 * ----- ----------- --- ----- ---------------------------------- *ELXPMCBM
00038 * 01.00 07-JAN-1993 BAK       CREATED                            *ELXPMCBM
00039 * 01.01 24-FEB-1993 BAK       ADD DAY/NIGHT PSYCH FOR PROF INPAT.*ELXPMCBM
00040 *                             CORRECTED ERRORS IN 7000 ROUTINES. *ELXPMCBM
00041 * 02.00 01-JUN-1993 BAK       ADD MAJOR MEDICAL CONTRACT SUPPORT *ELXPMCBM
00042 *                             ISSR # 13071                       *ELXPMCBM
00043 * 02.01 14-JUN-1993 AKK       REMOVED UNNECESSARY PERIODS IN     *ELXPMCBM
00044 *                             ORDER TO AVOID HAVOC EX. PARAGRAPH *ELXPMCBM
00045 *                             9320.                              *ELXPMCBM
00046 * 02.02 29-JUN-1993 BAK       ADD CALLS FOR VISIT, UNITS AND     *ELXPMCBM
00047 *                             CONFINEMENTS FOR ALL 'DAY' GROUPS  *ELXPMCBM
00048 * 02.03 23-AUG-1993 BAK       FIX MENTAL DAY & DRB CONF FACTORS  *ELXPMCBM
00049 *                             MOVE CORRECT VALUE LIMIT FOR FIELDS*ELXPMCBM
00050 *                             THAT DO NOT CONTAIN DOLLAR VALUES. *ELXPMCBM
00051 * 03.00 27-SEP-1993 BAK       SUPPORT FOR PHASE 2 ISSR 13071     *ELXPMCBM
00052 *                             SUPPORT FOR RPO AND MCN-P AND UNITS*ELXPMCBM
00053 *                             FOR NURSING MAX.                   *ELXPMCBM
00054 * 03.00 27-SEP-1993 BAK       SUPPORT FOR PHASE 2 ISSR 13071     *ELXPMCBM
00055 * 03.00 12/06/93    RGO       FIX BUG WITH PRIVATE NURSING.      *ELXPMCBM
00056 *                             A VALUE-QUALIFIER OF 5 IS FOR      *ELXPMCBM
00057 *                             DOLLARS,  AND WE ARE NOT SET UP TO *ELXPMCBM
00058 *                             HANDLE DOLLARS NOW.  INSTEAD OF    *ELXPMCBM
00059 *                             PERFORMING 9153-, 5230- WILL PERFOM*ELXPMCBM
00060 *                             5235-.                             *ELXPMCBM
00061 * 3/10/95  RGO   CPO PROJECT. MADE CHANGES TO PARAGRAPH 0150-.   *ELXPMCBM
00062 * RGO 10/20/95  FIX PROD ABEND DUE TO NO IBGR-INTERNAL-TABS-TABLE*ELXPMCBM
00063 *               SEE PARAGRAPH 0100-.                             *ELXPMCBM
00064 *                                                                *ELXPMCBM
00065 * 4/04/96  RGO   CBL PROJECT. MADE CHANGES TO PARAGRAPH 0150-.   *ELXPMCBM
00066 *                FOR COMMUNITY BLUE PROJECT                      *ELXPMCBM
00067 *                                                                *ELXPMCBM
00068 *09/07/00  AKK   ADD SUPPORT FOR BAE.                            *ELXPMCBM
00069 *                                                                *ELXPMCBM
00070 *03/12/03  AKK   REGEN'D W/ PMCCOMM ELS VERSION.                 *ELXPMCBM
00071 *                                                                *ELXPMCBM
00072 *04/01/03  AKK   MORE CHANGES DUE TO ENDEVOR                     *ELXPMCBM
00073 *                                                                *ELXPMCBM
00074 * 05.00 01-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCBM
00075 *                                                                *ELXPMCBM
00076 * 05.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCBM
00077 *                                                                 ELXPMCBM
00078 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCBM
00079 *                                                                *ELXPMCBM
00080 ******************************************************************ELXPMCBM
00081                                                                   ELXPMCBM
00082                                                                   ELXPMCBM
00083      EJECT                                                        ELXPMCBM
00084  DATA DIVISION.                                                   ELXPMCBM
00085  WORKING-STORAGE SECTION.                                         ELXPMCBM
00086  01  FILLER                     PICTURE X(32)                     ELXPMCBM
00087           VALUE '****ELXPMCBM WORKING STORAGE****'.               ELXPMCBM
00088                                                                   ELXPMCBM
00089  01  WS-RETURN-CODE               PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00090                                                                   ELXPMCBM
00091      88  WS-SUCCESSFUL-CALL               VALUE ZERO.             ELXPMCBM
00092      88  WS-UNIDENT-PARM                  VALUE +8.               ELXPMCBM
00093      88  WS-MISSING-PARM                  VALUE +12.              ELXPMCBM
00094      88  WS-INTERNAL-ERROR                VALUE +16.              ELXPMCBM
00095                                                                   ELXPMCBM
00096  01  WS-SAVE-SUBSCRIPTS.                                          ELXPMCBM
00097                                                                   ELXPMCBM
00098      05  WS-SAVE-SUB              PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00099      05  WS-SAVE-SUB-BSC          PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00100      05  WS-SAVE-SUB-MM           PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00101      05  WS-ALC-SUB               PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00102      05  WS-DRG-SUB               PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00103      05  WS-LFM-DLR-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00104      05  WS-LFM-DAY-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00105      05  WS-OTR-DLR-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00106      05  WS-OTR-DAY-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00107      05  WS-SUB-WORK              PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00108                                                                   ELXPMCBM
00109                                                                   ELXPMCBM
00110  01  WS-NUM-APPL-ENTRS            PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00111  01  WS-APPL-ENTRS-BSC            PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00112  01  WS-APPL-ENTRS-MM             PIC S9(04) COMP VALUE ZERO.     ELXPMCBM
00113  01  WS-VALUE-AMT-LONG            PIC S9(09) COMP-3 VALUE ZERO.   ELXPMCBM
00114  01  WS-VALUE-AMT-SHORT           PIC S9(07) COMP-3 VALUE ZERO.   ELXPMCBM
00115  01  WS-BNF-QUAL                  PIC X(01) VALUE SPACES.         ELXPMCBM
00116  01  WS-BNF-PERD                  PIC X(02) VALUE SPACES.         ELXPMCBM
00117  01  WS-SAVE-LOB-IND              PIC X(01) VALUE SPACES.         ELXPMCBM
00118                                                                   ELXPMCBM
00119                                                                   ELXPMCBM
00120  01  WS-SWITCHES.                                                 ELXPMCBM
00121                                                                   ELXPMCBM
00122    05  WS-PRG-VARIATION-CONTROL   PIC X(01) VALUE 'N'.            ELXPMCBM
00123      88  WS-PRG-VAR-FOUND                 VALUE 'Y'.              ELXPMCBM
00124      88  WS-PRG-VAR-NOT-FOUND             VALUE 'N'.              ELXPMCBM
00125                                                                   ELXPMCBM
00126    05  WS-IBGR-SLOT-CONTROL       PIC X(01) VALUE 'N'.            ELXPMCBM
00127      88  WS-IBGR-FOUND                    VALUE 'Y'.              ELXPMCBM
00128      88  WS-IBGR-NOT-FOUND                VALUE 'N'.              ELXPMCBM
00129                                                                   ELXPMCBM
00130    05  WS-BNFTPRD-CONTROL         PIC X(01) VALUE 'N'.            ELXPMCBM
00131      88  WS-BNFTPRD-FOUND                 VALUE 'Y'.              ELXPMCBM
00132      88  WS-BNFTPRD-NOT-FOUND             VALUE 'N'.              ELXPMCBM
00133                                                                   ELXPMCBM
00134    05  WS-INTRNLDSC-CONTORL       PIC X(01) VALUE 'N'.            ELXPMCBM
00135      88  WS-INTRNLDSC-FOUND               VALUE 'Y'.              ELXPMCBM
00136      88  WS-INTRNLDSC-NOT-FOUND           VALUE 'N'.              ELXPMCBM
00137                                                                   ELXPMCBM
00138    05  WS-MCNP-PENALTY-IND        PIC X(01) VALUE 'N'.            ELXPMCBM
00139      88  WS-MCNP-PEN-FOUND               VALUE 'Y'.               ELXPMCBM
00140      88  WS-MCNP-PEN-NOT-FOUND           VALUE 'N'.               ELXPMCBM
00141                                                                   ELXPMCBM
00142    05  WS-MCNP-INCENT-IND        PIC X(01) VALUE 'N'.             ELXPMCBM
00143      88  WS-MCNP-INC-FOUND               VALUE 'Y'.               ELXPMCBM
00144      88  WS-MCNP-INC-NOT-FOUND           VALUE 'N'.               ELXPMCBM
00145                                                                   ELXPMCBM
00146    05  WS-PPO-PENALTY-IND        PIC X(01) VALUE 'N'.             ELXPMCBM
00147      88  WS-PPO-PEN-FOUND               VALUE 'Y'.                ELXPMCBM
00148      88  WS-PPO-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00149                                                                   ELXPMCBM
00150    05  WS-PPO-INCENT-IND        PIC X(01) VALUE 'N'.              ELXPMCBM
00151      88  WS-PPO-INC-FOUND               VALUE 'Y'.                ELXPMCBM
00152      88  WS-PPO-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00153                                                                   ELXPMCBM
00154    05  WS-RPO-PENALTY-IND        PIC X(01) VALUE 'N'.             ELXPMCBM
00155      88  WS-RPO-PEN-FOUND               VALUE 'Y'.                ELXPMCBM
00156      88  WS-RPO-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00157                                                                   ELXPMCBM
00158    05  WS-RPO-INCENT-IND        PIC X(01) VALUE 'N'.              ELXPMCBM
00159      88  WS-RPO-INC-FOUND               VALUE 'Y'.                ELXPMCBM
00160      88  WS-RPO-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00161                                                                   ELXPMCBM
00162    05  WS-BAE-PENALTY-IND        PIC X(01) VALUE 'N'.             ELXPMCBM
00163      88  WS-BAE-PEN-FOUND               VALUE 'Y'.                ELXPMCBM
00164      88  WS-BAE-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00165                                                                   ELXPMCBM
00166    05  WS-BAE-INCENT-IND        PIC X(01) VALUE 'N'.              ELXPMCBM
00167      88  WS-BAE-INC-FOUND               VALUE 'Y'.                ELXPMCBM
00168      88  WS-BAE-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBM
00169                                                                   ELXPMCBM
00170  01  WS-WEIGHTS.                                                  ELXPMCBM
00171      02  WS-WT-BNFT-PRD         COMP-1    VALUE 0.750000E+00.     ELXPMCBM
00172      02  WS-WT-INDVDL           COMP-1    VALUE 0.500000E+00.     ELXPMCBM
00173      02  WS-WT-INST-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00174      02  WS-WT-PROF-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00175      02  WS-WT-INST-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00176      02  WS-WT-PROF-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00177      02  WS-WT-IP               COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00178      02  WS-WT-OP               COMP-1    VALUE 0.100000E+00.     ELXPMCBM
00179      02  WS-WT-PLAN             COMP-1    VALUE 0.250000E+00.     ELXPMCBM
00180      02  WS-WT-SP               COMP-1    VALUE 0.500000E+00.     ELXPMCBM
00181                                                                   ELXPMCBM
00182  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCBM
00183      02  WS-CF-ZERO             COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00184      02  WS-CF-TRUE             COMP-1    VALUE +1.000000E+00.    ELXPMCBM
00185      02  WS-CF-FALSE            COMP-1    VALUE -1.000000E+00.    ELXPMCBM
00186      02  WS-CF-75               COMP-1    VALUE +0.750000E+00.    ELXPMCBM
00187      02  WS-CF-85               COMP-1    VALUE +0.850000E+00.    ELXPMCBM
00188      02  WS-CF-95               COMP-1    VALUE +0.950000E+00.    ELXPMCBM
00189      02  WS-TEST-CONF-FACT      COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00190      02  WS-TEST-CONF-BSC       COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00191      02  WS-TEST-CONF-MM        COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00192      02  WS-TEST-THRESHOLD      COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00193      02  WS-NO-IBGR-CF          COMP-1    VALUE +0.000000E+00.    ELXPMCBM
00194                                                                   ELXPMCBM
00195  01  WS-CONFIDENCE-WORK.                                          ELXPMCBM
00196      02  WS-CF-1                COMP-1.                           ELXPMCBM
00197      02  WS-CF-2                COMP-1.                           ELXPMCBM
00198      02  WS-CF-3                COMP-1.                           ELXPMCBM
00199      02  WS-CF-4                COMP-1.                           ELXPMCBM
00200      02  WS-CF-5                COMP-1.                           ELXPMCBM
00201      02  WS-CF-6                COMP-1.                           ELXPMCBM
00202                                                                   ELXPMCBM
00203 * INSTITUTIONAL INPATIENT                                         ELXPMCBM
00204                                                                   ELXPMCBM
00205  01  WS-INST-IP-ALC.                                              ELXPMCBM
00206      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBM
00207      02  FILLER                 PIC X(06) VALUE 'ARPI W'.         ELXPMCBM
00208      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCBM
00209                                                                   ELXPMCBM
00210  01  WS-INST-IP-DPSY.                                             ELXPMCBM
00211      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00212      02  FILLER                 PIC X(06) VALUE 'DPSY A'.         ELXPMCBM
00213                                                                   ELXPMCBM
00214  01  WS-INST-IP-DRB.                                              ELXPMCBM
00215      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00216      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCBM
00217                                                                   ELXPMCBM
00218  01  WS-INST-IP-DRG.                                              ELXPMCBM
00219      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBM
00220      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCBM
00221      02  FILLER                 PIC X(06) VALUE 'DRPI W'.         ELXPMCBM
00222                                                                   ELXPMCBM
00223  01  WS-INST-IP-NPSY.                                             ELXPMCBM
00224      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00225      02  FILLER                 PIC X(06) VALUE 'NPSY A'.         ELXPMCBM
00226                                                                   ELXPMCBM
00227  01  WS-INST-IP-PDN.                                              ELXPMCBM
00228      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00229      02  FILLER                 PIC X(06) VALUE 'NRSI B'.         ELXPMCBM
00230                                                                   ELXPMCBM
00231  01  WS-INST-IP-PSYS.                                             ELXPMCBM
00232      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBM
00233      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCBM
00234      02  FILLER                 PIC X(06) VALUE 'PSYI W'.         ELXPMCBM
00235                                                                   ELXPMCBM
00236 * INSTITUTIONAL OUTPATIENT                                        ELXPMCBM
00237                                                                   ELXPMCBM
00238  01  WS-INST-OP-ALC.                                              ELXPMCBM
00239      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00240      02  FILLER                 PIC X(06) VALUE 'ARPO W'.         ELXPMCBM
00241                                                                   ELXPMCBM
00242  01  WS-INST-OP-DRG.                                              ELXPMCBM
00243      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00244      02  FILLER                 PIC X(06) VALUE 'DRPO W'.         ELXPMCBM
00245                                                                   ELXPMCBM
00246  01  WS-INST-OP-PDN.                                              ELXPMCBM
00247      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00248      02  FILLER                 PIC X(06) VALUE 'NRSO B'.         ELXPMCBM
00249                                                                   ELXPMCBM
00250  01  WS-INST-OP-PSYS.                                             ELXPMCBM
00251      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00252      02  FILLER                 PIC X(06) VALUE 'PSYO W'.         ELXPMCBM
00253                                                                   ELXPMCBM
00254 * PROFESSIONAL INPATIENT                                          ELXPMCBM
00255                                                                   ELXPMCBM
00256  01  WS-PROF-IP-ALC.                                              ELXPMCBM
00257      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00258      02  FILLER                 PIC X(06) VALUE 'AHI  D'.         ELXPMCBM
00259                                                                   ELXPMCBM
00260  01  WS-PROF-IP-DPV.                                              ELXPMCBM
00261      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00262      02  FILLER                 PIC X(06) VALUE 'DPV  D'.         ELXPMCBM
00263                                                                   ELXPMCBM
00264  01  WS-PROF-IP-DRG.                                              ELXPMCBM
00265      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00266      02  FILLER                 PIC X(06) VALUE 'DRI  D'.         ELXPMCBM
00267                                                                   ELXPMCBM
00268  01  WS-PROF-IP-NPV.                                              ELXPMCBM
00269      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00270      02  FILLER                 PIC X(06) VALUE 'NPV  D'.         ELXPMCBM
00271                                                                   ELXPMCBM
00272  01  WS-PROF-IP-PDN.                                              ELXPMCBM
00273      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00274      02  FILLER                 PIC X(06) VALUE 'NRSI E'.         ELXPMCBM
00275                                                                   ELXPMCBM
00276  01  WS-PROF-IP-PSYS.                                             ELXPMCBM
00277      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00278      02  FILLER                 PIC X(06) VALUE 'MNI  D'.         ELXPMCBM
00279                                                                   ELXPMCBM
00280 * PROFESSIONAL OUTPATIENT                                         ELXPMCBM
00281                                                                   ELXPMCBM
00282  01  WS-PROF-OP-PDN.                                              ELXPMCBM
00283      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBM
00284      02  FILLER                 PIC X(06) VALUE 'NRSO E'.         ELXPMCBM
00285                                                                   ELXPMCBM
00286  01  WS-PROF-OP-PSYS.                                             ELXPMCBM
00287      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBM
00288      02  FILLER                 PIC X(06) VALUE 'GPO  E'.         ELXPMCBM
00289      02  FILLER                 PIC X(06) VALUE 'IPO  E'.         ELXPMCBM
00290                                                                   ELXPMCBM
00291  01  WS-POINTERS.                                                 ELXPMCBM
00292      02  WS-ALAB-POINTER        POINTER.                          ELXPMCBM
00293      02  WS-DLRB-POINTER        POINTER.                          ELXPMCBM
00294      02  WS-DPSY-POINTER        POINTER.                          ELXPMCBM
00295      02  WS-DRAB-POINTER        POINTER.                          ELXPMCBM
00296      02  WS-NPSY-POINTER        POINTER.                          ELXPMCBM
00297      02  WS-PRDN-POINTER        POINTER.                          ELXPMCBM
00298      02  WS-PSYS-POINTER        POINTER.                          ELXPMCBM
00299                                                                   ELXPMCBM
00300  01  WS-SWITCHES.                                                 ELXPMCBM
00301      05                         PIC X(01).                        ELXPMCBM
00302         88  SW-TRMNL-ERR                  VALUE 'Y'.              ELXPMCBM
00303         88  SW-NO-TRMNL-ERR               VALUE 'N'.              ELXPMCBM
00304                                                                   ELXPMCBM
00305  COPY ELSCFTB5.                                                   ELXPMCBM
00306  COPY ELSCFTBA.                                                   ELXPMCBM
00307  COPY ELSCVG2C.                                                   ELXPMCBM
00308                                                                   ELXPMCBM
00309  01  FILLER                     PICTURE X(32)                     ELXPMCBM
00310           VALUE '*END ELXPMCBM WORKING STORAGE***'.               ELXPMCBM
00311      EJECT                                                        ELXPMCBM
00312  LINKAGE SECTION.                                                 ELXPMCBM
00313 *    EJECT                                                        ELXPMCBM
00314  COPY ELSCIA2C.                                                   ELXPMCBM
00315 *    EJECT                                                        ELXPMCBM
00316  COPY ELSCSACC.                                                   ELXPMCBM
00317 *    EJECT                                                        ELXPMCBM
00318  COPY ELSATBLC.                                                   ELXPMCBM
00319 *    EJECT                                                        ELXPMCBM
00320  COPY ELSPMCID.                                                   ELXPMCBM
00321 *    EJECT                                                        ELXPMCBM
00322  COPY ELSIBGRC.                                                   ELXPMCBM
00323 *    EJECT                                                        ELXPMCBM
00324  01  PMCI-COMM-AREA.                                              ELXPMCBM
00325  COPY PMCCOMM.                                                    ELXPMCBM
00326 *    EJECT                                                        ELXPMCBM
00327  COPY ELSBPVLC.                                                   ELXPMCBM
00328  01  LS-MATCH-LIST               PIC X.                           ELXPMCBM
00329 *    EJECT                                                        ELXPMCBM
00330  PROCEDURE DIVISION USING PMCI-COMM-AREA                          ELXPMCBM
00331                           NAES-INTERMEDIATE-DATA                  ELXPMCBM
00332                           CSAC-ACCUMULATOR-TABLE                  ELXPMCBM
00333                           IBGR-INTERNAL-TABS-TABLE.               ELXPMCBM
00334 ************************************************************      ELXPMCBM
00335 *                                                          *      ELXPMCBM
00336 *          MAINLINE ROUTINE                                *      ELXPMCBM
00337 *                                                          *      ELXPMCBM
00338 ************************************************************      ELXPMCBM
00339  0000-MAINLINE.                                                   ELXPMCBM
00340                                                                   ELXPMCBM
00341      IF ADDRESS OF PMCI-COMM-AREA = NULL                          ELXPMCBM
00342         NEXT SENTENCE                                             ELXPMCBM
00343      ELSE                                                         ELXPMCBM
00344         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCBM
00345         SET PMCI-BC-NO-ERROR TO TRUE                              ELXPMCBM
00346         SET SW-NO-TRMNL-ERR TO TRUE                               ELXPMCBM
00347         IF ADDRESS OF CSAC-ACCUMULATOR-TABLE = NULL               ELXPMCBM
00348            MOVE +4601 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBM
00349            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBM
00350         ELSE                                                      ELXPMCBM
00351            SET ADDRESS OF LS-MATCH-LIST TO NULL                   ELXPMCBM
00352            PERFORM 0100-EXTRCT-BNFT-MAXS.                         ELXPMCBM
00353                                                                   ELXPMCBM
00354      GOBACK.                                                      ELXPMCBM
00355                                                                   ELXPMCBM
00356 ************************************************************      ELXPMCBM
00357 *                                                          *      ELXPMCBM
00358 *        EXTRACT BENEFIT MAXIMUMS                          *      ELXPMCBM
00359 *                                                          *      ELXPMCBM
00360 ************************************************************      ELXPMCBM
00361  0100-EXTRCT-BNFT-MAXS.                                           ELXPMCBM
00362                                                                   ELXPMCBM
00363      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELXPMCBM
00364                      CSAC-ABM-GC-TBL-PTR.                         ELXPMCBM
00365      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULLS                 ELXPMCBM
00366        OR ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULLS             ELXPMCBM
00367          SET DRB-CALL TO TRUE                                     ELXPMCBM
00368                   SET PMCI-MAX-COMBO-NONE TO TRUE                 ELXPMCBM
00369                   SET PMCI-AL-CALL TO TRUE                        ELXPMCBM
00370                   SET PMCI-MD-CALL TO TRUE                        ELXPMCBM
00371                   SET DNPD-CALL TO TRUE                           ELXPMCBM
00372                   SET DNPN-CALL TO TRUE                           ELXPMCBM
00373                   SET PMCI-IQ-CALL TO TRUE                        ELXPMCBM
00374                   SET PMCI-MM-CALL TO TRUE                        ELXPMCBM
00375                    SET LFMD-CALL TO TRUE                          ELXPMCBM
00376                    SET MNDD-CALL TO TRUE                          ELXPMCBM
00377                    SET MNDT-CALL TO TRUE                          ELXPMCBM
00378                    SET PRDN-CALL TO TRUE                          ELXPMCBM
00379                                                                   ELXPMCBM
00380                                                                   ELXPMCBM
00381      ELSE                                                         ELXPMCBM
00382         PERFORM 0120-IDNTFY-AVLBL-BNFT-MAXS.                      ELXPMCBM
00383                                                                   ELXPMCBM
00384 ************************************************************      ELXPMCBM
00385 *                                                          *      ELXPMCBM
00386 *        IDENTIFY AVAILABLE BENEFIT MAXIMUMS               *      ELXPMCBM
00387 *                                                          *      ELXPMCBM
00388 ************************************************************      ELXPMCBM
00389  0120-IDNTFY-AVLBL-BNFT-MAXS.                                     ELXPMCBM
00390                                                                   ELXPMCBM
00391      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCBM
00392      IF PMCI-PRV-CALL OR PMCI-PRV-NONE                            ELXPMCBM
00393         CONTINUE                                                  ELXPMCBM
00394      ELSE                                                         ELXPMCBM
00395         PERFORM 0130-VERIFY-COST-CONTAINMENT                      ELXPMCBM
00396             VARYING ATBL-IDX FROM 1 BY 1                          ELXPMCBM
00397                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBM
00398      SET WS-PRG-VAR-NOT-FOUND TO TRUE.                            ELXPMCBM
00399      PERFORM 0150-INTLZ-SP                                        ELXPMCBM
00400            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBM
00401                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBM
00402                                                                   ELXPMCBM
00403      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
00404         IF PMCI-INSTITUTIONAL                                     ELXPMCBM
00405            IF PMCI-INPATIENT                                      ELXPMCBM
00406               PERFORM 1000-IDNTFY-INST-IP-MAXS                    ELXPMCBM
00407            ELSE                                                   ELXPMCBM
00408               PERFORM 2000-IDNTFY-INST-OP-MAXS                    ELXPMCBM
00409         ELSE                                                      ELXPMCBM
00410            IF PMCI-INPATIENT                                      ELXPMCBM
00411               PERFORM 3000-IDNTFY-PROF-IP-MAXS                    ELXPMCBM
00412            ELSE                                                   ELXPMCBM
00413               PERFORM 4000-IDNTFY-PROF-OP-MAXS.                   ELXPMCBM
00414 ************************************************************      ELXPMCBM
00415 *                                                          *      ELXPMCBM
00416 *        VERIFY COST CONTAINMENT PROGRAMS                  *      ELXPMCBM
00417 *                                                          *      ELXPMCBM
00418 ************************************************************      ELXPMCBM
00419  0130-VERIFY-COST-CONTAINMENT.                                    ELXPMCBM
00420                                                                   ELXPMCBM
00421      EVALUATE TRUE                                                ELXPMCBM
00422         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBM
00423            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBM
00424         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBM
00425            PERFORM 0132-TEST-PPO-CSTCNMT-OUT                      ELXPMCBM
00426         WHEN PMCI-PRV-BAE-IN                                      ELXPMCBM
00427            PERFORM 0133-TEST-BAE-CSTCNMT-IN                       ELXPMCBM
00428         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBM
00429            PERFORM 0133-TEST-BAE-CSTCNMT-OUT                      ELXPMCBM
00430         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBM
00431            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBM
00432         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBM
00433            PERFORM 0134-TEST-RPO-CSTCNMT-OUT                      ELXPMCBM
00434         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBM
00435            PERFORM 0134-TEST-RPO-CSTCNMT-IN                       ELXPMCBM
00436         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBM
00437            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBM
00438         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBM
00439            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBM
00440         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBM
00441            PERFORM 0136-TEST-MCNP-CSTCNMT-OUT                     ELXPMCBM
00442         WHEN OTHER                                                ELXPMCBM
00443            CONTINUE                                               ELXPMCBM
00444      END-EVALUATE.                                                ELXPMCBM
00445 ************************************************************      ELXPMCBM
00446 *                                                          *      ELXPMCBM
00447 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00448 *          FOR IN NETWORK                                  *      ELXPMCBM
00449 ************************************************************      ELXPMCBM
00450  0132-TEST-PPO-CSTCNMT-IN.                                        ELXPMCBM
00451                                                                   ELXPMCBM
00452      EVALUATE TRUE                                                ELXPMCBM
00453         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBM
00454             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBM
00455         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBM
00456             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00457         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBM
00458             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00459         WHEN OTHER                                                ELXPMCBM
00460             CONTINUE                                              ELXPMCBM
00461      END-EVALUATE.                                                ELXPMCBM
00462                                                                   ELXPMCBM
00463 ************************************************************      ELXPMCBM
00464 *                                                          *      ELXPMCBM
00465 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00466 *          FOR IN NETWORK                                  *      ELXPMCBM
00467 ************************************************************      ELXPMCBM
00468  0133-TEST-BAE-CSTCNMT-IN.                                        ELXPMCBM
00469                                                                   ELXPMCBM
00470      EVALUATE TRUE                                                ELXPMCBM
00471         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBM
00472             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00473         WHEN OTHER                                                ELXPMCBM
00474             CONTINUE                                              ELXPMCBM
00475      END-EVALUATE.                                                ELXPMCBM
00476                                                                   ELXPMCBM
00477 ************************************************************      ELXPMCBM
00478 *                                                          *      ELXPMCBM
00479 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00480 *          FOR OUT OF NETWORK                              *      ELXPMCBM
00481 ************************************************************      ELXPMCBM
00482  0132-TEST-PPO-CSTCNMT-OUT.                                       ELXPMCBM
00483                                                                   ELXPMCBM
00484      EVALUATE TRUE                                                ELXPMCBM
00485         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBM
00486             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBM
00487         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBM
00488             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00489         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBM
00490             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00491         WHEN OTHER                                                ELXPMCBM
00492             CONTINUE                                              ELXPMCBM
00493      END-EVALUATE.                                                ELXPMCBM
00494                                                                   ELXPMCBM
00495 ************************************************************      ELXPMCBM
00496 *                                                          *      ELXPMCBM
00497 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00498 *          FOR OUT OF NETWORK                              *      ELXPMCBM
00499 ************************************************************      ELXPMCBM
00500  0133-TEST-BAE-CSTCNMT-OUT.                                       ELXPMCBM
00501                                                                   ELXPMCBM
00502      EVALUATE TRUE                                                ELXPMCBM
00503         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBM
00504             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBM
00505         WHEN OTHER                                                ELXPMCBM
00506             CONTINUE                                              ELXPMCBM
00507      END-EVALUATE.                                                ELXPMCBM
00508                                                                   ELXPMCBM
00509 ************************************************************      ELXPMCBM
00510 *                                                          *      ELXPMCBM
00511 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00512 *          FOR IN NETWORK                                  *      ELXPMCBM
00513 ************************************************************      ELXPMCBM
00514  0134-TEST-RPO-CSTCNMT-IN.                                        ELXPMCBM
00515                                                                   ELXPMCBM
00516      EVALUATE TRUE                                                ELXPMCBM
00517         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBM
00518             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBM
00519         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBM
00520             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBM
00521         WHEN OTHER                                                ELXPMCBM
00522             CONTINUE                                              ELXPMCBM
00523      END-EVALUATE.                                                ELXPMCBM
00524                                                                   ELXPMCBM
00525 ************************************************************      ELXPMCBM
00526 *                                                          *      ELXPMCBM
00527 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBM
00528 *          FOR OUT OF NETWORK                              *      ELXPMCBM
00529 ************************************************************      ELXPMCBM
00530  0134-TEST-RPO-CSTCNMT-OUT.                                       ELXPMCBM
00531                                                                   ELXPMCBM
00532      EVALUATE TRUE                                                ELXPMCBM
00533         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBM
00534             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBM
00535         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBM
00536             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBM
00537         WHEN OTHER                                                ELXPMCBM
00538             CONTINUE                                              ELXPMCBM
00539      END-EVALUATE.                                                ELXPMCBM
00540                                                                   ELXPMCBM
00541 ************************************************************      ELXPMCBM
00542 *                                                          *      ELXPMCBM
00543 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBM
00544 *          FOR REFERRAL OR NO REFERRAL REQUIRED            *      ELXPMCBM
00545 ************************************************************      ELXPMCBM
00546  0136-TEST-MCNP-CSTCNMT-IN.                                       ELXPMCBM
00547                                                                   ELXPMCBM
00548      EVALUATE TRUE                                                ELXPMCBM
00549         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBM
00550             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBM
00551         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBM
00552             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBM
00553         WHEN OTHER                                                ELXPMCBM
00554             CONTINUE                                              ELXPMCBM
00555      END-EVALUATE.                                                ELXPMCBM
00556                                                                   ELXPMCBM
00557 ************************************************************      ELXPMCBM
00558 *                                                          *      ELXPMCBM
00559 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBM
00560 *          FOR NON REFERRAL                                *      ELXPMCBM
00561 ************************************************************      ELXPMCBM
00562  0136-TEST-MCNP-CSTCNMT-OUT.                                      ELXPMCBM
00563                                                                   ELXPMCBM
00564      EVALUATE TRUE                                                ELXPMCBM
00565         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBM
00566             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBM
00567         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBM
00568             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBM
00569         WHEN OTHER                                                ELXPMCBM
00570             CONTINUE                                              ELXPMCBM
00571      END-EVALUATE.                                                ELXPMCBM
00572                                                                   ELXPMCBM
00573 ************************************************************      ELXPMCBM
00574 *                                                          *      ELXPMCBM
00575 *        INITIALIZE SPECIAL CASE FACTORS                   *      ELXPMCBM
00576 * ADDED CODE TO HANDLE CPO. RGO 3/10                       *      ELXPMCBM
00577 ************************************************************      ELXPMCBM
00578  0150-INTLZ-SP.                                                   ELXPMCBM
00579                                                                   ELXPMCBM
00580      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX) TO                          ELXPMCBM
00581                    ATBL-CF-SP-FCTRS (ATBL-IDX).                   ELXPMCBM
00582      EVALUATE TRUE                                                ELXPMCBM
00583         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBM
00584            PERFORM 0160-SCN-SP-CCP-IPPO                           ELXPMCBM
00585         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBM
00586            PERFORM 0161-SCN-SP-CCP-OPPO                           ELXPMCBM
00587         WHEN PMCI-PRV-BAE-IN                                      ELXPMCBM
00588            PERFORM 0163-SCN-SP-CCP-IBAE                           ELXPMCBM
00589         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBM
00590            PERFORM 0164-SCN-SP-CCP-OBAE                           ELXPMCBM
00591         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBM
00592            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBM
00593         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBM
00594            PERFORM 0171-SCN-SP-CCP-ORPO                           ELXPMCBM
00595         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBM
00596            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBM
00597         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBM
00598            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBM
00599         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBM
00600            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBM
00601         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBM
00602            PERFORM 0181-SCN-SP-CCP-OMCNP                          ELXPMCBM
00603 *                                                                 ELXPMCBM
00604         WHEN PMCI-PRV-CPO-MET                                     ELXPMCBM
00605            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBM
00606               MOVE WS-CF-ZERO TO                                  ELXPMCBM
00607                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBM
00608            ELSE                                                   ELXPMCBM
00609               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K1'          ELXPMCBM
00610                  MOVE WS-CF-TRUE TO                               ELXPMCBM
00611                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00612               ELSE                                                ELXPMCBM
00613                  MOVE WS-CF-FALSE TO                              ELXPMCBM
00614                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00615               END-IF                                              ELXPMCBM
00616            END-IF                                                 ELXPMCBM
00617                                                                   ELXPMCBM
00618         WHEN PMCI-PRV-CPO-PPO-MET                                 ELXPMCBM
00619            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBM
00620               MOVE WS-CF-ZERO TO                                  ELXPMCBM
00621                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00622            ELSE                                                   ELXPMCBM
00623               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'KI'         ELXPMCBM
00624                     MOVE WS-CF-TRUE TO                            ELXPMCBM
00625                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00626               ELSE                                                ELXPMCBM
00627                  MOVE WS-CF-FALSE TO                              ELXPMCBM
00628                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00629               END-IF                                              ELXPMCBM
00630            END-IF                                                 ELXPMCBM
00631         WHEN PMCI-PRV-CPO-PPO-NOT-MET                             ELXPMCBM
00632            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBM
00633               MOVE WS-CF-ZERO TO                                  ELXPMCBM
00634                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00635            ELSE                                                   ELXPMCBM
00636               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K4'         ELXPMCBM
00637                     MOVE WS-CF-TRUE TO                            ELXPMCBM
00638                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00639               ELSE                                                ELXPMCBM
00640                  MOVE WS-CF-FALSE TO                              ELXPMCBM
00641                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00642               END-IF                                              ELXPMCBM
00643            END-IF                                                 ELXPMCBM
00644                                                                   ELXPMCBM
00645 *   ***** COMMUNITY BLUE.  RGO 4/96                               ELXPMCBM
00646         WHEN PMCI-PRV-CBL-IN                                      ELXPMCBM
00647            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBM
00648               MOVE WS-CF-ZERO TO                                  ELXPMCBM
00649                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBM
00650            ELSE                                                   ELXPMCBM
00651               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J1'          ELXPMCBM
00652                  MOVE WS-CF-TRUE TO                               ELXPMCBM
00653                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00654               ELSE                                                ELXPMCBM
00655                  MOVE WS-CF-FALSE TO                              ELXPMCBM
00656                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00657               END-IF                                              ELXPMCBM
00658            END-IF                                                 ELXPMCBM
00659                                                                   ELXPMCBM
00660         WHEN PMCI-PRV-CBL-OUT                                     ELXPMCBM
00661            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBM
00662               MOVE WS-CF-ZERO TO                                  ELXPMCBM
00663                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00664            ELSE                                                   ELXPMCBM
00665               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J4'         ELXPMCBM
00666                     MOVE WS-CF-TRUE TO                            ELXPMCBM
00667                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00668               ELSE                                                ELXPMCBM
00669                  MOVE WS-CF-FALSE TO                              ELXPMCBM
00670                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBM
00671               END-IF                                              ELXPMCBM
00672            END-IF                                                 ELXPMCBM
00673 *                                                                 ELXPMCBM
00674         WHEN PMCI-PRV-CALL                                        ELXPMCBM
00675            PERFORM 0190-SCN-SP-CCP-CALL                           ELXPMCBM
00676         WHEN OTHER                                                ELXPMCBM
00677            CONTINUE                                               ELXPMCBM
00678      END-EVALUATE.                                                ELXPMCBM
00679                                                                   ELXPMCBM
00680 ************************************************************      ELXPMCBM
00681 *                                                          *      ELXPMCBM
00682 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR PPO   *      ELXPMCBM
00683 *                                                          *      ELXPMCBM
00684 ************************************************************      ELXPMCBM
00685  0160-SCN-SP-CCP-IPPO.                                            ELXPMCBM
00686                                                                   ELXPMCBM
00687      EVALUATE TRUE                                                ELXPMCBM
00688         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00689           IF WS-PPO-INC-FOUND                                     ELXPMCBM
00690             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00691           ELSE                                                    ELXPMCBM
00692             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00693           END-IF                                                  ELXPMCBM
00694         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBM
00695             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00696             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00697         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBM
00698             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00699             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00700         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBM
00701             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00702             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00703         WHEN OTHER                                                ELXPMCBM
00704             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00705      END-EVALUATE.                                                ELXPMCBM
00706                                                                   ELXPMCBM
00707 ************************************************************      ELXPMCBM
00708 *                                                          *      ELXPMCBM
00709 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR BAE   *      ELXPMCBM
00710 *                                                          *      ELXPMCBM
00711 ************************************************************      ELXPMCBM
00712  0163-SCN-SP-CCP-IBAE.                                            ELXPMCBM
00713                                                                   ELXPMCBM
00714      EVALUATE TRUE                                                ELXPMCBM
00715         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00716           IF WS-BAE-PEN-FOUND                                     ELXPMCBM
00717             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00718           END-IF                                                  ELXPMCBM
00719         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBM
00720             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00721             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00722         WHEN OTHER                                                ELXPMCBM
00723             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00724      END-EVALUATE.                                                ELXPMCBM
00725                                                                   ELXPMCBM
00726 ************************************************************      ELXPMCBM
00727 *                                                          *      ELXPMCBM
00728 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-PPO   *      ELXPMCBM
00729 *                                                          *      ELXPMCBM
00730 ************************************************************      ELXPMCBM
00731  0161-SCN-SP-CCP-OPPO.                                            ELXPMCBM
00732                                                                   ELXPMCBM
00733      EVALUATE TRUE                                                ELXPMCBM
00734         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00735           IF WS-PPO-PEN-FOUND                                     ELXPMCBM
00736             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00737           ELSE                                                    ELXPMCBM
00738             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00739           END-IF                                                  ELXPMCBM
00740         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBM
00741             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00742             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00743         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBM
00744             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00745             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00746         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBM
00747             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00748             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00749         WHEN OTHER                                                ELXPMCBM
00750             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00751      END-EVALUATE.                                                ELXPMCBM
00752                                                                   ELXPMCBM
00753 ************************************************************      ELXPMCBM
00754 *                                                          *      ELXPMCBM
00755 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-BAE   *      ELXPMCBM
00756 *                                                          *      ELXPMCBM
00757 ************************************************************      ELXPMCBM
00758  0164-SCN-SP-CCP-OBAE.                                            ELXPMCBM
00759                                                                   ELXPMCBM
00760      EVALUATE TRUE                                                ELXPMCBM
00761         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00762           IF WS-BAE-PEN-FOUND                                     ELXPMCBM
00763             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00764           END-IF                                                  ELXPMCBM
00765         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBM
00766             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00767             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00768         WHEN OTHER                                                ELXPMCBM
00769             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00770      END-EVALUATE.                                                ELXPMCBM
00771                                                                   ELXPMCBM
00772 ************************************************************      ELXPMCBM
00773 *                                                          *      ELXPMCBM
00774 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR RPO   *      ELXPMCBM
00775 *                                                          *      ELXPMCBM
00776 ************************************************************      ELXPMCBM
00777  0170-SCN-SP-CCP-IRPO.                                            ELXPMCBM
00778                                                                   ELXPMCBM
00779      EVALUATE TRUE                                                ELXPMCBM
00780         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00781           IF WS-RPO-INC-FOUND                                     ELXPMCBM
00782             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00783           ELSE                                                    ELXPMCBM
00784             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00785           END-IF                                                  ELXPMCBM
00786         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBM
00787             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00788             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00789         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBM
00790             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00791             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00792         WHEN OTHER                                                ELXPMCBM
00793             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00794      END-EVALUATE.                                                ELXPMCBM
00795                                                                   ELXPMCBM
00796 ************************************************************      ELXPMCBM
00797 *                                                          *      ELXPMCBM
00798 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-RPO   *      ELXPMCBM
00799 *                                                          *      ELXPMCBM
00800 ************************************************************      ELXPMCBM
00801  0171-SCN-SP-CCP-ORPO.                                            ELXPMCBM
00802                                                                   ELXPMCBM
00803      EVALUATE TRUE                                                ELXPMCBM
00804         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00805           IF WS-RPO-PEN-FOUND                                     ELXPMCBM
00806             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00807           ELSE                                                    ELXPMCBM
00808             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00809           END-IF                                                  ELXPMCBM
00810         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBM
00811             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00812             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00813         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBM
00814             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00815             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00816         WHEN OTHER                                                ELXPMCBM
00817             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00818      END-EVALUATE.                                                ELXPMCBM
00819                                                                   ELXPMCBM
00820 ************************************************************      ELXPMCBM
00821 *                                                          *      ELXPMCBM
00822 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR MCNP  *      ELXPMCBM
00823 *                                                          *      ELXPMCBM
00824 ************************************************************      ELXPMCBM
00825  0180-SCN-SP-CCP-IMCNP.                                           ELXPMCBM
00826                                                                   ELXPMCBM
00827      EVALUATE TRUE                                                ELXPMCBM
00828         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00829           IF WS-MCNP-INC-FOUND                                    ELXPMCBM
00830             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00831           ELSE                                                    ELXPMCBM
00832             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00833           END-IF                                                  ELXPMCBM
00834         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBM
00835             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00836             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00837         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBM
00838             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00839             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00840         WHEN OTHER                                                ELXPMCBM
00841             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00842      END-EVALUATE.                                                ELXPMCBM
00843                                                                   ELXPMCBM
00844 ************************************************************      ELXPMCBM
00845 *                                                          *      ELXPMCBM
00846 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-MCNP  *      ELXPMCBM
00847 *                                                          *      ELXPMCBM
00848 ************************************************************      ELXPMCBM
00849  0181-SCN-SP-CCP-OMCNP.                                           ELXPMCBM
00850                                                                   ELXPMCBM
00851      EVALUATE TRUE                                                ELXPMCBM
00852         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00853           IF WS-MCNP-PEN-FOUND                                    ELXPMCBM
00854             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00855           ELSE                                                    ELXPMCBM
00856             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00857           END-IF                                                  ELXPMCBM
00858         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBM
00859             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00860             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00861         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBM
00862             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00863             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00864         WHEN OTHER                                                ELXPMCBM
00865             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00866      END-EVALUATE.                                                ELXPMCBM
00867                                                                   ELXPMCBM
00868 ************************************************************      ELXPMCBM
00869 *                                                          *      ELXPMCBM
00870 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PRG*      ELXPMCBM
00871 *                                                          *      ELXPMCBM
00872 ************************************************************      ELXPMCBM
00873  0190-SCN-SP-CCP-CALL.                                            ELXPMCBM
00874                                                                   ELXPMCBM
00875      EVALUATE TRUE                                                ELXPMCBM
00876         WHEN PMCI-PRG-PPO-APPLIES                                 ELXPMCBM
00877             PERFORM 0191-SCN-SP-CCP-CALL-PPO                      ELXPMCBM
00878         WHEN PMCI-PRG-RPO-APPLIES                                 ELXPMCBM
00879             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBM
00880         WHEN PMCI-PRG-RPO-PPO-APPLIES                             ELXPMCBM
00881             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBM
00882         WHEN PMCI-PRG-MCNP-APPLIES                                ELXPMCBM
00883             PERFORM 0193-SCN-SP-CCP-CALL-MCNP                     ELXPMCBM
00884         WHEN PMCI-PRG-BAE-APPLIES                                 ELXPMCBM
00885             PERFORM 0194-SCN-SP-CCP-CALL-BAE                      ELXPMCBM
00886         WHEN OTHER                                                ELXPMCBM
00887             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00888      END-EVALUATE.                                                ELXPMCBM
00889                                                                   ELXPMCBM
00890 ************************************************************      ELXPMCBM
00891 *                                                          *      ELXPMCBM
00892 *SET CALL FOR PPO FACTORS                                  *      ELXPMCBM
00893 *                                                          *      ELXPMCBM
00894 ************************************************************      ELXPMCBM
00895  0191-SCN-SP-CCP-CALL-PPO.                                        ELXPMCBM
00896                                                                   ELXPMCBM
00897      EVALUATE TRUE                                                ELXPMCBM
00898         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00899             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00900         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBM
00901             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00902             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00903         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBM
00904             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00905             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00906         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBM
00907             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00908             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00909         WHEN OTHER                                                ELXPMCBM
00910             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00911      END-EVALUATE.                                                ELXPMCBM
00912 ************************************************************      ELXPMCBM
00913 *                                                          *      ELXPMCBM
00914 *SET CALL FOR RPO FACTORS                                  *      ELXPMCBM
00915 *                                                          *      ELXPMCBM
00916 ************************************************************      ELXPMCBM
00917  0192-SCN-SP-CCP-CALL-RPO.                                        ELXPMCBM
00918                                                                   ELXPMCBM
00919      EVALUATE TRUE                                                ELXPMCBM
00920         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00921             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00922         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBM
00923             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00924             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00925         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBM
00926             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00927             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00928         WHEN OTHER                                                ELXPMCBM
00929             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00930      END-EVALUATE.                                                ELXPMCBM
00931 ************************************************************      ELXPMCBM
00932 *                                                          *      ELXPMCBM
00933 *SET CALL FOR MCNP FACTORS                                 *      ELXPMCBM
00934 *                                                          *      ELXPMCBM
00935 ************************************************************      ELXPMCBM
00936  0193-SCN-SP-CCP-CALL-MCNP.                                       ELXPMCBM
00937                                                                   ELXPMCBM
00938      EVALUATE TRUE                                                ELXPMCBM
00939         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00940             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00941         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBM
00942             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00943             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00944         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBM
00945             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00946             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00947         WHEN OTHER                                                ELXPMCBM
00948             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00949      END-EVALUATE.                                                ELXPMCBM
00950      IF PMCI-REFERRAL-EXISTS                                      ELXPMCBM
00951         MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX).      ELXPMCBM
00952                                                                   ELXPMCBM
00953 ************************************************************      ELXPMCBM
00954 *                                                          *      ELXPMCBM
00955 *SET CALL FOR BAE FACTORS                                  *      ELXPMCBM
00956 *                                                          *      ELXPMCBM
00957 ************************************************************      ELXPMCBM
00958  0194-SCN-SP-CCP-CALL-BAE.                                        ELXPMCBM
00959                                                                   ELXPMCBM
00960      EVALUATE TRUE                                                ELXPMCBM
00961         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBM
00962             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00963         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBM
00964             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBM
00965             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBM
00966         WHEN OTHER                                                ELXPMCBM
00967             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBM
00968      END-EVALUATE.                                                ELXPMCBM
00969                                                                   ELXPMCBM
00970 ************************************************************      ELXPMCBM
00971 *                                                          *      ELXPMCBM
00972 * IDENTIFY INSTITUTIONAL INPATIENT MAXIMUMS                *      ELXPMCBM
00973 *                                                          *      ELXPMCBM
00974 ************************************************************      ELXPMCBM
00975  1000-IDNTFY-INST-IP-MAXS.                                        ELXPMCBM
00976                                                                   ELXPMCBM
00977      PERFORM 1001-INITIALIZE.                                     ELXPMCBM
00978      PERFORM 1100-IDNTFY-INST-IP-DRB-MAX.                         ELXPMCBM
00979      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
00980         PERFORM 1200-IDNTFY-INST-IP-PDN-MAX                       ELXPMCBM
00981         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
00982            PERFORM 1300-IDNTFY-INST-IP-PSYCH-MAX                  ELXPMCBM
00983            PERFORM 1400-IDNTFY-INST-IP-SB-ABS-MAX.                ELXPMCBM
00984                                                                   ELXPMCBM
00985 ************************************************************      ELXPMCBM
00986 *                                                          *      ELXPMCBM
00987 * IDENTIFY INSTITUTIONAL INPATIENT MAXIMUMS/INITIALIZE     *      ELXPMCBM
00988 *                                                          *      ELXPMCBM
00989 ************************************************************      ELXPMCBM
00990  1001-INITIALIZE.                                                 ELXPMCBM
00991                                                                   ELXPMCBM
00992      CALL 'ELUADDRS' USING WS-INST-IP-DRB                         ELXPMCBM
00993                          WS-DLRB-POINTER.                         ELXPMCBM
00994      CALL 'ELUADDRS' USING WS-INST-IP-PDN                         ELXPMCBM
00995                          WS-PRDN-POINTER.                         ELXPMCBM
00996      CALL 'ELUADDRS' USING WS-INST-IP-PSYS                        ELXPMCBM
00997                          WS-PSYS-POINTER.                         ELXPMCBM
00998      CALL 'ELUADDRS' USING WS-INST-IP-DPSY                        ELXPMCBM
00999                          WS-NPSY-POINTER.                         ELXPMCBM
01000      CALL 'ELUADDRS' USING WS-INST-IP-NPSY                        ELXPMCBM
01001                          WS-DPSY-POINTER.                         ELXPMCBM
01002      CALL 'ELUADDRS' USING WS-INST-IP-ALC                         ELXPMCBM
01003                          WS-ALAB-POINTER.                         ELXPMCBM
01004      CALL 'ELUADDRS' USING WS-INST-IP-DRG                         ELXPMCBM
01005                          WS-DRAB-POINTER.                         ELXPMCBM
01006                                                                   ELXPMCBM
01007 ************************************************************      ELXPMCBM
01008 *                                                          *      ELXPMCBM
01009 * IDENTIFY INSTITUTIONAL INPATIENT DAILY ROOM/BOARD MAXIMUM*      ELXPMCBM
01010 *                                                          *      ELXPMCBM
01011 ************************************************************      ELXPMCBM
01012  1100-IDNTFY-INST-IP-DRB-MAX.                                     ELXPMCBM
01013                                                                   ELXPMCBM
01014      IF NAES-II-DRB-YES                                           ELXPMCBM
01015         PERFORM 5100-SRCH-DRB-MAX                                 ELXPMCBM
01016      ELSE                                                         ELXPMCBM
01017         SET PRRT-NO-COVERAGE TO TRUE                              ELXPMCBM
01018         SET PMCI-IQ-NO-COVERAGE TO TRUE.                          ELXPMCBM
01019                                                                   ELXPMCBM
01020 ************************************************************      ELXPMCBM
01021 *                                                          *      ELXPMCBM
01022 * IDENTIFY INSTITUTIONAL INPATIENT PRIVATE DUTY NURSING MAX*      ELXPMCBM
01023 *                                                          *      ELXPMCBM
01024 ************************************************************      ELXPMCBM
01025  1200-IDNTFY-INST-IP-PDN-MAX.                                     ELXPMCBM
01026                                                                   ELXPMCBM
01027      IF PMCI-PDN-COVERED                                          ELXPMCBM
01028         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCBM
01029      ELSE                                                         ELXPMCBM
01030         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCBM
01031                                                                   ELXPMCBM
01032 ************************************************************      ELXPMCBM
01033 *                                                          *      ELXPMCBM
01034 * IDENTIFY INSTITUTIONAL INPATIENT PSYCH SERVICES MAXIMUMS *      ELXPMCBM
01035 *                                                          *      ELXPMCBM
01036 ************************************************************      ELXPMCBM
01037  1300-IDNTFY-INST-IP-PSYCH-MAX.                                   ELXPMCBM
01038                                                                   ELXPMCBM
01039      IF PSY-YES                                                   ELXPMCBM
01040         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCBM
01041      ELSE                                                         ELXPMCBM
01042         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCBM
01043         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCBM
01044         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCBM
01045         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCBM
01046      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01047         IF NAES-II-DPSY-YES OR NAES-II-NPSY-YES                   ELXPMCBM
01048            PERFORM 7000-SRCH-DAY-NGHT-PSYCH-MAX                   ELXPMCBM
01049         ELSE                                                      ELXPMCBM
01050            SET DNPD-NO-COVERAGE TO TRUE                           ELXPMCBM
01051            SET DNPN-NO-COVERAGE TO TRUE.                          ELXPMCBM
01052                                                                   ELXPMCBM
01053 ************************************************************      ELXPMCBM
01054 *                                                          *      ELXPMCBM
01055 * IDENTIFY INSTITUTIONAL INPATIENT SUBSTANCE ABUSE MAXIMUMS*      ELXPMCBM
01056 *                                                          *      ELXPMCBM
01057 ************************************************************      ELXPMCBM
01058  1400-IDNTFY-INST-IP-SB-ABS-MAX.                                  ELXPMCBM
01059                                                                   ELXPMCBM
01060      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCBM
01061         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCBM
01062      ELSE                                                         ELXPMCBM
01063         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCBM
01064         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCBM
01065                                                                   ELXPMCBM
01066 ************************************************************      ELXPMCBM
01067 *                                                          *      ELXPMCBM
01068 * IDENTIFY INSTITUTIONAL OUTPATIENT MAXIMUMS               *      ELXPMCBM
01069 *                                                          *      ELXPMCBM
01070 ************************************************************      ELXPMCBM
01071  2000-IDNTFY-INST-OP-MAXS.                                        ELXPMCBM
01072                                                                   ELXPMCBM
01073      PERFORM 2001-INITIALIZE.                                     ELXPMCBM
01074      PERFORM 2200-IDNTFY-INST-OP-PDN-MAX.                         ELXPMCBM
01075      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01076         PERFORM 2300-IDNTFY-INST-OP-PSYCH-MAX                     ELXPMCBM
01077         PERFORM 2400-IDNTFY-INST-OP-SB-ABS-MAX.                   ELXPMCBM
01078                                                                   ELXPMCBM
01079 ************************************************************      ELXPMCBM
01080 *                                                          *      ELXPMCBM
01081 * IDENTIFY INSTITUTIONAL OUTPATIENT MAXIMUMS/INITIALIZE    *      ELXPMCBM
01082 *                                                          *      ELXPMCBM
01083 ************************************************************      ELXPMCBM
01084  2001-INITIALIZE.                                                 ELXPMCBM
01085                                                                   ELXPMCBM
01086      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCBM
01087      SET WS-NPSY-POINTER TO NULL.                                 ELXPMCBM
01088      SET WS-DPSY-POINTER TO NULL.                                 ELXPMCBM
01089      CALL 'ELUADDRS' USING WS-INST-OP-PDN                         ELXPMCBM
01090                          WS-PRDN-POINTER.                         ELXPMCBM
01091      CALL 'ELUADDRS' USING WS-INST-OP-PSYS                        ELXPMCBM
01092                          WS-PSYS-POINTER.                         ELXPMCBM
01093      CALL 'ELUADDRS' USING WS-INST-OP-ALC                         ELXPMCBM
01094                          WS-ALAB-POINTER.                         ELXPMCBM
01095      CALL 'ELUADDRS' USING WS-INST-OP-DRG                         ELXPMCBM
01096                          WS-DRAB-POINTER.                         ELXPMCBM
01097      SET DNPD-NOT-APPLICABLE TO TRUE.                             ELXPMCBM
01098      SET DNPN-NOT-APPLICABLE TO TRUE.                             ELXPMCBM
01099      SET PRRT-NO-COVERAGE TO TRUE.                                ELXPMCBM
01100      SET PMCI-IQ-NO-COVERAGE TO TRUE.                             ELXPMCBM
01101                                                                   ELXPMCBM
01102 ************************************************************      ELXPMCBM
01103 *                                                          *      ELXPMCBM
01104 * IDENTIFY INSTITUTIONAL OUTPATIENT DAILY ROOM/BOARD MAXIMUM*     ELXPMCBM
01105 *                                                          *      ELXPMCBM
01106 ************************************************************      ELXPMCBM
01107  2200-IDNTFY-INST-OP-PDN-MAX.                                     ELXPMCBM
01108                                                                   ELXPMCBM
01109      IF PMCI-PDN-COVERED                                          ELXPMCBM
01110         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCBM
01111      ELSE                                                         ELXPMCBM
01112         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCBM
01113                                                                   ELXPMCBM
01114 ************************************************************      ELXPMCBM
01115 *                                                          *      ELXPMCBM
01116 * IDENTIFY INSTITUTIONAL OUTPATIENT PSYCH SERVICES MAXIMUMS*      ELXPMCBM
01117 *                                                          *      ELXPMCBM
01118 ************************************************************      ELXPMCBM
01119  2300-IDNTFY-INST-OP-PSYCH-MAX.                                   ELXPMCBM
01120                                                                   ELXPMCBM
01121      IF PSY-YES                                                   ELXPMCBM
01122         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCBM
01123      ELSE                                                         ELXPMCBM
01124         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCBM
01125         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCBM
01126         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCBM
01127         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCBM
01128                                                                   ELXPMCBM
01129 ************************************************************      ELXPMCBM
01130 *                                                          *      ELXPMCBM
01131 * IDENTIFY INSTITUTIONAL OUTPATIENT SUBSTANCE ABUSE MAXIMUMS*     ELXPMCBM
01132 *                                                          *      ELXPMCBM
01133 ************************************************************      ELXPMCBM
01134  2400-IDNTFY-INST-OP-SB-ABS-MAX.                                  ELXPMCBM
01135                                                                   ELXPMCBM
01136      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCBM
01137         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCBM
01138      ELSE                                                         ELXPMCBM
01139         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCBM
01140         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCBM
01141                                                                   ELXPMCBM
01142 ************************************************************      ELXPMCBM
01143 *                                                          *      ELXPMCBM
01144 * IDENTIFY PROFESSIONAL INPATIENT MAXIMUMS                 *      ELXPMCBM
01145 *                                                          *      ELXPMCBM
01146 ************************************************************      ELXPMCBM
01147  3000-IDNTFY-PROF-IP-MAXS.                                        ELXPMCBM
01148                                                                   ELXPMCBM
01149      PERFORM 3001-INITIALIZE.                                     ELXPMCBM
01150      PERFORM 3200-IDNTFY-PROF-IP-PDN-MAX.                         ELXPMCBM
01151      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01152         PERFORM 3300-IDNTFY-PROF-IP-PSYCH-MAX                     ELXPMCBM
01153         PERFORM 3400-IDNTFY-PROF-IP-SB-ABS-MAX.                   ELXPMCBM
01154                                                                   ELXPMCBM
01155 ************************************************************      ELXPMCBM
01156 *                                                          *      ELXPMCBM
01157 * IDENTIFY PROFESSIONAL INPATIENT MAXIMUMS/INITIALIZE      *      ELXPMCBM
01158 *                                                          *      ELXPMCBM
01159 ************************************************************      ELXPMCBM
01160  3001-INITIALIZE.                                                 ELXPMCBM
01161                                                                   ELXPMCBM
01162      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCBM
01163      CALL 'ELUADDRS' USING WS-PROF-IP-PDN                         ELXPMCBM
01164                          WS-PRDN-POINTER.                         ELXPMCBM
01165      CALL 'ELUADDRS' USING WS-PROF-IP-PSYS                        ELXPMCBM
01166                          WS-PSYS-POINTER.                         ELXPMCBM
01167      CALL 'ELUADDRS' USING WS-PROF-IP-ALC                         ELXPMCBM
01168                          WS-ALAB-POINTER.                         ELXPMCBM
01169      CALL 'ELUADDRS' USING WS-PROF-IP-DRG                         ELXPMCBM
01170                          WS-DRAB-POINTER.                         ELXPMCBM
01171      CALL 'ELUADDRS' USING WS-PROF-IP-DPV                         ELXPMCBM
01172                          WS-DPSY-POINTER.                         ELXPMCBM
01173      CALL 'ELUADDRS' USING WS-PROF-IP-NPV                         ELXPMCBM
01174                          WS-NPSY-POINTER.                         ELXPMCBM
01175      SET DRB-NOT-APPLICABLE TO TRUE.                              ELXPMCBM
01176                                                                   ELXPMCBM
01177 ************************************************************      ELXPMCBM
01178 *                                                          *      ELXPMCBM
01179 * IDENTIFY PROFESSIONAL INPATIENT PRIVATE DUTY NURSING MAX*       ELXPMCBM
01180 *                                                          *      ELXPMCBM
01181 ************************************************************      ELXPMCBM
01182  3200-IDNTFY-PROF-IP-PDN-MAX.                                     ELXPMCBM
01183                                                                   ELXPMCBM
01184      IF PMCI-PDN-COVERED                                          ELXPMCBM
01185         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCBM
01186      ELSE                                                         ELXPMCBM
01187         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCBM
01188                                                                   ELXPMCBM
01189 ************************************************************      ELXPMCBM
01190 *                                                          *      ELXPMCBM
01191 * IDENTIFY PROFESSIONAL INPATIENT PSYCH SERVICES MAXIMUMS *       ELXPMCBM
01192 *                                                          *      ELXPMCBM
01193 ************************************************************      ELXPMCBM
01194  3300-IDNTFY-PROF-IP-PSYCH-MAX.                                   ELXPMCBM
01195                                                                   ELXPMCBM
01196      IF PSY-YES                                                   ELXPMCBM
01197         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCBM
01198      ELSE                                                         ELXPMCBM
01199         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCBM
01200         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCBM
01201         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCBM
01202         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCBM
01203      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01204         IF NAES-PI-DPV-YES OR NAES-PI-NPV-YES                     ELXPMCBM
01205            PERFORM 7000-SRCH-DAY-NGHT-PSYCH-MAX                   ELXPMCBM
01206         ELSE                                                      ELXPMCBM
01207            SET DNPD-NO-COVERAGE TO TRUE                           ELXPMCBM
01208            SET DNPN-NO-COVERAGE TO TRUE.                          ELXPMCBM
01209                                                                   ELXPMCBM
01210 ************************************************************      ELXPMCBM
01211 *                                                          *      ELXPMCBM
01212 * IDENTIFY PROFESSIONAL INPATIENT SUBSTANCE ABUSE MAXIMUMS*       ELXPMCBM
01213 *                                                          *      ELXPMCBM
01214 ************************************************************      ELXPMCBM
01215  3400-IDNTFY-PROF-IP-SB-ABS-MAX.                                  ELXPMCBM
01216                                                                   ELXPMCBM
01217      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCBM
01218         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCBM
01219      ELSE                                                         ELXPMCBM
01220         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCBM
01221         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCBM
01222                                                                   ELXPMCBM
01223 ************************************************************      ELXPMCBM
01224 *                                                          *      ELXPMCBM
01225 * IDENTIFY PROFESSIONAL OUTPATIENT MAXIMUMS                *      ELXPMCBM
01226 *                                                          *      ELXPMCBM
01227 ************************************************************      ELXPMCBM
01228  4000-IDNTFY-PROF-OP-MAXS.                                        ELXPMCBM
01229                                                                   ELXPMCBM
01230      PERFORM 4001-INITIALIZE.                                     ELXPMCBM
01231      PERFORM 4200-IDNTFY-PROF-OP-PDN-MAX.                         ELXPMCBM
01232      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01233         PERFORM 4300-IDNTFY-PROF-OP-PSYCH-MAX                     ELXPMCBM
01234         PERFORM 4400-IDNTFY-PROF-OP-SB-ABS-MAX.                   ELXPMCBM
01235                                                                   ELXPMCBM
01236 ************************************************************      ELXPMCBM
01237 *                                                          *      ELXPMCBM
01238 * IDENTIFY PROFESSIONAL OUTPATIENT MAXIMUMS/INITIALIZE     *      ELXPMCBM
01239 *                                                          *      ELXPMCBM
01240 ************************************************************      ELXPMCBM
01241  4001-INITIALIZE.                                                 ELXPMCBM
01242                                                                   ELXPMCBM
01243      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCBM
01244      SET WS-NPSY-POINTER TO NULL.                                 ELXPMCBM
01245      SET WS-DPSY-POINTER TO NULL.                                 ELXPMCBM
01246      CALL 'ELUADDRS' USING WS-PROF-OP-PDN                         ELXPMCBM
01247                          WS-PRDN-POINTER.                         ELXPMCBM
01248      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCBM
01249                          WS-PSYS-POINTER.                         ELXPMCBM
01250      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCBM
01251                          WS-ALAB-POINTER.                         ELXPMCBM
01252      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCBM
01253                          WS-DRAB-POINTER.                         ELXPMCBM
01254      SET DRB-NOT-APPLICABLE TO TRUE.                              ELXPMCBM
01255      SET DNPD-NOT-APPLICABLE TO TRUE.                             ELXPMCBM
01256      SET DNPN-NOT-APPLICABLE TO TRUE.                             ELXPMCBM
01257                                                                   ELXPMCBM
01258 ************************************************************      ELXPMCBM
01259 *                                                          *      ELXPMCBM
01260 * IDENTIFY PROFESSIONAL OUTPATIENT DAILY ROOM/BOARD MAXIMUM*      ELXPMCBM
01261 *                                                          *      ELXPMCBM
01262 ************************************************************      ELXPMCBM
01263  4200-IDNTFY-PROF-OP-PDN-MAX.                                     ELXPMCBM
01264                                                                   ELXPMCBM
01265      IF PMCI-PDN-COVERED                                          ELXPMCBM
01266         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCBM
01267      ELSE                                                         ELXPMCBM
01268         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCBM
01269                                                                   ELXPMCBM
01270 ************************************************************      ELXPMCBM
01271 *                                                          *      ELXPMCBM
01272 * IDENTIFY PROFESSIONAL OUTPATIENT PSYCH SERVICES MAXIMUMS*       ELXPMCBM
01273 *                                                          *      ELXPMCBM
01274 ************************************************************      ELXPMCBM
01275  4300-IDNTFY-PROF-OP-PSYCH-MAX.                                   ELXPMCBM
01276                                                                   ELXPMCBM
01277      IF PSY-YES                                                   ELXPMCBM
01278         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCBM
01279      ELSE                                                         ELXPMCBM
01280         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCBM
01281         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCBM
01282         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCBM
01283         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCBM
01284                                                                   ELXPMCBM
01285 ************************************************************      ELXPMCBM
01286 *                                                          *      ELXPMCBM
01287 * IDENTIFY PROFESSIONAL OUTPATIENT SUBSTANCE ABUSE MAXIMUMS*      ELXPMCBM
01288 *                                                          *      ELXPMCBM
01289 ************************************************************      ELXPMCBM
01290  4400-IDNTFY-PROF-OP-SB-ABS-MAX.                                  ELXPMCBM
01291                                                                   ELXPMCBM
01292      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCBM
01293         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCBM
01294      ELSE                                                         ELXPMCBM
01295         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCBM
01296         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCBM
01297                                                                   ELXPMCBM
01298 ************************************************************      ELXPMCBM
01299 *                                                          *      ELXPMCBM
01300 * SEARCH FOR DAILY ROOM AND BOARD MAXIMUMS                 *      ELXPMCBM
01301 *                                                          *      ELXPMCBM
01302 ************************************************************      ELXPMCBM
01303  5100-SRCH-DRB-MAX.                                               ELXPMCBM
01304                                                                   ELXPMCBM
01305      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBM
01306         SET PMCI-IQ-NOT-APPLICABLE TO TRUE                        ELXPMCBM
01307         CONTINUE                                                  ELXPMCBM
01308      ELSE                                                         ELXPMCBM
01309         PERFORM 5101-RCMPT-BNFT-PRVSN-DRB                         ELXPMCBM
01310         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01311            PERFORM 5110-RCMPT-CF-DRB                              ELXPMCBM
01312               VARYING ATBL-IDX FROM 1 BY 1                        ELXPMCBM
01313                  UNTIL ATBL-IDX > ATBL-MAX-IDX OR                 ELXPMCBM
01314                    PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO            ELXPMCBM
01315            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01316               MOVE CVG2-OV-THRSHLD-DRB TO WS-TEST-THRESHOLD       ELXPMCBM
01317               PERFORM 5160-SRCH-DRB-DAY-MAX                       ELXPMCBM
01318            END-IF                                                 ELXPMCBM
01319         END-IF                                                    ELXPMCBM
01320      END-IF.                                                      ELXPMCBM
01321                                                                   ELXPMCBM
01322 ************************************************************      ELXPMCBM
01323 *                                                          *      ELXPMCBM
01324 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR DAILY ROOM/BOARD  *      ELXPMCBM
01325 *                                                          *      ELXPMCBM
01326 ************************************************************      ELXPMCBM
01327  5101-RCMPT-BNFT-PRVSN-DRB.                                       ELXPMCBM
01328                                                                   ELXPMCBM
01329      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DLRB-POINTER.       ELXPMCBM
01330      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
01331      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
01332                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
01333      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBM
01334      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
01335         MOVE +4607 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
01336         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
01337                                                                   ELXPMCBM
01338 ************************************************************      ELXPMCBM
01339 *                                                          *      ELXPMCBM
01340 * RECOMPUTE CONF FACTORS FOR DAILY ROOM AND BOARD          *      ELXPMCBM
01341 *                                                          *      ELXPMCBM
01342 ************************************************************      ELXPMCBM
01343  5110-RCMPT-CF-DRB.                                               ELXPMCBM
01344                                                                   ELXPMCBM
01345      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCBM
01346      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01347         MOVE CFTA-CF-DRB (CFTA-IDX) TO                            ELXPMCBM
01348                         ATBL-CF-BNFT-PRD (ATBL-IDX)               ELXPMCBM
01349         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBM
01350         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCBM
01351         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01352            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCBM
01353                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCBM
01354            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBM
01355            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01356               MOVE CFT5-CF-INTD-RM-BRD (CFT5-IDX) TO              ELXPMCBM
01357                      ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX).        ELXPMCBM
01358                                                                   ELXPMCBM
01359 ************************************************************      ELXPMCBM
01360 *                                                          *      ELXPMCBM
01361 * SEARCH FOR DAILY ROOM AND BOARD DAY MAXIMUM              *      ELXPMCBM
01362 *                                                          *      ELXPMCBM
01363 ************************************************************      ELXPMCBM
01364  5160-SRCH-DRB-DAY-MAX.                                           ELXPMCBM
01365                                                                   ELXPMCBM
01366      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCBM
01367          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCBM
01368              UNTIL ATBL-IDX > ATBL-MAX-IDX.                       ELXPMCBM
01369      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01370      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01371         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01372         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01373            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01374            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01375                 ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCBM
01376                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01377                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1' OR    ELXPMCBM
01378                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01379               SET PMCI-IQ-CALL TO TRUE                            ELXPMCBM
01380            ELSE                                                   ELXPMCBM
01381               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCBM
01382                                                          OR '6'   ELXPMCBM
01383                  SET PMCI-IQ-CALL TO TRUE                         ELXPMCBM
01384               ELSE                                                ELXPMCBM
01385                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
01386                  PERFORM 9600-DETERMINE-VALUE-LONG                ELXPMCBM
01387                  MOVE WS-VALUE-AMT-LONG TO                        ELXPMCBM
01388                                 PMCI-MAX-INPATIENT                ELXPMCBM
01389                  MOVE WS-BNF-QUAL TO PMCI-MAX-INPATIENT-QUALIFIER ELXPMCBM
01390                END-IF                                             ELXPMCBM
01391            END-IF                                                 ELXPMCBM
01392         ELSE                                                      ELXPMCBM
01393            SET PMCI-IQ-NOT-APPLICABLE TO TRUE                     ELXPMCBM
01394         END-IF                                                    ELXPMCBM
01395      END-IF.                                                      ELXPMCBM
01396                                                                   ELXPMCBM
01397 ************************************************************      ELXPMCBM
01398 *                                                          *      ELXPMCBM
01399 * SEARCH FOR PRIVATE DUTY NURSING MAXIMUMS                 *      ELXPMCBM
01400 *                                                          *      ELXPMCBM
01401 ************************************************************      ELXPMCBM
01402  5200-SRCH-PDN-DLR-MAX.                                           ELXPMCBM
01403                                                                   ELXPMCBM
01404      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBM
01405         SET PRDN-NOT-APPLICABLE TO TRUE                           ELXPMCBM
01406      ELSE                                                         ELXPMCBM
01407         PERFORM 5201-RCMPT-BNFT-PRVSN-PDN                         ELXPMCBM
01408         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01409            PERFORM 5220-SCN-PDN-DLR-MAX.                          ELXPMCBM
01410                                                                   ELXPMCBM
01411 ************************************************************      ELXPMCBM
01412 *                                                          *      ELXPMCBM
01413 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR PRIVATE DUTY NURSE*      ELXPMCBM
01414 *                                                          *      ELXPMCBM
01415 ************************************************************      ELXPMCBM
01416  5201-RCMPT-BNFT-PRVSN-PDN.                                       ELXPMCBM
01417                                                                   ELXPMCBM
01418      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-PRDN-POINTER.       ELXPMCBM
01419      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
01420      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
01421                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
01422      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCBM
01423      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
01424         MOVE +4608 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
01425         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
01426                                                                   ELXPMCBM
01427 ************************************************************      ELXPMCBM
01428 *                                                          *      ELXPMCBM
01429 * SEARCH FOR PRIVATE DUTY NURSING DOLLAR MAXIMUM           *      ELXPMCBM
01430 *                                                          *      ELXPMCBM
01431 ************************************************************      ELXPMCBM
01432  5220-SCN-PDN-DLR-MAX.                                            ELXPMCBM
01433                                                                   ELXPMCBM
01434      PERFORM 5230-RCMPT-SP-PDN-DLR                                ELXPMCBM
01435          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCBM
01436              UNTIL ATBL-IDX > ATBL-MAX-IDX OR                     ELXPMCBM
01437                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.              ELXPMCBM
01438      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01439         PERFORM 5290-TST-PDN-DLR-MAX.                             ELXPMCBM
01440                                                                   ELXPMCBM
01441 ************************************************************      ELXPMCBM
01442 *                                                          *      ELXPMCBM
01443 * RECOMPUTE CONF FACTORS FOR PRIVATE DUTY NURSING          *      ELXPMCBM
01444 *                                                          *      ELXPMCBM
01445 ************************************************************      ELXPMCBM
01446  5230-RCMPT-SP-PDN-DLR.                                           ELXPMCBM
01447                                                                   ELXPMCBM
01448      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCBM
01449      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01450         MOVE CFTA-CF-PDN (CFTA-IDX) TO                            ELXPMCBM
01451                     ATBL-CF-BNFT-PRD (ATBL-IDX)                   ELXPMCBM
01452         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBM
01453         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCBM
01454         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01455            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCBM
01456                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCBM
01457            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBM
01458            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01459               MOVE CFT5-CF-INTD-NRSNG (CFT5-IDX) TO               ELXPMCBM
01460                        ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)       ELXPMCBM
01461               PERFORM 5235-NURSING-DAYS-VISITS.                   ELXPMCBM
01462                                                                   ELXPMCBM
01463 ******************************************************************ELXPMCBM
01464 *5235-NURSING-DAYS-VISITS.                                        ELXPMCBM
01465 *                                                                 ELXPMCBM
01466 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DAYS AND VISITS        ELXPMCBM
01467 *                                                                 ELXPMCBM
01468 * BECAUSE PRIVATE NURSING IS NOT DESIGNED TO HANDLE DOLLARS, A NEWELXPMCBM
01469 * PARAGRAGH WAS CREATED TO DEAL WITH ALL QUALIFIER VALUES.  THIS  ELXPMCBM
01470 * IS SIMILIAR TO 9153-, BUT IS HANDLES A VALUE OF '5', AND THE    ELXPMCBM
01471 * WS-CF VALUES HAVE BEEN MODIFIED.                RGO 12/10/93.   ELXPMCBM
01472 *                                                                 ELXPMCBM
01473 ******************************************************************ELXPMCBM
01474  5235-NURSING-DAYS-VISITS.                                        ELXPMCBM
01475                                                                   ELXPMCBM
01476      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '3'              ELXPMCBM
01477         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCBM
01478      ELSE                                                         ELXPMCBM
01479      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'              ELXPMCBM
01480         MOVE WS-CF-85 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)            ELXPMCBM
01481      ELSE                                                         ELXPMCBM
01482      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '5'                     ELXPMCBM
01483         MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)            ELXPMCBM
01484      ELSE                                                         ELXPMCBM
01485         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCBM
01486                                                                   ELXPMCBM
01487 ************************************************************      ELXPMCBM
01488 *                                                          *      ELXPMCBM
01489 * SEARCH FOR PRIVATE DUTY NURSING DAY MAXIMUM              *      ELXPMCBM
01490 *                                                          *      ELXPMCBM
01491 * - ADDED CALL FOR ATBL-QUALIFIER-VALUE OF 5. RGO 12/10/93 *      ELXPMCBM
01492 *                                                          *      ELXPMCBM
01493 ************************************************************      ELXPMCBM
01494  5290-TST-PDN-DLR-MAX.                                            ELXPMCBM
01495                                                                   ELXPMCBM
01496      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCBM
01497      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01498         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01499         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01500            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01501            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCBM
01502               OR                                                  ELXPMCBM
01503                (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCBM
01504                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01505                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1')      ELXPMCBM
01506               OR                                                  ELXPMCBM
01507                ATBL-VALUE-QUALIFIER(ATBL-IDX) = '5'               ELXPMCBM
01508               OR                                                  ELXPMCBM
01509                WS-NUM-APPL-ENTRS > 1                              ELXPMCBM
01510               SET PRDN-CALL TO TRUE                               ELXPMCBM
01511            ELSE                                                   ELXPMCBM
01512               PERFORM 9110-FND-BNFT-PRD-CF                        ELXPMCBM
01513               PERFORM 9650-DETERMINE-VALUE-SHORT                  ELXPMCBM
01514               MOVE WS-VALUE-AMT-SHORT TO                          ELXPMCBM
01515                                 PMCI-PRIVATE-DUTY-NURSE-MAX       ELXPMCBM
01516               MOVE WS-BNF-QUAL TO PMCI-PRIVATE-DUTY-NURSE-QUAL    ELXPMCBM
01517               MOVE WS-SAVE-LOB-IND TO PMCI-PRVTE-DTY-NRS-FROM-IND ELXPMCBM
01518            END-IF                                                 ELXPMCBM
01519         ELSE                                                      ELXPMCBM
01520            SET PRDN-NOT-APPLICABLE TO TRUE                        ELXPMCBM
01521         END-IF                                                    ELXPMCBM
01522      END-IF.                                                      ELXPMCBM
01523                                                                   ELXPMCBM
01524 ************************************************************      ELXPMCBM
01525 *                                                          *      ELXPMCBM
01526 *     SEARCH FOR MENTAL MAXIMUMS                           *      ELXPMCBM
01527 *                                                          *      ELXPMCBM
01528 ************************************************************      ELXPMCBM
01529  6000-SRCH-MNTL-MAX.                                              ELXPMCBM
01530                                                                   ELXPMCBM
01531      PERFORM 6001-RCMPT-BNFT-PRVSN-PSYCH                          ELXPMCBM
01532      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01533         PERFORM 6010-RCMPT-CF-MNTL-CNDTNS                         ELXPMCBM
01534            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBM
01535               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCBM
01536                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO               ELXPMCBM
01537         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01538            MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD         ELXPMCBM
01539            PERFORM 6020-SRCH-PSYCH-LFTM-MAX                       ELXPMCBM
01540            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01541               PERFORM 6030-SRCH-PSYC-OTHR-MAX                     ELXPMCBM
01542            END-IF                                                 ELXPMCBM
01543         END-IF                                                    ELXPMCBM
01544      END-IF.                                                      ELXPMCBM
01545                                                                   ELXPMCBM
01546                                                                   ELXPMCBM
01547 ************************************************************      ELXPMCBM
01548 *                                                          *      ELXPMCBM
01549 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR PSYCHIATRIC SERV  *      ELXPMCBM
01550 *                                                          *      ELXPMCBM
01551 ************************************************************      ELXPMCBM
01552  6001-RCMPT-BNFT-PRVSN-PSYCH.                                     ELXPMCBM
01553                                                                   ELXPMCBM
01554      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-PSYS-POINTER.       ELXPMCBM
01555      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
01556      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
01557                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
01558      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBM
01559      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
01560         MOVE +4611 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
01561         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
01562                                                                   ELXPMCBM
01563 ************************************************************      ELXPMCBM
01564 *                                                          *      ELXPMCBM
01565 * RECOMPUTE CONF FACTORS FOR MENTAL CONDITIONS             *      ELXPMCBM
01566 *                                                          *      ELXPMCBM
01567 ************************************************************      ELXPMCBM
01568  6010-RCMPT-CF-MNTL-CNDTNS.                                       ELXPMCBM
01569                                                                   ELXPMCBM
01570      MOVE WS-CF-ZERO TO WS-NO-IBGR-CF.                            ELXPMCBM
01571      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCBM
01572      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01573         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBM
01574         PERFORM 9020-RCMPT-CNDTN-BT-MNTL                          ELXPMCBM
01575         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01576            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBM
01577            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01578               MOVE CFT5-CF-INTD-PSYCH (CFT5-IDX) TO               ELXPMCBM
01579                     ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX).         ELXPMCBM
01580                                                                   ELXPMCBM
01581 ************************************************************      ELXPMCBM
01582 *                                                          *      ELXPMCBM
01583 *     SEARCH FOR PSYCHIATRIC SERVICE LIFETIME MAXIMUMS     *      ELXPMCBM
01584 *                                                          *      ELXPMCBM
01585 ************************************************************      ELXPMCBM
01586  6020-SRCH-PSYCH-LFTM-MAX.                                        ELXPMCBM
01587                                                                   ELXPMCBM
01588      PERFORM 6911-RCMPT-BNFT-PRD-LFTM-MNTL                        ELXPMCBM
01589         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01590            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCBM
01591      PERFORM 6100-SRCH-LFTM-MNTL-DLR-MAX.                         ELXPMCBM
01592      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01593         PERFORM 6200-SRCH-LFTM-MNTL-DAY-MAX.                      ELXPMCBM
01594                                                                   ELXPMCBM
01595 ************************************************************      ELXPMCBM
01596 *                                                          *      ELXPMCBM
01597 *     SEARCH FOR PSYCHIATRIC SERVICES OTHER MAXIMUMS       *      ELXPMCBM
01598 *                                                          *      ELXPMCBM
01599 ************************************************************      ELXPMCBM
01600  6030-SRCH-PSYC-OTHR-MAX.                                         ELXPMCBM
01601                                                                   ELXPMCBM
01602      PERFORM 6912-RCMPT-BNFT-PRD-OTHR-MNTL                        ELXPMCBM
01603         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01604            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCBM
01605               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCBM
01606      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01607         PERFORM 6300-SRCH-OTHR-MNTL-DLR-MAX                       ELXPMCBM
01608         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01609            PERFORM 6400-SRCH-OTHR-MNTL-DAY-MAX.                   ELXPMCBM
01610                                                                   ELXPMCBM
01611 ************************************************************      ELXPMCBM
01612 *                                                          *      ELXPMCBM
01613 * SEARCH FOR LIFETIME MENTAL DOLLAR MAXIMUMS               *      ELXPMCBM
01614 *                                                          *      ELXPMCBM
01615 ************************************************************      ELXPMCBM
01616  6100-SRCH-LFTM-MNTL-DLR-MAX.                                     ELXPMCBM
01617                                                                   ELXPMCBM
01618      PERFORM 9151-RCMPT-VL-QLFR-CF-DLR                            ELXPMCBM
01619         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01620            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCBM
01621      PERFORM 6190-TST-LFTM-MNTL-DLR-MAX.                          ELXPMCBM
01622                                                                   ELXPMCBM
01623 ************************************************************      ELXPMCBM
01624 *                                                          *      ELXPMCBM
01625 * TEST AND SET LIFETIME MENTAL DOLLAR MAXIMUM              *      ELXPMCBM
01626 *                                                          *      ELXPMCBM
01627 ************************************************************      ELXPMCBM
01628  6190-TST-LFTM-MNTL-DLR-MAX.                                      ELXPMCBM
01629                                                                   ELXPMCBM
01630      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01631      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01632         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01633         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01634            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01635            MOVE WS-SAVE-SUB TO WS-LFM-DLR-SUB                     ELXPMCBM
01636            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01637                (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCBM
01638                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01639                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
01640                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01641               SET MNDD-CALL TO TRUE                               ELXPMCBM
01642            ELSE                                                   ELXPMCBM
01643               PERFORM 9110-FND-BNFT-PRD-CF                        ELXPMCBM
01644               PERFORM 9650-DETERMINE-VALUE-SHORT                  ELXPMCBM
01645               MOVE WS-VALUE-AMT-SHORT TO                          ELXPMCBM
01646                                 PMCI-LIFETIME-MENTAL-MAX          ELXPMCBM
01647               MOVE WS-BNF-QUAL TO PMCI-LIFETIME-MENTAL-MAX-QUAL   ELXPMCBM
01648               MOVE WS-SAVE-LOB-IND TO PMCI-LFTM-MNTL-FROM-IND     ELXPMCBM
01649            END-IF                                                 ELXPMCBM
01650         ELSE                                                      ELXPMCBM
01651            SET MNDD-NOT-APPLICABLE TO TRUE                        ELXPMCBM
01652         END-IF                                                    ELXPMCBM
01653      END-IF.                                                      ELXPMCBM
01654                                                                   ELXPMCBM
01655 ************************************************************      ELXPMCBM
01656 *                                                          *      ELXPMCBM
01657 * SEARCH FOR LIFETIME MENTAL DAY MAXIMUMS                  *      ELXPMCBM
01658 *                                                          *      ELXPMCBM
01659 ************************************************************      ELXPMCBM
01660  6200-SRCH-LFTM-MNTL-DAY-MAX.                                     ELXPMCBM
01661                                                                   ELXPMCBM
01662      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCBM
01663         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01664            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCBM
01665               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCBM
01666      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01667         PERFORM 6290-TST-LFTM-MNTL-DAY-MAX.                       ELXPMCBM
01668                                                                   ELXPMCBM
01669 ************************************************************      ELXPMCBM
01670 *                                                          *      ELXPMCBM
01671 * TEST AND SET LIFETIME MENTAL DAY MAXIMUM                 *      ELXPMCBM
01672 *                                                          *      ELXPMCBM
01673 ************************************************************      ELXPMCBM
01674  6290-TST-LFTM-MNTL-DAY-MAX.                                      ELXPMCBM
01675                                                                   ELXPMCBM
01676      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01677      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01678         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01679         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01680            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01681            MOVE WS-SAVE-SUB TO WS-LFM-DAY-SUB                     ELXPMCBM
01682            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01683                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
01684                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01685                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
01686                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01687               SET LFMD-CALL TO TRUE                               ELXPMCBM
01688            ELSE                                                   ELXPMCBM
01689               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCBM
01690                                                          OR '6'   ELXPMCBM
01691                  SET LFMD-CALL TO TRUE                            ELXPMCBM
01692               ELSE                                                ELXPMCBM
01693                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
01694                  PERFORM 9600-DETERMINE-VALUE-LONG                ELXPMCBM
01695                  MOVE WS-VALUE-AMT-LONG TO                        ELXPMCBM
01696                                 PMCI-DAYS-LIFETIME-MENTAL         ELXPMCBM
01697                  MOVE WS-BNF-QUAL TO                              ELXPMCBM
01698                                  PMCI-DAYS-LIFETIME-METAL-QUAL    ELXPMCBM
01699                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
01700                                  PMCI-LFTM-MNTL-DAYS-FROM-IND     ELXPMCBM
01701               END-IF                                              ELXPMCBM
01702            END-IF                                                 ELXPMCBM
01703         ELSE                                                      ELXPMCBM
01704            SET LFMD-NO-COVERAGE TO TRUE                           ELXPMCBM
01705         END-IF                                                    ELXPMCBM
01706      END-IF.                                                      ELXPMCBM
01707                                                                   ELXPMCBM
01708 ************************************************************      ELXPMCBM
01709 *                                                          *      ELXPMCBM
01710 * SEARCH FOR OTHER MENTAL DOLLAR MAXIMUMS                  *      ELXPMCBM
01711 *                                                          *      ELXPMCBM
01712 ************************************************************      ELXPMCBM
01713  6300-SRCH-OTHR-MNTL-DLR-MAX.                                     ELXPMCBM
01714                                                                   ELXPMCBM
01715      PERFORM 9151-RCMPT-VL-QLFR-CF-DLR                            ELXPMCBM
01716         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01717            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCBM
01718      PERFORM 6390-TST-OTHR-MNTL-DLR-MAX.                          ELXPMCBM
01719                                                                   ELXPMCBM
01720 ************************************************************      ELXPMCBM
01721 *                                                          *      ELXPMCBM
01722 * TEST AND SET OTHER MENTAL DOLLAR MAXIMUM                 *      ELXPMCBM
01723 *                                                          *      ELXPMCBM
01724 ************************************************************      ELXPMCBM
01725  6390-TST-OTHR-MNTL-DLR-MAX.                                      ELXPMCBM
01726                                                                   ELXPMCBM
01727      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01728      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01729         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01730         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01731            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01732            MOVE WS-SAVE-SUB TO WS-OTR-DLR-SUB                     ELXPMCBM
01733            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01734                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
01735                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01736                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
01737                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01738               SET MNDT-CALL TO TRUE                               ELXPMCBM
01739            ELSE                                                   ELXPMCBM
01740               PERFORM 9110-FND-BNFT-PRD-CF                        ELXPMCBM
01741               PERFORM 9650-DETERMINE-VALUE-SHORT                  ELXPMCBM
01742               MOVE WS-VALUE-AMT-SHORT TO                          ELXPMCBM
01743                                 PMCI-MENTAL-DOLLARS               ELXPMCBM
01744               MOVE WS-BNF-QUAL TO PMCI-MENTAL-DOLLARS-QUAL        ELXPMCBM
01745               MOVE WS-SAVE-LOB-IND TO                             ELXPMCBM
01746                               PMCI-LFTM-MNTL-FROM-IND             ELXPMCBM
01747            END-IF                                                 ELXPMCBM
01748         ELSE                                                      ELXPMCBM
01749            SET MNDT-NOT-APPLICABLE TO TRUE                        ELXPMCBM
01750         END-IF                                                    ELXPMCBM
01751      END-IF.                                                      ELXPMCBM
01752                                                                   ELXPMCBM
01753 ************************************************************      ELXPMCBM
01754 *                                                          *      ELXPMCBM
01755 * SEARCH FOR OTHER MENTAL DAY MAXIMUMS                     *      ELXPMCBM
01756 *                                                          *      ELXPMCBM
01757 ************************************************************      ELXPMCBM
01758  6400-SRCH-OTHR-MNTL-DAY-MAX.                                     ELXPMCBM
01759                                                                   ELXPMCBM
01760      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCBM
01761         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01762            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCBM
01763      PERFORM 6490-TST-OTHR-MNTL-DAY-MAX.                          ELXPMCBM
01764                                                                   ELXPMCBM
01765 ************************************************************      ELXPMCBM
01766 *                                                          *      ELXPMCBM
01767 * TEST AND SET OTHER MENTAL DAY MAXIMUM                    *      ELXPMCBM
01768 *                                                          *      ELXPMCBM
01769 ************************************************************      ELXPMCBM
01770  6490-TST-OTHR-MNTL-DAY-MAX.                                      ELXPMCBM
01771                                                                   ELXPMCBM
01772      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01773      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01774         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01775         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01776            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01777            MOVE WS-SAVE-SUB TO WS-OTR-DAY-SUB                     ELXPMCBM
01778            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01779                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
01780                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01781                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
01782                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01783               SET PMCI-MM-CALL TO TRUE                            ELXPMCBM
01784            ELSE                                                   ELXPMCBM
01785               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCBM
01786                                                          OR '6'   ELXPMCBM
01787                  SET PMCI-MM-CALL TO TRUE                         ELXPMCBM
01788               ELSE                                                ELXPMCBM
01789                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
01790                  PERFORM 9600-DETERMINE-VALUE-LONG                ELXPMCBM
01791                  MOVE WS-VALUE-AMT-LONG TO                        ELXPMCBM
01792                                 PMCI-MAX-MENTAL                   ELXPMCBM
01793                  MOVE WS-BNF-QUAL TO PMCI-MAX-MENTAL-QUALIFIER    ELXPMCBM
01794                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
01795                               PMCI-MAX-MNTL-FROM-IND              ELXPMCBM
01796               END-IF                                              ELXPMCBM
01797            END-IF                                                 ELXPMCBM
01798         ELSE                                                      ELXPMCBM
01799            SET PMCI-MM-NOT-APPLICABLE TO TRUE                     ELXPMCBM
01800         END-IF                                                    ELXPMCBM
01801      END-IF.                                                      ELXPMCBM
01802                                                                   ELXPMCBM
01803 ************************************************************      ELXPMCBM
01804 *                                                          *      ELXPMCBM
01805 *   RECOMPUTE BENEFIT PERIOD FACTOR FOR LIFETIME MENTAL    *      ELXPMCBM
01806 *                                                          *      ELXPMCBM
01807 ************************************************************      ELXPMCBM
01808  6911-RCMPT-BNFT-PRD-LFTM-MNTL.                                   ELXPMCBM
01809                                                                   ELXPMCBM
01810      MOVE ATBL-CF-LFTM (ATBL-IDX) TO                              ELXPMCBM
01811                          ATBL-CF-BNFT-PRD (ATBL-IDX).             ELXPMCBM
01812                                                                   ELXPMCBM
01813 ************************************************************      ELXPMCBM
01814 *                                                          *      ELXPMCBM
01815 *   RECOMPUTE BENEFIT PERIOD FACTOR FOR OTHER MENTAL       *      ELXPMCBM
01816 *                                                          *      ELXPMCBM
01817 ************************************************************      ELXPMCBM
01818  6912-RCMPT-BNFT-PRD-OTHR-MNTL.                                   ELXPMCBM
01819                                                                   ELXPMCBM
01820      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCBM
01821      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01822         MOVE CFTA-CF-OTHR-MNTL (CFTA-IDX) TO                      ELXPMCBM
01823                         ATBL-CF-BNFT-PRD (ATBL-IDX).              ELXPMCBM
01824                                                                   ELXPMCBM
01825 ************************************************************      ELXPMCBM
01826 *                                                          *      ELXPMCBM
01827 * SEARCH FOR DAY NIGHT PSYCH DAY MAXIMUMS                  *      ELXPMCBM
01828 *                                                          *      ELXPMCBM
01829 ************************************************************      ELXPMCBM
01830  7000-SRCH-DAY-NGHT-PSYCH-MAX.                                    ELXPMCBM
01831                                                                   ELXPMCBM
01832      PERFORM 7010-RCMPT-CF-DAY-NGHT-PSYCH                         ELXPMCBM
01833         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
01834            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCBM
01835               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCBM
01836      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01837         IF PMCI-INSTITUTIONAL                                     ELXPMCBM
01838            IF NAES-II-DPSY-YES                                    ELXPMCBM
01839               PERFORM 7100-SRCH-DAY-PSYCH-DAY-MAX                 ELXPMCBM
01840            ELSE                                                   ELXPMCBM
01841               SET DNPD-NOT-APPLICABLE TO TRUE                     ELXPMCBM
01842            END-IF                                                 ELXPMCBM
01843            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01844               IF NAES-II-NPSY-YES                                 ELXPMCBM
01845               PERFORM 7200-SRCH-NGHT-PSYCH-DAY-MAX                ELXPMCBM
01846            ELSE                                                   ELXPMCBM
01847               SET DNPN-NOT-APPLICABLE TO TRUE                     ELXPMCBM
01848               END-IF                                              ELXPMCBM
01849            END-IF                                                 ELXPMCBM
01850         ELSE                                                      ELXPMCBM
01851            IF NAES-PI-DPV-YES                                     ELXPMCBM
01852               PERFORM 7100-SRCH-DAY-PSYCH-DAY-MAX                 ELXPMCBM
01853            ELSE                                                   ELXPMCBM
01854               SET DNPD-NOT-APPLICABLE TO TRUE                     ELXPMCBM
01855            END-IF                                                 ELXPMCBM
01856            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01857               IF NAES-PI-NPV-YES                                  ELXPMCBM
01858                  PERFORM 7200-SRCH-NGHT-PSYCH-DAY-MAX             ELXPMCBM
01859            ELSE                                                   ELXPMCBM
01860                  SET DNPN-NOT-APPLICABLE TO TRUE                  ELXPMCBM
01861               END-IF                                              ELXPMCBM
01862            END-IF                                                 ELXPMCBM
01863         END-IF.                                                   ELXPMCBM
01864                                                                   ELXPMCBM
01865 ************************************************************      ELXPMCBM
01866 *                                                          *      ELXPMCBM
01867 * RECOMPUTE CONFIDENCE FACTORS FOR DAY/NIGHT PSYCH         *      ELXPMCBM
01868 *                                                          *      ELXPMCBM
01869 ************************************************************      ELXPMCBM
01870  7010-RCMPT-CF-DAY-NGHT-PSYCH.                                    ELXPMCBM
01871                                                                   ELXPMCBM
01872      PERFORM 9010-RCMPT-BNFT-PRD-PSYCH.                           ELXPMCBM
01873      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01874         PERFORM 9020-RCMPT-CNDTN-BT-MNTL                          ELXPMCBM
01875         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01876            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBM
01877            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01878               MOVE CFT5-CF-INTD-PSYCH (CFT5-IDX) TO               ELXPMCBM
01879                     ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)          ELXPMCBM
01880               IF PMCI-BC-SUCCESSFUL                               ELXPMCBM
01881                  PERFORM 9152-RCMPT-VL-QLFR-CF-DAY.               ELXPMCBM
01882                                                                   ELXPMCBM
01883 ************************************************************      ELXPMCBM
01884 *                                                          *      ELXPMCBM
01885 * SEARCH FOR DAY PSYCH DAY MAXIMUM                         *      ELXPMCBM
01886 *                                                          *      ELXPMCBM
01887 ************************************************************      ELXPMCBM
01888  7100-SRCH-DAY-PSYCH-DAY-MAX.                                     ELXPMCBM
01889                                                                   ELXPMCBM
01890      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBM
01891         SET DNPD-NOT-APPLICABLE TO TRUE                           ELXPMCBM
01892      ELSE                                                         ELXPMCBM
01893         PERFORM 7101-RCMPT-BNFT-PRVSN-DAY-PSY                     ELXPMCBM
01894         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01895            PERFORM 9120-RCMPT-BNFT-PRVSN-CF                       ELXPMCBM
01896              VARYING ATBL-IDX FROM 1 BY 1                         ELXPMCBM
01897                UNTIL ATBL-IDX > ATBL-MAX-IDX                      ELXPMCBM
01898            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01899               MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD      ELXPMCBM
01900               PERFORM 7190-TST-DAY-PSYCH-DAY-MAX                  ELXPMCBM
01901            END-IF                                                 ELXPMCBM
01902         END-IF                                                    ELXPMCBM
01903      END-IF.                                                      ELXPMCBM
01904                                                                   ELXPMCBM
01905 ************************************************************      ELXPMCBM
01906 *                                                          *      ELXPMCBM
01907 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACTORS-DAY PSYCH *      ELXPMCBM
01908 *                                                          *      ELXPMCBM
01909 ************************************************************      ELXPMCBM
01910  7101-RCMPT-BNFT-PRVSN-DAY-PSY.                                   ELXPMCBM
01911                                                                   ELXPMCBM
01912      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DPSY-POINTER.       ELXPMCBM
01913      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
01914      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
01915                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
01916      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBM
01917      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
01918         MOVE +4609 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
01919         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
01920                                                                   ELXPMCBM
01921 ************************************************************      ELXPMCBM
01922 *                                                          *      ELXPMCBM
01923 * TEST AND SET DAY PSYCH DAY MAXIMUM                       *      ELXPMCBM
01924 *                                                          *      ELXPMCBM
01925 ************************************************************      ELXPMCBM
01926  7190-TST-DAY-PSYCH-DAY-MAX.                                      ELXPMCBM
01927                                                                   ELXPMCBM
01928      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
01929      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
01930         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
01931         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
01932            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
01933            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
01934                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
01935                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
01936                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
01937                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
01938               SET DNPD-CALL TO TRUE                               ELXPMCBM
01939            ELSE                                                   ELXPMCBM
01940               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCBM
01941                                                          OR '6'   ELXPMCBM
01942                  SET DNPD-CALL TO TRUE                            ELXPMCBM
01943               ELSE                                                ELXPMCBM
01944                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
01945                  PERFORM 9600-DETERMINE-VALUE-LONG                ELXPMCBM
01946                  MOVE WS-VALUE-AMT-LONG TO                        ELXPMCBM
01947                                 PMCI-DN-DAY-PSYCH                 ELXPMCBM
01948                  MOVE WS-BNF-QUAL TO PMCI-DN-DAY-PSYCH-QUALIFIER  ELXPMCBM
01949                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
01950                               PMCI-DAY-PSYCH-FROM-IND             ELXPMCBM
01951               END-IF                                              ELXPMCBM
01952            END-IF                                                 ELXPMCBM
01953         ELSE                                                      ELXPMCBM
01954            SET DNPD-NOT-APPLICABLE TO TRUE                        ELXPMCBM
01955         END-IF                                                    ELXPMCBM
01956      END-IF.                                                      ELXPMCBM
01957                                                                   ELXPMCBM
01958 ************************************************************      ELXPMCBM
01959 *                                                          *      ELXPMCBM
01960 * SEARCH FOR NIGHT PSYCH DAY MAXIMUM                       *      ELXPMCBM
01961 *                                                          *      ELXPMCBM
01962 ************************************************************      ELXPMCBM
01963  7200-SRCH-NGHT-PSYCH-DAY-MAX.                                    ELXPMCBM
01964                                                                   ELXPMCBM
01965      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBM
01966         SET DNPN-NOT-APPLICABLE TO TRUE                           ELXPMCBM
01967      ELSE                                                         ELXPMCBM
01968         PERFORM 7201-RCMPT-BNFT-PRVSN-NGHT-PSY                    ELXPMCBM
01969         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
01970            PERFORM 9120-RCMPT-BNFT-PRVSN-CF                       ELXPMCBM
01971              VARYING ATBL-IDX FROM 1 BY 1                         ELXPMCBM
01972                UNTIL ATBL-IDX > ATBL-MAX-IDX                      ELXPMCBM
01973            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
01974               MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD      ELXPMCBM
01975               PERFORM 7290-TST-NGHT-PSYCH-DAY-MAX                 ELXPMCBM
01976            END-IF                                                 ELXPMCBM
01977         END-IF                                                    ELXPMCBM
01978      END-IF.                                                      ELXPMCBM
01979                                                                   ELXPMCBM
01980 ************************************************************      ELXPMCBM
01981 *                                                          *      ELXPMCBM
01982 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACTORS-NGHT PSY  *      ELXPMCBM
01983 *                                                          *      ELXPMCBM
01984 ************************************************************      ELXPMCBM
01985  7201-RCMPT-BNFT-PRVSN-NGHT-PSY.                                  ELXPMCBM
01986                                                                   ELXPMCBM
01987      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-NPSY-POINTER.       ELXPMCBM
01988      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
01989      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
01990                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
01991      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBM
01992      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
01993         MOVE +4610 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
01994         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
01995                                                                   ELXPMCBM
01996 ************************************************************      ELXPMCBM
01997 *                                                          *      ELXPMCBM
01998 * TEST AND SET NIGHT PSYCH DAY MAXIMUM                     *      ELXPMCBM
01999 *                                                          *      ELXPMCBM
02000 ************************************************************      ELXPMCBM
02001  7290-TST-NGHT-PSYCH-DAY-MAX.                                     ELXPMCBM
02002                                                                   ELXPMCBM
02003      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
02004      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02005         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
02006         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
02007            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
02008            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
02009                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
02010                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
02011                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
02012                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
02013               SET DNPN-CALL TO TRUE                               ELXPMCBM
02014            ELSE                                                   ELXPMCBM
02015               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCBM
02016                                                          OR '6'   ELXPMCBM
02017                  SET DNPN-CALL TO TRUE                            ELXPMCBM
02018               ELSE                                                ELXPMCBM
02019                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
02020                  PERFORM 9600-DETERMINE-VALUE-LONG                ELXPMCBM
02021                  MOVE WS-VALUE-AMT-LONG TO                        ELXPMCBM
02022                                 PMCI-DN-NIGHT-PSYCH               ELXPMCBM
02023                  MOVE WS-BNF-QUAL TO                              ELXPMCBM
02024                               PMCI-DN-NIGHT-PSYCH-QUALIFIER       ELXPMCBM
02025                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
02026                               PMCI-NGHT-PSYCH-FROM-IND            ELXPMCBM
02027            END-IF                                                 ELXPMCBM
02028         ELSE                                                      ELXPMCBM
02029            SET DNPN-NOT-APPLICABLE TO TRUE                        ELXPMCBM
02030         END-IF                                                    ELXPMCBM
02031      END-IF.                                                      ELXPMCBM
02032                                                                   ELXPMCBM
02033 ************************************************************      ELXPMCBM
02034 *                                                          *      ELXPMCBM
02035 *  SEARCH FOR SUBSTANCE ABUSE MAXIMUMS                     *      ELXPMCBM
02036 *                                                          *      ELXPMCBM
02037 ************************************************************      ELXPMCBM
02038  8000-SRCH-SB-ABS-MAX.                                            ELXPMCBM
02039                                                                   ELXPMCBM
02040      PERFORM 8010-RCMPT-CF-SB-ABS                                 ELXPMCBM
02041         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCBM
02042            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCBM
02043               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCBM
02044      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02045         IF SUB-ABUSE-ALC-YES                                      ELXPMCBM
02046            PERFORM 8100-SRCH-ALCHL-MAX                            ELXPMCBM
02047         ELSE                                                      ELXPMCBM
02048            SET PMCI-AL-NO-COVERAGE TO TRUE.                       ELXPMCBM
02049      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02050         IF SUB-ABUSE-DRG-YES                                      ELXPMCBM
02051            PERFORM 8200-SRCH-DRG-MAX                              ELXPMCBM
02052         ELSE                                                      ELXPMCBM
02053            SET PMCI-MD-NO-COVERAGE TO TRUE.                       ELXPMCBM
02054      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02055         PERFORM 8500-TST-CMBND-MDA-MAX.                           ELXPMCBM
02056                                                                   ELXPMCBM
02057 ************************************************************      ELXPMCBM
02058 *                                                          *      ELXPMCBM
02059 * RECOMPUTE CONFIDENCE FACTORS FOR SUBSTANCE ABUSE         *      ELXPMCBM
02060 *                                                          *      ELXPMCBM
02061 ************************************************************      ELXPMCBM
02062  8010-RCMPT-CF-SB-ABS.                                            ELXPMCBM
02063                                                                   ELXPMCBM
02064      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCBM
02065      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02066         MOVE CFTA-CF-SB-ABS (CFTA-IDX) TO                         ELXPMCBM
02067                         ATBL-CF-BNFT-PRD (ATBL-IDX)               ELXPMCBM
02068         PERFORM 9130-FND-INTRNL-DSCRPTR-CF                        ELXPMCBM
02069         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBM
02070            MOVE CFT5-CF-INTD-SB-ABS (CFT5-IDX) TO                 ELXPMCBM
02071                       ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)        ELXPMCBM
02072            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBM
02073               PERFORM 8040-RCMPT-VL-QLFR-CF-SB-ABS.               ELXPMCBM
02074                                                                   ELXPMCBM
02075 ************************************************************      ELXPMCBM
02076 *                                                          *      ELXPMCBM
02077 * RECOMPUTE VALUE QUALIFIER FACTOR FOR SUBSTANCE ABUSE     *      ELXPMCBM
02078 *                                                          *      ELXPMCBM
02079 ************************************************************      ELXPMCBM
02080  8040-RCMPT-VL-QLFR-CF-SB-ABS.                                    ELXPMCBM
02081                                                                   ELXPMCBM
02082      IF PMCI-INSTITUTIONAL                                        ELXPMCBM
02083         EVALUATE TRUE                                             ELXPMCBM
02084            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'             ELXPMCBM
02085                MOVE WS-CF-95 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02086            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2'             ELXPMCBM
02087                MOVE WS-CF-85 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02088            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '5'      ELXPMCBM
02089                                                         OR '6'    ELXPMCBM
02090                MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02091            WHEN OTHER                                             ELXPMCBM
02092                MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX). ELXPMCBM
02093      IF PMCI-PROFESSIONAL                                         ELXPMCBM
02094         EVALUATE TRUE                                             ELXPMCBM
02095            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2'             ELXPMCBM
02096                MOVE WS-CF-95 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02097            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'             ELXPMCBM
02098                MOVE WS-CF-85 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02099            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '5'      ELXPMCBM
02100                                                         OR '6'    ELXPMCBM
02101                MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCBM
02102            WHEN OTHER                                             ELXPMCBM
02103                MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX). ELXPMCBM
02104                                                                   ELXPMCBM
02105 ************************************************************      ELXPMCBM
02106 *                                                          *      ELXPMCBM
02107 * SEARCH FOR ALCOHOL MAXIMUM                               *      ELXPMCBM
02108 *                                                          *      ELXPMCBM
02109 ************************************************************      ELXPMCBM
02110  8100-SRCH-ALCHL-MAX.                                             ELXPMCBM
02111                                                                   ELXPMCBM
02112      PERFORM 8101-RCMPT-BNFT-PRVSN-ALCHL.                         ELXPMCBM
02113      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02114         MOVE CVG2-OV-THRSHLD-SBSTNC-ABS TO WS-TEST-THRESHOLD      ELXPMCBM
02115         PERFORM 8110-RCMPT-CF-ALCHL                               ELXPMCBM
02116            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBM
02117               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCBM
02118                  PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.             ELXPMCBM
02119      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02120         PERFORM 8190-TST-ALCHL-MAX.                               ELXPMCBM
02121                                                                   ELXPMCBM
02122 ************************************************************      ELXPMCBM
02123 *                                                          *      ELXPMCBM
02124 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACT FOR ALCOHOL  *      ELXPMCBM
02125 *                                                          *      ELXPMCBM
02126 ************************************************************      ELXPMCBM
02127  8101-RCMPT-BNFT-PRVSN-ALCHL.                                     ELXPMCBM
02128                                                                   ELXPMCBM
02129      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-ALAB-POINTER.       ELXPMCBM
02130      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
02131      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
02132                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
02133      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCBM
02134      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
02135         MOVE +4612 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
02136         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
02137                                                                   ELXPMCBM
02138 ************************************************************      ELXPMCBM
02139 *                                                          *      ELXPMCBM
02140 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTORS FOR ALCOHOL    *      ELXPMCBM
02141 *                                                          *      ELXPMCBM
02142 ************************************************************      ELXPMCBM
02143  8110-RCMPT-CF-ALCHL.                                             ELXPMCBM
02144                                                                   ELXPMCBM
02145      MOVE WS-CF-ZERO TO WS-NO-IBGR-CF.                            ELXPMCBM
02146      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCBM
02147      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02148         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBM
02149         PERFORM 8120-RCMPT-CNDTN-BT-ALCHL.                        ELXPMCBM
02150                                                                   ELXPMCBM
02151 ************************************************************      ELXPMCBM
02152 *                                                          *      ELXPMCBM
02153 * RECOMPUTE CONDITION BITS FACTOR FOR ALCOHOL CONDITIONS   *      ELXPMCBM
02154 *                                                          *      ELXPMCBM
02155 ************************************************************      ELXPMCBM
02156  8120-RCMPT-CNDTN-BT-ALCHL.                                       ELXPMCBM
02157                                                                   ELXPMCBM
02158      IF ATBL-COND-ALCOHOL-BIT (ATBL-IDX) = '1'                    ELXPMCBM
02159         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCBM
02160            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCBM
02161         ELSE                                                      ELXPMCBM
02162            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCBM
02163      ELSE                                                         ELXPMCBM
02164      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCBM
02165               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCBM
02166         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                   ELXPMCBM
02167            MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)  ELXPMCBM
02168         ELSE                                                      ELXPMCBM
02169            MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO               ELXPMCBM
02170                            ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)        ELXPMCBM
02171      ELSE                                                         ELXPMCBM
02172         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCBM
02173                                                                   ELXPMCBM
02174 ************************************************************      ELXPMCBM
02175 *                                                          *      ELXPMCBM
02176 * TEST AND SET ALCOHOL MAXIMUMS                            *      ELXPMCBM
02177 *                                                          *      ELXPMCBM
02178 ************************************************************      ELXPMCBM
02179  8190-TST-ALCHL-MAX.                                              ELXPMCBM
02180                                                                   ELXPMCBM
02181      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
02182      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02183         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
02184         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
02185            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
02186            MOVE WS-SAVE-SUB TO WS-ALC-SUB                         ELXPMCBM
02187            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
02188                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
02189                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
02190                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
02191                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
02192               SET PMCI-AL-CALL TO TRUE                            ELXPMCBM
02193            ELSE                                                   ELXPMCBM
02194               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'     ELXPMCBM
02195                  SET PMCI-AL-CALL TO TRUE                         ELXPMCBM
02196               ELSE                                                ELXPMCBM
02197                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
02198                  MOVE WS-BNF-QUAL TO PMCI-MAX-ALCOHOL-QUALIFIER   ELXPMCBM
02199                  MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO          ELXPMCBM
02200                            PMCI-MAX-ALCOHOL-UNIT                  ELXPMCBM
02201                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
02202                               PMCI-SBSTNCE-ABS-ALC-IND            ELXPMCBM
02203                  IF PMCI-AL-DOLLARS                               ELXPMCBM
02204                     PERFORM 9650-DETERMINE-VALUE-SHORT            ELXPMCBM
02205                     MOVE WS-VALUE-AMT-SHORT TO                    ELXPMCBM
02206                                      PMCI-MAX-ALCOHOL             ELXPMCBM
02207                  ELSE                                             ELXPMCBM
02208                     PERFORM 9600-DETERMINE-VALUE-LONG             ELXPMCBM
02209                     MOVE WS-VALUE-AMT-LONG TO                     ELXPMCBM
02210                                      PMCI-MAX-ALCOHOL             ELXPMCBM
02211                  END-IF                                           ELXPMCBM
02212               END-IF                                              ELXPMCBM
02213            END-IF                                                 ELXPMCBM
02214         ELSE                                                      ELXPMCBM
02215            SET PMCI-AL-NOT-APPLICABLE TO TRUE                     ELXPMCBM
02216         END-IF                                                    ELXPMCBM
02217      END-IF.                                                      ELXPMCBM
02218                                                                   ELXPMCBM
02219 ************************************************************      ELXPMCBM
02220 *                                                          *      ELXPMCBM
02221 * SEARCH FOR DRUG    MAXIMUM                               *      ELXPMCBM
02222 *                                                          *      ELXPMCBM
02223 ************************************************************      ELXPMCBM
02224  8200-SRCH-DRG-MAX.                                               ELXPMCBM
02225                                                                   ELXPMCBM
02226      PERFORM 8201-RCMPT-BNFT-PRVSN-DRG.                           ELXPMCBM
02227      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02228         MOVE CVG2-OV-THRSHLD-SBSTNC-ABS TO WS-TEST-THRESHOLD      ELXPMCBM
02229         PERFORM 8210-RCMPT-CF-DRG                                 ELXPMCBM
02230            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBM
02231               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCBM
02232                  PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.             ELXPMCBM
02233      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02234         PERFORM 8290-TST-DRG-MAX.                                 ELXPMCBM
02235                                                                   ELXPMCBM
02236 ************************************************************      ELXPMCBM
02237 *                                                          *      ELXPMCBM
02238 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACT FOR DRUG     *      ELXPMCBM
02239 *                                                          *      ELXPMCBM
02240 ************************************************************      ELXPMCBM
02241  8201-RCMPT-BNFT-PRVSN-DRG.                                       ELXPMCBM
02242                                                                   ELXPMCBM
02243      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DRAB-POINTER.       ELXPMCBM
02244      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBM
02245      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBM
02246                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBM
02247      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCBM
02248      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBM
02249         MOVE +4613 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBM
02250         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBM
02251                                                                   ELXPMCBM
02252 ************************************************************      ELXPMCBM
02253 *                                                          *      ELXPMCBM
02254 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTORS FOR DRUG       *      ELXPMCBM
02255 *                                                          *      ELXPMCBM
02256 ************************************************************      ELXPMCBM
02257  8210-RCMPT-CF-DRG.                                               ELXPMCBM
02258                                                                   ELXPMCBM
02259      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCBM
02260      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02261         PERFORM 8220-RCMPT-CNDTN-BT-DRG.                          ELXPMCBM
02262                                                                   ELXPMCBM
02263 ************************************************************      ELXPMCBM
02264 *                                                          *      ELXPMCBM
02265 * RECOMPUTE CONDITION BITS FACTOR FOR DRUG CONDITIONS      *      ELXPMCBM
02266 *                                                          *      ELXPMCBM
02267 ************************************************************      ELXPMCBM
02268  8220-RCMPT-CNDTN-BT-DRG.                                         ELXPMCBM
02269                                                                   ELXPMCBM
02270      IF ATBL-COND-DRUG-BIT (ATBL-IDX) = '1'                       ELXPMCBM
02271         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCBM
02272            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCBM
02273         ELSE                                                      ELXPMCBM
02274            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCBM
02275      ELSE                                                         ELXPMCBM
02276      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCBM
02277               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCBM
02278         MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO                  ELXPMCBM
02279                         ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)           ELXPMCBM
02280      ELSE                                                         ELXPMCBM
02281         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCBM
02282                                                                   ELXPMCBM
02283 ************************************************************      ELXPMCBM
02284 *                                                          *      ELXPMCBM
02285 * TEST AND SET DRUG MAXIMUMS                               *      ELXPMCBM
02286 *                                                          *      ELXPMCBM
02287 ************************************************************      ELXPMCBM
02288  8290-TST-DRG-MAX.                                                ELXPMCBM
02289                                                                   ELXPMCBM
02290      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBM
02291      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02292         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBM
02293         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBM
02294            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBM
02295            MOVE WS-SAVE-SUB TO WS-DRG-SUB                         ELXPMCBM
02296            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBM
02297                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBM
02298                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBM
02299                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBM
02300                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBM
02301               SET PMCI-MD-CALL TO TRUE                            ELXPMCBM
02302            ELSE                                                   ELXPMCBM
02303               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'     ELXPMCBM
02304                  SET PMCI-MD-CALL TO TRUE                         ELXPMCBM
02305               ELSE                                                ELXPMCBM
02306                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCBM
02307                  MOVE WS-BNF-QUAL TO PMCI-MAX-DRUG-QUALIFIER      ELXPMCBM
02308                  MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO          ELXPMCBM
02309                            PMCI-MAX-DRUG-UNIT                     ELXPMCBM
02310                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCBM
02311                               PMCI-SBSTNCE-ABS-DRG-IND            ELXPMCBM
02312                  IF PMCI-MD-DOLLARS                               ELXPMCBM
02313                     PERFORM 9650-DETERMINE-VALUE-SHORT            ELXPMCBM
02314                     MOVE WS-VALUE-AMT-SHORT TO                    ELXPMCBM
02315                                 PMCI-MAX-DRUG                     ELXPMCBM
02316                  ELSE                                             ELXPMCBM
02317                     PERFORM 9600-DETERMINE-VALUE-LONG             ELXPMCBM
02318                     MOVE WS-VALUE-AMT-LONG TO                     ELXPMCBM
02319                                 PMCI-MAX-DRUG                     ELXPMCBM
02320                  END-IF                                           ELXPMCBM
02321               END-IF                                              ELXPMCBM
02322            END-IF                                                 ELXPMCBM
02323         ELSE                                                      ELXPMCBM
02324            SET PMCI-MD-NOT-APPLICABLE TO TRUE                     ELXPMCBM
02325         END-IF                                                    ELXPMCBM
02326      END-IF.                                                      ELXPMCBM
02327                                                                   ELXPMCBM
02328 ************************************************************      ELXPMCBM
02329 *                                                          *      ELXPMCBM
02330 * TEST FOR COMBINED MENTAL DRUG AND ALCOHOL MAXIMUMS       *      ELXPMCBM
02331 *                                                          *      ELXPMCBM
02332 ************************************************************      ELXPMCBM
02333  8500-TST-CMBND-MDA-MAX.                                          ELXPMCBM
02334                                                                   ELXPMCBM
02335      IF WS-ALC-SUB = WS-DRG-SUB                                   ELXPMCBM
02336         IF WS-ALC-SUB = WS-LFM-DLR-SUB OR                         ELXPMCBM
02337                WS-ALC-SUB = WS-OTR-DLR-SUB OR                     ELXPMCBM
02338                WS-ALC-SUB = WS-LFM-DAY-SUB OR                     ELXPMCBM
02339                WS-ALC-SUB = WS-OTR-DAY-SUB                        ELXPMCBM
02340            SET PMCI-MAX-COMBO-AMD TO TRUE                         ELXPMCBM
02341            ELSE                                                   ELXPMCBM
02342            SET PMCI-MAX-COMBO-AD TO TRUE                          ELXPMCBM
02343      ELSE                                                         ELXPMCBM
02344      IF WS-ALC-SUB = WS-LFM-DLR-SUB OR                            ELXPMCBM
02345             WS-ALC-SUB = WS-OTR-DLR-SUB OR                        ELXPMCBM
02346             WS-ALC-SUB = WS-LFM-DAY-SUB OR                        ELXPMCBM
02347             WS-ALC-SUB = WS-OTR-DAY-SUB                           ELXPMCBM
02348         SET PMCI-MAX-COMBO-AM TO TRUE                             ELXPMCBM
02349      ELSE                                                         ELXPMCBM
02350      IF WS-DRG-SUB = WS-LFM-DLR-SUB OR                            ELXPMCBM
02351             WS-DRG-SUB = WS-OTR-DLR-SUB OR                        ELXPMCBM
02352             WS-DRG-SUB = WS-LFM-DAY-SUB OR                        ELXPMCBM
02353             WS-DRG-SUB = WS-OTR-DAY-SUB                           ELXPMCBM
02354         SET PMCI-MAX-COMBO-MD TO TRUE                             ELXPMCBM
02355      ELSE                                                         ELXPMCBM
02356      SET PMCI-MAX-COMBO-NONE TO TRUE.                             ELXPMCBM
02357                                                                   ELXPMCBM
02358 ************************************************************      ELXPMCBM
02359 *                                                          *      ELXPMCBM
02360 *  RECOMPUTE BENEFIT PERIOD FACTOR FOR PSYCH               *      ELXPMCBM
02361 *                                                          *      ELXPMCBM
02362 ************************************************************      ELXPMCBM
02363  9010-RCMPT-BNFT-PRD-PSYCH.                                       ELXPMCBM
02364                                                                   ELXPMCBM
02365      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCBM
02366      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBM
02367         MOVE CFTA-CF-PSYCH (CFTA-IDX) TO                          ELXPMCBM
02368                         ATBL-CF-BNFT-PRD (ATBL-IDX).              ELXPMCBM
02369                                                                   ELXPMCBM
02370 ************************************************************      ELXPMCBM
02371 *                                                          *      ELXPMCBM
02372 *  RECOMPUTE CONDITION BITS FACTOR FOR MENTAL              *      ELXPMCBM
02373 *                                                          *      ELXPMCBM
02374 ************************************************************      ELXPMCBM
02375  9020-RCMPT-CNDTN-BT-MNTL.                                        ELXPMCBM
02376                                                                   ELXPMCBM
02377      IF ATBL-COND-MENTAL-BIT (ATBL-IDX) = '1'                     ELXPMCBM
02378         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCBM
02379            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCBM
02380         ELSE                                                      ELXPMCBM
02381            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCBM
02382      ELSE                                                         ELXPMCBM
02383      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCBM
02384               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCBM
02385         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                   ELXPMCBM
02386            MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)  ELXPMCBM
02387         ELSE                                                      ELXPMCBM
02388            MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO               ELXPMCBM
02389                         ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)           ELXPMCBM
02390      ELSE                                                         ELXPMCBM
02391         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCBM
02392                                                                   ELXPMCBM
02393 ************************************************************      ELXPMCBM
02394 *                                                          *      ELXPMCBM
02395 * FIND BENEFIT PERIOD CONFIDENCE FACTOR TABLE ENTRY        *      ELXPMCBM
02396 *                                                          *      ELXPMCBM
02397 ************************************************************      ELXPMCBM
02398  9110-FND-BNFT-PRD-CF.                                            ELXPMCBM
02399                                                                   ELXPMCBM
02400      SET WS-BNFTPRD-NOT-FOUND TO TRUE.                            ELXPMCBM
02401      SET CFTA-MAX-IDX TO CFTA-NBR-ENTRS.                          ELXPMCBM
02402      PERFORM VARYING CFTA-IDX FROM 1 BY 1                         ELXPMCBM
02403         UNTIL CFTA-IDX > CFTA-MAX-IDX OR                          ELXPMCBM
02404           WS-BNFTPRD-FOUND                                        ELXPMCBM
02405         IF ATBL-BENEFIT-PERIOD (ATBL-IDX) =                       ELXPMCBM
02406                             CFTA-BNFT-PRD (CFTA-IDX)              ELXPMCBM
02407            SET WS-BNFTPRD-FOUND TO TRUE                           ELXPMCBM
02408            MOVE CFTA-PMCI-BNFT-PRD (CFTA-IDX) TO WS-BNF-QUAL      ELXPMCBM
02409            SET WS-SUB-WORK TO CFTA-IDX                            ELXPMCBM
02410            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCBM
02411            SET CFTA-IDX TO WS-SUB-WORK                            ELXPMCBM
02412         END-IF                                                    ELXPMCBM
02413      END-PERFORM.                                                 ELXPMCBM
02414      IF WS-BNFTPRD-NOT-FOUND                                      ELXPMCBM
02415         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBM
02416         MOVE +4603 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBM
02417                                                                   ELXPMCBM
02418 ************************************************************      ELXPMCBM
02419 *                                                          *      ELXPMCBM
02420 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBM
02421 *                                                          *      ELXPMCBM
02422 ************************************************************      ELXPMCBM
02423  9120-RCMPT-BNFT-PRVSN-CF.                                        ELXPMCBM
02424                                                                   ELXPMCBM
02425      IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                      ELXPMCBM
02426         MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)    ELXPMCBM
02427         SET WS-IBGR-FOUND TO TRUE                                 ELXPMCBM
02428      ELSE                                                         ELXPMCBM
02429         SET WS-IBGR-NOT-FOUND TO TRUE                             ELXPMCBM
02430         SET IBGR-MAX-IDX TO IBGR-TBL-CNT                          ELXPMCBM
02431         PERFORM VARYING IBGR-IDX FROM 1 BY 1                      ELXPMCBM
02432            UNTIL IBGR-IDX > IBGR-MAX-IDX OR                       ELXPMCBM
02433                WS-IBGR-FOUND                                      ELXPMCBM
02434         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) =                     ELXPMCBM
02435                          IBGR-SLOT-NUMBER (IBGR-IDX)              ELXPMCBM
02436            SET WS-IBGR-FOUND TO TRUE                              ELXPMCBM
02437            MOVE IBGR-CF-LIST-MTCH (IBGR-IDX) TO                   ELXPMCBM
02438                            ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)       ELXPMCBM
02439         END-IF                                                    ELXPMCBM
02440         END-PERFORM.                                              ELXPMCBM
02441      IF WS-IBGR-NOT-FOUND                                         ELXPMCBM
02442         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBM
02443         MOVE +4604 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBM
02444                                                                   ELXPMCBM
02445 ************************************************************      ELXPMCBM
02446 *                                                          *      ELXPMCBM
02447 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBM
02448 *                                                          *      ELXPMCBM
02449 ************************************************************      ELXPMCBM
02450  9130-FND-INTRNL-DSCRPTR-CF.                                      ELXPMCBM
02451                                                                   ELXPMCBM
02452      SET WS-INTRNLDSC-NOT-FOUND TO TRUE.                          ELXPMCBM
02453      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELXPMCBM
02454      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELXPMCBM
02455         UNTIL CFT5-IDX > CFT5-MAX-IDX OR                          ELXPMCBM
02456           WS-INTRNLDSC-FOUND                                      ELXPMCBM
02457         IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) =                  ELXPMCBM
02458                           CFT5-INTD (CFT5-IDX)                    ELXPMCBM
02459            SET WS-INTRNLDSC-FOUND TO TRUE                         ELXPMCBM
02460            SET WS-SUB-WORK TO CFT5-IDX                            ELXPMCBM
02461            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCBM
02462            SET CFT5-IDX TO WS-SUB-WORK                            ELXPMCBM
02463         END-IF                                                    ELXPMCBM
02464      END-PERFORM.                                                 ELXPMCBM
02465      IF WS-INTRNLDSC-NOT-FOUND                                    ELXPMCBM
02466         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBM
02467         MOVE +4605 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBM
02468                                                                   ELXPMCBM
02469 ************************************************************      ELXPMCBM
02470 *                                                          *      ELXPMCBM
02471 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DOLLARS         *      ELXPMCBM
02472 *                                                          *      ELXPMCBM
02473 ************************************************************      ELXPMCBM
02474  9151-RCMPT-VL-QLFR-CF-DLR.                                       ELXPMCBM
02475                                                                   ELXPMCBM
02476      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '5'                     ELXPMCBM
02477         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCBM
02478      ELSE                                                         ELXPMCBM
02479         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCBM
02480                                                                   ELXPMCBM
02481                                                                   ELXPMCBM
02482 ************************************************************      ELXPMCBM
02483 *                                                          *      ELXPMCBM
02484 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DAYS            *      ELXPMCBM
02485 *                                                          *      ELXPMCBM
02486 ************************************************************      ELXPMCBM
02487  9152-RCMPT-VL-QLFR-CF-DAY.                                       ELXPMCBM
02488                                                                   ELXPMCBM
02489      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'                     ELXPMCBM
02490         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCBM
02491      ELSE                                                         ELXPMCBM
02492      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4' OR '6'       ELXPMCBM
02493         MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)            ELXPMCBM
02494      ELSE                                                         ELXPMCBM
02495         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCBM
02496                                                                   ELXPMCBM
02497                                                                   ELXPMCBM
02498 ************************************************************      ELXPMCBM
02499 *                                                          *      ELXPMCBM
02500 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DAYS AND VISITS *      ELXPMCBM
02501 *                                                          *      ELXPMCBM
02502 ************************************************************      ELXPMCBM
02503  9153-RCMPT-VL-QLFR-CF-DAY-VST.                                   ELXPMCBM
02504                                                                   ELXPMCBM
02505      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '3'              ELXPMCBM
02506         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCBM
02507      ELSE                                                         ELXPMCBM
02508      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'              ELXPMCBM
02509         MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)            ELXPMCBM
02510      ELSE                                                         ELXPMCBM
02511         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCBM
02512                                                                   ELXPMCBM
02513                                                                   ELXPMCBM
02514 ************************************************************      ELXPMCBM
02515 *                                                          *      ELXPMCBM
02516 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTOR AND SCAN FOR    *      ELXPMCBM
02517 *    APPLICABLE ENTRY.                                     *      ELXPMCBM
02518 *                                                          *      ELXPMCBM
02519 ************************************************************      ELXPMCBM
02520  9200-RCMPT-SP-SCN-APLCBL-ENTRY.                                  ELXPMCBM
02521                                                                   ELXPMCBM
02522      CALL 'ELKSPCFF'  USING ATBL-ACCUMULATOR-TABLE.               ELXPMCBM
02523      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBM
02524      IF WS-SUCCESSFUL-CALL                                        ELXPMCBM
02525         MOVE ZERO TO WS-SAVE-SUB-BSC                              ELXPMCBM
02526         MOVE WS-CF-FALSE TO WS-TEST-CONF-BSC                      ELXPMCBM
02527         MOVE ZERO TO WS-APPL-ENTRS-BSC                            ELXPMCBM
02528         MOVE ZERO TO WS-SAVE-SUB-MM                               ELXPMCBM
02529         MOVE WS-CF-FALSE TO WS-TEST-CONF-MM                       ELXPMCBM
02530         MOVE ZERO TO WS-APPL-ENTRS-MM                             ELXPMCBM
02531         PERFORM 9220-RCMPT-CRNT-ACCM-TBL-CF                       ELXPMCBM
02532            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBM
02533               UNTIL ATBL-IDX > ATBL-MAX-IDX                       ELXPMCBM
02534      ELSE                                                         ELXPMCBM
02535         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBM
02536         MOVE +4606 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBM
02537                                                                   ELXPMCBM
02538                                                                   ELXPMCBM
02539 ************************************************************      ELXPMCBM
02540 *                                                          *      ELXPMCBM
02541 *  RECOMPUTE CURRENT ACCUMULATOR TABLE WORK CONF FACTOR    *      ELXPMCBM
02542 *                                                          *      ELXPMCBM
02543 ************************************************************      ELXPMCBM
02544  9220-RCMPT-CRNT-ACCM-TBL-CF.                                     ELXPMCBM
02545                                                                   ELXPMCBM
02546      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCBM
02547         IF PMCI-INSTITUTIONAL                                     ELXPMCBM
02548            IF PMCI-INPATIENT                                      ELXPMCBM
02549               PERFORM 9230-RCMPT-INST-INP                         ELXPMCBM
02550            ELSE                                                   ELXPMCBM
02551               PERFORM 9240-RCMPT-INST-OUT                         ELXPMCBM
02552         ELSE                                                      ELXPMCBM
02553            IF PMCI-INPATIENT                                      ELXPMCBM
02554               PERFORM 9250-RCMPT-PROF-INP                         ELXPMCBM
02555            ELSE                                                   ELXPMCBM
02556               PERFORM 9260-RCMPT-PROF-OUT.                        ELXPMCBM
02557      IF PMCI-BSC-CNTRCT-GRP EQUAL SPACE                           ELXPMCBM
02558         IF PMCI-INSTITUTIONAL                                     ELXPMCBM
02559            IF PMCI-INPATIENT                                      ELXPMCBM
02560               PERFORM 9235-RCMPT-INST-INP-MM                      ELXPMCBM
02561            ELSE                                                   ELXPMCBM
02562               PERFORM 9245-RCMPT-INST-OUT-MM                      ELXPMCBM
02563         ELSE                                                      ELXPMCBM
02564            IF PMCI-INPATIENT                                      ELXPMCBM
02565               PERFORM 9255-RCMPT-PROF-INP-MM                      ELXPMCBM
02566            ELSE                                                   ELXPMCBM
02567               PERFORM 9265-RCMPT-PROF-OUT-MM.                     ELXPMCBM
02568                                                                   ELXPMCBM
02569 ************************************************************      ELXPMCBM
02570 *                                                          *      ELXPMCBM
02571 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02572 *                                                          *      ELXPMCBM
02573 ************************************************************      ELXPMCBM
02574  9230-RCMPT-INST-INP.                                             ELXPMCBM
02575                                                                   ELXPMCBM
02576      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02577                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02578                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02579                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBM
02580                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBM
02581                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02582                             ATBL-CF-SP (ATBL-IDX).                ELXPMCBM
02583      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02584                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBM
02585         CONTINUE                                                  ELXPMCBM
02586      ELSE                                                         ELXPMCBM
02587         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02588                      WS-WT-BNFT-PRD                               ELXPMCBM
02589         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02590                      WS-WT-INDVDL                                 ELXPMCBM
02591         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBM
02592                      WS-WT-INST-BAS                               ELXPMCBM
02593         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBM
02594                      WS-WT-IP                                     ELXPMCBM
02595         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02596                      WS-WT-PLAN                                   ELXPMCBM
02597         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02598                      WS-WT-SP                                     ELXPMCBM
02599         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02600      END-IF.                                                      ELXPMCBM
02601                                                                   ELXPMCBM
02602      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02603         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBM
02604         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBM
02605            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBM
02606            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02607                                    WS-TEST-CONF-BSC.              ELXPMCBM
02608                                                                   ELXPMCBM
02609      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBM
02610         PERFORM 9235-RCMPT-INST-INP-MM.                           ELXPMCBM
02611                                                                   ELXPMCBM
02612 ************************************************************      ELXPMCBM
02613 *                                                          *      ELXPMCBM
02614 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02615 *  IF MAJOR MEDICAL CONTRACT                               *      ELXPMCBM
02616 ************************************************************      ELXPMCBM
02617  9235-RCMPT-INST-INP-MM.                                          ELXPMCBM
02618                                                                   ELXPMCBM
02619 * PROCESS MAJOR MEDICAL CALCULATIONS.                             ELXPMCBM
02620      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02621                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02622                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02623                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBM
02624                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBM
02625                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02626                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02627      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02628                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBM
02629         CONTINUE                                                  ELXPMCBM
02630      ELSE                                                         ELXPMCBM
02631         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02632                      WS-WT-BNFT-PRD                               ELXPMCBM
02633         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02634                      WS-WT-INDVDL                                 ELXPMCBM
02635         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBM
02636                      WS-WT-INST-SUP                               ELXPMCBM
02637         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBM
02638                      WS-WT-IP                                     ELXPMCBM
02639         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02640                      WS-WT-PLAN                                   ELXPMCBM
02641         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02642                      WS-WT-SP                                     ELXPMCBM
02643         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02644      END-IF.                                                      ELXPMCBM
02645                                                                   ELXPMCBM
02646      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02647         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBM
02648         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBM
02649            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBM
02650            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02651                                    WS-TEST-CONF-MM.               ELXPMCBM
02652                                                                   ELXPMCBM
02653 ************************************************************      ELXPMCBM
02654 *                                                          *      ELXPMCBM
02655 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02656 *                                                          *      ELXPMCBM
02657 ************************************************************      ELXPMCBM
02658  9240-RCMPT-INST-OUT.                                             ELXPMCBM
02659                                                                   ELXPMCBM
02660      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02661                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02662                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02663                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBM
02664                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBM
02665                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02666                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02667      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02668                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBM
02669         CONTINUE                                                  ELXPMCBM
02670         ELSE                                                      ELXPMCBM
02671         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02672                      WS-WT-BNFT-PRD                               ELXPMCBM
02673         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02674                      WS-WT-INDVDL                                 ELXPMCBM
02675         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBM
02676                      WS-WT-INST-BAS                               ELXPMCBM
02677         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBM
02678                      WS-WT-OP                                     ELXPMCBM
02679         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02680                       WS-WT-PLAN                                  ELXPMCBM
02681         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02682                      WS-WT-SP                                     ELXPMCBM
02683         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02684         END-IF.                                                   ELXPMCBM
02685                                                                   ELXPMCBM
02686      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02687         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBM
02688         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBM
02689            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBM
02690            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02691                                    WS-TEST-CONF-BSC.              ELXPMCBM
02692                                                                   ELXPMCBM
02693      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBM
02694         PERFORM 9245-RCMPT-INST-OUT-MM.                           ELXPMCBM
02695 ************************************************************      ELXPMCBM
02696 *                                                          *      ELXPMCBM
02697 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02698 *  MAJOR MEDICAL CONTRACTS ONLY.                           *      ELXPMCBM
02699 ************************************************************      ELXPMCBM
02700  9245-RCMPT-INST-OUT-MM.                                          ELXPMCBM
02701                                                                   ELXPMCBM
02702                                                                   ELXPMCBM
02703 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBM
02704      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02705                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02706                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02707                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBM
02708                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBM
02709                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02710                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02711      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02712                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBM
02713         CONTINUE                                                  ELXPMCBM
02714         ELSE                                                      ELXPMCBM
02715         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02716                      WS-WT-BNFT-PRD                               ELXPMCBM
02717         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02718                      WS-WT-INDVDL                                 ELXPMCBM
02719         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBM
02720                      WS-WT-INST-SUP                               ELXPMCBM
02721         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBM
02722                      WS-WT-OP                                     ELXPMCBM
02723         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02724                       WS-WT-PLAN                                  ELXPMCBM
02725         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02726                      WS-WT-SP                                     ELXPMCBM
02727         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02728      END-IF.                                                      ELXPMCBM
02729                                                                   ELXPMCBM
02730      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02731         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBM
02732         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBM
02733            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBM
02734            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02735                                    WS-TEST-CONF-MM.               ELXPMCBM
02736                                                                   ELXPMCBM
02737 ************************************************************      ELXPMCBM
02738 *                                                          *      ELXPMCBM
02739 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02740 *                                                          *      ELXPMCBM
02741 ************************************************************      ELXPMCBM
02742  9250-RCMPT-PROF-INP.                                             ELXPMCBM
02743                                                                   ELXPMCBM
02744      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02745                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02746                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02747                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBM
02748                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBM
02749                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02750                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02751      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02752                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBM
02753         CONTINUE                                                  ELXPMCBM
02754      ELSE                                                         ELXPMCBM
02755          COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *          ELXPMCBM
02756                      WS-WT-BNFT-PRD                               ELXPMCBM
02757          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBM
02758                      WS-WT-INDVDL                                 ELXPMCBM
02759          COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *          ELXPMCBM
02760                      WS-WT-PROF-BAS                               ELXPMCBM
02761          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBM
02762                      WS-WT-IP                                     ELXPMCBM
02763          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBM
02764                      WS-WT-PLAN                                   ELXPMCBM
02765          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBM
02766                      WS-WT-SP                                     ELXPMCBM
02767          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBM
02768      END-IF.                                                      ELXPMCBM
02769                                                                   ELXPMCBM
02770      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02771         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBM
02772         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBM
02773            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBM
02774            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02775                                    WS-TEST-CONF-BSC.              ELXPMCBM
02776                                                                   ELXPMCBM
02777      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBM
02778         PERFORM 9255-RCMPT-PROF-INP-MM.                           ELXPMCBM
02779                                                                   ELXPMCBM
02780 ************************************************************      ELXPMCBM
02781 *                                                          *      ELXPMCBM
02782 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02783 *   MAJOR MEDICAL CONTRACTS ONLY                           *      ELXPMCBM
02784 ************************************************************      ELXPMCBM
02785  9255-RCMPT-PROF-INP-MM.                                          ELXPMCBM
02786                                                                   ELXPMCBM
02787 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBM
02788                                                                   ELXPMCBM
02789      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02790                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02791                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02792                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBM
02793                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBM
02794                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02795                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02796      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02797                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBM
02798         CONTINUE                                                  ELXPMCBM
02799      ELSE                                                         ELXPMCBM
02800          COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *          ELXPMCBM
02801                      WS-WT-BNFT-PRD                               ELXPMCBM
02802          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBM
02803                      WS-WT-INDVDL                                 ELXPMCBM
02804          COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *          ELXPMCBM
02805                      WS-WT-PROF-SUP                               ELXPMCBM
02806          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBM
02807                      WS-WT-IP                                     ELXPMCBM
02808          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBM
02809                      WS-WT-PLAN                                   ELXPMCBM
02810          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBM
02811                      WS-WT-SP                                     ELXPMCBM
02812          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBM
02813      END-IF.                                                      ELXPMCBM
02814                                                                   ELXPMCBM
02815      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02816         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBM
02817         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBM
02818            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBM
02819            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02820                                    WS-TEST-CONF-MM.               ELXPMCBM
02821                                                                   ELXPMCBM
02822 ************************************************************      ELXPMCBM
02823 *                                                          *      ELXPMCBM
02824 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02825 *                                                          *      ELXPMCBM
02826 ************************************************************      ELXPMCBM
02827  9260-RCMPT-PROF-OUT.                                             ELXPMCBM
02828                                                                   ELXPMCBM
02829      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02830                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02831                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02832                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBM
02833                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBM
02834                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02835                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02836      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02837                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBM
02838         CONTINUE                                                  ELXPMCBM
02839      ELSE                                                         ELXPMCBM
02840         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02841                      WS-WT-BNFT-PRD                               ELXPMCBM
02842         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02843                      WS-WT-INDVDL                                 ELXPMCBM
02844         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCBM
02845                      WS-WT-PROF-BAS                               ELXPMCBM
02846         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBM
02847                      WS-WT-OP                                     ELXPMCBM
02848         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02849                      WS-WT-PLAN                                   ELXPMCBM
02850         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02851                      WS-WT-SP                                     ELXPMCBM
02852         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02853      END-IF.                                                      ELXPMCBM
02854                                                                   ELXPMCBM
02855      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02856         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBM
02857         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBM
02858            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBM
02859            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02860                                    WS-TEST-CONF-BSC.              ELXPMCBM
02861                                                                   ELXPMCBM
02862      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBM
02863         PERFORM 9265-RCMPT-PROF-OUT-MM.                           ELXPMCBM
02864                                                                   ELXPMCBM
02865 ************************************************************      ELXPMCBM
02866 *                                                          *      ELXPMCBM
02867 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBM
02868 *  MAJOR MEDICAL CONTRACTS ONLY                            *      ELXPMCBM
02869 ************************************************************      ELXPMCBM
02870  9265-RCMPT-PROF-OUT-MM.                                          ELXPMCBM
02871                                                                   ELXPMCBM
02872 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBM
02873                                                                   ELXPMCBM
02874      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02875                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBM
02876                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBM
02877                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBM
02878                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBM
02879                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBM
02880                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBM
02881      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBM
02882                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBM
02883         CONTINUE                                                  ELXPMCBM
02884      ELSE                                                         ELXPMCBM
02885         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBM
02886                      WS-WT-BNFT-PRD                               ELXPMCBM
02887         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBM
02888                      WS-WT-INDVDL                                 ELXPMCBM
02889         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCBM
02890                      WS-WT-PROF-SUP                               ELXPMCBM
02891         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBM
02892                      WS-WT-OP                                     ELXPMCBM
02893         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBM
02894                      WS-WT-PLAN                                   ELXPMCBM
02895         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBM
02896                      WS-WT-SP                                     ELXPMCBM
02897         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBM
02898      END-IF.                                                      ELXPMCBM
02899                                                                   ELXPMCBM
02900      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBM
02901         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBM
02902         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBM
02903            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBM
02904            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBM
02905                                    WS-TEST-CONF-MM.               ELXPMCBM
02906 ************************************************************      ELXPMCBM
02907 *                                                          *      ELXPMCBM
02908 *  COMBINE FACTORS                                         *      ELXPMCBM
02909 *                                                          *      ELXPMCBM
02910 ************************************************************      ELXPMCBM
02911  9300-COMBINE-FACTORS.                                            ELXPMCBM
02912                                                                   ELXPMCBM
02913      CALL 'ELKFLCMB'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBM
02914                            WS-CF-1                                ELXPMCBM
02915                            WS-CF-2                                ELXPMCBM
02916                            WS-CF-3                                ELXPMCBM
02917                            WS-CF-4                                ELXPMCBM
02918                            WS-CF-5                                ELXPMCBM
02919                            WS-CF-6.                               ELXPMCBM
02920                                                                   ELXPMCBM
02921 ************************************************************      ELXPMCBM
02922 *                                                          *      ELXPMCBM
02923 *  DETERMINE IF BASIC OR MAJOR MEDICAL BENEFIT APPLIES     *      ELXPMCBM
02924 *                                                          *      ELXPMCBM
02925 ************************************************************      ELXPMCBM
02926                                                                   ELXPMCBM
02927  9500-DETERMINE-BASIC-MM.                                         ELXPMCBM
02928                                                                   ELXPMCBM
02929      MOVE SPACES TO WS-SAVE-LOB-IND.                              ELXPMCBM
02930      IF WS-SAVE-SUB-BSC NOT EQUAL ZERO AND WS-SAVE-SUB-MM         ELXPMCBM
02931               NOT EQUAL ZERO                                      ELXPMCBM
02932         MOVE '+' TO WS-SAVE-LOB-IND                               ELXPMCBM
02933         MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                       ELXPMCBM
02934         MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS               ELXPMCBM
02935      ELSE                                                         ELXPMCBM
02936         IF WS-SAVE-SUB-MM NOT EQUAL ZERO                          ELXPMCBM
02937            MOVE '*' TO WS-SAVE-LOB-IND                            ELXPMCBM
02938            MOVE WS-SAVE-SUB-MM TO WS-SAVE-SUB                     ELXPMCBM
02939            MOVE WS-APPL-ENTRS-MM TO WS-NUM-APPL-ENTRS             ELXPMCBM
02940         ELSE                                                      ELXPMCBM
02941            MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                    ELXPMCBM
02942            MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS.           ELXPMCBM
02943 ************************************************************      ELXPMCBM
02944 *                                                          *      ELXPMCBM
02945 *  DETERMINE ATBL VALUE LIMIT USAGE FOR FULL AMOUNT        *      ELXPMCBM
02946 *                                                          *      ELXPMCBM
02947 ************************************************************      ELXPMCBM
02948                                                                   ELXPMCBM
02949  9600-DETERMINE-VALUE-LONG.                                       ELXPMCBM
02950                                                                   ELXPMCBM
02951      IF PMCI-PRV-CALL                                             ELXPMCBM
02952         IF WS-PRG-VAR-FOUND                                       ELXPMCBM
02953            MOVE 'C' TO WS-BNF-QUAL                                ELXPMCBM
02954         ELSE                                                      ELXPMCBM
02955            MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO              ELXPMCBM
02956                              WS-VALUE-AMT-LONG                    ELXPMCBM
02957      ELSE                                                         ELXPMCBM
02958         MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO                 ELXPMCBM
02959                              WS-VALUE-AMT-LONG.                   ELXPMCBM
02960 ************************************************************      ELXPMCBM
02961 *                                                          *      ELXPMCBM
02962 *  DETERMINE ATBL VALUE LIMIT USAGE TRUNCATED DECIMAL AMOUNT      ELXPMCBM
02963 *                                                          *      ELXPMCBM
02964 ************************************************************      ELXPMCBM
02965                                                                   ELXPMCBM
02966  9650-DETERMINE-VALUE-SHORT.                                      ELXPMCBM
02967                                                                   ELXPMCBM
02968      IF PMCI-PRV-CALL                                             ELXPMCBM
02969         IF WS-PRG-VAR-FOUND                                       ELXPMCBM
02970            MOVE 'C' TO WS-BNF-QUAL                                ELXPMCBM
02971         ELSE                                                      ELXPMCBM
02972            MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                    ELXPMCBM
02973                              WS-VALUE-AMT-SHORT                   ELXPMCBM
02974      ELSE                                                         ELXPMCBM
02975         MOVE ATBL-VALUE-LIMIT  (ATBL-IDX) TO                      ELXPMCBM
02976                              WS-VALUE-AMT-SHORT.                  ELXPMCBM
