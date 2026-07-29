00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCBC
00003  PROGRAM-ID.         ELXPMCBC                                        LV004
00004                                                                   ELXPMCBC
00005  AUTHOR.             BARBARA KEIB                                 ELXPMCBC
00006                                                                   ELXPMCBC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCBC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCBC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCBC
00010                      233 N. MICHIGAN AVE                          ELXPMCBC
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCBC
00012                                                                   ELXPMCBC
00013                                                                   ELXPMCBC
00014  DATE-WRITTEN.       14-OCT-1993.                                 ELXPMCBC
00015                                                                   ELXPMCBC
00016  DATE-COMPILED.                                                   ELXPMCBC
00017                                                                   ELXPMCBC
00018  SECURITY.           COPYRIGHT 1993,                              ELXPMCBC
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCBC
00020      SKIP3                                                        ELXPMCBC
00021  ENVIRONMENT DIVISION.                                            ELXPMCBC
00022                                                                   ELXPMCBC
00023  CONFIGURATION SECTION.                                           ELXPMCBC
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCBC
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCBC
00026      EJECT                                                        ELXPMCBC
00027 ******************************************************************ELXPMCBC
00028 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCBC
00029 *      EXTRACT CO-PAY DEDUCTIBLES FOR ELIGIBILITY SUMMARY        *ELXPMCBC
00030 *                                                                *ELXPMCBC
00031 *     ******THIS PROGRAM MUST BE BATCH COMPILED********          *ELXPMCBC
00032 ******************************************************************ELXPMCBC
00033 *                      MAINTENANCE HISTORY                       *ELXPMCBC
00034 *                                                                *ELXPMCBC
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCBC
00036 * ----- ----------- --- ----- ---------------------------------- *ELXPMCBC
00037 * 01.00 14-OCT-1993 BAK       CREATED- ISSR 13071 PHASE 2        *ELXPMCBC
00038 *       3/13/95  RGO  CPO PROJECT. UPDATED PARAGRAPH 0150- TO    *ELXPMCBC
00039 *                     HANDLE CPO.                                *ELXPMCBC
00040 *                                                                *ELXPMCBC
00041 * RGO 10/19/95   CHANGED 0100-EXTRACT TO NOT PROCESS WHEN THE    *ELXPMCBC
00042 *                IBGR TABULAR TABLE IS NULL                      *ELXPMCBC
00043 *                                                                *ELXPMCBC
00044 * RGO   4/04/96       CBL PROJECT. UPDATED PARAGRAPH 0150- TO    *ELXPMCBC
00045 *                     HANDLE COMMUNITY BLUE.                     *ELXPMCBC
00046 *                                                                *ELXPMCBC
00047 * AKK   2/29/00  ADDED ERSO B TO INST OP TO SEE IF CO PAYS WILL  *ELXPMCBC
00048 *                BE DISPLAYED.                                   *ELXPMCBC
00049 *                                                                *ELXPMCBC
00050 * AKK   3/14/00  ADDED TEST FIELDS TO SEE WHAT CF'S ARE          *ELXPMCBC
00051 *                                                                *ELXPMCBC
00052 * AKK   4/12/00  ADDED 5190-5190-TST-EAC-COPAY BACK INTO         *ELXPMCBC
00053 *                5100- PARAGRAH.                                 *ELXPMCBC
00054 *                                                                *ELXPMCBC
00055 * AKK   5/02/00  ADDED CODE TO FORCE OUT OFFICE VISIT CO PAYS    *ELXPMCBC
00056 *                BY CHECKING BEN PERIOD, INTERAL DESCRIPTOR AND  *ELXPMCBC
00057 *                POT.                                            *ELXPMCBC
00058 *                                                                *ELXPMCBC
00059 * AKK   5/03/00  ADDED CODE TO FORCE OUT EMERGENCY    CO PAYS    *ELXPMCBC
00060 *                BY CHECKING BEN PERIOD, INTERAL DESCRIPTOR AND  *ELXPMCBC
00061 *                POT.                                            *ELXPMCBC
00062 *                                                                *ELXPMCBC
00063 *  JP   8/15/00  USES ACP ACCUMULATOR TABLE (IF NOT NULL) INSTEAD*ELXPMCBC
00064 *                OF ADL TO PROCESS; ADDED BENEFIT PERIOD & PLACE *ELXPMCBC
00065 *                OF TREATMENT INDICATORS TO FORCE ACP COPAYS.    *ELXPMCBC
00066 *                                                                *ELXPMCBC
00067 *  JP   9/28/00  LOADS ACCUM CDE TABLE WITH ACP DATA.            *ELXPMCBC
00068 *                                                                *ELXPMCBC
00069 *  AKK  3/12/03  REGEN'D WITH PMCCOMM COPYOOK ADDED TO ENDEV.    *ELXPMCBC
00070 *                                                                *ELXPMCBC
00071 * AKK   01-APR-2003 AKK CHANGES FOR ENDEVOR, DUE TO BA31 CHANGES  ELXPMCBC
00072 *                                                                *ELXPMCBC
00073 *                                                                *ELXPMCBC
00074 * 05.00 01-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCBC
00075 *                                                                *ELXPMCBC
00076 * 05.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCBC
00077 *                                                                 ELXPMCBC
00078 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCBC
00079 *                                                                *ELXPMCBC
00080 ******************************************************************ELXPMCBC
00081                                                                   ELXPMCBC
00082      EJECT                                                        ELXPMCBC
00083  DATA DIVISION.                                                   ELXPMCBC
00084  WORKING-STORAGE SECTION.                                         ELXPMCBC
00085  01  FILLER                     PICTURE X(32)                     ELXPMCBC
00086           VALUE '****ELXPMCBC WORKING STORAGE****'.               ELXPMCBC
00087                                                                   ELXPMCBC
00088  01  WS-TEST-FIELDS.                                              ELXPMCBC
00089      05  WS-CF-WORK-ENTRY     PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00090      05  WS-CF-BNFT-PRD       PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00091      05  WS-CF-INDVDL         PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00092      05  WS-CF-INST-BAS       PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00093      05  WS-CF-OP             PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00094      05  WS-CF-PLAN           PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00095      05  WS-CF-SP             PIC S9V9(10) VALUE ZERO.            ELXPMCBC
00096                                                                   ELXPMCBC
00097  01  WS-RETURN-CODE               PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00098                                                                   ELXPMCBC
00099      88  WS-SUCCESSFUL-CALL               VALUE ZERO.             ELXPMCBC
00100      88  WS-UNIDENT-PARM                  VALUE +8.               ELXPMCBC
00101      88  WS-MISSING-PARM                  VALUE +12.              ELXPMCBC
00102      88  WS-INTERNAL-ERROR                VALUE +16.              ELXPMCBC
00103                                                                   ELXPMCBC
00104  01  WS-SAVE-SUBSCRIPTS.                                          ELXPMCBC
00105                                                                   ELXPMCBC
00106      05  WS-SAVE-SUB              PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00107      05  WS-SAVE-SUB-BSC          PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00108      05  WS-SAVE-SUB-MM           PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00109      05  WS-SUB-WORK              PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00110                                                                   ELXPMCBC
00111                                                                   ELXPMCBC
00112  01  WS-NUM-APPL-ENTRS            PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00113  01  WS-APPL-ENTRS-BSC            PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00114  01  WS-APPL-ENTRS-MM             PIC S9(04) COMP VALUE ZERO.     ELXPMCBC
00115  01  WS-COPAY-VALUE               PIC S9(05) COMP-3 VALUE ZERO.   ELXPMCBC
00116  01  WS-BNF-QUAL                  PIC X(01) VALUE SPACES.         ELXPMCBC
00117  01  WS-BNF-PERD                  PIC X(02) VALUE SPACES.         ELXPMCBC
00118  01  WS-SAVE-LOB-IND              PIC X(01) VALUE SPACES.         ELXPMCBC
00119                                                                   ELXPMCBC
00120                                                                   ELXPMCBC
00121  01  WS-SWITCHES.                                                 ELXPMCBC
00122                                                                   ELXPMCBC
00123    05  WS-PRG-VARIATION-CONTROL   PIC X(01) VALUE 'N'.            ELXPMCBC
00124      88  WS-PRG-VAR-FOUND                 VALUE 'Y'.              ELXPMCBC
00125      88  WS-PRG-VAR-NOT-FOUND             VALUE 'N'.              ELXPMCBC
00126                                                                   ELXPMCBC
00127    05  WS-PROCESS-OVS-SWITCH      PIC X(01) VALUE 'N'.            ELXPMCBC
00128      88  PROCESSING-OVS                   VALUE 'Y'.              ELXPMCBC
00129      88  NOT-PROCESSING-OVS               VALUE 'N'.              ELXPMCBC
00130                                                                   ELXPMCBC
00131    05  WS-PROCESS-EMER-SWITCH     PIC X(01) VALUE 'N'.            ELXPMCBC
00132      88  PROCESSING-EMER                  VALUE 'Y'.              ELXPMCBC
00133      88  NOT-PROCESSING-EMER              VALUE 'N'.              ELXPMCBC
00134                                                                   ELXPMCBC
00135    05  WS-IBGR-SLOT-CONTROL       PIC X(01) VALUE 'N'.            ELXPMCBC
00136      88  WS-IBGR-FOUND                    VALUE 'Y'.              ELXPMCBC
00137      88  WS-IBGR-NOT-FOUND                VALUE 'N'.              ELXPMCBC
00138                                                                   ELXPMCBC
00139    05  WS-BNFTPRD-CONTROL         PIC X(01) VALUE 'N'.            ELXPMCBC
00140      88  WS-BNFTPRD-FOUND                 VALUE 'Y'.              ELXPMCBC
00141      88  WS-BNFTPRD-NOT-FOUND             VALUE 'N'.              ELXPMCBC
00142                                                                   ELXPMCBC
00143    05  WS-INTRNLDSC-CONTORL       PIC X(01) VALUE 'N'.            ELXPMCBC
00144      88  WS-INTRNLDSC-FOUND               VALUE 'Y'.              ELXPMCBC
00145      88  WS-INTRNLDSC-NOT-FOUND           VALUE 'N'.              ELXPMCBC
00146                                                                   ELXPMCBC
00147    05  WS-VALUE-INDICATOR         PIC X(01)  VALUE ' '.           ELXPMCBC
00148      88  WS-VALUE-CALL                    VALUE 'C'.              ELXPMCBC
00149      88  WS-VALUE-FOUND                   VALUE '2'.              ELXPMCBC
00150                                                                   ELXPMCBC
00151    05  WS-MCNP-PENALTY-IND        PIC X(01)  VALUE 'N'.           ELXPMCBC
00152      88  WS-MCNP-PEN-FOUND                VALUE 'Y'.              ELXPMCBC
00153      88  WS-MCNP-PEN-NOT-FOUND            VALUE 'N'.              ELXPMCBC
00154                                                                   ELXPMCBC
00155    05  WS-MCNP-INCENT-IND         PIC X(01)  VALUE 'N'.           ELXPMCBC
00156      88  WS-MCNP-INC-FOUND                VALUE 'Y'.              ELXPMCBC
00157      88  WS-MCNP-INC-NOT-FOUND            VALUE 'N'.              ELXPMCBC
00158                                                                   ELXPMCBC
00159    05  WS-PPO-PENALTY-IND        PIC X(01)  VALUE 'N'.            ELXPMCBC
00160      88  WS-PPO-PEN-FOUND                VALUE 'Y'.               ELXPMCBC
00161      88  WS-PPO-PEN-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00162                                                                   ELXPMCBC
00163    05  WS-PPO-INCENT-IND         PIC X(01)  VALUE 'N'.            ELXPMCBC
00164      88  WS-PPO-INC-FOUND                VALUE 'Y'.               ELXPMCBC
00165      88  WS-PPO-INC-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00166                                                                   ELXPMCBC
00167    05  WS-RPO-PENALTY-IND        PIC X(01)  VALUE 'N'.            ELXPMCBC
00168      88  WS-RPO-PEN-FOUND                VALUE 'Y'.               ELXPMCBC
00169      88  WS-RPO-PEN-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00170                                                                   ELXPMCBC
00171    05  WS-RPO-INCENT-IND         PIC X(01)  VALUE 'N'.            ELXPMCBC
00172      88  WS-RPO-INC-FOUND                VALUE 'Y'.               ELXPMCBC
00173      88  WS-RPO-INC-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00174                                                                   ELXPMCBC
00175    05  WS-BAE-PENALTY-IND        PIC X(01)  VALUE 'N'.            ELXPMCBC
00176      88  WS-BAE-PEN-FOUND                VALUE 'Y'.               ELXPMCBC
00177      88  WS-BAE-PEN-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00178                                                                   ELXPMCBC
00179    05  WS-BAE-INCENT-IND         PIC X(01)  VALUE 'N'.            ELXPMCBC
00180      88  WS-BAE-INC-FOUND                VALUE 'Y'.               ELXPMCBC
00181      88  WS-BAE-INC-NOT-FOUND            VALUE 'N'.               ELXPMCBC
00182                                                                   ELXPMCBC
00183  01  WS-WEIGHTS.                                                  ELXPMCBC
00184      02  WS-WT-BNFT-PRD         COMP-1    VALUE 0.750000E+00.     ELXPMCBC
00185      02  WS-WT-INDVDL           COMP-1    VALUE 0.500000E+00.     ELXPMCBC
00186      02  WS-WT-INST-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00187      02  WS-WT-PROF-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00188      02  WS-WT-INST-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00189      02  WS-WT-PROF-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00190      02  WS-WT-IP               COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00191      02  WS-WT-OP               COMP-1    VALUE 0.100000E+00.     ELXPMCBC
00192      02  WS-WT-PLAN             COMP-1    VALUE 0.250000E+00.     ELXPMCBC
00193      02  WS-WT-SP               COMP-1    VALUE 0.500000E+00.     ELXPMCBC
00194                                                                   ELXPMCBC
00195  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCBC
00196      02  WS-CF-ZERO             COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00197      02  WS-CF-TRUE             COMP-1    VALUE +1.000000E+00.    ELXPMCBC
00198      02  WS-CF-FALSE            COMP-1    VALUE -1.000000E+00.    ELXPMCBC
00199      02  WS-CF-75               COMP-1    VALUE +0.750000E+00.    ELXPMCBC
00200      02  WS-CF-85               COMP-1    VALUE +0.850000E+00.    ELXPMCBC
00201      02  WS-CF-95               COMP-1    VALUE +0.950000E+00.    ELXPMCBC
00202      02  WS-TEST-CONF-FACT      COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00203      02  WS-TEST-CONF-BSC       COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00204      02  WS-TEST-CONF-MM        COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00205      02  WS-TEST-THRESHOLD      COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00206      02  WS-CF                  COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00207      02  WS-NO-IBGR-CF          COMP-1    VALUE +0.000000E+00.    ELXPMCBC
00208                                                                   ELXPMCBC
00209  01  WS-CONFIDENCE-WORK.                                          ELXPMCBC
00210      02  WS-CF-1                COMP-1.                           ELXPMCBC
00211      02  WS-CF-2                COMP-1.                           ELXPMCBC
00212      02  WS-CF-3                COMP-1.                           ELXPMCBC
00213      02  WS-CF-4                COMP-1.                           ELXPMCBC
00214      02  WS-CF-5                COMP-1.                           ELXPMCBC
00215      02  WS-CF-6                COMP-1.                           ELXPMCBC
00216                                                                   ELXPMCBC
00217 * INSTITUTIONAL OUTPATIENT                                        ELXPMCBC
00218                                                                   ELXPMCBC
00219  01  WS-INST-OP-EMERA.                                            ELXPMCBC
00220      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBC
00221      02  FILLER                 PIC X(06) VALUE 'ERSO B'.         ELXPMCBC
00222      02  FILLER                 PIC X(06) VALUE 'EAER B'.         ELXPMCBC
00223                                                                   ELXPMCBC
00224  01  WS-INST-OP-EMERM.                                            ELXPMCBC
00225      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBC
00226      02  FILLER                 PIC X(06) VALUE 'ERSO B'.         ELXPMCBC
00227      02  FILLER                 PIC X(06) VALUE 'EMER B'.         ELXPMCBC
00228                                                                   ELXPMCBC
00229  01  WS-INST-OP-EMERL.                                            ELXPMCBC
00230      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBC
00231      02  FILLER                 PIC X(06) VALUE 'EAER B'.         ELXPMCBC
00232      02  FILLER                 PIC X(06) VALUE 'EMER B'.         ELXPMCBC
00233                                                                   ELXPMCBC
00234 * PROFESSIONAL OUTPATIENT                                         ELXPMCBC
00235                                                                   ELXPMCBC
00236  01  WS-PROF-OP-EMERA.                                            ELXPMCBC
00237      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBC
00238      02  FILLER                 PIC X(06) VALUE 'EAC  E'.         ELXPMCBC
00239                                                                   ELXPMCBC
00240  01  WS-PROF-OP-EMERM.                                            ELXPMCBC
00241      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBC
00242      02  FILLER                 PIC X(06) VALUE 'EMC  E'.         ELXPMCBC
00243                                                                   ELXPMCBC
00244  01  WS-PROF-OP-EMERL.                                            ELXPMCBC
00245      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBC
00246      02  FILLER                 PIC X(06) VALUE 'EAC  E'.         ELXPMCBC
00247      02  FILLER                 PIC X(06) VALUE 'EMC  E'.         ELXPMCBC
00248                                                                   ELXPMCBC
00249  01  WS-PROF-OP-OVSTS.                                            ELXPMCBC
00250      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBC
00251      02  FILLER                 PIC X(06) VALUE 'OVIS E'.         ELXPMCBC
00252                                                                   ELXPMCBC
00253  01  WS-POINTERS.                                                 ELXPMCBC
00254      02  WS-EMERA-POINTER        POINTER.                         ELXPMCBC
00255      02  WS-EMERM-POINTER        POINTER.                         ELXPMCBC
00256      02  WS-EMERL-POINTER        POINTER.                         ELXPMCBC
00257      02  WS-OVST-POINTER         POINTER.                         ELXPMCBC
00258                                                                   ELXPMCBC
00259  01  WS-SWITCHES.                                                 ELXPMCBC
00260      05                         PIC X(01).                        ELXPMCBC
00261         88  SW-TRMNL-ERR                  VALUE 'Y'.              ELXPMCBC
00262         88  SW-NO-TRMNL-ERR               VALUE 'N'.              ELXPMCBC
00263                                                                   ELXPMCBC
00264  COPY ELSCFTB5.                                                   ELXPMCBC
00265  COPY ELSCFTBA.                                                   ELXPMCBC
00266  COPY ELSCVG2C.                                                   ELXPMCBC
00267                                                                   ELXPMCBC
00268  01  FILLER                     PICTURE X(32)                     ELXPMCBC
00269           VALUE '*END ELXPMCBC WORKING STORAGE***'.               ELXPMCBC
00270      EJECT                                                        ELXPMCBC
00271  LINKAGE SECTION.                                                 ELXPMCBC
00272 *    EJECT                                                        ELXPMCBC
00273  COPY ELSCIA2C.                                                   ELXPMCBC
00274 *    EJECT                                                        ELXPMCBC
00275  COPY ELSCSACC.                                                   ELXPMCBC
00276 *    EJECT                                                        ELXPMCBC
00277  COPY ELSATBLC.                                                   ELXPMCBC
00278 *    EJECT                                                        ELXPMCBC
00279  COPY ELSPMCID.                                                   ELXPMCBC
00280 *    EJECT                                                        ELXPMCBC
00281  COPY ELSIBGRC.                                                   ELXPMCBC
00282 *    EJECT                                                        ELXPMCBC
00283  01  PMCI-COMM-AREA.                                              ELXPMCBC
00284  COPY PMCCOMM.                                                    ELXPMCBC
00285 * ACCUM CDE TABLE                                                 ELXPMCBC
00286  COPY ELSACCDE.                                                   ELXPMCBC
00287 *    EJECT                                                        ELXPMCBC
00288  COPY ELSBPVLC.                                                   ELXPMCBC
00289  01  LS-MATCH-LIST               PIC X.                           ELXPMCBC
00290 *    EJECT                                                        ELXPMCBC
00291  PROCEDURE DIVISION USING PMCI-COMM-AREA                          ELXPMCBC
00292                           NAES-INTERMEDIATE-DATA                  ELXPMCBC
00293                           CSAC-ACCUMULATOR-TABLE                  ELXPMCBC
00294                           IBGR-INTERNAL-TABS-TABLE                ELXPMCBC
00295                           ACCDE-ATBL-ACCUMULATOR-TABLE.           ELXPMCBC
00296 ************************************************************      ELXPMCBC
00297 *                                                          *      ELXPMCBC
00298 *          MAINLINE ROUTINE                                *      ELXPMCBC
00299 *                                                          *      ELXPMCBC
00300 ************************************************************      ELXPMCBC
00301  0000-MAINLINE.                                                   ELXPMCBC
00302                                                                   ELXPMCBC
00303      IF ADDRESS OF PMCI-COMM-AREA = NULL                          ELXPMCBC
00304         NEXT SENTENCE                                             ELXPMCBC
00305      ELSE                                                         ELXPMCBC
00306         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCBC
00307         SET PMCI-BC-NO-ERROR TO TRUE                              ELXPMCBC
00308         SET SW-NO-TRMNL-ERR TO TRUE                               ELXPMCBC
00309         IF ADDRESS OF CSAC-ACCUMULATOR-TABLE = NULL               ELXPMCBC
00310            MOVE +4701 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBC
00311            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBC
00312         ELSE                                                      ELXPMCBC
00313            SET ADDRESS OF LS-MATCH-LIST TO NULL                   ELXPMCBC
00314            PERFORM 0100-EXTRCT-DED-VALS.                          ELXPMCBC
00315                                                                   ELXPMCBC
00316      GOBACK.                                                      ELXPMCBC
00317                                                                   ELXPMCBC
00318 ************************************************************      ELXPMCBC
00319 *                                                          *      ELXPMCBC
00320 *        EXTRACT BENEFIT DEDUCTIBLES                       *      ELXPMCBC
00321 *                                                          *      ELXPMCBC
00322 ************************************************************      ELXPMCBC
00323  0100-EXTRCT-DED-VALS.                                            ELXPMCBC
00324                                                                   ELXPMCBC
00325      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELXPMCBC
00326                      CSAC-ACP-GC-TBL-PTR.                         ELXPMCBC
00327                                                                   ELXPMCBC
00328      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULLS                 ELXPMCBC
00329        OR ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULLS             ELXPMCBC
00330        SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                   ELXPMCBC
00331                       CSAC-ADL-GC-TBL-PTR                         ELXPMCBC
00332      END-IF                                                       ELXPMCBC
00333                                                                   ELXPMCBC
00334      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULLS                 ELXPMCBC
00335        OR ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULLS             ELXPMCBC
00336                                                                   ELXPMCBC
00337         SET EMAC-CALL TO TRUE                                     ELXPMCBC
00338         SET EMMD-CALL TO TRUE                                     ELXPMCBC
00339         SET EMLF-CALL TO TRUE                                     ELXPMCBC
00340         SET OFVS-CALL           TO TRUE                           ELXPMCBC
00341      ELSE                                                         ELXPMCBC
00342         PERFORM 0120-IDNTFY-AVLBL-DED-VALS.                       ELXPMCBC
00343                                                                   ELXPMCBC
00344 ************************************************************      ELXPMCBC
00345 *                                                          *      ELXPMCBC
00346 *        IDENTIFY AVAILABLE BENEFIT DEDUCTIBLES            *      ELXPMCBC
00347 *                                                          *      ELXPMCBC
00348 ************************************************************      ELXPMCBC
00349  0120-IDNTFY-AVLBL-DED-VALS.                                      ELXPMCBC
00350                                                                   ELXPMCBC
00351      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCBC
00352      IF PMCI-PRV-CALL OR PMCI-PRV-NONE                            ELXPMCBC
00353         CONTINUE                                                  ELXPMCBC
00354      ELSE                                                         ELXPMCBC
00355         PERFORM 0130-VERIFY-COST-CONTAINMENT                      ELXPMCBC
00356             VARYING ATBL-IDX FROM 1 BY 1                          ELXPMCBC
00357                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBC
00358      SET WS-PRG-VAR-NOT-FOUND TO TRUE.                            ELXPMCBC
00359      PERFORM 0150-INTLZ-SP                                        ELXPMCBC
00360            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBC
00361                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBC
00362                                                                   ELXPMCBC
00363      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
00364         IF PMCI-INSTITUTIONAL AND PMCI-OUTPATIENT                 ELXPMCBC
00365               PERFORM 2000-IDNTFY-INST-OP-VALS                    ELXPMCBC
00366         ELSE                                                      ELXPMCBC
00367         IF PMCI-PROFESSIONAL AND PMCI-OUTPATIENT                  ELXPMCBC
00368            PERFORM 4000-IDNTFY-PROF-OP-VALS                       ELXPMCBC
00369         ELSE                                                      ELXPMCBC
00370         SET EMAC-NONE TO TRUE                                     ELXPMCBC
00371         SET EMMD-NONE TO TRUE                                     ELXPMCBC
00372         SET EMLF-NONE TO TRUE                                     ELXPMCBC
00373         SET OFVS-NOT-APPLICABLE TO TRUE.                          ELXPMCBC
00374                                                                   ELXPMCBC
00375 ************************************************************      ELXPMCBC
00376 *                                                          *      ELXPMCBC
00377 *        VERIFY COST CONTAINMENT PROGRAMS                  *      ELXPMCBC
00378 *                                                          *      ELXPMCBC
00379 ************************************************************      ELXPMCBC
00380  0130-VERIFY-COST-CONTAINMENT.                                    ELXPMCBC
00381                                                                   ELXPMCBC
00382      EVALUATE TRUE                                                ELXPMCBC
00383         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBC
00384            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBC
00385         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBC
00386            PERFORM 0132-TEST-PPO-CSTCNMT-OUT                      ELXPMCBC
00387         WHEN PMCI-PRV-BAE-IN                                      ELXPMCBC
00388            PERFORM 0133-TEST-BAE-CSTCNMT-IN                       ELXPMCBC
00389         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBC
00390            PERFORM 0133-TEST-BAE-CSTCNMT-OUT                      ELXPMCBC
00391         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBC
00392            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBC
00393         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBC
00394            PERFORM 0134-TEST-RPO-CSTCNMT-OUT                      ELXPMCBC
00395         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBC
00396            PERFORM 0134-TEST-RPO-CSTCNMT-IN                       ELXPMCBC
00397         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBC
00398            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBC
00399         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBC
00400            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBC
00401         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBC
00402            PERFORM 0136-TEST-MCNP-CSTCNMT-OUT                     ELXPMCBC
00403         WHEN OTHER                                                ELXPMCBC
00404            CONTINUE                                               ELXPMCBC
00405      END-EVALUATE.                                                ELXPMCBC
00406 ************************************************************      ELXPMCBC
00407 *                                                          *      ELXPMCBC
00408 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00409 *          FOR IN NETWORK                                  *      ELXPMCBC
00410 ************************************************************      ELXPMCBC
00411  0132-TEST-PPO-CSTCNMT-IN.                                        ELXPMCBC
00412                                                                   ELXPMCBC
00413      EVALUATE TRUE                                                ELXPMCBC
00414         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBC
00415             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBC
00416         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00417             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBC
00418         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00419             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBC
00420         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBC
00421             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBC
00422         WHEN OTHER                                                ELXPMCBC
00423             CONTINUE                                              ELXPMCBC
00424      END-EVALUATE.                                                ELXPMCBC
00425                                                                   ELXPMCBC
00426 ************************************************************      ELXPMCBC
00427 *                                                          *      ELXPMCBC
00428 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00429 *          FOR IN NETWORK                                  *      ELXPMCBC
00430 ************************************************************      ELXPMCBC
00431  0133-TEST-BAE-CSTCNMT-IN.                                        ELXPMCBC
00432                                                                   ELXPMCBC
00433      EVALUATE TRUE                                                ELXPMCBC
00434         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00435             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBC
00436         WHEN OTHER                                                ELXPMCBC
00437             CONTINUE                                              ELXPMCBC
00438      END-EVALUATE.                                                ELXPMCBC
00439                                                                   ELXPMCBC
00440 ************************************************************      ELXPMCBC
00441 *                                                          *      ELXPMCBC
00442 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00443 *          FOR OUT OF NETWORK                              *      ELXPMCBC
00444 ************************************************************      ELXPMCBC
00445  0132-TEST-PPO-CSTCNMT-OUT.                                       ELXPMCBC
00446                                                                   ELXPMCBC
00447      EVALUATE TRUE                                                ELXPMCBC
00448         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBC
00449             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBC
00450         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00451             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBC
00452         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00453             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBC
00454         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBC
00455             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBC
00456         WHEN OTHER                                                ELXPMCBC
00457             CONTINUE                                              ELXPMCBC
00458      END-EVALUATE.                                                ELXPMCBC
00459                                                                   ELXPMCBC
00460 ************************************************************      ELXPMCBC
00461 *                                                          *      ELXPMCBC
00462 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00463 *          FOR OUT OF NETWORK                              *      ELXPMCBC
00464 ************************************************************      ELXPMCBC
00465  0133-TEST-BAE-CSTCNMT-OUT.                                       ELXPMCBC
00466                                                                   ELXPMCBC
00467      EVALUATE TRUE                                                ELXPMCBC
00468         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00469             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBC
00470         WHEN OTHER                                                ELXPMCBC
00471             CONTINUE                                              ELXPMCBC
00472      END-EVALUATE.                                                ELXPMCBC
00473                                                                   ELXPMCBC
00474 ************************************************************      ELXPMCBC
00475 *                                                          *      ELXPMCBC
00476 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00477 *          FOR IN NETWORK                                  *      ELXPMCBC
00478 ************************************************************      ELXPMCBC
00479  0134-TEST-RPO-CSTCNMT-IN.                                        ELXPMCBC
00480                                                                   ELXPMCBC
00481      EVALUATE TRUE                                                ELXPMCBC
00482         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBC
00483             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBC
00484         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBC
00485             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBC
00486         WHEN OTHER                                                ELXPMCBC
00487             CONTINUE                                              ELXPMCBC
00488      END-EVALUATE.                                                ELXPMCBC
00489                                                                   ELXPMCBC
00490 ************************************************************      ELXPMCBC
00491 *                                                          *      ELXPMCBC
00492 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBC
00493 *          FOR OUT OF NETWORK                              *      ELXPMCBC
00494 ************************************************************      ELXPMCBC
00495  0134-TEST-RPO-CSTCNMT-OUT.                                       ELXPMCBC
00496                                                                   ELXPMCBC
00497      EVALUATE TRUE                                                ELXPMCBC
00498         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBC
00499             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBC
00500         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBC
00501             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBC
00502         WHEN OTHER                                                ELXPMCBC
00503             CONTINUE                                              ELXPMCBC
00504      END-EVALUATE.                                                ELXPMCBC
00505                                                                   ELXPMCBC
00506 ************************************************************      ELXPMCBC
00507 *                                                          *      ELXPMCBC
00508 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBC
00509 *          FOR REFERRAL OR NO REFERRAL REQUIRED            *      ELXPMCBC
00510 ************************************************************      ELXPMCBC
00511  0136-TEST-MCNP-CSTCNMT-IN.                                       ELXPMCBC
00512                                                                   ELXPMCBC
00513      EVALUATE TRUE                                                ELXPMCBC
00514         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBC
00515             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBC
00516         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBC
00517             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBC
00518         WHEN OTHER                                                ELXPMCBC
00519             CONTINUE                                              ELXPMCBC
00520      END-EVALUATE.                                                ELXPMCBC
00521                                                                   ELXPMCBC
00522 ************************************************************      ELXPMCBC
00523 *                                                          *      ELXPMCBC
00524 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBC
00525 *          FOR NON REFERRAL                                *      ELXPMCBC
00526 ************************************************************      ELXPMCBC
00527  0136-TEST-MCNP-CSTCNMT-OUT.                                      ELXPMCBC
00528                                                                   ELXPMCBC
00529      EVALUATE TRUE                                                ELXPMCBC
00530         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBC
00531             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBC
00532         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBC
00533             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBC
00534         WHEN OTHER                                                ELXPMCBC
00535             CONTINUE                                              ELXPMCBC
00536      END-EVALUATE.                                                ELXPMCBC
00537                                                                   ELXPMCBC
00538 ************************************************************      ELXPMCBC
00539 *                                                          *      ELXPMCBC
00540 *        INITIALIZE SPECIAL CASE FACTORS                   *      ELXPMCBC
00541 *                                                          *      ELXPMCBC
00542 * 3/13/95  RGO. CPO PROJECT.                               *      ELXPMCBC
00543 ************************************************************      ELXPMCBC
00544  0150-INTLZ-SP.                                                   ELXPMCBC
00545                                                                   ELXPMCBC
00546      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX) TO                          ELXPMCBC
00547                    ATBL-CF-SP-FCTRS (ATBL-IDX).                   ELXPMCBC
00548      EVALUATE TRUE                                                ELXPMCBC
00549         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBC
00550            PERFORM 0160-SCN-SP-CCP-IPPO                           ELXPMCBC
00551         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBC
00552            PERFORM 0161-SCN-SP-CCP-OPPO                           ELXPMCBC
00553         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBC
00554            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBC
00555         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBC
00556            PERFORM 0171-SCN-SP-CCP-ORPO                           ELXPMCBC
00557         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBC
00558            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBC
00559         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBC
00560            PERFORM 0172-SCN-SP-CCP-OBAE                           ELXPMCBC
00561         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBC
00562            PERFORM 0172-SCN-SP-CCP-IBAE                           ELXPMCBC
00563         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBC
00564            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBC
00565         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBC
00566            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBC
00567         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBC
00568            PERFORM 0181-SCN-SP-CCP-OMCNP                          ELXPMCBC
00569         WHEN PMCI-PRV-CALL                                        ELXPMCBC
00570            PERFORM 0190-SCN-SP-CCP-CALL                           ELXPMCBC
00571 *                                                                 ELXPMCBC
00572         WHEN PMCI-PRV-CPO-MET                                     ELXPMCBC
00573            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBC
00574               MOVE WS-CF-ZERO TO                                  ELXPMCBC
00575                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBC
00576            ELSE                                                   ELXPMCBC
00577               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K1'          ELXPMCBC
00578                  MOVE WS-CF-TRUE TO                               ELXPMCBC
00579                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00580               ELSE                                                ELXPMCBC
00581                  MOVE WS-CF-FALSE TO                              ELXPMCBC
00582                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00583               END-IF                                              ELXPMCBC
00584            END-IF                                                 ELXPMCBC
00585                                                                   ELXPMCBC
00586         WHEN PMCI-PRV-CPO-PPO-MET                                 ELXPMCBC
00587            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBC
00588               MOVE WS-CF-ZERO TO                                  ELXPMCBC
00589                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00590            ELSE                                                   ELXPMCBC
00591               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'KI'         ELXPMCBC
00592                     MOVE WS-CF-TRUE TO                            ELXPMCBC
00593                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00594               ELSE                                                ELXPMCBC
00595                  MOVE WS-CF-FALSE TO                              ELXPMCBC
00596                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00597               END-IF                                              ELXPMCBC
00598            END-IF                                                 ELXPMCBC
00599         WHEN PMCI-PRV-CPO-PPO-NOT-MET                             ELXPMCBC
00600            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBC
00601               MOVE WS-CF-ZERO TO                                  ELXPMCBC
00602                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00603            ELSE                                                   ELXPMCBC
00604               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K4'         ELXPMCBC
00605                     MOVE WS-CF-TRUE TO                            ELXPMCBC
00606                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00607               ELSE                                                ELXPMCBC
00608                  MOVE WS-CF-FALSE TO                              ELXPMCBC
00609                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00610               END-IF                                              ELXPMCBC
00611            END-IF                                                 ELXPMCBC
00612 *                                                                 ELXPMCBC
00613         WHEN PMCI-PRV-CBL-IN                                      ELXPMCBC
00614            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBC
00615               MOVE WS-CF-ZERO TO                                  ELXPMCBC
00616                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBC
00617            ELSE                                                   ELXPMCBC
00618               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J1'          ELXPMCBC
00619                  MOVE WS-CF-TRUE TO                               ELXPMCBC
00620                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00621               ELSE                                                ELXPMCBC
00622                  MOVE WS-CF-FALSE TO                              ELXPMCBC
00623                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00624               END-IF                                              ELXPMCBC
00625            END-IF                                                 ELXPMCBC
00626                                                                   ELXPMCBC
00627         WHEN PMCI-PRV-CBL-OUT                                     ELXPMCBC
00628            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBC
00629               MOVE WS-CF-ZERO TO                                  ELXPMCBC
00630                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00631            ELSE                                                   ELXPMCBC
00632               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J4'         ELXPMCBC
00633                     MOVE WS-CF-TRUE TO                            ELXPMCBC
00634                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00635               ELSE                                                ELXPMCBC
00636                  MOVE WS-CF-FALSE TO                              ELXPMCBC
00637                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBC
00638               END-IF                                              ELXPMCBC
00639            END-IF                                                 ELXPMCBC
00640 *                                                                 ELXPMCBC
00641         WHEN OTHER                                                ELXPMCBC
00642            CONTINUE                                               ELXPMCBC
00643      END-EVALUATE.                                                ELXPMCBC
00644                                                                   ELXPMCBC
00645 ************************************************************      ELXPMCBC
00646 *                                                          *      ELXPMCBC
00647 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR PPO   *      ELXPMCBC
00648 *                                                          *      ELXPMCBC
00649 ************************************************************      ELXPMCBC
00650  0160-SCN-SP-CCP-IPPO.                                            ELXPMCBC
00651                                                                   ELXPMCBC
00652      EVALUATE TRUE                                                ELXPMCBC
00653         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00654           IF WS-PPO-INC-FOUND                                     ELXPMCBC
00655             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00656           ELSE                                                    ELXPMCBC
00657             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00658           END-IF                                                  ELXPMCBC
00659         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBC
00660             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00661             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00662         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00663             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00664             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00665         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00666             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00667             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00668         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBC
00669             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00670             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00671         WHEN OTHER                                                ELXPMCBC
00672             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00673      END-EVALUATE.                                                ELXPMCBC
00674                                                                   ELXPMCBC
00675 ************************************************************      ELXPMCBC
00676 *                                                          *      ELXPMCBC
00677 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-PPO   *      ELXPMCBC
00678 *                                                          *      ELXPMCBC
00679 ************************************************************      ELXPMCBC
00680  0161-SCN-SP-CCP-OPPO.                                            ELXPMCBC
00681                                                                   ELXPMCBC
00682      EVALUATE TRUE                                                ELXPMCBC
00683         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00684           IF WS-PPO-PEN-FOUND                                     ELXPMCBC
00685             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00686           ELSE                                                    ELXPMCBC
00687             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00688           END-IF                                                  ELXPMCBC
00689         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBC
00690             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00691             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00692         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00693             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00694             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00695         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00696             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00697             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00698         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBC
00699             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00700             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00701         WHEN OTHER                                                ELXPMCBC
00702             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00703      END-EVALUATE.                                                ELXPMCBC
00704                                                                   ELXPMCBC
00705 ************************************************************      ELXPMCBC
00706 *                                                          *      ELXPMCBC
00707 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-BAE   *      ELXPMCBC
00708 *                                                          *      ELXPMCBC
00709 ************************************************************      ELXPMCBC
00710  0172-SCN-SP-CCP-OBAE.                                            ELXPMCBC
00711                                                                   ELXPMCBC
00712      EVALUATE TRUE                                                ELXPMCBC
00713         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00714           IF WS-BAE-PEN-FOUND                                     ELXPMCBC
00715             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00716           END-IF                                                  ELXPMCBC
00717         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00718             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00719             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00720         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00721             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00722             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00723         WHEN OTHER                                                ELXPMCBC
00724             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00725      END-EVALUATE.                                                ELXPMCBC
00726                                                                   ELXPMCBC
00727 ************************************************************      ELXPMCBC
00728 *                                                          *      ELXPMCBC
00729 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR RPO   *      ELXPMCBC
00730 *                                                          *      ELXPMCBC
00731 ************************************************************      ELXPMCBC
00732  0170-SCN-SP-CCP-IRPO.                                            ELXPMCBC
00733                                                                   ELXPMCBC
00734      EVALUATE TRUE                                                ELXPMCBC
00735         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00736           IF WS-RPO-INC-FOUND                                     ELXPMCBC
00737             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00738           ELSE                                                    ELXPMCBC
00739             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00740           END-IF                                                  ELXPMCBC
00741         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBC
00742             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00743             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00744         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBC
00745             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00746             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00747         WHEN OTHER                                                ELXPMCBC
00748             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00749      END-EVALUATE.                                                ELXPMCBC
00750                                                                   ELXPMCBC
00751 ************************************************************      ELXPMCBC
00752 *                                                          *      ELXPMCBC
00753 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-RPO   *      ELXPMCBC
00754 *                                                          *      ELXPMCBC
00755 ************************************************************      ELXPMCBC
00756  0171-SCN-SP-CCP-ORPO.                                            ELXPMCBC
00757                                                                   ELXPMCBC
00758      EVALUATE TRUE                                                ELXPMCBC
00759         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00760           IF WS-RPO-PEN-FOUND                                     ELXPMCBC
00761             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00762           ELSE                                                    ELXPMCBC
00763             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00764           END-IF                                                  ELXPMCBC
00765         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBC
00766             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00767             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00768         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBC
00769             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00770             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00771         WHEN OTHER                                                ELXPMCBC
00772             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00773      END-EVALUATE.                                                ELXPMCBC
00774                                                                   ELXPMCBC
00775 ************************************************************      ELXPMCBC
00776 *                                                          *      ELXPMCBC
00777 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR BAE   *      ELXPMCBC
00778 *                                                          *      ELXPMCBC
00779 ************************************************************      ELXPMCBC
00780  0172-SCN-SP-CCP-IBAE.                                            ELXPMCBC
00781                                                                   ELXPMCBC
00782      EVALUATE TRUE                                                ELXPMCBC
00783         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00784           IF WS-BAE-PEN-FOUND                                     ELXPMCBC
00785             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00786           END-IF                                                  ELXPMCBC
00787         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00788             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00789             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00790         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00791             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00792             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00793         WHEN OTHER                                                ELXPMCBC
00794             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00795      END-EVALUATE.                                                ELXPMCBC
00796                                                                   ELXPMCBC
00797 ************************************************************      ELXPMCBC
00798 *                                                          *      ELXPMCBC
00799 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR MCNP  *      ELXPMCBC
00800 *                                                          *      ELXPMCBC
00801 ************************************************************      ELXPMCBC
00802  0180-SCN-SP-CCP-IMCNP.                                           ELXPMCBC
00803                                                                   ELXPMCBC
00804      EVALUATE TRUE                                                ELXPMCBC
00805         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00806           IF WS-MCNP-INC-FOUND                                    ELXPMCBC
00807             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00808           ELSE                                                    ELXPMCBC
00809             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00810           END-IF                                                  ELXPMCBC
00811         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBC
00812             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00813             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00814         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBC
00815             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00816             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00817         WHEN OTHER                                                ELXPMCBC
00818             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00819      END-EVALUATE.                                                ELXPMCBC
00820                                                                   ELXPMCBC
00821 ************************************************************      ELXPMCBC
00822 *                                                          *      ELXPMCBC
00823 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-MCNP  *      ELXPMCBC
00824 *                                                          *      ELXPMCBC
00825 ************************************************************      ELXPMCBC
00826  0181-SCN-SP-CCP-OMCNP.                                           ELXPMCBC
00827                                                                   ELXPMCBC
00828      EVALUATE TRUE                                                ELXPMCBC
00829         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00830           IF WS-MCNP-PEN-FOUND                                    ELXPMCBC
00831             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00832           ELSE                                                    ELXPMCBC
00833             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00834           END-IF                                                  ELXPMCBC
00835         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBC
00836             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00837             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00838         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBC
00839             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00840             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00841         WHEN OTHER                                                ELXPMCBC
00842             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00843      END-EVALUATE.                                                ELXPMCBC
00844                                                                   ELXPMCBC
00845 ************************************************************      ELXPMCBC
00846 *                                                          *      ELXPMCBC
00847 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PRG*      ELXPMCBC
00848 *                                                          *      ELXPMCBC
00849 ************************************************************      ELXPMCBC
00850  0190-SCN-SP-CCP-CALL.                                            ELXPMCBC
00851                                                                   ELXPMCBC
00852      EVALUATE TRUE                                                ELXPMCBC
00853         WHEN PMCI-PRG-PPO-APPLIES                                 ELXPMCBC
00854             PERFORM 0191-SCN-SP-CCP-CALL-PPO                      ELXPMCBC
00855         WHEN PMCI-PRG-RPO-APPLIES                                 ELXPMCBC
00856             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBC
00857         WHEN PMCI-PRG-RPO-PPO-APPLIES                             ELXPMCBC
00858             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBC
00859         WHEN PMCI-PRG-MCNP-APPLIES                                ELXPMCBC
00860             PERFORM 0193-SCN-SP-CCP-CALL-MCNP                     ELXPMCBC
00861         WHEN PMCI-PRG-BAE-APPLIES                                 ELXPMCBC
00862             PERFORM 0194-SCN-SP-CCP-CALL-BAE                      ELXPMCBC
00863         WHEN OTHER                                                ELXPMCBC
00864             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00865      END-EVALUATE.                                                ELXPMCBC
00866                                                                   ELXPMCBC
00867 ************************************************************      ELXPMCBC
00868 *                                                          *      ELXPMCBC
00869 *SET CALL FOR PPO FACTORS                                  *      ELXPMCBC
00870 *                                                          *      ELXPMCBC
00871 ************************************************************      ELXPMCBC
00872  0191-SCN-SP-CCP-CALL-PPO.                                        ELXPMCBC
00873                                                                   ELXPMCBC
00874      EVALUATE TRUE                                                ELXPMCBC
00875         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00876             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00877         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBC
00878             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00879             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00880         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBC
00881             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00882             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00883         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBC
00884             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00885             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00886         WHEN OTHER                                                ELXPMCBC
00887             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00888      END-EVALUATE.                                                ELXPMCBC
00889 ************************************************************      ELXPMCBC
00890 *                                                          *      ELXPMCBC
00891 *SET CALL FOR BAE FACTORS                                  *      ELXPMCBC
00892 *                                                          *      ELXPMCBC
00893 ************************************************************      ELXPMCBC
00894  0194-SCN-SP-CCP-CALL-BAE.                                        ELXPMCBC
00895                                                                   ELXPMCBC
00896      EVALUATE TRUE                                                ELXPMCBC
00897         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00898             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00899         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBC
00900             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00901             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00902         WHEN OTHER                                                ELXPMCBC
00903             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00904      END-EVALUATE.                                                ELXPMCBC
00905 ************************************************************      ELXPMCBC
00906 *                                                          *      ELXPMCBC
00907 *SET CALL FOR RPO FACTORS                                  *      ELXPMCBC
00908 *                                                          *      ELXPMCBC
00909 ************************************************************      ELXPMCBC
00910  0192-SCN-SP-CCP-CALL-RPO.                                        ELXPMCBC
00911                                                                   ELXPMCBC
00912      EVALUATE TRUE                                                ELXPMCBC
00913         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00914             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00915         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBC
00916             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00917             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00918         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBC
00919             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00920             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00921         WHEN OTHER                                                ELXPMCBC
00922             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00923      END-EVALUATE.                                                ELXPMCBC
00924 ************************************************************      ELXPMCBC
00925 *                                                          *      ELXPMCBC
00926 *SET CALL FOR MCNP FACTORS                                 *      ELXPMCBC
00927 *                                                          *      ELXPMCBC
00928 ************************************************************      ELXPMCBC
00929  0193-SCN-SP-CCP-CALL-MCNP.                                       ELXPMCBC
00930                                                                   ELXPMCBC
00931      EVALUATE TRUE                                                ELXPMCBC
00932         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBC
00933             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00934         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBC
00935             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00936             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00937         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBC
00938             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBC
00939             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBC
00940         WHEN OTHER                                                ELXPMCBC
00941             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBC
00942      END-EVALUATE.                                                ELXPMCBC
00943      IF PMCI-REFERRAL-EXISTS                                      ELXPMCBC
00944         MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX).      ELXPMCBC
00945                                                                   ELXPMCBC
00946                                                                   ELXPMCBC
00947 ************************************************************      ELXPMCBC
00948 *                                                          *      ELXPMCBC
00949 * IDENTIFY INSTITUTIONAL OUTPATIENT DEDUCTIBLES            *      ELXPMCBC
00950 *                                                          *      ELXPMCBC
00951 ************************************************************      ELXPMCBC
00952  2000-IDNTFY-INST-OP-VALS.                                        ELXPMCBC
00953                                                                   ELXPMCBC
00954      PERFORM 2001-INITIALIZE.                                     ELXPMCBC
00955      PERFORM 5000-DETERMINE-EMERGENCY.                            ELXPMCBC
00956                                                                   ELXPMCBC
00957 ************************************************************      ELXPMCBC
00958 *                                                          *      ELXPMCBC
00959 * IDENTIFY INSTITUTIONAL OUTPATIENT DEDUCTIBLES/INITIALIZE *      ELXPMCBC
00960 *                                                          *      ELXPMCBC
00961 ************************************************************      ELXPMCBC
00962  2001-INITIALIZE.                                                 ELXPMCBC
00963                                                                   ELXPMCBC
00964      CALL 'ELUADDRS' USING WS-INST-OP-EMERA                       ELXPMCBC
00965                          WS-EMERA-POINTER.                        ELXPMCBC
00966      CALL 'ELUADDRS' USING WS-INST-OP-EMERM                       ELXPMCBC
00967                          WS-EMERM-POINTER.                        ELXPMCBC
00968      CALL 'ELUADDRS' USING WS-INST-OP-EMERL                       ELXPMCBC
00969                          WS-EMERL-POINTER.                        ELXPMCBC
00970      SET OFVS-NOT-APPLICABLE TO TRUE.                             ELXPMCBC
00971                                                                   ELXPMCBC
00972 ************************************************************      ELXPMCBC
00973 *                                                          *      ELXPMCBC
00974 * IDENTIFY PROFESSIONAL OUTPATIENT DEDUCTIBLES             *      ELXPMCBC
00975 *                                                          *      ELXPMCBC
00976 ************************************************************      ELXPMCBC
00977  4000-IDNTFY-PROF-OP-VALS.                                        ELXPMCBC
00978                                                                   ELXPMCBC
00979      PERFORM 4001-INITIALIZE.                                     ELXPMCBC
00980      PERFORM 4500-IDNTFY-PROF-OP-OVST-COPAY.                      ELXPMCBC
00981      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
00982         PERFORM 5000-DETERMINE-EMERGENCY.                         ELXPMCBC
00983 ************************************************************      ELXPMCBC
00984 *                                                          *      ELXPMCBC
00985 * IDENTIFY PROFESSIONAL OUTPATIENT DEDUCTIBLES/INITIALIZE  *      ELXPMCBC
00986 *                                                          *      ELXPMCBC
00987 ************************************************************      ELXPMCBC
00988  4001-INITIALIZE.                                                 ELXPMCBC
00989                                                                   ELXPMCBC
00990      CALL 'ELUADDRS' USING WS-PROF-OP-EMERA                       ELXPMCBC
00991                          WS-EMERA-POINTER.                        ELXPMCBC
00992      CALL 'ELUADDRS' USING WS-PROF-OP-EMERM                       ELXPMCBC
00993                          WS-EMERM-POINTER.                        ELXPMCBC
00994      CALL 'ELUADDRS' USING WS-PROF-OP-EMERL                       ELXPMCBC
00995                          WS-EMERL-POINTER.                        ELXPMCBC
00996      CALL 'ELUADDRS' USING WS-PROF-OP-OVSTS                       ELXPMCBC
00997                          WS-OVST-POINTER.                         ELXPMCBC
00998                                                                   ELXPMCBC
00999 ************************************************************      ELXPMCBC
01000 *                                                          *      ELXPMCBC
01001 * IDENTIFY PROFESSIONAL OUTPATIENT OFFICE VISIT            *      ELXPMCBC
01002 *                                                          *      ELXPMCBC
01003 ************************************************************      ELXPMCBC
01004  4500-IDNTFY-PROF-OP-OVST-COPAY.                                  ELXPMCBC
01005                                                                   ELXPMCBC
01006      IF PMCI-OFF-VISITS-COVERED                                   ELXPMCBC
01007         MOVE ZEROS TO WS-COPAY-VALUE                              ELXPMCBC
01008         MOVE SPACES TO WS-VALUE-INDICATOR                         ELXPMCBC
01009         SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-OVST-POINTER     ELXPMCBC
01010         SET OFVS-NOT-APPLICABLE TO TRUE                           ELXPMCBC
01011         PERFORM 7000-SRCH-IBGR-COPAY-OFV                          ELXPMCBC
01012         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBC
01013            MOVE CVG2-OV-THRSHLD-ADL TO WS-TEST-THRESHOLD          ELXPMCBC
01014            PERFORM 4690-TST-OFV-COPAY.                            ELXPMCBC
01015                                                                   ELXPMCBC
01016 ************************************************************      ELXPMCBC
01017 *                                                          *      ELXPMCBC
01018 * SEARCH FOR OFFICE VISIT COPAY AMOUNT                     *      ELXPMCBC
01019 *                                                          *      ELXPMCBC
01020 ************************************************************      ELXPMCBC
01021  4690-TST-OFV-COPAY.                                              ELXPMCBC
01022                                                                   ELXPMCBC
01023      SET PROCESSING-OVS TO TRUE                                   ELXPMCBC
01024      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCBC
01025      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01026         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBC
01027         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBC
01028            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBC
01029            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBC
01030                (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCBC
01031                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBC
01032                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBC
01033                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBC
01034               SET OFVS-CALL TO TRUE                               ELXPMCBC
01035            ELSE                                                   ELXPMCBC
01036               PERFORM 9400-FND-BNFT-PRD-CF                        ELXPMCBC
01037               PERFORM 9600-DETERMINE-VALUE                        ELXPMCBC
01038               PERFORM 9700-LOAD-ACCUM-CDE-ATBL-TABLE              ELXPMCBC
01039               MOVE WS-COPAY-VALUE TO                              ELXPMCBC
01040                              PMCI-OFFICE-VISITS-CO-PAY-VL         ELXPMCBC
01041               IF CFTA-BNFT-PRD (CFTA-IDX) = 'CA'                  ELXPMCBC
01042                    MOVE '1' TO PMCI-OFFICE-VISITS-CO-PAY-BP       ELXPMCBC
01043                  ELSE                                             ELXPMCBC
01044                    MOVE WS-BNF-QUAL TO                            ELXPMCBC
01045                              PMCI-OFFICE-VISITS-CO-PAY-BP         ELXPMCBC
01046               END-IF                                              ELXPMCBC
01047            END-IF                                                 ELXPMCBC
01048         ELSE                                                      ELXPMCBC
01049            SET OFVS-NOT-APPLICABLE TO TRUE                        ELXPMCBC
01050         END-IF                                                    ELXPMCBC
01051      END-IF.                                                      ELXPMCBC
01052      INITIALIZE WS-PROCESS-OVS-SWITCH.                            ELXPMCBC
01053                                                                   ELXPMCBC
01054 ************************************************************      ELXPMCBC
01055 *                                                          *      ELXPMCBC
01056 * DETERMINE IF EMERGENCY PROCESSING IS REQUIRED            *      ELXPMCBC
01057 *                                                          *      ELXPMCBC
01058 ************************************************************      ELXPMCBC
01059  5000-DETERMINE-EMERGENCY.                                        ELXPMCBC
01060                                                                   ELXPMCBC
01061      IF EAC-HOURS                                                 ELXPMCBC
01062         PERFORM 5100-SRCH-EAC-COPAY                               ELXPMCBC
01063 *EMC-NO-HOURS ADDED WHEN TESTING BLUE STORM.  EMC COPAYS DO EXIST ELXPMCBC
01064 *WHEN THERE IS NO SPECIFIC HOURS LIMIT FOR MEDICAL EMERGENCIES    ELXPMCBC
01065 *                                                                 ELXPMCBC
01066         IF EMC-HOURS OR EMC-NO-HOURS                              ELXPMCBC
01067            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBC
01068               PERFORM 5200-SRCH-EMC-COPAY                         ELXPMCBC
01069               IF PMCI-BC-SUCCESSFUL                               ELXPMCBC
01070                  PERFORM 5300-SRCH-EMLF-COPAY                     ELXPMCBC
01071         ELSE                                                      ELXPMCBC
01072            SET EMMD-NO-COVERAGE TO TRUE                           ELXPMCBC
01073            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBC
01074               PERFORM 5300-SRCH-EMLF-COPAY                        ELXPMCBC
01075         END-IF                                                    ELXPMCBC
01076      ELSE                                                         ELXPMCBC
01077         SET EMAC-NO-COVERAGE TO TRUE                              ELXPMCBC
01078         IF EMC-HOURS                                              ELXPMCBC
01079            PERFORM 5200-SRCH-EMC-COPAY                            ELXPMCBC
01080            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBC
01081               PERFORM 5300-SRCH-EMLF-COPAY                        ELXPMCBC
01082         ELSE                                                      ELXPMCBC
01083            SET EMMD-NO-COVERAGE TO TRUE                           ELXPMCBC
01084            SET EMLF-NO-COVERAGE TO TRUE                           ELXPMCBC
01085         END-IF                                                    ELXPMCBC
01086      END-IF.                                                      ELXPMCBC
01087                                                                   ELXPMCBC
01088 ************************************************************      ELXPMCBC
01089 *                                                          *      ELXPMCBC
01090 * SEARCH FOR EMERGENCY ACCIDENT COPAY                      *      ELXPMCBC
01091 *                                                          *      ELXPMCBC
01092 ************************************************************      ELXPMCBC
01093  5100-SRCH-EAC-COPAY.                                             ELXPMCBC
01094                                                                   ELXPMCBC
01095      MOVE ZEROS TO WS-COPAY-VALUE.                                ELXPMCBC
01096      MOVE SPACES TO WS-VALUE-INDICATOR.                           ELXPMCBC
01097      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-EMERA-POINTER.      ELXPMCBC
01098      SET EMAC-NOT-APPLICABLE TO TRUE                              ELXPMCBC
01099      PERFORM 6000-SRCH-IBGR-COPAY.                                ELXPMCBC
01100      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01101         MOVE CVG2-OV-THRSHLD-ADL TO WS-TEST-THRESHOLD             ELXPMCBC
01102         PERFORM 5190-TST-EAC-COPAY.                               ELXPMCBC
01103                                                                   ELXPMCBC
01104 ************************************************************      ELXPMCBC
01105 *                                                          *      ELXPMCBC
01106 * SEARCH FOR EMERGENCY ACCIDENT COPAY AMOUNT               *      ELXPMCBC
01107 *                                                          *      ELXPMCBC
01108 ************************************************************      ELXPMCBC
01109  5190-TST-EAC-COPAY.                                              ELXPMCBC
01110                                                                   ELXPMCBC
01111      SET PROCESSING-EMER TO TRUE                                  ELXPMCBC
01112      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCBC
01113      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01114         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBC
01115         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBC
01116            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBC
01117            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBC
01118                      WS-NUM-APPL-ENTRS > 1                        ELXPMCBC
01119               SET EMAC-CALL TO TRUE                               ELXPMCBC
01120            ELSE                                                   ELXPMCBC
01121               PERFORM 9400-FND-BNFT-PRD-CF                        ELXPMCBC
01122               PERFORM 9600-DETERMINE-VALUE                        ELXPMCBC
01123               MOVE WS-COPAY-VALUE TO                              ELXPMCBC
01124                              PMCI-EMERG-ACCDNT-CO-PAY-VL          ELXPMCBC
01125               MOVE WS-BNF-QUAL TO                                 ELXPMCBC
01126                              PMCI-EMERG-ACCDNT-CO-PAY-BP          ELXPMCBC
01127            END-IF                                                 ELXPMCBC
01128         ELSE                                                      ELXPMCBC
01129            SET EMAC-NOT-APPLICABLE TO TRUE                        ELXPMCBC
01130         END-IF                                                    ELXPMCBC
01131      END-IF.                                                      ELXPMCBC
01132      INITIALIZE WS-PROCESS-EMER-SWITCH.                           ELXPMCBC
01133                                                                   ELXPMCBC
01134 ************************************************************      ELXPMCBC
01135 *                                                          *      ELXPMCBC
01136 * SEARCH FOR EMERGENCY MEDICAL COPAY                       *      ELXPMCBC
01137 *                                                          *      ELXPMCBC
01138 ************************************************************      ELXPMCBC
01139  5200-SRCH-EMC-COPAY.                                             ELXPMCBC
01140                                                                   ELXPMCBC
01141      MOVE ZEROS TO WS-COPAY-VALUE.                                ELXPMCBC
01142      MOVE SPACES TO WS-VALUE-INDICATOR.                           ELXPMCBC
01143      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-EMERM-POINTER.      ELXPMCBC
01144      SET EMMD-NOT-APPLICABLE TO TRUE                              ELXPMCBC
01145      PERFORM 6000-SRCH-IBGR-COPAY.                                ELXPMCBC
01146      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01147         MOVE CVG2-OV-THRSHLD-ADL TO WS-TEST-THRESHOLD             ELXPMCBC
01148         PERFORM 5290-TST-EAM-COPAY.                               ELXPMCBC
01149 ************************************************************      ELXPMCBC
01150 *                                                          *      ELXPMCBC
01151 * SEARCH FOR EMERGENCY MEDICAL  COPAY AMOUNT               *      ELXPMCBC
01152 *                                                          *      ELXPMCBC
01153 ************************************************************      ELXPMCBC
01154  5290-TST-EAM-COPAY.                                              ELXPMCBC
01155                                                                   ELXPMCBC
01156      SET PROCESSING-EMER TO TRUE                                  ELXPMCBC
01157      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCBC
01158      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01159         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBC
01160         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBC
01161            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBC
01162            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBC
01163                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBC
01164               SET EMMD-CALL TO TRUE                               ELXPMCBC
01165            ELSE                                                   ELXPMCBC
01166               PERFORM 9400-FND-BNFT-PRD-CF                        ELXPMCBC
01167               PERFORM 9600-DETERMINE-VALUE                        ELXPMCBC
01168               MOVE WS-COPAY-VALUE TO                              ELXPMCBC
01169                              PMCI-EMERG-MEDCL-CO-PAY-VL           ELXPMCBC
01170               MOVE WS-BNF-QUAL TO                                 ELXPMCBC
01171                              PMCI-EMERG-MEDCL-CO-PAY-BP           ELXPMCBC
01172            END-IF                                                 ELXPMCBC
01173         ELSE                                                      ELXPMCBC
01174            SET EMMD-NOT-APPLICABLE TO TRUE                        ELXPMCBC
01175         END-IF                                                    ELXPMCBC
01176      END-IF.                                                      ELXPMCBC
01177      INITIALIZE WS-PROCESS-EMER-SWITCH.                           ELXPMCBC
01178                                                                   ELXPMCBC
01179 ************************************************************      ELXPMCBC
01180 *                                                          *      ELXPMCBC
01181 * SEARCH FOR EMERGENCY LIFE THREATENING CO-PAY             *      ELXPMCBC
01182 *                                                          *      ELXPMCBC
01183 ************************************************************      ELXPMCBC
01184  5300-SRCH-EMLF-COPAY.                                            ELXPMCBC
01185                                                                   ELXPMCBC
01186      MOVE ZEROS TO WS-COPAY-VALUE.                                ELXPMCBC
01187      MOVE SPACES TO WS-VALUE-INDICATOR.                           ELXPMCBC
01188      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-EMERL-POINTER.      ELXPMCBC
01189      SET EMLF-NOT-APPLICABLE TO TRUE                              ELXPMCBC
01190      PERFORM 6000-SRCH-IBGR-COPAY.                                ELXPMCBC
01191      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01192         MOVE CVG2-OV-THRSHLD-ADL TO WS-TEST-THRESHOLD             ELXPMCBC
01193         PERFORM 5390-TST-ELFT-COPAY.                              ELXPMCBC
01194 ************************************************************      ELXPMCBC
01195 *                                                          *      ELXPMCBC
01196 * SEARCH FOR EMERGENCY LIFE THREATENING COPAY AMOUNT       *      ELXPMCBC
01197 *                                                          *      ELXPMCBC
01198 ************************************************************      ELXPMCBC
01199  5390-TST-ELFT-COPAY.                                             ELXPMCBC
01200                                                                   ELXPMCBC
01201      SET PROCESSING-EMER TO TRUE.                                 ELXPMCBC
01202      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCBC
01203      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01204         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBC
01205         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBC
01206            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBC
01207            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBC
01208                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBC
01209               SET EMLF-CALL TO TRUE                               ELXPMCBC
01210            ELSE                                                   ELXPMCBC
01211               PERFORM 9400-FND-BNFT-PRD-CF                        ELXPMCBC
01212               PERFORM 9600-DETERMINE-VALUE                        ELXPMCBC
01213               MOVE WS-COPAY-VALUE TO                              ELXPMCBC
01214                              PMCI-EMERG-LFTHRN-CO-PAY-VL          ELXPMCBC
01215               MOVE WS-BNF-QUAL TO                                 ELXPMCBC
01216                              PMCI-EMERG-LFTHRN-CO-PAY-BP          ELXPMCBC
01217            END-IF                                                 ELXPMCBC
01218         ELSE                                                      ELXPMCBC
01219            SET EMLF-NOT-APPLICABLE TO TRUE                        ELXPMCBC
01220         END-IF                                                    ELXPMCBC
01221      END-IF.                                                      ELXPMCBC
01222      INITIALIZE WS-PROCESS-EMER-SWITCH.                           ELXPMCBC
01223                                                                   ELXPMCBC
01224 ************************************************************      ELXPMCBC
01225 *                                                          *      ELXPMCBC
01226 * SEARCH BENEFIT PROVISIONS FOR EMERGENCY CONDITIONS       *      ELXPMCBC
01227 *                                                          *      ELXPMCBC
01228 ************************************************************      ELXPMCBC
01229  6000-SRCH-IBGR-COPAY.                                            ELXPMCBC
01230                                                                   ELXPMCBC
01231      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBC
01232         CONTINUE                                                  ELXPMCBC
01233      ELSE                                                         ELXPMCBC
01234         PERFORM 6001-RCMPT-BNFT-PRVSN-EMER                        ELXPMCBC
01235         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBC
01236            PERFORM 6020-SCN-EMER-COPAY.                           ELXPMCBC
01237                                                                   ELXPMCBC
01238 ************************************************************      ELXPMCBC
01239 *                                                          *      ELXPMCBC
01240 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR EMERGENCY COPAYS*        ELXPMCBC
01241 *                                                          *      ELXPMCBC
01242 ************************************************************      ELXPMCBC
01243  6001-RCMPT-BNFT-PRVSN-EMER.                                      ELXPMCBC
01244                                                                   ELXPMCBC
01245      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBC
01246      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBC
01247                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBC
01248      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCBC
01249      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBC
01250         MOVE +4708 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBC
01251         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBC
01252                                                                   ELXPMCBC
01253 ************************************************************      ELXPMCBC
01254 *                                                          *      ELXPMCBC
01255 * SEARCH FOR EMERGENCY COPAY                               *      ELXPMCBC
01256 *                                                          *      ELXPMCBC
01257 ************************************************************      ELXPMCBC
01258  6020-SCN-EMER-COPAY.                                             ELXPMCBC
01259                                                                   ELXPMCBC
01260      PERFORM 6030-RCMPT-SP-EMER                                   ELXPMCBC
01261          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCBC
01262              UNTIL ATBL-IDX > ATBL-MAX-IDX OR                     ELXPMCBC
01263                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.              ELXPMCBC
01264                                                                   ELXPMCBC
01265 ************************************************************      ELXPMCBC
01266 *                                                          *      ELXPMCBC
01267 * RECOMPUTE CONF FACTORS FOR EMERGENCY COPAYS              *      ELXPMCBC
01268 *                                                          *      ELXPMCBC
01269 ************************************************************      ELXPMCBC
01270  6030-RCMPT-SP-EMER.                                              ELXPMCBC
01271                                                                   ELXPMCBC
01272      PERFORM 9400-FND-BNFT-PRD-CF                                 ELXPMCBC
01273      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01274        IF ATBL-BENEFIT-PERIOD (ATBL-IDX) = '0Y'                   ELXPMCBC
01275           MOVE WS-CF-TRUE TO CFTA-CF-EMRGNC (CFTA-IDX)            ELXPMCBC
01276        ELSE                                                       ELXPMCBC
01277         MOVE CFTA-CF-EMRGNC (CFTA-IDX) TO                         ELXPMCBC
01278                  ATBL-CF-BNFT-PRD (ATBL-IDX)                      ELXPMCBC
01279        END-IF                                                     ELXPMCBC
01280         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBC
01281         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCBC
01282         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBC
01283            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCBC
01284                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCBC
01285            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBC
01286            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBC
01287               MOVE CFT5-CF-INTD-EMRGNC (CFT5-IDX) TO              ELXPMCBC
01288                        ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)       ELXPMCBC
01289               PERFORM 9150-RCMPT-VL-QLFR-CF-PER-DLR.              ELXPMCBC
01290                                                                   ELXPMCBC
01291 ************************************************************      ELXPMCBC
01292 *                                                          *      ELXPMCBC
01293 * SEARCH BENEFIT PROVISIONS FOR OFFICE VISITS              *      ELXPMCBC
01294 *                                                          *      ELXPMCBC
01295 ************************************************************      ELXPMCBC
01296  7000-SRCH-IBGR-COPAY-OFV.                                        ELXPMCBC
01297                                                                   ELXPMCBC
01298      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCBC
01299         CONTINUE                                                  ELXPMCBC
01300      ELSE                                                         ELXPMCBC
01301         PERFORM 7001-RCMPT-BNFT-PRVSN-OFVS                        ELXPMCBC
01302         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBC
01303            PERFORM 7020-SCN-OFVS-COPAY.                           ELXPMCBC
01304                                                                   ELXPMCBC
01305 ************************************************************      ELXPMCBC
01306 *                                                          *      ELXPMCBC
01307 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR OFFICE VISITS COPAY      ELXPMCBC
01308 *                                                          *      ELXPMCBC
01309 ************************************************************      ELXPMCBC
01310  7001-RCMPT-BNFT-PRVSN-OFVS.                                      ELXPMCBC
01311                                                                   ELXPMCBC
01312      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBC
01313      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBC
01314                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBC
01315      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCBC
01316      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBC
01317         MOVE +4708 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBC
01318         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBC
01319                                                                   ELXPMCBC
01320 ************************************************************      ELXPMCBC
01321 *                                                          *      ELXPMCBC
01322 * SEARCH FOR OFFICE VISIT COPAY                            *      ELXPMCBC
01323 *                                                          *      ELXPMCBC
01324 ************************************************************      ELXPMCBC
01325  7020-SCN-OFVS-COPAY.                                             ELXPMCBC
01326                                                                   ELXPMCBC
01327      PERFORM 7030-RCMPT-SP-OFVS                                   ELXPMCBC
01328          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCBC
01329              UNTIL ATBL-IDX > ATBL-MAX-IDX OR                     ELXPMCBC
01330                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.              ELXPMCBC
01331                                                                   ELXPMCBC
01332 ************************************************************      ELXPMCBC
01333 *                                                          *      ELXPMCBC
01334 * RECOMPUTE CONF FACTORS FOR OFFICE VISIT COPAYS           *      ELXPMCBC
01335 *                                                          *      ELXPMCBC
01336 ************************************************************      ELXPMCBC
01337  7030-RCMPT-SP-OFVS.                                              ELXPMCBC
01338                                                                   ELXPMCBC
01339      PERFORM 9400-FND-BNFT-PRD-CF                                 ELXPMCBC
01340      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBC
01341        IF ATBL-BENEFIT-PERIOD (ATBL-IDX) = '0Y'                   ELXPMCBC
01342          MOVE WS-CF-TRUE TO ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01343        ELSE                                                       ELXPMCBC
01344         MOVE CFTA-CF-OFVST (CFTA-IDX) TO                          ELXPMCBC
01345                  ATBL-CF-BNFT-PRD (ATBL-IDX)                      ELXPMCBC
01346        END-IF                                                     ELXPMCBC
01347         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBC
01348         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCBC
01349         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBC
01350            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCBC
01351                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCBC
01352            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBC
01353            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBC
01354               MOVE CFT5-CF-INTD-OFVST (CFT5-IDX) TO               ELXPMCBC
01355                        ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)       ELXPMCBC
01356               PERFORM 9150-RCMPT-VL-QLFR-CF-PER-DLR.              ELXPMCBC
01357                                                                   ELXPMCBC
01358 ************************************************************      ELXPMCBC
01359 *                                                          *      ELXPMCBC
01360 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBC
01361 *                                                          *      ELXPMCBC
01362 ************************************************************      ELXPMCBC
01363  9120-RCMPT-BNFT-PRVSN-CF.                                        ELXPMCBC
01364                                                                   ELXPMCBC
01365      IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                      ELXPMCBC
01366         MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)    ELXPMCBC
01367         SET WS-IBGR-FOUND TO TRUE                                 ELXPMCBC
01368      ELSE                                                         ELXPMCBC
01369         SET WS-IBGR-NOT-FOUND TO TRUE                             ELXPMCBC
01370         SET IBGR-MAX-IDX TO IBGR-TBL-CNT                          ELXPMCBC
01371         PERFORM VARYING IBGR-IDX FROM 1 BY 1                      ELXPMCBC
01372            UNTIL IBGR-IDX > IBGR-MAX-IDX OR                       ELXPMCBC
01373                WS-IBGR-FOUND                                      ELXPMCBC
01374         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) =                     ELXPMCBC
01375                          IBGR-SLOT-NUMBER (IBGR-IDX)              ELXPMCBC
01376            SET WS-IBGR-FOUND TO TRUE                              ELXPMCBC
01377            MOVE IBGR-CF-LIST-MTCH (IBGR-IDX) TO                   ELXPMCBC
01378                            ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)       ELXPMCBC
01379         END-IF                                                    ELXPMCBC
01380         END-PERFORM.                                              ELXPMCBC
01381      IF WS-IBGR-NOT-FOUND                                         ELXPMCBC
01382         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBC
01383         MOVE +4704 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBC
01384                                                                   ELXPMCBC
01385 ************************************************************      ELXPMCBC
01386 *                                                          *      ELXPMCBC
01387 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBC
01388 *                                                          *      ELXPMCBC
01389 ************************************************************      ELXPMCBC
01390  9130-FND-INTRNL-DSCRPTR-CF.                                      ELXPMCBC
01391                                                                   ELXPMCBC
01392      SET WS-INTRNLDSC-NOT-FOUND TO TRUE.                          ELXPMCBC
01393      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELXPMCBC
01394      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELXPMCBC
01395         UNTIL CFT5-IDX > CFT5-MAX-IDX OR                          ELXPMCBC
01396           WS-INTRNLDSC-FOUND                                      ELXPMCBC
01397         IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) =                  ELXPMCBC
01398                           CFT5-INTD (CFT5-IDX)                    ELXPMCBC
01399            SET WS-INTRNLDSC-FOUND TO TRUE                         ELXPMCBC
01400            SET WS-SUB-WORK TO CFT5-IDX                            ELXPMCBC
01401            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCBC
01402            SET CFT5-IDX TO WS-SUB-WORK                            ELXPMCBC
01403         END-IF                                                    ELXPMCBC
01404      END-PERFORM.                                                 ELXPMCBC
01405      IF WS-INTRNLDSC-NOT-FOUND                                    ELXPMCBC
01406         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBC
01407         MOVE +4705 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBC
01408                                                                   ELXPMCBC
01409 ************************************************************      ELXPMCBC
01410 *                                                          *      ELXPMCBC
01411 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DOLLARS         *      ELXPMCBC
01412 *                                                          *      ELXPMCBC
01413 ************************************************************      ELXPMCBC
01414  9150-RCMPT-VL-QLFR-CF-PER-DLR.                                   ELXPMCBC
01415                                                                   ELXPMCBC
01416      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '5'                     ELXPMCBC
01417         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCBC
01418      ELSE                                                         ELXPMCBC
01419         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCBC
01420                                                                   ELXPMCBC
01421 ************************************************************      ELXPMCBC
01422 *                                                          *      ELXPMCBC
01423 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTOR AND SCAN FOR    *      ELXPMCBC
01424 *    APPLICABLE ENTRY.                                     *      ELXPMCBC
01425 *                                                          *      ELXPMCBC
01426 ************************************************************      ELXPMCBC
01427  9200-RCMPT-SP-SCN-APLCBL-ENTRY.                                  ELXPMCBC
01428                                                                   ELXPMCBC
01429      CALL 'ELKSPCFF'  USING ATBL-ACCUMULATOR-TABLE.               ELXPMCBC
01430      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBC
01431      IF WS-SUCCESSFUL-CALL                                        ELXPMCBC
01432         MOVE ZERO TO WS-SAVE-SUB-BSC                              ELXPMCBC
01433         MOVE WS-CF-FALSE TO WS-TEST-CONF-BSC                      ELXPMCBC
01434         MOVE ZERO TO WS-APPL-ENTRS-BSC                            ELXPMCBC
01435         MOVE ZERO TO WS-SAVE-SUB-MM                               ELXPMCBC
01436         MOVE WS-CF-FALSE TO WS-TEST-CONF-MM                       ELXPMCBC
01437         MOVE ZERO TO WS-APPL-ENTRS-MM                             ELXPMCBC
01438         PERFORM 9220-RCMPT-CRNT-ACCM-TBL-CF                       ELXPMCBC
01439            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBC
01440               UNTIL ATBL-IDX > ATBL-MAX-IDX                       ELXPMCBC
01441      ELSE                                                         ELXPMCBC
01442         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBC
01443         MOVE +4706 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBC
01444                                                                   ELXPMCBC
01445                                                                   ELXPMCBC
01446 ************************************************************      ELXPMCBC
01447 *                                                          *      ELXPMCBC
01448 *  RECOMPUTE CURRENT ACCUMULATOR TABLE WORK CONF FACTOR    *      ELXPMCBC
01449 *                                                          *      ELXPMCBC
01450 ************************************************************      ELXPMCBC
01451  9220-RCMPT-CRNT-ACCM-TBL-CF.                                     ELXPMCBC
01452                                                                   ELXPMCBC
01453      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCBC
01454         IF PMCI-INSTITUTIONAL                                     ELXPMCBC
01455            IF PMCI-INPATIENT                                      ELXPMCBC
01456               PERFORM 9230-RCMPT-INST-INP                         ELXPMCBC
01457            ELSE                                                   ELXPMCBC
01458               PERFORM 9240-RCMPT-INST-OUT                         ELXPMCBC
01459         ELSE                                                      ELXPMCBC
01460            IF PMCI-INPATIENT                                      ELXPMCBC
01461               PERFORM 9250-RCMPT-PROF-INP                         ELXPMCBC
01462            ELSE                                                   ELXPMCBC
01463               PERFORM 9260-RCMPT-PROF-OUT.                        ELXPMCBC
01464      IF PMCI-BSC-CNTRCT-GRP EQUAL SPACE                           ELXPMCBC
01465         IF PMCI-INSTITUTIONAL                                     ELXPMCBC
01466            IF PMCI-INPATIENT                                      ELXPMCBC
01467               PERFORM 9235-RCMPT-INST-INP-MM                      ELXPMCBC
01468            ELSE                                                   ELXPMCBC
01469               PERFORM 9245-RCMPT-INST-OUT-MM                      ELXPMCBC
01470         ELSE                                                      ELXPMCBC
01471            IF PMCI-INPATIENT                                      ELXPMCBC
01472               PERFORM 9255-RCMPT-PROF-INP-MM                      ELXPMCBC
01473            ELSE                                                   ELXPMCBC
01474               PERFORM 9265-RCMPT-PROF-OUT-MM.                     ELXPMCBC
01475                                                                   ELXPMCBC
01476 ************************************************************      ELXPMCBC
01477 *                                                          *      ELXPMCBC
01478 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01479 *                                                          *      ELXPMCBC
01480 ************************************************************      ELXPMCBC
01481  9230-RCMPT-INST-INP.                                             ELXPMCBC
01482                                                                   ELXPMCBC
01483      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01484                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01485                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01486                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBC
01487                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBC
01488                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01489                             ATBL-CF-SP (ATBL-IDX).                ELXPMCBC
01490      MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-CF-WORK-ENTRY       ELXPMCBC
01491      MOVE ATBL-CF-BNFT-PRD (ATBL-IDX) TO WS-CF-BNFT-PRD           ELXPMCBC
01492      MOVE ATBL-CF-INDVDL (ATBL-IDX)   TO WS-CF-INDVDL             ELXPMCBC
01493      MOVE ATBL-CF-INST-BAS (ATBL-IDX) TO WS-CF-INST-BAS           ELXPMCBC
01494      MOVE ATBL-CF-IP (ATBL-IDX)       TO WS-CF-OP                 ELXPMCBC
01495      MOVE ATBL-CF-PLAN (ATBL-IDX)     TO WS-CF-PLAN               ELXPMCBC
01496      MOVE ATBL-CF-SP (ATBL-IDX)       TO WS-CF-SP                 ELXPMCBC
01497      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01498                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBC
01499         CONTINUE                                                  ELXPMCBC
01500      ELSE                                                         ELXPMCBC
01501         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01502                      WS-WT-BNFT-PRD                               ELXPMCBC
01503         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01504                      WS-WT-INDVDL                                 ELXPMCBC
01505         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBC
01506                      WS-WT-INST-BAS                               ELXPMCBC
01507         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBC
01508                      WS-WT-IP                                     ELXPMCBC
01509         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01510                      WS-WT-PLAN                                   ELXPMCBC
01511         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01512                      WS-WT-SP                                     ELXPMCBC
01513         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01514      END-IF.                                                      ELXPMCBC
01515                                                                   ELXPMCBC
01516      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01517         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBC
01518         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBC
01519            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01520            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01521                                    WS-TEST-CONF-BSC.              ELXPMCBC
01522                                                                   ELXPMCBC
01523 *ADDED BELOW FOR BLUESTORM AND FOR POSSIBLY FIX FOR PMCI          ELXPMCBC
01524                                                                   ELXPMCBC
01525      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT >  WS-TEST-THRESHOLD    ELXPMCBC
01526      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT = WS-TEST-THRESHOLD     ELXPMCBC
01527         IF (ATBL-BENEFIT-PERIOD (ATBL-IDX) = '0I' OR '0Y')        ELXPMCBC
01528          AND (ATBL-PLACE-OF-TREATMENT (ATBL-IDX) = '0A' OR        ELXPMCBC
01529                                           '0R' OR '0Q')           ELXPMCBC
01530          AND (ATBL-INTERNAL-DESCRIPTOR  (ATBL-IDX) =              ELXPMCBC
01531             'OFFICEVIS') AND PROCESSING-OVS                       ELXPMCBC
01532             IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'            ELXPMCBC
01533               AND PMCI-NON-PPO-PROVIDER                           ELXPMCBC
01534                IF WS-APPL-ENTRS-BSC = 0                           ELXPMCBC
01535                   ADD +1 TO WS-APPL-ENTRS-BSC                     ELXPMCBC
01536                ELSE                                               ELXPMCBC
01537                   CONTINUE                                        ELXPMCBC
01538                SET WS-SAVE-SUB-BSC TO ATBL-IDX                    ELXPMCBC
01539             ELSE                                                  ELXPMCBC
01540              IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'           ELXPMCBC
01541                 AND PMCI-NO-REFERRAL                              ELXPMCBC
01542                IF WS-APPL-ENTRS-BSC = 0                           ELXPMCBC
01543                   ADD +1 TO WS-APPL-ENTRS-BSC                     ELXPMCBC
01544                ELSE                                               ELXPMCBC
01545                   CONTINUE                                        ELXPMCBC
01546                SET WS-SAVE-SUB-BSC TO ATBL-IDX                    ELXPMCBC
01547             ELSE                                                  ELXPMCBC
01548             IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'            ELXPMCBC
01549               AND (PMCI-PPO-PROVIDER OR                           ELXPMCBC
01550                     PMCI-REFERRAL-NOT-REQUIRED)                   ELXPMCBC
01551                IF WS-APPL-ENTRS-BSC = 0                           ELXPMCBC
01552                   ADD +1 TO WS-APPL-ENTRS-BSC                     ELXPMCBC
01553                ELSE                                               ELXPMCBC
01554                   CONTINUE                                        ELXPMCBC
01555                SET WS-SAVE-SUB-BSC TO ATBL-IDX.                   ELXPMCBC
01556                                                                   ELXPMCBC
01557      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBC
01558         PERFORM 9235-RCMPT-INST-INP-MM.                           ELXPMCBC
01559                                                                   ELXPMCBC
01560 ************************************************************      ELXPMCBC
01561 *                                                          *      ELXPMCBC
01562 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01563 *  IF MAJOR MEDICAL CONTRACT                               *      ELXPMCBC
01564 ************************************************************      ELXPMCBC
01565  9235-RCMPT-INST-INP-MM.                                          ELXPMCBC
01566                                                                   ELXPMCBC
01567 * PROCESS MAJOR MEDICAL CALCULATIONS.                             ELXPMCBC
01568      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01569                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01570                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01571                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBC
01572                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBC
01573                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01574                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01575      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01576                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBC
01577         CONTINUE                                                  ELXPMCBC
01578      ELSE                                                         ELXPMCBC
01579         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01580                      WS-WT-BNFT-PRD                               ELXPMCBC
01581         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01582                      WS-WT-INDVDL                                 ELXPMCBC
01583         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBC
01584                      WS-WT-INST-SUP                               ELXPMCBC
01585         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBC
01586                      WS-WT-IP                                     ELXPMCBC
01587         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01588                      WS-WT-PLAN                                   ELXPMCBC
01589         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01590                      WS-WT-SP                                     ELXPMCBC
01591         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01592      END-IF.                                                      ELXPMCBC
01593                                                                   ELXPMCBC
01594      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01595         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBC
01596         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBC
01597            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBC
01598            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01599                                    WS-TEST-CONF-MM.               ELXPMCBC
01600                                                                   ELXPMCBC
01601 ************************************************************      ELXPMCBC
01602 *                                                          *      ELXPMCBC
01603 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01604 *                                                          *      ELXPMCBC
01605 ************************************************************      ELXPMCBC
01606  9240-RCMPT-INST-OUT.                                             ELXPMCBC
01607                                                                   ELXPMCBC
01608      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01609                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01610                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01611                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBC
01612                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBC
01613                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01614                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01615      MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-CF-WORK-ENTRY       ELXPMCBC
01616      MOVE ATBL-CF-INDVDL (ATBL-IDX)   TO WS-CF-INDVDL             ELXPMCBC
01617      MOVE ATBL-CF-INST-BAS (ATBL-IDX) TO WS-CF-INST-BAS           ELXPMCBC
01618      MOVE ATBL-CF-OP (ATBL-IDX)       TO WS-CF-OP                 ELXPMCBC
01619      MOVE ATBL-CF-PLAN (ATBL-IDX)     TO WS-CF-PLAN               ELXPMCBC
01620      MOVE ATBL-CF-SP (ATBL-IDX)       TO WS-CF-SP                 ELXPMCBC
01621      MOVE WS-CF-WORK-ENTRY  TO                                    ELXPMCBC
01622           ATBL-CF-WORK-ENTRY (ATBL-IDX)                           ELXPMCBC
01623      IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) = 'EMERGENCY'         ELXPMCBC
01624         AND                                                       ELXPMCBC
01625         ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'                   ELXPMCBC
01626         AND                                                       ELXPMCBC
01627         PMCI-BLUE-STORM-CALL                                      ELXPMCBC
01628            CONTINUE                                               ELXPMCBC
01629 *          MOVE WS-CF-TRUE TO ATBL-CF-WORK-ENTRY (ATBL-IDX)       ELXPMCBC
01630      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01631                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBC
01632         CONTINUE                                                  ELXPMCBC
01633         ELSE                                                      ELXPMCBC
01634         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01635                      WS-WT-BNFT-PRD                               ELXPMCBC
01636         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01637                      WS-WT-INDVDL                                 ELXPMCBC
01638         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBC
01639                      WS-WT-INST-BAS                               ELXPMCBC
01640         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBC
01641                      WS-WT-OP                                     ELXPMCBC
01642         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01643                       WS-WT-PLAN                                  ELXPMCBC
01644         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01645                      WS-WT-SP                                     ELXPMCBC
01646         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01647         END-IF.                                                   ELXPMCBC
01648 *ADDED BELOW FOR BLUESTORM AND FOR POSSIBLY FIX FOR PMCI          ELXPMCBC
01649                                                                   ELXPMCBC
01650      IF (ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT > WS-TEST-THRESHOLD    ELXPMCBC
01651      OR ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT = WS-TEST-THRESHOLD)    ELXPMCBC
01652          IF ATBL-BENEFIT-PERIOD (ATBL-IDX) = '0I' OR '0Y' OR      ELXPMCBC
01653                                        '0A' OR 'CA' OR 'CB'       ELXPMCBC
01654               PERFORM 9232-CHECK-REMAINING                        ELXPMCBC
01655      ELSE                                                         ELXPMCBC
01656      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01657         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBC
01658         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBC
01659            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01660            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01661                                    WS-TEST-CONF-BSC.              ELXPMCBC
01662                                                                   ELXPMCBC
01663                                                                   ELXPMCBC
01664      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBC
01665         PERFORM 9245-RCMPT-INST-OUT-MM.                           ELXPMCBC
01666 ************************************************************      ELXPMCBC
01667 *    THIS WILL HELP LOOK OVER                              *      ELXPMCBC
01668 *                                                          *      ELXPMCBC
01669 *                                                          *      ELXPMCBC
01670 ************************************************************      ELXPMCBC
01671  9232-CHECK-REMAINING.                                            ELXPMCBC
01672      IF  ATBL-PLACE-OF-TREATMENT (ATBL-IDX) = '0A' OR             ELXPMCBC
01673                                   '0Q' OR '0M' OR '0S'            ELXPMCBC
01674       IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) = 'EMERGENCY'        ELXPMCBC
01675             AND PROCESSING-EMER                                   ELXPMCBC
01676         IF (PMCI-PRG-MCNP-APPLIES OR PMCI-POS-APPLIES)            ELXPMCBC
01677          AND (ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00' AND         ELXPMCBC
01678             (PMCI-REFERRAL-EXISTS OR PMCI-REFERRAL-NOT-REQUIRED)) ELXPMCBC
01679           ADD +1 TO WS-APPL-ENTRS-BSC                             ELXPMCBC
01680          SET WS-SAVE-SUB-BSC TO ATBL-IDX                          ELXPMCBC
01681         ELSE                                                      ELXPMCBC
01682 *       IF (PMCI-PRG-MCNP-APPLIES                                 ELXPMCBC
01683          IF   ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9' AND         ELXPMCBC
01684            (PMCI-NO-REFERRAL OR PMCI-REFERRAL-INDICATOR =         ELXPMCBC
01685             SPACES)                                               ELXPMCBC
01686           ADD +1 TO WS-APPL-ENTRS-BSC                             ELXPMCBC
01687          SET WS-SAVE-SUB-BSC TO ATBL-IDX                          ELXPMCBC
01688         ELSE                                                      ELXPMCBC
01689           IF (ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94' AND         ELXPMCBC
01690             PMCI-NON-PPO-PROVIDER)                                ELXPMCBC
01691             ADD +1 TO WS-APPL-ENTRS-BSC                           ELXPMCBC
01692            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01693           ELSE                                                    ELXPMCBC
01694           IF (ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4' AND         ELXPMCBC
01695             PMCI-NON-PPO-PROVIDER)                                ELXPMCBC
01696             ADD +1 TO WS-APPL-ENTRS-BSC                           ELXPMCBC
01697            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01698           ELSE                                                    ELXPMCBC
01699             IF (ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00' AND       ELXPMCBC
01700               PMCI-PPO-PROVIDER) AND (NOT PMCI-PRG-MCNP-APPLIES   ELXPMCBC
01701               AND NOT PMCI-POS-APPLIES)                           ELXPMCBC
01702               ADD +1 TO WS-APPL-ENTRS-BSC                         ELXPMCBC
01703              SET WS-SAVE-SUB-BSC TO ATBL-IDX.                     ELXPMCBC
01704 ************************************************************      ELXPMCBC
01705 *                                                          *      ELXPMCBC
01706 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01707 *  MAJOR MEDICAL CONTRACTS ONLY.                           *      ELXPMCBC
01708 ************************************************************      ELXPMCBC
01709  9245-RCMPT-INST-OUT-MM.                                          ELXPMCBC
01710                                                                   ELXPMCBC
01711                                                                   ELXPMCBC
01712 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBC
01713      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01714                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01715                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01716                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBC
01717                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBC
01718                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01719                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01720      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01721                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBC
01722         CONTINUE                                                  ELXPMCBC
01723         ELSE                                                      ELXPMCBC
01724         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01725                      WS-WT-BNFT-PRD                               ELXPMCBC
01726         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01727                      WS-WT-INDVDL                                 ELXPMCBC
01728         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBC
01729                      WS-WT-INST-SUP                               ELXPMCBC
01730         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBC
01731                      WS-WT-OP                                     ELXPMCBC
01732         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01733                       WS-WT-PLAN                                  ELXPMCBC
01734         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01735                      WS-WT-SP                                     ELXPMCBC
01736         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01737      END-IF.                                                      ELXPMCBC
01738                                                                   ELXPMCBC
01739      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01740         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBC
01741         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBC
01742            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBC
01743            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01744                                    WS-TEST-CONF-MM.               ELXPMCBC
01745                                                                   ELXPMCBC
01746 ************************************************************      ELXPMCBC
01747 *                                                          *      ELXPMCBC
01748 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01749 *                                                          *      ELXPMCBC
01750 ************************************************************      ELXPMCBC
01751  9250-RCMPT-PROF-INP.                                             ELXPMCBC
01752                                                                   ELXPMCBC
01753      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01754                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01755                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01756                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBC
01757                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBC
01758                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01759                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01760      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01761                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBC
01762         CONTINUE                                                  ELXPMCBC
01763      ELSE                                                         ELXPMCBC
01764         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01765                      WS-WT-BNFT-PRD                               ELXPMCBC
01766          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBC
01767                      WS-WT-INDVDL                                 ELXPMCBC
01768          COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *          ELXPMCBC
01769                      WS-WT-PROF-BAS                               ELXPMCBC
01770          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBC
01771                      WS-WT-IP                                     ELXPMCBC
01772          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBC
01773                      WS-WT-PLAN                                   ELXPMCBC
01774          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBC
01775                      WS-WT-SP                                     ELXPMCBC
01776          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBC
01777      END-IF.                                                      ELXPMCBC
01778                                                                   ELXPMCBC
01779      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01780         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBC
01781         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBC
01782            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01783            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01784                                    WS-TEST-CONF-BSC.              ELXPMCBC
01785                                                                   ELXPMCBC
01786      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBC
01787         PERFORM 9255-RCMPT-PROF-INP-MM.                           ELXPMCBC
01788                                                                   ELXPMCBC
01789 ************************************************************      ELXPMCBC
01790 *                                                          *      ELXPMCBC
01791 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01792 *   MAJOR MEDICAL CONTRACTS ONLY                           *      ELXPMCBC
01793 ************************************************************      ELXPMCBC
01794  9255-RCMPT-PROF-INP-MM.                                          ELXPMCBC
01795                                                                   ELXPMCBC
01796 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBC
01797                                                                   ELXPMCBC
01798      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01799                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01800                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01801                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBC
01802                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBC
01803                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01804                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01805      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01806                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBC
01807         CONTINUE                                                  ELXPMCBC
01808      ELSE                                                         ELXPMCBC
01809         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01810                      WS-WT-BNFT-PRD                               ELXPMCBC
01811          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBC
01812                      WS-WT-INDVDL                                 ELXPMCBC
01813          COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *          ELXPMCBC
01814                      WS-WT-PROF-SUP                               ELXPMCBC
01815          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBC
01816                      WS-WT-IP                                     ELXPMCBC
01817          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBC
01818                      WS-WT-PLAN                                   ELXPMCBC
01819          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBC
01820                      WS-WT-SP                                     ELXPMCBC
01821          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBC
01822      END-IF.                                                      ELXPMCBC
01823                                                                   ELXPMCBC
01824      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01825         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBC
01826         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBC
01827            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBC
01828            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01829                                    WS-TEST-CONF-MM.               ELXPMCBC
01830                                                                   ELXPMCBC
01831 ************************************************************      ELXPMCBC
01832 *                                                          *      ELXPMCBC
01833 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01834 *                                                          *      ELXPMCBC
01835 ************************************************************      ELXPMCBC
01836  9260-RCMPT-PROF-OUT.                                             ELXPMCBC
01837                                                                   ELXPMCBC
01838      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01839                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01840                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01841                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBC
01842                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBC
01843                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01844                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01845      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01846                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBC
01847         CONTINUE                                                  ELXPMCBC
01848      ELSE                                                         ELXPMCBC
01849         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01850                      WS-WT-BNFT-PRD                               ELXPMCBC
01851         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01852                      WS-WT-INDVDL                                 ELXPMCBC
01853         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCBC
01854                      WS-WT-PROF-BAS                               ELXPMCBC
01855         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBC
01856                      WS-WT-OP                                     ELXPMCBC
01857         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01858                      WS-WT-PLAN                                   ELXPMCBC
01859         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01860                      WS-WT-SP                                     ELXPMCBC
01861         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01862      END-IF.                                                      ELXPMCBC
01863                                                                   ELXPMCBC
01864                                                                   ELXPMCBC
01865      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT >  WS-TEST-THRESHOLD    ELXPMCBC
01866      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) NOT = WS-TEST-THRESHOLD     ELXPMCBC
01867         IF (ATBL-BENEFIT-PERIOD (ATBL-IDX)                        ELXPMCBC
01868                             = '0I' OR '0Y' OR 'CA' OR 'CB')       ELXPMCBC
01869          AND (ATBL-PLACE-OF-TREATMENT (ATBL-IDX) = '0A' OR        ELXPMCBC
01870                                    '0R' OR '0M' OR '0Q')          ELXPMCBC
01871          IF  (ATBL-INTERNAL-DESCRIPTOR  (ATBL-IDX) =              ELXPMCBC
01872             'OFFICEVIS') AND PROCESSING-OVS                       ELXPMCBC
01873             PERFORM 9262-REMAINING-CHECKS.                        ELXPMCBC
01874                                                                   ELXPMCBC
01875      IF  (ATBL-INTERNAL-DESCRIPTOR  (ATBL-IDX) =                  ELXPMCBC
01876         'OFFICEVIS') AND PROCESSING-OVS                           ELXPMCBC
01877         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD     ELXPMCBC
01878           IF (ATBL-BENEFIT-PERIOD (ATBL-IDX)                      ELXPMCBC
01879                             = '0I' OR '0Y' OR 'CA' OR 'CB')       ELXPMCBC
01880            AND (ATBL-PLACE-OF-TREATMENT (ATBL-IDX) = '0A' OR      ELXPMCBC
01881                                      '0R' OR '0M' OR '0Q')        ELXPMCBC
01882              PERFORM 9262-REMAINING-CHECKS.                       ELXPMCBC
01883                                                                   ELXPMCBC
01884      IF  (ATBL-INTERNAL-DESCRIPTOR  (ATBL-IDX) NOT =              ELXPMCBC
01885         'OFFICEVIS') AND NOT PROCESSING-OVS                       ELXPMCBC
01886      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01887         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBC
01888         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBC
01889            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01890            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01891                                    WS-TEST-CONF-BSC.              ELXPMCBC
01892         PERFORM 9265-RCMPT-PROF-OUT-MM.                           ELXPMCBC
01893                                                                   ELXPMCBC
01894 ************************************************************      ELXPMCBC
01895 *                                                          *      ELXPMCBC
01896 *                                                          *      ELXPMCBC
01897 *                                                          *      ELXPMCBC
01898 ************************************************************      ELXPMCBC
01899  9262-REMAINING-CHECKS.                                           ELXPMCBC
01900      IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94' OR                ELXPMCBC
01901         (ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4')                 ELXPMCBC
01902        AND PMCI-NON-PPO-PROVIDER                                  ELXPMCBC
01903         IF WS-APPL-ENTRS-BSC = 0                                  ELXPMCBC
01904            ADD +1 TO WS-APPL-ENTRS-BSC                            ELXPMCBC
01905            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01906         ELSE                                                      ELXPMCBC
01907            CONTINUE                                               ELXPMCBC
01908      ELSE                                                         ELXPMCBC
01909       IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'                  ELXPMCBC
01910        AND (PMCI-PPO-PROVIDER AND                                 ELXPMCBC
01911              PMCI-NO-REFERRAL) AND                                ELXPMCBC
01912           (PMCI-POS-APPLIES OR PMCI-MCNP-APPLIES)                 ELXPMCBC
01913         IF WS-APPL-ENTRS-BSC = 0                                  ELXPMCBC
01914            ADD +1 TO WS-APPL-ENTRS-BSC                            ELXPMCBC
01915            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01916         ELSE                                                      ELXPMCBC
01917            CONTINUE                                               ELXPMCBC
01918      ELSE                                                         ELXPMCBC
01919      IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'                   ELXPMCBC
01920        AND (PMCI-PPO-PROVIDER AND                                 ELXPMCBC
01921             (PMCI-REFERRAL-EXISTS                                 ELXPMCBC
01922             OR PMCI-REFERRAL-NOT-REQUIRED)) AND                   ELXPMCBC
01923              (PMCI-PRG-MCNP-APPLIES OR PMCI-POS-APPLIES)          ELXPMCBC
01924         IF WS-APPL-ENTRS-BSC = 0                                  ELXPMCBC
01925            ADD +1 TO WS-APPL-ENTRS-BSC                            ELXPMCBC
01926            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01927         ELSE                                                      ELXPMCBC
01928            CONTINUE                                               ELXPMCBC
01929      ELSE                                                         ELXPMCBC
01930      IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'                   ELXPMCBC
01931        AND (PMCI-PPO-PROVIDER AND                                 ELXPMCBC
01932              PMCI-PRG-PPO-APPLIES OR PMCI-PRG-BAE-APPLIES)        ELXPMCBC
01933         IF WS-APPL-ENTRS-BSC = 0                                  ELXPMCBC
01934            ADD +1 TO WS-APPL-ENTRS-BSC                            ELXPMCBC
01935            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBC
01936         ELSE                                                      ELXPMCBC
01937            CONTINUE.                                              ELXPMCBC
01938 ************************************************************      ELXPMCBC
01939 *                                                          *      ELXPMCBC
01940 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBC
01941 *  MAJOR MEDICAL CONTRACTS ONLY                            *      ELXPMCBC
01942 ************************************************************      ELXPMCBC
01943  9265-RCMPT-PROF-OUT-MM.                                          ELXPMCBC
01944                                                                   ELXPMCBC
01945 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBC
01946                                                                   ELXPMCBC
01947      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01948                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCBC
01949                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBC
01950                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBC
01951                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBC
01952                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBC
01953                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBC
01954      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBC
01955                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBC
01956         CONTINUE                                                  ELXPMCBC
01957      ELSE                                                         ELXPMCBC
01958         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCBC
01959                      WS-WT-BNFT-PRD                               ELXPMCBC
01960         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBC
01961                      WS-WT-INDVDL                                 ELXPMCBC
01962         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCBC
01963                      WS-WT-PROF-SUP                               ELXPMCBC
01964         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBC
01965                      WS-WT-OP                                     ELXPMCBC
01966         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBC
01967                      WS-WT-PLAN                                   ELXPMCBC
01968         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBC
01969                      WS-WT-SP                                     ELXPMCBC
01970         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBC
01971      END-IF.                                                      ELXPMCBC
01972                                                                   ELXPMCBC
01973      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBC
01974         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBC
01975         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBC
01976            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBC
01977            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBC
01978                                    WS-TEST-CONF-MM.               ELXPMCBC
01979 ************************************************************      ELXPMCBC
01980 *                                                          *      ELXPMCBC
01981 *  COMBINE FACTORS                                         *      ELXPMCBC
01982 *                                                          *      ELXPMCBC
01983 ************************************************************      ELXPMCBC
01984  9300-COMBINE-FACTORS.                                            ELXPMCBC
01985                                                                   ELXPMCBC
01986      CALL 'ELKFLCMB'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBC
01987                            WS-CF-1                                ELXPMCBC
01988                            WS-CF-2                                ELXPMCBC
01989                            WS-CF-3                                ELXPMCBC
01990                            WS-CF-4                                ELXPMCBC
01991                            WS-CF-5                                ELXPMCBC
01992                            WS-CF-6.                               ELXPMCBC
01993                                                                   ELXPMCBC
01994 ************************************************************      ELXPMCBC
01995 *                                                          *      ELXPMCBC
01996 *  FIND BENEFIT PERIOD CONFIDENCE FACTOR TABLE ENTRY       *      ELXPMCBC
01997 *                                                          *      ELXPMCBC
01998 ************************************************************      ELXPMCBC
01999                                                                   ELXPMCBC
02000  9400-FND-BNFT-PRD-CF.                                            ELXPMCBC
02001                                                                   ELXPMCBC
02002      SET WS-BNFTPRD-NOT-FOUND TO TRUE.                            ELXPMCBC
02003      SET CFTA-MAX-IDX TO CFTA-NBR-ENTRS.                          ELXPMCBC
02004      PERFORM VARYING CFTA-IDX FROM 1 BY 1                         ELXPMCBC
02005         UNTIL CFTA-IDX > CFTA-MAX-IDX OR                          ELXPMCBC
02006           WS-BNFTPRD-FOUND                                        ELXPMCBC
02007         IF ATBL-BENEFIT-PERIOD (ATBL-IDX) =                       ELXPMCBC
02008                        CFTA-BNFT-PRD (CFTA-IDX)                   ELXPMCBC
02009            SET WS-BNFTPRD-FOUND TO TRUE                           ELXPMCBC
02010            MOVE CFTA-PMCI-BNFT-PRD (CFTA-IDX) TO WS-BNF-QUAL      ELXPMCBC
02011            SET WS-SUB-WORK TO CFTA-IDX                            ELXPMCBC
02012            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCBC
02013            SET CFTA-IDX TO WS-SUB-WORK                            ELXPMCBC
02014         END-IF                                                    ELXPMCBC
02015         END-PERFORM.                                              ELXPMCBC
02016      IF WS-BNFTPRD-NOT-FOUND                                      ELXPMCBC
02017         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBC
02018         MOVE +4703 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBC
02019 ************************************************************      ELXPMCBC
02020 *                                                          *      ELXPMCBC
02021 *  DETERMINE IF BASIC OR MAJOR MEDICAL BENEFIT APPLIES     *      ELXPMCBC
02022 *                                                          *      ELXPMCBC
02023 ************************************************************      ELXPMCBC
02024                                                                   ELXPMCBC
02025  9500-DETERMINE-BASIC-MM.                                         ELXPMCBC
02026                                                                   ELXPMCBC
02027      MOVE SPACES TO WS-SAVE-LOB-IND.                              ELXPMCBC
02028      IF WS-SAVE-SUB-BSC NOT EQUAL ZERO AND WS-SAVE-SUB-MM         ELXPMCBC
02029               NOT EQUAL ZERO                                      ELXPMCBC
02030         MOVE '+' TO WS-SAVE-LOB-IND                               ELXPMCBC
02031         MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                       ELXPMCBC
02032         MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS               ELXPMCBC
02033      ELSE                                                         ELXPMCBC
02034         IF WS-SAVE-SUB-MM NOT EQUAL ZERO                          ELXPMCBC
02035            MOVE '*' TO WS-SAVE-LOB-IND                            ELXPMCBC
02036            MOVE WS-SAVE-SUB-MM TO WS-SAVE-SUB                     ELXPMCBC
02037            MOVE WS-APPL-ENTRS-MM TO WS-NUM-APPL-ENTRS             ELXPMCBC
02038         ELSE                                                      ELXPMCBC
02039            MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                    ELXPMCBC
02040            MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS.           ELXPMCBC
02041 ************************************************************      ELXPMCBC
02042 *                                                          *      ELXPMCBC
02043 *  COMPUTE COPAY PERCENTAGE AND TEST PROVIDER TYPES        *      ELXPMCBC
02044 *                                                          *      ELXPMCBC
02045 ************************************************************      ELXPMCBC
02046                                                                   ELXPMCBC
02047  9600-DETERMINE-VALUE.                                            ELXPMCBC
02048                                                                   ELXPMCBC
02049      IF PMCI-PRV-CALL                                             ELXPMCBC
02050         IF WS-PRG-VAR-FOUND                                       ELXPMCBC
02051            MOVE 'C' TO WS-BNF-QUAL                                ELXPMCBC
02052         ELSE                                                      ELXPMCBC
02053            MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO WS-COPAY-VALUE     ELXPMCBC
02054      ELSE                                                         ELXPMCBC
02055         MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO WS-COPAY-VALUE.       ELXPMCBC
02056                                                                   ELXPMCBC
02057 ************************************************************      ELXPMCBC
02058 *                                                          *      ELXPMCBC
02059 *  LOAD ACCUM CDE TABLE AFTER ACP OCCURENCE IS CHOSEN      *      ELXPMCBC
02060 *                                                          *      ELXPMCBC
02061 ************************************************************      ELXPMCBC
02062                                                                   ELXPMCBC
02063  9700-LOAD-ACCUM-CDE-ATBL-TABLE.                                  ELXPMCBC
02064                                                                   ELXPMCBC
02065      IF ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE = NULL            ELXPMCBC
02066         MOVE +4710 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBC
02067         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBC
02068      ELSE                                                         ELXPMCBC
02069                                                                   ELXPMCBC
02070         ADD 1 TO AC-ATBL-TBL-CNT                                  ELXPMCBC
02071         SET AC-ATBL-IDX TO AC-ATBL-TBL-CNT                        ELXPMCBC
02072                                                                   ELXPMCBC
02073         MOVE ATBL-ACCUM-DESC (ATBL-IDX) TO                        ELXPMCBC
02074           AC-ATBL-ACCUM-DESC (AC-ATBL-IDX)                        ELXPMCBC
02075                                                                   ELXPMCBC
02076         IF ATBL-PSEU-NBR-USING-IND (ATBL-IDX) = 'Y'               ELXPMCBC
02077             MOVE ATBL-PSEUDO-GRP-NBR (ATBL-IDX) TO                ELXPMCBC
02078                AC-ATBL-PSEUDO-GRP-NO  (AC-ATBL-IDX)               ELXPMCBC
02079             MOVE ATBL-PSEUDO-SECT-NBR (ATBL-IDX) TO               ELXPMCBC
02080                AC-ATBL-PSEUDO-SEC-NO  (AC-ATBL-IDX)               ELXPMCBC
02081           ELSE                                                    ELXPMCBC
02082             MOVE ATBL-GRP-NBR (ATBL-IDX) TO                       ELXPMCBC
02083                AC-ATBL-PSEUDO-GRP-NO (AC-ATBL-IDX)                ELXPMCBC
02084           MOVE ATBL-SECT-NBR (ATBL-IDX) TO                        ELXPMCBC
02085                AC-ATBL-PSEUDO-SEC-NO (AC-ATBL-IDX)                ELXPMCBC
02086         END-IF                                                    ELXPMCBC
02087                                                                   ELXPMCBC
02088         MOVE ATBL-CON-FEAK-IND (ATBL-IDX)  TO                     ELXPMCBC
02089           AC-ATBL-CON-FEAK-IND (AC-ATBL-IDX)                      ELXPMCBC
02090         MOVE ATBL-CON-BGN-DT-MMDD (ATBL-IDX) TO                   ELXPMCBC
02091           AC-ATBL-CON-BGN-DT-MMDD (AC-ATBL-IDX)                   ELXPMCBC
02092                                                                   ELXPMCBC
02093         MOVE ATBL-BENEFIT-PERIOD (ATBL-IDX)  TO                   ELXPMCBC
02094           AC-ATBL-BENEFIT-PERIOD (AC-ATBL-IDX)                    ELXPMCBC
02095         MOVE ATBL-FAM-OR-INDIV (ATBL-IDX)  TO                     ELXPMCBC
02096           AC-ATBL-FAM-OR-INDIV (AC-ATBL-IDX)                      ELXPMCBC
02097         MOVE ATBL-L-O-B (ATBL-IDX)  TO                            ELXPMCBC
02098           AC-ATBL-L-O-B (AC-ATBL-IDX)                             ELXPMCBC
02099         MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX)  TO              ELXPMCBC
02100           AC-ATBL-INTERNAL-DESC (AC-ATBL-IDX)                     ELXPMCBC
02101         MOVE ATBL-SERVICE-GROUP (ATBL-IDX)  TO                    ELXPMCBC
02102           AC-ATBL-SERVICE-GROUP (AC-ATBL-IDX)                     ELXPMCBC
02103         MOVE ATBL-PLACE-OF-TREATMENT (ATBL-IDX)  TO               ELXPMCBC
02104           AC-ATBL-P-O-T (AC-ATBL-IDX)                             ELXPMCBC
02105         MOVE ATBL-CONDITION (ATBL-IDX)  TO                        ELXPMCBC
02106           AC-ATBL-CONDITION (AC-ATBL-IDX)                         ELXPMCBC
02107         MOVE ATBL-CO-PAY-IND (ATBL-IDX) TO                        ELXPMCBC
02108           AC-ATBL-CO-PAY-IND (AC-ATBL-IDX)                        ELXPMCBC
02109         MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                  ELXPMCBC
02110           AC-ATBL-COST-CONT-IND (AC-ATBL-IDX)                     ELXPMCBC
02111         MOVE ATBL-AGE-LIMIT-FROM (ATBL-IDX) TO                    ELXPMCBC
02112           AC-ATBL-AGE-LIMIT-FROM (AC-ATBL-IDX)                    ELXPMCBC
02113         MOVE ATBL-AGE-LIMIT-TO (ATBL-IDX) TO                      ELXPMCBC
02114           AC-ATBL-AGE-LIMIT-TO (AC-ATBL-IDX)                      ELXPMCBC
02115                                                                   ELXPMCBC
02116 **THE FOLLOWING FIELDS APPLY ONLY TO SPECIFIC ACCUMS              ELXPMCBC
02117                                                                   ELXPMCBC
02118         IF ATBL-ACCUM-DESC (ATBL-IDX) =                           ELXPMCBC
02119                         '#ACL  ' OR '#ADL  ' OR '#ACP  '          ELXPMCBC
02120              MOVE ATBL-MANDATORY-IND (ATBL-IDX)  TO               ELXPMCBC
02121                AC-ATBL-MANDATORY-IND (AC-ATBL-IDX)                ELXPMCBC
02122         ELSE                                                      ELXPMCBC
02123              MOVE SPACES TO                                       ELXPMCBC
02124                AC-ATBL-MANDATORY-IND (AC-ATBL-IDX)                ELXPMCBC
02125         END-IF                                                    ELXPMCBC
02126                                                                   ELXPMCBC
02127         IF ATBL-ACCUM-DESC (ATBL-IDX) = '#ACL  '                  ELXPMCBC
02128              MOVE ATBL-BISCEND-IND (ATBL-IDX)  TO                 ELXPMCBC
02129                AC-ATBL-BISCENDING-IND (AC-ATBL-IDX)               ELXPMCBC
02130         ELSE                                                      ELXPMCBC
02131              MOVE SPACES TO                                       ELXPMCBC
02132                AC-ATBL-BISCENDING-IND (AC-ATBL-IDX)               ELXPMCBC
02133         END-IF                                                    ELXPMCBC
02134                                                                   ELXPMCBC
02135         IF ATBL-ACCUM-DESC (ATBL-IDX) = '#ACL  ' OR '#AOL  '      ELXPMCBC
02136            MOVE ATBL-PERCENT-LEVEL (ATBL-IDX) TO                  ELXPMCBC
02137                AC-ATBL-PERCENT-LEVEL (AC-ATBL-IDX)                ELXPMCBC
02138         ELSE                                                      ELXPMCBC
02139            MOVE ZEROES TO                                         ELXPMCBC
02140                AC-ATBL-PERCENT-LEVEL (AC-ATBL-IDX)                ELXPMCBC
02141         END-IF                                                    ELXPMCBC
02142                                                                   ELXPMCBC
02143         MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO                   ELXPMCBC
02144           AC-ATBL-VALUE-QUALIFIER (AC-ATBL-IDX)                   ELXPMCBC
02145                                                                   ELXPMCBC
02146      END-IF.                                                      ELXPMCBC
02147                                                                   ELXPMCBC
