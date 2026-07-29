00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCBI
00003  PROGRAM-ID.         ELXPMCBI                                        LV004
00004                                                                   ELXPMCBI
00005  AUTHOR.             BARBARA KEIB                                 ELXPMCBI
00006                                                                   ELXPMCBI
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCBI
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCBI
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCBI
00010                      233 N. MICHIGAN AVE                          ELXPMCBI
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCBI
00012                                                                   ELXPMCBI
00013                                                                   ELXPMCBI
00014  DATE-WRITTEN.       13-OCT-1993.                                 ELXPMCBI
00015                                                                   ELXPMCBI
00016  DATE-COMPILED.                                                   ELXPMCBI
00017                                                                   ELXPMCBI
00018  SECURITY.           COPYRIGHT 1993,                              ELXPMCBI
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCBI
00020      SKIP3                                                        ELXPMCBI
00021  ENVIRONMENT DIVISION.                                            ELXPMCBI
00022                                                                   ELXPMCBI
00023  CONFIGURATION SECTION.                                           ELXPMCBI
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCBI
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCBI
00026      EJECT                                                        ELXPMCBI
00027 ******************************************************************ELXPMCBI
00028 *                                                                *ELXPMCBI
00029 *                                                                *ELXPMCBI
00030 *   OTHER COINSURANCE BENEFITS                                   *ELXPMCBI
00031 *                                                                *ELXPMCBI
00032 *    *******THIS PROGRAM MUST BE BATCH COMPILED**********        *ELXPMCBI
00033 *                                                                *ELXPMCBI
00034 ******************************************************************ELXPMCBI
00035 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCBI
00036 *                      MAINTENANCE HISTORY                       *ELXPMCBI
00037 *                                                                *ELXPMCBI
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCBI
00039 * ----- ----------- --- ----- ---------------------------------- *ELXPMCBI
00040 * 01.00 13-OCT-1993 BAK       CREATED                            *ELXPMCBI
00041 *       3/13/95  RGO    CPO PROJECT. CHANGED PARAGRAPH 0150- TO  *ELXPMCBI
00042 *                                    HANDLE CPO.                 *ELXPMCBI
00043 *                                                                *ELXPMCBI
00044 *                                                                *ELXPMCBI
00045 * RGO  10/20/95  FIX PROD ABEND DUE TO NO IBGR-INTERNAL-TABLE-TAB*ELXPMCBI
00046 *                SEE 0100-EXTRACT PARAGRAPH                      *ELXPMCBI
00047 *                                                                *ELXPMCBI
00048 * RGO   4/03/96         CBL PROJECT. CHANGED PARAGRAPH 0150- TO  *ELXPMCBI
00049 *                                    HANDLE COMMUNITY BLUE.      *ELXPMCBI
00050 *                                                                *ELXPMCBI
00051 * AKK   9/07/00  AKK    ADDE SUPPORT FOR BAE.                    *ELXPMCBI
00052 *                                                                *ELXPMCBI
00053 * AKK   3/12/03  AKK    RECOMPLE WITH PMCCOMM UPDATED VIA PROV   *ELXPMCBI
00054 *                       AREA                                     *ELXPMCBI
00055 * AKK   0/01/03         MORE CHANGES IN ENDEVOR                   ELXPMCBI
00056 *                                                                *ELXPMCBI
00057 * 05.00 01-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCBI
00058 *                                                                *ELXPMCBI
00059 * 05.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCBI
00060 *                                                                *ELXPMCBI
00061 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCBI
00062 ******************************************************************ELXPMCBI
00063                                                                   ELXPMCBI
00064      EJECT                                                        ELXPMCBI
00065  DATA DIVISION.                                                   ELXPMCBI
00066  WORKING-STORAGE SECTION.                                         ELXPMCBI
00067  01  FILLER                     PICTURE X(32)                     ELXPMCBI
00068           VALUE '****ELXPMCBI WORKING STORAGE****'.               ELXPMCBI
00069                                                                   ELXPMCBI
00070  01  WS-RETURN-CODE               PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00071                                                                   ELXPMCBI
00072      88  WS-SUCCESSFUL-CALL               VALUE ZERO.             ELXPMCBI
00073      88  WS-UNIDENT-PARM                  VALUE +8.               ELXPMCBI
00074      88  WS-MISSING-PARM                  VALUE +12.              ELXPMCBI
00075      88  WS-INTERNAL-ERROR                VALUE +16.              ELXPMCBI
00076                                                                   ELXPMCBI
00077  01  WS-SAVE-SUBSCRIPTS.                                          ELXPMCBI
00078                                                                   ELXPMCBI
00079      05  WS-SAVE-SUB              PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00080      05  WS-SAVE-SUB-BSC          PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00081      05  WS-SAVE-SUB-MM           PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00082      05  WS-OTR-DAY-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00083      05  WS-SUB-WORK              PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00084                                                                   ELXPMCBI
00085                                                                   ELXPMCBI
00086  01  WS-NUM-APPL-ENTRS            PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00087  01  WS-APPL-ENTRS-BSC            PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00088  01  WS-APPL-ENTRS-MM             PIC S9(04) COMP VALUE ZERO.     ELXPMCBI
00089  01  WS-BNF-QUAL                  PIC X(01) VALUE SPACES.         ELXPMCBI
00090  01  WS-BNF-PERD                  PIC X(02) VALUE SPACES.         ELXPMCBI
00091  01  WS-SAVE-LOB-IND              PIC X(01) VALUE SPACES.         ELXPMCBI
00092                                                                   ELXPMCBI
00093                                                                   ELXPMCBI
00094  01  WS-SWITCHES.                                                 ELXPMCBI
00095                                                                   ELXPMCBI
00096    05  WS-PRG-VARIATION-CONTROL   PIC X(01) VALUE 'N'.            ELXPMCBI
00097      88  WS-PRG-VAR-FOUND                 VALUE 'Y'.              ELXPMCBI
00098      88  WS-PRG-VAR-NOT-FOUND             VALUE 'N'.              ELXPMCBI
00099                                                                   ELXPMCBI
00100    05  WS-IBGR-SLOT-CONTROL       PIC X(01) VALUE 'N'.            ELXPMCBI
00101      88  WS-IBGR-FOUND                    VALUE 'Y'.              ELXPMCBI
00102      88  WS-IBGR-NOT-FOUND                VALUE 'N'.              ELXPMCBI
00103                                                                   ELXPMCBI
00104    05  WS-BNFTPRD-CONTROL         PIC X(01) VALUE 'N'.            ELXPMCBI
00105      88  WS-BNFTPRD-FOUND                 VALUE 'Y'.              ELXPMCBI
00106      88  WS-BNFTPRD-NOT-FOUND             VALUE 'N'.              ELXPMCBI
00107                                                                   ELXPMCBI
00108    05  WS-INTRNLDSC-CONTORL       PIC X(01) VALUE 'N'.            ELXPMCBI
00109      88  WS-INTRNLDSC-FOUND               VALUE 'Y'.              ELXPMCBI
00110      88  WS-INTRNLDSC-NOT-FOUND           VALUE 'N'.              ELXPMCBI
00111                                                                   ELXPMCBI
00112    05  WS-MCNP-PENALTY-IND       PIC X(01)  VALUE 'N'.            ELXPMCBI
00113      88  WS-MCNP-PEN-FOUND               VALUE 'Y'.               ELXPMCBI
00114      88  WS-MCNP-PEN-NOT-FOUND           VALUE 'N'.               ELXPMCBI
00115                                                                   ELXPMCBI
00116    05  WS-MCNP-INCENT-IND       PIC X(01)  VALUE 'N'.             ELXPMCBI
00117      88  WS-MCNP-INC-FOUND               VALUE 'Y'.               ELXPMCBI
00118      88  WS-MCNP-INC-NOT-FOUND           VALUE 'N'.               ELXPMCBI
00119                                                                   ELXPMCBI
00120    05  WS-PPO-PENALTY-IND       PIC X(01)  VALUE 'N'.             ELXPMCBI
00121      88  WS-PPO-PEN-FOUND               VALUE 'Y'.                ELXPMCBI
00122      88  WS-PPO-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00123                                                                   ELXPMCBI
00124    05  WS-PPO-INCENT-IND       PIC X(01)  VALUE 'N'.              ELXPMCBI
00125      88  WS-PPO-INC-FOUND               VALUE 'Y'.                ELXPMCBI
00126      88  WS-PPO-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00127                                                                   ELXPMCBI
00128    05  WS-RPO-PENALTY-IND       PIC X(01)  VALUE 'N'.             ELXPMCBI
00129      88  WS-RPO-PEN-FOUND               VALUE 'Y'.                ELXPMCBI
00130      88  WS-RPO-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00131                                                                   ELXPMCBI
00132    05  WS-RPO-INCENT-IND       PIC X(01)  VALUE 'N'.              ELXPMCBI
00133      88  WS-RPO-INC-FOUND               VALUE 'Y'.                ELXPMCBI
00134      88  WS-RPO-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00135                                                                   ELXPMCBI
00136    05  WS-BAE-PENALTY-IND       PIC X(01)  VALUE 'N'.             ELXPMCBI
00137      88  WS-BAE-PEN-FOUND               VALUE 'Y'.                ELXPMCBI
00138      88  WS-BAE-PEN-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00139                                                                   ELXPMCBI
00140    05  WS-BAE-INCENT-IND       PIC X(01)  VALUE 'N'.              ELXPMCBI
00141      88  WS-BAE-INC-FOUND               VALUE 'Y'.                ELXPMCBI
00142      88  WS-BAE-INC-NOT-FOUND           VALUE 'N'.                ELXPMCBI
00143                                                                   ELXPMCBI
00144  01  WS-WEIGHTS.                                                  ELXPMCBI
00145      02  WS-WT-BNFT-PRD         COMP-1    VALUE 0.750000E+00.     ELXPMCBI
00146      02  WS-WT-INDVDL           COMP-1    VALUE 0.500000E+00.     ELXPMCBI
00147      02  WS-WT-INST-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00148      02  WS-WT-PROF-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00149      02  WS-WT-INST-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00150      02  WS-WT-PROF-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00151      02  WS-WT-IP               COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00152      02  WS-WT-OP               COMP-1    VALUE 0.100000E+00.     ELXPMCBI
00153      02  WS-WT-PLAN             COMP-1    VALUE 0.250000E+00.     ELXPMCBI
00154      02  WS-WT-SP               COMP-1    VALUE 0.500000E+00.     ELXPMCBI
00155                                                                   ELXPMCBI
00156  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCBI
00157      02  WS-CF-ZERO             COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00158      02  WS-CF-TRUE             COMP-1    VALUE +1.000000E+00.    ELXPMCBI
00159      02  WS-CF-FALSE            COMP-1    VALUE -1.000000E+00.    ELXPMCBI
00160      02  WS-CF-75               COMP-1    VALUE +0.750000E+00.    ELXPMCBI
00161      02  WS-CF-85               COMP-1    VALUE +0.850000E+00.    ELXPMCBI
00162      02  WS-CF-95               COMP-1    VALUE +0.950000E+00.    ELXPMCBI
00163      02  WS-TEST-CONF-FACT      COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00164      02  WS-TEST-CONF-BSC       COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00165      02  WS-TEST-CONF-MM        COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00166      02  WS-TEST-THRESHOLD      COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00167      02  WS-NO-IBGR-CF          COMP-1    VALUE +0.000000E+00.    ELXPMCBI
00168                                                                   ELXPMCBI
00169  01  WS-CONFIDENCE-WORK.                                          ELXPMCBI
00170      02  WS-CF-1                COMP-1.                           ELXPMCBI
00171      02  WS-CF-2                COMP-1.                           ELXPMCBI
00172      02  WS-CF-3                COMP-1.                           ELXPMCBI
00173      02  WS-CF-4                COMP-1.                           ELXPMCBI
00174      02  WS-CF-5                COMP-1.                           ELXPMCBI
00175      02  WS-CF-6                COMP-1.                           ELXPMCBI
00176                                                                   ELXPMCBI
00177 * INSTITUTIONAL INPATIENT                                         ELXPMCBI
00178                                                                   ELXPMCBI
00179                                                                   ELXPMCBI
00180  01  WS-INST-IP-PSYS.                                             ELXPMCBI
00181      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBI
00182      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCBI
00183      02  FILLER                 PIC X(06) VALUE 'PSYI W'.         ELXPMCBI
00184                                                                   ELXPMCBI
00185 * INSTITUTIONAL OUTPATIENT                                        ELXPMCBI
00186                                                                   ELXPMCBI
00187  01  WS-INST-OP-PSYS.                                             ELXPMCBI
00188      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBI
00189      02  FILLER                 PIC X(06) VALUE 'PSYO W'.         ELXPMCBI
00190                                                                   ELXPMCBI
00191 * PROFESSIONAL INPATIENT                                          ELXPMCBI
00192                                                                   ELXPMCBI
00193  01  WS-PROF-IP-PSYS.                                             ELXPMCBI
00194      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCBI
00195      02  FILLER                 PIC X(06) VALUE 'MNI  D'.         ELXPMCBI
00196                                                                   ELXPMCBI
00197 * PROFESSIONAL OUTPATIENT                                         ELXPMCBI
00198                                                                   ELXPMCBI
00199  01  WS-PROF-OP-PSYS.                                             ELXPMCBI
00200      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCBI
00201      02  FILLER                 PIC X(06) VALUE 'GPO  E'.         ELXPMCBI
00202      02  FILLER                 PIC X(06) VALUE 'IPO  E'.         ELXPMCBI
00203                                                                   ELXPMCBI
00204  01  WS-POINTERS.                                                 ELXPMCBI
00205      02  WS-PSYS-POINTER        POINTER.                          ELXPMCBI
00206                                                                   ELXPMCBI
00207  01  WS-SWITCHES.                                                 ELXPMCBI
00208      05                         PIC X(01).                        ELXPMCBI
00209         88  SW-TRMNL-ERR                  VALUE 'Y'.              ELXPMCBI
00210         88  SW-NO-TRMNL-ERR               VALUE 'N'.              ELXPMCBI
00211                                                                   ELXPMCBI
00212  COPY ELSCFTB5.                                                   ELXPMCBI
00213  COPY ELSCFTBA.                                                   ELXPMCBI
00214  COPY ELSCVG2C.                                                   ELXPMCBI
00215                                                                   ELXPMCBI
00216  01  FILLER                     PICTURE X(32)                     ELXPMCBI
00217           VALUE '*END ELXPMCBI WORKING STORAGE***'.               ELXPMCBI
00218      EJECT                                                        ELXPMCBI
00219  LINKAGE SECTION.                                                 ELXPMCBI
00220 *    EJECT                                                        ELXPMCBI
00221  COPY ELSCIA2C.                                                   ELXPMCBI
00222 *    EJECT                                                        ELXPMCBI
00223  COPY ELSCSACC.                                                   ELXPMCBI
00224 *    EJECT                                                        ELXPMCBI
00225  COPY ELSATBLC.                                                   ELXPMCBI
00226 *    EJECT                                                        ELXPMCBI
00227  COPY ELSPMCID.                                                   ELXPMCBI
00228 *    EJECT                                                        ELXPMCBI
00229  COPY ELSIBGRC.                                                   ELXPMCBI
00230 *    EJECT                                                        ELXPMCBI
00231  01  PMCI-COMM-AREA.                                              ELXPMCBI
00232  COPY PMCCOMM.                                                    ELXPMCBI
00233 *    EJECT                                                        ELXPMCBI
00234  COPY ELSBPVLC.                                                   ELXPMCBI
00235  01  LS-MATCH-LIST               PIC X.                           ELXPMCBI
00236 *    EJECT                                                        ELXPMCBI
00237  PROCEDURE DIVISION USING PMCI-COMM-AREA                          ELXPMCBI
00238                           NAES-INTERMEDIATE-DATA                  ELXPMCBI
00239                           CSAC-ACCUMULATOR-TABLE                  ELXPMCBI
00240                           IBGR-INTERNAL-TABS-TABLE.               ELXPMCBI
00241 ************************************************************      ELXPMCBI
00242 *                                                          *      ELXPMCBI
00243 *          MAINLINE ROUTINE                                *      ELXPMCBI
00244 *                                                          *      ELXPMCBI
00245 ************************************************************      ELXPMCBI
00246  0000-MAINLINE.                                                   ELXPMCBI
00247                                                                   ELXPMCBI
00248      IF ADDRESS OF PMCI-COMM-AREA = NULL                          ELXPMCBI
00249         NEXT SENTENCE                                             ELXPMCBI
00250      ELSE                                                         ELXPMCBI
00251         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCBI
00252         SET PMCI-BC-NO-ERROR TO TRUE                              ELXPMCBI
00253         SET SW-NO-TRMNL-ERR TO TRUE                               ELXPMCBI
00254         IF ADDRESS OF CSAC-ACCUMULATOR-TABLE = NULL               ELXPMCBI
00255            MOVE +4801 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBI
00256            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBI
00257         ELSE                                                      ELXPMCBI
00258            SET ADDRESS OF LS-MATCH-LIST TO NULL                   ELXPMCBI
00259            PERFORM 0100-EXTRCT-BNFT-VALS.                         ELXPMCBI
00260                                                                   ELXPMCBI
00261      GOBACK.                                                      ELXPMCBI
00262                                                                   ELXPMCBI
00263 ************************************************************      ELXPMCBI
00264 *                                                          *      ELXPMCBI
00265 *        EXTRACT BENEFIT VALUES                            *      ELXPMCBI
00266 *                                                          *      ELXPMCBI
00267 ************************************************************      ELXPMCBI
00268  0100-EXTRCT-BNFT-VALS.                                           ELXPMCBI
00269                                                                   ELXPMCBI
00270      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELXPMCBI
00271                      CSAC-ACL-GC-TBL-PTR.                         ELXPMCBI
00272      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULLS                 ELXPMCBI
00273        OR ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULLS             ELXPMCBI
00274         SET PMCI-PSY-COINS-CALL TO TRUE                           ELXPMCBI
00275      ELSE                                                         ELXPMCBI
00276         PERFORM 0120-IDNTFY-AVLBL-BNFT-VALS.                      ELXPMCBI
00277                                                                   ELXPMCBI
00278 ************************************************************      ELXPMCBI
00279 *                                                          *      ELXPMCBI
00280 *        IDENTIFY AVAILABLE BENEFIT VALUES                 *      ELXPMCBI
00281 *                                                          *      ELXPMCBI
00282 ************************************************************      ELXPMCBI
00283  0120-IDNTFY-AVLBL-BNFT-VALS.                                     ELXPMCBI
00284                                                                   ELXPMCBI
00285      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCBI
00286      IF PMCI-PRV-CALL OR PMCI-PRV-NONE                            ELXPMCBI
00287         CONTINUE                                                  ELXPMCBI
00288      ELSE                                                         ELXPMCBI
00289         PERFORM 0130-VERIFY-COST-CONTAINMENT                      ELXPMCBI
00290             VARYING ATBL-IDX FROM 1 BY 1                          ELXPMCBI
00291                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBI
00292      SET WS-PRG-VAR-NOT-FOUND TO TRUE.                            ELXPMCBI
00293      PERFORM 0150-INTLZ-SP                                        ELXPMCBI
00294            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBI
00295                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCBI
00296                                                                   ELXPMCBI
00297      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBI
00298         IF PMCI-INSTITUTIONAL                                     ELXPMCBI
00299            IF PMCI-INPATIENT                                      ELXPMCBI
00300               PERFORM 1000-IDNTFY-INST-IP-VALS                    ELXPMCBI
00301            ELSE                                                   ELXPMCBI
00302               PERFORM 2000-IDNTFY-INST-OP-VALS                    ELXPMCBI
00303         ELSE                                                      ELXPMCBI
00304            IF PMCI-INPATIENT                                      ELXPMCBI
00305               PERFORM 3000-IDNTFY-PROF-IP-VALS                    ELXPMCBI
00306            ELSE                                                   ELXPMCBI
00307               PERFORM 4000-IDNTFY-PROF-OP-VALS.                   ELXPMCBI
00308                                                                   ELXPMCBI
00309 ************************************************************      ELXPMCBI
00310 *                                                          *      ELXPMCBI
00311 *        VERIFY COST CONTAINMENT PROGRAMS                  *      ELXPMCBI
00312 *                                                          *      ELXPMCBI
00313 ************************************************************      ELXPMCBI
00314  0130-VERIFY-COST-CONTAINMENT.                                    ELXPMCBI
00315                                                                   ELXPMCBI
00316      EVALUATE TRUE                                                ELXPMCBI
00317         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBI
00318            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBI
00319         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBI
00320            PERFORM 0132-TEST-PPO-CSTCNMT-OUT                      ELXPMCBI
00321         WHEN PMCI-PRV-BAE-IN                                      ELXPMCBI
00322            PERFORM 0133-TEST-BAE-CSTCNMT-IN                       ELXPMCBI
00323         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBI
00324            PERFORM 0133-TEST-BAE-CSTCNMT-OUT                      ELXPMCBI
00325         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBI
00326            PERFORM 0132-TEST-PPO-CSTCNMT-IN                       ELXPMCBI
00327         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBI
00328            PERFORM 0134-TEST-RPO-CSTCNMT-OUT                      ELXPMCBI
00329         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBI
00330            PERFORM 0134-TEST-RPO-CSTCNMT-IN                       ELXPMCBI
00331         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBI
00332            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBI
00333         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBI
00334            PERFORM 0136-TEST-MCNP-CSTCNMT-IN                      ELXPMCBI
00335         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBI
00336            PERFORM 0136-TEST-MCNP-CSTCNMT-OUT                     ELXPMCBI
00337         WHEN OTHER                                                ELXPMCBI
00338            CONTINUE                                               ELXPMCBI
00339      END-EVALUATE.                                                ELXPMCBI
00340 ************************************************************      ELXPMCBI
00341 *                                                          *      ELXPMCBI
00342 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00343 *          FOR IN NETWORK                                  *      ELXPMCBI
00344 ************************************************************      ELXPMCBI
00345  0132-TEST-PPO-CSTCNMT-IN.                                        ELXPMCBI
00346                                                                   ELXPMCBI
00347      EVALUATE TRUE                                                ELXPMCBI
00348         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBI
00349             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBI
00350         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBI
00351             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00352         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00353             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00354         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBI
00355             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00356         WHEN OTHER                                                ELXPMCBI
00357             CONTINUE                                              ELXPMCBI
00358      END-EVALUATE.                                                ELXPMCBI
00359                                                                   ELXPMCBI
00360 ************************************************************      ELXPMCBI
00361 *                                                          *      ELXPMCBI
00362 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00363 *          FOR IN NETWORK                                  *      ELXPMCBI
00364 ************************************************************      ELXPMCBI
00365  0133-TEST-BAE-CSTCNMT-IN.                                        ELXPMCBI
00366                                                                   ELXPMCBI
00367      EVALUATE TRUE                                                ELXPMCBI
00368         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00369             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBI
00370         WHEN OTHER                                                ELXPMCBI
00371             CONTINUE                                              ELXPMCBI
00372      END-EVALUATE.                                                ELXPMCBI
00373                                                                   ELXPMCBI
00374 ************************************************************      ELXPMCBI
00375 *                                                          *      ELXPMCBI
00376 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00377 *          FOR OUT OF NETWORK                              *      ELXPMCBI
00378 ************************************************************      ELXPMCBI
00379  0133-TEST-BAE-CSTCNMT-OUT.                                       ELXPMCBI
00380                                                                   ELXPMCBI
00381      EVALUATE TRUE                                                ELXPMCBI
00382         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00383             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCBI
00384         WHEN OTHER                                                ELXPMCBI
00385             CONTINUE                                              ELXPMCBI
00386      END-EVALUATE.                                                ELXPMCBI
00387                                                                   ELXPMCBI
00388 ************************************************************      ELXPMCBI
00389 *                                                          *      ELXPMCBI
00390 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00391 *          FOR OUT OF NETWORK                              *      ELXPMCBI
00392 ************************************************************      ELXPMCBI
00393  0132-TEST-PPO-CSTCNMT-OUT.                                       ELXPMCBI
00394                                                                   ELXPMCBI
00395      EVALUATE TRUE                                                ELXPMCBI
00396         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBI
00397             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCBI
00398         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBI
00399             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00400         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00401             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00402         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBI
00403             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCBI
00404         WHEN OTHER                                                ELXPMCBI
00405             CONTINUE                                              ELXPMCBI
00406      END-EVALUATE.                                                ELXPMCBI
00407                                                                   ELXPMCBI
00408 ************************************************************      ELXPMCBI
00409 *                                                          *      ELXPMCBI
00410 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00411 *          FOR IN NETWORK                                  *      ELXPMCBI
00412 ************************************************************      ELXPMCBI
00413  0134-TEST-RPO-CSTCNMT-IN.                                        ELXPMCBI
00414                                                                   ELXPMCBI
00415      EVALUATE TRUE                                                ELXPMCBI
00416         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBI
00417             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBI
00418         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBI
00419             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBI
00420         WHEN OTHER                                                ELXPMCBI
00421             CONTINUE                                              ELXPMCBI
00422      END-EVALUATE.                                                ELXPMCBI
00423                                                                   ELXPMCBI
00424 ************************************************************      ELXPMCBI
00425 *                                                          *      ELXPMCBI
00426 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCBI
00427 *          FOR OUT OF NETWORK                              *      ELXPMCBI
00428 ************************************************************      ELXPMCBI
00429  0134-TEST-RPO-CSTCNMT-OUT.                                       ELXPMCBI
00430                                                                   ELXPMCBI
00431      EVALUATE TRUE                                                ELXPMCBI
00432         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBI
00433             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCBI
00434         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBI
00435             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCBI
00436         WHEN OTHER                                                ELXPMCBI
00437             CONTINUE                                              ELXPMCBI
00438      END-EVALUATE.                                                ELXPMCBI
00439                                                                   ELXPMCBI
00440 ************************************************************      ELXPMCBI
00441 *                                                          *      ELXPMCBI
00442 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBI
00443 *          FOR REFERRAL OR NO REFERRAL REQUIRED            *      ELXPMCBI
00444 ************************************************************      ELXPMCBI
00445  0136-TEST-MCNP-CSTCNMT-IN.                                       ELXPMCBI
00446                                                                   ELXPMCBI
00447      EVALUATE TRUE                                                ELXPMCBI
00448         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBI
00449             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBI
00450         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBI
00451             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBI
00452         WHEN OTHER                                                ELXPMCBI
00453             CONTINUE                                              ELXPMCBI
00454      END-EVALUATE.                                                ELXPMCBI
00455                                                                   ELXPMCBI
00456 ************************************************************      ELXPMCBI
00457 *                                                          *      ELXPMCBI
00458 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCBI
00459 *          FOR NON REFERRAL                                *      ELXPMCBI
00460 ************************************************************      ELXPMCBI
00461  0136-TEST-MCNP-CSTCNMT-OUT.                                      ELXPMCBI
00462                                                                   ELXPMCBI
00463      EVALUATE TRUE                                                ELXPMCBI
00464         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBI
00465             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCBI
00466         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBI
00467             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCBI
00468         WHEN OTHER                                                ELXPMCBI
00469             CONTINUE                                              ELXPMCBI
00470      END-EVALUATE.                                                ELXPMCBI
00471                                                                   ELXPMCBI
00472 ************************************************************      ELXPMCBI
00473 *                                                          *      ELXPMCBI
00474 *        INITIALIZE SPECIAL CASE FACTORS                   *      ELXPMCBI
00475 *                                                          *      ELXPMCBI
00476 * 3/13/95 RGO. CPO PROJECT.                                *      ELXPMCBI
00477 ************************************************************      ELXPMCBI
00478  0150-INTLZ-SP.                                                   ELXPMCBI
00479                                                                   ELXPMCBI
00480      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX) TO                          ELXPMCBI
00481                    ATBL-CF-SP-FCTRS (ATBL-IDX).                   ELXPMCBI
00482      EVALUATE TRUE                                                ELXPMCBI
00483         WHEN PMCI-PRV-PPO-IN                                      ELXPMCBI
00484            PERFORM 0160-SCN-SP-CCP-IPPO                           ELXPMCBI
00485         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCBI
00486            PERFORM 0161-SCN-SP-CCP-OPPO                           ELXPMCBI
00487         WHEN PMCI-PRV-BAE-IN                                      ELXPMCBI
00488            PERFORM 0162-SCN-SP-CCP-IBAE                           ELXPMCBI
00489         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCBI
00490            PERFORM 0163-SCN-SP-CCP-OBAE                           ELXPMCBI
00491         WHEN PMCI-PRV-RPO-IN                                      ELXPMCBI
00492            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBI
00493         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCBI
00494            PERFORM 0171-SCN-SP-CCP-ORPO                           ELXPMCBI
00495         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCBI
00496            PERFORM 0170-SCN-SP-CCP-IRPO                           ELXPMCBI
00497         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCBI
00498            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBI
00499         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCBI
00500            PERFORM 0180-SCN-SP-CCP-IMCNP                          ELXPMCBI
00501         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCBI
00502            PERFORM 0181-SCN-SP-CCP-OMCNP                          ELXPMCBI
00503         WHEN PMCI-PRV-CALL                                        ELXPMCBI
00504            PERFORM 0190-SCN-SP-CCP-CALL                           ELXPMCBI
00505 *                                                                 ELXPMCBI
00506                                                                   ELXPMCBI
00507         WHEN PMCI-PRV-CPO-MET                                     ELXPMCBI
00508            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBI
00509               MOVE WS-CF-ZERO TO                                  ELXPMCBI
00510                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBI
00511            ELSE                                                   ELXPMCBI
00512               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K1'          ELXPMCBI
00513                  MOVE WS-CF-TRUE TO                               ELXPMCBI
00514                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00515               ELSE                                                ELXPMCBI
00516                  MOVE WS-CF-FALSE TO                              ELXPMCBI
00517                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00518               END-IF                                              ELXPMCBI
00519            END-IF                                                 ELXPMCBI
00520                                                                   ELXPMCBI
00521         WHEN PMCI-PRV-CPO-PPO-MET                                 ELXPMCBI
00522            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBI
00523               MOVE WS-CF-ZERO TO                                  ELXPMCBI
00524                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00525            ELSE                                                   ELXPMCBI
00526               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'KI'         ELXPMCBI
00527                     MOVE WS-CF-TRUE TO                            ELXPMCBI
00528                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00529               ELSE                                                ELXPMCBI
00530                  MOVE WS-CF-FALSE TO                              ELXPMCBI
00531                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00532               END-IF                                              ELXPMCBI
00533            END-IF                                                 ELXPMCBI
00534         WHEN PMCI-PRV-CPO-PPO-NOT-MET                             ELXPMCBI
00535            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBI
00536               MOVE WS-CF-ZERO TO                                  ELXPMCBI
00537                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00538            ELSE                                                   ELXPMCBI
00539               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K4'         ELXPMCBI
00540                     MOVE WS-CF-TRUE TO                            ELXPMCBI
00541                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00542               ELSE                                                ELXPMCBI
00543                  MOVE WS-CF-FALSE TO                              ELXPMCBI
00544                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00545               END-IF                                              ELXPMCBI
00546            END-IF                                                 ELXPMCBI
00547                                                                   ELXPMCBI
00548 *  **** COMMUNITY BLUE   **********                               ELXPMCBI
00549         WHEN PMCI-PRV-CBL-IN                                      ELXPMCBI
00550            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBI
00551               MOVE WS-CF-ZERO TO                                  ELXPMCBI
00552                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCBI
00553            ELSE                                                   ELXPMCBI
00554               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J1'          ELXPMCBI
00555                  MOVE WS-CF-TRUE TO                               ELXPMCBI
00556                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00557               ELSE                                                ELXPMCBI
00558                  MOVE WS-CF-FALSE TO                              ELXPMCBI
00559                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00560               END-IF                                              ELXPMCBI
00561            END-IF                                                 ELXPMCBI
00562                                                                   ELXPMCBI
00563         WHEN PMCI-PRV-CBL-OUT                                     ELXPMCBI
00564            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCBI
00565               MOVE WS-CF-ZERO TO                                  ELXPMCBI
00566                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00567            ELSE                                                   ELXPMCBI
00568               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J4'         ELXPMCBI
00569                     MOVE WS-CF-TRUE TO                            ELXPMCBI
00570                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00571               ELSE                                                ELXPMCBI
00572                  MOVE WS-CF-FALSE TO                              ELXPMCBI
00573                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCBI
00574               END-IF                                              ELXPMCBI
00575            END-IF                                                 ELXPMCBI
00576                                                                   ELXPMCBI
00577                                                                   ELXPMCBI
00578         WHEN OTHER                                                ELXPMCBI
00579            CONTINUE                                               ELXPMCBI
00580      END-EVALUATE.                                                ELXPMCBI
00581                                                                   ELXPMCBI
00582 ************************************************************      ELXPMCBI
00583 *                                                          *      ELXPMCBI
00584 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR PPO   *      ELXPMCBI
00585 *                                                          *      ELXPMCBI
00586 ************************************************************      ELXPMCBI
00587  0160-SCN-SP-CCP-IPPO.                                            ELXPMCBI
00588                                                                   ELXPMCBI
00589      EVALUATE TRUE                                                ELXPMCBI
00590         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00591           IF WS-PPO-INC-FOUND                                     ELXPMCBI
00592             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00593           ELSE                                                    ELXPMCBI
00594             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00595           END-IF                                                  ELXPMCBI
00596         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBI
00597             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00598             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00599         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBI
00600             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00601             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00602         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBI
00603             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00604             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00605         WHEN OTHER                                                ELXPMCBI
00606             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00607      END-EVALUATE.                                                ELXPMCBI
00608                                                                   ELXPMCBI
00609 ************************************************************      ELXPMCBI
00610 *                                                          *      ELXPMCBI
00611 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR BAE   *      ELXPMCBI
00612 *                                                          *      ELXPMCBI
00613 ************************************************************      ELXPMCBI
00614  0162-SCN-SP-CCP-IBAE.                                            ELXPMCBI
00615                                                                   ELXPMCBI
00616      EVALUATE TRUE                                                ELXPMCBI
00617         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00618           IF WS-BAE-PEN-FOUND                                     ELXPMCBI
00619             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00620           END-IF                                                  ELXPMCBI
00621         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00622             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00623             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00624         WHEN OTHER                                                ELXPMCBI
00625             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00626      END-EVALUATE.                                                ELXPMCBI
00627                                                                   ELXPMCBI
00628 ************************************************************      ELXPMCBI
00629 *                                                          *      ELXPMCBI
00630 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-PPO   *      ELXPMCBI
00631 *                                                          *      ELXPMCBI
00632 ************************************************************      ELXPMCBI
00633  0161-SCN-SP-CCP-OPPO.                                            ELXPMCBI
00634                                                                   ELXPMCBI
00635      EVALUATE TRUE                                                ELXPMCBI
00636         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00637           IF WS-PPO-PEN-FOUND                                     ELXPMCBI
00638             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00639           ELSE                                                    ELXPMCBI
00640             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00641           END-IF                                                  ELXPMCBI
00642         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBI
00643             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00644             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00645         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBI
00646             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00647             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00648         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBI
00649             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00650             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00651         WHEN OTHER                                                ELXPMCBI
00652             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00653      END-EVALUATE.                                                ELXPMCBI
00654                                                                   ELXPMCBI
00655 ************************************************************      ELXPMCBI
00656 *                                                          *      ELXPMCBI
00657 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-BAE   *      ELXPMCBI
00658 *                                                          *      ELXPMCBI
00659 ************************************************************      ELXPMCBI
00660  0163-SCN-SP-CCP-OBAE.                                            ELXPMCBI
00661                                                                   ELXPMCBI
00662      EVALUATE TRUE                                                ELXPMCBI
00663         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00664           IF WS-BAE-PEN-FOUND                                     ELXPMCBI
00665             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00666           END-IF                                                  ELXPMCBI
00667         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00668             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00669             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00670         WHEN OTHER                                                ELXPMCBI
00671             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00672      END-EVALUATE.                                                ELXPMCBI
00673                                                                   ELXPMCBI
00674 ************************************************************      ELXPMCBI
00675 *                                                          *      ELXPMCBI
00676 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR RPO   *      ELXPMCBI
00677 *                                                          *      ELXPMCBI
00678 ************************************************************      ELXPMCBI
00679  0170-SCN-SP-CCP-IRPO.                                            ELXPMCBI
00680                                                                   ELXPMCBI
00681      EVALUATE TRUE                                                ELXPMCBI
00682         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00683           IF WS-RPO-INC-FOUND                                     ELXPMCBI
00684             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00685           ELSE                                                    ELXPMCBI
00686             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00687           END-IF                                                  ELXPMCBI
00688         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBI
00689             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00690             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00691         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBI
00692             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00693             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00694         WHEN OTHER                                                ELXPMCBI
00695             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00696      END-EVALUATE.                                                ELXPMCBI
00697                                                                   ELXPMCBI
00698 ************************************************************      ELXPMCBI
00699 *                                                          *      ELXPMCBI
00700 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-RPO   *      ELXPMCBI
00701 *                                                          *      ELXPMCBI
00702 ************************************************************      ELXPMCBI
00703  0171-SCN-SP-CCP-ORPO.                                            ELXPMCBI
00704                                                                   ELXPMCBI
00705      EVALUATE TRUE                                                ELXPMCBI
00706         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00707           IF WS-RPO-PEN-FOUND                                     ELXPMCBI
00708             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00709           ELSE                                                    ELXPMCBI
00710             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00711           END-IF                                                  ELXPMCBI
00712         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBI
00713             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00714             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00715         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBI
00716             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00717             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00718         WHEN OTHER                                                ELXPMCBI
00719             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00720      END-EVALUATE.                                                ELXPMCBI
00721                                                                   ELXPMCBI
00722 ************************************************************      ELXPMCBI
00723 *                                                          *      ELXPMCBI
00724 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR MCNP  *      ELXPMCBI
00725 *                                                          *      ELXPMCBI
00726 ************************************************************      ELXPMCBI
00727  0180-SCN-SP-CCP-IMCNP.                                           ELXPMCBI
00728                                                                   ELXPMCBI
00729      EVALUATE TRUE                                                ELXPMCBI
00730         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00731           IF WS-MCNP-INC-FOUND                                    ELXPMCBI
00732             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00733           ELSE                                                    ELXPMCBI
00734             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00735           END-IF                                                  ELXPMCBI
00736         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBI
00737             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00738             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00739         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBI
00740             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00741             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00742         WHEN OTHER                                                ELXPMCBI
00743             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00744      END-EVALUATE.                                                ELXPMCBI
00745                                                                   ELXPMCBI
00746 ************************************************************      ELXPMCBI
00747 *                                                          *      ELXPMCBI
00748 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-MCNP  *      ELXPMCBI
00749 *                                                          *      ELXPMCBI
00750 ************************************************************      ELXPMCBI
00751  0181-SCN-SP-CCP-OMCNP.                                           ELXPMCBI
00752                                                                   ELXPMCBI
00753      EVALUATE TRUE                                                ELXPMCBI
00754         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00755           IF WS-MCNP-PEN-FOUND                                    ELXPMCBI
00756             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00757           ELSE                                                    ELXPMCBI
00758             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00759           END-IF                                                  ELXPMCBI
00760         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBI
00761             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00762             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00763         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBI
00764             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00765             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00766         WHEN OTHER                                                ELXPMCBI
00767             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00768      END-EVALUATE.                                                ELXPMCBI
00769                                                                   ELXPMCBI
00770 ************************************************************      ELXPMCBI
00771 *                                                          *      ELXPMCBI
00772 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PRG*      ELXPMCBI
00773 *                                                          *      ELXPMCBI
00774 ************************************************************      ELXPMCBI
00775  0190-SCN-SP-CCP-CALL.                                            ELXPMCBI
00776                                                                   ELXPMCBI
00777      EVALUATE TRUE                                                ELXPMCBI
00778         WHEN PMCI-PRG-PPO-APPLIES                                 ELXPMCBI
00779             PERFORM 0191-SCN-SP-CCP-CALL-PPO                      ELXPMCBI
00780         WHEN PMCI-PRG-RPO-APPLIES                                 ELXPMCBI
00781             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBI
00782         WHEN PMCI-PRG-RPO-PPO-APPLIES                             ELXPMCBI
00783             PERFORM 0192-SCN-SP-CCP-CALL-RPO                      ELXPMCBI
00784         WHEN PMCI-PRG-MCNP-APPLIES                                ELXPMCBI
00785             PERFORM 0193-SCN-SP-CCP-CALL-MCNP                     ELXPMCBI
00786         WHEN PMCI-PRG-BAE-APPLIES                                 ELXPMCBI
00787             PERFORM 0194-SCN-SP-CCP-CALL-BAE                      ELXPMCBI
00788         WHEN OTHER                                                ELXPMCBI
00789             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00790      END-EVALUATE.                                                ELXPMCBI
00791                                                                   ELXPMCBI
00792 ************************************************************      ELXPMCBI
00793 *                                                          *      ELXPMCBI
00794 *SET CALL FOR PPO FACTORS                                  *      ELXPMCBI
00795 *                                                          *      ELXPMCBI
00796 ************************************************************      ELXPMCBI
00797  0191-SCN-SP-CCP-CALL-PPO.                                        ELXPMCBI
00798                                                                   ELXPMCBI
00799      EVALUATE TRUE                                                ELXPMCBI
00800         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00801             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00802         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCBI
00803             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00804             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00805         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCBI
00806             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00807             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00808         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCBI
00809             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00810             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00811         WHEN OTHER                                                ELXPMCBI
00812             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00813      END-EVALUATE.                                                ELXPMCBI
00814 ************************************************************      ELXPMCBI
00815 *                                                          *      ELXPMCBI
00816 *SET CALL FOR BAE FACTORS                                  *      ELXPMCBI
00817 *                                                          *      ELXPMCBI
00818 ************************************************************      ELXPMCBI
00819  0194-SCN-SP-CCP-CALL-BAE.                                        ELXPMCBI
00820                                                                   ELXPMCBI
00821      EVALUATE TRUE                                                ELXPMCBI
00822         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00823             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00824         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCBI
00825             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00826             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00827         WHEN OTHER                                                ELXPMCBI
00828             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00829      END-EVALUATE.                                                ELXPMCBI
00830 ************************************************************      ELXPMCBI
00831 *                                                          *      ELXPMCBI
00832 *SET CALL FOR RPO FACTORS                                  *      ELXPMCBI
00833 *                                                          *      ELXPMCBI
00834 ************************************************************      ELXPMCBI
00835  0192-SCN-SP-CCP-CALL-RPO.                                        ELXPMCBI
00836                                                                   ELXPMCBI
00837      EVALUATE TRUE                                                ELXPMCBI
00838         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00839             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00840         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCBI
00841             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00842             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00843         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCBI
00844             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00845             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00846         WHEN OTHER                                                ELXPMCBI
00847             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00848      END-EVALUATE.                                                ELXPMCBI
00849 ************************************************************      ELXPMCBI
00850 *                                                          *      ELXPMCBI
00851 *SET CALL FOR MCNP FACTORS                                 *      ELXPMCBI
00852 *                                                          *      ELXPMCBI
00853 ************************************************************      ELXPMCBI
00854  0193-SCN-SP-CCP-CALL-MCNP.                                       ELXPMCBI
00855                                                                   ELXPMCBI
00856      EVALUATE TRUE                                                ELXPMCBI
00857         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCBI
00858             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00859         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCBI
00860             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00861             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00862         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCBI
00863             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCBI
00864             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCBI
00865         WHEN OTHER                                                ELXPMCBI
00866             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCBI
00867      END-EVALUATE.                                                ELXPMCBI
00868      IF PMCI-REFERRAL-EXISTS                                      ELXPMCBI
00869         MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX).      ELXPMCBI
00870                                                                   ELXPMCBI
00871                                                                   ELXPMCBI
00872 ************************************************************      ELXPMCBI
00873 *                                                          *      ELXPMCBI
00874 * IDENTIFY INSTITUTIONAL INPATIENT VALUES                  *      ELXPMCBI
00875 *                                                          *      ELXPMCBI
00876 ************************************************************      ELXPMCBI
00877  1000-IDNTFY-INST-IP-VALS.                                        ELXPMCBI
00878                                                                   ELXPMCBI
00879      PERFORM 1001-INITIALIZE.                                     ELXPMCBI
00880      PERFORM 1300-IDNTFY-INST-IP-PSYCH-VAL.                       ELXPMCBI
00881                                                                   ELXPMCBI
00882 ************************************************************      ELXPMCBI
00883 *                                                          *      ELXPMCBI
00884 * IDENTIFY INSTITUTIONAL INPATIENT VALUES/INITIALIZE       *      ELXPMCBI
00885 *                                                          *      ELXPMCBI
00886 ************************************************************      ELXPMCBI
00887  1001-INITIALIZE.                                                 ELXPMCBI
00888                                                                   ELXPMCBI
00889      CALL 'ELUADDRS' USING WS-INST-IP-PSYS                        ELXPMCBI
00890                          WS-PSYS-POINTER.                         ELXPMCBI
00891                                                                   ELXPMCBI
00892 ************************************************************      ELXPMCBI
00893 *                                                          *      ELXPMCBI
00894 * IDENTIFY INSTITUTIONAL INPATIENT PSYCH SERVICES VALUES *        ELXPMCBI
00895 *                                                          *      ELXPMCBI
00896 ************************************************************      ELXPMCBI
00897  1300-IDNTFY-INST-IP-PSYCH-VAL.                                   ELXPMCBI
00898                                                                   ELXPMCBI
00899      IF PSY-YES                                                   ELXPMCBI
00900         PERFORM 6000-SRCH-MNTL-VAL                                ELXPMCBI
00901      ELSE                                                         ELXPMCBI
00902         SET PMCI-PSY-COINS-NONE TO TRUE.                          ELXPMCBI
00903                                                                   ELXPMCBI
00904 ************************************************************      ELXPMCBI
00905 *                                                          *      ELXPMCBI
00906 * IDENTIFY INSTITUTIONAL OUTPATIENT VALUES                 *      ELXPMCBI
00907 *                                                          *      ELXPMCBI
00908 ************************************************************      ELXPMCBI
00909  2000-IDNTFY-INST-OP-VALS.                                        ELXPMCBI
00910                                                                   ELXPMCBI
00911      PERFORM 2001-INITIALIZE.                                     ELXPMCBI
00912      PERFORM 2300-IDNTFY-INST-OP-PSYCH-VAL.                       ELXPMCBI
00913                                                                   ELXPMCBI
00914 ************************************************************      ELXPMCBI
00915 *                                                          *      ELXPMCBI
00916 * IDENTIFY INSTITUTIONAL OUTPATIENT VALUES/INITIALIZE      *      ELXPMCBI
00917 *                                                          *      ELXPMCBI
00918 ************************************************************      ELXPMCBI
00919  2001-INITIALIZE.                                                 ELXPMCBI
00920                                                                   ELXPMCBI
00921      CALL 'ELUADDRS' USING WS-INST-OP-PSYS                        ELXPMCBI
00922                          WS-PSYS-POINTER.                         ELXPMCBI
00923                                                                   ELXPMCBI
00924 ************************************************************      ELXPMCBI
00925 *                                                          *      ELXPMCBI
00926 * IDENTIFY INSTITUTIONAL OUTPATIENT PSYCH SERVICES VALUES*        ELXPMCBI
00927 *                                                          *      ELXPMCBI
00928 ************************************************************      ELXPMCBI
00929  2300-IDNTFY-INST-OP-PSYCH-VAL.                                   ELXPMCBI
00930                                                                   ELXPMCBI
00931      IF PSY-YES                                                   ELXPMCBI
00932         PERFORM 6000-SRCH-MNTL-VAL                                ELXPMCBI
00933      ELSE                                                         ELXPMCBI
00934         SET PMCI-PSY-COINS-NONE TO TRUE.                          ELXPMCBI
00935                                                                   ELXPMCBI
00936 ************************************************************      ELXPMCBI
00937 *                                                          *      ELXPMCBI
00938 * IDENTIFY PROFESSIONAL INPATIENT VALUES                   *      ELXPMCBI
00939 *                                                          *      ELXPMCBI
00940 ************************************************************      ELXPMCBI
00941  3000-IDNTFY-PROF-IP-VALS.                                        ELXPMCBI
00942                                                                   ELXPMCBI
00943      PERFORM 3001-INITIALIZE.                                     ELXPMCBI
00944      PERFORM 3300-IDNTFY-PROF-IP-PSYCH-VAL.                       ELXPMCBI
00945                                                                   ELXPMCBI
00946 ************************************************************      ELXPMCBI
00947 *                                                          *      ELXPMCBI
00948 * IDENTIFY PROFESSIONAL INPATIENT  VALUES/INITIALIZE       *      ELXPMCBI
00949 *                                                          *      ELXPMCBI
00950 ************************************************************      ELXPMCBI
00951  3001-INITIALIZE.                                                 ELXPMCBI
00952                                                                   ELXPMCBI
00953      CALL 'ELUADDRS' USING WS-PROF-IP-PSYS                        ELXPMCBI
00954                          WS-PSYS-POINTER.                         ELXPMCBI
00955                                                                   ELXPMCBI
00956 ************************************************************      ELXPMCBI
00957 *                                                          *      ELXPMCBI
00958 * IDENTIFY PROFESSIONAL INPATIENT PSYCH SERVICES VALUES *         ELXPMCBI
00959 *                                                          *      ELXPMCBI
00960 ************************************************************      ELXPMCBI
00961  3300-IDNTFY-PROF-IP-PSYCH-VAL.                                   ELXPMCBI
00962                                                                   ELXPMCBI
00963      IF PSY-YES                                                   ELXPMCBI
00964         PERFORM 6000-SRCH-MNTL-VAL                                ELXPMCBI
00965      ELSE                                                         ELXPMCBI
00966         SET PMCI-PSY-COINS-NONE TO TRUE.                          ELXPMCBI
00967                                                                   ELXPMCBI
00968 ************************************************************      ELXPMCBI
00969 *                                                          *      ELXPMCBI
00970 * IDENTIFY PROFESSIONAL OUTPATIENT VALUES                  *      ELXPMCBI
00971 *                                                          *      ELXPMCBI
00972 ************************************************************      ELXPMCBI
00973  4000-IDNTFY-PROF-OP-VALS.                                        ELXPMCBI
00974                                                                   ELXPMCBI
00975      PERFORM 4001-INITIALIZE.                                     ELXPMCBI
00976      PERFORM 4300-IDNTFY-PROF-OP-PSYCH-VAL.                       ELXPMCBI
00977                                                                   ELXPMCBI
00978 ************************************************************      ELXPMCBI
00979 *                                                          *      ELXPMCBI
00980 * IDENTIFY PROFESSIONAL OUTPATIENT VALUES/INITIALIZE       *      ELXPMCBI
00981 *                                                          *      ELXPMCBI
00982 ************************************************************      ELXPMCBI
00983  4001-INITIALIZE.                                                 ELXPMCBI
00984                                                                   ELXPMCBI
00985      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCBI
00986                          WS-PSYS-POINTER.                         ELXPMCBI
00987                                                                   ELXPMCBI
00988 ************************************************************      ELXPMCBI
00989 *                                                          *      ELXPMCBI
00990 * IDENTIFY PROFESSIONAL OUTPATIENT PSYCH SERVICES VALUES*         ELXPMCBI
00991 *                                                          *      ELXPMCBI
00992 ************************************************************      ELXPMCBI
00993  4300-IDNTFY-PROF-OP-PSYCH-VAL.                                   ELXPMCBI
00994                                                                   ELXPMCBI
00995      IF PSY-YES                                                   ELXPMCBI
00996         PERFORM 6000-SRCH-MNTL-VAL                                ELXPMCBI
00997      ELSE                                                         ELXPMCBI
00998         SET PMCI-PSY-COINS-NONE TO TRUE.                          ELXPMCBI
00999                                                                   ELXPMCBI
01000 ************************************************************      ELXPMCBI
01001 *                                                          *      ELXPMCBI
01002 *     SEARCH FOR MENTAL VALUES                             *      ELXPMCBI
01003 *                                                          *      ELXPMCBI
01004 ************************************************************      ELXPMCBI
01005  6000-SRCH-MNTL-VAL.                                              ELXPMCBI
01006                                                                   ELXPMCBI
01007      PERFORM 6001-RCMPT-BNFT-PRVSN-PSYCH                          ELXPMCBI
01008      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBI
01009         PERFORM 6010-RCMPT-CF-MNTL-CNDTNS                         ELXPMCBI
01010            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBI
01011               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCBI
01012                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO               ELXPMCBI
01013         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBI
01014            MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD         ELXPMCBI
01015            PERFORM 6490-TST-COINS-VAL                             ELXPMCBI
01016         END-IF                                                    ELXPMCBI
01017      END-IF.                                                      ELXPMCBI
01018                                                                   ELXPMCBI
01019                                                                   ELXPMCBI
01020 ************************************************************      ELXPMCBI
01021 *                                                          *      ELXPMCBI
01022 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR PSYC COINSURANCE  *      ELXPMCBI
01023 *                                                          *      ELXPMCBI
01024 ************************************************************      ELXPMCBI
01025  6001-RCMPT-BNFT-PRVSN-PSYCH.                                     ELXPMCBI
01026                                                                   ELXPMCBI
01027      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-PSYS-POINTER.       ELXPMCBI
01028      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCBI
01029      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCBI
01030                                BPVL-BNFT-PRVSN-TBL.               ELXPMCBI
01031      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBI
01032      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCBI
01033         MOVE +4811 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBI
01034         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCBI
01035                                                                   ELXPMCBI
01036 ************************************************************      ELXPMCBI
01037 *                                                          *      ELXPMCBI
01038 * RECOMPUTE CONF FACTORS FOR MENTAL CONDITIONS             *      ELXPMCBI
01039 *                                                          *      ELXPMCBI
01040 ************************************************************      ELXPMCBI
01041  6010-RCMPT-CF-MNTL-CNDTNS.                                       ELXPMCBI
01042                                                                   ELXPMCBI
01043      MOVE WS-CF-ZERO TO WS-NO-IBGR-CF.                            ELXPMCBI
01044      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCBI
01045      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBI
01046         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCBI
01047         PERFORM 9020-RCMPT-CNDTN-BT-MNTL                          ELXPMCBI
01048         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBI
01049            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCBI
01050            IF PMCI-BC-SUCCESSFUL                                  ELXPMCBI
01051               MOVE CFT5-CF-INTD-PSYCH (CFT5-IDX) TO               ELXPMCBI
01052                     ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX).         ELXPMCBI
01053                                                                   ELXPMCBI
01054                                                                   ELXPMCBI
01055 ************************************************************      ELXPMCBI
01056 *                                                          *      ELXPMCBI
01057 * TEST AND SET PSYCH COINSURANCE VALUE                     *      ELXPMCBI
01058 *                                                          *      ELXPMCBI
01059 ************************************************************      ELXPMCBI
01060  6490-TST-COINS-VAL.                                              ELXPMCBI
01061                                                                   ELXPMCBI
01062      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCBI
01063      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBI
01064         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCBI
01065         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCBI
01066            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCBI
01067            MOVE WS-SAVE-SUB TO WS-OTR-DAY-SUB                     ELXPMCBI
01068 *THIS IS ADDED FOR BLUE STORM                                     ELXPMCBI
01069 *                                                                 ELXPMCBI
01070            IF PMCI-BLUE-STORM-CALL  AND                           ELXPMCBI
01071               ATBL-COND-ALL-BIT (ATBL-IDX) NOT = '1'              ELXPMCBI
01072                 INITIALIZE WS-PRG-VARIATION-CONTROL               ELXPMCBI
01073                 MOVE '1' TO ATBL-COND-ALL-BIT (ATBL-IDX)          ELXPMCBI
01074            END-IF                                                 ELXPMCBI
01075 *END OF BLUE STORM ADD                                            ELXPMCBI
01076 *                                                                 ELXPMCBI
01077            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCBI
01078                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCBI
01079                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCBI
01080                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCBI
01081                       WS-NUM-APPL-ENTRS > 1                       ELXPMCBI
01082               SET PMCI-PSY-COINS-CALL TO TRUE                     ELXPMCBI
01083            ELSE                                                   ELXPMCBI
01084               PERFORM 6495-CALC-PSY-COINS                         ELXPMCBI
01085            END-IF                                                 ELXPMCBI
01086         ELSE                                                      ELXPMCBI
01087         SET PMCI-PSY-COINS-NOT-APPL TO TRUE                       ELXPMCBI
01088         END-IF                                                    ELXPMCBI
01089      END-IF.                                                      ELXPMCBI
01090                                                                   ELXPMCBI
01091 ************************************************************      ELXPMCBI
01092 *                                                          *      ELXPMCBI
01093 *  COMPUTE COINSURANCE PERCENTAGE                          *      ELXPMCBI
01094 *                                                          *      ELXPMCBI
01095 ************************************************************      ELXPMCBI
01096  6495-CALC-PSY-COINS.                                             ELXPMCBI
01097      IF PMCI-PRV-CALL                                             ELXPMCBI
01098         IF WS-PRG-VAR-FOUND                                       ELXPMCBI
01099            SET PMCI-PSY-COINS-CALL TO TRUE                        ELXPMCBI
01100         ELSE                                                      ELXPMCBI
01101            SET PMCI-PSY-COINS-PERCENT TO TRUE                     ELXPMCBI
01102            COMPUTE PMCI-PSY-COINS-PERC-VALUE =                    ELXPMCBI
01103                       100 - ATBL-PERCENT-LEVEL (ATBL-IDX)         ELXPMCBI
01104            MOVE WS-SAVE-LOB-IND TO                                ELXPMCBI
01105                               PMCI-MAX-MNTL-FROM-IND              ELXPMCBI
01106      ELSE                                                         ELXPMCBI
01107      IF ATBL-ASCEND-DESCEND-IND (ATBL-IDX) = '0' AND              ELXPMCBI
01108                 ATBL-COND-ALL-BIT (ATBL-IDX) = '1'                ELXPMCBI
01109         SET PMCI-PSY-COINS-PERCENT TO TRUE                        ELXPMCBI
01110         COMPUTE PMCI-PSY-COINS-PERC-VALUE =                       ELXPMCBI
01111                      100 - ATBL-PERCENT-LEVEL (ATBL-IDX)          ELXPMCBI
01112         MOVE WS-SAVE-LOB-IND TO                                   ELXPMCBI
01113                               PMCI-MAX-MNTL-FROM-IND              ELXPMCBI
01114      ELSE                                                         ELXPMCBI
01115         SET PMCI-PSY-COINS-CALL TO TRUE.                          ELXPMCBI
01116 ************************************************************      ELXPMCBI
01117 *                                                          *      ELXPMCBI
01118 *  RECOMPUTE CONDITION BITS FACTOR FOR MENTAL              *      ELXPMCBI
01119 *                                                          *      ELXPMCBI
01120 ************************************************************      ELXPMCBI
01121  9020-RCMPT-CNDTN-BT-MNTL.                                        ELXPMCBI
01122                                                                   ELXPMCBI
01123      IF ATBL-COND-MENTAL-BIT (ATBL-IDX) = '1'                     ELXPMCBI
01124         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCBI
01125            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCBI
01126         ELSE                                                      ELXPMCBI
01127            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCBI
01128      ELSE                                                         ELXPMCBI
01129      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCBI
01130               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCBI
01131         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                   ELXPMCBI
01132            MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)  ELXPMCBI
01133         ELSE                                                      ELXPMCBI
01134            MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO               ELXPMCBI
01135                         ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)           ELXPMCBI
01136      ELSE                                                         ELXPMCBI
01137         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCBI
01138                                                                   ELXPMCBI
01139 ************************************************************      ELXPMCBI
01140 *                                                          *      ELXPMCBI
01141 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBI
01142 *                                                          *      ELXPMCBI
01143 ************************************************************      ELXPMCBI
01144  9120-RCMPT-BNFT-PRVSN-CF.                                        ELXPMCBI
01145                                                                   ELXPMCBI
01146       IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = ZERO                  ELXPMCBI
01147          MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)   ELXPMCBI
01148          SET WS-IBGR-FOUND TO TRUE                                ELXPMCBI
01149       ELSE                                                        ELXPMCBI
01150          SET WS-IBGR-NOT-FOUND TO TRUE                            ELXPMCBI
01151          SET IBGR-MAX-IDX TO IBGR-TBL-CNT                         ELXPMCBI
01152          PERFORM VARYING IBGR-IDX FROM 1 BY 1                     ELXPMCBI
01153              UNTIL IBGR-IDX > IBGR-MAX-IDX OR                     ELXPMCBI
01154                          WS-IBGR-FOUND                            ELXPMCBI
01155          IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) =                    ELXPMCBI
01156                        IBGR-SLOT-NUMBER (IBGR-IDX)                ELXPMCBI
01157             SET WS-IBGR-FOUND TO TRUE                             ELXPMCBI
01158             MOVE IBGR-CF-LIST-MTCH (IBGR-IDX) TO                  ELXPMCBI
01159                           ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)        ELXPMCBI
01160          END-IF                                                   ELXPMCBI
01161          END-PERFORM.                                             ELXPMCBI
01162      IF WS-IBGR-NOT-FOUND                                         ELXPMCBI
01163         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBI
01164         MOVE +4804 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBI
01165                                                                   ELXPMCBI
01166 ************************************************************      ELXPMCBI
01167 *                                                          *      ELXPMCBI
01168 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCBI
01169 *                                                          *      ELXPMCBI
01170 ************************************************************      ELXPMCBI
01171  9130-FND-INTRNL-DSCRPTR-CF.                                      ELXPMCBI
01172                                                                   ELXPMCBI
01173      SET WS-INTRNLDSC-NOT-FOUND TO TRUE.                          ELXPMCBI
01174      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELXPMCBI
01175      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELXPMCBI
01176         UNTIL CFT5-IDX > CFT5-MAX-IDX OR                          ELXPMCBI
01177           WS-INTRNLDSC-FOUND                                      ELXPMCBI
01178         IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) =                  ELXPMCBI
01179                           CFT5-INTD (CFT5-IDX)                    ELXPMCBI
01180            SET WS-INTRNLDSC-FOUND TO TRUE                         ELXPMCBI
01181            SET WS-SUB-WORK TO CFT5-IDX                            ELXPMCBI
01182            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCBI
01183            SET CFT5-IDX TO WS-SUB-WORK                            ELXPMCBI
01184         END-IF                                                    ELXPMCBI
01185      END-PERFORM.                                                 ELXPMCBI
01186      IF WS-INTRNLDSC-NOT-FOUND                                    ELXPMCBI
01187         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBI
01188         MOVE +4805 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBI
01189                                                                   ELXPMCBI
01190 ************************************************************      ELXPMCBI
01191 *                                                          *      ELXPMCBI
01192 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTOR AND SCAN FOR    *      ELXPMCBI
01193 *    APPLICABLE ENTRY.                                     *      ELXPMCBI
01194 *                                                          *      ELXPMCBI
01195 ************************************************************      ELXPMCBI
01196  9200-RCMPT-SP-SCN-APLCBL-ENTRY.                                  ELXPMCBI
01197                                                                   ELXPMCBI
01198      CALL 'ELKSPCFF'  USING ATBL-ACCUMULATOR-TABLE.               ELXPMCBI
01199      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCBI
01200      IF WS-SUCCESSFUL-CALL                                        ELXPMCBI
01201         MOVE ZERO TO WS-SAVE-SUB-BSC                              ELXPMCBI
01202         MOVE WS-CF-FALSE TO WS-TEST-CONF-BSC                      ELXPMCBI
01203         MOVE ZERO TO WS-APPL-ENTRS-BSC                            ELXPMCBI
01204         MOVE ZERO TO WS-SAVE-SUB-MM                               ELXPMCBI
01205         MOVE WS-CF-FALSE TO WS-TEST-CONF-MM                       ELXPMCBI
01206         MOVE ZERO TO WS-APPL-ENTRS-MM                             ELXPMCBI
01207         PERFORM 9220-RCMPT-CRNT-ACCM-TBL-CF                       ELXPMCBI
01208            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCBI
01209               UNTIL ATBL-IDX > ATBL-MAX-IDX                       ELXPMCBI
01210      ELSE                                                         ELXPMCBI
01211         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBI
01212         MOVE +4806 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBI
01213                                                                   ELXPMCBI
01214                                                                   ELXPMCBI
01215 ************************************************************      ELXPMCBI
01216 *                                                          *      ELXPMCBI
01217 *  RECOMPUTE CURRENT ACCUMULATOR TABLE WORK CONF FACTOR    *      ELXPMCBI
01218 *                                                          *      ELXPMCBI
01219 ************************************************************      ELXPMCBI
01220  9220-RCMPT-CRNT-ACCM-TBL-CF.                                     ELXPMCBI
01221                                                                   ELXPMCBI
01222      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCBI
01223         IF PMCI-INSTITUTIONAL                                     ELXPMCBI
01224            IF PMCI-INPATIENT                                      ELXPMCBI
01225               PERFORM 9230-RCMPT-INST-INP                         ELXPMCBI
01226            ELSE                                                   ELXPMCBI
01227               PERFORM 9240-RCMPT-INST-OUT                         ELXPMCBI
01228         ELSE                                                      ELXPMCBI
01229            IF PMCI-INPATIENT                                      ELXPMCBI
01230               PERFORM 9250-RCMPT-PROF-INP                         ELXPMCBI
01231            ELSE                                                   ELXPMCBI
01232               PERFORM 9260-RCMPT-PROF-OUT.                        ELXPMCBI
01233      IF PMCI-BSC-CNTRCT-GRP EQUAL SPACE                           ELXPMCBI
01234         IF PMCI-INSTITUTIONAL                                     ELXPMCBI
01235            IF PMCI-INPATIENT                                      ELXPMCBI
01236               PERFORM 9235-RCMPT-INST-INP-MM                      ELXPMCBI
01237            ELSE                                                   ELXPMCBI
01238               PERFORM 9245-RCMPT-INST-OUT-MM                      ELXPMCBI
01239         ELSE                                                      ELXPMCBI
01240            IF PMCI-INPATIENT                                      ELXPMCBI
01241               PERFORM 9255-RCMPT-PROF-INP-MM                      ELXPMCBI
01242            ELSE                                                   ELXPMCBI
01243               PERFORM 9265-RCMPT-PROF-OUT-MM.                     ELXPMCBI
01244                                                                   ELXPMCBI
01245 ************************************************************      ELXPMCBI
01246 *                                                          *      ELXPMCBI
01247 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01248 *                                                          *      ELXPMCBI
01249 ************************************************************      ELXPMCBI
01250  9230-RCMPT-INST-INP.                                             ELXPMCBI
01251                                                                   ELXPMCBI
01252      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01253                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01254                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBI
01255                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBI
01256                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01257                             ATBL-CF-SP (ATBL-IDX).                ELXPMCBI
01258      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01259                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBI
01260         CONTINUE                                                  ELXPMCBI
01261      ELSE                                                         ELXPMCBI
01262         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01263                      WS-WT-INDVDL                                 ELXPMCBI
01264         COMPUTE WS-CF-2 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBI
01265                      WS-WT-INST-BAS                               ELXPMCBI
01266         COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBI
01267                      WS-WT-IP                                     ELXPMCBI
01268         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01269                      WS-WT-PLAN                                   ELXPMCBI
01270         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01271                      WS-WT-SP                                     ELXPMCBI
01272         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01273      END-IF.                                                      ELXPMCBI
01274                                                                   ELXPMCBI
01275      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01276         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBI
01277         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBI
01278            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBI
01279            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01280                                    WS-TEST-CONF-BSC.              ELXPMCBI
01281                                                                   ELXPMCBI
01282      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBI
01283         PERFORM 9235-RCMPT-INST-INP-MM.                           ELXPMCBI
01284                                                                   ELXPMCBI
01285 ************************************************************      ELXPMCBI
01286 *                                                          *      ELXPMCBI
01287 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01288 *  IF MAJOR MEDICAL CONTRACT                               *      ELXPMCBI
01289 ************************************************************      ELXPMCBI
01290  9235-RCMPT-INST-INP-MM.                                          ELXPMCBI
01291                                                                   ELXPMCBI
01292 * PROCESS MAJOR MEDICAL CALCULATIONS.                             ELXPMCBI
01293      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01294                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01295                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBI
01296                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBI
01297                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01298                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01299      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01300                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBI
01301         CONTINUE                                                  ELXPMCBI
01302      ELSE                                                         ELXPMCBI
01303         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01304                      WS-WT-INDVDL                                 ELXPMCBI
01305         COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBI
01306                      WS-WT-INST-SUP                               ELXPMCBI
01307         COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCBI
01308                      WS-WT-IP                                     ELXPMCBI
01309         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01310                      WS-WT-PLAN                                   ELXPMCBI
01311         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01312                      WS-WT-SP                                     ELXPMCBI
01313         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01314      END-IF.                                                      ELXPMCBI
01315                                                                   ELXPMCBI
01316      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01317         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBI
01318         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBI
01319            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBI
01320            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01321                                    WS-TEST-CONF-MM.               ELXPMCBI
01322                                                                   ELXPMCBI
01323 ************************************************************      ELXPMCBI
01324 *                                                          *      ELXPMCBI
01325 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01326 *                                                          *      ELXPMCBI
01327 ************************************************************      ELXPMCBI
01328  9240-RCMPT-INST-OUT.                                             ELXPMCBI
01329                                                                   ELXPMCBI
01330      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01331                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01332                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCBI
01333                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBI
01334                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01335                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01336      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01337                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBI
01338         CONTINUE                                                  ELXPMCBI
01339         ELSE                                                      ELXPMCBI
01340         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01341                      WS-WT-INDVDL                                 ELXPMCBI
01342         COMPUTE WS-CF-2 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCBI
01343                      WS-WT-INST-BAS                               ELXPMCBI
01344         COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBI
01345                      WS-WT-OP                                     ELXPMCBI
01346         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01347                       WS-WT-PLAN                                  ELXPMCBI
01348         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01349                      WS-WT-SP                                     ELXPMCBI
01350         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01351         END-IF.                                                   ELXPMCBI
01352                                                                   ELXPMCBI
01353      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01354         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBI
01355         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBI
01356            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBI
01357            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01358                                    WS-TEST-CONF-BSC.              ELXPMCBI
01359                                                                   ELXPMCBI
01360      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBI
01361         PERFORM 9245-RCMPT-INST-OUT-MM.                           ELXPMCBI
01362 ************************************************************      ELXPMCBI
01363 *                                                          *      ELXPMCBI
01364 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01365 *  MAJOR MEDICAL CONTRACTS ONLY.                           *      ELXPMCBI
01366 ************************************************************      ELXPMCBI
01367  9245-RCMPT-INST-OUT-MM.                                          ELXPMCBI
01368                                                                   ELXPMCBI
01369                                                                   ELXPMCBI
01370 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBI
01371      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01372                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01373                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCBI
01374                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBI
01375                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01376                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01377      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01378                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCBI
01379         CONTINUE                                                  ELXPMCBI
01380         ELSE                                                      ELXPMCBI
01381         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01382                      WS-WT-INDVDL                                 ELXPMCBI
01383         COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCBI
01384                      WS-WT-INST-SUP                               ELXPMCBI
01385         COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBI
01386                      WS-WT-OP                                     ELXPMCBI
01387         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01388                       WS-WT-PLAN                                  ELXPMCBI
01389         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01390                      WS-WT-SP                                     ELXPMCBI
01391         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01392      END-IF.                                                      ELXPMCBI
01393                                                                   ELXPMCBI
01394      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01395         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBI
01396         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBI
01397            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBI
01398            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01399                                    WS-TEST-CONF-MM.               ELXPMCBI
01400                                                                   ELXPMCBI
01401 ************************************************************      ELXPMCBI
01402 *                                                          *      ELXPMCBI
01403 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01404 *                                                          *      ELXPMCBI
01405 ************************************************************      ELXPMCBI
01406  9250-RCMPT-PROF-INP.                                             ELXPMCBI
01407                                                                   ELXPMCBI
01408      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01409                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01410                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBI
01411                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBI
01412                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01413                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01414      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01415                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBI
01416         CONTINUE                                                  ELXPMCBI
01417      ELSE                                                         ELXPMCBI
01418          COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBI
01419                      WS-WT-INDVDL                                 ELXPMCBI
01420          COMPUTE WS-CF-2 = ATBL-CF-PROF-BAS (ATBL-IDX) *          ELXPMCBI
01421                      WS-WT-PROF-BAS                               ELXPMCBI
01422          COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBI
01423                      WS-WT-IP                                     ELXPMCBI
01424          COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBI
01425                      WS-WT-PLAN                                   ELXPMCBI
01426          COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBI
01427                      WS-WT-SP                                     ELXPMCBI
01428          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBI
01429      END-IF.                                                      ELXPMCBI
01430                                                                   ELXPMCBI
01431      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01432         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBI
01433         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBI
01434            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBI
01435            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01436                                    WS-TEST-CONF-BSC.              ELXPMCBI
01437                                                                   ELXPMCBI
01438      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBI
01439         PERFORM 9255-RCMPT-PROF-INP-MM.                           ELXPMCBI
01440                                                                   ELXPMCBI
01441 ************************************************************      ELXPMCBI
01442 *                                                          *      ELXPMCBI
01443 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01444 *   MAJOR MEDICAL CONTRACTS ONLY                           *      ELXPMCBI
01445 ************************************************************      ELXPMCBI
01446  9255-RCMPT-PROF-INP-MM.                                          ELXPMCBI
01447                                                                   ELXPMCBI
01448 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBI
01449                                                                   ELXPMCBI
01450      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01451                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01452                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBI
01453                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCBI
01454                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01455                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01456      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01457                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCBI
01458         CONTINUE                                                  ELXPMCBI
01459      ELSE                                                         ELXPMCBI
01460          COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCBI
01461                      WS-WT-INDVDL                                 ELXPMCBI
01462          COMPUTE WS-CF-2 = ATBL-CF-PROF-SUP (ATBL-IDX) *          ELXPMCBI
01463                      WS-WT-PROF-SUP                               ELXPMCBI
01464          COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCBI
01465                      WS-WT-IP                                     ELXPMCBI
01466          COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCBI
01467                      WS-WT-PLAN                                   ELXPMCBI
01468          COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCBI
01469                      WS-WT-SP                                     ELXPMCBI
01470          PERFORM 9300-COMBINE-FACTORS                             ELXPMCBI
01471      END-IF.                                                      ELXPMCBI
01472                                                                   ELXPMCBI
01473      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01474         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBI
01475         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBI
01476            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBI
01477            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01478                                    WS-TEST-CONF-MM.               ELXPMCBI
01479                                                                   ELXPMCBI
01480 ************************************************************      ELXPMCBI
01481 *                                                          *      ELXPMCBI
01482 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01483 *                                                          *      ELXPMCBI
01484 ************************************************************      ELXPMCBI
01485  9260-RCMPT-PROF-OUT.                                             ELXPMCBI
01486                                                                   ELXPMCBI
01487      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01488                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01489                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCBI
01490                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBI
01491                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01492                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01493      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01494                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBI
01495         CONTINUE                                                  ELXPMCBI
01496      ELSE                                                         ELXPMCBI
01497         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01498                      WS-WT-INDVDL                                 ELXPMCBI
01499         COMPUTE WS-CF-2 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCBI
01500                      WS-WT-PROF-BAS                               ELXPMCBI
01501         COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBI
01502                      WS-WT-OP                                     ELXPMCBI
01503         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01504                      WS-WT-PLAN                                   ELXPMCBI
01505         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01506                      WS-WT-SP                                     ELXPMCBI
01507         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01508      END-IF.                                                      ELXPMCBI
01509                                                                   ELXPMCBI
01510      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01511         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCBI
01512         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCBI
01513            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCBI
01514            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01515                                    WS-TEST-CONF-BSC.              ELXPMCBI
01516                                                                   ELXPMCBI
01517      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCBI
01518         PERFORM 9265-RCMPT-PROF-OUT-MM.                           ELXPMCBI
01519                                                                   ELXPMCBI
01520 ************************************************************      ELXPMCBI
01521 *                                                          *      ELXPMCBI
01522 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCBI
01523 *  MAJOR MEDICAL CONTRACTS ONLY                            *      ELXPMCBI
01524 ************************************************************      ELXPMCBI
01525  9265-RCMPT-PROF-OUT-MM.                                          ELXPMCBI
01526                                                                   ELXPMCBI
01527 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCBI
01528                                                                   ELXPMCBI
01529      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01530                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCBI
01531                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCBI
01532                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCBI
01533                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCBI
01534                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCBI
01535      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCBI
01536                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCBI
01537         CONTINUE                                                  ELXPMCBI
01538      ELSE                                                         ELXPMCBI
01539         COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCBI
01540                      WS-WT-INDVDL                                 ELXPMCBI
01541         COMPUTE WS-CF-2 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCBI
01542                      WS-WT-PROF-SUP                               ELXPMCBI
01543         COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCBI
01544                      WS-WT-OP                                     ELXPMCBI
01545         COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCBI
01546                      WS-WT-PLAN                                   ELXPMCBI
01547         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCBI
01548                      WS-WT-SP                                     ELXPMCBI
01549         PERFORM 9300-COMBINE-FACTORS                              ELXPMCBI
01550      END-IF.                                                      ELXPMCBI
01551                                                                   ELXPMCBI
01552      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCBI
01553         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCBI
01554         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCBI
01555            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCBI
01556            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCBI
01557                                    WS-TEST-CONF-MM.               ELXPMCBI
01558 ************************************************************      ELXPMCBI
01559 *                                                          *      ELXPMCBI
01560 *  COMBINE FACTORS                                         *      ELXPMCBI
01561 *                                                          *      ELXPMCBI
01562 ************************************************************      ELXPMCBI
01563  9300-COMBINE-FACTORS.                                            ELXPMCBI
01564                                                                   ELXPMCBI
01565      CALL 'ELKFLCMB'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCBI
01566                            WS-CF-1                                ELXPMCBI
01567                            WS-CF-2                                ELXPMCBI
01568                            WS-CF-3                                ELXPMCBI
01569                            WS-CF-4                                ELXPMCBI
01570                            WS-CF-5.                               ELXPMCBI
01571                                                                   ELXPMCBI
01572 ************************************************************      ELXPMCBI
01573 *                                                          *      ELXPMCBI
01574 *  DETERMINE IF BASIC OR MAJOR MEDICAL BENEFIT APPLIES     *      ELXPMCBI
01575 *                                                          *      ELXPMCBI
01576 ************************************************************      ELXPMCBI
01577                                                                   ELXPMCBI
01578  9500-DETERMINE-BASIC-MM.                                         ELXPMCBI
01579                                                                   ELXPMCBI
01580      MOVE SPACES TO WS-SAVE-LOB-IND.                              ELXPMCBI
01581      IF WS-SAVE-SUB-BSC NOT EQUAL ZERO AND WS-SAVE-SUB-MM         ELXPMCBI
01582               NOT EQUAL ZERO                                      ELXPMCBI
01583         MOVE '+' TO WS-SAVE-LOB-IND                               ELXPMCBI
01584         MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                       ELXPMCBI
01585         MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS               ELXPMCBI
01586      ELSE                                                         ELXPMCBI
01587         IF WS-SAVE-SUB-MM NOT EQUAL ZERO                          ELXPMCBI
01588            MOVE '*' TO WS-SAVE-LOB-IND                            ELXPMCBI
01589            MOVE WS-SAVE-SUB-MM TO WS-SAVE-SUB                     ELXPMCBI
01590            MOVE WS-APPL-ENTRS-MM TO WS-NUM-APPL-ENTRS             ELXPMCBI
01591         ELSE                                                      ELXPMCBI
01592            MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                    ELXPMCBI
01593            MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS.           ELXPMCBI
