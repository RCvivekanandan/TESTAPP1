00001 *   LINECOUNT(100),XREF(SHORT),NOLIST,NOMAP                       08/26/05
00002  IDENTIFICATION DIVISION.                                         ELXPMCAB
00003                                                                      LV003
00004  PROGRAM-ID.         ELXPMCAB                                     ELXPMCAB
00005                                                                   ELXPMCAB
00006  AUTHOR.             BARBARA KEIB                                 ELXPMCAB
00007                                                                   ELXPMCAB
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCAB
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCAB
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCAB
00011                      233 N. MICHIGAN AVE                          ELXPMCAB
00012                      CHICAGO, ILLINOIS 60601                      ELXPMCAB
00013                                                                   ELXPMCAB
00014  DATE-WRITTEN.       07-JAN-1993.                                 ELXPMCAB
00015                                                                   ELXPMCAB
00016  DATE-COMPILED.                                                   ELXPMCAB
00017                                                                   ELXPMCAB
00018  SECURITY.           COPYRIGHT 1993,                              ELXPMCAB
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCAB
00020      SKIP3                                                        ELXPMCAB
00021  ENVIRONMENT DIVISION.                                            ELXPMCAB
00022                                                                   ELXPMCAB
00023                                                                   ELXPMCAB
00024  CONFIGURATION SECTION.                                           ELXPMCAB
00025  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCAB
00026  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCAB
00027      EJECT                                                        ELXPMCAB
00028 ******************************************************************ELXPMCAB
00029 *                                                                *ELXPMCAB
00030 *      EXTRACT BENEFIT ACCUMULATOR DATA FOR ELIGIBILITY SUMMARY  *ELXPMCAB
00031 *                                                                *ELXPMCAB
00032 ******************************************************************ELXPMCAB
00033 *                      MAINTENANCE HISTORY                       *ELXPMCAB
00034 *                                                                *ELXPMCAB
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCAB
00036 * ----- ----------- --- ----- ---------------------------------- *ELXPMCAB
00037 * 01.00 07-JAN-1993 BAK       CREATED                            *ELXPMCAB
00038 * 01.01 24-FEB-1993 BAK       ADD DAY/NIGHT PSYCH FOR PROF INPAT.*ELXPMCAB
00039 *                             CORRECTED ERRORS IN 7000 ROUTINES. *ELXPMCAB
00040 * 02.00 01-JUN-1993 BAK       ADD MAJOR MEDICAL CONTRACT SUPPORT *ELXPMCAB
00041 *                             ISSR # 13071                       *ELXPMCAB
00042 * 02.01 14-JUN-1993 AKK       REMOVED UNNECESSARY PERIODS IN     *ELXPMCAB
00043 *                             ORDER TO AVOID HAVOC EX. PARAGRAPH *ELXPMCAB
00044 *                             9320.                              *ELXPMCAB
00045 * 02.02 29-JUN-1993 BAK       ADD CALLS FOR VISIT, UNITS AND     *ELXPMCAB
00046 *                             CONFINEMENTS FOR ALL 'DAY' GROUPS  *ELXPMCAB
00047 * 02.03 23-AUG-1993 BAK       FIX MENTAL DAY & DRB CONF FACTORS  *ELXPMCAB
00048 *                             MOVE CORRECT VALUE LIMIT FOR FIELDS*ELXPMCAB
00049 *                             THAT DO NOT CONTAIN DOLLAR VALUES. *ELXPMCAB
00050 ******************************************************************ELXPMCAB
00051                                                                   ELXPMCAB
00052                                                                   ELXPMCAB
00053      EJECT                                                        ELXPMCAB
00054  DATA DIVISION.                                                   ELXPMCAB
00055  WORKING-STORAGE SECTION.                                         ELXPMCAB
00056  01  FILLER                     PICTURE X(32)                     ELXPMCAB
00057           VALUE '****ELXPMCAB WORKING STORAGE****'.               ELXPMCAB
00058                                                                   ELXPMCAB
00059  01  WS-RETURN-CODE               PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00060                                                                   ELXPMCAB
00061      88  WS-SUCCESSFUL-CALL               VALUE ZERO.             ELXPMCAB
00062      88  WS-UNIDENT-PARM                  VALUE +8.               ELXPMCAB
00063      88  WS-MISSING-PARM                  VALUE +12.              ELXPMCAB
00064      88  WS-INTERNAL-ERROR                VALUE +16.              ELXPMCAB
00065                                                                   ELXPMCAB
00066  01  WS-SAVE-SUBSCRIPTS.                                          ELXPMCAB
00067                                                                   ELXPMCAB
00068      05  WS-SAVE-SUB              PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00069      05  WS-SAVE-SUB-BSC          PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00070      05  WS-SAVE-SUB-MM           PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00071      05  WS-ALC-SUB               PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00072      05  WS-DRG-SUB               PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00073      05  WS-LFM-DLR-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00074      05  WS-LFM-DAY-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00075      05  WS-OTR-DLR-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00076      05  WS-OTR-DAY-SUB           PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00077      05  WS-SUB-WORK              PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00078                                                                   ELXPMCAB
00079                                                                   ELXPMCAB
00080  01  WS-NUM-APPL-ENTRS            PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00081  01  WS-APPL-ENTRS-BSC            PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00082  01  WS-APPL-ENTRS-MM             PIC S9(04) COMP VALUE ZERO.     ELXPMCAB
00083  01  WS-BNF-QUAL                  PIC X(01) VALUE SPACES.         ELXPMCAB
00084  01  WS-BNF-PERD                  PIC X(02) VALUE SPACES.         ELXPMCAB
00085  01  WS-SAVE-LOB-IND              PIC X(01) VALUE SPACES.         ELXPMCAB
00086                                                                   ELXPMCAB
00087                                                                   ELXPMCAB
00088  01  WS-SWITCHES.                                                 ELXPMCAB
00089                                                                   ELXPMCAB
00090    05  WS-PPO-VARIATION-CONTROL   PIC X(01) VALUE 'N'.            ELXPMCAB
00091      88  WS-PPO-VAR-FOUND                 VALUE 'Y'.              ELXPMCAB
00092      88  WS-PPO-VAR-NOT-FOUND             VALUE 'N'.              ELXPMCAB
00093                                                                   ELXPMCAB
00094    05  WS-IBGR-SLOT-CONTROL       PIC X(01) VALUE 'N'.            ELXPMCAB
00095      88  WS-IBGR-FOUND                    VALUE 'Y'.              ELXPMCAB
00096      88  WS-IBGR-NOT-FOUND                VALUE 'N'.              ELXPMCAB
00097                                                                   ELXPMCAB
00098    05  WS-BNFTPRD-CONTROL         PIC X(01) VALUE 'N'.            ELXPMCAB
00099      88  WS-BNFTPRD-FOUND                 VALUE 'Y'.              ELXPMCAB
00100      88  WS-BNFTPRD-NOT-FOUND             VALUE 'N'.              ELXPMCAB
00101                                                                   ELXPMCAB
00102    05  WS-INTRNLDSC-CONTORL       PIC X(01) VALUE 'N'.            ELXPMCAB
00103      88  WS-INTRNLDSC-FOUND               VALUE 'Y'.              ELXPMCAB
00104      88  WS-INTRNLDSC-NOT-FOUND           VALUE 'N'.              ELXPMCAB
00105                                                                   ELXPMCAB
00106  01  WS-WEIGHTS.                                                  ELXPMCAB
00107      02  WS-WT-BNFT-PRD         COMP-1    VALUE 0.750000E+00.     ELXPMCAB
00108      02  WS-WT-INDVDL           COMP-1    VALUE 0.500000E+00.     ELXPMCAB
00109      02  WS-WT-INST-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00110      02  WS-WT-PROF-BAS         COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00111      02  WS-WT-INST-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00112      02  WS-WT-PROF-SUP         COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00113      02  WS-WT-IP               COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00114      02  WS-WT-OP               COMP-1    VALUE 0.100000E+00.     ELXPMCAB
00115      02  WS-WT-PLAN             COMP-1    VALUE 0.250000E+00.     ELXPMCAB
00116      02  WS-WT-SP               COMP-1    VALUE 0.500000E+00.     ELXPMCAB
00117                                                                   ELXPMCAB
00118  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCAB
00119      02  WS-CF-ZERO             COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00120      02  WS-CF-TRUE             COMP-1    VALUE +1.000000E+00.    ELXPMCAB
00121      02  WS-CF-FALSE            COMP-1    VALUE -1.000000E+00.    ELXPMCAB
00122      02  WS-CF-75               COMP-1    VALUE +0.750000E+00.    ELXPMCAB
00123      02  WS-CF-85               COMP-1    VALUE +0.850000E+00.    ELXPMCAB
00124      02  WS-CF-95               COMP-1    VALUE +0.950000E+00.    ELXPMCAB
00125      02  WS-TEST-CONF-FACT      COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00126      02  WS-TEST-CONF-BSC       COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00127      02  WS-TEST-CONF-MM        COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00128      02  WS-TEST-THRESHOLD      COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00129      02  WS-NO-IBGR-CF          COMP-1    VALUE +0.000000E+00.    ELXPMCAB
00130                                                                   ELXPMCAB
00131  01  WS-CONFIDENCE-WORK.                                          ELXPMCAB
00132      02  WS-CF-1                COMP-1.                           ELXPMCAB
00133      02  WS-CF-2                COMP-1.                           ELXPMCAB
00134      02  WS-CF-3                COMP-1.                           ELXPMCAB
00135      02  WS-CF-4                COMP-1.                           ELXPMCAB
00136      02  WS-CF-5                COMP-1.                           ELXPMCAB
00137      02  WS-CF-6                COMP-1.                           ELXPMCAB
00138                                                                   ELXPMCAB
00139 * INSTITUTIONAL INPATIENT                                         ELXPMCAB
00140                                                                   ELXPMCAB
00141  01  WS-INST-IP-ALC.                                              ELXPMCAB
00142      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCAB
00143      02  FILLER                 PIC X(06) VALUE 'ARPI W'.         ELXPMCAB
00144      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCAB
00145                                                                   ELXPMCAB
00146  01  WS-INST-IP-DPSY.                                             ELXPMCAB
00147      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00148      02  FILLER                 PIC X(06) VALUE 'DPSY A'.         ELXPMCAB
00149                                                                   ELXPMCAB
00150  01  WS-INST-IP-DRB.                                              ELXPMCAB
00151      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00152      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCAB
00153                                                                   ELXPMCAB
00154  01  WS-INST-IP-DRG.                                              ELXPMCAB
00155      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCAB
00156      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCAB
00157      02  FILLER                 PIC X(06) VALUE 'DRPI W'.         ELXPMCAB
00158                                                                   ELXPMCAB
00159  01  WS-INST-IP-NPSY.                                             ELXPMCAB
00160      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00161      02  FILLER                 PIC X(06) VALUE 'NPSY A'.         ELXPMCAB
00162                                                                   ELXPMCAB
00163  01  WS-INST-IP-PDN.                                              ELXPMCAB
00164      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00165      02  FILLER                 PIC X(06) VALUE 'NRSI B'.         ELXPMCAB
00166                                                                   ELXPMCAB
00167  01  WS-INST-IP-PSYS.                                             ELXPMCAB
00168      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCAB
00169      02  FILLER                 PIC X(06) VALUE 'DRB  A'.         ELXPMCAB
00170      02  FILLER                 PIC X(06) VALUE 'PSYI W'.         ELXPMCAB
00171                                                                   ELXPMCAB
00172 * INSTITUTIONAL OUTPATIENT                                        ELXPMCAB
00173                                                                   ELXPMCAB
00174  01  WS-INST-OP-ALC.                                              ELXPMCAB
00175      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00176      02  FILLER                 PIC X(06) VALUE 'ARPO W'.         ELXPMCAB
00177                                                                   ELXPMCAB
00178  01  WS-INST-OP-DRG.                                              ELXPMCAB
00179      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00180      02  FILLER                 PIC X(06) VALUE 'DRPO W'.         ELXPMCAB
00181                                                                   ELXPMCAB
00182  01  WS-INST-OP-PDN.                                              ELXPMCAB
00183      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00184      02  FILLER                 PIC X(06) VALUE 'NRSO B'.         ELXPMCAB
00185                                                                   ELXPMCAB
00186  01  WS-INST-OP-PSYS.                                             ELXPMCAB
00187      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00188      02  FILLER                 PIC X(06) VALUE 'PSYO W'.         ELXPMCAB
00189                                                                   ELXPMCAB
00190 * PROFESSIONAL INPATIENT                                          ELXPMCAB
00191                                                                   ELXPMCAB
00192  01  WS-PROF-IP-ALC.                                              ELXPMCAB
00193      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00194      02  FILLER                 PIC X(06) VALUE 'AHI  D'.         ELXPMCAB
00195                                                                   ELXPMCAB
00196  01  WS-PROF-IP-DPV.                                              ELXPMCAB
00197      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00198      02  FILLER                 PIC X(06) VALUE 'DPV  D'.         ELXPMCAB
00199                                                                   ELXPMCAB
00200  01  WS-PROF-IP-DRG.                                              ELXPMCAB
00201      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00202      02  FILLER                 PIC X(06) VALUE 'DRI  D'.         ELXPMCAB
00203                                                                   ELXPMCAB
00204  01  WS-PROF-IP-NPV.                                              ELXPMCAB
00205      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00206      02  FILLER                 PIC X(06) VALUE 'NPV  D'.         ELXPMCAB
00207                                                                   ELXPMCAB
00208  01  WS-PROF-IP-PDN.                                              ELXPMCAB
00209      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00210      02  FILLER                 PIC X(06) VALUE 'NRSI E'.         ELXPMCAB
00211                                                                   ELXPMCAB
00212  01  WS-PROF-IP-PSYS.                                             ELXPMCAB
00213      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00214      02  FILLER                 PIC X(06) VALUE 'MNI  D'.         ELXPMCAB
00215                                                                   ELXPMCAB
00216 * PROFESSIONAL OUTPATIENT                                         ELXPMCAB
00217                                                                   ELXPMCAB
00218  01  WS-PROF-OP-PDN.                                              ELXPMCAB
00219      02  FILLER                 PIC S9(04) COMP VALUE +1.         ELXPMCAB
00220      02  FILLER                 PIC X(06) VALUE 'NRSO E'.         ELXPMCAB
00221                                                                   ELXPMCAB
00222  01  WS-PROF-OP-PSYS.                                             ELXPMCAB
00223      02  FILLER                 PIC S9(04) COMP VALUE +2.         ELXPMCAB
00224      02  FILLER                 PIC X(06) VALUE 'GPO  E'.         ELXPMCAB
00225      02  FILLER                 PIC X(06) VALUE 'IPO  E'.         ELXPMCAB
00226                                                                   ELXPMCAB
00227  01  WS-POINTERS.                                                 ELXPMCAB
00228      02  WS-ALAB-POINTER        POINTER.                          ELXPMCAB
00229      02  WS-DLRB-POINTER        POINTER.                          ELXPMCAB
00230      02  WS-DPSY-POINTER        POINTER.                          ELXPMCAB
00231      02  WS-DRAB-POINTER        POINTER.                          ELXPMCAB
00232      02  WS-NPSY-POINTER        POINTER.                          ELXPMCAB
00233      02  WS-PRDN-POINTER        POINTER.                          ELXPMCAB
00234      02  WS-PSYS-POINTER        POINTER.                          ELXPMCAB
00235                                                                   ELXPMCAB
00236  01  WS-SWITCHES.                                                 ELXPMCAB
00237      05                         PIC X(01).                        ELXPMCAB
00238         88  SW-TRMNL-ERR                  VALUE 'Y'.              ELXPMCAB
00239         88  SW-NO-TRMNL-ERR               VALUE 'N'.              ELXPMCAB
00240                                                                   ELXPMCAB
00241  COPY ELSCFTB5 SUPPRESS.                                          ELXPMCAB
00242  COPY ELSCFTBA SUPPRESS.                                          ELXPMCAB
00243  COPY ELSCVG2C SUPPRESS.                                          ELXPMCAB
00244                                                                   ELXPMCAB
00245  01  FILLER                     PICTURE X(32)                     ELXPMCAB
00246           VALUE '*END ELXPMCAB WORKING STORAGE***'.               ELXPMCAB
00247      EJECT                                                        ELXPMCAB
00248  LINKAGE SECTION.                                                 ELXPMCAB
00249 *    EJECT                                                        ELXPMCAB
00250  COPY ELSCIA2C SUPPRESS.                                          ELXPMCAB
00251 *    EJECT                                                        ELXPMCAB
00252  COPY ELSCSACC SUPPRESS.                                          ELXPMCAB
00253 *    EJECT                                                        ELXPMCAB
00254  COPY ELSATBLC SUPPRESS.                                          ELXPMCAB
00255 *    EJECT                                                        ELXPMCAB
00256  COPY ELSPMCID SUPPRESS.                                          ELXPMCAB
00257 *    EJECT                                                        ELXPMCAB
00258  COPY ELSIBGRC SUPPRESS.                                          ELXPMCAB
00259 *    EJECT                                                        ELXPMCAB
00260  01  PMCI-COMM-AREA.                                              ELXPMCAB
00261  COPY PMCCOMM SUPPRESS.                                           ELXPMCAB
00262 *    EJECT                                                        ELXPMCAB
00263  COPY ELSBPVLC SUPPRESS.                                          ELXPMCAB
00264  01  LS-MATCH-LIST               PIC X.                           ELXPMCAB
00265 *    EJECT                                                        ELXPMCAB
00266  PROCEDURE DIVISION USING PMCI-COMM-AREA                          ELXPMCAB
00267                           NAES-INTERMEDIATE-DATA                  ELXPMCAB
00268                           CSAC-ACCUMULATOR-TABLE                  ELXPMCAB
00269                           IBGR-INTERNAL-TABS-TABLE.               ELXPMCAB
00270 ************************************************************      ELXPMCAB
00271 *                                                          *      ELXPMCAB
00272 *          MAINLINE ROUTINE                                *      ELXPMCAB
00273 *                                                          *      ELXPMCAB
00274 ************************************************************      ELXPMCAB
00275  0000-MAINLINE.                                                   ELXPMCAB
00276                                                                   ELXPMCAB
00277      IF ADDRESS OF PMCI-COMM-AREA = NULL                          ELXPMCAB
00278         NEXT SENTENCE                                             ELXPMCAB
00279      ELSE                                                         ELXPMCAB
00280         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCAB
00281         SET PMCI-BC-NO-ERROR TO TRUE                              ELXPMCAB
00282         SET SW-NO-TRMNL-ERR TO TRUE                               ELXPMCAB
00283         IF ADDRESS OF CSAC-ACCUMULATOR-TABLE = NULL               ELXPMCAB
00284            MOVE +4601 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCAB
00285            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCAB
00286         ELSE                                                      ELXPMCAB
00287            SET ADDRESS OF LS-MATCH-LIST TO NULL                   ELXPMCAB
00288            PERFORM 0100-EXTRCT-BNFT-MAXS.                         ELXPMCAB
00289                                                                   ELXPMCAB
00290      GOBACK.                                                      ELXPMCAB
00291                                                                   ELXPMCAB
00292 ************************************************************      ELXPMCAB
00293 *                                                          *      ELXPMCAB
00294 *        EXTRACT BENEFIT MAXIMUMS                          *      ELXPMCAB
00295 *                                                          *      ELXPMCAB
00296 ************************************************************      ELXPMCAB
00297  0100-EXTRCT-BNFT-MAXS.                                           ELXPMCAB
00298                                                                   ELXPMCAB
00299      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELXPMCAB
00300                      CSAC-ABM-GC-TBL-PTR.                         ELXPMCAB
00301      IF ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULLS                 ELXPMCAB
00302         SET DRB-NOT-APPLICABLE TO TRUE                            ELXPMCAB
00303         SET PMCI-MAX-COMBO-NONE TO TRUE                           ELXPMCAB
00304         SET PMCI-AL-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00305         SET PMCI-MD-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00306         SET DNPD-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00307         SET DNPN-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00308         SET PMCI-IQ-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00309         SET LFMD-UNLIMITED TO TRUE                                ELXPMCAB
00310         SET PMCI-MM-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00311         SET MNDD-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00312         SET MNDT-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00313         SET PRDN-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00314      ELSE                                                         ELXPMCAB
00315         PERFORM 0120-IDNTFY-AVLBL-BNFT-MAXS.                      ELXPMCAB
00316                                                                   ELXPMCAB
00317 ************************************************************      ELXPMCAB
00318 *                                                          *      ELXPMCAB
00319 *        IDENTIFY AVAILABLE BENEFIT MAXIMUMS               *      ELXPMCAB
00320 *                                                          *      ELXPMCAB
00321 ************************************************************      ELXPMCAB
00322  0120-IDNTFY-AVLBL-BNFT-MAXS.                                     ELXPMCAB
00323                                                                   ELXPMCAB
00324      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAB
00325      PERFORM 0190-INTLZ-SP                                        ELXPMCAB
00326            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAB
00327                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCAB
00328                                                                   ELXPMCAB
00329      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00330         IF PMCI-INSTITUTIONAL                                     ELXPMCAB
00331            IF PMCI-INPATIENT                                      ELXPMCAB
00332               PERFORM 1000-IDNTFY-INST-IP-MAXS                    ELXPMCAB
00333            ELSE                                                   ELXPMCAB
00334               PERFORM 2000-IDNTFY-INST-OP-MAXS                    ELXPMCAB
00335         ELSE                                                      ELXPMCAB
00336            IF PMCI-INPATIENT                                      ELXPMCAB
00337               PERFORM 3000-IDNTFY-PROF-IP-MAXS                    ELXPMCAB
00338            ELSE                                                   ELXPMCAB
00339               PERFORM 4000-IDNTFY-PROF-OP-MAXS.                   ELXPMCAB
00340                                                                   ELXPMCAB
00341 ************************************************************      ELXPMCAB
00342 *                                                          *      ELXPMCAB
00343 *        INITIALIZE SPECIAL CASE FACTORS                   *      ELXPMCAB
00344 *                                                          *      ELXPMCAB
00345 ************************************************************      ELXPMCAB
00346  0190-INTLZ-SP.                                                   ELXPMCAB
00347                                                                   ELXPMCAB
00348      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX) TO                          ELXPMCAB
00349                    ATBL-CF-SP-FCTRS (ATBL-IDX).                   ELXPMCAB
00350      IF PMCI-PPO-YES                                              ELXPMCAB
00351         SET WS-PPO-VAR-NOT-FOUND TO TRUE.                         ELXPMCAB
00352      EVALUATE TRUE                                                ELXPMCAB
00353         WHEN PMCI-PPO-PROV                                        ELXPMCAB
00354            PERFORM 0191-SCN-SP-CCP-PPO                            ELXPMCAB
00355         WHEN PMCI-NOT-PPO-PROV                                    ELXPMCAB
00356            PERFORM 0192-SCN-SP-CCP-NPPO                           ELXPMCAB
00357         WHEN PMCI-PPO-PROV-CALL                                   ELXPMCAB
00358            PERFORM 0193-SCN-SP-CCP-UPPO                           ELXPMCAB
00359         WHEN OTHER                                                ELXPMCAB
00360         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAB
00361         MOVE +4602 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
00362      END-EVALUATE.                                                ELXPMCAB
00363                                                                   ELXPMCAB
00364 ************************************************************      ELXPMCAB
00365 *                                                          *      ELXPMCAB
00366 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR PPO   *      ELXPMCAB
00367 *                                                          *      ELXPMCAB
00368 ************************************************************      ELXPMCAB
00369  0191-SCN-SP-CCP-PPO.                                             ELXPMCAB
00370                                                                   ELXPMCAB
00371      EVALUATE TRUE                                                ELXPMCAB
00372         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAB
00373             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00374         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAB
00375             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00376             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00377         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAB
00378             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00379             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00380         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAB
00381             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00382             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00383         WHEN OTHER                                                ELXPMCAB
00384             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00385      END-EVALUATE.                                                ELXPMCAB
00386                                                                   ELXPMCAB
00387 ************************************************************      ELXPMCAB
00388 *                                                          *      ELXPMCAB
00389 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-PPO   *      ELXPMCAB
00390 *                                                          *      ELXPMCAB
00391 ************************************************************      ELXPMCAB
00392  0192-SCN-SP-CCP-NPPO.                                            ELXPMCAB
00393                                                                   ELXPMCAB
00394      EVALUATE TRUE                                                ELXPMCAB
00395         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAB
00396             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00397         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAB
00398             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00399             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00400         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAB
00401             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00402             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00403         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAB
00404             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00405             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00406         WHEN OTHER                                                ELXPMCAB
00407             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00408      END-EVALUATE.                                                ELXPMCAB
00409                                                                   ELXPMCAB
00410 ************************************************************      ELXPMCAB
00411 *                                                          *      ELXPMCAB
00412 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PPO*      ELXPMCAB
00413 *                                                          *      ELXPMCAB
00414 ************************************************************      ELXPMCAB
00415  0193-SCN-SP-CCP-UPPO.                                            ELXPMCAB
00416                                                                   ELXPMCAB
00417      EVALUATE TRUE                                                ELXPMCAB
00418         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAB
00419             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00420         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAB
00421             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00422             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00423         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAB
00424             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00425             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00426         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAB
00427             SET WS-PPO-VAR-FOUND TO TRUE                          ELXPMCAB
00428             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAB
00429         WHEN OTHER                                                ELXPMCAB
00430             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAB
00431      END-EVALUATE.                                                ELXPMCAB
00432                                                                   ELXPMCAB
00433 ************************************************************      ELXPMCAB
00434 *                                                          *      ELXPMCAB
00435 * IDENTIFY INSTITUTIONAL INPATIENT MAXIMUMS                *      ELXPMCAB
00436 *                                                          *      ELXPMCAB
00437 ************************************************************      ELXPMCAB
00438  1000-IDNTFY-INST-IP-MAXS.                                        ELXPMCAB
00439                                                                   ELXPMCAB
00440      PERFORM 1001-INITIALIZE.                                     ELXPMCAB
00441      PERFORM 1100-IDNTFY-INST-IP-DRB-MAX.                         ELXPMCAB
00442      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00443         PERFORM 1200-IDNTFY-INST-IP-PDN-MAX                       ELXPMCAB
00444         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
00445            PERFORM 1300-IDNTFY-INST-IP-PSYCH-MAX                  ELXPMCAB
00446            PERFORM 1400-IDNTFY-INST-IP-SB-ABS-MAX.                ELXPMCAB
00447                                                                   ELXPMCAB
00448 ************************************************************      ELXPMCAB
00449 *                                                          *      ELXPMCAB
00450 * IDENTIFY INSTITUTIONAL INPATIENT MAXIMUMS/INITIALIZE     *      ELXPMCAB
00451 *                                                          *      ELXPMCAB
00452 ************************************************************      ELXPMCAB
00453  1001-INITIALIZE.                                                 ELXPMCAB
00454                                                                   ELXPMCAB
00455      CALL 'ELUADDRS' USING WS-INST-IP-DRB                         ELXPMCAB
00456                          WS-DLRB-POINTER.                         ELXPMCAB
00457      CALL 'ELUADDRS' USING WS-INST-IP-PDN                         ELXPMCAB
00458                          WS-PRDN-POINTER.                         ELXPMCAB
00459      CALL 'ELUADDRS' USING WS-INST-IP-PSYS                        ELXPMCAB
00460                          WS-PSYS-POINTER.                         ELXPMCAB
00461      CALL 'ELUADDRS' USING WS-INST-IP-DPSY                        ELXPMCAB
00462                          WS-NPSY-POINTER.                         ELXPMCAB
00463      CALL 'ELUADDRS' USING WS-INST-IP-NPSY                        ELXPMCAB
00464                          WS-DPSY-POINTER.                         ELXPMCAB
00465      CALL 'ELUADDRS' USING WS-INST-IP-ALC                         ELXPMCAB
00466                          WS-ALAB-POINTER.                         ELXPMCAB
00467      CALL 'ELUADDRS' USING WS-INST-IP-DRG                         ELXPMCAB
00468                          WS-DRAB-POINTER.                         ELXPMCAB
00469                                                                   ELXPMCAB
00470 ************************************************************      ELXPMCAB
00471 *                                                          *      ELXPMCAB
00472 * IDENTIFY INSTITUTIONAL INPATIENT DAILY ROOM/BOARD MAXIMUM*      ELXPMCAB
00473 *                                                          *      ELXPMCAB
00474 ************************************************************      ELXPMCAB
00475  1100-IDNTFY-INST-IP-DRB-MAX.                                     ELXPMCAB
00476                                                                   ELXPMCAB
00477      IF NAES-II-DRB-YES                                           ELXPMCAB
00478         PERFORM 5100-SRCH-DRB-MAX                                 ELXPMCAB
00479      ELSE                                                         ELXPMCAB
00480         SET DRB-NO-COVERAGE TO TRUE                               ELXPMCAB
00481         SET PRRT-NO-COVERAGE TO TRUE                              ELXPMCAB
00482         SET PMCI-IQ-NO-COVERAGE TO TRUE.                          ELXPMCAB
00483                                                                   ELXPMCAB
00484 ************************************************************      ELXPMCAB
00485 *                                                          *      ELXPMCAB
00486 * IDENTIFY INSTITUTIONAL INPATIENT PRIVATE DUTY NURSING MAX*      ELXPMCAB
00487 *                                                          *      ELXPMCAB
00488 ************************************************************      ELXPMCAB
00489  1200-IDNTFY-INST-IP-PDN-MAX.                                     ELXPMCAB
00490                                                                   ELXPMCAB
00491      IF PMCI-PDN-COVERED                                          ELXPMCAB
00492         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCAB
00493      ELSE                                                         ELXPMCAB
00494         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCAB
00495                                                                   ELXPMCAB
00496 ************************************************************      ELXPMCAB
00497 *                                                          *      ELXPMCAB
00498 * IDENTIFY INSTITUTIONAL INPATIENT PSYCH SERVICES MAXIMUMS *      ELXPMCAB
00499 *                                                          *      ELXPMCAB
00500 ************************************************************      ELXPMCAB
00501  1300-IDNTFY-INST-IP-PSYCH-MAX.                                   ELXPMCAB
00502                                                                   ELXPMCAB
00503      IF PSY-YES                                                   ELXPMCAB
00504         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCAB
00505      ELSE                                                         ELXPMCAB
00506         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCAB
00507         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCAB
00508         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCAB
00509         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCAB
00510      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00511         IF NAES-II-DPSY-YES OR NAES-II-NPSY-YES                   ELXPMCAB
00512            PERFORM 7000-SRCH-DAY-NGHT-PSYCH-MAX                   ELXPMCAB
00513         ELSE                                                      ELXPMCAB
00514            SET DNPD-NO-COVERAGE TO TRUE                           ELXPMCAB
00515            SET DNPN-NO-COVERAGE TO TRUE.                          ELXPMCAB
00516                                                                   ELXPMCAB
00517 ************************************************************      ELXPMCAB
00518 *                                                          *      ELXPMCAB
00519 * IDENTIFY INSTITUTIONAL INPATIENT SUBSTANCE ABUSE MAXIMUMS*      ELXPMCAB
00520 *                                                          *      ELXPMCAB
00521 ************************************************************      ELXPMCAB
00522  1400-IDNTFY-INST-IP-SB-ABS-MAX.                                  ELXPMCAB
00523                                                                   ELXPMCAB
00524      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCAB
00525         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCAB
00526      ELSE                                                         ELXPMCAB
00527         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCAB
00528         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCAB
00529                                                                   ELXPMCAB
00530 ************************************************************      ELXPMCAB
00531 *                                                          *      ELXPMCAB
00532 * IDENTIFY INSTITUTIONAL OUTPATIENT MAXIMUMS               *      ELXPMCAB
00533 *                                                          *      ELXPMCAB
00534 ************************************************************      ELXPMCAB
00535  2000-IDNTFY-INST-OP-MAXS.                                        ELXPMCAB
00536                                                                   ELXPMCAB
00537      PERFORM 2001-INITIALIZE.                                     ELXPMCAB
00538      PERFORM 2200-IDNTFY-INST-OP-PDN-MAX.                         ELXPMCAB
00539      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00540         PERFORM 2300-IDNTFY-INST-OP-PSYCH-MAX                     ELXPMCAB
00541         PERFORM 2400-IDNTFY-INST-OP-SB-ABS-MAX.                   ELXPMCAB
00542                                                                   ELXPMCAB
00543 ************************************************************      ELXPMCAB
00544 *                                                          *      ELXPMCAB
00545 * IDENTIFY INSTITUTIONAL OUTPATIENT MAXIMUMS/INITIALIZE    *      ELXPMCAB
00546 *                                                          *      ELXPMCAB
00547 ************************************************************      ELXPMCAB
00548  2001-INITIALIZE.                                                 ELXPMCAB
00549                                                                   ELXPMCAB
00550      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCAB
00551      SET WS-NPSY-POINTER TO NULL.                                 ELXPMCAB
00552      SET WS-DPSY-POINTER TO NULL.                                 ELXPMCAB
00553      CALL 'ELUADDRS' USING WS-INST-OP-PDN                         ELXPMCAB
00554                          WS-PRDN-POINTER.                         ELXPMCAB
00555      CALL 'ELUADDRS' USING WS-INST-OP-PSYS                        ELXPMCAB
00556                          WS-PSYS-POINTER.                         ELXPMCAB
00557      CALL 'ELUADDRS' USING WS-INST-OP-ALC                         ELXPMCAB
00558                          WS-ALAB-POINTER.                         ELXPMCAB
00559      CALL 'ELUADDRS' USING WS-INST-OP-DRG                         ELXPMCAB
00560                          WS-DRAB-POINTER.                         ELXPMCAB
00561      SET DRB-NOT-APPLICABLE TO TRUE.                              ELXPMCAB
00562      SET DNPD-NOT-APPLICABLE TO TRUE.                             ELXPMCAB
00563      SET DNPN-NOT-APPLICABLE TO TRUE.                             ELXPMCAB
00564                                                                   ELXPMCAB
00565 ************************************************************      ELXPMCAB
00566 *                                                          *      ELXPMCAB
00567 * IDENTIFY INSTITUTIONAL OUTPATIENT DAILY ROOM/BOARD MAXIMUM*     ELXPMCAB
00568 *                                                          *      ELXPMCAB
00569 ************************************************************      ELXPMCAB
00570  2200-IDNTFY-INST-OP-PDN-MAX.                                     ELXPMCAB
00571                                                                   ELXPMCAB
00572      IF PMCI-PDN-COVERED                                          ELXPMCAB
00573         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCAB
00574      ELSE                                                         ELXPMCAB
00575         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCAB
00576                                                                   ELXPMCAB
00577 ************************************************************      ELXPMCAB
00578 *                                                          *      ELXPMCAB
00579 * IDENTIFY INSTITUTIONAL OUTPATIENT PSYCH SERVICES MAXIMUMS*      ELXPMCAB
00580 *                                                          *      ELXPMCAB
00581 ************************************************************      ELXPMCAB
00582  2300-IDNTFY-INST-OP-PSYCH-MAX.                                   ELXPMCAB
00583                                                                   ELXPMCAB
00584      IF PSY-YES                                                   ELXPMCAB
00585         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCAB
00586      ELSE                                                         ELXPMCAB
00587         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCAB
00588         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCAB
00589         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCAB
00590         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCAB
00591                                                                   ELXPMCAB
00592 ************************************************************      ELXPMCAB
00593 *                                                          *      ELXPMCAB
00594 * IDENTIFY INSTITUTIONAL OUTPATIENT SUBSTANCE ABUSE MAXIMUMS*     ELXPMCAB
00595 *                                                          *      ELXPMCAB
00596 ************************************************************      ELXPMCAB
00597  2400-IDNTFY-INST-OP-SB-ABS-MAX.                                  ELXPMCAB
00598                                                                   ELXPMCAB
00599      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCAB
00600         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCAB
00601      ELSE                                                         ELXPMCAB
00602         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCAB
00603         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCAB
00604                                                                   ELXPMCAB
00605 ************************************************************      ELXPMCAB
00606 *                                                          *      ELXPMCAB
00607 * IDENTIFY PROFESSIONAL INPATIENT MAXIMUMS                 *      ELXPMCAB
00608 *                                                          *      ELXPMCAB
00609 ************************************************************      ELXPMCAB
00610  3000-IDNTFY-PROF-IP-MAXS.                                        ELXPMCAB
00611                                                                   ELXPMCAB
00612      PERFORM 3001-INITIALIZE.                                     ELXPMCAB
00613      PERFORM 3200-IDNTFY-PROF-IP-PDN-MAX.                         ELXPMCAB
00614      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00615         PERFORM 3300-IDNTFY-PROF-IP-PSYCH-MAX                     ELXPMCAB
00616         PERFORM 3400-IDNTFY-PROF-IP-SB-ABS-MAX.                   ELXPMCAB
00617                                                                   ELXPMCAB
00618 ************************************************************      ELXPMCAB
00619 *                                                          *      ELXPMCAB
00620 * IDENTIFY PROFESSIONAL INPATIENT MAXIMUMS/INITIALIZE      *      ELXPMCAB
00621 *                                                          *      ELXPMCAB
00622 ************************************************************      ELXPMCAB
00623  3001-INITIALIZE.                                                 ELXPMCAB
00624                                                                   ELXPMCAB
00625      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCAB
00626      CALL 'ELUADDRS' USING WS-PROF-IP-PDN                         ELXPMCAB
00627                          WS-PRDN-POINTER.                         ELXPMCAB
00628      CALL 'ELUADDRS' USING WS-PROF-IP-PSYS                        ELXPMCAB
00629                          WS-PSYS-POINTER.                         ELXPMCAB
00630      CALL 'ELUADDRS' USING WS-PROF-IP-ALC                         ELXPMCAB
00631                          WS-ALAB-POINTER.                         ELXPMCAB
00632      CALL 'ELUADDRS' USING WS-PROF-IP-DRG                         ELXPMCAB
00633                          WS-DRAB-POINTER.                         ELXPMCAB
00634      CALL 'ELUADDRS' USING WS-PROF-IP-DPV                         ELXPMCAB
00635                          WS-DPSY-POINTER.                         ELXPMCAB
00636      CALL 'ELUADDRS' USING WS-PROF-IP-NPV                         ELXPMCAB
00637                          WS-NPSY-POINTER.                         ELXPMCAB
00638      SET DRB-NOT-APPLICABLE TO TRUE.                              ELXPMCAB
00639                                                                   ELXPMCAB
00640 ************************************************************      ELXPMCAB
00641 *                                                          *      ELXPMCAB
00642 * IDENTIFY PROFESSIONAL INPATIENT PRIVATE DUTY NURSING MAX*       ELXPMCAB
00643 *                                                          *      ELXPMCAB
00644 ************************************************************      ELXPMCAB
00645  3200-IDNTFY-PROF-IP-PDN-MAX.                                     ELXPMCAB
00646                                                                   ELXPMCAB
00647      IF PMCI-PDN-COVERED                                          ELXPMCAB
00648         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCAB
00649      ELSE                                                         ELXPMCAB
00650         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCAB
00651                                                                   ELXPMCAB
00652 ************************************************************      ELXPMCAB
00653 *                                                          *      ELXPMCAB
00654 * IDENTIFY PROFESSIONAL INPATIENT PSYCH SERVICES MAXIMUMS *       ELXPMCAB
00655 *                                                          *      ELXPMCAB
00656 ************************************************************      ELXPMCAB
00657  3300-IDNTFY-PROF-IP-PSYCH-MAX.                                   ELXPMCAB
00658                                                                   ELXPMCAB
00659      IF PSY-YES                                                   ELXPMCAB
00660         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCAB
00661      ELSE                                                         ELXPMCAB
00662         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCAB
00663         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCAB
00664         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCAB
00665         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCAB
00666      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00667         IF NAES-PI-DPV-YES OR NAES-PI-NPV-YES                     ELXPMCAB
00668            PERFORM 7000-SRCH-DAY-NGHT-PSYCH-MAX                   ELXPMCAB
00669         ELSE                                                      ELXPMCAB
00670            SET DNPD-NO-COVERAGE TO TRUE                           ELXPMCAB
00671            SET DNPN-NO-COVERAGE TO TRUE.                          ELXPMCAB
00672                                                                   ELXPMCAB
00673 ************************************************************      ELXPMCAB
00674 *                                                          *      ELXPMCAB
00675 * IDENTIFY PROFESSIONAL INPATIENT SUBSTANCE ABUSE MAXIMUMS*       ELXPMCAB
00676 *                                                          *      ELXPMCAB
00677 ************************************************************      ELXPMCAB
00678  3400-IDNTFY-PROF-IP-SB-ABS-MAX.                                  ELXPMCAB
00679                                                                   ELXPMCAB
00680      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCAB
00681         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCAB
00682      ELSE                                                         ELXPMCAB
00683         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCAB
00684         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCAB
00685                                                                   ELXPMCAB
00686 ************************************************************      ELXPMCAB
00687 *                                                          *      ELXPMCAB
00688 * IDENTIFY PROFESSIONAL OUTPATIENT MAXIMUMS                *      ELXPMCAB
00689 *                                                          *      ELXPMCAB
00690 ************************************************************      ELXPMCAB
00691  4000-IDNTFY-PROF-OP-MAXS.                                        ELXPMCAB
00692                                                                   ELXPMCAB
00693      PERFORM 4001-INITIALIZE.                                     ELXPMCAB
00694      PERFORM 4200-IDNTFY-PROF-OP-PDN-MAX.                         ELXPMCAB
00695      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00696         PERFORM 4300-IDNTFY-PROF-OP-PSYCH-MAX                     ELXPMCAB
00697         PERFORM 4400-IDNTFY-PROF-OP-SB-ABS-MAX.                   ELXPMCAB
00698                                                                   ELXPMCAB
00699 ************************************************************      ELXPMCAB
00700 *                                                          *      ELXPMCAB
00701 * IDENTIFY PROFESSIONAL OUTPATIENT MAXIMUMS/INITIALIZE     *      ELXPMCAB
00702 *                                                          *      ELXPMCAB
00703 ************************************************************      ELXPMCAB
00704  4001-INITIALIZE.                                                 ELXPMCAB
00705                                                                   ELXPMCAB
00706      SET WS-DLRB-POINTER TO NULL.                                 ELXPMCAB
00707      SET WS-NPSY-POINTER TO NULL.                                 ELXPMCAB
00708      SET WS-DPSY-POINTER TO NULL.                                 ELXPMCAB
00709      CALL 'ELUADDRS' USING WS-PROF-OP-PDN                         ELXPMCAB
00710                          WS-PRDN-POINTER.                         ELXPMCAB
00711      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCAB
00712                          WS-PSYS-POINTER.                         ELXPMCAB
00713      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCAB
00714                          WS-ALAB-POINTER.                         ELXPMCAB
00715      CALL 'ELUADDRS' USING WS-PROF-OP-PSYS                        ELXPMCAB
00716                          WS-DRAB-POINTER.                         ELXPMCAB
00717      SET DRB-NOT-APPLICABLE TO TRUE.                              ELXPMCAB
00718      SET DNPD-NOT-APPLICABLE TO TRUE.                             ELXPMCAB
00719      SET DNPN-NOT-APPLICABLE TO TRUE.                             ELXPMCAB
00720                                                                   ELXPMCAB
00721 ************************************************************      ELXPMCAB
00722 *                                                          *      ELXPMCAB
00723 * IDENTIFY PROFESSIONAL OUTPATIENT DAILY ROOM/BOARD MAXIMUM*      ELXPMCAB
00724 *                                                          *      ELXPMCAB
00725 ************************************************************      ELXPMCAB
00726  4200-IDNTFY-PROF-OP-PDN-MAX.                                     ELXPMCAB
00727                                                                   ELXPMCAB
00728      IF PMCI-PDN-COVERED                                          ELXPMCAB
00729         PERFORM 5200-SRCH-PDN-DLR-MAX                             ELXPMCAB
00730      ELSE                                                         ELXPMCAB
00731         SET PRDN-NO-COVERAGE TO TRUE.                             ELXPMCAB
00732                                                                   ELXPMCAB
00733 ************************************************************      ELXPMCAB
00734 *                                                          *      ELXPMCAB
00735 * IDENTIFY PROFESSIONAL OUTPATIENT PSYCH SERVICES MAXIMUMS*       ELXPMCAB
00736 *                                                          *      ELXPMCAB
00737 ************************************************************      ELXPMCAB
00738  4300-IDNTFY-PROF-OP-PSYCH-MAX.                                   ELXPMCAB
00739                                                                   ELXPMCAB
00740      IF PSY-YES                                                   ELXPMCAB
00741         PERFORM 6000-SRCH-MNTL-MAX                                ELXPMCAB
00742      ELSE                                                         ELXPMCAB
00743         SET MNDD-NO-COVERAGE TO TRUE                              ELXPMCAB
00744         SET MNDT-NO-COVERAGE TO TRUE                              ELXPMCAB
00745         SET LFMD-NO-COVERAGE TO TRUE                              ELXPMCAB
00746         SET PMCI-MM-NO-COVERAGE TO TRUE.                          ELXPMCAB
00747                                                                   ELXPMCAB
00748 ************************************************************      ELXPMCAB
00749 *                                                          *      ELXPMCAB
00750 * IDENTIFY PROFESSIONAL OUTPATIENT SUBSTANCE ABUSE MAXIMUMS*      ELXPMCAB
00751 *                                                          *      ELXPMCAB
00752 ************************************************************      ELXPMCAB
00753  4400-IDNTFY-PROF-OP-SB-ABS-MAX.                                  ELXPMCAB
00754                                                                   ELXPMCAB
00755      IF SUB-ABUSE-ALC-YES OR SUB-ABUSE-DRG-YES                    ELXPMCAB
00756         PERFORM 8000-SRCH-SB-ABS-MAX                              ELXPMCAB
00757      ELSE                                                         ELXPMCAB
00758         SET PMCI-AL-NO-COVERAGE TO TRUE                           ELXPMCAB
00759         SET PMCI-MD-NO-COVERAGE TO TRUE.                          ELXPMCAB
00760                                                                   ELXPMCAB
00761 ************************************************************      ELXPMCAB
00762 *                                                          *      ELXPMCAB
00763 * SEARCH FOR DAILY ROOM AND BOARD MAXIMUMS                 *      ELXPMCAB
00764 *                                                          *      ELXPMCAB
00765 ************************************************************      ELXPMCAB
00766  5100-SRCH-DRB-MAX.                                               ELXPMCAB
00767                                                                   ELXPMCAB
00768      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCAB
00769         SET DRB-NOT-APPLICABLE TO TRUE                            ELXPMCAB
00770         SET PMCI-IQ-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00771         CONTINUE                                                  ELXPMCAB
00772      ELSE                                                         ELXPMCAB
00773         PERFORM 5101-RCMPT-BNFT-PRVSN-DRB                         ELXPMCAB
00774         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
00775            PERFORM 5110-RCMPT-CF-DRB                              ELXPMCAB
00776               VARYING ATBL-IDX FROM 1 BY 1                        ELXPMCAB
00777                  UNTIL ATBL-IDX > ATBL-MAX-IDX OR                 ELXPMCAB
00778                    PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO            ELXPMCAB
00779            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
00780               MOVE CVG2-OV-THRSHLD-DRB TO WS-TEST-THRESHOLD       ELXPMCAB
00781               PERFORM 5150-SRCH-DRB-DLR-MAX                       ELXPMCAB
00782               IF PMCI-BC-SUCCESSFUL                               ELXPMCAB
00783                  PERFORM 5160-SRCH-DRB-DAY-MAX                    ELXPMCAB
00784               END-IF                                              ELXPMCAB
00785            END-IF                                                 ELXPMCAB
00786         END-IF                                                    ELXPMCAB
00787      END-IF.                                                      ELXPMCAB
00788                                                                   ELXPMCAB
00789 ************************************************************      ELXPMCAB
00790 *                                                          *      ELXPMCAB
00791 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR DAILY ROOM/BOARD  *      ELXPMCAB
00792 *                                                          *      ELXPMCAB
00793 ************************************************************      ELXPMCAB
00794  5101-RCMPT-BNFT-PRVSN-DRB.                                       ELXPMCAB
00795                                                                   ELXPMCAB
00796      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DLRB-POINTER.       ELXPMCAB
00797      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
00798      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
00799                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
00800      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAB
00801      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
00802         MOVE +4607 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
00803         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
00804                                                                   ELXPMCAB
00805 ************************************************************      ELXPMCAB
00806 *                                                          *      ELXPMCAB
00807 * RECOMPUTE CONF FACTORS FOR DAILY ROOM AND BOARD          *      ELXPMCAB
00808 *                                                          *      ELXPMCAB
00809 ************************************************************      ELXPMCAB
00810  5110-RCMPT-CF-DRB.                                               ELXPMCAB
00811                                                                   ELXPMCAB
00812      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCAB
00813      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00814         MOVE CFTA-CF-DRB (CFTA-IDX) TO                            ELXPMCAB
00815                         ATBL-CF-BNFT-PRD (ATBL-IDX)               ELXPMCAB
00816         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCAB
00817         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCAB
00818         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
00819            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCAB
00820                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCAB
00821            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCAB
00822            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
00823               MOVE CFT5-CF-INTD-RM-BRD (CFT5-IDX) TO              ELXPMCAB
00824                      ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX).        ELXPMCAB
00825                                                                   ELXPMCAB
00826 ************************************************************      ELXPMCAB
00827 *                                                          *      ELXPMCAB
00828 * SEARCH FOR DAILY ROOM AND BOARD DOLLAR MAXIMUM           *      ELXPMCAB
00829 *                                                          *      ELXPMCAB
00830 ************************************************************      ELXPMCAB
00831  5150-SRCH-DRB-DLR-MAX.                                           ELXPMCAB
00832                                                                   ELXPMCAB
00833      PERFORM 9151-RCMPT-VL-QLFR-CF-DLR                            ELXPMCAB
00834          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAB
00835              UNTIL ATBL-IDX > ATBL-MAX-IDX.                       ELXPMCAB
00836      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
00837      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00838         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
00839         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
00840            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
00841            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
00842                  ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
00843                    ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND   ELXPMCAB
00844                      ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1' OR   ELXPMCAB
00845                        WS-NUM-APPL-ENTRS > 1                      ELXPMCAB
00846                SET DRB-CALL TO TRUE                               ELXPMCAB
00847            ELSE                                                   ELXPMCAB
00848                MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                ELXPMCAB
00849                                 PMCI-DAILY-ROOM-BOARD             ELXPMCAB
00850                PERFORM 9110-FND-BNFT-PRD-CF                       ELXPMCAB
00851                MOVE WS-BNF-QUAL TO PMCI-DAILY-ROOM-BOARD-QUAL     ELXPMCAB
00852                MOVE WS-SAVE-LOB-IND TO PMCI-DRB-FROM-IND          ELXPMCAB
00853            END-IF                                                 ELXPMCAB
00854         ELSE                                                      ELXPMCAB
00855            SET DRB-NOT-APPLICABLE TO TRUE                         ELXPMCAB
00856         END-IF                                                    ELXPMCAB
00857      END-IF.                                                      ELXPMCAB
00858                                                                   ELXPMCAB
00859 ************************************************************      ELXPMCAB
00860 *                                                          *      ELXPMCAB
00861 * SEARCH FOR DAILY ROOM AND BOARD DAY MAXIMUM              *      ELXPMCAB
00862 *                                                          *      ELXPMCAB
00863 ************************************************************      ELXPMCAB
00864  5160-SRCH-DRB-DAY-MAX.                                           ELXPMCAB
00865                                                                   ELXPMCAB
00866      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCAB
00867          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAB
00868              UNTIL ATBL-IDX > ATBL-MAX-IDX.                       ELXPMCAB
00869      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
00870      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00871         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
00872         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
00873            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
00874            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
00875                 ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCAB
00876                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
00877                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1' OR    ELXPMCAB
00878                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
00879               SET PMCI-IQ-CALL TO TRUE                            ELXPMCAB
00880            ELSE                                                   ELXPMCAB
00881               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCAB
00882                                                          OR '6'   ELXPMCAB
00883                  SET PMCI-IQ-CALL TO TRUE                         ELXPMCAB
00884               ELSE                                                ELXPMCAB
00885                  MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO        ELXPMCAB
00886                                 PMCI-MAX-INPATIENT                ELXPMCAB
00887                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
00888                  MOVE WS-BNF-QUAL TO PMCI-MAX-INPATIENT-QUALIFIER ELXPMCAB
00889                END-IF                                             ELXPMCAB
00890            END-IF                                                 ELXPMCAB
00891         ELSE                                                      ELXPMCAB
00892            SET PMCI-IQ-NOT-APPLICABLE TO TRUE                     ELXPMCAB
00893         END-IF                                                    ELXPMCAB
00894      END-IF.                                                      ELXPMCAB
00895                                                                   ELXPMCAB
00896 ************************************************************      ELXPMCAB
00897 *                                                          *      ELXPMCAB
00898 * SEARCH FOR PRIVATE DUTY NURSING MAXIMUMS                 *      ELXPMCAB
00899 *                                                          *      ELXPMCAB
00900 ************************************************************      ELXPMCAB
00901  5200-SRCH-PDN-DLR-MAX.                                           ELXPMCAB
00902                                                                   ELXPMCAB
00903      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCAB
00904         SET PRDN-NOT-APPLICABLE TO TRUE                           ELXPMCAB
00905      ELSE                                                         ELXPMCAB
00906         PERFORM 5201-RCMPT-BNFT-PRVSN-PDN                         ELXPMCAB
00907         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
00908            PERFORM 5220-SCN-PDN-DLR-MAX.                          ELXPMCAB
00909                                                                   ELXPMCAB
00910 ************************************************************      ELXPMCAB
00911 *                                                          *      ELXPMCAB
00912 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR PRIVATE DUTY NURSE*      ELXPMCAB
00913 *                                                          *      ELXPMCAB
00914 ************************************************************      ELXPMCAB
00915  5201-RCMPT-BNFT-PRVSN-PDN.                                       ELXPMCAB
00916                                                                   ELXPMCAB
00917      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-PRDN-POINTER.       ELXPMCAB
00918      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
00919      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
00920                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
00921      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCAB
00922      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
00923         MOVE +4608 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
00924         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
00925                                                                   ELXPMCAB
00926 ************************************************************      ELXPMCAB
00927 *                                                          *      ELXPMCAB
00928 * SEARCH FOR PRIVATE DUTY NURSING DOLLAR MAXIMUM           *      ELXPMCAB
00929 *                                                          *      ELXPMCAB
00930 ************************************************************      ELXPMCAB
00931  5220-SCN-PDN-DLR-MAX.                                            ELXPMCAB
00932                                                                   ELXPMCAB
00933      PERFORM 5230-RCMPT-SP-PDN-DLR                                ELXPMCAB
00934          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAB
00935              UNTIL ATBL-IDX > ATBL-MAX-IDX OR                     ELXPMCAB
00936                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.              ELXPMCAB
00937      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00938         PERFORM 5290-TST-PDN-DLR-MAX.                             ELXPMCAB
00939                                                                   ELXPMCAB
00940 ************************************************************      ELXPMCAB
00941 *                                                          *      ELXPMCAB
00942 * RECOMPUTE CONF FACTORS FOR PRIVATE DUTY NURSING          *      ELXPMCAB
00943 *                                                          *      ELXPMCAB
00944 ************************************************************      ELXPMCAB
00945  5230-RCMPT-SP-PDN-DLR.                                           ELXPMCAB
00946                                                                   ELXPMCAB
00947      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCAB
00948      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00949         MOVE CFTA-CF-PDN (CFTA-IDX) TO                            ELXPMCAB
00950                     ATBL-CF-BNFT-PRD (ATBL-IDX)                   ELXPMCAB
00951         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCAB
00952         PERFORM 9120-RCMPT-BNFT-PRVSN-CF                          ELXPMCAB
00953         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
00954            MOVE ATBL-CF-OV-CNDTN-BTS (ATBL-IDX) TO                ELXPMCAB
00955                       ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)             ELXPMCAB
00956            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCAB
00957            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
00958               MOVE CFT5-CF-INTD-NRSNG (CFT5-IDX) TO               ELXPMCAB
00959                        ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)       ELXPMCAB
00960               PERFORM 9151-RCMPT-VL-QLFR-CF-DLR.                  ELXPMCAB
00961                                                                   ELXPMCAB
00962 ************************************************************      ELXPMCAB
00963 *                                                          *      ELXPMCAB
00964 * SEARCH FOR PRIVATE DUTY NURSING DAY MAXIMUM              *      ELXPMCAB
00965 *                                                          *      ELXPMCAB
00966 ************************************************************      ELXPMCAB
00967  5290-TST-PDN-DLR-MAX.                                            ELXPMCAB
00968                                                                   ELXPMCAB
00969      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY                       ELXPMCAB
00970      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
00971         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
00972         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
00973            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
00974            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
00975                (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCAB
00976                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
00977                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
00978                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
00979               SET PRDN-CALL TO TRUE                               ELXPMCAB
00980            ELSE                                                   ELXPMCAB
00981               MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                 ELXPMCAB
00982                                 PMCI-PRIVATE-DUTY-NURSE-MAX       ELXPMCAB
00983                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
00984               MOVE WS-BNF-QUAL TO PMCI-PRIVATE-DUTY-NURSE-QUAL    ELXPMCAB
00985               MOVE WS-SAVE-LOB-IND TO PMCI-PRVTE-DTY-NRS-FROM-IND ELXPMCAB
00986            END-IF                                                 ELXPMCAB
00987         ELSE                                                      ELXPMCAB
00988            SET PRDN-NOT-APPLICABLE TO TRUE                        ELXPMCAB
00989         END-IF                                                    ELXPMCAB
00990      END-IF.                                                      ELXPMCAB
00991                                                                   ELXPMCAB
00992 ************************************************************      ELXPMCAB
00993 *                                                          *      ELXPMCAB
00994 *     SEARCH FOR MENTAL MAXIMUMS                           *      ELXPMCAB
00995 *                                                          *      ELXPMCAB
00996 ************************************************************      ELXPMCAB
00997  6000-SRCH-MNTL-MAX.                                              ELXPMCAB
00998                                                                   ELXPMCAB
00999      PERFORM 6001-RCMPT-BNFT-PRVSN-PSYCH                          ELXPMCAB
01000      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01001         PERFORM 6010-RCMPT-CF-MNTL-CNDTNS                         ELXPMCAB
01002            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAB
01003               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCAB
01004                 PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO               ELXPMCAB
01005         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01006            MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD         ELXPMCAB
01007            PERFORM 6020-SRCH-PSYCH-LFTM-MAX                       ELXPMCAB
01008            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01009               PERFORM 6030-SRCH-PSYC-OTHR-MAX                     ELXPMCAB
01010            END-IF                                                 ELXPMCAB
01011         END-IF                                                    ELXPMCAB
01012      END-IF.                                                      ELXPMCAB
01013                                                                   ELXPMCAB
01014                                                                   ELXPMCAB
01015 ************************************************************      ELXPMCAB
01016 *                                                          *      ELXPMCAB
01017 * RECOMPUTE BENEFIT PROV CONF FACTOR FOR PSYCHIATRIC SERV  *      ELXPMCAB
01018 *                                                          *      ELXPMCAB
01019 ************************************************************      ELXPMCAB
01020  6001-RCMPT-BNFT-PRVSN-PSYCH.                                     ELXPMCAB
01021                                                                   ELXPMCAB
01022      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-PSYS-POINTER.       ELXPMCAB
01023      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
01024      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
01025                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
01026      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAB
01027      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
01028         MOVE +4611 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
01029         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
01030                                                                   ELXPMCAB
01031 ************************************************************      ELXPMCAB
01032 *                                                          *      ELXPMCAB
01033 * RECOMPUTE CONF FACTORS FOR MENTAL CONDITIONS             *      ELXPMCAB
01034 *                                                          *      ELXPMCAB
01035 ************************************************************      ELXPMCAB
01036  6010-RCMPT-CF-MNTL-CNDTNS.                                       ELXPMCAB
01037                                                                   ELXPMCAB
01038      MOVE WS-CF-ZERO TO WS-NO-IBGR-CF.                            ELXPMCAB
01039      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCAB
01040      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01041         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCAB
01042         PERFORM 9020-RCMPT-CNDTN-BT-MNTL                          ELXPMCAB
01043         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01044            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCAB
01045            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01046               MOVE CFT5-CF-INTD-PSYCH (CFT5-IDX) TO               ELXPMCAB
01047                     ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX).         ELXPMCAB
01048                                                                   ELXPMCAB
01049 ************************************************************      ELXPMCAB
01050 *                                                          *      ELXPMCAB
01051 *     SEARCH FOR PSYCHIATRIC SERVICE LIFETIME MAXIMUMS     *      ELXPMCAB
01052 *                                                          *      ELXPMCAB
01053 ************************************************************      ELXPMCAB
01054  6020-SRCH-PSYCH-LFTM-MAX.                                        ELXPMCAB
01055                                                                   ELXPMCAB
01056      PERFORM 6911-RCMPT-BNFT-PRD-LFTM-MNTL                        ELXPMCAB
01057         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01058            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCAB
01059      PERFORM 6100-SRCH-LFTM-MNTL-DLR-MAX.                         ELXPMCAB
01060      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01061         PERFORM 6200-SRCH-LFTM-MNTL-DAY-MAX.                      ELXPMCAB
01062                                                                   ELXPMCAB
01063 ************************************************************      ELXPMCAB
01064 *                                                          *      ELXPMCAB
01065 *     SEARCH FOR PSYCHIATRIC SERVICES OTHER MAXIMUMS       *      ELXPMCAB
01066 *                                                          *      ELXPMCAB
01067 ************************************************************      ELXPMCAB
01068  6030-SRCH-PSYC-OTHR-MAX.                                         ELXPMCAB
01069                                                                   ELXPMCAB
01070      PERFORM 6912-RCMPT-BNFT-PRD-OTHR-MNTL                        ELXPMCAB
01071         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01072            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCAB
01073               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCAB
01074      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01075         PERFORM 6300-SRCH-OTHR-MNTL-DLR-MAX                       ELXPMCAB
01076         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01077            PERFORM 6400-SRCH-OTHR-MNTL-DAY-MAX.                   ELXPMCAB
01078                                                                   ELXPMCAB
01079 ************************************************************      ELXPMCAB
01080 *                                                          *      ELXPMCAB
01081 * SEARCH FOR LIFETIME MENTAL DOLLAR MAXIMUMS               *      ELXPMCAB
01082 *                                                          *      ELXPMCAB
01083 ************************************************************      ELXPMCAB
01084  6100-SRCH-LFTM-MNTL-DLR-MAX.                                     ELXPMCAB
01085                                                                   ELXPMCAB
01086      PERFORM 9151-RCMPT-VL-QLFR-CF-DLR                            ELXPMCAB
01087         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01088            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCAB
01089      PERFORM 6190-TST-LFTM-MNTL-DLR-MAX.                          ELXPMCAB
01090                                                                   ELXPMCAB
01091 ************************************************************      ELXPMCAB
01092 *                                                          *      ELXPMCAB
01093 * TEST AND SET LIFETIME MENTAL DOLLAR MAXIMUM              *      ELXPMCAB
01094 *                                                          *      ELXPMCAB
01095 ************************************************************      ELXPMCAB
01096  6190-TST-LFTM-MNTL-DLR-MAX.                                      ELXPMCAB
01097                                                                   ELXPMCAB
01098      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01099      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01100         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01101         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01102            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01103            MOVE WS-SAVE-SUB TO WS-LFM-DLR-SUB                     ELXPMCAB
01104            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01105                (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND            ELXPMCAB
01106                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01107                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01108                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01109               SET MNDD-CALL TO TRUE                               ELXPMCAB
01110            ELSE                                                   ELXPMCAB
01111               MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                 ELXPMCAB
01112                                 PMCI-LIFETIME-MENTAL-MAX          ELXPMCAB
01113                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01114               MOVE WS-BNF-QUAL TO PMCI-LIFETIME-MENTAL-MAX-QUAL   ELXPMCAB
01115               MOVE WS-SAVE-LOB-IND TO PMCI-LFTM-MNTL-FROM-IND     ELXPMCAB
01116            END-IF                                                 ELXPMCAB
01117         ELSE                                                      ELXPMCAB
01118            SET MNDD-NOT-APPLICABLE TO TRUE                        ELXPMCAB
01119         END-IF                                                    ELXPMCAB
01120      END-IF.                                                      ELXPMCAB
01121                                                                   ELXPMCAB
01122 ************************************************************      ELXPMCAB
01123 *                                                          *      ELXPMCAB
01124 * SEARCH FOR LIFETIME MENTAL DAY MAXIMUMS                  *      ELXPMCAB
01125 *                                                          *      ELXPMCAB
01126 ************************************************************      ELXPMCAB
01127  6200-SRCH-LFTM-MNTL-DAY-MAX.                                     ELXPMCAB
01128                                                                   ELXPMCAB
01129      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCAB
01130         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01131            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCAB
01132               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCAB
01133      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01134         PERFORM 6290-TST-LFTM-MNTL-DAY-MAX.                       ELXPMCAB
01135                                                                   ELXPMCAB
01136 ************************************************************      ELXPMCAB
01137 *                                                          *      ELXPMCAB
01138 * TEST AND SET LIFETIME MENTAL DAY MAXIMUM                 *      ELXPMCAB
01139 *                                                          *      ELXPMCAB
01140 ************************************************************      ELXPMCAB
01141  6290-TST-LFTM-MNTL-DAY-MAX.                                      ELXPMCAB
01142                                                                   ELXPMCAB
01143      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01144      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01145         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01146         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01147            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01148            MOVE WS-SAVE-SUB TO WS-LFM-DAY-SUB                     ELXPMCAB
01149            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01150                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01151                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01152                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01153                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01154               SET LFMD-CALL TO TRUE                               ELXPMCAB
01155            ELSE                                                   ELXPMCAB
01156               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCAB
01157                                                          OR '6'   ELXPMCAB
01158                  SET LFMD-CALL TO TRUE                            ELXPMCAB
01159               ELSE                                                ELXPMCAB
01160                  MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO        ELXPMCAB
01161                                 PMCI-DAYS-LIFETIME-MENTAL         ELXPMCAB
01162                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01163                  MOVE WS-BNF-QUAL TO                              ELXPMCAB
01164                                  PMCI-DAYS-LIFETIME-METAL-QUAL    ELXPMCAB
01165                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01166                                  PMCI-LFTM-MNTL-DAYS-FROM-IND     ELXPMCAB
01167               END-IF                                              ELXPMCAB
01168            END-IF                                                 ELXPMCAB
01169         ELSE                                                      ELXPMCAB
01170            SET LFMD-NO-COVERAGE TO TRUE                           ELXPMCAB
01171         END-IF                                                    ELXPMCAB
01172      END-IF.                                                      ELXPMCAB
01173                                                                   ELXPMCAB
01174 ************************************************************      ELXPMCAB
01175 *                                                          *      ELXPMCAB
01176 * SEARCH FOR OTHER MENTAL DOLLAR MAXIMUMS                  *      ELXPMCAB
01177 *                                                          *      ELXPMCAB
01178 ************************************************************      ELXPMCAB
01179  6300-SRCH-OTHR-MNTL-DLR-MAX.                                     ELXPMCAB
01180                                                                   ELXPMCAB
01181      PERFORM 9151-RCMPT-VL-QLFR-CF-DLR                            ELXPMCAB
01182         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01183            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCAB
01184      PERFORM 6390-TST-OTHR-MNTL-DLR-MAX.                          ELXPMCAB
01185                                                                   ELXPMCAB
01186 ************************************************************      ELXPMCAB
01187 *                                                          *      ELXPMCAB
01188 * TEST AND SET OTHER MENTAL DOLLAR MAXIMUM                 *      ELXPMCAB
01189 *                                                          *      ELXPMCAB
01190 ************************************************************      ELXPMCAB
01191  6390-TST-OTHR-MNTL-DLR-MAX.                                      ELXPMCAB
01192                                                                   ELXPMCAB
01193      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01194      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01195         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01196         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01197            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01198            MOVE WS-SAVE-SUB TO WS-OTR-DLR-SUB                     ELXPMCAB
01199            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01200                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01201                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01202                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01203                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01204               SET MNDT-CALL TO TRUE                               ELXPMCAB
01205            ELSE                                                   ELXPMCAB
01206               MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                 ELXPMCAB
01207                                 PMCI-MENTAL-DOLLARS               ELXPMCAB
01208                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01209               MOVE WS-BNF-QUAL TO PMCI-MENTAL-DOLLARS-QUAL        ELXPMCAB
01210               MOVE WS-SAVE-LOB-IND TO                             ELXPMCAB
01211                               PMCI-LFTM-MNTL-FROM-IND             ELXPMCAB
01212            END-IF                                                 ELXPMCAB
01213         ELSE                                                      ELXPMCAB
01214            SET MNDT-NOT-APPLICABLE TO TRUE                        ELXPMCAB
01215         END-IF                                                    ELXPMCAB
01216      END-IF.                                                      ELXPMCAB
01217                                                                   ELXPMCAB
01218 ************************************************************      ELXPMCAB
01219 *                                                          *      ELXPMCAB
01220 * SEARCH FOR OTHER MENTAL DAY MAXIMUMS                     *      ELXPMCAB
01221 *                                                          *      ELXPMCAB
01222 ************************************************************      ELXPMCAB
01223  6400-SRCH-OTHR-MNTL-DAY-MAX.                                     ELXPMCAB
01224                                                                   ELXPMCAB
01225      PERFORM 9152-RCMPT-VL-QLFR-CF-DAY                            ELXPMCAB
01226         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01227            UNTIL ATBL-IDX > ATBL-MAX-IDX.                         ELXPMCAB
01228      PERFORM 6490-TST-OTHR-MNTL-DAY-MAX.                          ELXPMCAB
01229                                                                   ELXPMCAB
01230 ************************************************************      ELXPMCAB
01231 *                                                          *      ELXPMCAB
01232 * TEST AND SET OTHER MENTAL DAY MAXIMUM                    *      ELXPMCAB
01233 *                                                          *      ELXPMCAB
01234 ************************************************************      ELXPMCAB
01235  6490-TST-OTHR-MNTL-DAY-MAX.                                      ELXPMCAB
01236                                                                   ELXPMCAB
01237      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01238      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01239         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01240         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01241            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01242            MOVE WS-SAVE-SUB TO WS-OTR-DAY-SUB                     ELXPMCAB
01243            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01244                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01245                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01246                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01247                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01248               SET PMCI-MM-CALL TO TRUE                            ELXPMCAB
01249            ELSE                                                   ELXPMCAB
01250               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCAB
01251                                                          OR '6'   ELXPMCAB
01252                  SET PMCI-MM-CALL TO TRUE                         ELXPMCAB
01253               ELSE                                                ELXPMCAB
01254                  MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO        ELXPMCAB
01255                                 PMCI-MAX-MENTAL                   ELXPMCAB
01256                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01257                  MOVE WS-BNF-QUAL TO PMCI-MAX-MENTAL-QUALIFIER    ELXPMCAB
01258                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01259                               PMCI-MAX-MNTL-FROM-IND              ELXPMCAB
01260               END-IF                                              ELXPMCAB
01261            END-IF                                                 ELXPMCAB
01262         ELSE                                                      ELXPMCAB
01263            SET PMCI-MM-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01264         END-IF                                                    ELXPMCAB
01265      END-IF.                                                      ELXPMCAB
01266                                                                   ELXPMCAB
01267 ************************************************************      ELXPMCAB
01268 *                                                          *      ELXPMCAB
01269 *   RECOMPUTE BENEFIT PERIOD FACTOR FOR LIFETIME MENTAL    *      ELXPMCAB
01270 *                                                          *      ELXPMCAB
01271 ************************************************************      ELXPMCAB
01272  6911-RCMPT-BNFT-PRD-LFTM-MNTL.                                   ELXPMCAB
01273                                                                   ELXPMCAB
01274      MOVE ATBL-CF-LFTM (ATBL-IDX) TO                              ELXPMCAB
01275                          ATBL-CF-BNFT-PRD (ATBL-IDX).             ELXPMCAB
01276                                                                   ELXPMCAB
01277 ************************************************************      ELXPMCAB
01278 *                                                          *      ELXPMCAB
01279 *   RECOMPUTE BENEFIT PERIOD FACTOR FOR OTHER MENTAL       *      ELXPMCAB
01280 *                                                          *      ELXPMCAB
01281 ************************************************************      ELXPMCAB
01282  6912-RCMPT-BNFT-PRD-OTHR-MNTL.                                   ELXPMCAB
01283                                                                   ELXPMCAB
01284      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCAB
01285      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01286         MOVE CFTA-CF-OTHR-MNTL (CFTA-IDX) TO                      ELXPMCAB
01287                         ATBL-CF-BNFT-PRD (ATBL-IDX).              ELXPMCAB
01288                                                                   ELXPMCAB
01289 ************************************************************      ELXPMCAB
01290 *                                                          *      ELXPMCAB
01291 * SEARCH FOR DAY NIGHT PSYCH DAY MAXIMUMS                  *      ELXPMCAB
01292 *                                                          *      ELXPMCAB
01293 ************************************************************      ELXPMCAB
01294  7000-SRCH-DAY-NGHT-PSYCH-MAX.                                    ELXPMCAB
01295                                                                   ELXPMCAB
01296      PERFORM 7010-RCMPT-CF-DAY-NGHT-PSYCH                         ELXPMCAB
01297         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01298            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCAB
01299               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCAB
01300      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01301         IF PMCI-INSTITUTIONAL                                     ELXPMCAB
01302            IF NAES-II-DPSY-YES                                    ELXPMCAB
01303               PERFORM 7100-SRCH-DAY-PSYCH-DAY-MAX                 ELXPMCAB
01304            ELSE                                                   ELXPMCAB
01305               SET DNPD-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01306            END-IF                                                 ELXPMCAB
01307            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01308               IF NAES-II-NPSY-YES                                 ELXPMCAB
01309               PERFORM 7200-SRCH-NGHT-PSYCH-DAY-MAX                ELXPMCAB
01310            ELSE                                                   ELXPMCAB
01311               SET DNPN-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01312               END-IF                                              ELXPMCAB
01313            END-IF                                                 ELXPMCAB
01314         ELSE                                                      ELXPMCAB
01315            IF NAES-PI-DPV-YES                                     ELXPMCAB
01316               PERFORM 7100-SRCH-DAY-PSYCH-DAY-MAX                 ELXPMCAB
01317            ELSE                                                   ELXPMCAB
01318               SET DNPD-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01319            END-IF                                                 ELXPMCAB
01320            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01321               IF NAES-PI-NPV-YES                                  ELXPMCAB
01322                  PERFORM 7200-SRCH-NGHT-PSYCH-DAY-MAX             ELXPMCAB
01323            ELSE                                                   ELXPMCAB
01324                  SET DNPN-NOT-APPLICABLE TO TRUE                  ELXPMCAB
01325               END-IF                                              ELXPMCAB
01326            END-IF                                                 ELXPMCAB
01327         END-IF.                                                   ELXPMCAB
01328                                                                   ELXPMCAB
01329 ************************************************************      ELXPMCAB
01330 *                                                          *      ELXPMCAB
01331 * RECOMPUTE CONFIDENCE FACTORS FOR DAY/NIGHT PSYCH         *      ELXPMCAB
01332 *                                                          *      ELXPMCAB
01333 ************************************************************      ELXPMCAB
01334  7010-RCMPT-CF-DAY-NGHT-PSYCH.                                    ELXPMCAB
01335                                                                   ELXPMCAB
01336      PERFORM 9010-RCMPT-BNFT-PRD-PSYCH.                           ELXPMCAB
01337      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01338         PERFORM 9020-RCMPT-CNDTN-BT-MNTL                          ELXPMCAB
01339         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01340            PERFORM 9130-FND-INTRNL-DSCRPTR-CF                     ELXPMCAB
01341            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01342               MOVE CFT5-CF-INTD-PSYCH (CFT5-IDX) TO               ELXPMCAB
01343                     ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)          ELXPMCAB
01344               IF PMCI-BC-SUCCESSFUL                               ELXPMCAB
01345                  PERFORM 9152-RCMPT-VL-QLFR-CF-DAY.               ELXPMCAB
01346                                                                   ELXPMCAB
01347 ************************************************************      ELXPMCAB
01348 *                                                          *      ELXPMCAB
01349 * SEARCH FOR DAY PSYCH DAY MAXIMUM                         *      ELXPMCAB
01350 *                                                          *      ELXPMCAB
01351 ************************************************************      ELXPMCAB
01352  7100-SRCH-DAY-PSYCH-DAY-MAX.                                     ELXPMCAB
01353                                                                   ELXPMCAB
01354      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCAB
01355         SET DNPD-NOT-APPLICABLE TO TRUE                           ELXPMCAB
01356      ELSE                                                         ELXPMCAB
01357         PERFORM 7101-RCMPT-BNFT-PRVSN-DAY-PSY                     ELXPMCAB
01358         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01359            PERFORM 9120-RCMPT-BNFT-PRVSN-CF                       ELXPMCAB
01360              VARYING ATBL-IDX FROM 1 BY 1                         ELXPMCAB
01361                UNTIL ATBL-IDX > ATBL-MAX-IDX                      ELXPMCAB
01362            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01363               MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD      ELXPMCAB
01364               PERFORM 7190-TST-DAY-PSYCH-DAY-MAX                  ELXPMCAB
01365            END-IF                                                 ELXPMCAB
01366         END-IF                                                    ELXPMCAB
01367      END-IF.                                                      ELXPMCAB
01368                                                                   ELXPMCAB
01369 ************************************************************      ELXPMCAB
01370 *                                                          *      ELXPMCAB
01371 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACTORS-DAY PSYCH *      ELXPMCAB
01372 *                                                          *      ELXPMCAB
01373 ************************************************************      ELXPMCAB
01374  7101-RCMPT-BNFT-PRVSN-DAY-PSY.                                   ELXPMCAB
01375                                                                   ELXPMCAB
01376      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DPSY-POINTER.       ELXPMCAB
01377      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
01378      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
01379                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
01380      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAB
01381      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
01382         MOVE +4609 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
01383         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
01384                                                                   ELXPMCAB
01385 ************************************************************      ELXPMCAB
01386 *                                                          *      ELXPMCAB
01387 * TEST AND SET DAY PSYCH DAY MAXIMUM                       *      ELXPMCAB
01388 *                                                          *      ELXPMCAB
01389 ************************************************************      ELXPMCAB
01390  7190-TST-DAY-PSYCH-DAY-MAX.                                      ELXPMCAB
01391                                                                   ELXPMCAB
01392      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01393      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01394         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01395         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01396            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01397            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01398                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01399                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01400                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01401                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01402               SET DNPD-CALL TO TRUE                               ELXPMCAB
01403            ELSE                                                   ELXPMCAB
01404               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCAB
01405                                                          OR '6'   ELXPMCAB
01406                  SET DNPD-CALL TO TRUE                            ELXPMCAB
01407               ELSE                                                ELXPMCAB
01408                  MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO        ELXPMCAB
01409                                 PMCI-DN-DAY-PSYCH                 ELXPMCAB
01410                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01411                  MOVE WS-BNF-QUAL TO PMCI-DN-DAY-PSYCH-QUALIFIER  ELXPMCAB
01412                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01413                               PMCI-DAY-PSYCH-FROM-IND             ELXPMCAB
01414               END-IF                                              ELXPMCAB
01415            END-IF                                                 ELXPMCAB
01416         ELSE                                                      ELXPMCAB
01417            SET DNPD-NOT-APPLICABLE TO TRUE                        ELXPMCAB
01418         END-IF                                                    ELXPMCAB
01419      END-IF.                                                      ELXPMCAB
01420                                                                   ELXPMCAB
01421 ************************************************************      ELXPMCAB
01422 *                                                          *      ELXPMCAB
01423 * SEARCH FOR NIGHT PSYCH DAY MAXIMUM                       *      ELXPMCAB
01424 *                                                          *      ELXPMCAB
01425 ************************************************************      ELXPMCAB
01426  7200-SRCH-NGHT-PSYCH-DAY-MAX.                                    ELXPMCAB
01427                                                                   ELXPMCAB
01428      IF ADDRESS OF IBGR-INTERNAL-TABS-TABLE = NULL                ELXPMCAB
01429         SET DNPN-NOT-APPLICABLE TO TRUE                           ELXPMCAB
01430      ELSE                                                         ELXPMCAB
01431         PERFORM 7201-RCMPT-BNFT-PRVSN-NGHT-PSY                    ELXPMCAB
01432         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01433            PERFORM 9120-RCMPT-BNFT-PRVSN-CF                       ELXPMCAB
01434              VARYING ATBL-IDX FROM 1 BY 1                         ELXPMCAB
01435                UNTIL ATBL-IDX > ATBL-MAX-IDX                      ELXPMCAB
01436            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01437               MOVE CVG2-OV-THRSHLD-MNTL TO WS-TEST-THRESHOLD      ELXPMCAB
01438               PERFORM 7290-TST-NGHT-PSYCH-DAY-MAX                 ELXPMCAB
01439            END-IF                                                 ELXPMCAB
01440         END-IF                                                    ELXPMCAB
01441      END-IF.                                                      ELXPMCAB
01442                                                                   ELXPMCAB
01443 ************************************************************      ELXPMCAB
01444 *                                                          *      ELXPMCAB
01445 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACTORS-NGHT PSY  *      ELXPMCAB
01446 *                                                          *      ELXPMCAB
01447 ************************************************************      ELXPMCAB
01448  7201-RCMPT-BNFT-PRVSN-NGHT-PSY.                                  ELXPMCAB
01449                                                                   ELXPMCAB
01450      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-NPSY-POINTER.       ELXPMCAB
01451      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
01452      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
01453                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
01454      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAB
01455      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
01456         MOVE +4610 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
01457         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
01458                                                                   ELXPMCAB
01459 ************************************************************      ELXPMCAB
01460 *                                                          *      ELXPMCAB
01461 * TEST AND SET NIGHT PSYCH DAY MAXIMUM                     *      ELXPMCAB
01462 *                                                          *      ELXPMCAB
01463 ************************************************************      ELXPMCAB
01464  7290-TST-NGHT-PSYCH-DAY-MAX.                                     ELXPMCAB
01465                                                                   ELXPMCAB
01466      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01467      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01468         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01469         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01470            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01471            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01472                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01473                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01474                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01475                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01476               SET DNPN-CALL TO TRUE                               ELXPMCAB
01477            ELSE                                                   ELXPMCAB
01478               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4'     ELXPMCAB
01479                                                          OR '6'   ELXPMCAB
01480                  SET DNPN-CALL TO TRUE                            ELXPMCAB
01481               ELSE                                                ELXPMCAB
01482                  MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO        ELXPMCAB
01483                                 PMCI-DN-NIGHT-PSYCH               ELXPMCAB
01484                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01485                  MOVE WS-BNF-QUAL TO                              ELXPMCAB
01486                               PMCI-DN-NIGHT-PSYCH-QUALIFIER       ELXPMCAB
01487                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01488                               PMCI-NGHT-PSYCH-FROM-IND            ELXPMCAB
01489            END-IF                                                 ELXPMCAB
01490         ELSE                                                      ELXPMCAB
01491            SET DNPN-NOT-APPLICABLE TO TRUE                        ELXPMCAB
01492         END-IF                                                    ELXPMCAB
01493      END-IF.                                                      ELXPMCAB
01494                                                                   ELXPMCAB
01495 ************************************************************      ELXPMCAB
01496 *                                                          *      ELXPMCAB
01497 *  SEARCH FOR SUBSTANCE ABUSE MAXIMUMS                     *      ELXPMCAB
01498 *                                                          *      ELXPMCAB
01499 ************************************************************      ELXPMCAB
01500  8000-SRCH-SB-ABS-MAX.                                            ELXPMCAB
01501                                                                   ELXPMCAB
01502      PERFORM 8010-RCMPT-CF-SB-ABS                                 ELXPMCAB
01503         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAB
01504            UNTIL ATBL-IDX > ATBL-MAX-IDX OR                       ELXPMCAB
01505               PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.                ELXPMCAB
01506      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01507         IF SUB-ABUSE-ALC-YES                                      ELXPMCAB
01508            PERFORM 8100-SRCH-ALCHL-MAX                            ELXPMCAB
01509         ELSE                                                      ELXPMCAB
01510            SET PMCI-AL-NO-COVERAGE TO TRUE.                       ELXPMCAB
01511      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01512         IF SUB-ABUSE-DRG-YES                                      ELXPMCAB
01513            PERFORM 8200-SRCH-DRG-MAX                              ELXPMCAB
01514         ELSE                                                      ELXPMCAB
01515            SET PMCI-MD-NO-COVERAGE TO TRUE.                       ELXPMCAB
01516      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01517         PERFORM 8500-TST-CMBND-MDA-MAX.                           ELXPMCAB
01518                                                                   ELXPMCAB
01519 ************************************************************      ELXPMCAB
01520 *                                                          *      ELXPMCAB
01521 * RECOMPUTE CONFIDENCE FACTORS FOR SUBSTANCE ABUSE         *      ELXPMCAB
01522 *                                                          *      ELXPMCAB
01523 ************************************************************      ELXPMCAB
01524  8010-RCMPT-CF-SB-ABS.                                            ELXPMCAB
01525                                                                   ELXPMCAB
01526      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCAB
01527      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01528         MOVE CFTA-CF-SB-ABS (CFTA-IDX) TO                         ELXPMCAB
01529                         ATBL-CF-BNFT-PRD (ATBL-IDX)               ELXPMCAB
01530         PERFORM 9130-FND-INTRNL-DSCRPTR-CF                        ELXPMCAB
01531         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAB
01532            MOVE CFT5-CF-INTD-SB-ABS (CFT5-IDX) TO                 ELXPMCAB
01533                       ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)        ELXPMCAB
01534            IF PMCI-BC-SUCCESSFUL                                  ELXPMCAB
01535               PERFORM 8040-RCMPT-VL-QLFR-CF-SB-ABS.               ELXPMCAB
01536                                                                   ELXPMCAB
01537 ************************************************************      ELXPMCAB
01538 *                                                          *      ELXPMCAB
01539 * RECOMPUTE VALUE QUALIFIER FACTOR FOR SUBSTANCE ABUSE     *      ELXPMCAB
01540 *                                                          *      ELXPMCAB
01541 ************************************************************      ELXPMCAB
01542  8040-RCMPT-VL-QLFR-CF-SB-ABS.                                    ELXPMCAB
01543                                                                   ELXPMCAB
01544      IF PMCI-INSTITUTIONAL                                        ELXPMCAB
01545         EVALUATE TRUE                                             ELXPMCAB
01546            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'             ELXPMCAB
01547                MOVE WS-CF-95 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01548            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2'             ELXPMCAB
01549                MOVE WS-CF-85 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01550            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '5'      ELXPMCAB
01551                                                         OR '6'    ELXPMCAB
01552                MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01553            WHEN OTHER                                             ELXPMCAB
01554                MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX). ELXPMCAB
01555      IF PMCI-PROFESSIONAL                                         ELXPMCAB
01556         EVALUATE TRUE                                             ELXPMCAB
01557            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2'             ELXPMCAB
01558                MOVE WS-CF-95 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01559            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'             ELXPMCAB
01560                MOVE WS-CF-85 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01561            WHEN ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '5'      ELXPMCAB
01562                                                         OR '6'    ELXPMCAB
01563                MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)     ELXPMCAB
01564            WHEN OTHER                                             ELXPMCAB
01565                MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX). ELXPMCAB
01566                                                                   ELXPMCAB
01567 ************************************************************      ELXPMCAB
01568 *                                                          *      ELXPMCAB
01569 * SEARCH FOR ALCOHOL MAXIMUM                               *      ELXPMCAB
01570 *                                                          *      ELXPMCAB
01571 ************************************************************      ELXPMCAB
01572  8100-SRCH-ALCHL-MAX.                                             ELXPMCAB
01573                                                                   ELXPMCAB
01574      PERFORM 8101-RCMPT-BNFT-PRVSN-ALCHL.                         ELXPMCAB
01575      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01576         MOVE CVG2-OV-THRSHLD-SBSTNC-ABS TO WS-TEST-THRESHOLD      ELXPMCAB
01577         PERFORM 8110-RCMPT-CF-ALCHL                               ELXPMCAB
01578            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAB
01579               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCAB
01580                  PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.             ELXPMCAB
01581      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01582         PERFORM 8190-TST-ALCHL-MAX.                               ELXPMCAB
01583                                                                   ELXPMCAB
01584 ************************************************************      ELXPMCAB
01585 *                                                          *      ELXPMCAB
01586 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACT FOR ALCOHOL  *      ELXPMCAB
01587 *                                                          *      ELXPMCAB
01588 ************************************************************      ELXPMCAB
01589  8101-RCMPT-BNFT-PRVSN-ALCHL.                                     ELXPMCAB
01590                                                                   ELXPMCAB
01591      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-ALAB-POINTER.       ELXPMCAB
01592      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
01593      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
01594                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
01595      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCAB
01596      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
01597         MOVE +4612 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
01598         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
01599                                                                   ELXPMCAB
01600 ************************************************************      ELXPMCAB
01601 *                                                          *      ELXPMCAB
01602 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTORS FOR ALCOHOL    *      ELXPMCAB
01603 *                                                          *      ELXPMCAB
01604 ************************************************************      ELXPMCAB
01605  8110-RCMPT-CF-ALCHL.                                             ELXPMCAB
01606                                                                   ELXPMCAB
01607      MOVE WS-CF-ZERO TO WS-NO-IBGR-CF.                            ELXPMCAB
01608      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCAB
01609      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01610         MOVE WS-CF-FALSE TO WS-NO-IBGR-CF                         ELXPMCAB
01611         PERFORM 8120-RCMPT-CNDTN-BT-ALCHL.                        ELXPMCAB
01612                                                                   ELXPMCAB
01613 ************************************************************      ELXPMCAB
01614 *                                                          *      ELXPMCAB
01615 * RECOMPUTE CONDITION BITS FACTOR FOR ALCOHOL CONDITIONS   *      ELXPMCAB
01616 *                                                          *      ELXPMCAB
01617 ************************************************************      ELXPMCAB
01618  8120-RCMPT-CNDTN-BT-ALCHL.                                       ELXPMCAB
01619                                                                   ELXPMCAB
01620      IF ATBL-COND-ALCOHOL-BIT (ATBL-IDX) = '1'                    ELXPMCAB
01621         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCAB
01622            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCAB
01623         ELSE                                                      ELXPMCAB
01624            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCAB
01625      ELSE                                                         ELXPMCAB
01626      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCAB
01627               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCAB
01628         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                   ELXPMCAB
01629            MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)  ELXPMCAB
01630         ELSE                                                      ELXPMCAB
01631            MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO               ELXPMCAB
01632                            ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)        ELXPMCAB
01633      ELSE                                                         ELXPMCAB
01634         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCAB
01635                                                                   ELXPMCAB
01636 ************************************************************      ELXPMCAB
01637 *                                                          *      ELXPMCAB
01638 * TEST AND SET ALCOHOL MAXIMUMS                            *      ELXPMCAB
01639 *                                                          *      ELXPMCAB
01640 ************************************************************      ELXPMCAB
01641  8190-TST-ALCHL-MAX.                                              ELXPMCAB
01642                                                                   ELXPMCAB
01643      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01644      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01645         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01646         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01647            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01648            MOVE WS-SAVE-SUB TO WS-ALC-SUB                         ELXPMCAB
01649            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01650                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01651                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01652                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01653                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01654               SET PMCI-AL-CALL TO TRUE                            ELXPMCAB
01655            ELSE                                                   ELXPMCAB
01656               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'     ELXPMCAB
01657                  SET PMCI-AL-CALL TO TRUE                         ELXPMCAB
01658               ELSE                                                ELXPMCAB
01659                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01660                  MOVE WS-BNF-QUAL TO PMCI-MAX-ALCOHOL-QUALIFIER   ELXPMCAB
01661                  MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO          ELXPMCAB
01662                            PMCI-MAX-ALCOHOL-UNIT                  ELXPMCAB
01663                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01664                               PMCI-SBSTNCE-ABS-ALC-IND            ELXPMCAB
01665                  IF PMCI-AL-DOLLARS                               ELXPMCAB
01666                     MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO           ELXPMCAB
01667                                      PMCI-MAX-ALCOHOL             ELXPMCAB
01668                  ELSE                                             ELXPMCAB
01669                     MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO     ELXPMCAB
01670                                      PMCI-MAX-ALCOHOL             ELXPMCAB
01671                  END-IF                                           ELXPMCAB
01672               END-IF                                              ELXPMCAB
01673            END-IF                                                 ELXPMCAB
01674         ELSE                                                      ELXPMCAB
01675            SET PMCI-AL-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01676         END-IF                                                    ELXPMCAB
01677      END-IF.                                                      ELXPMCAB
01678                                                                   ELXPMCAB
01679 ************************************************************      ELXPMCAB
01680 *                                                          *      ELXPMCAB
01681 * SEARCH FOR DRUG    MAXIMUM                               *      ELXPMCAB
01682 *                                                          *      ELXPMCAB
01683 ************************************************************      ELXPMCAB
01684  8200-SRCH-DRG-MAX.                                               ELXPMCAB
01685                                                                   ELXPMCAB
01686      PERFORM 8201-RCMPT-BNFT-PRVSN-DRG.                           ELXPMCAB
01687      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01688         MOVE CVG2-OV-THRSHLD-SBSTNC-ABS TO WS-TEST-THRESHOLD      ELXPMCAB
01689         PERFORM 8210-RCMPT-CF-DRG                                 ELXPMCAB
01690            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAB
01691               UNTIL ATBL-IDX > ATBL-MAX-IDX OR                    ELXPMCAB
01692                  PMCI-BLUE-CHIP-RETURN-CODE NOT ZERO.             ELXPMCAB
01693      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01694         PERFORM 8290-TST-DRG-MAX.                                 ELXPMCAB
01695                                                                   ELXPMCAB
01696 ************************************************************      ELXPMCAB
01697 *                                                          *      ELXPMCAB
01698 * RECOMPUTE BENEFIT PROVISION CONFIDENCE FACT FOR DRUG     *      ELXPMCAB
01699 *                                                          *      ELXPMCAB
01700 ************************************************************      ELXPMCAB
01701  8201-RCMPT-BNFT-PRVSN-DRG.                                       ELXPMCAB
01702                                                                   ELXPMCAB
01703      SET ADDRESS OF BPVL-BNFT-PRVSN-TBL TO WS-DRAB-POINTER.       ELXPMCAB
01704      SET IBGR-CF-CALC-MTCH TO TRUE                                ELXPMCAB
01705      CALL 'ELKIBGRF'  USING IBGR-INTERNAL-TABS-TABLE              ELXPMCAB
01706                                BPVL-BNFT-PRVSN-TBL.               ELXPMCAB
01707      MOVE RETURN-CODE TO WS-RETURN-CODE                           ELXPMCAB
01708      IF WS-RETURN-CODE NOT ZERO                                   ELXPMCAB
01709         MOVE +4613 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAB
01710         SET PMCI-BC-INTERNAL-ERROR TO TRUE.                       ELXPMCAB
01711                                                                   ELXPMCAB
01712 ************************************************************      ELXPMCAB
01713 *                                                          *      ELXPMCAB
01714 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTORS FOR DRUG       *      ELXPMCAB
01715 *                                                          *      ELXPMCAB
01716 ************************************************************      ELXPMCAB
01717  8210-RCMPT-CF-DRG.                                               ELXPMCAB
01718                                                                   ELXPMCAB
01719      PERFORM 9120-RCMPT-BNFT-PRVSN-CF.                            ELXPMCAB
01720      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01721         PERFORM 8220-RCMPT-CNDTN-BT-DRG.                          ELXPMCAB
01722                                                                   ELXPMCAB
01723 ************************************************************      ELXPMCAB
01724 *                                                          *      ELXPMCAB
01725 * RECOMPUTE CONDITION BITS FACTOR FOR DRUG CONDITIONS      *      ELXPMCAB
01726 *                                                          *      ELXPMCAB
01727 ************************************************************      ELXPMCAB
01728  8220-RCMPT-CNDTN-BT-DRG.                                         ELXPMCAB
01729                                                                   ELXPMCAB
01730      IF ATBL-COND-DRUG-BIT (ATBL-IDX) = '1'                       ELXPMCAB
01731         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCAB
01732            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCAB
01733         ELSE                                                      ELXPMCAB
01734            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCAB
01735      ELSE                                                         ELXPMCAB
01736      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCAB
01737               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCAB
01738         MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO                  ELXPMCAB
01739                         ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)           ELXPMCAB
01740      ELSE                                                         ELXPMCAB
01741         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCAB
01742                                                                   ELXPMCAB
01743 ************************************************************      ELXPMCAB
01744 *                                                          *      ELXPMCAB
01745 * TEST AND SET DRUG MAXIMUMS                               *      ELXPMCAB
01746 *                                                          *      ELXPMCAB
01747 ************************************************************      ELXPMCAB
01748  8290-TST-DRG-MAX.                                                ELXPMCAB
01749                                                                   ELXPMCAB
01750      PERFORM 9200-RCMPT-SP-SCN-APLCBL-ENTRY.                      ELXPMCAB
01751      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01752         PERFORM 9500-DETERMINE-BASIC-MM                           ELXPMCAB
01753         IF WS-NUM-APPL-ENTRS > ZERO                               ELXPMCAB
01754            SET ATBL-IDX TO WS-SAVE-SUB                            ELXPMCAB
01755            MOVE WS-SAVE-SUB TO WS-DRG-SUB                         ELXPMCAB
01756            IF ATBL-COND-ICD-BIT (ATBL-IDX) = '1' OR               ELXPMCAB
01757                 (ATBL-COND-ALL-BIT (ATBL-IDX) = '1' AND           ELXPMCAB
01758                   ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1' AND    ELXPMCAB
01759                     ATBL-COND-NON-EMER-BIT (ATBL-IDX) = '1') OR   ELXPMCAB
01760                       WS-NUM-APPL-ENTRS > 1                       ELXPMCAB
01761               SET PMCI-MD-CALL TO TRUE                            ELXPMCAB
01762            ELSE                                                   ELXPMCAB
01763               IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '4' OR '6'     ELXPMCAB
01764                  SET PMCI-MD-CALL TO TRUE                         ELXPMCAB
01765               ELSE                                                ELXPMCAB
01766                  PERFORM 9110-FND-BNFT-PRD-CF                     ELXPMCAB
01767                  MOVE WS-BNF-QUAL TO PMCI-MAX-DRUG-QUALIFIER      ELXPMCAB
01768                  MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO          ELXPMCAB
01769                            PMCI-MAX-DRUG-UNIT                     ELXPMCAB
01770                  MOVE WS-SAVE-LOB-IND TO                          ELXPMCAB
01771                               PMCI-SBSTNCE-ABS-DRG-IND            ELXPMCAB
01772                  IF PMCI-MD-DOLLARS                               ELXPMCAB
01773                     MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO           ELXPMCAB
01774                                 PMCI-MAX-DRUG                     ELXPMCAB
01775                  ELSE                                             ELXPMCAB
01776                     MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX) TO     ELXPMCAB
01777                                 PMCI-MAX-DRUG                     ELXPMCAB
01778                  END-IF                                           ELXPMCAB
01779               END-IF                                              ELXPMCAB
01780            END-IF                                                 ELXPMCAB
01781         ELSE                                                      ELXPMCAB
01782            SET PMCI-MD-NOT-APPLICABLE TO TRUE                     ELXPMCAB
01783         END-IF                                                    ELXPMCAB
01784      END-IF.                                                      ELXPMCAB
01785                                                                   ELXPMCAB
01786 ************************************************************      ELXPMCAB
01787 *                                                          *      ELXPMCAB
01788 * TEST FOR COMBINED MENTAL DRUG AND ALCOHOL MAXIMUMS       *      ELXPMCAB
01789 *                                                          *      ELXPMCAB
01790 ************************************************************      ELXPMCAB
01791  8500-TST-CMBND-MDA-MAX.                                          ELXPMCAB
01792                                                                   ELXPMCAB
01793      IF WS-ALC-SUB = WS-DRG-SUB                                   ELXPMCAB
01794         IF WS-ALC-SUB = WS-LFM-DLR-SUB OR                         ELXPMCAB
01795                WS-ALC-SUB = WS-OTR-DLR-SUB OR                     ELXPMCAB
01796                WS-ALC-SUB = WS-LFM-DAY-SUB OR                     ELXPMCAB
01797                WS-ALC-SUB = WS-OTR-DAY-SUB                        ELXPMCAB
01798            SET PMCI-MAX-COMBO-AMD TO TRUE                         ELXPMCAB
01799            ELSE                                                   ELXPMCAB
01800            SET PMCI-MAX-COMBO-AD TO TRUE                          ELXPMCAB
01801      ELSE                                                         ELXPMCAB
01802      IF WS-ALC-SUB = WS-LFM-DLR-SUB OR                            ELXPMCAB
01803             WS-ALC-SUB = WS-OTR-DLR-SUB OR                        ELXPMCAB
01804             WS-ALC-SUB = WS-LFM-DAY-SUB OR                        ELXPMCAB
01805             WS-ALC-SUB = WS-OTR-DAY-SUB                           ELXPMCAB
01806         SET PMCI-MAX-COMBO-AM TO TRUE                             ELXPMCAB
01807      ELSE                                                         ELXPMCAB
01808      IF WS-DRG-SUB = WS-LFM-DLR-SUB OR                            ELXPMCAB
01809             WS-DRG-SUB = WS-OTR-DLR-SUB OR                        ELXPMCAB
01810             WS-DRG-SUB = WS-LFM-DAY-SUB OR                        ELXPMCAB
01811             WS-DRG-SUB = WS-OTR-DAY-SUB                           ELXPMCAB
01812         SET PMCI-MAX-COMBO-MD TO TRUE                             ELXPMCAB
01813      ELSE                                                         ELXPMCAB
01814      SET PMCI-MAX-COMBO-NONE TO TRUE.                             ELXPMCAB
01815                                                                   ELXPMCAB
01816 ************************************************************      ELXPMCAB
01817 *                                                          *      ELXPMCAB
01818 *  RECOMPUTE BENEFIT PERIOD FACTOR FOR PSYCH               *      ELXPMCAB
01819 *                                                          *      ELXPMCAB
01820 ************************************************************      ELXPMCAB
01821  9010-RCMPT-BNFT-PRD-PSYCH.                                       ELXPMCAB
01822                                                                   ELXPMCAB
01823      PERFORM 9110-FND-BNFT-PRD-CF.                                ELXPMCAB
01824      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAB
01825         MOVE CFTA-CF-PSYCH (CFTA-IDX) TO                          ELXPMCAB
01826                         ATBL-CF-BNFT-PRD (ATBL-IDX).              ELXPMCAB
01827                                                                   ELXPMCAB
01828 ************************************************************      ELXPMCAB
01829 *                                                          *      ELXPMCAB
01830 *  RECOMPUTE CONDITION BITS FACTOR FOR MENTAL              *      ELXPMCAB
01831 *                                                          *      ELXPMCAB
01832 ************************************************************      ELXPMCAB
01833  9020-RCMPT-CNDTN-BT-MNTL.                                        ELXPMCAB
01834                                                                   ELXPMCAB
01835      IF ATBL-COND-MENTAL-BIT (ATBL-IDX) = '1'                     ELXPMCAB
01836         IF ATBL-COND-EXCLUSION-BIT (ATBL-IDX) = '1'               ELXPMCAB
01837            MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)    ELXPMCAB
01838         ELSE                                                      ELXPMCAB
01839            MOVE WS-CF-TRUE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)     ELXPMCAB
01840      ELSE                                                         ELXPMCAB
01841      IF ATBL-COND-ALL-BIT (ATBL-IDX) = '1' OR                     ELXPMCAB
01842               ATBL-COND-ICD-BIT (ATBL-IDX) = '1'                  ELXPMCAB
01843         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                   ELXPMCAB
01844            MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)  ELXPMCAB
01845         ELSE                                                      ELXPMCAB
01846            MOVE ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX) TO               ELXPMCAB
01847                         ATBL-CF-SP-CNDTN-BTS (ATBL-IDX)           ELXPMCAB
01848      ELSE                                                         ELXPMCAB
01849         MOVE WS-CF-FALSE TO ATBL-CF-SP-CNDTN-BTS (ATBL-IDX).      ELXPMCAB
01850                                                                   ELXPMCAB
01851 ************************************************************      ELXPMCAB
01852 *                                                          *      ELXPMCAB
01853 * FIND BENEFIT PERIOD CONFIDENCE FACTOR TABLE ENTRY        *      ELXPMCAB
01854 *                                                          *      ELXPMCAB
01855 ************************************************************      ELXPMCAB
01856  9110-FND-BNFT-PRD-CF.                                            ELXPMCAB
01857                                                                   ELXPMCAB
01858      SET WS-BNFTPRD-NOT-FOUND TO TRUE.                            ELXPMCAB
01859      SET CFTA-MAX-IDX TO CFTA-NBR-ENTRS.                          ELXPMCAB
01860      PERFORM VARYING CFTA-IDX FROM 1 BY 1                         ELXPMCAB
01861         UNTIL CFTA-IDX > CFTA-MAX-IDX OR                          ELXPMCAB
01862           WS-BNFTPRD-FOUND                                        ELXPMCAB
01863         IF ATBL-BENEFIT-PERIOD (ATBL-IDX) =                       ELXPMCAB
01864                             CFTA-BNFT-PRD (CFTA-IDX)              ELXPMCAB
01865            SET WS-BNFTPRD-FOUND TO TRUE                           ELXPMCAB
01866            MOVE CFTA-PMCI-BNFT-PRD (CFTA-IDX) TO WS-BNF-QUAL      ELXPMCAB
01867            SET WS-SUB-WORK TO CFTA-IDX                            ELXPMCAB
01868            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCAB
01869            SET CFTA-IDX TO WS-SUB-WORK                            ELXPMCAB
01870         END-IF                                                    ELXPMCAB
01871      END-PERFORM.                                                 ELXPMCAB
01872      IF WS-BNFTPRD-NOT-FOUND                                      ELXPMCAB
01873         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAB
01874         MOVE +4603 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAB
01875                                                                   ELXPMCAB
01876 ************************************************************      ELXPMCAB
01877 *                                                          *      ELXPMCAB
01878 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCAB
01879 *                                                          *      ELXPMCAB
01880 ************************************************************      ELXPMCAB
01881  9120-RCMPT-BNFT-PRVSN-CF.                                        ELXPMCAB
01882                                                                   ELXPMCAB
01883      IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) = 0                      ELXPMCAB
01884         MOVE WS-NO-IBGR-CF TO ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)    ELXPMCAB
01885         SET WS-IBGR-FOUND TO TRUE                                 ELXPMCAB
01886      ELSE                                                         ELXPMCAB
01887         SET WS-IBGR-NOT-FOUND TO TRUE                             ELXPMCAB
01888         SET IBGR-MAX-IDX TO IBGR-TBL-CNT                          ELXPMCAB
01889         PERFORM VARYING IBGR-IDX FROM 1 BY 1                      ELXPMCAB
01890            UNTIL IBGR-IDX > IBGR-MAX-IDX OR                       ELXPMCAB
01891                WS-IBGR-FOUND                                      ELXPMCAB
01892         IF ATBL-IBGR-SLOT-NUMBER (ATBL-IDX) =                     ELXPMCAB
01893                          IBGR-SLOT-NUMBER (IBGR-IDX)              ELXPMCAB
01894            SET WS-IBGR-FOUND TO TRUE                              ELXPMCAB
01895            MOVE IBGR-CF-LIST-MTCH (IBGR-IDX) TO                   ELXPMCAB
01896                            ATBL-CF-SP-BNFT-PRVSN (ATBL-IDX)       ELXPMCAB
01897         END-IF                                                    ELXPMCAB
01898         END-PERFORM.                                              ELXPMCAB
01899      IF WS-IBGR-NOT-FOUND                                         ELXPMCAB
01900         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAB
01901         MOVE +4604 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAB
01902                                                                   ELXPMCAB
01903 ************************************************************      ELXPMCAB
01904 *                                                          *      ELXPMCAB
01905 * FIND INTERNAL DESCRIPTOR CONFIDENCE FACTOR TABLE ENTRY   *      ELXPMCAB
01906 *                                                          *      ELXPMCAB
01907 ************************************************************      ELXPMCAB
01908  9130-FND-INTRNL-DSCRPTR-CF.                                      ELXPMCAB
01909                                                                   ELXPMCAB
01910      SET WS-INTRNLDSC-NOT-FOUND TO TRUE.                          ELXPMCAB
01911      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELXPMCAB
01912      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELXPMCAB
01913         UNTIL CFT5-IDX > CFT5-MAX-IDX OR                          ELXPMCAB
01914           WS-INTRNLDSC-FOUND                                      ELXPMCAB
01915         IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) =                  ELXPMCAB
01916                           CFT5-INTD (CFT5-IDX)                    ELXPMCAB
01917            SET WS-INTRNLDSC-FOUND TO TRUE                         ELXPMCAB
01918            SET WS-SUB-WORK TO CFT5-IDX                            ELXPMCAB
01919            SUBTRACT +1 FROM WS-SUB-WORK                           ELXPMCAB
01920            SET CFT5-IDX TO WS-SUB-WORK                            ELXPMCAB
01921         END-IF                                                    ELXPMCAB
01922      END-PERFORM.                                                 ELXPMCAB
01923      IF WS-INTRNLDSC-NOT-FOUND                                    ELXPMCAB
01924         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAB
01925         MOVE +4605 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAB
01926                                                                   ELXPMCAB
01927 ************************************************************      ELXPMCAB
01928 *                                                          *      ELXPMCAB
01929 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DOLLARS         *      ELXPMCAB
01930 *                                                          *      ELXPMCAB
01931 ************************************************************      ELXPMCAB
01932  9151-RCMPT-VL-QLFR-CF-DLR.                                       ELXPMCAB
01933                                                                   ELXPMCAB
01934      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '5'                     ELXPMCAB
01935         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCAB
01936      ELSE                                                         ELXPMCAB
01937         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCAB
01938                                                                   ELXPMCAB
01939                                                                   ELXPMCAB
01940 ************************************************************      ELXPMCAB
01941 *                                                          *      ELXPMCAB
01942 *     RECOMPUTE VALUE QUALIFIER FACTOR FOR DAYS            *      ELXPMCAB
01943 *                                                          *      ELXPMCAB
01944 ************************************************************      ELXPMCAB
01945  9152-RCMPT-VL-QLFR-CF-DAY.                                       ELXPMCAB
01946                                                                   ELXPMCAB
01947      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '3'                     ELXPMCAB
01948         MOVE WS-CF-TRUE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)          ELXPMCAB
01949      ELSE                                                         ELXPMCAB
01950      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '2' OR '4' OR '6'       ELXPMCAB
01951         MOVE WS-CF-75 TO ATBL-CF-SP-VL-QLFR (ATBL-IDX)            ELXPMCAB
01952      ELSE                                                         ELXPMCAB
01953         MOVE WS-CF-FALSE TO ATBL-CF-SP-VL-QLFR (ATBL-IDX).        ELXPMCAB
01954                                                                   ELXPMCAB
01955                                                                   ELXPMCAB
01956 ************************************************************      ELXPMCAB
01957 *                                                          *      ELXPMCAB
01958 * RECOMPUTE SPECIAL CASE CONFIDENCE FACTOR AND SCAN FOR    *      ELXPMCAB
01959 *    APPLICABLE ENTRY.                                     *      ELXPMCAB
01960 *                                                          *      ELXPMCAB
01961 ************************************************************      ELXPMCAB
01962  9200-RCMPT-SP-SCN-APLCBL-ENTRY.                                  ELXPMCAB
01963                                                                   ELXPMCAB
01964      CALL 'ELKSPCFF'  USING ATBL-ACCUMULATOR-TABLE.               ELXPMCAB
01965      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAB
01966      IF WS-SUCCESSFUL-CALL                                        ELXPMCAB
01967         MOVE ZERO TO WS-SAVE-SUB-BSC                              ELXPMCAB
01968         MOVE WS-CF-FALSE TO WS-TEST-CONF-BSC                      ELXPMCAB
01969         MOVE ZERO TO WS-APPL-ENTRS-BSC                            ELXPMCAB
01970         MOVE ZERO TO WS-SAVE-SUB-MM                               ELXPMCAB
01971         MOVE WS-CF-FALSE TO WS-TEST-CONF-MM                       ELXPMCAB
01972         MOVE ZERO TO WS-APPL-ENTRS-MM                             ELXPMCAB
01973         PERFORM 9220-RCMPT-CRNT-ACCM-TBL-CF                       ELXPMCAB
01974            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAB
01975               UNTIL ATBL-IDX > ATBL-MAX-IDX                       ELXPMCAB
01976      ELSE                                                         ELXPMCAB
01977         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAB
01978         MOVE +4606 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAB
01979                                                                   ELXPMCAB
01980                                                                   ELXPMCAB
01981 ************************************************************      ELXPMCAB
01982 *                                                          *      ELXPMCAB
01983 *  RECOMPUTE CURRENT ACCUMULATOR TABLE WORK CONF FACTOR    *      ELXPMCAB
01984 *                                                          *      ELXPMCAB
01985 ************************************************************      ELXPMCAB
01986  9220-RCMPT-CRNT-ACCM-TBL-CF.                                     ELXPMCAB
01987                                                                   ELXPMCAB
01988      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAB
01989         IF PMCI-INSTITUTIONAL                                     ELXPMCAB
01990            IF PMCI-INPATIENT                                      ELXPMCAB
01991               PERFORM 9230-RCMPT-INST-INP                         ELXPMCAB
01992            ELSE                                                   ELXPMCAB
01993               PERFORM 9240-RCMPT-INST-OUT                         ELXPMCAB
01994         ELSE                                                      ELXPMCAB
01995            IF PMCI-INPATIENT                                      ELXPMCAB
01996               PERFORM 9250-RCMPT-PROF-INP                         ELXPMCAB
01997            ELSE                                                   ELXPMCAB
01998               PERFORM 9260-RCMPT-PROF-OUT.                        ELXPMCAB
01999      IF PMCI-BSC-CNTRCT-GRP EQUAL SPACE                           ELXPMCAB
02000         IF PMCI-INSTITUTIONAL                                     ELXPMCAB
02001            IF PMCI-INPATIENT                                      ELXPMCAB
02002               PERFORM 9235-RCMPT-INST-INP-MM                      ELXPMCAB
02003            ELSE                                                   ELXPMCAB
02004               PERFORM 9245-RCMPT-INST-OUT-MM                      ELXPMCAB
02005         ELSE                                                      ELXPMCAB
02006            IF PMCI-INPATIENT                                      ELXPMCAB
02007               PERFORM 9255-RCMPT-PROF-INP-MM                      ELXPMCAB
02008            ELSE                                                   ELXPMCAB
02009               PERFORM 9265-RCMPT-PROF-OUT-MM.                     ELXPMCAB
02010                                                                   ELXPMCAB
02011 ************************************************************      ELXPMCAB
02012 *                                                          *      ELXPMCAB
02013 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02014 *                                                          *      ELXPMCAB
02015 ************************************************************      ELXPMCAB
02016  9230-RCMPT-INST-INP.                                             ELXPMCAB
02017                                                                   ELXPMCAB
02018      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02019                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02020                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02021                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCAB
02022                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCAB
02023                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02024                             ATBL-CF-SP (ATBL-IDX).                ELXPMCAB
02025      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02026                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCAB
02027         CONTINUE                                                  ELXPMCAB
02028      ELSE                                                         ELXPMCAB
02029         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02030                      WS-WT-BNFT-PRD                               ELXPMCAB
02031         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02032                      WS-WT-INDVDL                                 ELXPMCAB
02033         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAB
02034                      WS-WT-INST-BAS                               ELXPMCAB
02035         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAB
02036                      WS-WT-IP                                     ELXPMCAB
02037         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02038                      WS-WT-PLAN                                   ELXPMCAB
02039         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02040                      WS-WT-SP                                     ELXPMCAB
02041         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02042      END-IF.                                                      ELXPMCAB
02043                                                                   ELXPMCAB
02044      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02045         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCAB
02046         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCAB
02047            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCAB
02048            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02049                                    WS-TEST-CONF-BSC.              ELXPMCAB
02050                                                                   ELXPMCAB
02051      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCAB
02052         PERFORM 9235-RCMPT-INST-INP-MM.                           ELXPMCAB
02053                                                                   ELXPMCAB
02054 ************************************************************      ELXPMCAB
02055 *                                                          *      ELXPMCAB
02056 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02057 *  IF MAJOR MEDICAL CONTRACT                               *      ELXPMCAB
02058 ************************************************************      ELXPMCAB
02059  9235-RCMPT-INST-INP-MM.                                          ELXPMCAB
02060                                                                   ELXPMCAB
02061 * PROCESS MAJOR MEDICAL CALCULATIONS.                             ELXPMCAB
02062      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02063                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02064                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02065                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCAB
02066                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCAB
02067                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02068                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02069      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02070                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCAB
02071         CONTINUE                                                  ELXPMCAB
02072      ELSE                                                         ELXPMCAB
02073         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02074                      WS-WT-BNFT-PRD                               ELXPMCAB
02075         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02076                      WS-WT-INDVDL                                 ELXPMCAB
02077         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAB
02078                      WS-WT-INST-SUP                               ELXPMCAB
02079         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAB
02080                      WS-WT-IP                                     ELXPMCAB
02081         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02082                      WS-WT-PLAN                                   ELXPMCAB
02083         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02084                      WS-WT-SP                                     ELXPMCAB
02085         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02086      END-IF.                                                      ELXPMCAB
02087                                                                   ELXPMCAB
02088      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02089         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCAB
02090         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCAB
02091            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCAB
02092            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02093                                    WS-TEST-CONF-MM.               ELXPMCAB
02094                                                                   ELXPMCAB
02095 ************************************************************      ELXPMCAB
02096 *                                                          *      ELXPMCAB
02097 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02098 *                                                          *      ELXPMCAB
02099 ************************************************************      ELXPMCAB
02100  9240-RCMPT-INST-OUT.                                             ELXPMCAB
02101                                                                   ELXPMCAB
02102      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02103                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02104                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02105                             ATBL-CF-INST-BAS (ATBL-IDX)           ELXPMCAB
02106                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCAB
02107                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02108                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02109      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02110                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCAB
02111         CONTINUE                                                  ELXPMCAB
02112         ELSE                                                      ELXPMCAB
02113         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02114                      WS-WT-BNFT-PRD                               ELXPMCAB
02115         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02116                      WS-WT-INDVDL                                 ELXPMCAB
02117         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAB
02118                      WS-WT-INST-BAS                               ELXPMCAB
02119         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAB
02120                      WS-WT-OP                                     ELXPMCAB
02121         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02122                       WS-WT-PLAN                                  ELXPMCAB
02123         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02124                      WS-WT-SP                                     ELXPMCAB
02125         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02126         END-IF.                                                   ELXPMCAB
02127                                                                   ELXPMCAB
02128      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02129         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCAB
02130         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCAB
02131            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCAB
02132            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02133                                    WS-TEST-CONF-BSC.              ELXPMCAB
02134                                                                   ELXPMCAB
02135      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCAB
02136         PERFORM 9245-RCMPT-INST-OUT-MM.                           ELXPMCAB
02137 ************************************************************      ELXPMCAB
02138 *                                                          *      ELXPMCAB
02139 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02140 *  MAJOR MEDICAL CONTRACTS ONLY.                           *      ELXPMCAB
02141 ************************************************************      ELXPMCAB
02142  9245-RCMPT-INST-OUT-MM.                                          ELXPMCAB
02143                                                                   ELXPMCAB
02144                                                                   ELXPMCAB
02145 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCAB
02146      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02147                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02148                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02149                             ATBL-CF-INST-SUP (ATBL-IDX)           ELXPMCAB
02150                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCAB
02151                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02152                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02153      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02154                          WS-CF-TRUE OR WS-CF-FALSE                ELXPMCAB
02155         CONTINUE                                                  ELXPMCAB
02156         ELSE                                                      ELXPMCAB
02157         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02158                      WS-WT-BNFT-PRD                               ELXPMCAB
02159         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02160                      WS-WT-INDVDL                                 ELXPMCAB
02161         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAB
02162                      WS-WT-INST-SUP                               ELXPMCAB
02163         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAB
02164                      WS-WT-OP                                     ELXPMCAB
02165         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02166                       WS-WT-PLAN                                  ELXPMCAB
02167         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02168                      WS-WT-SP                                     ELXPMCAB
02169         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02170      END-IF.                                                      ELXPMCAB
02171                                                                   ELXPMCAB
02172      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02173         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCAB
02174         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCAB
02175            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCAB
02176            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02177                                    WS-TEST-CONF-MM.               ELXPMCAB
02178                                                                   ELXPMCAB
02179 ************************************************************      ELXPMCAB
02180 *                                                          *      ELXPMCAB
02181 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02182 *                                                          *      ELXPMCAB
02183 ************************************************************      ELXPMCAB
02184  9250-RCMPT-PROF-INP.                                             ELXPMCAB
02185                                                                   ELXPMCAB
02186      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02187                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02188                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02189                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCAB
02190                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCAB
02191                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02192                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02193      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02194                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCAB
02195         CONTINUE                                                  ELXPMCAB
02196      ELSE                                                         ELXPMCAB
02197          COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *          ELXPMCAB
02198                      WS-WT-BNFT-PRD                               ELXPMCAB
02199          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCAB
02200                      WS-WT-INDVDL                                 ELXPMCAB
02201          COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *          ELXPMCAB
02202                      WS-WT-PROF-BAS                               ELXPMCAB
02203          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCAB
02204                      WS-WT-IP                                     ELXPMCAB
02205          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCAB
02206                      WS-WT-PLAN                                   ELXPMCAB
02207          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCAB
02208                      WS-WT-SP                                     ELXPMCAB
02209          PERFORM 9300-COMBINE-FACTORS                             ELXPMCAB
02210      END-IF.                                                      ELXPMCAB
02211                                                                   ELXPMCAB
02212      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02213         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCAB
02214         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCAB
02215            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCAB
02216            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02217                                    WS-TEST-CONF-BSC.              ELXPMCAB
02218                                                                   ELXPMCAB
02219      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCAB
02220         PERFORM 9255-RCMPT-PROF-INP-MM.                           ELXPMCAB
02221                                                                   ELXPMCAB
02222 ************************************************************      ELXPMCAB
02223 *                                                          *      ELXPMCAB
02224 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02225 *   MAJOR MEDICAL CONTRACTS ONLY                           *      ELXPMCAB
02226 ************************************************************      ELXPMCAB
02227  9255-RCMPT-PROF-INP-MM.                                          ELXPMCAB
02228                                                                   ELXPMCAB
02229 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCAB
02230                                                                   ELXPMCAB
02231      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02232                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02233                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02234                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCAB
02235                             ATBL-CF-IP (ATBL-IDX)                 ELXPMCAB
02236                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02237                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02238      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02239                                WS-CF-TRUE OR WS-CF-FALSE          ELXPMCAB
02240         CONTINUE                                                  ELXPMCAB
02241      ELSE                                                         ELXPMCAB
02242          COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *          ELXPMCAB
02243                      WS-WT-BNFT-PRD                               ELXPMCAB
02244          COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *            ELXPMCAB
02245                      WS-WT-INDVDL                                 ELXPMCAB
02246          COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *          ELXPMCAB
02247                      WS-WT-PROF-SUP                               ELXPMCAB
02248          COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                ELXPMCAB
02249                      WS-WT-IP                                     ELXPMCAB
02250          COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *              ELXPMCAB
02251                      WS-WT-PLAN                                   ELXPMCAB
02252          COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                ELXPMCAB
02253                      WS-WT-SP                                     ELXPMCAB
02254          PERFORM 9300-COMBINE-FACTORS                             ELXPMCAB
02255      END-IF.                                                      ELXPMCAB
02256                                                                   ELXPMCAB
02257      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02258         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCAB
02259         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCAB
02260            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCAB
02261            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02262                                    WS-TEST-CONF-MM.               ELXPMCAB
02263                                                                   ELXPMCAB
02264 ************************************************************      ELXPMCAB
02265 *                                                          *      ELXPMCAB
02266 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02267 *                                                          *      ELXPMCAB
02268 ************************************************************      ELXPMCAB
02269  9260-RCMPT-PROF-OUT.                                             ELXPMCAB
02270                                                                   ELXPMCAB
02271      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02272                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02273                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02274                             ATBL-CF-PROF-BAS (ATBL-IDX)           ELXPMCAB
02275                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCAB
02276                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02277                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02278      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02279                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCAB
02280         CONTINUE                                                  ELXPMCAB
02281      ELSE                                                         ELXPMCAB
02282         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02283                      WS-WT-BNFT-PRD                               ELXPMCAB
02284         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02285                      WS-WT-INDVDL                                 ELXPMCAB
02286         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAB
02287                      WS-WT-PROF-BAS                               ELXPMCAB
02288         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAB
02289                      WS-WT-OP                                     ELXPMCAB
02290         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02291                      WS-WT-PLAN                                   ELXPMCAB
02292         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02293                      WS-WT-SP                                     ELXPMCAB
02294         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02295      END-IF.                                                      ELXPMCAB
02296                                                                   ELXPMCAB
02297      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02298         ADD +1 TO WS-APPL-ENTRS-BSC                               ELXPMCAB
02299         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-BSC       ELXPMCAB
02300            SET WS-SAVE-SUB-BSC TO ATBL-IDX                        ELXPMCAB
02301            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02302                                    WS-TEST-CONF-BSC.              ELXPMCAB
02303                                                                   ELXPMCAB
02304      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                       ELXPMCAB
02305         PERFORM 9265-RCMPT-PROF-OUT-MM.                           ELXPMCAB
02306                                                                   ELXPMCAB
02307 ************************************************************      ELXPMCAB
02308 *                                                          *      ELXPMCAB
02309 *  RECOMPUTE CONFIDENCE FACTORS  USING COMBINE AND WEIGHTS *      ELXPMCAB
02310 *  MAJOR MEDICAL CONTRACTS ONLY                            *      ELXPMCAB
02311 ************************************************************      ELXPMCAB
02312  9265-RCMPT-PROF-OUT-MM.                                          ELXPMCAB
02313                                                                   ELXPMCAB
02314 * PROCESS MAJOR MEDICAL FACTORS.                                  ELXPMCAB
02315                                                                   ELXPMCAB
02316      CALL 'ELKFLAND'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02317                             ATBL-CF-BNFT-PRD (ATBL-IDX)           ELXPMCAB
02318                             ATBL-CF-INDVDL (ATBL-IDX)             ELXPMCAB
02319                             ATBL-CF-PROF-SUP (ATBL-IDX)           ELXPMCAB
02320                             ATBL-CF-OP (ATBL-IDX)                 ELXPMCAB
02321                             ATBL-CF-PLAN (ATBL-IDX)               ELXPMCAB
02322                             ATBL-CF-SP (ATBL-IDX)                 ELXPMCAB
02323      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAB
02324                            WS-CF-TRUE OR WS-CF-FALSE              ELXPMCAB
02325         CONTINUE                                                  ELXPMCAB
02326      ELSE                                                         ELXPMCAB
02327         COMPUTE WS-CF-1 = ATBL-CF-BNFT-PRD (ATBL-IDX) *           ELXPMCAB
02328                      WS-WT-BNFT-PRD                               ELXPMCAB
02329         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAB
02330                      WS-WT-INDVDL                                 ELXPMCAB
02331         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCAB
02332                      WS-WT-PROF-SUP                               ELXPMCAB
02333         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAB
02334                      WS-WT-OP                                     ELXPMCAB
02335         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAB
02336                      WS-WT-PLAN                                   ELXPMCAB
02337         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAB
02338                      WS-WT-SP                                     ELXPMCAB
02339         PERFORM 9300-COMBINE-FACTORS                              ELXPMCAB
02340      END-IF.                                                      ELXPMCAB
02341                                                                   ELXPMCAB
02342      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-TEST-THRESHOLD        ELXPMCAB
02343         ADD +1 TO WS-APPL-ENTRS-MM                                ELXPMCAB
02344         IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-TEST-CONF-MM        ELXPMCAB
02345            SET WS-SAVE-SUB-MM TO ATBL-IDX                         ELXPMCAB
02346            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO                  ELXPMCAB
02347                                    WS-TEST-CONF-MM.               ELXPMCAB
02348 ************************************************************      ELXPMCAB
02349 *                                                          *      ELXPMCAB
02350 *  COMBINE FACTORS                                         *      ELXPMCAB
02351 *                                                          *      ELXPMCAB
02352 ************************************************************      ELXPMCAB
02353  9300-COMBINE-FACTORS.                                            ELXPMCAB
02354                                                                   ELXPMCAB
02355      CALL 'ELKFLCMB'  USING ATBL-CF-WORK-ENTRY (ATBL-IDX)         ELXPMCAB
02356                            WS-CF-1                                ELXPMCAB
02357                            WS-CF-2                                ELXPMCAB
02358                            WS-CF-3                                ELXPMCAB
02359                            WS-CF-4                                ELXPMCAB
02360                            WS-CF-5                                ELXPMCAB
02361                            WS-CF-6.                               ELXPMCAB
02362                                                                   ELXPMCAB
02363 ************************************************************      ELXPMCAB
02364 *                                                          *      ELXPMCAB
02365 *  DETERMINE IF BASIC OR MAJOR MEDICAL BENEFIT APPLIES     *      ELXPMCAB
02366 *                                                          *      ELXPMCAB
02367 ************************************************************      ELXPMCAB
02368                                                                   ELXPMCAB
02369  9500-DETERMINE-BASIC-MM.                                         ELXPMCAB
02370                                                                   ELXPMCAB
02371      MOVE SPACES TO WS-SAVE-LOB-IND.                              ELXPMCAB
02372      IF WS-SAVE-SUB-BSC NOT EQUAL ZERO AND WS-SAVE-SUB-MM         ELXPMCAB
02373               NOT EQUAL ZERO                                      ELXPMCAB
02374         MOVE '+' TO WS-SAVE-LOB-IND                               ELXPMCAB
02375         MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                       ELXPMCAB
02376         MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS               ELXPMCAB
02377      ELSE                                                         ELXPMCAB
02378         IF WS-SAVE-SUB-MM NOT EQUAL ZERO                          ELXPMCAB
02379            MOVE '*' TO WS-SAVE-LOB-IND                            ELXPMCAB
02380            MOVE WS-SAVE-SUB-MM TO WS-SAVE-SUB                     ELXPMCAB
02381            MOVE WS-APPL-ENTRS-MM TO WS-NUM-APPL-ENTRS             ELXPMCAB
02382         ELSE                                                      ELXPMCAB
02383            MOVE WS-SAVE-SUB-BSC TO WS-SAVE-SUB                    ELXPMCAB
02384            MOVE WS-APPL-ENTRS-BSC TO WS-NUM-APPL-ENTRS.           ELXPMCAB
