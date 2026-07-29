00001  IDENTIFICATION DIVISION.                                         08/26/05
00002                                                                   ELXPMCAP
00003  PROGRAM-ID.         ELXPMCAP.                                       LV003
00004                                                                   ELXPMCAP
00005  AUTHOR.             ANNE KEFFER-KING.                            ELXPMCAP
00006                                                                   ELXPMCAP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCAP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCAP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCAP
00010                      233 N. MICHIGAN AVE                          ELXPMCAP
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCAP
00012                                                                   ELXPMCAP
00013                                                                   ELXPMCAP
00014  DATE-WRITTEN.       05-DEC-1992.                                 ELXPMCAP
00015                                                                   ELXPMCAP
00016  DATE-COMPILED.                                                   ELXPMCAP
00017                                                                   ELXPMCAP
00018  SECURITY.           COPYRIGHT 1992,                              ELXPMCAP
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCAP
00020      SKIP3                                                        ELXPMCAP
00021  TITLE 'ELIGIBILITY SUMMARY CCP ACCUMULATOR DISPLAY   '.          ELXPMCAP
00022  ENVIRONMENT DIVISION.                                            ELXPMCAP
00023                                                                   ELXPMCAP
00024  CONFIGURATION SECTION.                                           ELXPMCAP
00025  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCAP
00026  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCAP
00027      EJECT                                                        ELXPMCAP
00028 ******************************************************************ELXPMCAP
00029 *                                                                *ELXPMCAP
00030 *                                                                *ELXPMCAP
00031 *    PROGRAM:    ELXPMCAP                                        *ELXPMCAP
00032 *    DATE:       05-DEC-1992                                     *ELXPMCAP
00033 *    AUTHOR:     ANNE KING                                       *ELXPMCAP
00034 *    FUNCTION:   PREPARE DISPLAY FOR CCP ACCUMULATORS            *ELXPMCAP
00035 *                -LIFETIME MAXIMUMS FOR HOSPICE AND ATCP         *ELXPMCAP
00036 *                -DEDUCTIBLES FOR MSA OR PAR                     *ELXPMCAP
00037 *                -THIS IS A SUBROUTINE CALLED FROM ELXPMAC       *ELXPMCAP
00038 * NOTE:  NOSPACE WAS ALLOWED IN THE PMCI-COMM-AREA FOR PAR       *ELXPMCAP
00039 *        DEDUCTIBLE, SO WE ARE SHARING THE MSA DEDUCTIBLE SECTION*ELXPMCAP
00040 *        W/ PAR BECAUSE THEY ARE MUTAULLY EXCLUSIVE.             *ELXPMCAP
00041 *                                                                *ELXPMCAP
00042 * NOTE2: THIS PROGRAM MUST BE COMPILED USING BATCH COMPILE JCL   *ELXPMCAP
00043 *        (ELBATC2X)                                              *ELXPMCAP
00044 ******************************************************************ELXPMCAP
00045 ******************************************************************ELXPMCAP
00046 *                    ERROR CODE LOG                              *ELXPMCAP
00047 ******************************************************************ELXPMCAP
00048 *  +4500 - CALLED WITH A NON-SUCCESSFUL RETURN CODE              *ELXPMCAP
00049 *  +4501 - UNIDENTIFIED PARAMETERS FROM CALL TO ELKSPCFF-ATCP    *ELXPMCAP
00050 *  +4502 - MISSING PARAMETERS FROM CALL TO ELKSPCFF-ATCP         *ELXPMCAP
00051 *  +4503 - INTERNAL ERROR FROM CALL TO ELKSPCFF-ATCP             *ELXPMCAP
00052 *  +4504 - UNIDENTIFIED PARAMETERS FROM CALL TO ELKSPCFF-HOSP    *ELXPMCAP
00053 *  +4505 - MISSING PARAMETERS FROM CALL TO ELKSPCFF-HOSP         *ELXPMCAP
00054 *  +4506 - INTERNAL ERROR FROM CALL TO ELKSPCFF-HOSP             *ELXPMCAP
00055 *  +4507 - UNIDENTIFIED PARAMETERS FROM CALL TO ELKSPCFF-HOSP    *ELXPMCAP
00056 *  +4508 - MISSING PARAMETERS FROM CALL TO ELKSPCFF-MSA          *ELXPMCAP
00057 *  +4509 - INTERNAL ERROR FROM CALL TO ELKSPCFF-MSA              *ELXPMCAP
00058 ******************************************************************ELXPMCAP
00059 *                                                                *ELXPMCAP
00060 *                      MAINTENANCE HISTORY                       *ELXPMCAP
00061 *                                                                *ELXPMCAP
00062 * MOD      DATE     BY  DRPT                ACTION               *ELXPMCAP
00063 * ----- ----------- --- ----- ---------------------------------- *ELXPMCAP
00064 * 01.00 05-DEC-1992 AKK       CREATED                            *ELXPMCAP
00065 *                                                                *ELXPMCAP
00066 * 01.01 29-DEC-1992 JPB       ADDED COPY STATEMENT FOR ELSCVG2.  *ELXPMCAP
00067 *                                                                *ELXPMCAP
00068 * 01.02 ??-MAR-1993 RJL       REMOVED UNNECESSARY TESTS IN ATCP  *ELXPMCAP
00069 *                                                                *ELXPMCAP
00070 * 01.03 13-MAR-1993 AKK       REORDERED PARAGRAPHS IN NUMERIC    *ELXPMCAP
00071 *                             ORDER.                             *ELXPMCAP
00072 *                                                                *ELXPMCAP
00073 * 01.04 17-MAR-1993 CGL       REREORDERED PARAGRAPHS IN NUMERIC  *ELXPMCAP
00074 * 01.04 17-MAR-1993 CGL       ORDER, CORRECTION TO PRODUCES HOTS *ELXPMCAP
00075 *                             DOLLARS SE INTERNAL-DISCR-FND      *ELXPMCAP
00076 *                             SWITCH, AND RESTRUCTURED.          *ELXPMCAP
00077 *                                                                *ELXPMCAP
00078 * 02.00 01-JUN-1993 AKK       ADD CODE TO ACCESS MAJOR MEDICAL   *ELXPMCAP
00079 *                             BENEFITS. ISSR #13071              *ELXPMCAP
00080 * 02.01 13-JUL-1993 BAK       ADD MSA CALL CODE AND CORRECT HOTS *ELXPMCAP
00081 *                             FOR PROFESSIONAL OUTPATIENT.       *ELXPMCAP
00082 * 03.00 05-OCT-1993 BAK       ADD SUPPORT FOR MSA CO-INSURANCE   *ELXPMCAP
00083 *                             AND RE-ARRANGE PARAGRAPHS 13071-2  *ELXPMCAP
00084 * 03.01 01-DEC-1993 RGO       FIX FOR HOTS INSTITUTIONAL INPATIENTELXPMCAP
00085 *                             IN 2340-PRPR.                      *ELXPMCAP
00086 * 04.00 18-MAY-2000 AKK       ADD CODE TO FIX MSA DED.            ELXPMCAP
00087 *                                                                *ELXPMCAP
00088 * 04.00 12-MAR-2003 AKK       REGEN'D WITH PMCCOMM USED BY ELS    ELXPMCAP
00089 *                                                                *ELXPMCAP
00090 * 04.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELXPMCAP
00091 *                                                                *ELXPMCAP
00092 * 05.00 01-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCAP
00093 *                                                                *ELXPMCAP
00094 * 05.01 09-JAN-2004 AKK INTERTEST S0C7                           *ELXPMCAP
00095 *                                                                *ELXPMCAP
00096 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER CHANGES         *ELXPMCAP
00097 *                                                                *ELXPMCAP
00098 ******************************************************************ELXPMCAP
00099      EJECT                                                        ELXPMCAP
00100  DATA DIVISION.                                                   ELXPMCAP
00101  WORKING-STORAGE SECTION.                                         ELXPMCAP
00102  01  WS-HDG                      PICTURE X(32)                    ELXPMCAP
00103           VALUE '**** ELXPMCAP WS STARTS HERE ***'.               ELXPMCAP
00104 *                                                                 ELXPMCAP
00105  01  WS-MISC.                                                     ELXPMCAP
00106      05  HOLD-GSS-IDX            USAGE IS INDEX.                  ELXPMCAP
00107      05  WS-GCCP-MAX-IDX         USAGE IS INDEX.                  ELXPMCAP
00108      05  WS-DOLLARS              PIC X            VALUE '5'.      ELXPMCAP
00109      05  WS-ZERO                 PIC 9            VALUE  0.       ELXPMCAP
00110      05  WS-1                    PIC X            VALUE '1'.      ELXPMCAP
00111      05  WS-PROCESSING-SWITCH    PIC X.                           ELXPMCAP
00112          88  PROCESSING-ATCP                      VALUE 'A'.      ELXPMCAP
00113          88  PROCESSING-MSA                       VALUE 'M'.      ELXPMCAP
00114          88  PROCESSING-PAR                       VALUE 'P'.      ELXPMCAP
00115          88  PROCESSING-HOSP                      VALUE 'P'.      ELXPMCAP
00116      05  WS-COBOL-RETURN-CODE    PIC S9(04) COMP.                 ELXPMCAP
00117          88  WS-SUCCESSFUL-CALL                   VALUE +0.       ELXPMCAP
00118          88  WS-UNIDENT-PARM                      VALUE +8.       ELXPMCAP
00119          88  WS-MISSING-PARM                      VALUE +12.      ELXPMCAP
00120          88  WS-INTERNAL-ERROR                    VALUE +16.      ELXPMCAP
00121      05  ACCUM-PROCESSING-SW     PIC X(01).                       ELXPMCAP
00122          88 PROCESSING-MAX                        VALUE 'M'.      ELXPMCAP
00123          88 PROCESSING-DED                        VALUE 'D'.      ELXPMCAP
00124      05  ACCUM-PROCESSING-SW     PIC X(01).                       ELXPMCAP
00125          88 WS-GCCP-FOUND                         VALUE 'Y'.      ELXPMCAP
00126          88 WS-GCCP-NOT-FOUND                     VALUE 'N'.      ELXPMCAP
00127      05  INTERNAL-DESCRPTOR-SW   PIC X(01).                       ELXPMCAP
00128          88 INTRNL-DSCRPTR-FND                    VALUE 'I'.      ELXPMCAP
00129          88 INTRNL-DSCRPTR-NOT-FND                VALUE 'N'.      ELXPMCAP
00130 *                                                                 ELXPMCAP
00131 ******************************************************************ELXPMCAP
00132 * VALUES COMMON TO HOSPICE, MASOP, MSA AND WEEKEND USED FOR       ELXPMCAP
00133 *MSA ONLY HERE.                                                   ELXPMCAP
00134 ******************************************************************ELXPMCAP
00135  01  WS-COMMON-PART-IND         PIC X(02).                        ELXPMCAP
00136      88  WS-COMMON-INST-BSC     VALUE '01' '04'.                  ELXPMCAP
00137      88  WS-COMMON-INST-MM      VALUE '03' '07'.                  ELXPMCAP
00138      88  WS-COMMON-INST-BOTH    VALUE '05' '06'.                  ELXPMCAP
00139      88  WS-COMMON-INST-NO      VALUE '00' '02'.                  ELXPMCAP
00140                                                                   ELXPMCAP
00141      88  WS-COMMON-PROF-BSC     VALUE '02' '04'.                  ELXPMCAP
00142      88  WS-COMMON-PROF-MM      VALUE '03' '06'.                  ELXPMCAP
00143      88  WS-COMMON-PROF-BOTH    VALUE '05' '07'.                  ELXPMCAP
00144      88  WS-COMMON-PROF-NO      VALUE '00' '01'.                  ELXPMCAP
00145                                                                   ELXPMCAP
00146      88  WS-COMMON-CALL         VALUE '08'.                       ELXPMCAP
00147                                                                   ELXPMCAP
00148  01  WS-POT-GROUPINGS.                                            ELXPMCAP
00149      05  WS-TEST-POT      PIC X(02).                              ELXPMCAP
00150          88  INST-INPATIENT                 VALUE '0B' '0C' '0D'  ELXPMCAP
00151                                                   '0R' '0S' '0U'  ELXPMCAP
00152                                                   '0V' '01' '02'  ELXPMCAP
00153                                                   '03' '05' '06'  ELXPMCAP
00154                                                   '10'.           ELXPMCAP
00155          88  INST-OUTPATIENT                VALUE '0E' '0F' '0G'  ELXPMCAP
00156                                                   '0H' '0I' '0J'  ELXPMCAP
00157                                                   '0K' '0L' '0M'  ELXPMCAP
00158                                                   '0N' '0P' '0Q'  ELXPMCAP
00159                                                   '0R' '0S' '0T'  ELXPMCAP
00160                                                   '0U' '0V' '01'  ELXPMCAP
00161                                                   '03' '04' '05'  ELXPMCAP
00162                                                   '06' '07' '10'  ELXPMCAP
00163                                                   '11' '13'.      ELXPMCAP
00164          88  PROF-INPATIENT                 VALUE '0B' '0C' '0D'  ELXPMCAP
00165                                                   '0R' '0S' '0U'  ELXPMCAP
00166                                                   '0V' '02' '03'  ELXPMCAP
00167                                                   '05' '06' '10'. ELXPMCAP
00168                                                                   ELXPMCAP
00169          88  PROF-OUTPATIENT                VALUE '0A' '0B' '0C'  ELXPMCAP
00170                                                   '0E' '0F' '0G'  ELXPMCAP
00171                                                   '0H' '0J' '0K'  ELXPMCAP
00172                                                   '0M' '0N' '0P'  ELXPMCAP
00173                                                   '0Q' '0R' '0S'  ELXPMCAP
00174                                                   '0T' '0U' '01'  ELXPMCAP
00175                                                   '03' '04' '05'  ELXPMCAP
00176                                                   '06' '07' '08'  ELXPMCAP
00177                                                   '10' '11' '12'  ELXPMCAP
00178                                                   '13'.           ELXPMCAP
00179  01  WS-BENEFIT-PERIOD-GROUPINGS PIC X(02).                       ELXPMCAP
00180      88  WS-LIFETIME-GROUP                       VALUE '0B' '0Q'  ELXPMCAP
00181                                                    '0R' '0V' '0Z'.ELXPMCAP
00182      88  WS-ANNUAL-GROUP                         VALUE '0C' '0D'  ELXPMCAP
00183                                                    '0E' '0L' '0P' ELXPMCAP
00184                                                    '0T' '0U' '0W' ELXPMCAP
00185                                                    'AB'.          ELXPMCAP
00186      88  WS-CONFINEMENT-GROUP                    VALUE '0A' '0M'. ELXPMCAP
00187      88  WS-OCCURENCE-GROUP                      VALUE '0I' '0J'. ELXPMCAP
00188      88  WS-TREATMENT-GROUP                      VALUE 'AA' 'AC'. ELXPMCAP
00189      88  WS-VISIT-GROUP                          VALUE '0Y'.      ELXPMCAP
00190                                                                   ELXPMCAP
00191 *                                                                 ELXPMCAP
00192  01  WS-CCP-TEST-GROUP              PIC X(02).                    ELXPMCAP
00193      88  WS-PAR-GROUP                           VALUE '61' '62'   ELXPMCAP
00194                                                         '63' '64' ELXPMCAP
00195                                                         'E4' 'G4'.ELXPMCAP
00196      88  WS-MSA-GROUP                           VALUE '81' '82'   ELXPMCAP
00197                                                        '83' '84'  ELXPMCAP
00198                                                        '85'.      ELXPMCAP
00199  01  WS-CCP-GROUPINGS               PIC X(02).                    ELXPMCAP
00200      88  WS-ATCP-CMPLNC-GRP                      VALUE 'A1' 'H1'. ELXPMCAP
00201      88  WS-ATCP-CALL-GRP                        VALUE 'A2' 'A3'  ELXPMCAP
00202                                                    'A4'.          ELXPMCAP
00203      88  WS-HOSP-CMPLNC-GRP                      VALUE 'B1' 'F1'. ELXPMCAP
00204      88  WS-HOSP-CALL-GRP                        VALUE 'B2' 'B3'  ELXPMCAP
00205                                                    'B4'.          ELXPMCAP
00206      88  WS-MSA-CALL-GRP                         VALUE '81' '85'. ELXPMCAP
00207      88  WS-MSA-NON-CMPLNC-GRP                   VALUE '82' '83'  ELXPMCAP
00208                                                    '84'.          ELXPMCAP
00209      88  WS-PAR-CALL-GRP                         VALUE '61'.      ELXPMCAP
00210      88  WS-PAR-NON-CMPLNC-GRP                   VALUE '62' '63'  ELXPMCAP
00211                                                    '64' 'E4' 'G4'.ELXPMCAP
00212 *                                                                 ELXPMCAP
00213  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCAP
00214      05  WS-CF-1                  COMP-1.                         ELXPMCAP
00215      05  WS-CF-2                  COMP-1.                         ELXPMCAP
00216      05  WS-CF-3                  COMP-1.                         ELXPMCAP
00217      05  WS-CF-4                  COMP-1.                         ELXPMCAP
00218      05  WS-CF-5                  COMP-1.                         ELXPMCAP
00219      05  WS-CF-6                  COMP-1.                         ELXPMCAP
00220 *                                                                 ELXPMCAP
00221  01  WS-WEIGHTS.                                                  ELXPMCAP
00222 **FIRST CF IS BEING USED TO RECALC DED NOT FOR WEIGHTING PURPOSES ELXPMCAP
00223 **THIS WAS A GOOD PLACE TO STICK IT.                              ELXPMCAP
00224      05  WS-PAR-CF                COMP-1          VALUE 0.80E+00. ELXPMCAP
00225      05  WS-WT-LFTM               COMP-1          VALUE 0.75E+00. ELXPMCAP
00226      05  WS-WT-ANL                COMP-1          VALUE 0.75E+00. ELXPMCAP
00227      05  WS-WT-FMLY               COMP-1          VALUE 0.50E+00. ELXPMCAP
00228      05  WS-WT-INDVDL             COMP-1          VALUE 0.50E+00. ELXPMCAP
00229      05  WS-WT-INST-BAS           COMP-1          VALUE 0.50E+00. ELXPMCAP
00230      05  WS-WT-INST-SUP           COMP-1          VALUE 0.50E+00. ELXPMCAP
00231      05  WS-WT-PROF-BAS           COMP-1          VALUE 0.50E+00. ELXPMCAP
00232      05  WS-WT-PROF-SUP           COMP-1          VALUE 0.50E+00. ELXPMCAP
00233      05  WS-WT-IP                 COMP-1          VALUE 0.50E+00. ELXPMCAP
00234      05  WS-WT-OP                 COMP-1          VALUE 0.50E+00. ELXPMCAP
00235      05  WS-WT-PLAN               COMP-1          VALUE 0.25E+00. ELXPMCAP
00236      05  WS-WT-NON-PLAN           COMP-1          VALUE 0.25E+00. ELXPMCAP
00237      05  WS-WT-OV                 COMP-1          VALUE 0.50E+00. ELXPMCAP
00238      05  WS-WT-SP                 COMP-1          VALUE 0.50E+00. ELXPMCAP
00239                                                                   ELXPMCAP
00240  01  WS-ABSOLUTE-CONFIDENCE-FACTORS.                              ELXPMCAP
00241      05  WS-CF-TRUE               COMP-1          VALUE +1.00E+00.ELXPMCAP
00242      05  WS-CF-FALSE              COMP-1          VALUE -1.00E+00.ELXPMCAP
00243      05  WS-CF-ZERO               COMP-1          VALUE +0.00E+00.ELXPMCAP
00244                                                                   ELXPMCAP
00245  01  WS-TEST-ENTRIES.                                             ELXPMCAP
00246      05  WS-BSC-WORK-ENTRY        COMP-1          VALUE ZERO.     ELXPMCAP
00247      05  WS-BSC-SUB               PIC S9(04)      COMP.           ELXPMCAP
00248      05  WS-MM-WORK-ENTRY         COMP-1          VALUE ZERO.     ELXPMCAP
00249      05  WS-MM-SUB                PIC S9(04)      COMP.           ELXPMCAP
00250      05  WS-COUNT-BSC             PIC S9(03)    COMP-3 VALUE 0.   ELXPMCAP
00251      05  WS-COUNT-MM              PIC S9(03)    COMP-3 VALUE 0.   ELXPMCAP
00252                                                                   ELXPMCAP
00253  01  WS-SUBSCRIPTS.                                               ELXPMCAP
00254      05  WS-SPECIAL-SUB           PIC S9(04)      COMP.           ELXPMCAP
00255                                                                   ELXPMCAP
00256  01  WS-HOLD-AREA.                                                ELXPMCAP
00257      05  WS-DRVTN-VALUE           PIC X(01)       VALUE SPACE.    ELXPMCAP
00258 *                                                                 ELXPMCAP
00259 * ACCUM THRESHOLD VALUE TABLE.                                    ELXPMCAP
00260  COPY ELSCVG2C.                                                   ELXPMCAP
00261 * INTERNAL TABULAR CONFIDENCE FACTOR TABLE.                       ELXPMCAP
00262  COPY ELSCFTB5.                                                   ELXPMCAP
00263 *                                                                 ELXPMCAP
00264 *                                                                 ELXPMCAP
00265  LINKAGE SECTION.                                                 ELXPMCAP
00266  01  DFHCOMMAREA.                                                 ELXPMCAP
00267  COPY ELSCOMMC.                                                   ELXPMCAP
00268 *                                                                 ELXPMCAP
00269  01 PMCI-COMM-AREA.                                               ELXPMCAP
00270  COPY PMCCOMM.                                                    ELXPMCAP
00271 *                                                                 ELXPMCAP
00272  COPY ELSCSACC.                                                   ELXPMCAP
00273 *                                                                 ELXPMCAP
00274  COPY ELSATBLC.                                                   ELXPMCAP
00275 *                                                                 ELXPMCAP
00276  01 GROUP-SPECIFIC-RECORD.                                        ELXPMCAP
00277  COPY GCGROUPC.                                                   ELXPMCAP
00278 *                                                                 ELXPMCAP
00279 /***********************************************************      ELXPMCAP
00280 *                                                          *      ELXPMCAP
00281 *                    PROCEDURE DIVISION                    *      ELXPMCAP
00282 *                                                          *      ELXPMCAP
00283 ************************************************************      ELXPMCAP
00284                                                                   ELXPMCAP
00285  PROCEDURE DIVISION USING DFHCOMMAREA                             ELXPMCAP
00286                           PMCI-COMM-AREA                          ELXPMCAP
00287                           ATBL-ACCUMULATOR-TABLE                  ELXPMCAP
00288                           CSAC-ACCUMULATOR-TABLE                  ELXPMCAP
00289                           GROUP-SPECIFIC-RECORD.                  ELXPMCAP
00290                                                                   ELXPMCAP
00291  0000-ELXPMCAP-MAINLINE.                                          ELXPMCAP
00292      SET PMCI-BC-NO-ERROR TO TRUE.                                ELXPMCAP
00293      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAP
00294         PERFORM 0100-HOSPICE-ATCP-MAXIMUMS                        ELXPMCAP
00295         PERFORM 0200-MSA-PAR-ACCUMS                               ELXPMCAP
00296      ELSE                                                         ELXPMCAP
00297         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAP
00298         MOVE +4500 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAP
00299      END-IF                                                       ELXPMCAP
00300      GOBACK.                                                      ELXPMCAP
00301 *                                                                 ELXPMCAP
00302  0100-HOSPICE-ATCP-MAXIMUMS.                                      ELXPMCAP
00303      SET PROCESSING-MAX TO TRUE.                                  ELXPMCAP
00304      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAP
00305         TO  CSAC-ABM-GC-TBL-PTR.                                  ELXPMCAP
00306      IF HOTS-NO                                                   ELXPMCAP
00307          SET HOTS-LFT-MAX-NOT-APPL TO TRUE                        ELXPMCAP
00308      ELSE                                                         ELXPMCAP
00309          IF CSAC-ABM-GC-TBL-PTR = NULL                            ELXPMCAP
00310              SET HOTS-LFT-MAX-NO-LIMIT TO TRUE                    ELXPMCAP
00311          ELSE                                                     ELXPMCAP
00312              PERFORM 1000-SEARCH-FOR-ATCP-LFTM-MAX                ELXPMCAP
00313          END-IF                                                   ELXPMCAP
00314      END-IF.                                                      ELXPMCAP
00315                                                                   ELXPMCAP
00316      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAP
00317         IF HOSPICE-NO                                             ELXPMCAP
00318             SET HOSP-NOT-APPL TO TRUE                             ELXPMCAP
00319         ELSE                                                      ELXPMCAP
00320            IF CSAC-ABM-GC-TBL-PTR = NULL                          ELXPMCAP
00321                SET HOTS-LFT-MAX-NO-LIMIT TO TRUE                  ELXPMCAP
00322             ELSE                                                  ELXPMCAP
00323                PERFORM 2000-SEARCH-FOR-HOSP-LFTM-MAX              ELXPMCAP
00324             END-IF                                                ELXPMCAP
00325         END-IF                                                    ELXPMCAP
00326      END-IF.                                                      ELXPMCAP
00327 *                                                                 ELXPMCAP
00328  0200-MSA-PAR-ACCUMS.                                             ELXPMCAP
00329      SET PROCESSING-DED TO TRUE.                                  ELXPMCAP
00330      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAP
00331         TO  CSAC-ADL-GC-TBL-PTR.                                  ELXPMCAP
00332      IF CSAC-ADL-GC-TBL-PTR  = NULL                               ELXPMCAP
00333         PERFORM 0200-COINS-ACCUMS                                 ELXPMCAP
00334      ELSE                                                         ELXPMCAP
00335         PERFORM 0201-DO-PAR-OR-MSA.                               ELXPMCAP
00336 *                                                                 ELXPMCAP
00337  0200-COINS-ACCUMS.                                               ELXPMCAP
00338      SET MSAD-NOT-APPLICABLE TO TRUE                              ELXPMCAP
00339      SET PMCI-MSA-COINS-NONE TO TRUE                              ELXPMCAP
00340      IF PMCI-MSA-YES OR PRE-CERT-APPLIES                          ELXPMCAP
00341         PERFORM 0310-DETERMINE-MSA-PAR-COINS.                     ELXPMCAP
00342 *                                                                 ELXPMCAP
00343  0201-DO-PAR-OR-MSA.                                              ELXPMCAP
00344      IF PMCI-MSA-YES OR PRE-CERT-APPLIES                          ELXPMCAP
00345         INITIALIZE WS-BSC-SUB                                     ELXPMCAP
00346                    WS-MM-SUB                                      ELXPMCAP
00347         PERFORM 0300-SEARCH-FOR-MSA-PAR-DED                       ELXPMCAP
00348         PERFORM 0310-DETERMINE-MSA-PAR-COINS                      ELXPMCAP
00349      ELSE                                                         ELXPMCAP
00350         IF PMCI-MSA-CALL OR PRE-CALL                              ELXPMCAP
00351            SET MSAD-CALL TO TRUE                                  ELXPMCAP
00352            SET PMCI-MSA-COINS-CALL TO TRUE                        ELXPMCAP
00353      ELSE                                                         ELXPMCAP
00354         IF PMCI-MSA-NO OR PRE-CERT-DOES-NOT-APPLY                 ELXPMCAP
00355            SET MSAD-NOT-APPLICABLE TO TRUE                        ELXPMCAP
00356            SET PMCI-MSA-COINS-NOT-APPL TO TRUE.                   ELXPMCAP
00357 *                                                                 ELXPMCAP
00358  0300-SEARCH-FOR-MSA-PAR-DED.                                     ELXPMCAP
00359      MOVE ZEROS TO WS-COUNT-BSC                                   ELXPMCAP
00360      MOVE ZEROS TO WS-COUNT-MM                                    ELXPMCAP
00361      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAP
00362      PERFORM 0400-INTLZ-MSA-PAR-SP-FACTORS                        ELXPMCAP
00363          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAP
00364             UNTIL ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAP
00365      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAP
00366      MOVE RETURN-CODE TO WS-COBOL-RETURN-CODE.                    ELXPMCAP
00367      IF WS-SUCCESSFUL-CALL                                        ELXPMCAP
00368         PERFORM 2310-ELKSPCFF-CALL-SUCCESS                        ELXPMCAP
00369         IF WS-COUNT-BSC > 1 OR WS-COUNT-MM > 1                    ELXPMCAP
00370            SET MSAD-CALL TO TRUE                                  ELXPMCAP
00371         ELSE                                                      ELXPMCAP
00372            IF WS-COUNT-BSC = ZERO AND WS-COUNT-MM = ZERO          ELXPMCAP
00373               SET MSAD-NOT-APPLICABLE TO TRUE                     ELXPMCAP
00374            ELSE                                                   ELXPMCAP
00375               PERFORM 0410-DTRMN-MSA-PAR-DRVTN                    ELXPMCAP
00376               PERFORM 0420-DETERMINE-MSA-PAR-CVRG                 ELXPMCAP
00377            END-IF                                                 ELXPMCAP
00378         END-IF                                                    ELXPMCAP
00379      ELSE                                                         ELXPMCAP
00380         PERFORM 0490-SET-MSA-PAR-ERRS                             ELXPMCAP
00381      END-IF.                                                      ELXPMCAP
00382 *                                                                 ELXPMCAP
00383  0310-DETERMINE-MSA-PAR-COINS.                                    ELXPMCAP
00384      SET PMCI-MSA-COINS-NONE TO TRUE                              ELXPMCAP
00385      IF CSAC-ACL-GC-TBL-PTR  = NULL                               ELXPMCAP
00386         SET PMCI-COINS-NOT-APPL TO TRUE                           ELXPMCAP
00387      ELSE                                                         ELXPMCAP
00388         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELXPMCAP
00389                                 TO  CSAC-ACL-GC-TBL-PTR           ELXPMCAP
00390         PERFORM 0320-DETERMINE-MSA-PAR-COINS.                     ELXPMCAP
00391 *                                                                 ELXPMCAP
00392  0320-DETERMINE-MSA-PAR-COINS.                                    ELXPMCAP
00393      MOVE GCG-MED-SERV-ADV-PROG-IND TO WS-COMMON-PART-IND.        ELXPMCAP
00394      MOVE ZEROS TO WS-COUNT-BSC                                   ELXPMCAP
00395      MOVE ZEROS TO WS-COUNT-MM                                    ELXPMCAP
00396      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAP
00397      PERFORM 0400-INTLZ-MSA-PAR-SP-FACTORS                        ELXPMCAP
00398          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAP
00399             UNTIL ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAP
00400      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAP
00401      MOVE RETURN-CODE TO WS-COBOL-RETURN-CODE.                    ELXPMCAP
00402      IF WS-SUCCESSFUL-CALL                                        ELXPMCAP
00403         PERFORM 2310-ELKSPCFF-CALL-SUCCESS                        ELXPMCAP
00404         IF WS-COUNT-BSC > 1 OR WS-COUNT-MM > 1                    ELXPMCAP
00405            SET PMCI-MSA-COINS-CALL TO TRUE                        ELXPMCAP
00406         ELSE                                                      ELXPMCAP
00407            IF WS-COUNT-BSC = ZERO AND WS-COUNT-MM = ZERO          ELXPMCAP
00408               SET PMCI-MSA-COINS-NONE TO TRUE                     ELXPMCAP
00409            ELSE                                                   ELXPMCAP
00410               PERFORM 0410-DTRMN-MSA-PAR-DRVTN                    ELXPMCAP
00411               PERFORM 0450-DETERMINE-MSA-PAR-COIN                 ELXPMCAP
00412            END-IF                                                 ELXPMCAP
00413         END-IF                                                    ELXPMCAP
00414      ELSE                                                         ELXPMCAP
00415          PERFORM 0490-SET-MSA-PAR-ERRS                            ELXPMCAP
00416      END-IF.                                                      ELXPMCAP
00417 *                                                                 ELXPMCAP
00418  0400-INTLZ-MSA-PAR-SP-FACTORS.                                   ELXPMCAP
00419      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX)                             ELXPMCAP
00420              TO ATBL-CF-SP-FCTRS (ATBL-IDX).                      ELXPMCAP
00421      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00422         WS-CCP-GROUPINGS.                                         ELXPMCAP
00423      EVALUATE TRUE                                                ELXPMCAP
00424         WHEN WS-PAR-CALL-GRP OR WS-MSA-CALL-GRP                   ELXPMCAP
00425             MOVE WS-PAR-CF TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)    ELXPMCAP
00426         WHEN  WS-PAR-NON-CMPLNC-GRP OR WS-MSA-NON-CMPLNC-GRP      ELXPMCAP
00427              MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAP
00428         WHEN  OTHER                                               ELXPMCAP
00429              MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX) ELXPMCAP
00430      END-EVALUATE.                                                ELXPMCAP
00431                                                                   ELXPMCAP
00432 *                                                                 ELXPMCAP
00433  0410-DTRMN-MSA-PAR-DRVTN.                                        ELXPMCAP
00434      EVALUATE TRUE                                                ELXPMCAP
00435         WHEN WS-BSC-SUB > 0 AND                                   ELXPMCAP
00436             WS-MM-SUB > 0                                         ELXPMCAP
00437           SET ATBL-IDX TO WS-BSC-SUB                              ELXPMCAP
00438           MOVE '+' TO WS-DRVTN-VALUE                              ELXPMCAP
00439         WHEN WS-BSC-SUB > 0                                       ELXPMCAP
00440            SET ATBL-IDX TO WS-BSC-SUB                             ELXPMCAP
00441            MOVE ' ' TO WS-DRVTN-VALUE                             ELXPMCAP
00442         WHEN WS-MM-SUB > 0                                        ELXPMCAP
00443            SET ATBL-IDX TO WS-MM-SUB                              ELXPMCAP
00444            MOVE '*' TO WS-DRVTN-VALUE                             ELXPMCAP
00445      END-EVALUATE.                                                ELXPMCAP
00446 *                                                                 ELXPMCAP
00447  0420-DETERMINE-MSA-PAR-CVRG.                                     ELXPMCAP
00448      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00449         WS-CCP-GROUPINGS.                                         ELXPMCAP
00450       IF WS-MSA-NON-CMPLNC-GRP                                    ELXPMCAP
00451            OR WS-PAR-NON-CMPLNC-GRP                               ELXPMCAP
00452               PERFORM 0430-VERIFY-MSA-PAR-DED-FND                 ELXPMCAP
00453       ELSE                                                        ELXPMCAP
00454          IF  WS-MSA-CALL-GRP OR WS-PAR-CALL-GRP                   ELXPMCAP
00455              SET MSAD-CALL TO TRUE                                ELXPMCAP
00456          END-IF                                                   ELXPMCAP
00457       END-IF.                                                     ELXPMCAP
00458                                                                   ELXPMCAP
00459 *                                                                 ELXPMCAP
00460  0430-VERIFY-MSA-PAR-DED-FND.                                     ELXPMCAP
00461      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS              ELXPMCAP
00462         IF ATBL-COND-ALL-BIT (ATBL-IDX) = WS-1                    ELXPMCAP
00463            PERFORM 0440-COPY-MSA-PAR-VALUE-PMCI                   ELXPMCAP
00464            MOVE WS-DRVTN-VALUE TO PMCI-MSA-FROM-IND               ELXPMCAP
00465            IF GCG-DED-BASE-AMT-SOURCE-IND = WS-1                  ELXPMCAP
00466               SET MSAD-FROM-MEMBERSHIP TO TRUE                    ELXPMCAP
00467         END-IF                                                    ELXPMCAP
00468         ELSE                                                      ELXPMCAP
00469            SET MSAD-CALL TO TRUE                                  ELXPMCAP
00470      END-IF.                                                      ELXPMCAP
00471 *                                                                 ELXPMCAP
00472  0440-COPY-MSA-PAR-VALUE-PMCI.                                    ELXPMCAP
00473      MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO PMCI-MSA-DEDUCTIBLE.     ELXPMCAP
00474      MOVE ATBL-BENEFIT-PERIOD (ATBL-IDX)                          ELXPMCAP
00475            TO WS-BENEFIT-PERIOD-GROUPINGS.                        ELXPMCAP
00476      EVALUATE TRUE                                                ELXPMCAP
00477         WHEN  WS-VISIT-GROUP                                      ELXPMCAP
00478            SET MSAD-PER-VISIT TO TRUE                             ELXPMCAP
00479         WHEN  WS-OCCURENCE-GROUP                                  ELXPMCAP
00480            SET MSAD-PER-OCCURRENCE TO TRUE                        ELXPMCAP
00481         WHEN WS-CONFINEMENT-GROUP                                 ELXPMCAP
00482            SET MSAD-PER-CONFINEMENT TO TRUE                       ELXPMCAP
00483         WHEN WS-TREATMENT-GROUP                                   ELXPMCAP
00484           SET MSAD-PER-TREATMENT TO TRUE                          ELXPMCAP
00485         WHEN  WS-ANNUAL-GROUP                                     ELXPMCAP
00486            SET MSAD-PER-YEAR TO TRUE                              ELXPMCAP
00487      END-EVALUATE.                                                ELXPMCAP
00488 *                                                                 ELXPMCAP
00489  0450-DETERMINE-MSA-PAR-COIN.                                     ELXPMCAP
00490       MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                    ELXPMCAP
00491          WS-CCP-GROUPINGS.                                        ELXPMCAP
00492       IF WS-MSA-NON-CMPLNC-GRP                                    ELXPMCAP
00493                        OR WS-PAR-NON-CMPLNC-GRP                   ELXPMCAP
00494          PERFORM 0460-VERIFY-MSA-PAR-COINS-FND                    ELXPMCAP
00495       ELSE                                                        ELXPMCAP
00496          IF  WS-MSA-CALL-GRP OR WS-PAR-CALL-GRP                   ELXPMCAP
00497              SET PMCI-MSA-COINS-CALL TO TRUE                      ELXPMCAP
00498       ELSE                                                        ELXPMCAP
00499          SET PMCI-MSA-COINS-NOT-APPL TO TRUE.                     ELXPMCAP
00500                                                                   ELXPMCAP
00501 *                                                                 ELXPMCAP
00502  0460-VERIFY-MSA-PAR-COINS-FND.                                   ELXPMCAP
00503      IF ATBL-COND-ALL-BIT (ATBL-IDX) = WS-1                       ELXPMCAP
00504         MOVE WS-DRVTN-VALUE TO PMCI-MSA-FROM-IND                  ELXPMCAP
00505         COMPUTE PMCI-MSA-COINS-PERC-VALUE =                       ELXPMCAP
00506                 100 - ATBL-PERCENT-LEVEL (ATBL-IDX)               ELXPMCAP
00507         SET PMCI-MSA-COINS-PERCENT TO TRUE                        ELXPMCAP
00508      ELSE                                                         ELXPMCAP
00509         SET PMCI-MSA-COINS-CALL TO TRUE.                          ELXPMCAP
00510 *                                                                 ELXPMCAP
00511  0490-SET-MSA-PAR-ERRS.                                           ELXPMCAP
00512      SET PMCI-BC-INTERNAL-ERROR TO TRUE.                          ELXPMCAP
00513      EVALUATE TRUE                                                ELXPMCAP
00514         WHEN WS-UNIDENT-PARM                                      ELXPMCAP
00515           MOVE +4507 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCAP
00516         WHEN WS-MISSING-PARM                                      ELXPMCAP
00517           MOVE +4508 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCAP
00518         WHEN WS-INTERNAL-ERROR                                    ELXPMCAP
00519           MOVE +4509 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCAP
00520      END-EVALUATE.                                                ELXPMCAP
00521 *                                                                 ELXPMCAP
00522  1000-SEARCH-FOR-ATCP-LFTM-MAX.                                   ELXPMCAP
00523      INITIALIZE WS-BSC-SUB                                        ELXPMCAP
00524                 WS-MM-SUB                                         ELXPMCAP
00525                 WS-BSC-WORK-ENTRY                                 ELXPMCAP
00526                 WS-MM-WORK-ENTRY.                                 ELXPMCAP
00527      SET PROCESSING-ATCP TO TRUE.                                 ELXPMCAP
00528      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAP
00529      PERFORM 2100-INTLZ-ATCP-HOSP-SP-FCTRS                        ELXPMCAP
00530          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAP
00531             UNTIL ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAP
00532      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAP
00533      MOVE RETURN-CODE TO WS-COBOL-RETURN-CODE.                    ELXPMCAP
00534      IF WS-SUCCESSFUL-CALL                                        ELXPMCAP
00535         PERFORM 2310-ELKSPCFF-CALL-SUCCESS                        ELXPMCAP
00536         PERFORM 1204-SET-ATCP-PMCI-VALUE                          ELXPMCAP
00537      ELSE                                                         ELXPMCAP
00538         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAP
00539         EVALUATE TRUE                                             ELXPMCAP
00540           WHEN WS-UNIDENT-PARM                                    ELXPMCAP
00541              MOVE +4501 TO PMCI-BLUE-CHIP-ERROR-CODE              ELXPMCAP
00542           WHEN WS-MISSING-PARM                                    ELXPMCAP
00543              MOVE +4502 TO PMCI-BLUE-CHIP-ERROR-CODE              ELXPMCAP
00544           WHEN WS-INTERNAL-ERROR                                  ELXPMCAP
00545              MOVE +4503 TO PMCI-BLUE-CHIP-ERROR-CODE              ELXPMCAP
00546         END-EVALUATE                                              ELXPMCAP
00547      END-IF.                                                      ELXPMCAP
00548 *                                                                 ELXPMCAP
00549  1204-SET-ATCP-PMCI-VALUE.                                        ELXPMCAP
00550      EVALUATE TRUE                                                ELXPMCAP
00551         WHEN WS-BSC-SUB > 0 AND                                   ELXPMCAP
00552              WS-MM-SUB > 0                                        ELXPMCAP
00553            SET ATBL-IDX TO WS-BSC-SUB                             ELXPMCAP
00554            MOVE '+' TO WS-DRVTN-VALUE                             ELXPMCAP
00555         WHEN WS-BSC-SUB > 0                                       ELXPMCAP
00556            SET ATBL-IDX TO WS-BSC-SUB                             ELXPMCAP
00557            MOVE ' ' TO WS-DRVTN-VALUE                             ELXPMCAP
00558         WHEN WS-MM-SUB > 0                                        ELXPMCAP
00559            SET ATBL-IDX TO WS-MM-SUB                              ELXPMCAP
00560            MOVE '*' TO WS-DRVTN-VALUE                             ELXPMCAP
00561         WHEN OTHER                                                ELXPMCAP
00562            SET HOTS-LFT-MAX-NO-LIMIT TO TRUE                      ELXPMCAP
00563      END-EVALUATE.                                                ELXPMCAP
00564      IF WS-BSC-SUB > 0 OR                                         ELXPMCAP
00565              WS-MM-SUB > 0                                        ELXPMCAP
00566          IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS          ELXPMCAP
00567             PERFORM 1206-DETERMINE-ATCP-FOUND                     ELXPMCAP
00568          ELSE                                                     ELXPMCAP
00569             SET HOTS-LFT-MAX-CALL TO TRUE                         ELXPMCAP
00570          END-IF                                                   ELXPMCAP
00571      END-IF.                                                      ELXPMCAP
00572 *                                                                 ELXPMCAP
00573  1206-DETERMINE-ATCP-FOUND.                                       ELXPMCAP
00574      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00575         WS-CCP-GROUPINGS.                                         ELXPMCAP
00576      IF WS-ATCP-CMPLNC-GRP                                        ELXPMCAP
00577         PERFORM 1225-VERIFY-ATCP-MAX-FND                          ELXPMCAP
00578      ELSE                                                         ELXPMCAP
00579      IF WS-ATCP-CALL-GRP                                          ELXPMCAP
00580         SET HOTS-LFT-MAX-CALL TO TRUE.                            ELXPMCAP
00581 *                                                                 ELXPMCAP
00582  1225-VERIFY-ATCP-MAX-FND.                                        ELXPMCAP
00583      IF ATBL-COND-ALL-BIT (ATBL-IDX) = WS-1                       ELXPMCAP
00584         MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                       ELXPMCAP
00585                PMCI-HOTS-LIFETIME-MAX                             ELXPMCAP
00586         SET HOTS-LFT-MAX-DOLLARS TO TRUE                          ELXPMCAP
00587         MOVE WS-DRVTN-VALUE TO PMCI-HOTS-FROM-IND                 ELXPMCAP
00588      ELSE                                                         ELXPMCAP
00589         SET HOTS-LFT-MAX-CALL TO TRUE                             ELXPMCAP
00590      END-IF.                                                      ELXPMCAP
00591 *                                                                 ELXPMCAP
00592  2000-SEARCH-FOR-HOSP-LFTM-MAX.                                   ELXPMCAP
00593      INITIALIZE WS-BSC-SUB                                        ELXPMCAP
00594                 WS-MM-SUB                                         ELXPMCAP
00595                 WS-BSC-WORK-ENTRY                                 ELXPMCAP
00596                 WS-MM-WORK-ENTRY.                                 ELXPMCAP
00597      SET PROCESSING-HOSP TO TRUE.                                 ELXPMCAP
00598      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAP
00599      PERFORM 2100-INTLZ-ATCP-HOSP-SP-FCTRS                        ELXPMCAP
00600          VARYING ATBL-IDX FROM 1 BY 1                             ELXPMCAP
00601             UNTIL ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAP
00602      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAP
00603      MOVE RETURN-CODE TO WS-COBOL-RETURN-CODE.                    ELXPMCAP
00604      IF WS-SUCCESSFUL-CALL                                        ELXPMCAP
00605         PERFORM 2310-ELKSPCFF-CALL-SUCCESS                        ELXPMCAP
00606         PERFORM 2150-SET-HOSP-PMCI-VALUE                          ELXPMCAP
00607      ELSE                                                         ELXPMCAP
00608         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAP
00609         EVALUATE TRUE                                             ELXPMCAP
00610            WHEN WS-UNIDENT-PARM                                   ELXPMCAP
00611               MOVE +4504 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAP
00612            WHEN WS-MISSING-PARM                                   ELXPMCAP
00613               MOVE +4505 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAP
00614            WHEN WS-INTERNAL-ERROR                                 ELXPMCAP
00615               MOVE +4506 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAP
00616         END-EVALUATE                                              ELXPMCAP
00617      END-IF.                                                      ELXPMCAP
00618 *                                                                 ELXPMCAP
00619  2100-INTLZ-ATCP-HOSP-SP-FCTRS.                                   ELXPMCAP
00620      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX)                             ELXPMCAP
00621              TO ATBL-CF-SP-FCTRS (ATBL-IDX).                      ELXPMCAP
00622      IF PROCESSING-ATCP                                           ELXPMCAP
00623         PERFORM 2105-SET-ATCP-CNFDNC-FACTOR.                      ELXPMCAP
00624      IF PROCESSING-HOSP                                           ELXPMCAP
00625         PERFORM 2205-SET-HOSP-CNFDNC-FACTOR.                      ELXPMCAP
00626      PERFORM 8000-SET-INTRNL-DSCRPTRS-CF.                         ELXPMCAP
00627 *                                                                 ELXPMCAP
00628  2105-SET-ATCP-CNFDNC-FACTOR.                                     ELXPMCAP
00629      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00630         WS-CCP-GROUPINGS.                                         ELXPMCAP
00631      EVALUATE TRUE                                                ELXPMCAP
00632          WHEN  WS-ATCP-CMPLNC-GRP                                 ELXPMCAP
00633             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAP
00634          WHEN WS-ATCP-CALL-GRP                                    ELXPMCAP
00635             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAP
00636          WHEN OTHER                                               ELXPMCAP
00637             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAP
00638      END-EVALUATE.                                                ELXPMCAP
00639 *                                                                 ELXPMCAP
00640  2150-SET-HOSP-PMCI-VALUE.                                        ELXPMCAP
00641      EVALUATE TRUE                                                ELXPMCAP
00642         WHEN WS-BSC-SUB > 0 AND                                   ELXPMCAP
00643              WS-MM-SUB > 0                                        ELXPMCAP
00644            SET ATBL-IDX TO WS-BSC-SUB                             ELXPMCAP
00645            MOVE '+' TO WS-DRVTN-VALUE                             ELXPMCAP
00646         WHEN WS-BSC-SUB > 0                                       ELXPMCAP
00647            SET ATBL-IDX TO WS-BSC-SUB                             ELXPMCAP
00648            MOVE ' ' TO WS-DRVTN-VALUE                             ELXPMCAP
00649         WHEN WS-MM-SUB > 0                                        ELXPMCAP
00650            SET ATBL-IDX TO WS-MM-SUB                              ELXPMCAP
00651            MOVE '*' TO WS-DRVTN-VALUE                             ELXPMCAP
00652         WHEN OTHER                                                ELXPMCAP
00653            SET HOSP-LFT-MAX-NO-LIMIT TO TRUE                      ELXPMCAP
00654      END-EVALUATE.                                                ELXPMCAP
00655         IF WS-BSC-SUB > 0 OR WS-MM-SUB > 0                        ELXPMCAP
00656            IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS        ELXPMCAP
00657              PERFORM 2201-DETERMINE-HOSPICE-CVRG                  ELXPMCAP
00658            ELSE                                                   ELXPMCAP
00659               SET HOSP-LFT-MAX-CALL TO TRUE                       ELXPMCAP
00660            END-IF                                                 ELXPMCAP
00661         END-IF.                                                   ELXPMCAP
00662 *                                                                 ELXPMCAP
00663  2201-DETERMINE-HOSPICE-CVRG.                                     ELXPMCAP
00664      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00665         WS-CCP-GROUPINGS.                                         ELXPMCAP
00666      IF WS-HOSP-CMPLNC-GRP                                        ELXPMCAP
00667          PERFORM 2225-VERIFY-HOSP-MAX-FND                         ELXPMCAP
00668      ELSE                                                         ELXPMCAP
00669      IF WS-HOSP-CALL-GRP                                          ELXPMCAP
00670          SET HOSP-LFT-MAX-CALL TO TRUE.                           ELXPMCAP
00671 *                                                                 ELXPMCAP
00672  2205-SET-HOSP-CNFDNC-FACTOR.                                     ELXPMCAP
00673      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAP
00674         WS-CCP-GROUPINGS.                                         ELXPMCAP
00675      EVALUATE TRUE                                                ELXPMCAP
00676          WHEN WS-HOSP-CMPLNC-GRP                                  ELXPMCAP
00677             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAP
00678          WHEN WS-HOSP-CALL-GRP                                    ELXPMCAP
00679             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAP
00680          WHEN OTHER                                               ELXPMCAP
00681             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAP
00682      END-EVALUATE.                                                ELXPMCAP
00683 *                                                                 ELXPMCAP
00684  2225-VERIFY-HOSP-MAX-FND.                                        ELXPMCAP
00685      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX)                        ELXPMCAP
00686         TO WS-CCP-GROUPINGS.                                      ELXPMCAP
00687      IF ATBL-COND-ALL-BIT (ATBL-IDX) = WS-1                       ELXPMCAP
00688         MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO                       ELXPMCAP
00689                PMCI-HOSPICE-LIFETIME-MAX                          ELXPMCAP
00690         SET HOSP-LFT-MAX-LIMITED TO TRUE                          ELXPMCAP
00691         MOVE WS-DRVTN-VALUE TO PMCI-HSPC-FROM-IND                 ELXPMCAP
00692      ELSE                                                         ELXPMCAP
00693         SET HOSP-LFT-MAX-CALL TO TRUE                             ELXPMCAP
00694      END-IF.                                                      ELXPMCAP
00695 *                                                                 ELXPMCAP
00696 *                                                                 ELXPMCAP
00697  2310-ELKSPCFF-CALL-SUCCESS.                                      ELXPMCAP
00698      MOVE GCG-MED-SERV-ADV-PROG-IND TO WS-COMMON-PART-IND.        ELXPMCAP
00699      INITIALIZE WS-BSC-WORK-ENTRY                                 ELXPMCAP
00700                 WS-MM-WORK-ENTRY.                                 ELXPMCAP
00701      IF PROCESSING-MAX                                            ELXPMCAP
00702         PERFORM 2320-SETUP-MAX-RECALC                             ELXPMCAP
00703            VARYING ATBL-IDX FROM 1 BY 1                           ELXPMCAP
00704               UNTIL ATBL-IDX > ATBL-MAX-IDX                       ELXPMCAP
00705      ELSE                                                         ELXPMCAP
00706         IF PROCESSING-DED                                         ELXPMCAP
00707            PERFORM 3210-SETUP-DED-RECALC                          ELXPMCAP
00708              VARYING ATBL-IDX FROM 1 BY 1                         ELXPMCAP
00709                 UNTIL ATBL-IDX > ATBL-MAX-IDX                     ELXPMCAP
00710         END-IF                                                    ELXPMCAP
00711      END-IF.                                                      ELXPMCAP
00712 *                                                                 ELXPMCAP
00713  2320-SETUP-MAX-RECALC.                                           ELXPMCAP
00714      IF PMCI-INSTITUTIONAL                                        ELXPMCAP
00715         IF PMCI-INPATIENT                                         ELXPMCAP
00716            PERFORM 2321-RECAL-INST-IN                             ELXPMCAP
00717         ELSE                                                      ELXPMCAP
00718            PERFORM 2322-RECAL-INST-OUT                            ELXPMCAP
00719      ELSE                                                         ELXPMCAP
00720         IF PMCI-INPATIENT                                         ELXPMCAP
00721            PERFORM 2323-RECAL-PROF-IN                             ELXPMCAP
00722         ELSE                                                      ELXPMCAP
00723            PERFORM 2324-RECAL-PROF-OUT.                           ELXPMCAP
00724  2321-RECAL-INST-IN.                                              ELXPMCAP
00725            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00726               PERFORM 2330-RECAL-INST-IP-BSC-MAX.                 ELXPMCAP
00727            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00728               PERFORM 5330-RECAL-INST-IP-MM-MAX.                  ELXPMCAP
00729  2322-RECAL-INST-OUT.                                             ELXPMCAP
00730            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00731               PERFORM 2360-RECAL-INST-OP-BSC-MAX.                 ELXPMCAP
00732            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00733               PERFORM 5360-RECAL-INST-OP-MM-MAX.                  ELXPMCAP
00734  2323-RECAL-PROF-IN.                                              ELXPMCAP
00735            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00736               PERFORM 2380-RECAL-PROF-IP-BSC-MAX.                 ELXPMCAP
00737            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00738               PERFORM 5380-RECAL-PROF-IP-MM-MAX.                  ELXPMCAP
00739  2324-RECAL-PROF-OUT.                                             ELXPMCAP
00740            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00741               PERFORM 2400-RECAL-PROF-OP-BSC-MAX.                 ELXPMCAP
00742            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00743               PERFORM 5400-RECAL-PROF-OP-MM-MAX.                  ELXPMCAP
00744 *                                                                 ELXPMCAP
00745  2330-RECAL-INST-IP-BSC-MAX.                                      ELXPMCAP
00746      CALL 'ELKFLAND' USING                                        ELXPMCAP
00747                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00748                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
00749                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00750                     ATBL-CF-INST-BAS (ATBL-IDX)                   ELXPMCAP
00751                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
00752                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00753                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00754      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
00755         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00756         CONTINUE                                                  ELXPMCAP
00757      ELSE                                                         ELXPMCAP
00758         PERFORM 2340-PRPR-INST-IP-BSC-CMBN.                       ELXPMCAP
00759      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
00760         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00761         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00762          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00763          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00764          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00765        ELSE                                                       ELXPMCAP
00766           MOVE ATBL-PLACE-OF-TREATMENT(ATBL-IDX)                  ELXPMCAP
00767                           TO WS-TEST-POT                          ELXPMCAP
00768           IF ATBL-COST-CONTAIN-IND(ATBL-IDX) = '84'               ELXPMCAP
00769             IF ( INST-INPATIENT AND                               ELXPMCAP
00770             (WS-COMMON-INST-BSC OR WS-COMMON-INST-BOTH))          ELXPMCAP
00771 *           AND  GSS-MS-BC-IND (GSS-INDEX) NOT EQUAL '00'         ELXPMCAP
00772            SET WS-BSC-SUB TO ATBL-IDX                             ELXPMCAP
00773            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRYELXPMCAP
00774        END-IF.                                                    ELXPMCAP
00775 *                                                                 ELXPMCAP
00776  2340-PRPR-INST-IP-BSC-CMBN.                                      ELXPMCAP
00777      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
00778      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00779          WS-WT-INDVDL.                                            ELXPMCAP
00780      COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *              ELXPMCAP
00781          WS-WT-INST-BAS.                                          ELXPMCAP
00782      COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
00783      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00784                                                                   ELXPMCAP
00785 *    CHANGED CF-OV TO CF-SP, SO IT MATCHES CALL ARGUMENTS.RGO.    ELXPMCAP
00786      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00787      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
00788 *                                                                 ELXPMCAP
00789  2360-RECAL-INST-OP-BSC-MAX.                                      ELXPMCAP
00790      CALL 'ELKFLAND' USING                                        ELXPMCAP
00791                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00792                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
00793                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00794                     ATBL-CF-INST-BAS (ATBL-IDX)                   ELXPMCAP
00795                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
00796                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00797                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00798      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
00799         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00800         CONTINUE                                                  ELXPMCAP
00801      ELSE                                                         ELXPMCAP
00802         PERFORM 2370-PRPR-INST-OP-CMBN.                           ELXPMCAP
00803      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
00804         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00805         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00806          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00807          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00808          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00809      END-IF.                                                      ELXPMCAP
00810 *                                                                 ELXPMCAP
00811  2370-PRPR-INST-OP-CMBN.                                          ELXPMCAP
00812      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
00813      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00814          WS-WT-INDVDL.                                            ELXPMCAP
00815      COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *              ELXPMCAP
00816          WS-WT-INST-BAS.                                          ELXPMCAP
00817      COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
00818      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00819      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00820      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
00821 *                                                                 ELXPMCAP
00822  2380-RECAL-PROF-IP-BSC-MAX.                                      ELXPMCAP
00823      CALL 'ELKFLAND' USING                                        ELXPMCAP
00824                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00825                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
00826                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00827                     ATBL-CF-PROF-BAS (ATBL-IDX)                   ELXPMCAP
00828                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
00829                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00830                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00831      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
00832         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00833         CONTINUE                                                  ELXPMCAP
00834      ELSE                                                         ELXPMCAP
00835         PERFORM 2390-PRPR-PROF-IP-BSC-CMBN.                       ELXPMCAP
00836      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
00837         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00838         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00839          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00840          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00841        ELSE                                                       ELXPMCAP
00842           MOVE ATBL-PLACE-OF-TREATMENT(ATBL-IDX)                  ELXPMCAP
00843                           TO WS-TEST-POT                          ELXPMCAP
00844           IF ATBL-COST-CONTAIN-IND(ATBL-IDX) = '84'               ELXPMCAP
00845             IF ( PROF-INPATIENT AND                               ELXPMCAP
00846             (WS-COMMON-PROF-BSC OR WS-COMMON-PROF-BOTH))          ELXPMCAP
00847 *           AND  GSS-MS-BC-IND (GSS-INDEX) NOT EQUAL '00'         ELXPMCAP
00848            SET WS-BSC-SUB TO ATBL-IDX                             ELXPMCAP
00849            MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRYELXPMCAP
00850        END-IF                                                     ELXPMCAP
00851      END-IF.                                                      ELXPMCAP
00852 *                                                                 ELXPMCAP
00853  2390-PRPR-PROF-IP-BSC-CMBN.                                      ELXPMCAP
00854      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
00855      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00856          WS-WT-INDVDL.                                            ELXPMCAP
00857      COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *              ELXPMCAP
00858          WS-WT-PROF-BAS.                                          ELXPMCAP
00859      COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
00860      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00861      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00862      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
00863 *                                                                 ELXPMCAP
00864  2400-RECAL-PROF-OP-BSC-MAX.                                      ELXPMCAP
00865      CALL 'ELKFLAND' USING                                        ELXPMCAP
00866                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00867                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
00868                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00869                     ATBL-CF-PROF-BAS (ATBL-IDX)                   ELXPMCAP
00870                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
00871                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00872                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00873      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
00874         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00875         CONTINUE                                                  ELXPMCAP
00876      ELSE                                                         ELXPMCAP
00877         PERFORM 2410-PRPR-PROF-OP-BSC-CMBN.                       ELXPMCAP
00878      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
00879         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00880         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00881          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00882          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00883          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00884      END-IF.                                                      ELXPMCAP
00885 *                                                                 ELXPMCAP
00886  2410-PRPR-PROF-OP-BSC-CMBN.                                      ELXPMCAP
00887      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
00888      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00889          WS-WT-INDVDL.                                            ELXPMCAP
00890      COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *              ELXPMCAP
00891          WS-WT-PROF-BAS.                                          ELXPMCAP
00892      COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
00893      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00894      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00895      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
00896 *                                                                 ELXPMCAP
00897 *                                                                 ELXPMCAP
00898  3210-SETUP-DED-RECALC.                                           ELXPMCAP
00899      IF PMCI-INSTITUTIONAL                                        ELXPMCAP
00900         IF PMCI-INPATIENT                                         ELXPMCAP
00901            PERFORM 3211-RECAL-INST-IN                             ELXPMCAP
00902         ELSE                                                      ELXPMCAP
00903            PERFORM 3212-RECAL-INST-OUT                            ELXPMCAP
00904      ELSE                                                         ELXPMCAP
00905         IF PMCI-INPATIENT                                         ELXPMCAP
00906            PERFORM 3213-RECAL-PROF-IN                             ELXPMCAP
00907         ELSE                                                      ELXPMCAP
00908            PERFORM 3214-RECAL-PROF-OUT.                           ELXPMCAP
00909  3211-RECAL-INST-IN.                                              ELXPMCAP
00910            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00911               PERFORM 3220-RECAL-INST-IP-BSC-DED.                 ELXPMCAP
00912            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00913               PERFORM 6220-RECAL-INST-IP-MM-DED.                  ELXPMCAP
00914  3212-RECAL-INST-OUT.                                             ELXPMCAP
00915            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00916               PERFORM 3240-RECAL-INST-OP-BSC-DED.                 ELXPMCAP
00917            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00918               PERFORM 6240-RECAL-INST-OP-MM-DED.                  ELXPMCAP
00919  3213-RECAL-PROF-IN.                                              ELXPMCAP
00920            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00921               PERFORM 3230-RECAL-PROF-IP-BSC-DED.                 ELXPMCAP
00922            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00923               PERFORM 6230-RECAL-PROF-IP-MM-DED.                  ELXPMCAP
00924  3214-RECAL-PROF-OUT.                                             ELXPMCAP
00925            IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES                ELXPMCAP
00926               PERFORM 3250-RECAL-PROF-OP-BSC-DED.                 ELXPMCAP
00927            IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCAP
00928               PERFORM 6250-RECAL-PROF-OP-MM-DED.                  ELXPMCAP
00929 *                                                                 ELXPMCAP
00930  3220-RECAL-INST-IP-BSC-DED.                                      ELXPMCAP
00931      CALL 'ELKFLAND' USING                                        ELXPMCAP
00932                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00933                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00934                     ATBL-CF-INST-BAS (ATBL-IDX)                   ELXPMCAP
00935                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
00936                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00937                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00938      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
00939         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00940         CONTINUE                                                  ELXPMCAP
00941      ELSE                                                         ELXPMCAP
00942         PERFORM 3225-PRPR-INST-IP-BSC-CMBN.                       ELXPMCAP
00943      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
00944         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00945         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00946          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00947          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00948          ADD 1 TO WS-COUNT-BSC.                                   ELXPMCAP
00949 *                                                                 ELXPMCAP
00950  3225-PRPR-INST-IP-BSC-CMBN.                                      ELXPMCAP
00951      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00952          WS-WT-INDVDL.                                            ELXPMCAP
00953      COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
00954          WS-WT-INST-SUP.                                          ELXPMCAP
00955      COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
00956      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00957      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00958      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
00959                           WS-CF-1                                 ELXPMCAP
00960                           WS-CF-2                                 ELXPMCAP
00961                           WS-CF-3                                 ELXPMCAP
00962                           WS-CF-4                                 ELXPMCAP
00963                           WS-CF-5.                                ELXPMCAP
00964 *                                                                 ELXPMCAP
00965  3230-RECAL-PROF-IP-BSC-DED.                                      ELXPMCAP
00966      CALL 'ELKFLAND' USING                                        ELXPMCAP
00967                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
00968                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
00969                     ATBL-CF-PROF-BAS (ATBL-IDX)                   ELXPMCAP
00970                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
00971                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
00972                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
00973      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE   OR           ELXPMCAP
00974         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
00975         CONTINUE                                                  ELXPMCAP
00976      ELSE                                                         ELXPMCAP
00977         PERFORM 3235-PRPR-PROF-IP-BSC-CMBN                        ELXPMCAP
00978      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
00979         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
00980         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
00981          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
00982          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
00983          ADD 1 TO WS-COUNT-BSC.                                   ELXPMCAP
00984 *                                                                 ELXPMCAP
00985  3235-PRPR-PROF-IP-BSC-CMBN.                                      ELXPMCAP
00986      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
00987          WS-WT-INDVDL.                                            ELXPMCAP
00988      COMPUTE WS-CF-2 = ATBL-CF-PROF-BAS (ATBL-IDX) *              ELXPMCAP
00989          WS-WT-PROF-BAS.                                          ELXPMCAP
00990      COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
00991      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
00992      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
00993      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
00994                           WS-CF-1                                 ELXPMCAP
00995                           WS-CF-2                                 ELXPMCAP
00996                           WS-CF-3                                 ELXPMCAP
00997                           WS-CF-4                                 ELXPMCAP
00998                           WS-CF-5.                                ELXPMCAP
00999 *                                                                 ELXPMCAP
01000  3240-RECAL-INST-OP-BSC-DED.                                      ELXPMCAP
01001      CALL 'ELKFLAND' USING                                        ELXPMCAP
01002                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01003                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01004                     ATBL-CF-INST-BAS (ATBL-IDX)                   ELXPMCAP
01005                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01006                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01007                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01008      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01009         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01010         CONTINUE                                                  ELXPMCAP
01011      ELSE                                                         ELXPMCAP
01012         PERFORM 3245-PRPR-INST-OP-BSC-CMBN.                       ELXPMCAP
01013      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01014         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
01015         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01016          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
01017          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
01018          ADD 1 TO WS-COUNT-BSC.                                   ELXPMCAP
01019 *                                                                 ELXPMCAP
01020  3245-PRPR-INST-OP-BSC-CMBN.                                      ELXPMCAP
01021      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01022          WS-WT-INDVDL.                                            ELXPMCAP
01023      COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
01024          WS-WT-INST-SUP.                                          ELXPMCAP
01025      COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01026      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01027      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01028      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01029                           WS-CF-1                                 ELXPMCAP
01030                           WS-CF-2                                 ELXPMCAP
01031                           WS-CF-3                                 ELXPMCAP
01032                           WS-CF-4                                 ELXPMCAP
01033                           WS-CF-5.                                ELXPMCAP
01034 *                                                                 ELXPMCAP
01035  3250-RECAL-PROF-OP-BSC-DED.                                      ELXPMCAP
01036      CALL 'ELKFLAND' USING                                        ELXPMCAP
01037                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01038                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01039                     ATBL-CF-PROF-BAS (ATBL-IDX)                   ELXPMCAP
01040                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01041                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01042                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01043      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE   OR           ELXPMCAP
01044         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01045         CONTINUE                                                  ELXPMCAP
01046      ELSE                                                         ELXPMCAP
01047         PERFORM 3255-PRPR-PROF-OP-BSC-CMBN                        ELXPMCAP
01048      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01049         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BSC-WORK-ENTRY    ELXPMCAP
01050         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01051          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BSC-WORK-ENTRY  ELXPMCAP
01052          SET WS-BSC-SUB TO ATBL-IDX                               ELXPMCAP
01053          ADD 1 TO WS-COUNT-BSC.                                   ELXPMCAP
01054 *                                                                 ELXPMCAP
01055  3255-PRPR-PROF-OP-BSC-CMBN.                                      ELXPMCAP
01056      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01057          WS-WT-INDVDL.                                            ELXPMCAP
01058      COMPUTE WS-CF-2 = ATBL-CF-PROF-BAS (ATBL-IDX) *              ELXPMCAP
01059          WS-WT-PROF-BAS.                                          ELXPMCAP
01060      COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01061      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01062      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01063      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01064                           WS-CF-1                                 ELXPMCAP
01065                           WS-CF-2                                 ELXPMCAP
01066                           WS-CF-3                                 ELXPMCAP
01067                           WS-CF-4                                 ELXPMCAP
01068                           WS-CF-5.                                ELXPMCAP
01069                                                                   ELXPMCAP
01070                                                                   ELXPMCAP
01071  5330-RECAL-INST-IP-MM-MAX.                                       ELXPMCAP
01072      CALL 'ELKFLAND' USING                                        ELXPMCAP
01073                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01074                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
01075                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01076                     ATBL-CF-INST-SUP (ATBL-IDX)                   ELXPMCAP
01077                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
01078                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01079                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01080      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01081         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01082         CONTINUE                                                  ELXPMCAP
01083      ELSE                                                         ELXPMCAP
01084         PERFORM 5340-PRPR-INST-IP-MM-CMBN.                        ELXPMCAP
01085      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
01086         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01087         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01088          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01089          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01090      END-IF.                                                      ELXPMCAP
01091 *                                                                 ELXPMCAP
01092  5340-PRPR-INST-IP-MM-CMBN.                                       ELXPMCAP
01093      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
01094      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01095          WS-WT-INDVDL.                                            ELXPMCAP
01096      COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
01097          WS-WT-INST-SUP.                                          ELXPMCAP
01098      COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
01099      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01100      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01101      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
01102 *                                                                 ELXPMCAP
01103  5360-RECAL-INST-OP-MM-MAX.                                       ELXPMCAP
01104      CALL 'ELKFLAND' USING                                        ELXPMCAP
01105                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01106                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
01107                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01108                     ATBL-CF-INST-SUP (ATBL-IDX)                   ELXPMCAP
01109                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01110                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01111                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01112      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01113         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01114         CONTINUE                                                  ELXPMCAP
01115      ELSE                                                         ELXPMCAP
01116         PERFORM 5370-PRPR-INST-OP-SUPP-CMBN.                      ELXPMCAP
01117      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
01118         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01119         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01120          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01121          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01122      END-IF.                                                      ELXPMCAP
01123 *                                                                 ELXPMCAP
01124  5370-PRPR-INST-OP-SUPP-CMBN.                                     ELXPMCAP
01125      IF PROCESSING-MSA OR PROCESSING-PAR                          ELXPMCAP
01126          COMPUTE WS-CF-1 = ATBL-CF-ANL (ATBL-IDX) *               ELXPMCAP
01127              WS-WT-ANL                                            ELXPMCAP
01128      ELSE                                                         ELXPMCAP
01129          COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *              ELXPMCAP
01130             WS-WT-LFTM                                            ELXPMCAP
01131      END-IF.                                                      ELXPMCAP
01132      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01133          WS-WT-INDVDL.                                            ELXPMCAP
01134      COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
01135          WS-WT-INST-SUP.                                          ELXPMCAP
01136      COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01137      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01138      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01139      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
01140 *                                                                 ELXPMCAP
01141  5380-RECAL-PROF-IP-MM-MAX.                                       ELXPMCAP
01142      CALL 'ELKFLAND' USING                                        ELXPMCAP
01143                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01144                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
01145                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01146                     ATBL-CF-PROF-SUP (ATBL-IDX)                   ELXPMCAP
01147                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
01148                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01149                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01150      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01151         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01152         CONTINUE                                                  ELXPMCAP
01153      ELSE                                                         ELXPMCAP
01154         PERFORM 5390-PRPR-PROF-IP-MM-CMBN.                        ELXPMCAP
01155      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
01156         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01157         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01158          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01159          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01160      END-IF.                                                      ELXPMCAP
01161 *                                                                 ELXPMCAP
01162  5390-PRPR-PROF-IP-MM-CMBN.                                       ELXPMCAP
01163      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
01164      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01165          WS-WT-INDVDL.                                            ELXPMCAP
01166      COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *              ELXPMCAP
01167          WS-WT-PROF-SUP.                                          ELXPMCAP
01168      COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
01169      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01170      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01171      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
01172                                                                   ELXPMCAP
01173 *                                                                 ELXPMCAP
01174  5400-RECAL-PROF-OP-MM-MAX.                                       ELXPMCAP
01175      CALL 'ELKFLAND' USING                                        ELXPMCAP
01176                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01177                     ATBL-CF-LFTM (ATBL-IDX)                       ELXPMCAP
01178                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01179                     ATBL-CF-PROF-SUP (ATBL-IDX)                   ELXPMCAP
01180                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01181                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01182                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01183      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01184         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01185         CONTINUE                                                  ELXPMCAP
01186      ELSE                                                         ELXPMCAP
01187         PERFORM 5410-PRPR-PROF-OP-MM-CMBN.                        ELXPMCAP
01188      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ABM      ELXPMCAP
01189         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01190         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01191          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01192          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01193      END-IF.                                                      ELXPMCAP
01194 *                                                                 ELXPMCAP
01195  5410-PRPR-PROF-OP-MM-CMBN.                                       ELXPMCAP
01196      MOVE WS-CF-ZERO TO WS-CF-1.                                  ELXPMCAP
01197      COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01198          WS-WT-INDVDL.                                            ELXPMCAP
01199      COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *              ELXPMCAP
01200          WS-WT-PROF-SUP.                                          ELXPMCAP
01201      COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01202      COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01203      COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01204      PERFORM 9999-CALL-ELKFLCMB.                                  ELXPMCAP
01205 *                                                                 ELXPMCAP
01206  6220-RECAL-INST-IP-MM-DED.                                       ELXPMCAP
01207      CALL 'ELKFLAND' USING                                        ELXPMCAP
01208                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01209                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01210                     ATBL-CF-INST-SUP (ATBL-IDX)                   ELXPMCAP
01211                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
01212                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01213                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01214      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01215         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01216         CONTINUE                                                  ELXPMCAP
01217      ELSE                                                         ELXPMCAP
01218         PERFORM 6225-PRPR-INST-IP-MM-CMBN.                        ELXPMCAP
01219      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01220         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01221         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01222          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01223          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01224          ADD 1 TO WS-COUNT-MM                                     ELXPMCAP
01225      END-IF.                                                      ELXPMCAP
01226 *                                                                 ELXPMCAP
01227  6225-PRPR-INST-IP-MM-CMBN.                                       ELXPMCAP
01228      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01229          WS-WT-INDVDL.                                            ELXPMCAP
01230      COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
01231          WS-WT-INST-SUP.                                          ELXPMCAP
01232      COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
01233      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01234      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01235      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01236                           WS-CF-1                                 ELXPMCAP
01237                           WS-CF-2                                 ELXPMCAP
01238                           WS-CF-3                                 ELXPMCAP
01239                           WS-CF-4                                 ELXPMCAP
01240                           WS-CF-5.                                ELXPMCAP
01241 *                                                                 ELXPMCAP
01242  6230-RECAL-PROF-IP-MM-DED.                                       ELXPMCAP
01243      CALL 'ELKFLAND' USING                                        ELXPMCAP
01244                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01245                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01246                     ATBL-CF-PROF-SUP (ATBL-IDX)                   ELXPMCAP
01247                     ATBL-CF-IP (ATBL-IDX)                         ELXPMCAP
01248                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01249                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01250      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE   OR           ELXPMCAP
01251         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01252         CONTINUE                                                  ELXPMCAP
01253      ELSE                                                         ELXPMCAP
01254         PERFORM 6235-PRPR-PROF-IP-MM-CMBN.                        ELXPMCAP
01255      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01256         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01257         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01258          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01259          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01260          ADD 1 TO WS-COUNT-MM                                     ELXPMCAP
01261      END-IF.                                                      ELXPMCAP
01262 *                                                                 ELXPMCAP
01263  6235-PRPR-PROF-IP-MM-CMBN.                                       ELXPMCAP
01264      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01265          WS-WT-INDVDL.                                            ELXPMCAP
01266      COMPUTE WS-CF-2 = ATBL-CF-PROF-SUP (ATBL-IDX) *              ELXPMCAP
01267          WS-WT-PROF-SUP.                                          ELXPMCAP
01268      COMPUTE WS-CF-3 = ATBL-CF-IP (ATBL-IDX) * WS-WT-IP.          ELXPMCAP
01269      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01270      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01271      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01272                           WS-CF-1                                 ELXPMCAP
01273                           WS-CF-2                                 ELXPMCAP
01274                           WS-CF-3                                 ELXPMCAP
01275                           WS-CF-4                                 ELXPMCAP
01276                           WS-CF-5.                                ELXPMCAP
01277 *                                                                 ELXPMCAP
01278  6240-RECAL-INST-OP-MM-DED.                                       ELXPMCAP
01279      CALL 'ELKFLAND' USING                                        ELXPMCAP
01280                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01281                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01282                     ATBL-CF-INST-SUP (ATBL-IDX)                   ELXPMCAP
01283                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01284                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01285                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01286      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE OR             ELXPMCAP
01287         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01288         CONTINUE                                                  ELXPMCAP
01289      ELSE                                                         ELXPMCAP
01290         PERFORM 6245-PRPR-INST-OP-MM-CMBN.                        ELXPMCAP
01291      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01292         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01293         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01294          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01295          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01296          ADD 1 TO WS-COUNT-MM                                     ELXPMCAP
01297      END-IF.                                                      ELXPMCAP
01298 *                                                                 ELXPMCAP
01299  6245-PRPR-INST-OP-MM-CMBN.                                       ELXPMCAP
01300      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01301          WS-WT-INDVDL.                                            ELXPMCAP
01302      COMPUTE WS-CF-2 = ATBL-CF-INST-SUP (ATBL-IDX) *              ELXPMCAP
01303          WS-WT-INST-SUP.                                          ELXPMCAP
01304      COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01305      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01306      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01307      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01308                           WS-CF-1                                 ELXPMCAP
01309                           WS-CF-2                                 ELXPMCAP
01310                           WS-CF-3                                 ELXPMCAP
01311                           WS-CF-4                                 ELXPMCAP
01312                           WS-CF-5.                                ELXPMCAP
01313 *                                                                 ELXPMCAP
01314  6250-RECAL-PROF-OP-MM-DED.                                       ELXPMCAP
01315      CALL 'ELKFLAND' USING                                        ELXPMCAP
01316                     ATBL-CF-WORK-ENTRY (ATBL-IDX)                 ELXPMCAP
01317                     ATBL-CF-INDVDL (ATBL-IDX)                     ELXPMCAP
01318                     ATBL-CF-PROF-SUP (ATBL-IDX)                   ELXPMCAP
01319                     ATBL-CF-OP (ATBL-IDX)                         ELXPMCAP
01320                     ATBL-CF-PLAN (ATBL-IDX)                       ELXPMCAP
01321                     ATBL-CF-SP (ATBL-IDX).                        ELXPMCAP
01322      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) = WS-CF-TRUE   OR           ELXPMCAP
01323         ATBL-CF-WORK-ENTRY (ATBL-IDX) < ZERO                      ELXPMCAP
01324         CONTINUE                                                  ELXPMCAP
01325      ELSE                                                         ELXPMCAP
01326         PERFORM 6255-PRPR-PROF-OP-MM-CMBN.                        ELXPMCAP
01327      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >= CVG2-OV-THRSHLD-ADL      ELXPMCAP
01328         AND (ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-WORK-ENTRY     ELXPMCAP
01329         AND ATBL-VALUE-QUALIFIER (ATBL-IDX) = WS-DOLLARS)         ELXPMCAP
01330          MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-WORK-ENTRY   ELXPMCAP
01331          SET WS-MM-SUB TO ATBL-IDX                                ELXPMCAP
01332          ADD 1 TO WS-COUNT-MM                                     ELXPMCAP
01333      END-IF.                                                      ELXPMCAP
01334 *                                                                 ELXPMCAP
01335  6255-PRPR-PROF-OP-MM-CMBN.                                       ELXPMCAP
01336      COMPUTE WS-CF-1 = ATBL-CF-INDVDL (ATBL-IDX) *                ELXPMCAP
01337          WS-WT-INDVDL.                                            ELXPMCAP
01338      COMPUTE WS-CF-2 = ATBL-CF-PROF-SUP (ATBL-IDX) *              ELXPMCAP
01339          WS-WT-PROF-SUP.                                          ELXPMCAP
01340      COMPUTE WS-CF-3 = ATBL-CF-OP (ATBL-IDX) * WS-WT-OP.          ELXPMCAP
01341      COMPUTE WS-CF-4 = ATBL-CF-PLAN (ATBL-IDX) * WS-WT-PLAN.      ELXPMCAP
01342      COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) * WS-WT-SP.          ELXPMCAP
01343      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01344                           WS-CF-1                                 ELXPMCAP
01345                           WS-CF-2                                 ELXPMCAP
01346                           WS-CF-3                                 ELXPMCAP
01347                           WS-CF-4                                 ELXPMCAP
01348                           WS-CF-5.                                ELXPMCAP
01349 *                                                                 ELXPMCAP
01350  8000-SET-INTRNL-DSCRPTRS-CF.                                     ELXPMCAP
01351      SET INTRNL-DSCRPTR-NOT-FND TO TRUE.                          ELXPMCAP
01352      SET CFT5-MAX-IDX TO CFT5-NBR-ENTRS.                          ELXPMCAP
01353      PERFORM VARYING CFT5-IDX FROM 1 BY 1                         ELXPMCAP
01354         UNTIL CFT5-IDX > CFT5-MAX-IDX                             ELXPMCAP
01355           OR INTRNL-DSCRPTR-FND                                   ELXPMCAP
01356         IF ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX) =                  ELXPMCAP
01357              CFT5-INTD (CFT5-IDX)                                 ELXPMCAP
01358            PERFORM 8001-DTRMN-HOSP-ATCP-INTRNL                    ELXPMCAP
01359            SET INTRNL-DSCRPTR-FND TO TRUE                         ELXPMCAP
01360         END-IF                                                    ELXPMCAP
01361      END-PERFORM.                                                 ELXPMCAP
01362 *                                                                 ELXPMCAP
01363  8001-DTRMN-HOSP-ATCP-INTRNL.                                     ELXPMCAP
01364      IF PROCESSING-ATCP                                           ELXPMCAP
01365         MOVE CFT5-CF-INTD-ATCP (CFT5-IDX)                         ELXPMCAP
01366                 TO ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)           ELXPMCAP
01367      ELSE                                                         ELXPMCAP
01368         IF PROCESSING-HOSP                                        ELXPMCAP
01369            MOVE CFT5-CF-INTD-HSPC (CFT5-IDX)                      ELXPMCAP
01370                 TO ATBL-CF-SP-INTRNL-DSCRPTR (ATBL-IDX)           ELXPMCAP
01371         END-IF                                                    ELXPMCAP
01372      END-IF.                                                      ELXPMCAP
01373 *                                                                 ELXPMCAP
01374                                                                   ELXPMCAP
01375                                                                   ELXPMCAP
01376  9999-CALL-ELKFLCMB.                                              ELXPMCAP
01377      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAP
01378                           WS-CF-1                                 ELXPMCAP
01379                           WS-CF-2                                 ELXPMCAP
01380                           WS-CF-3                                 ELXPMCAP
01381                           WS-CF-4                                 ELXPMCAP
01382                           WS-CF-5                                 ELXPMCAP
01383                           WS-CF-6.                                ELXPMCAP
