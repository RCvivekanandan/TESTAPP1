00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCGC
00003  PROGRAM-ID.         ELXPMCGC.                                       LV004
00004                                                                   ELXPMCGC
00005  AUTHOR.             BARBARA KEIB                                 ELXPMCGC
00006                                                                   ELXPMCGC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCGC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCGC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCGC
00010                      233 N. MICHIGAN AVE                          ELXPMCGC
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCGC
00012                                                                   ELXPMCGC
00013  DATE-WRITTEN.       27-JUL-1992.                                 ELXPMCGC
00014                                                                   ELXPMCGC
00015  DATE-COMPILED.                                                   ELXPMCGC
00016                                                                   ELXPMCGC
00017  SECURITY.           COPYRIGHT 1992,                              ELXPMCGC
00018                      HEALTH CARE SERVICE CORPORATION              ELXPMCGC
00019      SKIP3                                                        ELXPMCGC
00020  ENVIRONMENT DIVISION.                                            ELXPMCGC
00021                                                                   ELXPMCGC
00022                                                                   ELXPMCGC
00023  CONFIGURATION SECTION.                                           ELXPMCGC
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELXPMCGC
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELXPMCGC
00026      EJECT                                                        ELXPMCGC
00027 ******************************************************************ELXPMCGC
00028 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCGC
00029 *      THIS MODULE FINDS THE GROUP SPECIFIC AND CONTRACT         *ELXPMCGC
00030 *      RECORDS NEEDED TO SATISFY THE ELIGIBILITY SUMMARY         *ELXPMCGC
00031 *      INTERFACE REQUEST.                                        *ELXPMCGC
00032 *                                                                *ELXPMCGC
00033 ******************************************************************ELXPMCGC
00034 *                      MAINTENANCE HISTORY                       *ELXPMCGC
00035 *                                                                *ELXPMCGC
00036 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCGC
00037 * ----- ----------- --- ----- ---------------------------------- *ELXPMCGC
00038 * 01.00 27-JUL-1992 BAK       CREATED                            *ELXPMCGC
00039 * 01.01 11-FEB-1993 BAK       ADDED CODE TO HANDLE DATE OF SERV. *ELXPMCGC
00040 *                             BEING USED AS DATE TO SELECT       *ELXPMCGC
00041 *                             FILES INSTEAD OF CURRENT SYSTEM    *ELXPMCGC
00042 *                             DATE--DATE SUPPLIED IN PMCI RECORD.*ELXPMCGC
00043 * 02.00 26-MAY-1993 BAK       ADDED MAJOR MEDICAL CONTRACT SUPRT.*ELXPMCGC
00044 *                             ISSR #13071 - PHASE 1              *ELXPMCGC
00045 * 02.01 28-JUN-1993 BAK       CORRECT FAMILY RELATIONSHIP COMPARE*ELXPMCGC
00046 * 02.02 04-AUG-1993 BAK       MOVE SELECTED DATE TO TABLULAR READ*ELXPMCGC
00047 *                             IN NAES AREA FOR ISR #13184        *ELXPMCGC
00048 * 02.03 17-SEP-1993 BAK       CHANGE TABULAR INDICATOR AREAS TO  *ELXPMCGC
00049 *                             TO DIGITS FOR PROVIDER CONTROL     *ELXPMCGC
00050 * 02.04 02-NOV-1993 BAK       ADD TEST TO FAMILY RELATIONSHIP TBL*ELXPMCGC
00051 *                             IF 22-26 THEN THIS IS MEDICARE CONT*ELXPMCGC
00052 * 03.00 09-JAN-1995 RGO     --SUPPLEMENTAL MEDICARE PROJECT.     *ELXPMCGC
00053 *                             ON THE CONTRACT RECORD, IF THE     *ELXPMCGC
00054 *                             MCARE-TYPE-IND IS FOR SUPPLEMENTAL *ELXPMCGC
00055 *                             MEDICARE AND THE LOB=1, THEN GET   *ELXPMCGC
00056 *                             THE ABM# SLOT NUMBER.              *ELXPMCGC
00057 *                           --FIX ASRA CAUSED BY WRONG PERIOD IN *ELXPMCGC
00058 *                             3000-, AND RESET RETURN CODE WHEN  *ELXPMCGC
00059 *                             LOOKING FOR MAJOR MEDICAL CONTRACT.*ELXPMCGC
00060 *                                                                *ELXPMCGC
00061 * APRIL 11, 1995 RGO   - AFTER GETTING THE TABULAR RECORD, CHECK *ELXPMCGC
00062 *                        NAES-TABS-RETN-CODE, NOT PMCI-BLUE-CHIP-*ELXPMCGC
00063 *                        RETURN CODE. IN 8000- PARAGRAPH.        *ELXPMCGC
00064 *                                                                *ELXPMCGC
00065 * JULY 14, 1995 RGO - IF IT IS A SUPPLEMENTAL MEDICARE PRODUCT,  *ELXPMCGC
00066 *                     CHECK THE GROUP SPECIFIC RECORD TO SEE     *ELXPMCGC
00067 *                     IF THEY HAVE BCBS DEDUCTIBLE.              *ELXPMCGC
00068 *                                                                *ELXPMCGC
00069 * NOVEMBER 11, 1997  AKK ADDED SUPPORT FOR YR 2000 AND TX        *ELXPMCGC
00070 *                        MERGER.                                 *ELXPMCGC
00071 *                                                                *ELXPMCGC
00072 * AUGUST 06, 1998    AKK ADDED ADDITION SUPPORT FOR YR 2000      *ELXPMCGC
00073 *                                                                *ELXPMCGC
00074 * NOVEMBER 04, 1998  AKK ADDING TEMP CODE TO SUPPORT ACP         *ELXPMCGC
00075 *                        TABULAR TO SET CALL ON WHEN FOUND       *ELXPMCGC
00076 *                        AT EITHER GROUP OR CONTRACT.  THIS CODE *ELXPMCGC
00077 *                        WILL UTIMATELY BE REMOVED.              *ELXPMCGC
00078 *                                                                *ELXPMCGC
00079 * APRIL 01, 2003     AKK MORE CHANGES DUE TO ENDEVOR.            *ELXPMCGC
00080 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCGC
00081 *                                                                *ELXPMCGC
00082 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCGC
00083 *                                                                *ELXPMCGC
00084 * 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCGC
00085 *                                                                *ELXPMCGC
00086 *                                                                *ELXPMCGC
00087 ******************************************************************ELXPMCGC
00088                                                                   ELXPMCGC
00089      EJECT                                                        ELXPMCGC
00090  DATA DIVISION.                                                   ELXPMCGC
00091  WORKING-STORAGE SECTION.                                         ELXPMCGC
00092  01  WS-HDG                     PICTURE X(32)                     ELXPMCGC
00093           VALUE '****ELXPMCGC WORKING STORAGE****'.               ELXPMCGC
00094                                                                   ELXPMCGC
00095  01  SUB1              COMP-3   PIC S9(3).                        ELXPMCGC
00096                                                                   ELXPMCGC
00097  01  WS-DATES-KEY.                                                ELXPMCGC
00098      05  WS-DAT-PLAN            PIC X(03).                        ELXPMCGC
00099      05  WS-DAT-NBR             PIC X(09).                        ELXPMCGC
00100      05  WS-DAT-SEC             PIC X(05).                        ELXPMCGC
00101      05  WS-DAT-PKG             PIC X(03).                        ELXPMCGC
00102      05  WS-DAT-LOB             PIC X(01).                        ELXPMCGC
00103      05  WS-DAT-FIL             PIC X(10).                        ELXPMCGC
00104      05  WS-DAT-IND             PIC X(01).                        ELXPMCGC
00105                                                                   ELXPMCGC
00106  01  WS-GROUP-KEY.                                                ELXPMCGC
00107      05  WS-GRP-PLAN-CODE       PIC X(03).                        ELXPMCGC
00108      05  WS-GRP-NBR             PIC X(09).                        ELXPMCGC
00109      05  WS-GRP-SEC             PIC X(05).                        ELXPMCGC
00110      05  WS-GRP-PKG-CODE        PIC X(03).                        ELXPMCGC
00111      05  WS-GRP-FRL             PIC X(02).                        ELXPMCGC
00112      05  WS-GRP-EFF             PIC S9(07) COMP-3.                ELXPMCGC
00113                                                                   ELXPMCGC
00114 *01  WS-CONTRACT-KEY-BSC       PIC X(29) VALUE SPACES.            ELXPMCGC
00115  01  WS-CONTRACT-KEY-BSC       PIC X(18) VALUE SPACES.            ELXPMCGC
00116                                                                   ELXPMCGC
00117  01  WS-CONTRACT-KEY-MAJ       PIC X(29) VALUE SPACES.            ELXPMCGC
00118                                                                   ELXPMCGC
00119  01  WS-KEY-AREA.                                                 ELXPMCGC
00120      05  WS-SAVE-DATES-KEY.                                       ELXPMCGC
00121          10  WS-SAV-EFF         PIC S9(07) COMP-3.                ELXPMCGC
00122          10  WS-SAV-PRV         PIC X(02).                        ELXPMCGC
00123          10  WS-SAV-FRL         PIC X(02).                        ELXPMCGC
00124          10  WS-SAV-TRM         PIC S9(07) COMP-3.                ELXPMCGC
00125                                                                   ELXPMCGC
00126  01  WS-SAVE-AREAS.                                               ELXPMCGC
00127      05  WS-NON-MEDICARE-SAVE   PIC X(12) VALUE SPACES.           ELXPMCGC
00128      05  WS-MEDICARE-SAVE       PIC X(12) VALUE SPACES.           ELXPMCGC
00129      05  WS-HOLD-DATES-G        PIC X(12) VALUE SPACES.           ELXPMCGC
00130      05  WS-HOLD-DATES-C        PIC X(10) VALUE SPACES.           ELXPMCGC
00131      05  WS-HOLD-MED-IND        PIC X(01) VALUE SPACES.           ELXPMCGC
00132                                                                   ELXPMCGC
00133                                                                   ELXPMCGC
00134  01  WS-KEY-STUFF.                                                ELXPMCGC
00135      05  SAV-PRV-TAB-KEY.                                         ELXPMCGC
00136          10  SAV-PRV         PIC X(02) VALUE SPACES.              ELXPMCGC
00137          10  SAV-TYP         PIC X(01) VALUE SPACES.              ELXPMCGC
00138      05  WS-SAV-LOB          PIC X(01) VALUE SPACES.              ELXPMCGC
00139      05  WS-BSC-LOB          PIC X(01) VALUE SPACES.              ELXPMCGC
00140      05  WS-MAJ-MED-LOB      PIC X(01) VALUE SPACES.              ELXPMCGC
00141      05  SAV-FRL             PIC X(02) VALUE SPACES.              ELXPMCGC
00142                                                                   ELXPMCGC
00143  01  WS-SAVE-TABULAR-INDICATORS.                                  ELXPMCGC
00144      05  WS-TAB-TYPE                PIC X(01) VALUE SPACES.       ELXPMCGC
00145      05  WS-TAB-KEY.                                              ELXPMCGC
00146          10  WS-TAB-GRP-PRV         PIC X(02) VALUE SPACES.       ELXPMCGC
00147          10  WS-TAB-CON-PRV         PIC X(02) VALUE SPACES.       ELXPMCGC
00148      05  WS-TAB-INDICATORS.                                       ELXPMCGC
00149          10  WS-TAB-IND1            PIC X(02) VALUE SPACES.       ELXPMCGC
00150          10  WS-TAB-IND2            PIC X(02) VALUE SPACES.       ELXPMCGC
00151      05  WS-TAB-INST-ATTRIBUTES.                                  ELXPMCGC
00152          10  WS-TAB-INST-PLAN       PIC X(01) VALUE SPACES.       ELXPMCGC
00153          10  WS-TAB-INST-EMPL       PIC X(01) VALUE SPACES.       ELXPMCGC
00154          10  FILLER                 PIC X(02) VALUE SPACES.       ELXPMCGC
00155      05  WS-TAB-PROF-ATTRIBUTES REDEFINES                         ELXPMCGC
00156                          WS-TAB-INST-ATTRIBUTES.                  ELXPMCGC
00157          10  WS-TAB-PROF-MPP        PIC X(01).                    ELXPMCGC
00158          10  WS-TAB-PROF-EMPL       PIC X(01).                    ELXPMCGC
00159          10  WS-TAB-PROF-PHAR       PIC X(01).                    ELXPMCGC
00160          10  WS-TAB-PROF-VISN       PIC X(01).                    ELXPMCGC
00161                                                                   ELXPMCGC
00162  01  WS-PROVIDER-TABLE-INDICATORS.                                ELXPMCGC
00163      05  FILLER                     PIC X(01) VALUE SPACES.       ELXPMCGC
00164      05  WS-PTBL-KEY.                                             ELXPMCGC
00165          10  WS-PTBL-GRP-PRV        PIC X(02) VALUE SPACES.       ELXPMCGC
00166          10  WS-PTBL-CON-PRV        PIC X(02) VALUE SPACES.       ELXPMCGC
00167      05  WS-PTBL-INDICATORS.                                      ELXPMCGC
00168          10  WS-PTBL-IND1           PIC X(02) VALUE SPACES.       ELXPMCGC
00169          10  WS-PTBL-IND2           PIC X(02) VALUE SPACES.       ELXPMCGC
00170      05  WS-PTBL-INST-ATTRIBUTES.                                 ELXPMCGC
00171          10  WS-PTBL-INST-PLAN      PIC X(01) VALUE SPACES.       ELXPMCGC
00172          10  WS-PTBL-INST-EMPL      PIC X(01) VALUE SPACES.       ELXPMCGC
00173          10  FILLER                 PIC X(02) VALUE SPACES.       ELXPMCGC
00174      05  WS-PTBL-PROF-ATTRIBUTES REDEFINES                        ELXPMCGC
00175                          WS-PTBL-INST-ATTRIBUTES.                 ELXPMCGC
00176          10  WS-PTBL-PROF-MPP      PIC X(01).                     ELXPMCGC
00177          10  WS-PTBL-PROF-EMPL     PIC X(01).                     ELXPMCGC
00178          10  WS-PTBL-PROF-PHAR     PIC X(01).                     ELXPMCGC
00179          10  WS-PTBL-PROF-VISN     PIC X(01).                     ELXPMCGC
00180                                                                   ELXPMCGC
00181  01  WS-TAB-READ-INDICATORS.                                      ELXPMCGC
00182      05  WS-TAB-RIND1           PIC X(01) VALUE SPACES.           ELXPMCGC
00183      05  WS-TAB-RIND2           PIC X(01) VALUE SPACES.           ELXPMCGC
00184      05  WS-TAB-RIND3           PIC X(01) VALUE SPACES.           ELXPMCGC
00185                                                                   ELXPMCGC
00186  01  WS-COUNTS.                                                   ELXPMCGC
00187      05  WS-TOT-COUNT           PIC S9(05) COMP-3 VALUE ZEROS.    ELXPMCGC
00188      05  WS-MEDICARE-COUNT      PIC S9(05) COMP-3 VALUE ZEROS.    ELXPMCGC
00189      05  WS-NON-MEDICARE-COUNT  PIC S9(05) COMP-3 VALUE ZEROS.    ELXPMCGC
00190      05  WS-DTE-MAX-IDX         PIC S9(04) COMP VALUE ZEROS.      ELXPMCGC
00191      05  WS-EMP-GRP-IDX-MAX     PIC S9(04) COMP VALUE ZEROS.      ELXPMCGC
00192      05  WS-EMP-NUM-IDX-MAX     PIC S9(04) COMP VALUE ZEROS.      ELXPMCGC
00193                                                                   ELXPMCGC
00194  01  WS-GROUP-AREA.                                               ELXPMCGC
00195      05  FILLER                 PIC X(03) VALUE ZEROES.           ELXPMCGC
00196      05  WS-GRP-X               PIC X(06).                        ELXPMCGC
00197                                                                   ELXPMCGC
00198  01  WS-MISC-SWITCHES.                                            ELXPMCGC
00199                                                                   ELXPMCGC
00200      05  WS-LINE-OF-BUSINESS       PIC X  VALUE 'N'.              ELXPMCGC
00201          88  LOB-FOUND                    VALUE 'Y'.              ELXPMCGC
00202          88  LOB-NOT-FOUND                VALUE 'N'.              ELXPMCGC
00203                                                                   ELXPMCGC
00204      05  WS-PROCESS-CONTROL        PIC X  VALUE 'N'.              ELXPMCGC
00205          88  PROCESSING-BASIC             VALUE 'B'.              ELXPMCGC
00206          88  PROCESSING-MAJOR-MED         VALUE 'M'.              ELXPMCGC
00207                                                                   ELXPMCGC
00208      05  WS-FAMILY-SEARCH-CONTROL    PIC X  VALUE 'N'.            ELXPMCGC
00209          88  FAMILY-SEARCH-OK             VALUE 'Y'.              ELXPMCGC
00210          88  FAMILY-SEARCH-REJECT         VALUE 'N'.              ELXPMCGC
00211                                                                   ELXPMCGC
00212      05  WS-FAMILY-LOOP-CONTROL     PIC X VALUE 'N'.              ELXPMCGC
00213          88  FAMILY-LOOP-STOP             VALUE 'Y'.              ELXPMCGC
00214          88  FAMILY-LOOP-NULL             VALUE 'N'.              ELXPMCGC
00215                                                                   ELXPMCGC
00216      05  WS-EMPLOYER                PIC X VALUE '0'.              ELXPMCGC
00217          88  EMPLOYER                     VALUE '1'.              ELXPMCGC
00218          88  NOT-EMPLOYER                 VALUE '0'.              ELXPMCGC
00219                                                                   ELXPMCGC
00220      05  WS-EMPLOYER-SEARCH-CONTROL PIC X VALUE 'N'.              ELXPMCGC
00221          88  EMPLOYER-SEARCH-STOP         VALUE 'Y'.              ELXPMCGC
00222          88  EMPLOYER-SEARCH-NULL         VALUE 'N'.              ELXPMCGC
00223                                                                   ELXPMCGC
00224      05  WS-TABULAR-CONTROL         PIC X VALUE 'N'.              ELXPMCGC
00225          88  TABS-NEEDED                  VALUE 'Y'.              ELXPMCGC
00226          88  TABS-NOT-NEEDED              VALUE 'N'.              ELXPMCGC
00227                                                                   ELXPMCGC
00228      05  WS-TABULAR-FOUND           PIC X VALUE 'N'.              ELXPMCGC
00229          88  TABS-FOUND                   VALUE 'Y'.              ELXPMCGC
00230          88  TABS-NOT-FOUND               VALUE 'N'.              ELXPMCGC
00231                                                                   ELXPMCGC
00232  01  WS-MISC-SWITCHES-2.                                          ELXPMCGC
00233                                                                   ELXPMCGC
00234      05  WS-PROVIDER-STATUS         PIC X VALUE 'N'.              ELXPMCGC
00235          88  PROVIDER-FOUND               VALUE 'Y'.              ELXPMCGC
00236          88  PROVIDER-NOT-FOUND           VALUE 'N'.              ELXPMCGC
00237                                                                   ELXPMCGC
00238      05  WS-PROVIDER-TABLE-CONTROL  PIC X VALUE 'N'.              ELXPMCGC
00239          88  PROVIDER-STOP                VALUE 'Y'.              ELXPMCGC
00240          88  PROVIDER-NULL                VALUE 'N'.              ELXPMCGC
00241                                                                   ELXPMCGC
00242      05  WS-INST-CONTROL            PIC X VALUE 'N'.              ELXPMCGC
00243          88  PRV-INST-STOP                VALUE 'Y'.              ELXPMCGC
00244          88  PRV-INST-NULL                VALUE 'N'.              ELXPMCGC
00245                                                                   ELXPMCGC
00246      05  WS-PROF-CONTROL            PIC X VALUE 'N'.              ELXPMCGC
00247          88  PRV-PROF-STOP                VALUE 'Y'.              ELXPMCGC
00248          88  PRV-PROF-NULL                VALUE 'N'.              ELXPMCGC
00249                                                                   ELXPMCGC
00250      05  WS-PROVIDER-CONTROL        PIC X VALUE 'N'.              ELXPMCGC
00251          88  PROVIDER-OK                  VALUE 'Y'.              ELXPMCGC
00252          88  PROVIDER-REJECT              VALUE 'N'.              ELXPMCGC
00253                                                                   ELXPMCGC
00254      05  WS-MCARE-IND         PIC X(2).                           ELXPMCGC
00255      88  SUPP-PRODUCT     VALUE '0T','04','05','06','07','0M'.    ELXPMCGC
00256      88  SUPP-DED-COV     VALUE '0T','06'.                        ELXPMCGC
00257      88  SUPP-DED-NOT-COV VALUE '04','05','0M'.                   ELXPMCGC
00258      88  SUPP-DED-CALL    VALUE '07'.                             ELXPMCGC
00259                                                                   ELXPMCGC
00260  01  WS-CURRENT.                                                  ELXPMCGC
00261      05  CURRENT-DATE.                                            ELXPMCGC
00262          10  CURRENT-DAY-CC  PIC X.                               ELXPMCGC
00263          10  CURRENT-DAY     PIC S9(05) COMP-3.                   ELXPMCGC
00264      05  CURRENT-DAY-CEN REDEFINES CURRENT-DATE                   ELXPMCGC
00265                              PIC S9(07) COMP-3.                   ELXPMCGC
00266                                                                   ELXPMCGC
00267  01  WS-DATE-WORK.                                                ELXPMCGC
00268      05  WS-WORK-CC            PIC 9(02).                         ELXPMCGC
00269      05  WS-WORK-YY            PIC 9(02).                         ELXPMCGC
00270      05  WS-WORK-MM            PIC 9(02).                         ELXPMCGC
00271      05  WS-WORK-DD            PIC 9(02).                         ELXPMCGC
00272                                                                   ELXPMCGC
00273  01  WS-SWITCHES.                                                 ELXPMCGC
00274      05                             PIC X(01).                    ELXPMCGC
00275          88  SW-TRMNL-ERR                     VALUE 'Y'.          ELXPMCGC
00276          88  SW-NO-TRMNL-ERR                  VALUE 'N'.          ELXPMCGC
00277                                                                   ELXPMCGC
00278  01  WS-NULL-PTR                POINTER VALUE NULL.               ELXPMCGC
00279                                                                   ELXPMCGC
00280                                                                   ELXPMCGC
00281  01  FILLER                     PICTURE X(32)                     ELXPMCGC
00282           VALUE '*END ELXPMCGC WORKING STORAGE***'.               ELXPMCGC
00283 *    EJECT                                                        ELXPMCGC
00284  COPY ELXLOBTC.                                                   ELXPMCGC
00285 *    EJECT                                                        ELXPMCGC
00286  COPY ELXFRLTC .                                                  ELXPMCGC
00287 *    EJECT                                                        ELXPMCGC
00288  COPY ELXTABTC .                                                  ELXPMCGC
00289 *    EJECT                                                        ELXPMCGC
00290  COPY ELXPVITC .                                                  ELXPMCGC
00291 *    EJECT                                                        ELXPMCGC
00292  COPY ELXPVPTC .                                                  ELXPMCGC
00293 *    EJECT                                                        ELXPMCGC
00294  COPY ELXPMITC .                                                  ELXPMCGC
00295 *    EJECT                                                        ELXPMCGC
00296  COPY ELXPMPTC .                                                  ELXPMCGC
00297 *    EJECT                                                        ELXPMCGC
00298  01  WS-HOSP-EMPL-GROUP-TABLE.                                    ELXPMCGC
00299  COPY HGCDEMPL .                                                  ELXPMCGC
00300 *    EJECT                                                        ELXPMCGC
00301  COPY CSDATES .                                                   ELXPMCGC
00302 *    EJECT                                                        ELXPMCGC
00303 *    EJECT                                                        ELXPMCGC
00304  COPY MLDATE01.                                                   ELXPMCGC
00305 *    EJECT                                                        ELXPMCGC
00306  COPY CSDATES .                                                   ELXPMCGC
00307 *    EJECT                                                        ELXPMCGC
00308  LINKAGE SECTION.                                                 ELXPMCGC
00309  01  DFHCOMMAREA.                                                 ELXPMCGC
00310  COPY ELSCOMMC .                                                  ELXPMCGC
00311 *    EJECT                                                        ELXPMCGC
00312  COPY ELSCIA2C .                                                  ELXPMCGC
00313 *    EJECT                                                        ELXPMCGC
00314  COPY ELSIOPMC .                                                  ELXPMCGC
00315 *    EJECT                                                        ELXPMCGC
00316  COPY ELSKEYSC .                                                  ELXPMCGC
00317 *    EJECT                                                        ELXPMCGC
00318  01  PMCI-COMM-AREA.                                              ELXPMCGC
00319  COPY PMCCOMM .                                                   ELXPMCGC
00320 *    EJECT                                                        ELXPMCGC
00321  COPY ELSPMCID .                                                  ELXPMCGC
00322 *    EJECT                                                        ELXPMCGC
00323  01  DTE-DATE-RECORD.                                             ELXPMCGC
00324  COPY GCDATESC .                                                  ELXPMCGC
00325 *    EJECT                                                        ELXPMCGC
00326  01  GCG-GCGRPSPC-RECORD.                                         ELXPMCGC
00327  COPY GCGROUPC .                                                  ELXPMCGC
00328 *    EJECT                                                        ELXPMCGC
00329  01  GCT-GCCONTRC-RECORD.                                         ELXPMCGC
00330  COPY GCCONTRC .                                                  ELXPMCGC
00331      EJECT                                                        ELXPMCGC
00332  PROCEDURE DIVISION.                                              ELXPMCGC
00333 ************************************************************      ELXPMCGC
00334 *                                                          *      ELXPMCGC
00335 *        ELIGIBILITY SUMMARY GRP-CONTRACT                  *      ELXPMCGC
00336 *                                                          *      ELXPMCGC
00337 ************************************************************      ELXPMCGC
00338  0000-ELIGIBILITY-SUM-GRP-CON.                                    ELXPMCGC
00339                                                                   ELXPMCGC
00340      SET SW-NO-TRMNL-ERR TO TRUE.                                 ELXPMCGC
00341      IF ECA-CIA-PTR = NULL OR                                     ELXPMCGC
00342                  EIBCALEN < LENGTH OF DFHCOMMAREA                 ELXPMCGC
00343         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCGC
00344      ELSE                                                         ELXPMCGC
00345         PERFORM 0100-INITIALIZATION.                              ELXPMCGC
00346      IF SW-NO-TRMNL-ERR                                           ELXPMCGC
00347         PERFORM 1000-PROCESS-GRP-CONTRACT-RETR.                   ELXPMCGC
00348      GOBACK.                                                      ELXPMCGC
00349                                                                   ELXPMCGC
00350 ************************************************************      ELXPMCGC
00351 *                                                          *      ELXPMCGC
00352 *        INITIALIZATION                                    *      ELXPMCGC
00353 *                                                          *      ELXPMCGC
00354 ************************************************************      ELXPMCGC
00355  0100-INITIALIZATION.                                             ELXPMCGC
00356                                                                   ELXPMCGC
00357      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCGC
00358              ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.            ELXPMCGC
00359      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCGC
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGC
00361              ADDRESS OF PMCI-COMM-AREA.                           ELXPMCGC
00362      IF CIA-RC-PTR-NULL                                           ELXPMCGC
00363          MOVE 17 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
00364          SET PMCI-BC-INTERNAL-ERROR TO TRUE                       ELXPMCGC
00365          CONTINUE                                                 ELXPMCGC
00366      ELSE                                                         ELXPMCGC
00367          SET CIA-GCDATES-DDN TO TRUE                              ELXPMCGC
00368          PERFORM 9000-CALL-STORAGE-MANAGER                        ELXPMCGC
00369          SET CIA-GCDATES-DDN TO TRUE                              ELXPMCGC
00370          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELXPMCGC
00371                ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS             ELXPMCGC
00372          IF CIA-RC-PTR-NULL                                       ELXPMCGC
00373              SET SW-TRMNL-ERR TO TRUE                             ELXPMCGC
00374              MOVE 18 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCGC
00375              SET PMCI-BC-INTERNAL-ERROR TO TRUE                   ELXPMCGC
00376              CONTINUE                                             ELXPMCGC
00377          ELSE                                                     ELXPMCGC
00378              SET CIA-GCGRPSPC-DDN TO TRUE                         ELXPMCGC
00379              PERFORM 9000-CALL-STORAGE-MANAGER                    ELXPMCGC
00380              SET CIA-GCGRPSPC-DDN TO TRUE                         ELXPMCGC
00381              CALL 'ELUSETAD' USING DFHCOMMAREA                    ELXPMCGC
00382                   ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS          ELXPMCGC
00383              IF CIA-RC-PTR-NULL                                   ELXPMCGC
00384                  SET SW-TRMNL-ERR TO TRUE                         ELXPMCGC
00385                  MOVE 19 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCGC
00386                  SET PMCI-BC-INTERNAL-ERROR TO TRUE               ELXPMCGC
00387                  CONTINUE                                         ELXPMCGC
00388              ELSE                                                 ELXPMCGC
00389                  SET CIA-GCCONTR-DDN TO TRUE                      ELXPMCGC
00390                  PERFORM 9000-CALL-STORAGE-MANAGER                ELXPMCGC
00391                  SET CIA-GCCONTR-DDN TO TRUE                      ELXPMCGC
00392                  CALL 'ELUSETAD' USING DFHCOMMAREA                ELXPMCGC
00393                       ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS      ELXPMCGC
00394                  IF CIA-RC-PTR-NULL                               ELXPMCGC
00395                      SET SW-TRMNL-ERR TO TRUE                     ELXPMCGC
00396                      MOVE 20 TO PMCI-BLUE-CHIP-ERROR-CODE         ELXPMCGC
00397                      SET PMCI-BC-INTERNAL-ERROR TO TRUE           ELXPMCGC
00398                      CONTINUE                                     ELXPMCGC
00399                  ELSE                                             ELXPMCGC
00400                      SET CIA-ELSKEYS-DDN TO TRUE                  ELXPMCGC
00401                      PERFORM 9000-CALL-STORAGE-MANAGER            ELXPMCGC
00402                      SET CIA-ELSKEYS-DDN TO TRUE                  ELXPMCGC
00403                      CALL 'ELUSETAD' USING DFHCOMMAREA            ELXPMCGC
00404                            ADDRESS OF KWA-FILE-KEY-WORK-AREA      ELXPMCGC
00405                      IF CIA-RC-PTR-NULL                           ELXPMCGC
00406                          SET SW-TRMNL-ERR TO TRUE                 ELXPMCGC
00407                          MOVE 21 TO PMCI-BLUE-CHIP-ERROR-CODE     ELXPMCGC
00408                          SET PMCI-BC-INTERNAL-ERROR TO TRUE       ELXPMCGC
00409                          CONTINUE                                 ELXPMCGC
00410                      ELSE                                         ELXPMCGC
00411                          SET CIA-ELSPMCID-DDN TO TRUE             ELXPMCGC
00412                          CALL 'ELUSETAD' USING DFHCOMMAREA        ELXPMCGC
00413                               ADDRESS OF NAES-INTERMEDIATE-DATA   ELXPMCGC
00414                          IF CIA-RC-PTR-NULL                       ELXPMCGC
00415                              SET SW-TRMNL-ERR TO TRUE             ELXPMCGC
00416                              MOVE 22 TO PMCI-BLUE-CHIP-ERROR-CODE ELXPMCGC
00417                              SET PMCI-BC-INTERNAL-ERROR TO TRUE   ELXPMCGC
00418                              CONTINUE                             ELXPMCGC
00419                          ELSE                                     ELXPMCGC
00420                              PERFORM 0200-GET-REQUEST-DATE        ELXPMCGC
00421                          END-IF                                   ELXPMCGC
00422                      END-IF                                       ELXPMCGC
00423                  END-IF                                           ELXPMCGC
00424              END-IF                                               ELXPMCGC
00425          END-IF                                                   ELXPMCGC
00426      END-IF.                                                      ELXPMCGC
00427 ************************************************************      ELXPMCGC
00428 *                                                          *      ELXPMCGC
00429 *   READ PMCI SERVICE DATE TO USE FOR FILE RETRIEVAL KEYS  *      ELXPMCGC
00430 *                                                          *      ELXPMCGC
00431 ************************************************************      ELXPMCGC
00432  0200-GET-REQUEST-DATE.                                           ELXPMCGC
00433                                                                   ELXPMCGC
00434      MOVE PMCI-DOS-CC TO WS-WORK-CC.                              ELXPMCGC
00435      MOVE PMCI-DOS-YY TO WS-WORK-YY.                              ELXPMCGC
00436      MOVE PMCI-DOS-MM TO WS-WORK-MM.                              ELXPMCGC
00437      MOVE PMCI-DOS-DD TO WS-WORK-DD.                              ELXPMCGC
00438      MOVE WS-DATE-WORK TO MLDATE-DATE1.                           ELXPMCGC
00439      MOVE 'CNV' TO MLDATE-FUNC.                                   ELXPMCGC
00440      MOVE 'Y'   TO MLDATE-FORM1.                                  ELXPMCGC
00441      MOVE 'J'   TO MLDATE-FORM2.                                  ELXPMCGC
00442      MOVE ZEROS TO MLDATE-RETURN, MLDATE-AMOUNT,                  ELXPMCGC
00443                        MLDATE-DATE2.                              ELXPMCGC
00444                                                                   ELXPMCGC
00445      EXEC CICS LINK PROGRAM('MLDATEC')                            ELXPMCGC
00446                      COMMAREA(MLDATE01)                           ELXPMCGC
00447                        LENGTH (28)                                ELXPMCGC
00448                          NOHANDLE                                 ELXPMCGC
00449                          END-EXEC.                                ELXPMCGC
00450                                                                   ELXPMCGC
00451      IF MLDATE-RETURN NOT = '00'                                  ELXPMCGC
00452         MOVE 13 TO PMCI-BLUE-CHIP-ERROR-CODE                      ELXPMCGC
00453         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCGC
00454      ELSE                                                         ELXPMCGC
00455 *       MOVE MLDATE-JULIAN2 TO CURRENT-DAY-CEN                    ELXPMCGC
00456         MOVE MLDATE-JUL2 TO CURRENT-DAY-CEN                       ELXPMCGC
00457         MOVE ZEROS TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCGC
00458         SET PMCI-BC-SUCCESSFUL TO TRUE.                           ELXPMCGC
00459                                                                   ELXPMCGC
00460 ************************************************************      ELXPMCGC
00461 *                                                          *      ELXPMCGC
00462 *    PROCESS GROUP SPECIFIC AND CONTRACT RECORD RETRIEVAL  *      ELXPMCGC
00463 *                                                          *      ELXPMCGC
00464 ************************************************************      ELXPMCGC
00465  1000-PROCESS-GRP-CONTRACT-RETR.                                  ELXPMCGC
00466                                                                   ELXPMCGC
00467      IF PMCI-BC-SUCCESSFUL                                        ELXPMCGC
00468         SET FML-MAX-INDX TO FML-ENTRY-CNT                         ELXPMCGC
00469         PERFORM 1100-OBTAIN-GROUP-SPEC-RECORD                     ELXPMCGC
00470         IF PMCI-BC-SUCCESSFUL                                     ELXPMCGC
00471            PERFORM 2100-DETERMINE-IF-EMPLOYER                     ELXPMCGC
00472            PERFORM 2600-DETERMINE-LINE-OF-BUS                     ELXPMCGC
00473            IF PMCI-BC-SUCCESSFUL                                  ELXPMCGC
00474               PERFORM 2000-OBTAIN-CONTRACT-RECORD.                ELXPMCGC
00475                                                                   ELXPMCGC
00476 ************************************************************      ELXPMCGC
00477 *                                                          *      ELXPMCGC
00478 *        OBTAIN GROUP SPECIFIC RECORD                      *      ELXPMCGC
00479 *                                                          *      ELXPMCGC
00480 ************************************************************      ELXPMCGC
00481  1100-OBTAIN-GROUP-SPEC-RECORD.                                   ELXPMCGC
00482                                                                   ELXPMCGC
00483      MOVE 000 TO KWA-GCDATES-PLAN-CODE.                           ELXPMCGC
00484 *    MOVE PMCI-GROUP-NBR TO WS-GROUP-AREA.                        ELXPMCGC
00485 *    MOVE WS-GROUP-AREA TO KWA-GCDATES-GROUP-NO.                  ELXPMCGC
00486      MOVE PMCI-GROUP-NBR TO KWA-GCDATES-GROUP-NUMBER.             ELXPMCGC
00487 *MAY CHANGE HERE                                                  ELXPMCGC
00488      MOVE PMCI-SECT-NUM   TO KWA-GCDATES-SECTION-NUMBER.          ELXPMCGC
00489      MOVE PMCI-PACKAGE-CODE TO KWA-GCDATES-PKG-CODE.              ELXPMCGC
00490      MOVE LOW-VALUES TO KWA-GCDATES-L-O-B.                        ELXPMCGC
00491      MOVE LOW-VALUES TO KWA-GCDATES-FILLER.                       ELXPMCGC
00492      MOVE 'G' TO KWA-GCDATES-FILE-REF.                            ELXPMCGC
00493      MOVE KWA-GCDATES-KEY TO WS-DATES-KEY.                        ELXPMCGC
00494      PERFORM 7000-READ-DATES-FILE.                                ELXPMCGC
00495      IF PMCI-BC-SUCCESSFUL                                        ELXPMCGC
00496          SET DTE-INDEX TO DTE-ENTRY-COUNT                         ELXPMCGC
00497          SET WS-DTE-MAX-IDX TO DTE-INDEX                          ELXPMCGC
00498          PERFORM 1200-SEARCH-GRP-SPEC-DATES-KEY                   ELXPMCGC
00499             VARYING DTE-INDEX FROM 1 BY 1                         ELXPMCGC
00500               UNTIL DTE-INDEX = WS-DTE-MAX-IDX                    ELXPMCGC
00501          PERFORM 6000-SELECT-CORRECT-DATE-KEY .                   ELXPMCGC
00502          IF PMCI-BC-SUCCESSFUL                                    ELXPMCGC
00503             PERFORM 7100-READ-GROUP-SPEC-FILE.                    ELXPMCGC
00504                                                                   ELXPMCGC
00505 ************************************************************      ELXPMCGC
00506 *                                                          *      ELXPMCGC
00507 *        SEARCH GROUP SPECIFIC DATES KEY                    *     ELXPMCGC
00508 *                                                          *      ELXPMCGC
00509 ************************************************************      ELXPMCGC
00510  1200-SEARCH-GRP-SPEC-DATES-KEY.                                  ELXPMCGC
00511                                                                   ELXPMCGC
00512      IF DTE-EFFDT-CEN (DTE-INDEX) <= CURRENT-DAY-CEN AND          ELXPMCGC
00513         DTE-TERMDT-CEN (DTE-INDEX) >= CURRENT-DAY-CEN             ELXPMCGC
00514         MOVE DTE-EFF-TERM (DTE-INDEX) TO WS-HOLD-DATES-G          ELXPMCGC
00515          PERFORM 5000-QUAL-MEDICARE-ELIGIBILITY.                  ELXPMCGC
00516                                                                   ELXPMCGC
00517 ************************************************************      ELXPMCGC
00518 *                                                          *      ELXPMCGC
00519 *        OBTAIN CONTRACT RECORD                            *      ELXPMCGC
00520 *                                                          *      ELXPMCGC
00521 ************************************************************      ELXPMCGC
00522  2000-OBTAIN-CONTRACT-RECORD.                                     ELXPMCGC
00523                                                                   ELXPMCGC
00524      IF WS-BSC-LOB NOT EQUAL 'R'                                  ELXPMCGC
00525         INITIALIZE WS-SAVE-AREAS,                                 ELXPMCGC
00526                    WS-CONTRACT-KEY-BSC,                           ELXPMCGC
00527                    WS-SAVE-TABULAR-INDICATORS,                    ELXPMCGC
00528                    WS-PROVIDER-TABLE-INDICATORS,                  ELXPMCGC
00529                    WS-COUNTS,                                     ELXPMCGC
00530                    WS-MISC-SWITCHES-2                             ELXPMCGC
00531         SET PROCESSING-BASIC TO TRUE                              ELXPMCGC
00532         MOVE WS-BSC-LOB TO WS-SAV-LOB                             ELXPMCGC
00533         PERFORM 3000-DERIVE-CONTRACT-KEY.                         ELXPMCGC
00534                                                                   ELXPMCGC
00535      IF WS-MAJ-MED-LOB NOT EQUAL 'R'                              ELXPMCGC
00536         INITIALIZE WS-SAVE-AREAS,                                 ELXPMCGC
00537                    WS-CONTRACT-KEY-MAJ,                           ELXPMCGC
00538                    WS-SAVE-TABULAR-INDICATORS,                    ELXPMCGC
00539                    WS-PROVIDER-TABLE-INDICATORS,                  ELXPMCGC
00540                    WS-COUNTS,                                     ELXPMCGC
00541                    WS-MISC-SWITCHES-2                             ELXPMCGC
00542         MOVE ZERO TO PMCI-BLUE-CHIP-RETURN-CODE                   ELXPMCGC
00543         SET PROCESSING-MAJOR-MED TO TRUE                          ELXPMCGC
00544         MOVE WS-MAJ-MED-LOB TO WS-SAV-LOB                         ELXPMCGC
00545         PERFORM 3000-DERIVE-CONTRACT-KEY.                         ELXPMCGC
00546                                                                   ELXPMCGC
00547 ************************************************************      ELXPMCGC
00548 *                                                          *      ELXPMCGC
00549 *        DETERMINE IF EMPLOYER                             *      ELXPMCGC
00550 *                                                          *      ELXPMCGC
00551 ************************************************************      ELXPMCGC
00552  2100-DETERMINE-IF-EMPLOYER.                                      ELXPMCGC
00553                                                                   ELXPMCGC
00554      SET PMCI-GROUP-INDEX TO 3.                                   ELXPMCGC
00555      SET WS-EMP-GRP-IDX-MAX TO PMCI-GROUP-INDEX.                  ELXPMCGC
00556      PERFORM 2200-SEARCH-PMCI-EMPL-GROUPS                         ELXPMCGC
00557          VARYING PMCI-GROUP-INDEX FROM 1 BY 1                     ELXPMCGC
00558          UNTIL PMCI-GROUP-INDEX > WS-EMP-GRP-IDX-MAX              ELXPMCGC
00559          OR EMPLOYER.                                             ELXPMCGC
00560      IF NOT-EMPLOYER                                              ELXPMCGC
00561          PERFORM 2300-VERIFY-EMPL-GRP-PROV-TBL.                   ELXPMCGC
00562                                                                   ELXPMCGC
00563 ************************************************************      ELXPMCGC
00564 *                                                          *      ELXPMCGC
00565 *        SEARCH PMCI EMPLOYER GROUPS                       *      ELXPMCGC
00566 *                                                          *      ELXPMCGC
00567 ************************************************************      ELXPMCGC
00568  2200-SEARCH-PMCI-EMPL-GROUPS.                                    ELXPMCGC
00569                                                                   ELXPMCGC
00570      IF WS-GRP-X =                                                ELXPMCGC
00571               PMCI-HOSP-EMPLOYEE-NO (PMCI-GROUP-INDEX) OR         ELXPMCGC
00572          WS-GRP-X =                                               ELXPMCGC
00573              PMCI-STUDENT-NURSE-NO (PMCI-GROUP-INDEX)             ELXPMCGC
00574            SET EMPLOYER TO TRUE.                                  ELXPMCGC
00575                                                                   ELXPMCGC
00576 ************************************************************      ELXPMCGC
00577 *                                                          *      ELXPMCGC
00578 *        VERIFY EMPLOYER GRP PROVIDER TABLE                *      ELXPMCGC
00579 *                                                          *      ELXPMCGC
00580 ************************************************************      ELXPMCGC
00581  2300-VERIFY-EMPL-GRP-PROV-TBL.                                   ELXPMCGC
00582                                                                   ELXPMCGC
00583      SET GRP-INDX TO 24.                                          ELXPMCGC
00584      SET WS-EMP-GRP-IDX-MAX TO GRP-INDX.                          ELXPMCGC
00585      PERFORM 2400-SEARCH-EMPL-GRP-PROV-TBL                        ELXPMCGC
00586          VARYING GRP-INDX FROM 1 BY 1                             ELXPMCGC
00587          UNTIL GRP-INDX > WS-EMP-GRP-IDX-MAX                      ELXPMCGC
00588          OR EMPLOYER.                                             ELXPMCGC
00589                                                                   ELXPMCGC
00590 ************************************************************      ELXPMCGC
00591 *                                                          *      ELXPMCGC
00592 *        SEARCH EMPLOYER GRP PROVIDE TABLE                 *      ELXPMCGC
00593 *                                                          *      ELXPMCGC
00594 ************************************************************      ELXPMCGC
00595  2400-SEARCH-EMPL-GRP-PROV-TBL.                                   ELXPMCGC
00596                                                                   ELXPMCGC
00597      IF WS-GRP-X = EMPLOYEE-GROUP (GRP-INDX)                      ELXPMCGC
00598         SET PROV-INDX TO 3                                        ELXPMCGC
00599         SET WS-EMP-NUM-IDX-MAX TO PROV-INDX                       ELXPMCGC
00600         PERFORM 2500-SEARCH-EMPL-GRP-PROV-NBR                     ELXPMCGC
00601            VARYING PROV-INDX FROM 1 BY 1                          ELXPMCGC
00602              UNTIL PROV-INDX > WS-EMP-NUM-IDX-MAX                 ELXPMCGC
00603                OR EMPLOYER.                                       ELXPMCGC
00604                                                                   ELXPMCGC
00605 ************************************************************      ELXPMCGC
00606 *                                                          *      ELXPMCGC
00607 *        SEARCH EMPLOYER GRP PROVIDE NUMBER                *      ELXPMCGC
00608 *                                                          *      ELXPMCGC
00609 ************************************************************      ELXPMCGC
00610  2500-SEARCH-EMPL-GRP-PROV-NBR.                                   ELXPMCGC
00611                                                                   ELXPMCGC
00612      IF PMCI-PROVIDER-NUMBER =                                    ELXPMCGC
00613                    PROVIDER-NUM (GRP-INDX PROV-INDX)              ELXPMCGC
00614          SET EMPLOYER TO TRUE.                                    ELXPMCGC
00615                                                                   ELXPMCGC
00616 ************************************************************      ELXPMCGC
00617 *                                                          *      ELXPMCGC
00618 *        DETERMINE LINE OF BUSINESS                        *      ELXPMCGC
00619 *                                                          *      ELXPMCGC
00620 ************************************************************      ELXPMCGC
00621  2600-DETERMINE-LINE-OF-BUS.                                      ELXPMCGC
00622                                                                   ELXPMCGC
00623      MOVE GCG-L-O-B-CONTRACT-LEVEL-IND TO                         ELXPMCGC
00624                                PMCI-LOB-LVL-IND.                  ELXPMCGC
00625      SET LOB-MAX-INDX TO LOB-ENTRY-CNT.                           ELXPMCGC
00626      PERFORM 2700-SEARCH-LINE-OF-BUS-TABLE                        ELXPMCGC
00627          VARYING LOB-INDX FROM 1 BY 1                             ELXPMCGC
00628             UNTIL LOB-INDX > LOB-MAX-INDX                         ELXPMCGC
00629                OR LOB-FOUND.                                      ELXPMCGC
00630      IF LOB-NOT-FOUND                                             ELXPMCGC
00631          SET PMCI-BC-NO-CONTRACT TO TRUE                          ELXPMCGC
00632          MOVE 08 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
00633      ELSE                                                         ELXPMCGC
00634      IF WS-BSC-LOB = 'R' AND WS-MAJ-MED-LOB = 'R'                 ELXPMCGC
00635         SET PMCI-BC-NO-CONTRACT TO TRUE                           ELXPMCGC
00636         MOVE 09 TO PMCI-BLUE-CHIP-ERROR-CODE.                     ELXPMCGC
00637                                                                   ELXPMCGC
00638 ************************************************************      ELXPMCGC
00639 *                                                          *      ELXPMCGC
00640 *        SEARCH LINE OF BUSINESS TABLE                     *      ELXPMCGC
00641 *                                                          *      ELXPMCGC
00642 ************************************************************      ELXPMCGC
00643  2700-SEARCH-LINE-OF-BUS-TABLE.                                   ELXPMCGC
00644                                                                   ELXPMCGC
00645          IF GCG-L-O-B-CONTRACT-LEVEL-IND =                        ELXPMCGC
00646                             LOB-KEY (LOB-INDX)                    ELXPMCGC
00647             SET LOB-FOUND TO TRUE                                 ELXPMCGC
00648             MOVE LOB-MAJ-MED (LOB-INDX) TO WS-MAJ-MED-LOB         ELXPMCGC
00649             IF PMCI-INSTITUTIONAL                                 ELXPMCGC
00650                MOVE LOB-INST (LOB-INDX) TO WS-BSC-LOB             ELXPMCGC
00651             ELSE                                                  ELXPMCGC
00652                MOVE LOB-PROF (LOB-INDX) TO WS-BSC-LOB.            ELXPMCGC
00653                                                                   ELXPMCGC
00654 ************************************************************      ELXPMCGC
00655 *                                                          *      ELXPMCGC
00656 *        DERIVE CONTRACT KEY                               *      ELXPMCGC
00657 *                                                          *      ELXPMCGC
00658 ************************************************************      ELXPMCGC
00659  3000-DERIVE-CONTRACT-KEY.                                        ELXPMCGC
00660                                                                   ELXPMCGC
00661 *MAY HAVE PMCI CHANGE HERE                                        ELXPMCGC
00662      MOVE 000 TO PMCI-PLAN-CODE.                                  ELXPMCGC
00663      MOVE PMCI-PLAN-CODE TO                                       ELXPMCGC
00664                  KWA-GCDATES-PLAN-CODE.                           ELXPMCGC
00665      MOVE PMCI-GROUP-NBR TO WS-DAT-NBR                            ELXPMCGC
00666                            KWA-GCDATES-GROUP-NUMBER.              ELXPMCGC
00667      MOVE PMCI-SECT-NUM TO KWA-GCDATES-SECTION-NUMBER.            ELXPMCGC
00668      MOVE PMCI-PACKAGE-CODE TO KWA-GCDATES-PKG-CODE.              ELXPMCGC
00669      MOVE WS-SAV-LOB TO KWA-GCDATES-L-O-B.                        ELXPMCGC
00670      MOVE LOW-VALUES TO KWA-GCDATES-FILLER.                       ELXPMCGC
00671      MOVE 'C' TO KWA-GCDATES-FILE-REF.                            ELXPMCGC
00672      MOVE KWA-GCDATES-KEY TO WS-DATES-KEY.                        ELXPMCGC
00673      MOVE WS-DATES-KEY TO KWA-GCDATES-KEY.                        ELXPMCGC
00674      PERFORM 7000-READ-DATES-FILE.                                ELXPMCGC
00675      IF PMCI-BC-SUCCESSFUL                                        ELXPMCGC
00676          PERFORM 3100-DETERMINE-IF-TABS-NEEDED                    ELXPMCGC
00677          SET DTE-INDEX TO DTE-ENTRY-COUNT                         ELXPMCGC
00678          SET WS-DTE-MAX-IDX TO DTE-INDEX                          ELXPMCGC
00679          PERFORM 3300-SEARCH-CONTRACT-DATE-KEY                    ELXPMCGC
00680             VARYING DTE-INDEX FROM 1 BY 1                         ELXPMCGC
00681               UNTIL DTE-INDEX = WS-DTE-MAX-IDX                    ELXPMCGC
00682          IF PMCI-BC-SUCCESSFUL                                    ELXPMCGC
00683             PERFORM 6000-SELECT-CORRECT-DATE-KEY                  ELXPMCGC
00684             IF PMCI-BC-SUCCESSFUL                                 ELXPMCGC
00685                PERFORM 7200-READ-CONTRACT-FILE.                   ELXPMCGC
00686      IF PMCI-BC-SUCCESSFUL AND                                    ELXPMCGC
00687         WS-SAV-LOB = '1'                                          ELXPMCGC
00688         PERFORM 7300-CHECK-SUPP-MED                               ELXPMCGC
00689         IF PMCI-SUPP-MED = 'Y'                                    ELXPMCGC
00690            PERFORM 7400-CK-BCBS-DED                               ELXPMCGC
00691               VARYING SUB1 FROM 1 BY 1                            ELXPMCGC
00692               UNTIL SUB1 > ( GCG-COUNT-TAB-PROVN-POINTERS - 1)    ELXPMCGC
00693         END-IF                                                    ELXPMCGC
00694      END-IF.                                                      ELXPMCGC
00695                                                                   ELXPMCGC
00696 ************************************************************      ELXPMCGC
00697 *                                                          *      ELXPMCGC
00698 *        DETERMINE IF TABULARS ARE NEEDED                  *      ELXPMCGC
00699 *                                                          *      ELXPMCGC
00700 ************************************************************      ELXPMCGC
00701  3100-DETERMINE-IF-TABS-NEEDED.                                   ELXPMCGC
00702                                                                   ELXPMCGC
00703      SET TAB-MAX-INDX TO TAB-ENTRY-CNT.                           ELXPMCGC
00704      MOVE DTE-PROVIDER-CNTRL (DTE-INDEX) TO WS-TAB-CON-PRV.       ELXPMCGC
00705      MOVE '0Z0Z' TO WS-TAB-INDICATORS, WS-PTBL-INDICATORS.        ELXPMCGC
00706      IF PROCESSING-BASIC                                          ELXPMCGC
00707         IF PMCI-INSTITUTIONAL                                     ELXPMCGC
00708            MOVE GCG-PROV-CONTROL-CONT-BC-IND TO SAV-PRV           ELXPMCGC
00709            MOVE GCG-PROV-CONTROL-CONT-BC-IND TO WS-TAB-GRP-PRV    ELXPMCGC
00710            MOVE WS-EMPLOYER TO WS-TAB-INST-EMPL                   ELXPMCGC
00711            MOVE PMCI-PLAN-INDICATOR TO WS-TAB-INST-PLAN           ELXPMCGC
00712            MOVE 'I' TO WS-TAB-TYPE, SAV-TYP                       ELXPMCGC
00713         ELSE                                                      ELXPMCGC
00714           MOVE GCG-PROV-CONTROL-CONT-BS-IND TO SAV-PRV            ELXPMCGC
00715           MOVE GCG-PROV-CONTROL-CONT-BS-IND TO WS-TAB-GRP-PRV     ELXPMCGC
00716           MOVE WS-EMPLOYER TO WS-TAB-PROF-EMPL                    ELXPMCGC
00717           MOVE '+' TO WS-TAB-PROF-PHAR                            ELXPMCGC
00718           MOVE '+' TO WS-TAB-PROF-VISN                            ELXPMCGC
00719           MOVE PMCI-MPP-INDICATOR TO WS-TAB-PROF-MPP              ELXPMCGC
00720           MOVE 'P' TO WS-TAB-TYPE, SAV-TYP                        ELXPMCGC
00721      ELSE                                                         ELXPMCGC
00722      IF PROCESSING-MAJOR-MED                                      ELXPMCGC
00723         IF PMCI-INSTITUTIONAL                                     ELXPMCGC
00724            MOVE GCG-PROV-CONTROL-CONT-MM-IND TO SAV-PRV           ELXPMCGC
00725            MOVE GCG-PROV-CONTROL-CONT-MM-IND TO WS-TAB-GRP-PRV    ELXPMCGC
00726            MOVE WS-EMPLOYER TO WS-TAB-INST-EMPL                   ELXPMCGC
00727            MOVE PMCI-PLAN-INDICATOR TO WS-TAB-INST-PLAN           ELXPMCGC
00728            MOVE 'I' TO WS-TAB-TYPE, SAV-TYP                       ELXPMCGC
00729          ELSE                                                     ELXPMCGC
00730           MOVE GCG-PROV-CONTROL-CONT-MM-IND TO SAV-PRV            ELXPMCGC
00731           MOVE GCG-PROV-CONTROL-CONT-MM-IND TO WS-TAB-GRP-PRV     ELXPMCGC
00732           MOVE WS-EMPLOYER TO WS-TAB-PROF-EMPL                    ELXPMCGC
00733           MOVE '+' TO WS-TAB-PROF-PHAR                            ELXPMCGC
00734           MOVE '+' TO WS-TAB-PROF-VISN                            ELXPMCGC
00735           MOVE PMCI-MPP-INDICATOR TO WS-TAB-PROF-MPP              ELXPMCGC
00736           MOVE 'P' TO WS-TAB-TYPE, SAV-TYP.                       ELXPMCGC
00737      PERFORM 3200-SEARCH-TAB-READ-IND-TABLE                       ELXPMCGC
00738          VARYING TAB-INDX FROM 1 BY 1                             ELXPMCGC
00739             UNTIL TAB-INDX > TAB-MAX-INDX OR                      ELXPMCGC
00740               PROVIDER-STOP.                                      ELXPMCGC
00741      IF PROVIDER-STOP AND                                         ELXPMCGC
00742         WS-TAB-READ-INDICATORS NOT EQUAL 'NNN' AND                ELXPMCGC
00743                PMCI-PROVIDER-NUMBER NOT EQUAL ZEROS               ELXPMCGC
00744         SET TABS-NEEDED TO TRUE                                   ELXPMCGC
00745         PERFORM 8000-CALL-TABULAR-READ-ROUTINE.                   ELXPMCGC
00746 ************************************************************      ELXPMCGC
00747 *                                                          *      ELXPMCGC
00748 *        SEARCH TABULAR READ INDICATOR TABLE               *      ELXPMCGC
00749 *                                                          *      ELXPMCGC
00750 ************************************************************      ELXPMCGC
00751  3200-SEARCH-TAB-READ-IND-TABLE.                                  ELXPMCGC
00752                                                                   ELXPMCGC
00753      IF SAV-PRV-TAB-KEY = TAB-KEY (TAB-INDX)                      ELXPMCGC
00754          MOVE TAB-READ-INDICATORS (TAB-INDX) TO                   ELXPMCGC
00755             WS-TAB-READ-INDICATORS                                ELXPMCGC
00756          SET PROVIDER-STOP TO TRUE                                ELXPMCGC
00757      ELSE                                                         ELXPMCGC
00758          MOVE 'NNN' TO WS-TAB-READ-INDICATORS.                    ELXPMCGC
00759                                                                   ELXPMCGC
00760 ************************************************************      ELXPMCGC
00761 *                                                          *      ELXPMCGC
00762 *        SEARCH CONTRACT DATE KEY                          *      ELXPMCGC
00763 *                                                          *      ELXPMCGC
00764 ************************************************************      ELXPMCGC
00765  3300-SEARCH-CONTRACT-DATE-KEY.                                   ELXPMCGC
00766                                                                   ELXPMCGC
00767      IF DTE-EFFDT-CEN (DTE-INDEX) <= CURRENT-DAY-CEN AND          ELXPMCGC
00768         DTE-TERMDT-CEN (DTE-INDEX) >= CURRENT-DAY-CEN             ELXPMCGC
00769         MOVE DTE-EFF-TERM (DTE-INDEX) TO WS-HOLD-DATES-C          ELXPMCGC
00770         MOVE DTE-PROVIDER-CNTRL (DTE-INDEX) TO WS-TAB-CON-PRV     ELXPMCGC
00771         IF PROCESSING-BASIC                                       ELXPMCGC
00772            IF PMCI-INSTITUTIONAL                                  ELXPMCGC
00773               PERFORM 4000-DERIVE-INST-PROVIDER                   ELXPMCGC
00774            ELSE                                                   ELXPMCGC
00775               PERFORM 4100-DERIVE-PROF-PROVIDER                   ELXPMCGC
00776            END-IF                                                 ELXPMCGC
00777            IF PROVIDER-OK                                         ELXPMCGC
00778               PERFORM 5000-QUAL-MEDICARE-ELIGIBILITY              ELXPMCGC
00779            END-IF                                                 ELXPMCGC
00780         ELSE                                                      ELXPMCGC
00781            IF PMCI-INSTITUTIONAL                                  ELXPMCGC
00782               PERFORM 4200-DERIVE-INST-PROVIDER-MM                ELXPMCGC
00783            ELSE                                                   ELXPMCGC
00784               PERFORM 4300-DERIVE-PROF-PROVIDER-MM                ELXPMCGC
00785            END-IF                                                 ELXPMCGC
00786            IF PROVIDER-OK                                         ELXPMCGC
00787               PERFORM 5000-QUAL-MEDICARE-ELIGIBILITY              ELXPMCGC
00788            END-IF                                                 ELXPMCGC
00789      END-IF.                                                      ELXPMCGC
00790                                                                   ELXPMCGC
00791 ************************************************************      ELXPMCGC
00792 *                                                          *      ELXPMCGC
00793 *        DERIVE INSTITUTIONAL PROVIDER                     *      ELXPMCGC
00794 *                                                          *      ELXPMCGC
00795 ************************************************************      ELXPMCGC
00796  4000-DERIVE-INST-PROVIDER.                                       ELXPMCGC
00797                                                                   ELXPMCGC
00798      SET PRV-INST-NULL TO TRUE.                                   ELXPMCGC
00799      SET PROVIDER-NOT-FOUND TO TRUE.                              ELXPMCGC
00800      SET PROVIDER-NULL TO TRUE.                                   ELXPMCGC
00801      SET IPRV-MAX-INDX TO IPRV-ENTRY-CNT.                         ELXPMCGC
00802      SET PROVIDER-REJECT TO TRUE.                                 ELXPMCGC
00803      PERFORM 4050-SEARCH-INST-PROV-TABLE                          ELXPMCGC
00804         VARYING IPRV-INDX FROM 1 BY 1                             ELXPMCGC
00805           UNTIL IPRV-INDX > IPRV-MAX-INDX OR                      ELXPMCGC
00806             PRV-INST-STOP.                                        ELXPMCGC
00807                                                                   ELXPMCGC
00808 ************************************************************      ELXPMCGC
00809 *                                                          *      ELXPMCGC
00810 *        SEARCH INSTITUTIONAL PROVIDER TABLE               *      ELXPMCGC
00811 *                                                          *      ELXPMCGC
00812 ************************************************************      ELXPMCGC
00813  4050-SEARCH-INST-PROV-TABLE.                                     ELXPMCGC
00814                                                                   ELXPMCGC
00815      IF WS-TAB-KEY = IPRV-KEY (IPRV-INDX)                         ELXPMCGC
00816          SET PRV-INST-STOP TO TRUE                                ELXPMCGC
00817          MOVE IPRV-KEY (IPRV-INDX) TO WS-PTBL-KEY                 ELXPMCGC
00818          MOVE IPRV-PLAN-IND (IPRV-INDX) TO WS-PTBL-INST-PLAN      ELXPMCGC
00819          MOVE IPRV-EMPL-IND (IPRV-INDX) TO WS-PTBL-INST-EMPL      ELXPMCGC
00820          MOVE IPRV-TAB-IND1 (IPRV-INDX) TO WS-PTBL-IND1           ELXPMCGC
00821          MOVE IPRV-TAB-IND2 (IPRV-INDX) TO WS-PTBL-IND2           ELXPMCGC
00822          IF WS-TAB-INDICATORS = WS-PTBL-INDICATORS                ELXPMCGC
00823             SET PROVIDER-FOUND TO TRUE.                           ELXPMCGC
00824      IF PROVIDER-FOUND                                            ELXPMCGC
00825          IF WS-PTBL-INST-PLAN = '+' OR                            ELXPMCGC
00826                          PMCI-PLAN-INDICATOR = WS-PTBL-INST-PLAN  ELXPMCGC
00827             IF WS-PTBL-INST-EMPL = '+' OR                         ELXPMCGC
00828                          WS-EMPLOYER = WS-PTBL-INST-EMPL          ELXPMCGC
00829                SET PROVIDER-OK TO TRUE.                           ELXPMCGC
00830                                                                   ELXPMCGC
00831 ************************************************************      ELXPMCGC
00832 *                                                          *      ELXPMCGC
00833 *        DERIVE PROFESSIONAL PROVIDER                      *      ELXPMCGC
00834 *                                                          *      ELXPMCGC
00835 ************************************************************      ELXPMCGC
00836  4100-DERIVE-PROF-PROVIDER.                                       ELXPMCGC
00837                                                                   ELXPMCGC
00838      SET PROVIDER-NULL TO TRUE.                                   ELXPMCGC
00839      SET PROVIDER-NOT-FOUND TO TRUE.                              ELXPMCGC
00840      SET PRV-PROF-NULL TO TRUE.                                   ELXPMCGC
00841      SET PPRV-MAX-INDX TO PPRV-ENTRY-CNT.                         ELXPMCGC
00842      SET PROVIDER-REJECT TO TRUE.                                 ELXPMCGC
00843      PERFORM 4150-SEARCH-PROF-PRV-TABLE                           ELXPMCGC
00844         VARYING PPRV-INDX FROM 1 BY 1                             ELXPMCGC
00845            UNTIL PPRV-INDX > PPRV-MAX-INDX OR                     ELXPMCGC
00846              PRV-PROF-STOP.                                       ELXPMCGC
00847                                                                   ELXPMCGC
00848 ************************************************************      ELXPMCGC
00849 *                                                          *      ELXPMCGC
00850 *        SEARCH PROFESSIONAL PROVIDER TABLE                *      ELXPMCGC
00851 *                                                          *      ELXPMCGC
00852 ************************************************************      ELXPMCGC
00853  4150-SEARCH-PROF-PRV-TABLE.                                      ELXPMCGC
00854                                                                   ELXPMCGC
00855      IF WS-TAB-KEY = PPRV-KEY (PPRV-INDX)                         ELXPMCGC
00856          SET PRV-PROF-STOP TO TRUE                                ELXPMCGC
00857          MOVE PPRV-KEY (PPRV-INDX) TO WS-PTBL-KEY                 ELXPMCGC
00858          MOVE PPRV-MPP-IND (PPRV-INDX) TO WS-PTBL-PROF-MPP        ELXPMCGC
00859          MOVE PPRV-EMPL-IND (PPRV-INDX) TO WS-PTBL-PROF-EMPL      ELXPMCGC
00860          MOVE PPRV-PHAR-IND (PPRV-INDX) TO WS-PTBL-PROF-PHAR      ELXPMCGC
00861          MOVE PPRV-VISN-IND (PPRV-INDX) TO WS-PTBL-PROF-VISN      ELXPMCGC
00862          MOVE PPRV-TAB-IND1 (PPRV-INDX) TO WS-PTBL-IND1           ELXPMCGC
00863          MOVE PPRV-TAB-IND2 (PPRV-INDX) TO WS-PTBL-IND2           ELXPMCGC
00864          IF WS-TAB-INDICATORS = WS-PTBL-INDICATORS                ELXPMCGC
00865             SET PROVIDER-FOUND TO TRUE.                           ELXPMCGC
00866      IF PROVIDER-FOUND                                            ELXPMCGC
00867          IF WS-PTBL-PROF-MPP = '+' OR                             ELXPMCGC
00868                             PMCI-MPP-INDICATOR = WS-PTBL-PROF-MPP ELXPMCGC
00869             IF WS-PTBL-PROF-PHAR = 'Z' OR                         ELXPMCGC
00870                             WS-PTBL-PROF-PHAR = '+'               ELXPMCGC
00871                IF WS-PTBL-PROF-VISN = 'Z' OR                      ELXPMCGC
00872                             WS-PTBL-PROF-VISN = '+'               ELXPMCGC
00873                   IF WS-PTBL-INST-EMPL = '+' OR                   ELXPMCGC
00874                             WS-EMPLOYER = WS-PTBL-INST-EMPL       ELXPMCGC
00875                      SET PROVIDER-OK TO TRUE.                     ELXPMCGC
00876                                                                   ELXPMCGC
00877 ************************************************************      ELXPMCGC
00878 *                                                          *      ELXPMCGC
00879 *        DERIVE INSTITUTIONAL PROVIDER  - MAJOR MEDICAL    *      ELXPMCGC
00880 *                                                          *      ELXPMCGC
00881 ************************************************************      ELXPMCGC
00882  4200-DERIVE-INST-PROVIDER-MM.                                    ELXPMCGC
00883                                                                   ELXPMCGC
00884      SET PRV-INST-NULL TO TRUE.                                   ELXPMCGC
00885      SET PROVIDER-NOT-FOUND TO TRUE.                              ELXPMCGC
00886      SET PROVIDER-NULL TO TRUE.                                   ELXPMCGC
00887      SET IMPRV-MAX-INDX TO IMPRV-ENTRY-CNT.                       ELXPMCGC
00888      SET PROVIDER-REJECT TO TRUE.                                 ELXPMCGC
00889      PERFORM 4250-SEARCH-INST-PROV-TABLE-MM                       ELXPMCGC
00890         VARYING IMPRV-INDX FROM 1 BY 1                            ELXPMCGC
00891           UNTIL IMPRV-INDX > IMPRV-MAX-INDX OR                    ELXPMCGC
00892             PRV-INST-STOP.                                        ELXPMCGC
00893                                                                   ELXPMCGC
00894 ************************************************************      ELXPMCGC
00895 *                                                          *      ELXPMCGC
00896 *        SEARCH INSTITUTIONAL PROVIDER TABLE - MAJOR MED   *      ELXPMCGC
00897 *                                                          *      ELXPMCGC
00898 ************************************************************      ELXPMCGC
00899  4250-SEARCH-INST-PROV-TABLE-MM.                                  ELXPMCGC
00900                                                                   ELXPMCGC
00901      IF WS-TAB-KEY = IMPRV-KEY (IMPRV-INDX)                       ELXPMCGC
00902          SET PRV-INST-STOP TO TRUE                                ELXPMCGC
00903          MOVE IMPRV-KEY (IMPRV-INDX) TO WS-PTBL-KEY               ELXPMCGC
00904          MOVE IMPRV-PLAN-IND (IMPRV-INDX) TO WS-PTBL-INST-PLAN    ELXPMCGC
00905          MOVE IMPRV-EMPL-IND (IMPRV-INDX) TO WS-PTBL-INST-EMPL    ELXPMCGC
00906          MOVE IMPRV-TAB-IND1 (IMPRV-INDX) TO WS-PTBL-IND1         ELXPMCGC
00907          MOVE IMPRV-TAB-IND2 (IMPRV-INDX) TO WS-PTBL-IND2         ELXPMCGC
00908          IF WS-TAB-INDICATORS = WS-PTBL-INDICATORS                ELXPMCGC
00909             SET PROVIDER-FOUND TO TRUE.                           ELXPMCGC
00910      IF PROVIDER-FOUND                                            ELXPMCGC
00911          IF WS-PTBL-INST-PLAN = '+' OR                            ELXPMCGC
00912               PMCI-PLAN-INDICATOR = WS-PTBL-INST-PLAN             ELXPMCGC
00913             IF WS-PTBL-INST-EMPL = '+' OR                         ELXPMCGC
00914                          WS-EMPLOYER = WS-PTBL-INST-EMPL          ELXPMCGC
00915                SET PROVIDER-OK TO TRUE.                           ELXPMCGC
00916                                                                   ELXPMCGC
00917 ************************************************************      ELXPMCGC
00918 *                                                          *      ELXPMCGC
00919 *        DERIVE PROFESSIONAL PROVIDER - MAJOR MEDICAL      *      ELXPMCGC
00920 *                                                          *      ELXPMCGC
00921 ************************************************************      ELXPMCGC
00922  4300-DERIVE-PROF-PROVIDER-MM.                                    ELXPMCGC
00923                                                                   ELXPMCGC
00924      SET PROVIDER-NULL TO TRUE.                                   ELXPMCGC
00925      SET PROVIDER-NOT-FOUND TO TRUE.                              ELXPMCGC
00926      SET PRV-PROF-NULL TO TRUE.                                   ELXPMCGC
00927      SET PMPRV-MAX-INDX TO PMPRV-ENTRY-CNT.                       ELXPMCGC
00928      SET PROVIDER-REJECT TO TRUE.                                 ELXPMCGC
00929      PERFORM 4350-SEARCH-PROF-PRV-TABLE-MM                        ELXPMCGC
00930         VARYING PMPRV-INDX FROM 1 BY 1                            ELXPMCGC
00931            UNTIL PMPRV-INDX > PMPRV-MAX-INDX OR                   ELXPMCGC
00932              PRV-PROF-STOP.                                       ELXPMCGC
00933                                                                   ELXPMCGC
00934 ************************************************************      ELXPMCGC
00935 *                                                          *      ELXPMCGC
00936 *        SEARCH PROFESSIONAL PROVIDER TABLE - MAJOR MEDICAL*      ELXPMCGC
00937 *                                                          *      ELXPMCGC
00938 ************************************************************      ELXPMCGC
00939  4350-SEARCH-PROF-PRV-TABLE-MM.                                   ELXPMCGC
00940                                                                   ELXPMCGC
00941      IF WS-TAB-KEY = PMPRV-KEY (PMPRV-INDX)                       ELXPMCGC
00942          SET PRV-PROF-STOP TO TRUE                                ELXPMCGC
00943          MOVE PMPRV-KEY (PMPRV-INDX) TO WS-PTBL-KEY               ELXPMCGC
00944          MOVE PMPRV-MPP-IND (PMPRV-INDX) TO WS-PTBL-PROF-MPP      ELXPMCGC
00945          MOVE PMPRV-EMPL-IND (PMPRV-INDX) TO WS-PTBL-PROF-EMPL    ELXPMCGC
00946          MOVE PMPRV-PHAR-IND (PMPRV-INDX) TO WS-PTBL-PROF-PHAR    ELXPMCGC
00947          MOVE PMPRV-VISN-IND (PMPRV-INDX) TO WS-PTBL-PROF-VISN    ELXPMCGC
00948          MOVE PMPRV-TAB-IND1 (PMPRV-INDX) TO WS-PTBL-IND1         ELXPMCGC
00949          MOVE PMPRV-TAB-IND2 (PMPRV-INDX) TO WS-PTBL-IND2         ELXPMCGC
00950          IF WS-TAB-INDICATORS = WS-PTBL-INDICATORS                ELXPMCGC
00951             SET PROVIDER-FOUND TO TRUE.                           ELXPMCGC
00952      IF PROVIDER-FOUND                                            ELXPMCGC
00953          IF WS-PTBL-PROF-MPP = '+' OR                             ELXPMCGC
00954                             PMCI-MPP-INDICATOR = WS-PTBL-PROF-MPP ELXPMCGC
00955             IF WS-PTBL-PROF-PHAR = 'Z' OR                         ELXPMCGC
00956                             WS-PTBL-PROF-PHAR = '+'               ELXPMCGC
00957                IF WS-PTBL-PROF-VISN = 'Z' OR                      ELXPMCGC
00958                             WS-PTBL-PROF-VISN = '+'               ELXPMCGC
00959                   IF WS-PTBL-INST-EMPL = '+' OR                   ELXPMCGC
00960                             WS-EMPLOYER = WS-PTBL-INST-EMPL       ELXPMCGC
00961                      SET PROVIDER-OK TO TRUE.                     ELXPMCGC
00962                                                                   ELXPMCGC
00963 ************************************************************      ELXPMCGC
00964 *                                                          *      ELXPMCGC
00965 *        QUALIFY MEDICARE ELIGIBILITY                      *      ELXPMCGC
00966 *                                                          *      ELXPMCGC
00967 ************************************************************      ELXPMCGC
00968  5000-QUAL-MEDICARE-ELIGIBILITY.                                  ELXPMCGC
00969                                                                   ELXPMCGC
00970      IF DTE-FAMILY-RELAT-LEVEL (DTE-INDEX) = '0M'                 ELXPMCGC
00971          MOVE DTE-EFF-TERM (DTE-INDEX) TO                         ELXPMCGC
00972              WS-MEDICARE-SAVE                                     ELXPMCGC
00973          ADD 1 TO WS-MEDICARE-COUNT                               ELXPMCGC
00974          ADD 1 TO WS-TOT-COUNT                                    ELXPMCGC
00975      ELSE                                                         ELXPMCGC
00976          PERFORM 5100-QUAL-FAMILY-RELATIONSHIP.                   ELXPMCGC
00977                                                                   ELXPMCGC
00978 ************************************************************      ELXPMCGC
00979 *                                                          *      ELXPMCGC
00980 *        QUALIFY FAMILY RELATIONSHIP                       *      ELXPMCGC
00981 *                                                          *      ELXPMCGC
00982 ************************************************************      ELXPMCGC
00983  5100-QUAL-FAMILY-RELATIONSHIP.                                   ELXPMCGC
00984                                                                   ELXPMCGC
00985      SET FAMILY-SEARCH-REJECT TO TRUE.                            ELXPMCGC
00986      SET FAMILY-LOOP-NULL TO TRUE.                                ELXPMCGC
00987      PERFORM 5200-SEARCH-FAM-RELATION-TABLE                       ELXPMCGC
00988           VARYING FML-INDX FROM 1 BY 1                            ELXPMCGC
00989           UNTIL FML-INDX > FML-MAX-INDX                           ELXPMCGC
00990           OR FAMILY-LOOP-STOP.                                    ELXPMCGC
00991      IF FAMILY-SEARCH-OK                                          ELXPMCGC
00992           PERFORM 5400-TEST-NON-MEDICARE-GROUP.                   ELXPMCGC
00993                                                                   ELXPMCGC
00994 ************************************************************      ELXPMCGC
00995 *                                                          *      ELXPMCGC
00996 *        SEARCH FAMILY RELATIONSHIP TABLE                  *      ELXPMCGC
00997 *                                                          *      ELXPMCGC
00998 ************************************************************      ELXPMCGC
00999  5200-SEARCH-FAM-RELATION-TABLE.                                  ELXPMCGC
01000                                                                   ELXPMCGC
01001      IF DTE-FAMILY-RELAT-LEVEL (DTE-INDEX) =                      ELXPMCGC
01002               FML-KEY (FML-INDX) AND                              ELXPMCGC
01003                     FML-TYPE (FML-INDX) = 'A' OR                  ELXPMCGC
01004         DTE-FAMILY-RELAT-LEVEL (DTE-INDEX) =                      ELXPMCGC
01005               FML-KEY (FML-INDX) AND                              ELXPMCGC
01006                              PMCI-PAT-RELATIONSHIP =              ELXPMCGC
01007                                          FML-TYPE (FML-INDX)      ELXPMCGC
01008         PERFORM 5300-QUALIFY-PATIENT-AGE.                         ELXPMCGC
01009                                                                   ELXPMCGC
01010 ************************************************************      ELXPMCGC
01011 *                                                          *      ELXPMCGC
01012 *        QUALIFY PATIENT AGE                               *      ELXPMCGC
01013 *                                                          *      ELXPMCGC
01014 ************************************************************      ELXPMCGC
01015  5300-QUALIFY-PATIENT-AGE.                                        ELXPMCGC
01016                                                                   ELXPMCGC
01017      IF PMCI-PAT-AGE >= FML-LOW-AGE (FML-INDX) AND                ELXPMCGC
01018                    PMCI-PAT-AGE <= FML-HIGH-AGE (FML-INDX)        ELXPMCGC
01019         SET FAMILY-SEARCH-OK TO TRUE                              ELXPMCGC
01020         SET FAMILY-LOOP-STOP TO TRUE                              ELXPMCGC
01021         MOVE FML-IND-MED (FML-INDX) TO WS-HOLD-MED-IND            ELXPMCGC
01022      ELSE                                                         ELXPMCGC
01023      IF FML-IND (FML-INDX) NOT EQUAL 'Y'                          ELXPMCGC
01024         SET FAMILY-SEARCH-REJECT TO TRUE                          ELXPMCGC
01025         SET FAMILY-LOOP-STOP TO TRUE.                             ELXPMCGC
01026                                                                   ELXPMCGC
01027 ************************************************************      ELXPMCGC
01028 *                                                          *      ELXPMCGC
01029 *        SAVE NON-MEDICARE GROUP                           *      ELXPMCGC
01030 *                                                          *      ELXPMCGC
01031 ************************************************************      ELXPMCGC
01032  5400-TEST-NON-MEDICARE-GROUP.                                    ELXPMCGC
01033                                                                   ELXPMCGC
01034      IF WS-HOLD-MED-IND = 'Y'                                     ELXPMCGC
01035         MOVE DTE-EFF-TERM (DTE-INDEX) TO                          ELXPMCGC
01036              WS-MEDICARE-SAVE                                     ELXPMCGC
01037         ADD 1 TO WS-MEDICARE-COUNT                                ELXPMCGC
01038         ADD 1 TO WS-TOT-COUNT                                     ELXPMCGC
01039      ELSE                                                         ELXPMCGC
01040         MOVE DTE-EFF-TERM (DTE-INDEX) TO                          ELXPMCGC
01041                WS-NON-MEDICARE-SAVE                               ELXPMCGC
01042         ADD 1 TO WS-NON-MEDICARE-COUNT                            ELXPMCGC
01043         ADD 1 TO WS-TOT-COUNT.                                    ELXPMCGC
01044                                                                   ELXPMCGC
01045 ************************************************************      ELXPMCGC
01046 *                                                          *      ELXPMCGC
01047 *        SELECT CORRECT DATE KEY                           *      ELXPMCGC
01048 *                                                          *      ELXPMCGC
01049 ************************************************************      ELXPMCGC
01050  6000-SELECT-CORRECT-DATE-KEY.                                    ELXPMCGC
01051                                                                   ELXPMCGC
01052        IF PMCI-MEDICARE-ELIGIBLE                                  ELXPMCGC
01053             PERFORM 6100-SELECT-MEDICARE-KEY                      ELXPMCGC
01054      ELSE                                                         ELXPMCGC
01055             PERFORM 6200-SELECT-NON-MEDICARE-KEY.                 ELXPMCGC
01056                                                                   ELXPMCGC
01057 ************************************************************      ELXPMCGC
01058 *                                                          *      ELXPMCGC
01059 *        SELECT MEDICARE KEY                               *      ELXPMCGC
01060 *                                                          *      ELXPMCGC
01061 ************************************************************      ELXPMCGC
01062  6100-SELECT-MEDICARE-KEY.                                        ELXPMCGC
01063                                                                   ELXPMCGC
01064        IF WS-MEDICARE-COUNT = 1                                   ELXPMCGC
01065            MOVE WS-MEDICARE-SAVE TO WS-SAVE-DATES-KEY             ELXPMCGC
01066      ELSE                                                         ELXPMCGC
01067        IF WS-MEDICARE-COUNT GREATER THAN 1                        ELXPMCGC
01068             SET PMCI-BC-MULTIPLE-CONTRACTS TO TRUE                ELXPMCGC
01069             MOVE 14 TO PMCI-BLUE-CHIP-ERROR-CODE                  ELXPMCGC
01070      ELSE                                                         ELXPMCGC
01071          PERFORM 6200-SELECT-NON-MEDICARE-KEY.                    ELXPMCGC
01072                                                                   ELXPMCGC
01073 ************************************************************      ELXPMCGC
01074 *                                                          *      ELXPMCGC
01075 *        SELECT NON-MEDICARE KEY.                          *      ELXPMCGC
01076 *                                                          *      ELXPMCGC
01077 ************************************************************      ELXPMCGC
01078  6200-SELECT-NON-MEDICARE-KEY.                                    ELXPMCGC
01079                                                                   ELXPMCGC
01080      IF WS-NON-MEDICARE-COUNT = 1                                 ELXPMCGC
01081          MOVE WS-NON-MEDICARE-SAVE TO WS-SAVE-DATES-KEY           ELXPMCGC
01082      ELSE                                                         ELXPMCGC
01083      IF WS-NON-MEDICARE-COUNT GREATER THAN 1                      ELXPMCGC
01084          SET PMCI-BC-MULTIPLE-CONTRACTS TO TRUE                   ELXPMCGC
01085          MOVE 15 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
01086      ELSE                                                         ELXPMCGC
01087      IF WS-NON-MEDICARE-COUNT = ZERO                              ELXPMCGC
01088          MOVE 16 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
01089          SET PMCI-BC-NO-CONTRACT TO TRUE.                         ELXPMCGC
01090 ************************************************************      ELXPMCGC
01091 *                                                          *      ELXPMCGC
01092 *        READ DATE FILE                                    *      ELXPMCGC
01093 *                                                          *      ELXPMCGC
01094 ************************************************************      ELXPMCGC
01095  7000-READ-DATES-FILE.                                            ELXPMCGC
01096                                                                   ELXPMCGC
01097      SET CIA-GCDATES-DDN  TO TRUE.                                ELXPMCGC
01098      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGC
01099               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELXPMCGC
01100      MOVE KWA-GCDATES-KEY TO IOP-FILE-KEY.                        ELXPMCGC
01101      SET IOP-REC-PTR       TO NULL.                               ELXPMCGC
01102      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCGC
01103      SET IOP-RD            TO TRUE.                               ELXPMCGC
01104      SET IOP-FCQ-NONE      TO TRUE.                               ELXPMCGC
01105      SET IOP-KVQ-EQ        TO TRUE.                               ELXPMCGC
01106      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCGC
01107      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCGC
01108      IF IOP-RC-OK                                                 ELXPMCGC
01109          SET ADDRESS OF DTE-DATE-RECORD                           ELXPMCGC
01110                  TO IOP-REC-PTR                                   ELXPMCGC
01111      ELSE                                                         ELXPMCGC
01112          MOVE 01 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
01113          SET PMCI-BC-NO-CONTRACT TO TRUE.                         ELXPMCGC
01114                                                                   ELXPMCGC
01115 ************************************************************      ELXPMCGC
01116 *                                                          *      ELXPMCGC
01117 *        READ GROUP SPECIFIC FILE                          *      ELXPMCGC
01118 *                                                          *      ELXPMCGC
01119 ************************************************************      ELXPMCGC
01120  7100-READ-GROUP-SPEC-FILE.                                       ELXPMCGC
01121                                                                   ELXPMCGC
01122      SET CIA-GCGRPSPC-DDN  TO TRUE.                               ELXPMCGC
01123      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGC
01124               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELXPMCGC
01125      MOVE 000 TO KWA-GCG-PLAN-CODE.                               ELXPMCGC
01126      MOVE PMCI-GROUP-NBR TO KWA-GCG-GROUP-NUMBER.                 ELXPMCGC
01127      MOVE PMCI-SECT-NUM    TO KWA-GCG-SECTION-NUMBER.             ELXPMCGC
01128      MOVE PMCI-PACKAGE-CODE TO KWA-GCG-PKG-CODE.                  ELXPMCGC
01129      MOVE WS-SAV-FRL TO KWA-GCG-FAM-REL-LVL.                      ELXPMCGC
01130      MOVE WS-SAV-EFF TO KWA-GCG-EFF-DATE-CENTURY.                 ELXPMCGC
01131      MOVE KWA-GCGRPSPC-KEY TO WS-GROUP-KEY.                       ELXPMCGC
01132      MOVE KWA-GCCONTR-KEY TO PMCI-GRP-SPCFC-KEY                   ELXPMCGC
01133      SET CIA-GCGRPSPC-DDN  TO TRUE.                               ELXPMCGC
01134      MOVE KWA-GCGRPSPC-KEY TO IOP-FILE-KEY.                       ELXPMCGC
01135      SET IOP-REC-PTR       TO NULL.                               ELXPMCGC
01136      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCGC
01137      SET IOP-RD            TO TRUE.                               ELXPMCGC
01138      SET IOP-FCQ-NONE      TO TRUE.                               ELXPMCGC
01139      SET IOP-KVQ-EQ        TO TRUE.                               ELXPMCGC
01140      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCGC
01141      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCGC
01142      IF IOP-RC-OK                                                 ELXPMCGC
01143          SET ADDRESS OF GCG-GCGRPSPC-RECORD                       ELXPMCGC
01144                  TO IOP-REC-PTR                                   ELXPMCGC
01145          SET CIA-ELSGRPSP-DDN  TO TRUE                            ELXPMCGC
01146          CALL 'ELUSAVAD' USING DFHCOMMAREA                        ELXPMCGC
01147                                IOP-REC-PTR                        ELXPMCGC
01148          SET IOP-REC-PTR TO NULL                                  ELXPMCGC
01149      ELSE                                                         ELXPMCGC
01150          MOVE 04 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
01151          SET PMCI-BC-NO-CONTRACT TO TRUE.                         ELXPMCGC
01152      IF GCG-INTER-RELATIONAL-CODE = ZEROS                         ELXPMCGC
01153          MOVE 05 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCGC
01154          SET PMCI-BC-NO-CONTRACT TO TRUE.                         ELXPMCGC
01155 ****************BELOW IS TEMPORARY CODE FOR ACP*******            ELXPMCGC
01156      IF IOP-RC-OK OR NOT PMCI-BC-NO-CONTRACT                      ELXPMCGC
01157         SET PMCI-ACCUM-CO-PAY-NA TO TRUE                          ELXPMCGC
01158         PERFORM VARYING GCG-INDEX FROM 1 BY 1 UNTIL               ELXPMCGC
01159             GCG-TAB-ID(GCG-INDEX) > '#ACP  ' OR                   ELXPMCGC
01160             GCG-INDEX > GCG-COUNT-TAB-PROVN-POINTERS              ELXPMCGC
01161               IF GCG-TAB-ID(GCG-INDEX) = '#ACP  ' AND             ELXPMCGC
01162                  GCG-TAB-SLOT-NO(GCG-INDEX) > 0                   ELXPMCGC
01163                  SET PMCI-ACCUM-CO-PAY-CALL TO TRUE               ELXPMCGC
01164               END-IF                                              ELXPMCGC
01165         END-PERFORM                                               ELXPMCGC
01166      END-IF.                                                      ELXPMCGC
01167 ****************END OF TEMPORARY CODE FOR ACP*******              ELXPMCGC
01168                                                                   ELXPMCGC
01169 ************************************************************      ELXPMCGC
01170 *                                                          *      ELXPMCGC
01171 *        READ CONTRACT FILE                                *      ELXPMCGC
01172 *                                                          *      ELXPMCGC
01173 ************************************************************      ELXPMCGC
01174  7200-READ-CONTRACT-FILE.                                         ELXPMCGC
01175                                                                   ELXPMCGC
01176      SET CIA-GCCONTR-DDN  TO TRUE.                                ELXPMCGC
01177      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGC
01178               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELXPMCGC
01179      MOVE 000 TO KWA-GCT-PLAN-CODE.                               ELXPMCGC
01180 *    MOVE WS-GROUP-AREA TO KWA-GCT-GROUP-NUMBER.                  ELXPMCGC
01181      MOVE PMCI-GROUP-NBR TO KWA-GCT-GROUP-NUMBER.                 ELXPMCGC
01182 *NEED TO BE CHANGED TO 5 DIGITS.                                  ELXPMCGC
01183      MOVE PMCI-SECT-NUM TO KWA-GCT-SECTION-NUMBER.                ELXPMCGC
01184      MOVE WS-SAV-FRL TO KWA-GCT-FAM-REL-LVL.                      ELXPMCGC
01185      MOVE WS-SAV-EFF TO KWA-GCT-EFFECTIVE-DATE-CENTURY.           ELXPMCGC
01186      MOVE WS-SAV-LOB TO KWA-GCT-L-O-B.                            ELXPMCGC
01187      MOVE WS-SAV-PRV TO KWA-GCT-PROVDR-CONTROL.                   ELXPMCGC
01188      IF PROCESSING-BASIC                                          ELXPMCGC
01189         MOVE KWA-GCCONTR-KEY TO PMCI-BSC-CNTRCT-KEY               ELXPMCGC
01190         MOVE KWA-GCCONTR-KEY TO WS-CONTRACT-KEY-BSC               ELXPMCGC
01191      ELSE                                                         ELXPMCGC
01192         MOVE KWA-GCCONTR-KEY TO PMCI-MM-CNTRCT-KEY                ELXPMCGC
01193         MOVE KWA-GCCONTR-KEY TO WS-CONTRACT-KEY-MAJ.              ELXPMCGC
01194      SET CIA-GCCONTR-DDN  TO TRUE.                                ELXPMCGC
01195      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCGC
01196      MOVE KWA-GCCONTR-KEY TO IOP-FILE-KEY.                        ELXPMCGC
01197      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCGC
01198      SET IOP-RD            TO TRUE.                               ELXPMCGC
01199      SET IOP-FCQ-NONE      TO TRUE.                               ELXPMCGC
01200      SET IOP-KVQ-EQ        TO TRUE.                               ELXPMCGC
01201      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCGC
01202      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCGC
01203      IF PROCESSING-BASIC                                          ELXPMCGC
01204         IF IOP-RC-OK                                              ELXPMCGC
01205            SET ADDRESS OF GCT-GCCONTRC-RECORD                     ELXPMCGC
01206                        TO IOP-REC-PTR                             ELXPMCGC
01207            SET CIA-ELSCONIB-DDN  TO TRUE                          ELXPMCGC
01208            CALL 'ELUSAVAD' USING DFHCOMMAREA                      ELXPMCGC
01209                               IOP-REC-PTR                         ELXPMCGC
01210            SET CIA-ELSCONPB-DDN  TO TRUE                          ELXPMCGC
01211            CALL 'ELUSAVAD' USING DFHCOMMAREA                      ELXPMCGC
01212                               IOP-REC-PTR                         ELXPMCGC
01213            SET IOP-REC-PTR TO NULL                                ELXPMCGC
01214         ELSE                                                      ELXPMCGC
01215            MOVE 06 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCGC
01216            SET PMCI-BC-NO-CONTRACT TO TRUE                        ELXPMCGC
01217            IF GCT-INTER-REL-CD = ZEROS                            ELXPMCGC
01218               MOVE 07 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCGC
01219               SET PMCI-BC-NO-CONTRACT TO TRUE.                    ELXPMCGC
01220      IF PROCESSING-MAJOR-MED                                      ELXPMCGC
01221         IF IOP-RC-OK                                              ELXPMCGC
01222            SET ADDRESS OF GCT-GCCONTRC-RECORD                     ELXPMCGC
01223                         TO IOP-REC-PTR                            ELXPMCGC
01224            SET CIA-ELSCONIS-DDN  TO TRUE                          ELXPMCGC
01225            CALL 'ELUSAVAD' USING DFHCOMMAREA                      ELXPMCGC
01226                                IOP-REC-PTR                        ELXPMCGC
01227            SET CIA-ELSCONPS-DDN  TO TRUE                          ELXPMCGC
01228            CALL 'ELUSAVAD' USING DFHCOMMAREA                      ELXPMCGC
01229                              IOP-REC-PTR                          ELXPMCGC
01230            SET IOP-REC-PTR TO NULL                                ELXPMCGC
01231         ELSE                                                      ELXPMCGC
01232            MOVE 28 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCGC
01233            SET PMCI-BC-NO-CONTRACT TO TRUE                        ELXPMCGC
01234            IF GCT-INTER-REL-CD = ZEROS                            ELXPMCGC
01235               MOVE 29 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCGC
01236               SET PMCI-BC-NO-CONTRACT TO TRUE.                    ELXPMCGC
01237 ********START TEMP CODE FOR ACP*****************************      ELXPMCGC
01238         IF IOP-RC-OK AND NOT PMCI-BC-NO-CONTRACT                  ELXPMCGC
01239            SET PMCI-ACCUM-CO-PAY-NA TO TRUE                       ELXPMCGC
01240            PERFORM VARYING GCT-TAB-INDEX FROM 1 BY 1              ELXPMCGC
01241               UNTIL GCT-CON-TAB-ID(GCT-TAB-INDEX) >               ELXPMCGC
01242                '#ACP  ' OR GCT-TAB-INDEX > 18                     ELXPMCGC
01243                   IF GCT-CON-TAB-ID(GCT-TAB-INDEX) = '#ACP  '     ELXPMCGC
01244                     AND GCT-CON-TAB-SLOT(GCT-TAB-INDEX) > 0       ELXPMCGC
01245                       SET PMCI-ACCUM-CO-PAY-CALL TO TRUE          ELXPMCGC
01246                   END-IF                                          ELXPMCGC
01247            END-PERFORM                                            ELXPMCGC
01248         END-IF.                                                   ELXPMCGC
01249 ********END   TEMP CODE FOR ACP*****************************      ELXPMCGC
01250                                                                   ELXPMCGC
01251 ************************************************************      ELXPMCGC
01252 *                                                          *      ELXPMCGC
01253 * 7300-  CHECK FOR SUPPLEMENTAL MEDICARE PRODUCT           *      ELXPMCGC
01254 *                                                          *      ELXPMCGC
01255 ************************************************************      ELXPMCGC
01256  7300-CHECK-SUPP-MED.                                             ELXPMCGC
01257                                                                   ELXPMCGC
01258      MOVE GCT-MCARE-TYPE-IND TO WS-MCARE-IND.                     ELXPMCGC
01259      IF SUPP-PRODUCT                                              ELXPMCGC
01260         AND GCT-TAB2-SLOT-NO NOT EQUAL ZEROS                      ELXPMCGC
01261         MOVE 'Y' TO PMCI-SUPP-MED                                 ELXPMCGC
01262         MOVE GCT-TAB2-SLOT-NO TO PMCI-ABM-SLOT                    ELXPMCGC
01263                                                                   ELXPMCGC
01264         EVALUATE TRUE                                             ELXPMCGC
01265             WHEN SUPP-DED-COV                                     ELXPMCGC
01266                  MOVE 'Y' TO PMCI-PARTB-ANN-DED                   ELXPMCGC
01267             WHEN SUPP-DED-NOT-COV                                 ELXPMCGC
01268                  MOVE 'N' TO PMCI-PARTB-ANN-DED                   ELXPMCGC
01269             WHEN OTHER                                            ELXPMCGC
01270                  MOVE 'C' TO PMCI-PARTB-ANN-DED                   ELXPMCGC
01271         END-EVALUATE.                                             ELXPMCGC
01272                                                                   ELXPMCGC
01273 ************************************************************      ELXPMCGC
01274 * 7400-  CHECK FOR SUPPLEMENTAL MEDICARE                   *      ELXPMCGC
01275 *        BCBS DEDUCTIBLE. (THERE WILL BE AN ADL TABULAR)   *      ELXPMCGC
01276 *                                                          *      ELXPMCGC
01277 ************************************************************      ELXPMCGC
01278  7400-CK-BCBS-DED.                                                ELXPMCGC
01279                                                                   ELXPMCGC
01280      IF (GCG-TAB-ID (SUB1) = '#ADL  '                             ELXPMCGC
01281           AND GCG-TAB-SLOT-NO (SUB1) > 0 )                        ELXPMCGC
01282         MOVE 'Y' TO PMCI-PARTB-BCBS-DED.                          ELXPMCGC
01283 ************************************************************      ELXPMCGC
01284 *                                                          *      ELXPMCGC
01285 *        CALL TABULAR READ ROUTINE                         *      ELXPMCGC
01286 *                                                          *      ELXPMCGC
01287 ************************************************************      ELXPMCGC
01288  8000-CALL-TABULAR-READ-ROUTINE.                                  ELXPMCGC
01289                                                                   ELXPMCGC
01290      IF PMCI-INSTITUTIONAL                                        ELXPMCGC
01291          SET NAES-TAB-INDX TO 1                                   ELXPMCGC
01292      ELSE                                                         ELXPMCGC
01293          SET NAES-TAB-INDX TO 4.                                  ELXPMCGC
01294      MOVE WS-TAB-RIND1 TO NAES-TABS-READ-IND (NAES-TAB-INDX).     ELXPMCGC
01295      SET NAES-TAB-INDX UP BY 1                                    ELXPMCGC
01296      MOVE WS-TAB-RIND2 TO NAES-TABS-READ-IND (NAES-TAB-INDX).     ELXPMCGC
01297      SET NAES-TAB-INDX UP BY 1                                    ELXPMCGC
01298      MOVE WS-TAB-RIND3 TO NAES-TABS-READ-IND (NAES-TAB-INDX).     ELXPMCGC
01299      MOVE SAV-TYP TO NAES-PRV-TYPE.                               ELXPMCGC
01300      MOVE PMCI-PROVIDER-NUMBER TO NAES-PROVIDER-NBR.              ELXPMCGC
01301 ***ANYTHING HERE???                                               ELXPMCGC
01302      MOVE CURRENT-DAY-CEN TO NAES-PRV-SELECT-DATE-CEN.            ELXPMCGC
01303 *** MAY BE CHANGES HERE                                           ELXPMCGC
01304 *                                                                 ELXPMCGC
01305      CALL 'ELXPMCTB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCGC
01306      IF CIA-RC-PTR-NULL                                           ELXPMCGC
01307         MOVE 17 TO PMCI-BLUE-CHIP-ERROR-CODE                      ELXPMCGC
01308         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCGC
01309      ELSE                                                         ELXPMCGC
01310         SET CIA-ELSPMCID-DDN TO TRUE                              ELXPMCGC
01311         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCGC
01312                  ADDRESS OF NAES-INTERMEDIATE-DATA                ELXPMCGC
01313         IF CIA-RC-PTR-NULL                                        ELXPMCGC
01314            MOVE 22 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCGC
01315            SET PMCI-BC-INTERNAL-ERROR TO TRUE.                    ELXPMCGC
01316                                                                   ELXPMCGC
01317 *    RGO 4/95. CORRECT WHAT IS CHECKED.                           ELXPMCGC
01318      IF NAES-TABS-RETN-CODE = 'Y'                                 ELXPMCGC
01319          MOVE NAES-TABS-IND1 TO WS-TAB-IND1                       ELXPMCGC
01320          MOVE NAES-TABS-IND2 TO WS-TAB-IND2.                      ELXPMCGC
01321                                                                   ELXPMCGC
01322 ************************************************************      ELXPMCGC
01323 *                                                          *      ELXPMCGC
01324 *        CALL STORAGE MANAGER                              *      ELXPMCGC
01325 *                                                          *      ELXPMCGC
01326 ************************************************************      ELXPMCGC
01327  9000-CALL-STORAGE-MANAGER.                                       ELXPMCGC
01328                                                                   ELXPMCGC
01329      MOVE ZERO TO CIA-AREA-LEN.                                   ELXPMCGC
01330      SET CIA-STG-GETMAIN TO TRUE.                                 ELXPMCGC
01331      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCGC
