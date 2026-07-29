00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTTHER2.                                            ELTTHER2
00003  AUTHOR. BOB OEHMEN, COPIED FROM ELTTHERP.                           LV001
00004  DATE-WRITTEN.   1/05/96.                                         ELTTHER2
00005  DATE-COMPILED.                                                   ELTTHER2
00006      SKIP3                                                        ELTTHER2
00007 ******************************************************************ELTTHER2
00008 *@>ELTTHER2                                                       ELTTHER2
00009 *@¬                                                               ELTTHER2
00010 *                        PROGRAM ABSTRACT                         ELTTHER2
00011 *                                                                 ELTTHER2
00012 *@¬ PROGRAM NAME:   E.L.S. THERAPY BENEFITS                       ELTTHER2
00013 *@¬                                                               ELTTHER2
00014 *@¬ PROGRAM I.D.:   ELTTHER2                                      ELTTHER2
00015 *@¬                                                               ELTTHER2
00016 *@¬ PURPOSE:   LINK'ED FROM ELTTHERP, THIS PROGRAM TAKES CARE OF  ELTTHER2
00017 *@¬            CARDIAC, RADIATION AND SPEECH THERAPY, WHICH HAVE  ELTTHER2
00018 *@¬            SPLIT OFF FROM BEING PROCESSED IN ELTTHERP.  THAT'SELTTHER2
00019 *@¬            SO THAT IT DOESN'T GET TOO BIG.                    ELTTHER2
00020 *@¬                                                               ELTTHER2
00021 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF THERAPY COVERAGE  ELTTHER2
00022 *@¬            AFFORDED A MEMBER BY HIS GROUP.  THIS INFORMATION  ELTTHER2
00023 *@¬            IS GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS   ELTTHER2
00024 *@¬            FOR THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR ELTTHER2
00025 *@¬            RANGE OF DATES.                                    ELTTHER2
00026 *              THIS PROGRAM WORKS WITH ALL THE SUB-TOPICS OF      ELTTHER2
00027 *              THERAPIES.                                         ELTTHER2
00028 *                                                                 ELTTHER2
00029 *@¬ RECORDS                                                       ELTTHER2
00030 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT, BENEFIT PROVISION FORMAT ELTTHER2
00031 *@¬          A, B, AND E.                                         ELTTHER2
00032 *@¬                                                               ELTTHER2
00033 *@¬ PROCESSING                                                    ELTTHER2
00034 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTTHER2
00035 *@¬                                                               ELTTHER2
00036 *@¬                                                               ELTTHER2
00037 ***************************************************************** ELTTHER2
00038 *                                                                 ELTTHER2
00039 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTTHER2
00040 *          *-*       U P D A T E   H I S T O R Y       *-*        ELTTHER2
00041 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTTHER2
00042 *                                                                 ELTTHER2
00043 **-CHG NUM-* *-DATE-* *WHO* *------DESCRIPTION------------------- ELTTHER2
00044 *                                                                 ELTTHER2
00045 *  RGO 2/27  UPDATED FOR 1 BUG (THE PRIOR ADMISSION RESTRITION)   ELTTHER2
00046 *            AND 1 FIX (ADD TREATMENT RESTRICTION INDICATOR).     ELTTHER2
00047 *            NOTE, THESE COMMENTS WERE MADE 3/8/96                ELTTHER2
00048 ***************************************************************** ELTTHER2
00049                                                                   ELTTHER2
00050 /                                                                 ELTTHER2
00051  ENVIRONMENT DIVISION.                                            ELTTHER2
00052      SKIP3                                                        ELTTHER2
00053  DATA DIVISION.                                                   ELTTHER2
00054  WORKING-STORAGE SECTION.                                         ELTTHER2
00055  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTTHER2
00056      '***ELTTHER2 WS BEGINS***'.                                  ELTTHER2
00057 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTTHER2
00058  01  WS-WORK-FIELDS.                                              ELTTHER2
00059      05  WS-HEX-00                     PIC X.                     ELTTHER2
00060      05  WS-CHAR-0                     PIC X.                     ELTTHER2
00061      05  WS-YES                        PIC X(01) VALUE 'Y'.       ELTTHER2
00062      05  WS-NO                         PIC X(01) VALUE 'N'.       ELTTHER2
00063      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTTHER2
00064      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTTHER2
00065      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTTHER2
00066      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTTHER2
00067      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTTHER2
00068      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTTHER2
00069      05  WS-FIRSTTIME-IND              PIC X.                     ELTTHER2
00070        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTTHER2
00071      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTTHER2
00072        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTTHER2
00073      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTTHER2
00074                                                                   ELTTHER2
00075                                                                   ELTTHER2
00076  01  SWITCHES.                                                    ELTTHER2
00077      05  WS-SUB-CMF                    PIC S999  COMP-3 VALUE +0. ELTTHER2
00078      05  DISPLAY-BAS-SUP         PIC X(01) VALUE 'N'.             ELTTHER2
00079      05  CALL-ELUOUTPT-IND       PIC X(01).                       ELTTHER2
00080          88  YES-CALL-ELUOUTPT              VALUE 'Y'.            ELTTHER2
00081      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTTHER2
00082          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTTHER2
00083                                                                   ELTTHER2
00084  01  FIRST-CODE-LINE.                                             ELTTHER2
00085      05  FIRST-CODE-CHAR79       PIC X(79).                       ELTTHER2
00086      05  FIRST-CODE-BROKE-UP   REDEFINES FIRST-CODE-CHAR79.       ELTTHER2
00087          10  FIRST-CHAR          PIC X(1).                        ELTTHER2
00088              88  FIRST-CHAR-SHOW-AS-IS  VALUE QUOTE.              ELTTHER2
00089          10  FIRST-THE-REST      PIC X(78).                       ELTTHER2
00090                                                                   ELTTHER2
00091 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTTHER2
00092  01  TABLE-MAX                   PIC S9(03) VALUE +20 COMP.       ELTTHER2
00093 * 9  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTTHER2
00094                                                                   ELTTHER2
00095  01  WS-BEN-PROV-ID.                                              ELTTHER2
00096                                                                   ELTTHER2
00097 ***************************************************************** ELTTHER2
00098 * RADIATION  THERAPY                                              ELTTHER2
00099 ***************************************************************** ELTTHER2
00100      05  WS-CHEMO-IP-INST-CNT          PIC S999 COMP-3  VALUE +12.ELTTHER2
00101      05  WS-CHEMO-IP-INST-TAB.                                    ELTTHER2
00102        10  FILLER                      PIC X(6)  VALUE 'BRRI B'.  ELTTHER2
00103        10  FILLER                      PIC X(6)  VALUE 'BRTI B'.  ELTTHER2
00104        10  FILLER                      PIC X(6)  VALUE 'BXTI B'.  ELTTHER2
00105        10  FILLER                      PIC X(6)  VALUE 'DXTI B'.  ELTTHER2
00106        10  FILLER                      PIC X(6)  VALUE 'MDRI B'.  ELTTHER2
00107        10  FILLER                      PIC X(6)  VALUE 'MRTI B'.  ELTTHER2
00108        10  FILLER                      PIC X(6)  VALUE 'RITI B'.  ELTTHER2
00109        10  FILLER                      PIC X(6)  VALUE 'RMSI B'.  ELTTHER2
00110        10  FILLER                      PIC X(6)  VALUE 'RRTI B'.  ELTTHER2
00111        10  FILLER                      PIC X(6)  VALUE 'RTPI B'.  ELTTHER2
00112        10  FILLER                      PIC X(6)  VALUE 'SMRI B'.  ELTTHER2
00113        10  FILLER                      PIC X(6)  VALUE 'SXTI B'.  ELTTHER2
00114      05  WS-CHEMO-IP-INST-LIST   REDEFINES   WS-CHEMO-IP-INST-TAB ELTTHER2
00115                                        PIC X(6)  OCCURS 12 TIMES. ELTTHER2
00116                                                                   ELTTHER2
00117      05  WS-CHEMO-OP-INST-CNT          PIC S999 COMP-3  VALUE +12.ELTTHER2
00118      05  WS-CHEMO-OP-INST-TAB.                                    ELTTHER2
00119        10  FILLER                      PIC X(6)  VALUE 'BRRO B'.  ELTTHER2
00120        10  FILLER                      PIC X(6)  VALUE 'BRTO B'.  ELTTHER2
00121        10  FILLER                      PIC X(6)  VALUE 'BXTO B'.  ELTTHER2
00122        10  FILLER                      PIC X(6)  VALUE 'DXTO B'.  ELTTHER2
00123        10  FILLER                      PIC X(6)  VALUE 'MDRO B'.  ELTTHER2
00124        10  FILLER                      PIC X(6)  VALUE 'MRTO B'.  ELTTHER2
00125        10  FILLER                      PIC X(6)  VALUE 'RITO B'.  ELTTHER2
00126        10  FILLER                      PIC X(6)  VALUE 'RMSO B'.  ELTTHER2
00127        10  FILLER                      PIC X(6)  VALUE 'RRTO B'.  ELTTHER2
00128        10  FILLER                      PIC X(6)  VALUE 'RTPO B'.  ELTTHER2
00129        10  FILLER                      PIC X(6)  VALUE 'SMRO B'.  ELTTHER2
00130        10  FILLER                      PIC X(6)  VALUE 'SXTO B'.  ELTTHER2
00131      05  WS-CHEMO-OP-INST-LIST   REDEFINES   WS-CHEMO-OP-INST-TAB ELTTHER2
00132                                        PIC X(6)  OCCURS 12 TIMES. ELTTHER2
00133                                                                   ELTTHER2
00134      05  WS-CHEMO-IP-PROF-CNT          PIC S999 COMP-3  VALUE 10. ELTTHER2
00135      05  WS-CHEMO-IP-PROF-TAB.                                    ELTTHER2
00136        10  FILLER                      PIC X(6)  VALUE 'BRRI E'.  ELTTHER2
00137        10  FILLER                      PIC X(6)  VALUE 'BRTI E'.  ELTTHER2
00138        10  FILLER                      PIC X(6)  VALUE 'BXTI E'.  ELTTHER2
00139        10  FILLER                      PIC X(6)  VALUE 'DXTI E'.  ELTTHER2
00140        10  FILLER                      PIC X(6)  VALUE 'MDRI E'.  ELTTHER2
00141        10  FILLER                      PIC X(6)  VALUE 'MRTI E'.  ELTTHER2
00142        10  FILLER                      PIC X(6)  VALUE 'RMSI E'.  ELTTHER2
00143        10  FILLER                      PIC X(6)  VALUE 'RTPI E'.  ELTTHER2
00144        10  FILLER                      PIC X(6)  VALUE 'SMRI E'.  ELTTHER2
00145        10  FILLER                      PIC X(6)  VALUE 'SXTI E'.  ELTTHER2
00146      05  WS-CHEMO-IP-PROF-LIST   REDEFINES   WS-CHEMO-IP-PROF-TAB ELTTHER2
00147                                        PIC X(6)  OCCURS 10 TIMES. ELTTHER2
00148                                                                   ELTTHER2
00149      05  WS-CHEMO-OP-PROF-CNT          PIC S999 COMP-3  VALUE 10. ELTTHER2
00150      05  WS-CHEMO-OP-PROF-TAB.                                    ELTTHER2
00151        10  FILLER                      PIC X(6)  VALUE 'BRRO E'.  ELTTHER2
00152        10  FILLER                      PIC X(6)  VALUE 'BRTO E'.  ELTTHER2
00153        10  FILLER                      PIC X(6)  VALUE 'BXTO E'.  ELTTHER2
00154        10  FILLER                      PIC X(6)  VALUE 'DXTO E'.  ELTTHER2
00155        10  FILLER                      PIC X(6)  VALUE 'MDRO E'.  ELTTHER2
00156        10  FILLER                      PIC X(6)  VALUE 'MRTO E'.  ELTTHER2
00157        10  FILLER                      PIC X(6)  VALUE 'RMSO E'.  ELTTHER2
00158        10  FILLER                      PIC X(6)  VALUE 'RTPO E'.  ELTTHER2
00159        10  FILLER                      PIC X(6)  VALUE 'SMRO E'.  ELTTHER2
00160        10  FILLER                      PIC X(6)  VALUE 'SXTO E'.  ELTTHER2
00161      05  WS-CHEMO-OP-PROF-LIST   REDEFINES   WS-CHEMO-OP-PROF-TAB ELTTHER2
00162                                        PIC X(6)  OCCURS 10 TIMES. ELTTHER2
00163                                                                   ELTTHER2
00164 ***************************************************************** ELTTHER2
00165 *   SPEECH   THERAPY                                              ELTTHER2
00166 ***************************************************************** ELTTHER2
00167      05  WS-SPEECH-IP-INST-CNT         PIC S999 COMP-3  VALUE +1. ELTTHER2
00168      05  WS-SPEECH-IP-INST-TAB.                                   ELTTHER2
00169        10  FILLER                      PIC X(6)  VALUE 'SPTI B'.  ELTTHER2
00170      05  WS-SPEECH-IP-INST-LIST REDEFINES   WS-SPEECH-IP-INST-TAB ELTTHER2
00171                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00172                                                                   ELTTHER2
00173      05  WS-SPEECH-OP-INST-CNT         PIC S999 COMP-3  VALUE +1. ELTTHER2
00174      05  WS-SPEECH-OP-INST-TAB.                                   ELTTHER2
00175        10  FILLER                      PIC X(6)  VALUE 'SPTO B'.  ELTTHER2
00176      05  WS-SPEECH-OP-INST-LIST REDEFINES   WS-SPEECH-OP-INST-TAB ELTTHER2
00177                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00178                                                                   ELTTHER2
00179      05  WS-SPEECH-IP-PROF-CNT         PIC S999 COMP-3  VALUE +1. ELTTHER2
00180      05  WS-SPEECH-IP-PROF-TAB.                                   ELTTHER2
00181        10  FILLER                      PIC X(6)  VALUE 'SPTI E'.  ELTTHER2
00182      05  WS-SPEECH-IP-PROF-LIST REDEFINES   WS-SPEECH-IP-PROF-TAB ELTTHER2
00183                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00184                                                                   ELTTHER2
00185      05  WS-SPEECH-OP-PROF-CNT         PIC S999 COMP-3  VALUE +1. ELTTHER2
00186      05  WS-SPEECH-OP-PROF-TAB.                                   ELTTHER2
00187        10  FILLER                      PIC X(6)  VALUE 'SPTO E'.  ELTTHER2
00188      05  WS-SPEECH-OP-PROF-LIST REDEFINES   WS-SPEECH-OP-PROF-TAB ELTTHER2
00189                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00190                                                                   ELTTHER2
00191 ***************************************************************** ELTTHER2
00192 * CARDIAC    THERAPY                                              ELTTHER2
00193 ***************************************************************** ELTTHER2
00194      05  WS-CARD-IP-INST-CNT           PIC S999 COMP-3  VALUE +1. ELTTHER2
00195      05  WS-CARD-IP-INST-TAB.                                     ELTTHER2
00196        10  FILLER                      PIC X(6)  VALUE 'CRPI B'.  ELTTHER2
00197      05  WS-CARD-IP-INST-LIST   REDEFINES   WS-CARD-IP-INST-TAB   ELTTHER2
00198                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00199                                                                   ELTTHER2
00200      05  WS-CARD-OP-INST-CNT           PIC S999 COMP-3  VALUE +1. ELTTHER2
00201      05  WS-CARD-OP-INST-TAB.                                     ELTTHER2
00202        10  FILLER                      PIC X(6)  VALUE 'CRPO B'.  ELTTHER2
00203      05  WS-CARD-OP-INST-LIST   REDEFINES   WS-CARD-OP-INST-TAB   ELTTHER2
00204                                        PIC X(6)  OCCURS  1 TIMES. ELTTHER2
00205                                                                   ELTTHER2
00206 * RGO. CARDIAC DOES NOT HAVE PROFESSIONAL. START DELETE.          ELTTHER2
00207      05  WS-CARD-IP-PROF-CNT           PIC S999 COMP-3  VALUE +4. ELTTHER2
00208      05  WS-CARD-IP-PROF-TAB.                                     ELTTHER2
00209        10  FILLER                      PIC X(6)  VALUE 'DRTI B'.  ELTTHER2
00210        10  FILLER                      PIC X(6)  VALUE 'DTI  B'.  ELTTHER2
00211        10  FILLER                      PIC X(6)  VALUE 'IHI  B'.  ELTTHER2
00212        10  FILLER                      PIC X(6)  VALUE 'MPTI D'.  ELTTHER2
00213      05  WS-CARD-IP-PROF-LIST   REDEFINES   WS-CARD-IP-PROF-TAB   ELTTHER2
00214                                        PIC X(6)  OCCURS  4 TIMES. ELTTHER2
00215                                                                   ELTTHER2
00216      05  WS-CARD-OP-PROF-CNT           PIC S999 COMP-3  VALUE +6. ELTTHER2
00217      05  WS-CARD-OP-PROF-TAB.                                     ELTTHER2
00218        10  FILLER                      PIC X(6)  VALUE 'DRTO E'.  ELTTHER2
00219        10  FILLER                      PIC X(6)  VALUE 'DTO  E'.  ELTTHER2
00220        10  FILLER                      PIC X(6)  VALUE 'EAIH E'.  ELTTHER2
00221        10  FILLER                      PIC X(6)  VALUE 'EMIH E'.  ELTTHER2
00222        10  FILLER                      PIC X(6)  VALUE 'IHO  E'.  ELTTHER2
00223        10  FILLER                      PIC X(6)  VALUE 'MPTO E'.  ELTTHER2
00224      05  WS-CARD-OP-PROF-LIST   REDEFINES   WS-CARD-OP-PROF-TAB   ELTTHER2
00225                                        PIC X(6)  OCCURS  6 TIMES. ELTTHER2
00226 * RGO. CARDIAC DOES NOT HAVE PROFESSIONAL. END   DELETE.          ELTTHER2
00227 /          D I S P L A Y   L I N E S                              ELTTHER2
00228  01  WS-ELS-DISPLAY-LINES.                                        ELTTHER2
00229    05  WS-HDR-2-PHYS-IP-INST.                                     ELTTHER2
00230      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHER2
00231      10  FILLER                    PIC X(49)                      ELTTHER2
00232         VALUE 'PHYSICAL THERAPY SERVICES INPATIENT INSTITUTIONAL'.ELTTHER2
00233      10  FILLER                    PIC X(15) VALUE LOW-VALUES.    ELTTHER2
00234                                                                   ELTTHER2
00235    05  WS-HDR-2-PHYS-OP-INST.                                     ELTTHER2
00236      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHER2
00237      10  FILLER                    PIC X(50)                      ELTTHER2
00238        VALUE 'PHYSICAL THERAPY SERVICES OUTPATIENT INSTITUTIONAL'.ELTTHER2
00239      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTTHER2
00240                                                                   ELTTHER2
00241    05  WS-HDR-2-PHYS-IP-PROF.                                     ELTTHER2
00242      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHER2
00243      10  FILLER                    PIC X(48)                      ELTTHER2
00244         VALUE 'PHYSICAL THERAPY SERVICES INPATIENT PROFESSIONAL'. ELTTHER2
00245      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTTHER2
00246                                                                   ELTTHER2
00247    05  WS-HDR-2-PHYS-OP-PROF.                                     ELTTHER2
00248      10  FILLER                    PIC X(15) VALUE SPACES.        ELTTHER2
00249      10  FILLER                    PIC X(49)                      ELTTHER2
00250        VALUE 'PHYSICAL THERAPY SERVICES OUTPATIENT PROFESSIONAL'. ELTTHER2
00251      10  FILLER                    PIC X(15) VALUE LOW-VALUES.    ELTTHER2
00252                                                                   ELTTHER2
00253    05  WS-HDR-2-CHEMO-IP-INST.                                    ELTTHER2
00254      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHER2
00255      10  FILLER                    PIC X(46)                      ELTTHER2
00256            VALUE 'RADIATION INPATIENT INSTITUTIONAL'.             ELTTHER2
00257      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHER2
00258                                                                   ELTTHER2
00259    05  WS-HDR-2-CHEMO-OP-INST.                                    ELTTHER2
00260      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHER2
00261      10  FILLER                    PIC X(47)                      ELTTHER2
00262           VALUE 'RADIATION OUTPATIENT INSTITUTIONAL'.             ELTTHER2
00263      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTTHER2
00264                                                                   ELTTHER2
00265    05  WS-HDR-2-CHEMO-IP-PROF.                                    ELTTHER2
00266      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHER2
00267      10  FILLER                    PIC X(45)                      ELTTHER2
00268             VALUE 'RADIATION INPATIENT PROFESSIONAL'.             ELTTHER2
00269      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHER2
00270                                                                   ELTTHER2
00271    05  WS-HDR-2-CHEMO-OP-PROF.                                    ELTTHER2
00272      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHER2
00273      10  FILLER                    PIC X(46)                      ELTTHER2
00274            VALUE 'RADIATION OUTPATIENT PROFESSIONAL'.             ELTTHER2
00275      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHER2
00276                                                                   ELTTHER2
00277    05  WS-HDR-2-SPEECH-IP-INST.                                   ELTTHER2
00278      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHER2
00279      10  FILLER                    PIC X(39)                      ELTTHER2
00280                VALUE 'SPEECH THERAPY INPATIENT INSTITUTIONAL'.    ELTTHER2
00281      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTTHER2
00282                                                                   ELTTHER2
00283    05  WS-HDR-2-SPEECH-OP-INST.                                   ELTTHER2
00284      10  FILLER                    PIC X(20) VALUE SPACES.        ELTTHER2
00285      10  FILLER                    PIC X(40)                      ELTTHER2
00286                VALUE 'SPEECH THERAPY OUTPATIENT INSTITUTIONAL'.   ELTTHER2
00287      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTTHER2
00288                                                                   ELTTHER2
00289    05  WS-HDR-2-SPEECH-IP-PROF.                                   ELTTHER2
00290      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHER2
00291      10  FILLER                    PIC X(38)                      ELTTHER2
00292                 VALUE 'SPEECH THERAPY INPATIENT PROFESSIONAL'.    ELTTHER2
00293      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTTHER2
00294                                                                   ELTTHER2
00295    05  WS-HDR-2-SPEECH-OP-PROF.                                   ELTTHER2
00296      10  FILLER                    PIC X(21) VALUE SPACES.        ELTTHER2
00297      10  FILLER                    PIC X(39)                      ELTTHER2
00298                VALUE 'SPEECH THERAPY OUTPATIENT PROFESSIONAL'.    ELTTHER2
00299      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTTHER2
00300                                                                   ELTTHER2
00301    05  WS-HDR-2-CARD-IP-INST.                                     ELTTHER2
00302      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHER2
00303      10  FILLER                    PIC X(45)                      ELTTHER2
00304             VALUE 'CARDIAC THERAPY INPATIENT INSTITUTIONAL'.      ELTTHER2
00305      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHER2
00306                                                                   ELTTHER2
00307    05  WS-HDR-2-CARD-OP-INST.                                     ELTTHER2
00308      10  FILLER                    PIC X(16) VALUE SPACES.        ELTTHER2
00309      10  FILLER                    PIC X(46)                      ELTTHER2
00310            VALUE 'CARDIAC THERAPY OUTPATIENT INSTITUTIONAL'.      ELTTHER2
00311      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTTHER2
00312                                                                   ELTTHER2
00313 * RGO. CARDIAC DOES NOT HAVE PROFESSIONAL. START DELETE.          ELTTHER2
00314    05  WS-HDR-2-CARD-IP-PROF.                                     ELTTHER2
00315      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHER2
00316      10  FILLER                    PIC X(44)                      ELTTHER2
00317             VALUE 'MISCELLANEOUS THERAPY INPATIENT PROFESSIONAL'. ELTTHER2
00318      10  FILLER                    PIC X(18) VALUE LOW-VALUES.    ELTTHER2
00319                                                                   ELTTHER2
00320    05  WS-HDR-2-CARD-OP-PROF.                                     ELTTHER2
00321      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHER2
00322      10  FILLER                    PIC X(43)                      ELTTHER2
00323             VALUE 'CARDIAC THERAPY PROFESSIONAL'.                 ELTTHER2
00324      10  FILLER                    PIC X(17) VALUE SPACES.        ELTTHER2
00325    05  WS-NO-COVERAGE.                                            ELTTHER2
00326      10  FILLER                    PIC X(02) VALUE SPACES.        ELTTHER2
00327      10  FILLER                    PIC X(60)                      ELTTHER2
00328                VALUE 'PROFESSIONAL BENEFITS ARE NOT COVERED'.     ELTTHER2
00329                                                                   ELTTHER2
00330 * RGO. CARDIAC DOES NOT HAVE PROFESSIONAL. END T DELETE.          ELTTHER2
00331                                                                   ELTTHER2
00332    05  WS-PHYS-THERAPY-SERVICES    PIC X(30)                      ELTTHER2
00333                            VALUE 'PHYSICAL THERAPY SERVICES:'.    ELTTHER2
00334                                                                   ELTTHER2
00335    05  WS-CHEMO-IP-SERVICES        PIC X(46)                      ELTTHER2
00336            VALUE 'RADIATION INPATIENT SERVICES:'.                 ELTTHER2
00337                                                                   ELTTHER2
00338    05  WS-CHEMO-OP-SERVICES        PIC X(47)                      ELTTHER2
00339           VALUE 'RADIATION OUTPATIENT SERVICES:'.                 ELTTHER2
00340                                                                   ELTTHER2
00341    05  WS-SPEECH-IP-SERVICES       PIC X(38)                      ELTTHER2
00342               VALUE 'SPEECH THERAPY INPATIENT SERVICES:'.         ELTTHER2
00343                                                                   ELTTHER2
00344    05  WS-SPEECH-OP-SERVICES       PIC X(39)                      ELTTHER2
00345               VALUE 'SPEECH THERAPY OUTPATIENT SERVICES:'.        ELTTHER2
00346                                                                   ELTTHER2
00347    05  WS-CARD-SERVICES            PIC X(35)                      ELTTHER2
00348                       VALUE 'CARDIAC THERAPY SERVICES:'.          ELTTHER2
00349                                                                   ELTTHER2
00350    05  WS-COMPARISON-INJURY.                                      ELTTHER2
00351      10  FILLER                    PIC X(48)                      ELTTHER2
00352          VALUE 'A COMPARISON OF THE INJURY TO THE EFFECTIVE DATE'.ELTTHER2
00353      10  FILLER                    PIC X(11)                      ELTTHER2
00354          VALUE ' INDICATES:'.                                     ELTTHER2
00355      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTTHER2
00356                                                                   ELTTHER2
00357    05  WS-TREAT-RESTRN.                                           ELTTHER2
00358      10  FILLER                  PIC  X(44) VALUE                 ELTTHER2
00359        'THESE SERVICES ARE RESTRICTED AS FOLLOWS:  '.             ELTTHER2
00360                                                                   ELTTHER2
00361    05  WS-CERT-REQ.                                               ELTTHER2
00362      10  FILLER                  PIC  X(44) VALUE                 ELTTHER2
00363          'CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.          ELTTHER2
00364                                                                   ELTTHER2
00365    05  WS-SERVICES-RENDERED      PIC X(25)                        ELTTHER2
00366          VALUE 'SERVICES MAY BE RENDERED '.                       ELTTHER2
00367                                                                   ELTTHER2
00368    05  WS-FOLLOWING-BEN.                                          ELTTHER2
00369      10  FILLER                  PIC  X(21) VALUE                 ELTTHER2
00370            'COVERED SERVICES ARE:'.                               ELTTHER2
00371                                                                   ELTTHER2
00372    05  WS-PAYABLE-AS               PIC X(40) VALUE                ELTTHER2
00373          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTTHER2
00374                                                                   ELTTHER2
00375    05  WS-PAYMNT-BASED             PIC X(20)                      ELTTHER2
00376          VALUE 'PAYMENT IS BASED ON:'.                            ELTTHER2
00377                                                                   ELTTHER2
00378    05  WS-ELIG-METHOD-OF-TREAT     PIC X(35)                      ELTTHER2
00379                       VALUE 'THE ELIGIBLE METHOD OF TREATMENT IS'.ELTTHER2
00380                                                                   ELTTHER2
00381    05  WS-MAX-NUM-OF-VISITS        PIC X(32)                      ELTTHER2
00382                          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE'.ELTTHER2
00383                                                                   ELTTHER2
00384    05  WS-MAX-AMT-PER-VISIT        PIC X(28)                      ELTTHER2
00385                             VALUE 'THE MAXIMUM AMOUNT PER VISIT'. ELTTHER2
00386                                                                   ELTTHER2
00387    05  WS-PRIOR-ADM.                                              ELTTHER2
00388        10  FILLER                  PIC X(55) VALUE                ELTTHER2
00389            'PRIOR INPATIENT ADMISSION REQUIRED FOR THIS SERVICE ANELTTHER2
00390 -          'D'.                                                   ELTTHER2
00391                                                                   ELTTHER2
00392    05  WS-PROF-INPT-CHRGES.                                       ELTTHER2
00393        10  FILLER                  PIC  X(61) VALUE               ELTTHER2
00394            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTTHER2
00395 -          'EPORT: '.                                             ELTTHER2
00396        10  FILLER                  PIC  X(18) VALUE LOW-VALUES.   ELTTHER2
00397                                                                   ELTTHER2
00398    05  WS-PROF-OUTPT-CHRGES.                                      ELTTHER2
00399        10  FILLER                  PIC  X(62) VALUE               ELTTHER2
00400            'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE ELTTHER2
00401 -          'REPORT: '.                                            ELTTHER2
00402        10  FILLER                  PIC  X(17) VALUE LOW-VALUES.   ELTTHER2
00403                                                                   ELTTHER2
00404    05  WS-HOLD-PROF-CHRG-MSG       PIC  X(79) VALUE LOW-VALUES.   ELTTHER2
00405                                                                   ELTTHER2
00406    05  WS-SAME-PROVIDER-RADIATN    PIC X(57)  VALUE               ELTTHER2
00407       'IF THE SAME PROVIDER IS BILLING RADIATION THERAPY/MEDICAL'.ELTTHER2
00408 *   |9876+4321|9876+4321|9876+4321|9876+4321|9876+4321|9876+4321| ELTTHER2
00409 *   6         5         4         3         2         1           ELTTHER2
00410 *  WS-MAX-DAYS CREATED BY RGO, SO THAT THIS TOPIC LOOKS LIKE      ELTTHER2
00411 *    ELTOBREL.                                                    ELTTHER2
00412    05  WS-MAX-DAYS.                                               ELTTHER2
00413      10  FILLER                PIC X(01).                         ELTTHER2
00414      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTTHER2
00415      10  FILLER                PIC X(01).                         ELTTHER2
00416      10  WS-DAYS-LITERAL       PIC X(07) VALUE 'VISITS '.         ELTTHER2
00417      10  FILLER                PIC X(01).                         ELTTHER2
00418      10  WS-DTL-MAX-IND        PIC X(66).                         ELTTHER2
00419                                                                   ELTTHER2
00420                                                                   ELTTHER2
00421    05  WS-BASIC.                                                  ELTTHER2
00422      10  WS-BASIC-LIT              PIC X(07)                      ELTTHER2
00423         VALUE 'BASIC: '.                                          ELTTHER2
00424      10  WS-DTL-BASIC-LONG.                                       ELTTHER2
00425        15  WS-BASIC-AMOUNT         PIC ZZ9.99-.                   ELTTHER2
00426        15  FILLER                  PIC X(5) VALUE SPACES.         ELTTHER2
00427      10  FILLER       REDEFINES    WS-DTL-BASIC-LONG.             ELTTHER2
00428        15  WS-BASIC-DAYS           PIC ZZ9.                       ELTTHER2
00429        15  FILLER                  PIC X(9).                      ELTTHER2
00430                                                                   ELTTHER2
00431    05  WS-SUPPLEMENTAL.                                           ELTTHER2
00432      10  WS-SUPP-LIT               PIC X(16)                      ELTTHER2
00433          VALUE 'SUPPLEMENTAL: '.                                  ELTTHER2
00434      10  WS-DTL-SUPP-LONG.                                        ELTTHER2
00435        15  WS-SUPP-AMOUNT          PIC ZZ9.99-.                   ELTTHER2
00436        15  FILLER                  PIC X(5) VALUE SPACES.         ELTTHER2
00437      10  FILLER       REDEFINES    WS-DTL-SUPP-LONG.              ELTTHER2
00438        15  WS-SUPP-DAYS            PIC ZZ9.                       ELTTHER2
00439        15  FILLER                  PIC X(9).                      ELTTHER2
00440                                                                   ELTTHER2
00441    05  WS-BASIC-DAYS-REDUCT-1ST.                                  ELTTHER2
00442      10  FILLER                    PIC X(2) VALUE SPACES.         ELTTHER2
00443      10  FILLER                    PIC X(32)                      ELTTHER2
00444                          VALUE 'BASIC DAY REDUCTION RATIO BASIC '.ELTTHER2
00445      10  WS-BASIC-DAYS-1ST         PIC ZZ9.                       ELTTHER2
00446      10  FILLER                    PIC X(5)  VALUE ' FOR '.       ELTTHER2
00447      10  WS-BASIC-DAYS-FOR-1ST     PIC ZZ9.                       ELTTHER2
00448      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTTHER2
00449                                                                   ELTTHER2
00450    05  WS-BASIC-DAYS-REDUCT-2ND.                                  ELTTHER2
00451      10  FILLER                    PIC X(2) VALUE SPACES.         ELTTHER2
00452      10  FILLER                    PIC X(36)                      ELTTHER2
00453                      VALUE 'BASIC DAY REDUCTION RATIO SECONDARY '.ELTTHER2
00454      10  WS-BASIC-DAYS-2ND         PIC ZZ9.                       ELTTHER2
00455      10  FILLER                    PIC X(5)  VALUE ' FOR '.       ELTTHER2
00456      10  WS-BASIC-DAYS-FOR-2ND     PIC ZZ9.                       ELTTHER2
00457      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTTHER2
00458                                                                   ELTTHER2
00459    05  WS-FOR-BASIC                PIC X(13)                      ELTTHER2
00460         VALUE '  FOR BASIC: '.                                    ELTTHER2
00461                                                                   ELTTHER2
00462    05  WS-FOR-SUPP                 PIC X(20)                      ELTTHER2
00463         VALUE '  FOR SUPPLEMENTAL: '.                             ELTTHER2
00464                                                                   ELTTHER2
00465    05  WS-AND-MUST-BEGIN.                                         ELTTHER2
00466      10  FILLER                    PIC X(40)                      ELTTHER2
00467                  VALUE ' FOR THIS SERVICE AND MUST BEGIN WITHIN '.ELTTHER2
00468      10  WS-MUST-BEGIN-DAYS        PIC ZZ9.                       ELTTHER2
00469      10  FILLER                    PIC X(20)                      ELTTHER2
00470                                      VALUE ' DAYS FROM DISCHARGE'.ELTTHER2
00471                                                                   ELTTHER2
00472    05  WS-THE                PIC X(04)                            ELTTHER2
00473          VALUE 'THE:'.                                            ELTTHER2
00474    05  WS-BASIC-THE                PIC X(20)                      ELTTHER2
00475          VALUE '         BASIC: THE '.                            ELTTHER2
00476                                                                   ELTTHER2
00477    05  WS-SUPP-THE                 PIC X(20)                      ELTTHER2
00478          VALUE '  SUPPLEMENTAL: THE '.                            ELTTHER2
00479                                                                   ELTTHER2
00480    05  WS-BASIC-MAX.                                              ELTTHER2
00481      10  FILLER                    PIC X(09) VALUE SPACES.        ELTTHER2
00482      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTTHER2
00483      10  WS-BASIC-MAX-AMT          PIC $$$9.                      ELTTHER2
00484      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTTHER2
00485                                                                   ELTTHER2
00486    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTTHER2
00487        VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTTHER2
00488                                                                   ELTTHER2
00489    05  WS-SUPP-MAX.                                               ELTTHER2
00490      10  FILLER                    PIC X(16)                      ELTTHER2
00491          VALUE '  SUPPLEMENTAL: '.                                ELTTHER2
00492      10  WS-SUPP-MAX-AMT           PIC ZZ9.                       ELTTHER2
00493      10  FILLER                    PIC X(60) VALUE LOW-VALUES.    ELTTHER2
00494                                                                   ELTTHER2
00495    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER '. ELTTHER2
00496                                                                   ELTTHER2
00497    05  WS-MAXIMUM-AMT-PER.                                        ELTTHER2
00498      10  FILLER                    PIC X(29)                      ELTTHER2
00499        VALUE 'THE MAXIMUM AMOUNT PER VISIT '.                     ELTTHER2
00500      10  FILLER                    PIC X(50) VALUE LOW-VALUES.    ELTTHER2
00501                                                                   ELTTHER2
00502    05  WS-FOR-SUPP-ACCIDENT.                                      ELTTHER2
00503      10  FILLER                    PIC X(26)                      ELTTHER2
00504        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTTHER2
00505      10  WS-NO-DAYS                PIC ZZ9.                       ELTTHER2
00506      10  WS-FOR-SUPP-ACC-DESC      PIC X(50).                     ELTTHER2
00507                                                                   ELTTHER2
00508    05  WS-CONTRACT-RELATED.                                       ELTTHER2
00509      10  FILLER                    PIC X(49)   VALUE              ELTTHER2
00510           'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS.'.    ELTTHER2
00511                                                                   ELTTHER2
00512    05  WS-PROVIDER-ELIGIBILITY.                                   ELTTHER2
00513      10  FILLER                    PIC X(44)   VALUE              ELTTHER2
00514        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTTHER2
00515                                                                   ELTTHER2
00516    05  WS-ACCUM-MSG1.                                             ELTTHER2
00517      10  FILLER                  PIC  X(79) VALUE                 ELTTHER2
00518      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTTHER2
00519 -    'CONSIDERATIONS.'.                                           ELTTHER2
00520                                                                   ELTTHER2
00521    05  WS-NO-TABULAR1.                                            ELTTHER2
00522      10  FILLER                    PIC X(51)  VALUE               ELTTHER2
00523         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTTHER2
00524      10  FILLER                    PIC X(22)  VALUE               ELTTHER2
00525         'GOING FROM BENEFIT ***'.                                 ELTTHER2
00526                                                                   ELTTHER2
00527    05  WS-NO-TABULAR2.                                            ELTTHER2
00528      10  FILLER                    PIC X(15)  VALUE               ELTTHER2
00529         '*** PROVISION: '.                                        ELTTHER2
00530      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTTHER2
00531      10  FILLER                    PIC X VALUE SPACE.             ELTTHER2
00532      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTTHER2
00533      10  FILLER                    PIC X(13)  VALUE               ELTTHER2
00534         ' TO TABULAR: '.                                          ELTTHER2
00535      10  WS-NO-TAB-ID              PIC X(6).                      ELTTHER2
00536      10  FILLER                    PIC X VALUE SPACE.             ELTTHER2
00537      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTTHER2
00538      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTTHER2
00539                                                                   ELTTHER2
00540    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTTHER2
00541    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTTHER2
00542      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTTHER2
00543    05  WS-TEMP-TEXT-AREA2.                                        ELTTHER2
00544      10  WS-TEMP2-CHARS            PIC X(5)    VALUE LOW-VALUES.  ELTTHER2
00545      10  FILLER                    PIC X(74).                     ELTTHER2
00546                                                                   ELTTHER2
00547  01  WS-END                            PIC X(16)  VALUE           ELTTHER2
00548      '*** W/S ENDS ***'.                                          ELTTHER2
00549 /             L I N K A G E   S E C T I O N                       ELTTHER2
00550  LINKAGE SECTION.                                                 ELTTHER2
00551  01  DFHCOMMAREA.                                                 ELTTHER2
00552      COPY ELSCOMMC.                                               ELTTHER2
00553 /                                                                 ELTTHER2
00554      COPY ELSCIA2C.                                               ELTTHER2
00555 /                                                                 ELTTHER2
00556      COPY ELSIOPMC.                                               ELTTHER2
00557 /                                                                 ELTTHER2
00558      COPY ELSKEYSC.                                               ELTTHER2
00559 /                                                                 ELTTHER2
00560      COPY ELSOUTPC.                                               ELTTHER2
00561 /                                                                 ELTTHER2
00562      COPY ELSSSCBC.                                               ELTTHER2
00563 /                                                                 ELTTHER2
00564      COPY ELSCMIFC.                                               ELTTHER2
00565 /                                                                 ELTTHER2
00566      COPY ELSCMDSC.                                               ELTTHER2
00567 /                                                                 ELTTHER2
00568      COPY ELSPRVNC.                                               ELTTHER2
00569 /                                                                 ELTTHER2
00570 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTTHER2
00571      COPY ELSPLGSW.                                               ELTTHER2
00572 *** BENEFIT PROVISION TABLE OF FLDS                               ELTTHER2
00573      COPY ELSPLGTB.                                               ELTTHER2
00574 /                                                                 ELTTHER2
00575      COPY ELSTCWAC.                                               ELTTHER2
00576 /                                                                 ELTTHER2
00577 /                                                                 ELTTHER2
00578 /        G R O U P   S P E C I F I C   R E C O R D                ELTTHER2
00579  01  GROUP-SPECIFIC-RECORD.                                       ELTTHER2
00580      COPY GCGROUPC.                                               ELTTHER2
00581 /        C O N T R A C T   R E C O R D                            ELTTHER2
00582  01  CONTRACT-RECORD.                                             ELTTHER2
00583      COPY GCCONTRC.                                               ELTTHER2
00584 /                  M A I N L I N E                                ELTTHER2
00585  PROCEDURE DIVISION.                                              ELTTHER2
00586                                                                   ELTTHER2
00587 ******************************************************************ELTTHER2
00588 *                                                                 ELTTHER2
00589 *   PERFORM THE MAINLINE OPERATIONS.                              ELTTHER2
00590 *                                                                 ELTTHER2
00591 ******************************************************************ELTTHER2
00592  0000-MAINLINE.                                                   ELTTHER2
00593 ****************************************************************  ELTTHER2
00594 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTTHER2
00595 ****************************************************************  ELTTHER2
00596                                                                   ELTTHER2
00597      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTTHER2
00598          EXEC CICS ABEND                                          ELTTHER2
00599                    ABCODE ('EL01')                                ELTTHER2
00600          END-EXEC                                                 ELTTHER2
00601      END-IF.                                                      ELTTHER2
00602                                                                   ELTTHER2
00603 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTTHER2
00604                                                                   ELTTHER2
00605      PERFORM 0002-SET-ADR-OF-CIA-ELS-COM.                         ELTTHER2
00606      PERFORM 0004-SET-ADR-OF-SSB-SEL-STAT.                        ELTTHER2
00607      PERFORM 0006-SET-ADR-OF-COF-OUT-INT.                         ELTTHER2
00608      PERFORM 0008-SET-ADR-OF-KWA-FILE-KEY.                        ELTTHER2
00609      PERFORM 0010-SET-ADR-OF-CMF-COD-MAN.                         ELTTHER2
00610      PERFORM 0012-SET-ADR-OF-TCAR-COMP-WRK.                       ELTTHER2
00611      PERFORM 0014-SET-ADR-OF-GRP-SPEC-REC.                        ELTTHER2
00612      PERFORM 0016-SET-ADR-OF-PLS-PAY-LVL.                         ELTTHER2
00613                                                                   ELTTHER2
00614      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTTHER2
00615                                                                   ELTTHER2
00616      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTTHER2
00617              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTTHER2
00618                                                                   ELTTHER2
00619      SET CIA-STG-GETMAIN TO TRUE.                                 ELTTHER2
00620      EXEC CICS LINK                                               ELTTHER2
00621                PROGRAM('ELUSTGMG')                                ELTTHER2
00622                COMMAREA(DFHCOMMAREA)                              ELTTHER2
00623      END-EXEC.                                                    ELTTHER2
00624                                                                   ELTTHER2
00625      PERFORM 0018-SET-ADR-OF-PVN-BEN-PROV.                        ELTTHER2
00626      PERFORM 0100-CHECK-LOB.                                      ELTTHER2
00627                                                                   ELTTHER2
00628      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTTHER2
00629                                                                   ELTTHER2
00630                                                                   ELTTHER2
00631      IF SSB-SUB-TOPIC = 'RADIATION       '                        ELTTHER2
00632          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00633             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00634                 PERFORM 2000-CHEMO-THRP-IP-INST-RTNE              ELTTHER2
00635                         THRU 2099-EXIT                            ELTTHER2
00636             END-IF                                                ELTTHER2
00637             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00638                 PERFORM 2200-CHEMO-THRP-OP-INST-RTNE              ELTTHER2
00639                         THRU 2299-EXIT                            ELTTHER2
00640             END-IF                                                ELTTHER2
00641          END-IF                                                   ELTTHER2
00642          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00643             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00644                 PERFORM 2400-CHEMO-THRP-IP-PROF-RTNE              ELTTHER2
00645                         THRU 2499-EXIT                            ELTTHER2
00646             END-IF                                                ELTTHER2
00647             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00648                 PERFORM 2600-CHEMO-THRP-OP-PROF-RTNE              ELTTHER2
00649                         THRU 2699-EXIT                            ELTTHER2
00650             END-IF                                                ELTTHER2
00651          END-IF                                                   ELTTHER2
00652      END-IF.                                                      ELTTHER2
00653                                                                   ELTTHER2
00654                                                                   ELTTHER2
00655      IF SSB-SUB-TOPIC = 'SPEECH           '                       ELTTHER2
00656          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00657             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00658                 PERFORM 5000-SPEECH-THRP-IP-INST-RTNE             ELTTHER2
00659                         THRU 5099-EXIT                            ELTTHER2
00660             END-IF                                                ELTTHER2
00661             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00662                 PERFORM 5200-SPEECH-THRP-OP-INST-RTNE             ELTTHER2
00663                         THRU 5299-EXIT                            ELTTHER2
00664             END-IF                                                ELTTHER2
00665          END-IF                                                   ELTTHER2
00666          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00667             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00668                 PERFORM 5400-SPEECH-THRP-IP-PROF-RTNE             ELTTHER2
00669                         THRU 5499-EXIT                            ELTTHER2
00670             END-IF                                                ELTTHER2
00671             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00672                 PERFORM 5600-SPEECH-THRP-OP-PROF-RTNE             ELTTHER2
00673                         THRU 5699-EXIT                            ELTTHER2
00674             END-IF                                                ELTTHER2
00675          END-IF                                                   ELTTHER2
00676      END-IF.                                                      ELTTHER2
00677                                                                   ELTTHER2
00678                                                                   ELTTHER2
00679      IF SSB-SUB-TOPIC = 'CARDIAC         '                        ELTTHER2
00680          IF SSB-PROV-CLASS-INST  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00681             IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00682                 PERFORM 6000-CARD-THERP-IP-INST-RTNE              ELTTHER2
00683                         THRU 6099-EXIT                            ELTTHER2
00684             END-IF                                                ELTTHER2
00685             IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00686                 PERFORM 6200-CARD-THERP-OP-INST-RTNE              ELTTHER2
00687                         THRU 6299-EXIT                            ELTTHER2
00688             END-IF                                                ELTTHER2
00689          END-IF                                                   ELTTHER2
00690 * RGO CARDIAC DOES NOT HAVE PROFFESIONAL BENIFITS.                ELTTHER2
00691 *     BECAUSE ALL OTHER THERAPY BENEFIT PROVISIONS DO HAVE        ELTTHER2
00692 *     HAVE PROFESSIONAL BP'S, RATHER THAN RE-DESIGN HOW THERAPIES ELTTHER2
00693 *     IS DONE, SIMPLY DISPLAY A MESSAGE OF NO COVERAGE.           ELTTHER2
00694 *                                                                 ELTTHER2
00695          IF SSB-PROV-CLASS-PROF  OR   SSB-PROV-CLASS-BOTH         ELTTHER2
00696             PERFORM 3000-COMMON-HEADER-RTNE                       ELTTHER2
00697             MOVE WS-PROVIDER-ELIGIBILITY TO COF-DTL-LINE(2)       ELTTHER2
00698             MOVE 2 TO COF-NBR-DTL-LINES                           ELTTHER2
00699             EXEC CICS LINK PROGRAM('ELUOUTPT')                    ELTTHER2
00700                            COMMAREA(DFHCOMMAREA)                  ELTTHER2
00701             END-EXEC                                              ELTTHER2
00702          END-IF.                                                  ELTTHER2
00703 *           IF SSB-SERV-CLASS-IP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00704 *               PERFORM 6400-CARD-THERP-IP-PROF-RTNE              ELTTHER2
00705 *                       THRU 6499-EXIT                            ELTTHER2
00706 *           END-IF                                                ELTTHER2
00707 *           IF SSB-SERV-CLASS-OP   OR   SSB-SERV-CLASS-BOTH       ELTTHER2
00708 *               PERFORM 6600-CARD-THERP-OP-PROF-RTNE              ELTTHER2
00709 *                       THRU 6699-EXIT                            ELTTHER2
00710 *           END-IF                                                ELTTHER2
00711 *        END-IF                                                   ELTTHER2
00712 *    END-IF.                                                      ELTTHER2
00713 * RGO CARDIAC DOES NOT HAVE PROF. DELETE                          ELTTHER2
00714                                                                   ELTTHER2
00715      IF (SSB-SUB-TOPIC = 'SPEECH          '  OR                   ELTTHER2
00716                          'RADIATION       '  OR                   ELTTHER2
00717                          'CARDIAC         ')            AND       ELTTHER2
00718          (SSB-PROV-CLASS-INST   OR                                ELTTHER2
00719           SSB-PROV-CLASS-PROF   OR                                ELTTHER2
00720           SSB-PROV-CLASS-BOTH)                          AND       ELTTHER2
00721          (SSB-SERV-CLASS-IP     OR                                ELTTHER2
00722           SSB-SERV-CLASS-OP     OR                                ELTTHER2
00723           SSB-SERV-CLASS-BOTH)                                    ELTTHER2
00724            CONTINUE                                               ELTTHER2
00725      ELSE                                                         ELTTHER2
00726          SET CIA-AB-UNDEF TO TRUE                                 ELTTHER2
00727          EXEC CICS ABEND                                          ELTTHER2
00728                    ABCODE(CIA-ABCODE)                             ELTTHER2
00729          END-EXEC                                                 ELTTHER2
00730      END-IF.                                                      ELTTHER2
00731                                                                   ELTTHER2
00732      MOVE 'E'  TO  COF-FUNCTION.                                  ELTTHER2
00733      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTTHER2
00734                     COF-NBR-DTL-LINES.                            ELTTHER2
00735      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
00736             END-EXEC.                                             ELTTHER2
00737                                                                   ELTTHER2
00738      EXEC CICS RETURN   END-EXEC.                                 ELTTHER2
00739                                                                   ELTTHER2
00740      GOBACK.                                                      ELTTHER2
00741  0002-SET-ADR-OF-CIA-ELS-COM.                                     ELTTHER2
00742      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTTHER2
00743          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTTHER2
00744                                                                   ELTTHER2
00745  0004-SET-ADR-OF-SSB-SEL-STAT.                                    ELTTHER2
00746      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTTHER2
00747      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00748          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTTHER2
00749                                                                   ELTTHER2
00750  0006-SET-ADR-OF-COF-OUT-INT.                                     ELTTHER2
00751      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTTHER2
00752      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00753          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTTHER2
00754                                                                   ELTTHER2
00755  0008-SET-ADR-OF-KWA-FILE-KEY.                                    ELTTHER2
00756      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTTHER2
00757      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00758          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTTHER2
00759                                                                   ELTTHER2
00760  0010-SET-ADR-OF-CMF-COD-MAN.                                     ELTTHER2
00761      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTTHER2
00762      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00763          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTTHER2
00764                                                                   ELTTHER2
00765  0012-SET-ADR-OF-TCAR-COMP-WRK.                                   ELTTHER2
00766      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTTHER2
00767      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00768          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTTHER2
00769                                                                   ELTTHER2
00770  0014-SET-ADR-OF-GRP-SPEC-REC.                                    ELTTHER2
00771      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTTHER2
00772      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00773          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTTHER2
00774                                                                   ELTTHER2
00775  0016-SET-ADR-OF-PLS-PAY-LVL.                                     ELTTHER2
00776      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTTHER2
00777      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00778          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTTHER2
00779                                                                   ELTTHER2
00780  0018-SET-ADR-OF-PVN-BEN-PROV.                                    ELTTHER2
00781      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTTHER2
00782      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00783          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTTHER2
00784                                                                   ELTTHER2
00785  0020-SET-ADR-OF-PLT-PAY-LVL.                                     ELTTHER2
00786      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTTHER2
00787      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00788          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTTHER2
00789                                                                   ELTTHER2
00790                                                                   ELTTHER2
00791 ****************************************************************  ELTTHER2
00792 * 0100-CHECK-LOB   CHECK ANY OF THE CONTRACT RECORDS TO SEE    *  ELTTHER2
00793 *      IF THE LOB EQUALS '1,','2' OR '3'.                      *  ELTTHER2
00794 *      IF SO, THEN WE WILL DISPLAY 'BASIC:' AND/OR             *  ELTTHER2
00795 *         'SUPPLEMENTAL:' ON THE SCREEN LATER ON.              *  ELTTHER2
00796 ****************************************************************  ELTTHER2
00797  0100-CHECK-LOB.                                                  ELTTHER2
00798                                                                   ELTTHER2
00799      MOVE 'N' TO DISPLAY-BAS-SUP.                                 ELTTHER2
00800                                                                   ELTTHER2
00801      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTTHER2
00802      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00803                      ADDRESS OF CONTRACT-RECORD.                  ELTTHER2
00804                                                                   ELTTHER2
00805      IF CIA-RC-OK                                                 ELTTHER2
00806         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHER2
00807             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHER2
00808                                                                   ELTTHER2
00809      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTTHER2
00810      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00811                      ADDRESS OF CONTRACT-RECORD.                  ELTTHER2
00812                                                                   ELTTHER2
00813      IF CIA-RC-OK                                                 ELTTHER2
00814         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHER2
00815             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHER2
00816                                                                   ELTTHER2
00817      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTTHER2
00818      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00819                      ADDRESS OF CONTRACT-RECORD.                  ELTTHER2
00820                                                                   ELTTHER2
00821      IF CIA-RC-OK                                                 ELTTHER2
00822         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHER2
00823             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHER2
00824                                                                   ELTTHER2
00825      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTTHER2
00826      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
00827                      ADDRESS OF CONTRACT-RECORD.                  ELTTHER2
00828                                                                   ELTTHER2
00829      IF CIA-RC-OK                                                 ELTTHER2
00830         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTTHER2
00831             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTTHER2
00832                                                                   ELTTHER2
00833  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTTHER2
00834                                                                   ELTTHER2
00835      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB    ELTTHER2
00836          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX)           =  'B'      ELTTHER2
00837              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTTHER2
00838                                                                   ELTTHER2
00839      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTTHER2
00840         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
00841         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTTHER2
00842         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTTHER2
00843                                                   CMF-CODE-VALUE  ELTTHER2
00844         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTTHER2
00845         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTTHER2
00846         PERFORM 4000-CODES-MANUAL-LONG                            ELTTHER2
00847         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTTHER2
00848         IF WS-CIA  >  20 OR  =  20                                ELTTHER2
00849            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTTHER2
00850            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTTHER2
00851                COMMAREA(DFHCOMMAREA)                              ELTTHER2
00852                 END-EXEC                                          ELTTHER2
00853            MOVE +1  TO  WS-CIA.                                   ELTTHER2
00854                                                                   ELTTHER2
00855  1090-PROBLEM-WITH-INDICES.                                       ELTTHER2
00856                                                                   ELTTHER2
00857      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTTHER2
00858      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
00859      MOVE 'P'  TO  COF-FUNCTION.                                  ELTTHER2
00860                                                                   ELTTHER2
00861      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
00862              END-EXEC.                                            ELTTHER2
00863                                                                   ELTTHER2
00864  1099-EXIT.            EXIT.                                      ELTTHER2
00865 /  R A D I A T I O N    I P   I N S T N L                         ELTTHER2
00866 ***************************************************************** ELTTHER2
00867 *  R A D I A T I O N     I P   I N S T N L                        ELTTHER2
00868 *                                                                 ELTTHER2
00869 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHER2
00870 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHER2
00871 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHER2
00872 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHER2
00873 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHER2
00874 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHER2
00875 *  MODULE.                                                        ELTTHER2
00876 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHER2
00877 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHER2
00878 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHER2
00879 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHER2
00880 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHER2
00881 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHER2
00882 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHER2
00883 *                                                                 ELTTHER2
00884 ***************************************************************** ELTTHER2
00885  2000-CHEMO-THRP-IP-INST-RTNE.                                    ELTTHER2
00886      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
00887                                                                   ELTTHER2
00888      MOVE WS-HDR-2-CHEMO-IP-INST  TO  COF-HDR-LINE(2).            ELTTHER2
00889                                                                   ELTTHER2
00890      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
00891      PERFORM WITH TEST BEFORE                                     ELTTHER2
00892              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
00893              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
00894         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
00895         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
00896         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
00897         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
00898      END-PERFORM.                                                 ELTTHER2
00899      MOVE WS-CHEMO-IP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHER2
00900                                                                   ELTTHER2
00901                                                                   ELTTHER2
00902      PERFORM WITH TEST BEFORE                                     ELTTHER2
00903         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
00904         UNTIL   WS-SUB  >     WS-CHEMO-IP-INST-CNT                ELTTHER2
00905           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
00906           MOVE WS-CHEMO-IP-INST-LIST (WS-SUB)                     ELTTHER2
00907                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
00908            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
00909                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
00910      END-PERFORM.                                                 ELTTHER2
00911                                                                   ELTTHER2
00912      MOVE WS-CHEMO-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHER2
00913                                                                   ELTTHER2
00914      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
00915      END-EXEC.                                                    ELTTHER2
00916                                                                   ELTTHER2
00917      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
00918      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
00919      END-EXEC.                                                    ELTTHER2
00920                                                                   ELTTHER2
00921      IF PVN-COVG-NONE                                             ELTTHER2
00922         GO TO 2099-EXIT.                                          ELTTHER2
00923                                                                   ELTTHER2
00924      MOVE +1  TO  WS-CIA.                                         ELTTHER2
00925 * START OF 2000-                                                  ELTTHER2
00926                                                                   ELTTHER2
00927      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
00928            PSP-PROVN-PRICING-METHD,                               ELTTHER2
00929            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
00930            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
00931            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
00932            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
00933            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
00934            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
00935            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
00936            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
00937            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
00938            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
00939            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
00940            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
00941                                                                   ELTTHER2
00942      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
00943                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
00944                     END-EXEC.                                     ELTTHER2
00945                                                                   ELTTHER2
00946      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
00947                                                                   ELTTHER2
00948                                                                   ELTTHER2
00949      PERFORM 2030-FIND-FIRST-NONZERO                              ELTTHER2
00950         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
00951         UNTIL WS-SUB  >  WS-CHEMO-IP-INST-CNT.                    ELTTHER2
00952                                                                   ELTTHER2
00953      GO TO 2099-EXIT.                                             ELTTHER2
00954  2030-FIND-FIRST-NONZERO.                                         ELTTHER2
00955      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
00956      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
00957         CONTINUE                                                  ELTTHER2
00958      ELSE                                                         ELTTHER2
00959         PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT.           ELTTHER2
00960                                                                   ELTTHER2
00961 *** 2040- IS JUST THE SAME AS 6000- IN PROGRAM ELTTHERP           ELTTHER2
00962  2040-BUILD-SCREEN-LINES.                                         ELTTHER2
00963                                                                   ELTTHER2
00964      SET PLT-INDEX1  TO                                           ELTTHER2
00965                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
00966      IF WS-NOT-FIRST-TIME                                         ELTTHER2
00967         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
00968         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
00969         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
00970             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
00971             END-EXEC                                              ELTTHER2
00972      ELSE                                                         ELTTHER2
00973         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
00974                                                                   ELTTHER2
00975      MOVE 1  TO  WS-CIA.                                          ELTTHER2
00976      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
00977         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
00978            SET PLT-INDEX2  TO  2                                  ELTTHER2
00979         ELSE                                                      ELTTHER2
00980            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
00981            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
00982            GO TO 2040-EXIT                                        ELTTHER2
00983      ELSE                                                         ELTTHER2
00984         SET PLT-INDEX2  TO  1.                                    ELTTHER2
00985                                                                   ELTTHER2
00986 **---------------------------------------------------------------+ELTTHER2
00987 **                                                               |ELTTHER2
00988 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
00989      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
00990      ADD  +1  TO  WS-CIA.                                         ELTTHER2
00991      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
00992                                                                   ELTTHER2
00993      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
00994                                                                   ELTTHER2
00995      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
00996         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
00997         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-IP-INST-CNT.          ELTTHER2
00998      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
00999                                                                   ELTTHER2
01000      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
01001      MOVE +1  TO  WS-CIA                                          ELTTHER2
01002      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01003               END-EXEC.                                           ELTTHER2
01004 **                                                               |ELTTHER2
01005 **---------------------------------------------------------------+ELTTHER2
01006                                                                   ELTTHER2
01007 **---------------------------------------------------------------+ELTTHER2
01008 **                                                               |ELTTHER2
01009 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
01010      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
01011 **                                                               |ELTTHER2
01012 **---------------------------------------------------------------+ELTTHER2
01013                                                                   ELTTHER2
01014 **---------------------------------------------------------------+ELTTHER2
01015 **                                                               |ELTTHER2
01016 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
01017 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
01018 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
01019      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
01020 **                                                               |ELTTHER2
01021 **---------------------------------------------------------------+ELTTHER2
01022                                                                   ELTTHER2
01023 **---------------------------------------------------------------+ELTTHER2
01024 **                                                               |ELTTHER2
01025 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHER2
01026 **                  H O S P I T A L   B I L L                    |ELTTHER2
01027      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHER2
01028      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
01029 **                                                               |ELTTHER2
01030 **---------------------------------------------------------------+ELTTHER2
01031                                                                   ELTTHER2
01032 **---------------------------------------------------------------+ELTTHER2
01033 **                                                               |ELTTHER2
01034 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHER2
01035      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
01036 **                                                               |ELTTHER2
01037 **---------------------------------------------------------------+ELTTHER2
01038                                                                   ELTTHER2
01039 **---------------------------------------------------------------+ELTTHER2
01040 **                                                               |ELTTHER2
01041 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
01042      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
01043         THRU 3250-EXIT.                                           ELTTHER2
01044 **                                                               |ELTTHER2
01045 **---------------------------------------------------------------+ELTTHER2
01046                                                                   ELTTHER2
01047 **---------------------------------------------------------------+ELTTHER2
01048 **                                                               |ELTTHER2
01049 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
01050      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
01051 **                                                               |ELTTHER2
01052 **---------------------------------------------------------------+ELTTHER2
01053                                                                   ELTTHER2
01054 **---------------------------------------------------------------+ELTTHER2
01055 **                                                               |ELTTHER2
01056 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
01057 **                         A N D                                 |ELTTHER2
01058 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
01059      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
01060 **                                                               |ELTTHER2
01061 **---------------------------------------------------------------+ELTTHER2
01062                                                                   ELTTHER2
01063 **---------------------------------------------------------------+ELTTHER2
01064 **                                                               |ELTTHER2
01065 **               P E R F O R M   T A B U L A R                   |ELTTHER2
01066      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
01067 **                                                               |ELTTHER2
01068 **---------------------------------------------------------------+ELTTHER2
01069                                                                   ELTTHER2
01070  2040-EXIT.  EXIT.                                                ELTTHER2
01071                                                                   ELTTHER2
01072  2099-EXIT.            EXIT.                                      ELTTHER2
01073 /  R A D I A T I O N   O P   I N S T N L                          ELTTHER2
01074  2200-CHEMO-THRP-OP-INST-RTNE.                                    ELTTHER2
01075      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
01076                                                                   ELTTHER2
01077      MOVE WS-HDR-2-CHEMO-OP-INST  TO  COF-HDR-LINE(2).            ELTTHER2
01078                                                                   ELTTHER2
01079      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
01080      PERFORM WITH TEST BEFORE                                     ELTTHER2
01081              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
01082              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
01083         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
01084         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01085         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01086         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
01087      END-PERFORM.                                                 ELTTHER2
01088      MOVE WS-CHEMO-OP-INST-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHER2
01089                                                                   ELTTHER2
01090                                                                   ELTTHER2
01091      PERFORM WITH TEST BEFORE                                     ELTTHER2
01092         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
01093         UNTIL   WS-SUB  >     WS-CHEMO-OP-INST-CNT                ELTTHER2
01094           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
01095           MOVE WS-CHEMO-OP-INST-LIST(WS-SUB)                      ELTTHER2
01096                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
01097            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
01098                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
01099      END-PERFORM.                                                 ELTTHER2
01100                                                                   ELTTHER2
01101      MOVE WS-CHEMO-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHER2
01102                                                                   ELTTHER2
01103      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
01104      END-EXEC.                                                    ELTTHER2
01105                                                                   ELTTHER2
01106      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
01107      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01108      END-EXEC.                                                    ELTTHER2
01109                                                                   ELTTHER2
01110                                                                   ELTTHER2
01111      IF PVN-COVG-NONE                                             ELTTHER2
01112         GO TO 2299-EXIT.                                          ELTTHER2
01113                                                                   ELTTHER2
01114      MOVE +1  TO  WS-CIA.                                         ELTTHER2
01115      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
01116            PSP-PROVN-PRICING-METHD,                               ELTTHER2
01117            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
01118            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
01119            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
01120            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
01121            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
01122            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
01123            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
01124            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
01125            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
01126            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
01127            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
01128            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHER2
01129            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHER2
01130            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
01131                                                                   ELTTHER2
01132                                                                   ELTTHER2
01133      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
01134                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
01135                     END-EXEC.                                     ELTTHER2
01136                                                                   ELTTHER2
01137      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
01138                                                                   ELTTHER2
01139      PERFORM 2230-FIND-FIRST-NONZERO                              ELTTHER2
01140         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
01141         UNTIL WS-SUB  >  WS-CHEMO-OP-INST-CNT.                    ELTTHER2
01142                                                                   ELTTHER2
01143      GO TO 2299-EXIT.                                             ELTTHER2
01144  2230-FIND-FIRST-NONZERO.                                         ELTTHER2
01145      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01146      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
01147         CONTINUE                                                  ELTTHER2
01148      ELSE                                                         ELTTHER2
01149         PERFORM 2240-BUILD-SCREEN-LINES THRU 2240-EXIT.           ELTTHER2
01150                                                                   ELTTHER2
01151  2240-BUILD-SCREEN-LINES.                                         ELTTHER2
01152                                                                   ELTTHER2
01153      SET PLT-INDEX1  TO                                           ELTTHER2
01154                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
01155      IF WS-NOT-FIRST-TIME                                         ELTTHER2
01156         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
01157         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
01158         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
01159             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
01160              END-EXEC                                             ELTTHER2
01161      ELSE                                                         ELTTHER2
01162         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
01163                                                                   ELTTHER2
01164      MOVE 1  TO  WS-CIA.                                          ELTTHER2
01165      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
01166         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
01167            SET PLT-INDEX2  TO  2                                  ELTTHER2
01168         ELSE                                                      ELTTHER2
01169            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
01170            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
01171            GO TO 2240-EXIT                                        ELTTHER2
01172      ELSE                                                         ELTTHER2
01173         SET PLT-INDEX2  TO  1.                                    ELTTHER2
01174                                                                   ELTTHER2
01175 **---------------------------------------------------------------+ELTTHER2
01176 **                                                               |ELTTHER2
01177 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
01178      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
01179      ADD  +1  TO  WS-CIA.                                         ELTTHER2
01180      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
01181                                                                   ELTTHER2
01182      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
01183                                                                   ELTTHER2
01184      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
01185         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
01186         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-OP-INST-CNT.          ELTTHER2
01187      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01188                                                                   ELTTHER2
01189      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
01190      MOVE +1  TO  WS-CIA                                          ELTTHER2
01191      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01192               END-EXEC.                                           ELTTHER2
01193 **                                                               |ELTTHER2
01194 **---------------------------------------------------------------+ELTTHER2
01195                                                                   ELTTHER2
01196 **---------------------------------------------------------------+ELTTHER2
01197 **                                                               |ELTTHER2
01198 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
01199      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
01200 **                                                               |ELTTHER2
01201 **---------------------------------------------------------------+ELTTHER2
01202                                                                   ELTTHER2
01203 **---------------------------------------------------------------+ELTTHER2
01204 **                                                               |ELTTHER2
01205 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
01206 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
01207 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
01208      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
01209 **                                                               |ELTTHER2
01210 **---------------------------------------------------------------+ELTTHER2
01211                                                                   ELTTHER2
01212 **---------------------------------------------------------------+ELTTHER2
01213 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHER2
01214 **                    H O S P I T A L    B I L L                 |ELTTHER2
01215      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHER2
01216      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
01217 **                                                               |ELTTHER2
01218 **---------------------------------------------------------------+ELTTHER2
01219                                                                   ELTTHER2
01220 **---------------------------------------------------------------+ELTTHER2
01221 **                                                               |ELTTHER2
01222 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHER2
01223      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
01224 **                                                               |ELTTHER2
01225 **---------------------------------------------------------------+ELTTHER2
01226                                                                   ELTTHER2
01227 **---------------------------------------------------------------+ELTTHER2
01228 **                                                               |ELTTHER2
01229 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
01230      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
01231         THRU 3250-EXIT.                                           ELTTHER2
01232 **                                                               |ELTTHER2
01233 **---------------------------------------------------------------+ELTTHER2
01234                                                                   ELTTHER2
01235 **---------------------------------------------------------------+ELTTHER2
01236 **                                                               |ELTTHER2
01237 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
01238      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHER2
01239 **                                                               |ELTTHER2
01240 **---------------------------------------------------------------+ELTTHER2
01241                                                                   ELTTHER2
01242 **---------------------------------------------------------------+ELTTHER2
01243 **                                                               |ELTTHER2
01244 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
01245      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
01246 **                                                               |ELTTHER2
01247 **---------------------------------------------------------------+ELTTHER2
01248                                                                   ELTTHER2
01249 **---------------------------------------------------------------+ELTTHER2
01250 **                                                               |ELTTHER2
01251 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
01252 **                         A N D                                 |ELTTHER2
01253 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
01254      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
01255 **                                                               |ELTTHER2
01256 **---------------------------------------------------------------+ELTTHER2
01257                                                                   ELTTHER2
01258 **---------------------------------------------------------------+ELTTHER2
01259 **                                                               |ELTTHER2
01260 **               P E R F O R M   T A B U L A R                   |ELTTHER2
01261      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
01262 **                                                               |ELTTHER2
01263 **---------------------------------------------------------------+ELTTHER2
01264  2240-EXIT.  EXIT.                                                ELTTHER2
01265                                                                   ELTTHER2
01266                                                                   ELTTHER2
01267  2299-EXIT.            EXIT.                                      ELTTHER2
01268 /  R A D I A T I O N   I P   P R O F S L                          ELTTHER2
01269  2400-CHEMO-THRP-IP-PROF-RTNE.                                    ELTTHER2
01270      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
01271                                                                   ELTTHER2
01272      MOVE WS-HDR-2-CHEMO-IP-PROF  TO  COF-HDR-LINE(2).            ELTTHER2
01273                                                                   ELTTHER2
01274      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
01275      PERFORM WITH TEST BEFORE                                     ELTTHER2
01276              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
01277              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
01278         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
01279         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01280         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01281         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
01282      END-PERFORM.                                                 ELTTHER2
01283      MOVE WS-CHEMO-IP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHER2
01284                                                                   ELTTHER2
01285                                                                   ELTTHER2
01286      PERFORM WITH TEST BEFORE                                     ELTTHER2
01287         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
01288         UNTIL   WS-SUB  >     WS-CHEMO-IP-PROF-CNT                ELTTHER2
01289           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
01290           MOVE WS-CHEMO-IP-PROF-LIST(WS-SUB)                      ELTTHER2
01291                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
01292            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
01293                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
01294      END-PERFORM.                                                 ELTTHER2
01295                                                                   ELTTHER2
01296      MOVE WS-CHEMO-IP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHER2
01297                                                                   ELTTHER2
01298      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
01299      END-EXEC.                                                    ELTTHER2
01300                                                                   ELTTHER2
01301      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
01302      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01303      END-EXEC.                                                    ELTTHER2
01304                                                                   ELTTHER2
01305      IF PVN-COVG-NONE                                             ELTTHER2
01306         GO TO 2499-EXIT.                                          ELTTHER2
01307                                                                   ELTTHER2
01308      MOVE +1  TO  WS-CIA.                                         ELTTHER2
01309      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
01310            PSP-PROVN-PRICING-METHD,                               ELTTHER2
01311            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
01312            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
01313            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
01314            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
01315            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
01316            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
01317            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
01318            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
01319            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
01320            PSE-BEN-SCOPE-ID,                                      ELTTHER2
01321            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
01322            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
01323            PSE-MAX-AMT-PER-VISIT.                                 ELTTHER2
01324                                                                   ELTTHER2
01325                                                                   ELTTHER2
01326      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
01327                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
01328                     END-EXEC.                                     ELTTHER2
01329                                                                   ELTTHER2
01330      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
01331                                                                   ELTTHER2
01332      PERFORM 2430-FIND-FIRST-NONZERO                              ELTTHER2
01333         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
01334         UNTIL WS-SUB  >  WS-CHEMO-IP-PROF-CNT.                    ELTTHER2
01335                                                                   ELTTHER2
01336      GO TO 2499-EXIT.                                             ELTTHER2
01337  2430-FIND-FIRST-NONZERO.                                         ELTTHER2
01338      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01339      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
01340         CONTINUE                                                  ELTTHER2
01341      ELSE                                                         ELTTHER2
01342         PERFORM 2440-BUILD-SCREEN-LINES THRU 2440-EXIT.           ELTTHER2
01343                                                                   ELTTHER2
01344  2440-BUILD-SCREEN-LINES.                                         ELTTHER2
01345                                                                   ELTTHER2
01346      SET PLT-INDEX1  TO                                           ELTTHER2
01347                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
01348      IF WS-NOT-FIRST-TIME                                         ELTTHER2
01349         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
01350         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
01351         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
01352             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
01353                                         END-EXEC                  ELTTHER2
01354      ELSE                                                         ELTTHER2
01355         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
01356                                                                   ELTTHER2
01357      MOVE 1  TO  WS-CIA.                                          ELTTHER2
01358      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
01359         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
01360            SET PLT-INDEX2  TO  2                                  ELTTHER2
01361         ELSE                                                      ELTTHER2
01362            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
01363            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
01364            GO TO 2440-EXIT                                        ELTTHER2
01365      ELSE                                                         ELTTHER2
01366         SET PLT-INDEX2  TO  1.                                    ELTTHER2
01367                                                                   ELTTHER2
01368 **---------------------------------------------------------------+ELTTHER2
01369 **                                                               |ELTTHER2
01370 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
01371      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
01372      ADD  +1  TO  WS-CIA.                                         ELTTHER2
01373      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
01374      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
01375         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
01376         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-IP-PROF-CNT.          ELTTHER2
01377      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01378                                                                   ELTTHER2
01379      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
01380      MOVE +1  TO  WS-CIA                                          ELTTHER2
01381      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01382                                         END-EXEC.                 ELTTHER2
01383 **                                                               |ELTTHER2
01384 **---------------------------------------------------------------+ELTTHER2
01385 **---------------------------------------------------------------+ELTTHER2
01386 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
01387      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
01388 **                                                               |ELTTHER2
01389 **---------------------------------------------------------------+ELTTHER2
01390                                                                   ELTTHER2
01391 **---------------------------------------------------------------+ELTTHER2
01392 **                                                               |ELTTHER2
01393 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
01394      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
01395 **                                                               |ELTTHER2
01396 **---------------------------------------------------------------+ELTTHER2
01397                                                                   ELTTHER2
01398 **---------------------------------------------------------------+ELTTHER2
01399 **                                                               |ELTTHER2
01400 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
01401 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
01402 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
01403      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
01404 **                                                               |ELTTHER2
01405 **---------------------------------------------------------------+ELTTHER2
01406                                                                   ELTTHER2
01407 **---------------------------------------------------------------+ELTTHER2
01408 **                                                               |ELTTHER2
01409 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
01410      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
01411         THRU 3250-EXIT.                                           ELTTHER2
01412 **                                                               |ELTTHER2
01413 **---------------------------------------------------------------+ELTTHER2
01414                                                                   ELTTHER2
01415 **---------------------------------------------------------------+ELTTHER2
01416 **                                                               |ELTTHER2
01417 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
01418      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
01419 **                                                               |ELTTHER2
01420 **---------------------------------------------------------------+ELTTHER2
01421                                                                   ELTTHER2
01422 **---------------------------------------------------------------+ELTTHER2
01423 **                                                               |ELTTHER2
01424 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
01425      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
01426 **                                                               |ELTTHER2
01427 **---------------------------------------------------------------+ELTTHER2
01428                                                                   ELTTHER2
01429 **---------------------------------------------------------------+ELTTHER2
01430 **                                                               |ELTTHER2
01431 **          S A M E   P R O V I D E R   B I L L I N G            |ELTTHER2
01432 **    I P   R A D I A T I O N   T H E R A P Y / M E D I C A L    |ELTTHER2
01433                                                                   ELTTHER2
01434      PERFORM 2450-SET-ADR-OF-CON-REC-B.                           ELTTHER2
01435      IF CIA-RC-PTR-NULL                                           ELTTHER2
01436         PERFORM 2460-SET-ADR-OF-CON-REC-S                         ELTTHER2
01437         IF CIA-RC-PTR-NULL                                        ELTTHER2
01438            CONTINUE                                               ELTTHER2
01439      ELSE                                                         ELTTHER2
01440          MOVE 'Y' TO CALL-ELUOUTPT-IND                            ELTTHER2
01441          MOVE WS-SAME-PROVIDER-RADIATN TO COF-DTL-LINE (WS-CIA)   ELTTHER2
01442          ADD +1                   TO WS-CIA                       ELTTHER2
01443          PERFORM 2450-SET-ADR-OF-CON-REC-B                        ELTTHER2
01444          IF CIA-RC-PTR-NULL                                       ELTTHER2
01445             PERFORM 4200-SAME-PROV-BILL-THRP-BAS                  ELTTHER2
01446          END-IF                                                   ELTTHER2
01447          PERFORM 2460-SET-ADR-OF-CON-REC-S                        ELTTHER2
01448          IF CIA-RC-PTR-NULL                                       ELTTHER2
01449             PERFORM 4250-SAME-PROV-BILL-THRP-SUP                  ELTTHER2
01450          END-IF                                                   ELTTHER2
01451          IF YES-CALL-ELUOUTPT                                     ELTTHER2
01452              MOVE 'N' TO CALL-ELUOUTPT-IND                        ELTTHER2
01453              ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES          ELTTHER2
01454              EXEC CICS  LINK  PROGRAM('ELUOUTPT')                 ELTTHER2
01455                               COMMAREA(DFHCOMMAREA)               ELTTHER2
01456              END-EXEC                                             ELTTHER2
01457              MOVE 1  TO  WS-CIA                                   ELTTHER2
01458          ELSE                                                     ELTTHER2
01459              ADD -1 TO WS-CIA                                     ELTTHER2
01460          END-IF                                                   ELTTHER2
01461      END-IF.                                                      ELTTHER2
01462                                                                   ELTTHER2
01463 **                                                               |ELTTHER2
01464 **---------------------------------------------------------------+ELTTHER2
01465                                                                   ELTTHER2
01466 **---------------------------------------------------------------+ELTTHER2
01467 **                                                               |ELTTHER2
01468 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
01469 **                         A N D                                 |ELTTHER2
01470 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
01471      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
01472 **                                                               |ELTTHER2
01473 **---------------------------------------------------------------+ELTTHER2
01474                                                                   ELTTHER2
01475 **---------------------------------------------------------------+ELTTHER2
01476 **                                                               |ELTTHER2
01477 **               P E R F O R M   T A B U L A R                   |ELTTHER2
01478      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
01479 **                                                               |ELTTHER2
01480 **---------------------------------------------------------------+ELTTHER2
01481                                                                   ELTTHER2
01482  2440-EXIT.  EXIT.                                                ELTTHER2
01483                                                                   ELTTHER2
01484  2450-SET-ADR-OF-CON-REC-B.                                       ELTTHER2
01485      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTTHER2
01486      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
01487          ADDRESS OF CONTRACT-RECORD.                              ELTTHER2
01488                                                                   ELTTHER2
01489  2460-SET-ADR-OF-CON-REC-S.                                       ELTTHER2
01490      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTTHER2
01491      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
01492          ADDRESS OF CONTRACT-RECORD.                              ELTTHER2
01493                                                                   ELTTHER2
01494  2499-EXIT.            EXIT.                                      ELTTHER2
01495 /  R A D I A T I O N  OP   P R O F S L                            ELTTHER2
01496  2600-CHEMO-THRP-OP-PROF-RTNE.                                    ELTTHER2
01497      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
01498                                                                   ELTTHER2
01499      MOVE WS-HDR-2-CHEMO-OP-PROF  TO  COF-HDR-LINE(2).            ELTTHER2
01500                                                                   ELTTHER2
01501      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
01502      PERFORM WITH TEST BEFORE                                     ELTTHER2
01503              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
01504              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
01505         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
01506         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01507         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
01508         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
01509      END-PERFORM.                                                 ELTTHER2
01510      MOVE WS-CHEMO-OP-PROF-CNT TO  PVN-NBR-BEN-PROVN.             ELTTHER2
01511                                                                   ELTTHER2
01512                                                                   ELTTHER2
01513      PERFORM WITH TEST BEFORE                                     ELTTHER2
01514         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
01515         UNTIL   WS-SUB  >     WS-CHEMO-OP-PROF-CNT                ELTTHER2
01516           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
01517           MOVE WS-CHEMO-OP-PROF-LIST(WS-SUB)                      ELTTHER2
01518                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
01519            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
01520                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
01521      END-PERFORM.                                                 ELTTHER2
01522                                                                   ELTTHER2
01523      MOVE WS-CHEMO-OP-SERVICES  TO  SSB-TOPIC-PHRASE.             ELTTHER2
01524                                                                   ELTTHER2
01525      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
01526      END-EXEC.                                                    ELTTHER2
01527                                                                   ELTTHER2
01528      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
01529      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01530      END-EXEC.                                                    ELTTHER2
01531                                                                   ELTTHER2
01532      IF PVN-COVG-NONE                                             ELTTHER2
01533         GO TO 2699-EXIT.                                          ELTTHER2
01534                                                                   ELTTHER2
01535      MOVE +1  TO  WS-CIA.                                         ELTTHER2
01536      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
01537            PSP-PROVN-PRICING-METHD,                               ELTTHER2
01538            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
01539            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
01540            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
01541            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
01542            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
01543            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
01544            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
01545            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
01546            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
01547            PSE-BEN-SCOPE-ID,                                      ELTTHER2
01548            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
01549            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
01550            PSE-MAX-AMT-PER-VISIT,                                 ELTTHER2
01551            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHER2
01552            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHER2
01553                                                                   ELTTHER2
01554                                                                   ELTTHER2
01555      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
01556                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
01557                     END-EXEC.                                     ELTTHER2
01558                                                                   ELTTHER2
01559      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
01560                                                                   ELTTHER2
01561      PERFORM 2630-FIND-FIRST-NONZERO                              ELTTHER2
01562         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
01563         UNTIL WS-SUB  >  WS-CHEMO-OP-PROF-CNT.                    ELTTHER2
01564                                                                   ELTTHER2
01565      GO TO 2699-EXIT.                                             ELTTHER2
01566  2630-FIND-FIRST-NONZERO.                                         ELTTHER2
01567      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01568      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
01569         CONTINUE                                                  ELTTHER2
01570      ELSE                                                         ELTTHER2
01571         PERFORM 2640-BUILD-SCREEN-LINES THRU 2640-EXIT.           ELTTHER2
01572                                                                   ELTTHER2
01573  2640-BUILD-SCREEN-LINES.                                         ELTTHER2
01574                                                                   ELTTHER2
01575      SET PLT-INDEX1  TO                                           ELTTHER2
01576                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
01577      IF WS-NOT-FIRST-TIME                                         ELTTHER2
01578         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
01579         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
01580         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
01581             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
01582                                         END-EXEC                  ELTTHER2
01583      ELSE                                                         ELTTHER2
01584         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
01585                                                                   ELTTHER2
01586      MOVE 1  TO  WS-CIA.                                          ELTTHER2
01587      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
01588         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
01589            SET PLT-INDEX2  TO  2                                  ELTTHER2
01590         ELSE                                                      ELTTHER2
01591            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
01592            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
01593            GO TO 2640-EXIT                                        ELTTHER2
01594      ELSE                                                         ELTTHER2
01595         SET PLT-INDEX2  TO  1.                                    ELTTHER2
01596                                                                   ELTTHER2
01597 **---------------------------------------------------------------+ELTTHER2
01598 **                                                               |ELTTHER2
01599 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
01600      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
01601      ADD  +1  TO  WS-CIA.                                         ELTTHER2
01602      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
01603      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
01604         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
01605         UNTIL  PVN-BEN-PROVN-IDX > WS-CHEMO-OP-PROF-CNT.          ELTTHER2
01606      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
01607                                                                   ELTTHER2
01608      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
01609      MOVE +1  TO  WS-CIA                                          ELTTHER2
01610      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
01611                                         END-EXEC.                 ELTTHER2
01612 **                                                               |ELTTHER2
01613 **---------------------------------------------------------------+ELTTHER2
01614 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
01615      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
01616 **                                                               |ELTTHER2
01617 **---------------------------------------------------------------+ELTTHER2
01618                                                                   ELTTHER2
01619 **---------------------------------------------------------------+ELTTHER2
01620 **                                                               |ELTTHER2
01621 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
01622      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
01623 **                                                               |ELTTHER2
01624 **---------------------------------------------------------------+ELTTHER2
01625                                                                   ELTTHER2
01626 **---------------------------------------------------------------+ELTTHER2
01627 **                                                               |ELTTHER2
01628 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
01629 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
01630 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
01631      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
01632 **                                                               |ELTTHER2
01633 **---------------------------------------------------------------+ELTTHER2
01634                                                                   ELTTHER2
01635 **---------------------------------------------------------------+ELTTHER2
01636 **                                                               |ELTTHER2
01637 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
01638      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
01639         THRU 3250-EXIT.                                           ELTTHER2
01640 **                                                               |ELTTHER2
01641 **---------------------------------------------------------------+ELTTHER2
01642                                                                   ELTTHER2
01643 **---------------------------------------------------------------+ELTTHER2
01644 **                                                               |ELTTHER2
01645 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
01646      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
01647 **                                                               |ELTTHER2
01648 **---------------------------------------------------------------+ELTTHER2
01649                                                                   ELTTHER2
01650 **---------------------------------------------------------------+ELTTHER2
01651 **                                                               |ELTTHER2
01652 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
01653      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
01654 **                                                               |ELTTHER2
01655 **---------------------------------------------------------------+ELTTHER2
01656                                                                   ELTTHER2
01657 **---------------------------------------------------------------+ELTTHER2
01658 **                                                               |ELTTHER2
01659 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
01660      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHER2
01661 **                                                               |ELTTHER2
01662 **---------------------------------------------------------------+ELTTHER2
01663                                                                   ELTTHER2
01664 **---------------------------------------------------------------+ELTTHER2
01665 **                                                               |ELTTHER2
01666 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
01667 **                         A N D                                 |ELTTHER2
01668 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
01669      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
01670 **                                                               |ELTTHER2
01671 **---------------------------------------------------------------+ELTTHER2
01672                                                                   ELTTHER2
01673 **---------------------------------------------------------------+ELTTHER2
01674 **                                                               |ELTTHER2
01675 **               P E R F O R M   T A B U L A R                   |ELTTHER2
01676      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
01677 **                                                               |ELTTHER2
01678 **---------------------------------------------------------------+ELTTHER2
01679  2640-EXIT.  EXIT.                                                ELTTHER2
01680                                                                   ELTTHER2
01681  2699-EXIT.            EXIT.                                      ELTTHER2
01682                                                                   ELTTHER2
01683                                                                   ELTTHER2
01684                                                                   ELTTHER2
01685 /*****************************************************************ELTTHER2
01686 **                                                               |ELTTHER2
01687 *     C O M M O N   H E A D E R   R O U T I N E                   ELTTHER2
01688 ** 3000-                                                         |ELTTHER2
01689 ******************************************************************ELTTHER2
01690  3000-COMMON-HEADER-RTNE.                                         ELTTHER2
01691                                                                   ELTTHER2
01692      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTTHER2
01693      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTTHER2
01694      MOVE 'P'            TO  COF-FUNCTION.                        ELTTHER2
01695      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTTHER2
01696                     COF-NBR-DTL-LINES.                            ELTTHER2
01697                                                                   ELTTHER2
01698      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTTHER2
01699                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
01700                     END-EXEC.                                     ELTTHER2
01701                                                                   ELTTHER2
01702      MOVE SPACE  TO  COF-FUNCTION.                                ELTTHER2
01703      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTTHER2
01704                                                                   ELTTHER2
01705  3099-EXIT.  EXIT.                                                ELTTHER2
01706 ******************************************************************ELTTHER2
01707 **                                                               |ELTTHER2
01708 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
01709 ** 3050-   RGO                                                   |ELTTHER2
01710 ******************************************************************ELTTHER2
01711  3050-PLACE-TREAT.                                                ELTTHER2
01712      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
01713                                                              ZERO ELTTHER2
01714         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
01715         MOVE 2 TO WS-CIA                                          ELTTHER2
01716         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)      ELTTHER2
01717                                                                   ELTTHER2
01718         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
01719         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTTHER2
01720         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
01721                                               TO  CMF-CODE-VALUE  ELTTHER2
01722                                                                   ELTTHER2
01723         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
01724         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
01725                                                                   ELTTHER2
01726         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
01727             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
01728             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
01729                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
01730                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
01731         ELSE                                                      ELTTHER2
01732             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
01733         END-IF                                                    ELTTHER2
01734      END-IF.                                                      ELTTHER2
01735                                                                   ELTTHER2
01736      IF YES-CALL-ELUOUTPT                                         ELTTHER2
01737         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
01738         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
01739      END-IF.                                                      ELTTHER2
01740                                                                   ELTTHER2
01741 /*****************************************************************ELTTHER2
01742 *      P R O V I S I O N   P R I C I N G   M E T H O D            ELTTHER2
01743 * 3100-                                                           ELTTHER2
01744 ******************************************************************ELTTHER2
01745  3100-PROV-PRICING-METHD.                                         ELTTHER2
01746                                                                   ELTTHER2
01747                                                                   ELTTHER2
01748 * START CHECK FOR POSSIBLE ERROR                                  ELTTHER2
01749      SET  PLT-INDEX2  TO  1.                                      ELTTHER2
01750      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
01751         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTTHER2
01752                          AND                                      ELTTHER2
01753         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHER2
01754         SET  PLT-INDEX2  TO  2                                    ELTTHER2
01755         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTTHER2
01756                                                              ZERO ELTTHER2
01757            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG          ELTTHER2
01758            MOVE 1  TO  WS-CIA                                     ELTTHER2
01759            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTTHER2
01760                                                                   ELTTHER2
01761      SET  PLT-INDEX2  TO  1.                                      ELTTHER2
01762      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
01763         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTTHER2
01764                               AND                                 ELTTHER2
01765         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTTHER2
01766         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG             ELTTHER2
01767         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTTHER2
01768         ADD +1  TO  WS-CIA.                                       ELTTHER2
01769                                                                   ELTTHER2
01770      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTTHER2
01771         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHER2
01772         SET  PLT-INDEX2  TO  2                                    ELTTHER2
01773         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTTHER2
01774                                                              ZERO ELTTHER2
01775            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-LONG          ELTTHER2
01776            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHER2
01777            ADD +1  TO  WS-CIA.                                    ELTTHER2
01778                                                                   ELTTHER2
01779 * END   CHECK FOR POSSIBLE ERROR                                  ELTTHER2
01780                                                                   ELTTHER2
01781      SET  PLT-INDEX2  TO  1.                                      ELTTHER2
01782      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
01783         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTTHER2
01784                                             ZERO AND  NOT = '19'  ELTTHER2
01785         MOVE 2 TO WS-CIA                                          ELTTHER2
01786         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTTHER2
01787         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
01788                                                                   ELTTHER2
01789         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
01790             ADD 1 TO WS-CIA                                       ELTTHER2
01791             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
01792         END-IF                                                    ELTTHER2
01793                                                                   ELTTHER2
01794         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
01795         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
01796         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTTHER2
01797                                               TO   CMF-CODE-VALUE ELTTHER2
01798         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
01799         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
01800                                                                   ELTTHER2
01801         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
01802             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
01803             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
01804                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
01805                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
01806         ELSE                                                      ELTTHER2
01807             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
01808         END-IF                                                    ELTTHER2
01809      END-IF.                                                      ELTTHER2
01810                                                                   ELTTHER2
01811      SET  PLT-INDEX2  TO  2.                                      ELTTHER2
01812      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTTHER2
01813         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTTHER2
01814               ZERO AND  NOT = '19'                                ELTTHER2
01815                                                                   ELTTHER2
01816         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
01817             MOVE 2 TO WS-CIA                                      ELTTHER2
01818             MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)          ELTTHER2
01819             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHER2
01820                                                                   ELTTHER2
01821         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
01822             ADD 1 TO WS-CIA                                       ELTTHER2
01823             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
01824         END-IF                                                    ELTTHER2
01825                                                                   ELTTHER2
01826         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
01827         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
01828         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
01829                                                    CMF-CODE-VALUE ELTTHER2
01830         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
01831         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
01832                                                                   ELTTHER2
01833         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
01834             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
01835             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
01836                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
01837                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
01838         ELSE                                                      ELTTHER2
01839             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
01840         END-IF                                                    ELTTHER2
01841      END-IF.                                                      ELTTHER2
01842                                                                   ELTTHER2
01843      IF YES-CALL-ELUOUTPT                                         ELTTHER2
01844         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
01845         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
01846      END-IF.                                                      ELTTHER2
01847                                                                   ELTTHER2
01848      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
01849         SET PLT-INDEX2  TO  2                                     ELTTHER2
01850      ELSE                                                         ELTTHER2
01851         SET PLT-INDEX2  TO  1.                                    ELTTHER2
01852                                                                   ELTTHER2
01853                                                                   ELTTHER2
01854 /*****************************************************************ELTTHER2
01855 *  P R O F.   C H A R G E S   O N   H O S P I T A L   B I L L     ELTTHER2
01856 *  3150-                                                          ELTTHER2
01857 ******************************************************************ELTTHER2
01858  3150-PROF-CHGR-HSP-CLM.                                          ELTTHER2
01859                                                                   ELTTHER2
01860      SET PLT-INDEX2  TO  1.                                       ELTTHER2
01861                                                                   ELTTHER2
01862      IF (PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)  NOT  =  ZERO       ELTTHER2
01863         AND (WS-DISPLAY-B-FORMAT-TEXT  =  'Y')                    ELTTHER2
01864         AND (PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
01865                   NOT  =  '0'  AND  NOT  =  LOW-VALUES))          ELTTHER2
01866                MOVE 2 TO  WS-CIA                                  ELTTHER2
01867                MOVE WS-HOLD-PROF-CHRG-MSG                         ELTTHER2
01868                               TO COF-DTL-LINE (WS-CIA)            ELTTHER2
01869                MOVE 'Y'         TO  CALL-ELUOUTPT-IND             ELTTHER2
01870                                                                   ELTTHER2
01871                IF DISPLAY-BAS-SUP = 'Y'                           ELTTHER2
01872                    ADD 1 TO WS-CIA                                ELTTHER2
01873                    MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)     ELTTHER2
01874                END-IF                                             ELTTHER2
01875                                                                   ELTTHER2
01876                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTTHER2
01877                MOVE 'PROF-CHRG-HSP-CLM'                           ELTTHER2
01878                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTTHER2
01879                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTTHER2
01880                                   TO  CMF-CODE-VALUE              ELTTHER2
01881                                                                   ELTTHER2
01882                PERFORM 9300-CALL-CODES-MANUAL                     ELTTHER2
01883                                                                   ELTTHER2
01884                MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79        ELTTHER2
01885                IF FIRST-CHAR-SHOW-AS-IS                           ELTTHER2
01886                    MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)       ELTTHER2
01887                    PERFORM 9400-MOVE-TO-COFDTL                    ELTTHER2
01888                         VARYING WS-SUB-CMF FROM 1 BY 1            ELTTHER2
01889                         UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES    ELTTHER2
01890                ELSE                                               ELTTHER2
01891                    PERFORM 9400-COMPRESS-STRING-MOVE              ELTTHER2
01892                END-IF                                             ELTTHER2
01893      END-IF.                                                      ELTTHER2
01894                                                                   ELTTHER2
01895      SET PLT-INDEX2  TO  2.                                       ELTTHER2
01896                                                                   ELTTHER2
01897      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTTHER2
01898         AND WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTTHER2
01899         AND (PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
01900                         NOT  =  '0'  AND  NOT  =  LOW-VALUES)     ELTTHER2
01901         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
01902             MOVE 2 TO WS-CIA                                      ELTTHER2
01903             MOVE WS-HOLD-PROF-CHRG-MSG TO COF-DTL-LINE (WS-CIA)   ELTTHER2
01904             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHER2
01905                                                                   ELTTHER2
01906         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
01907             ADD 1 TO WS-CIA                                       ELTTHER2
01908             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
01909         END-IF                                                    ELTTHER2
01910         MOVE 'BPB'                TO  CMF-RECORD-PREFIX           ELTTHER2
01911         MOVE 'PROF-CHRG-HSP-CLM'                                  ELTTHER2
01912                            TO         CMF-ELEMENT-SYSTEM-NAME     ELTTHER2
01913         MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
01914                            TO         CMF-CODE-VALUE              ELTTHER2
01915                                                                   ELTTHER2
01916         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
01917         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
01918                                                                   ELTTHER2
01919         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
01920             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
01921             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
01922                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
01923                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
01924         ELSE                                                      ELTTHER2
01925             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
01926         END-IF                                                    ELTTHER2
01927      END-IF.                                                      ELTTHER2
01928                                                                   ELTTHER2
01929      IF YES-CALL-ELUOUTPT                                         ELTTHER2
01930         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
01931         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
01932      END-IF.                                                      ELTTHER2
01933                                                                   ELTTHER2
01934 /*****************************************************************ELTTHER2
01935 *     E L I G I B L E   M E T H O D   T R E A T M E N T           ELTTHER2
01936 * 3200- (V)                                                       ELTTHER2
01937 ******************************************************************ELTTHER2
01938  3200-ELIG-METHOD-TREAT.                                          ELTTHER2
01939                                                                   ELTTHER2
01940      SET PLT-INDEX2  TO  1.                                       ELTTHER2
01941      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                      ELTTHER2
01942         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
01943         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHER2
01944                                                       NOT = '0'   ELTTHER2
01945         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHER2
01946                                                NOT = LOW-VALUES   ELTTHER2
01947         MOVE 2 TO WS-CIA                                          ELTTHER2
01948         MOVE WS-ELIG-METHOD-OF-TREAT TO COF-DTL-LINE(WS-CIA)      ELTTHER2
01949         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
01950                                                                   ELTTHER2
01951         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
01952             ADD 1 TO WS-CIA                                       ELTTHER2
01953             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
01954         END-IF                                                    ELTTHER2
01955                                                                   ELTTHER2
01956         MOVE 'BPB' TO CMF-RECORD-PREFIX                           ELTTHER2
01957         MOVE 'ELIG-METHD-OF-TREAT-IND'                            ELTTHER2
01958                          TO CMF-ELEMENT-SYSTEM-NAME               ELTTHER2
01959         MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHER2
01960                          TO CMF-CODE-VALUE                        ELTTHER2
01961                                                                   ELTTHER2
01962         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
01963         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
01964                                                                   ELTTHER2
01965         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
01966             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
01967             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
01968                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
01969                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
01970         ELSE                                                      ELTTHER2
01971             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
01972         END-IF                                                    ELTTHER2
01973      END-IF.                                                      ELTTHER2
01974                                                                   ELTTHER2
01975      SET PLT-INDEX2  TO  2.                                       ELTTHER2
01976      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                      ELTTHER2
01977         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
01978         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHER2
01979                                          NOT = '0'                ELTTHER2
01980         AND PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)   ELTTHER2
01981                                          NOT = LOW-VALUES         ELTTHER2
01982                                                                   ELTTHER2
01983         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
01984             MOVE 2 TO WS-CIA                                      ELTTHER2
01985             MOVE WS-ELIG-METHOD-OF-TREAT TO COF-DTL-LINE(WS-CIA)  ELTTHER2
01986             MOVE 'Y'  TO  CALL-ELUOUTPT-IND                       ELTTHER2
01987         END-IF                                                    ELTTHER2
01988                                                                   ELTTHER2
01989         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
01990             ADD 1 TO WS-CIA                                       ELTTHER2
01991             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
01992         END-IF                                                    ELTTHER2
01993                                                                   ELTTHER2
01994         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHER2
01995         MOVE 'ELIG-METHD-OF-TREAT-IND' TO  CMF-ELEMENT-SYSTEM-NAMEELTTHER2
01996         MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHER2
01997                                                TO   CMF-CODE-VALUEELTTHER2
01998                                                                   ELTTHER2
01999         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02000         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02001                                                                   ELTTHER2
02002         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02003             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02004             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02005                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02006                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02007         ELSE                                                      ELTTHER2
02008             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02009         END-IF                                                    ELTTHER2
02010      END-IF.                                                      ELTTHER2
02011                                                                   ELTTHER2
02012      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02013         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02014         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02015      END-IF.                                                      ELTTHER2
02016                                                                   ELTTHER2
02017                                                                   ELTTHER2
02018 /*****************************************************************ELTTHER2
02019 *   TRANSFER TO OTHER RESPONSIBILITY INDICATOR                    ELTTHER2
02020 * 3250- (V)                                                       ELTTHER2
02021 ******************************************************************ELTTHER2
02022  3250-TRANSFER-OTHER-RESPON-IND.                                  ELTTHER2
02023                                                                   ELTTHER2
02024      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)          =  ZEROES    ELTTHER2
02025         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZEROES    ELTTHER2
02026            SET PLT-INDEX2  TO  2                                  ELTTHER2
02027         ELSE                                                      ELTTHER2
02028            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
02029            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
02030            GO TO 3250-EXIT                                        ELTTHER2
02031      ELSE                                                         ELTTHER2
02032         SET PLT-INDEX2  TO  1.                                    ELTTHER2
02033                                                                   ELTTHER2
02034      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZERO ELTTHER2
02035         OR PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02036                                                   = LOW-VALUES    ELTTHER2
02037         GO TO 3250-EXIT.                                          ELTTHER2
02038                                                                   ELTTHER2
02039      MOVE 2 TO WS-CIA.                                            ELTTHER2
02040      MOVE 'BP'                     TO  CMF-RECORD-PREFIX.         ELTTHER2
02041      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTTHER2
02042      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTTHER2
02043           TO CMF-CODE-VALUE.                                      ELTTHER2
02044      PERFORM 9300-CALL-CODES-MANUAL                               ELTTHER2
02045      MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79.                 ELTTHER2
02046                                                                   ELTTHER2
02047      IF FIRST-CHAR-SHOW-AS-IS                                     ELTTHER2
02048          MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)                 ELTTHER2
02049          PERFORM 9400-MOVE-TO-COFDTL                              ELTTHER2
02050               VARYING WS-SUB-CMF FROM 1 BY 1                      ELTTHER2
02051               UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES              ELTTHER2
02052      ELSE                                                         ELTTHER2
02053          PERFORM 9400-COMPRESS-STRING-MOVE                        ELTTHER2
02054      END-IF.                                                      ELTTHER2
02055                                                                   ELTTHER2
02056      MOVE 'N' TO  CALL-ELUOUTPT-IND.                              ELTTHER2
02057      PERFORM 9200-TEXT-OUTPUT-REQUEST.                            ELTTHER2
02058  3250-EXIT.  EXIT.                                                ELTTHER2
02059 /*****************************************************************ELTTHER2
02060 *      H O S P I T A L   C O N D I T I O N   R E L .   I N D .    ELTTHER2
02061 * 3300-   (V2)                                                    ELTTHER2
02062 ******************************************************************ELTTHER2
02063  3300-HOSP-COND-REL-IND.                                          ELTTHER2
02064                                                                   ELTTHER2
02065      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02066      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHER2
02067         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02068         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02069                                 NOT = '0'                         ELTTHER2
02070         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02071                                 NOT = LOW-VALUES                  ELTTHER2
02072         MOVE 2 TO WS-CIA                                          ELTTHER2
02073         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHER2
02074         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
02075                                                                   ELTTHER2
02076         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02077             ADD 1 TO WS-CIA                                       ELTTHER2
02078             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02079         END-IF                                                    ELTTHER2
02080                                                                   ELTTHER2
02081         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02082         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHER2
02083         MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02084                                                     CMF-CODE-VALUEELTTHER2
02085                                                                   ELTTHER2
02086         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02087         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02088                                                                   ELTTHER2
02089         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02090             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02091             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02092                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02093                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02094         ELSE                                                      ELTTHER2
02095             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02096         END-IF                                                    ELTTHER2
02097      END-IF.                                                      ELTTHER2
02098                                                                   ELTTHER2
02099      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHER2
02100         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02101         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02102                                 NOT = '0'                         ELTTHER2
02103         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02104                                 NOT = LOW-VALUES                  ELTTHER2
02105         MOVE 2 TO WS-CIA                                          ELTTHER2
02106         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHER2
02107         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
02108                                                                   ELTTHER2
02109         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02110             ADD 1 TO WS-CIA                                       ELTTHER2
02111             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02112         END-IF                                                    ELTTHER2
02113                                                                   ELTTHER2
02114         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02115         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHER2
02116         MOVE PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02117                                                     CMF-CODE-VALUEELTTHER2
02118                                                                   ELTTHER2
02119         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02120         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02121                                                                   ELTTHER2
02122         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02123             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02124             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02125                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02126                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02127         ELSE                                                      ELTTHER2
02128             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02129         END-IF                                                    ELTTHER2
02130      END-IF.                                                      ELTTHER2
02131                                                                   ELTTHER2
02132      SET PLT-INDEX2  TO  2.                                       ELTTHER2
02133                                                                   ELTTHER2
02134      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHER2
02135         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02136         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02137                                 NOT = '0'                         ELTTHER2
02138         AND PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02139                                 NOT = LOW-VALUES                  ELTTHER2
02140         MOVE 2 TO WS-CIA                                          ELTTHER2
02141         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHER2
02142         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
02143                                                                   ELTTHER2
02144         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02145             ADD 1 TO WS-CIA                                       ELTTHER2
02146             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
02147         END-IF                                                    ELTTHER2
02148                                                                   ELTTHER2
02149         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02150         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHER2
02151         MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02152                                                     CMF-CODE-VALUEELTTHER2
02153                                                                   ELTTHER2
02154         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02155         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02156                                                                   ELTTHER2
02157         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02158             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02159             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02160                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02161                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02162         ELSE                                                      ELTTHER2
02163             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02164         END-IF                                                    ELTTHER2
02165      END-IF.                                                      ELTTHER2
02166                                                                   ELTTHER2
02167      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHER2
02168         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02169         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02170                                 NOT = '0'                         ELTTHER2
02171         AND PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)     ELTTHER2
02172                                 NOT = LOW-VALUES                  ELTTHER2
02173         MOVE 2 TO WS-CIA                                          ELTTHER2
02174         MOVE WS-THE TO COF-DTL-LINE(WS-CIA)                       ELTTHER2
02175         MOVE 'Y' TO CALL-ELUOUTPT-IND                             ELTTHER2
02176                                                                   ELTTHER2
02177         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02178             ADD 1 TO WS-CIA                                       ELTTHER2
02179             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
02180         END-IF                                                    ELTTHER2
02181                                                                   ELTTHER2
02182         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02183         MOVE 'HOSP-COND-RELATSP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTTHER2
02184         MOVE PLA-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02185                                                     CMF-CODE-VALUEELTTHER2
02186                                                                   ELTTHER2
02187         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02188         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02189                                                                   ELTTHER2
02190         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02191             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02192             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02193                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02194                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02195         ELSE                                                      ELTTHER2
02196             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02197         END-IF                                                    ELTTHER2
02198      END-IF.                                                      ELTTHER2
02199                                                                   ELTTHER2
02200      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02201         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02202         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02203      END-IF.                                                      ELTTHER2
02204                                                                   ELTTHER2
02205                                                                   ELTTHER2
02206 ****************************************************************  ELTTHER2
02207 * 3350-TREATEMENT RESTRICTION INDICATOR                        *  ELTTHER2
02208 ****************************************************************  ELTTHER2
02209  3350-TREAT-RESTRN.                                               ELTTHER2
02210                                                                   ELTTHER2
02211      SET  PLT-INDEX2  TO  1.                                      ELTTHER2
02212      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTTHER2
02213         AND                                                       ELTTHER2
02214        (PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)             ELTTHER2
02215             NOT = ZERO AND NOT = LOW-VALUES)                      ELTTHER2
02216          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTTHER2
02217          MOVE +2                    TO  WS-CIA                    ELTTHER2
02218          MOVE WS-TREAT-RESTRN       TO  COF-DTL-LINE (WS-CIA)     ELTTHER2
02219          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02220          MOVE 'TREAT-RESTRN-IND'                                  ELTTHER2
02221                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTTHER2
02222          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02223                TO  CMF-CODE-VALUE                                 ELTTHER2
02224          PERFORM 9300-CALL-CODES-MANUAL                           ELTTHER2
02225 ******                                                            ELTTHER2
02226          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTTHER2
02227                                                                   ELTTHER2
02228          IF DISPLAY-BAS-SUP = 'Y'                                 ELTTHER2
02229              ADD 1 TO WS-CIA                                      ELTTHER2
02230              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTTHER2
02231          END-IF                                                   ELTTHER2
02232                                                                   ELTTHER2
02233          IF FIRST-CHAR-SHOW-AS-IS                                 ELTTHER2
02234             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTTHER2
02235             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02236                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTTHER2
02237                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTTHER2
02238          ELSE                                                     ELTTHER2
02239              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTTHER2
02240          END-IF                                                   ELTTHER2
02241      END-IF.                                                      ELTTHER2
02242 ******                                                            ELTTHER2
02243                                                                   ELTTHER2
02244      SET  PLT-INDEX2  TO  2.                                      ELTTHER2
02245      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHER2
02246         AND (PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)        ELTTHER2
02247             NOT = ZERO AND NOT = LOW-VALUES)                      ELTTHER2
02248         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
02249             MOVE 'Y'                TO  CALL-ELUOUTPT-IND         ELTTHER2
02250             MOVE +2                 TO  WS-CIA                    ELTTHER2
02251             MOVE WS-TREAT-RESTRN    TO  COF-DTL-LINE (WS-CIA)     ELTTHER2
02252         END-IF                                                    ELTTHER2
02253                                                                   ELTTHER2
02254          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02255          MOVE 'TREAT-RESTRN-IND'                                  ELTTHER2
02256                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTTHER2
02257          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02258               TO  CMF-CODE-VALUE                                  ELTTHER2
02259          PERFORM 9300-CALL-CODES-MANUAL                           ELTTHER2
02260 ******                                                            ELTTHER2
02261          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTTHER2
02262                                                                   ELTTHER2
02263          IF DISPLAY-BAS-SUP = 'Y'                                 ELTTHER2
02264              ADD 1 TO WS-CIA                                      ELTTHER2
02265              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTTHER2
02266          END-IF                                                   ELTTHER2
02267                                                                   ELTTHER2
02268          IF FIRST-CHAR-SHOW-AS-IS                                 ELTTHER2
02269             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTTHER2
02270             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02271                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTTHER2
02272                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTTHER2
02273          ELSE                                                     ELTTHER2
02274              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTTHER2
02275          END-IF                                                   ELTTHER2
02276      END-IF.                                                      ELTTHER2
02277 ******                                                            ELTTHER2
02278                                                                   ELTTHER2
02279      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02280         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02281          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTTHER2
02282          MOVE 0 TO WS-CIA.                                        ELTTHER2
02283                                                                   ELTTHER2
02284      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
02285         SET PLT-INDEX2  TO  2                                     ELTTHER2
02286      ELSE                                                         ELTTHER2
02287         SET PLT-INDEX2  TO  1.                                    ELTTHER2
02288                                                                   ELTTHER2
02289 ****************************************************************  ELTTHER2
02290 *       C E R T I F I C A T E  R  E Q U I R E M E N T          *  ELTTHER2
02291 ****************************************************************  ELTTHER2
02292  3360-CERT-REQ-IND.                                               ELTTHER2
02293                                                                   ELTTHER2
02294      SET  PLT-INDEX2  TO  1.                                      ELTTHER2
02295      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTTHER2
02296         AND                                                       ELTTHER2
02297        (PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTTHER2
02298               NOT = ZERO AND NOT = LOW-VALUES)                    ELTTHER2
02299                                                                   ELTTHER2
02300          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTTHER2
02301          MOVE +2                    TO  WS-CIA                    ELTTHER2
02302          MOVE WS-CERT-REQ TO  COF-DTL-LINE (WS-CIA)               ELTTHER2
02303                                                                   ELTTHER2
02304          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02305          MOVE 'CERTFN-REQRM-IND'                                  ELTTHER2
02306                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTTHER2
02307          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02308                TO  CMF-CODE-VALUE                                 ELTTHER2
02309          PERFORM 9300-CALL-CODES-MANUAL                           ELTTHER2
02310                                                                   ELTTHER2
02311 ******                                                            ELTTHER2
02312          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTTHER2
02313                                                                   ELTTHER2
02314          IF DISPLAY-BAS-SUP = 'Y'                                 ELTTHER2
02315              ADD 1 TO WS-CIA                                      ELTTHER2
02316              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTTHER2
02317          END-IF                                                   ELTTHER2
02318                                                                   ELTTHER2
02319          IF FIRST-CHAR-SHOW-AS-IS                                 ELTTHER2
02320             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTTHER2
02321             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02322                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTTHER2
02323                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTTHER2
02324          ELSE                                                     ELTTHER2
02325              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTTHER2
02326          END-IF                                                   ELTTHER2
02327      END-IF.                                                      ELTTHER2
02328 ******                                                            ELTTHER2
02329                                                                   ELTTHER2
02330      SET  PLT-INDEX2  TO  2.                                      ELTTHER2
02331      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTTHER2
02332         AND (PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTTHER2
02333               NOT  =  ZERO AND NOT = LOW-VALUES)                  ELTTHER2
02334          IF NOT YES-CALL-ELUOUTPT                                 ELTTHER2
02335              MOVE 'Y'               TO  CALL-ELUOUTPT-IND         ELTTHER2
02336              MOVE +2                TO  WS-CIA                    ELTTHER2
02337              MOVE WS-CERT-REQ       TO  COF-DTL-LINE (WS-CIA)     ELTTHER2
02338          END-IF                                                   ELTTHER2
02339                                                                   ELTTHER2
02340          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02341          MOVE 'CERTFN-REQRM-IND'                                  ELTTHER2
02342                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTTHER2
02343          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02344               TO  CMF-CODE-VALUE                                  ELTTHER2
02345          PERFORM 9300-CALL-CODES-MANUAL                           ELTTHER2
02346 ******                                                            ELTTHER2
02347          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTTHER2
02348                                                                   ELTTHER2
02349          IF DISPLAY-BAS-SUP = 'Y'                                 ELTTHER2
02350              ADD 1 TO WS-CIA                                      ELTTHER2
02351              MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02352          END-IF                                                   ELTTHER2
02353                                                                   ELTTHER2
02354          IF FIRST-CHAR-SHOW-AS-IS                                 ELTTHER2
02355             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTTHER2
02356             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02357                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTTHER2
02358                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTTHER2
02359          ELSE                                                     ELTTHER2
02360              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTTHER2
02361          END-IF                                                   ELTTHER2
02362      END-IF.                                                      ELTTHER2
02363 ******                                                            ELTTHER2
02364                                                                   ELTTHER2
02365      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02366         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02367         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02368         MOVE 0 TO WS-CIA                                          ELTTHER2
02369      END-IF.                                                      ELTTHER2
02370                                                                   ELTTHER2
02371      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
02372         SET PLT-INDEX2  TO  2                                     ELTTHER2
02373      ELSE                                                         ELTTHER2
02374         SET PLT-INDEX2  TO  1.                                    ELTTHER2
02375                                                                   ELTTHER2
02376 /*****************************************************************ELTTHER2
02377 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C    ELTTHER2
02378 **   AND                                                          ELTTHER2
02379 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C      ELTTHER2
02380 **   3400- (V)                                                    ELTTHER2
02381 ******************************************************************ELTTHER2
02382  3400-SPILLOVER-COINS-N-DEDBL.                                    ELTTHER2
02383                                                                   ELTTHER2
02384      SET PLT-INDEX2  TO  2.                                       ELTTHER2
02385      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTTHER2
02386         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTTHER2
02387                                                       NOT =  '0'  ELTTHER2
02388         AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTTHER2
02389                                         NOT = LOW-VALUES          ELTTHER2
02390         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02391         MOVE 2 TO WS-CIA                                          ELTTHER2
02392         MOVE WS-SPILLOVER TO COF-DTL-LINE (WS-CIA)                ELTTHER2
02393         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
02394         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTTHER2
02395                                           CMF-ELEMENT-SYSTEM-NAME ELTTHER2
02396         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTTHER2
02397                                           TO   CMF-CODE-VALUE     ELTTHER2
02398                                                                   ELTTHER2
02399         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02400         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02401                                                                   ELTTHER2
02402         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02403             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02404             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02405                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02406                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02407         ELSE                                                      ELTTHER2
02408             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02409         END-IF                                                    ELTTHER2
02410      END-IF.                                                      ELTTHER2
02411 **                                                               |ELTTHER2
02412 **---------------------------------------------------------------+ELTTHER2
02413 **                                                               |ELTTHER2
02414                                                                   ELTTHER2
02415      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTTHER2
02416         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTTHER2
02417                                                       NOT =  '0'  ELTTHER2
02418         AND PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTTHER2
02419              NOT = LOW-VALUES                                     ELTTHER2
02420         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02421         ADD 1 TO WS-CIA                                           ELTTHER2
02422         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTTHER2
02423         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTTHER2
02424         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTTHER2
02425         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTTHER2
02426                                                     CMF-CODE-VALUEELTTHER2
02427                                                                   ELTTHER2
02428         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02429         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02430                                                                   ELTTHER2
02431         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02432             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02433             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02434                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02435                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02436         ELSE                                                      ELTTHER2
02437             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02438         END-IF                                                    ELTTHER2
02439      END-IF.                                                      ELTTHER2
02440                                                                   ELTTHER2
02441      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02442         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02443         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02444      END-IF.                                                      ELTTHER2
02445                                                                   ELTTHER2
02446 /*****************************************************************ELTTHER2
02447 *      H O S P I T A L   A D M I S S I O N   R E S T R I C T I O NELTTHER2
02448 **   3500- (V)                                                    ELTTHER2
02449 ******************************************************************ELTTHER2
02450  3500-HOSP-ADM-RESTRN.                                            ELTTHER2
02451                                                                   ELTTHER2
02452      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02453      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHER2
02454         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02455         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02456                              NOT = '0'                            ELTTHER2
02457         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02458                              NOT = LOW-VALUES                     ELTTHER2
02459                                                                   ELTTHER2
02460         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02461         MOVE 2 TO WS-CIA                                          ELTTHER2
02462                                                                   ELTTHER2
02463         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02464         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02465         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02466                                                     CMF-CODE-VALUEELTTHER2
02467         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02468                                                                   ELTTHER2
02469         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02470         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02471             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02472             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02473                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02474                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02475         ELSE                                                      ELTTHER2
02476             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02477         END-IF                                                    ELTTHER2
02478                                                                   ELTTHER2
02479         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02480                                                 WS-MUST-BEGIN-DAYSELTTHER2
02481         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02482             ADD 1 TO WS-CIA                                       ELTTHER2
02483             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02484         END-IF                                                    ELTTHER2
02485         ADD 1 TO WS-CIA                                           ELTTHER2
02486         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02487                                                                   ELTTHER2
02488      END-IF.                                                      ELTTHER2
02489 *   *****************                                             ELTTHER2
02490                                                                   ELTTHER2
02491      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHER2
02492         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02493         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02494                              NOT = '0'                            ELTTHER2
02495         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02496                              NOT = LOW-VALUES                     ELTTHER2
02497                                                                   ELTTHER2
02498         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02499         MOVE 2 TO WS-CIA                                          ELTTHER2
02500                                                                   ELTTHER2
02501         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02502         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02503         MOVE PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02504                                                     CMF-CODE-VALUEELTTHER2
02505         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02506                                                                   ELTTHER2
02507         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02508         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02509             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02510             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02511                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02512                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02513         ELSE                                                      ELTTHER2
02514             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02515         END-IF                                                    ELTTHER2
02516                                                                   ELTTHER2
02517         MOVE PLA-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02518                                                 WS-MUST-BEGIN-DAYSELTTHER2
02519         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02520             ADD 1 TO WS-CIA                                       ELTTHER2
02521             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02522         END-IF                                                    ELTTHER2
02523         ADD 1 TO WS-CIA                                           ELTTHER2
02524         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02525                                                                   ELTTHER2
02526      END-IF.                                                      ELTTHER2
02527 *   *****************                                             ELTTHER2
02528                                                                   ELTTHER2
02529      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'E'                    ELTTHER2
02530         AND PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02531         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02532                              NOT = '0'                            ELTTHER2
02533         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02534                              NOT = LOW-VALUES                     ELTTHER2
02535                                                                   ELTTHER2
02536         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02537         MOVE 2 TO WS-CIA                                          ELTTHER2
02538                                                                   ELTTHER2
02539         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02540         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02541         MOVE PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02542                                                     CMF-CODE-VALUEELTTHER2
02543         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02544                                                                   ELTTHER2
02545         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02546         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02547             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02548             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02549                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02550                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02551         ELSE                                                      ELTTHER2
02552             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02553         END-IF                                                    ELTTHER2
02554                                                                   ELTTHER2
02555         MOVE PLE-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02556                                                 WS-MUST-BEGIN-DAYSELTTHER2
02557         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02558             ADD 1 TO WS-CIA                                       ELTTHER2
02559             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02560         END-IF                                                    ELTTHER2
02561         ADD 1 TO WS-CIA                                           ELTTHER2
02562         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02563                                                                   ELTTHER2
02564      END-IF.                                                      ELTTHER2
02565 *   *****************                                             ELTTHER2
02566      SET PLT-INDEX2  TO  2.                                       ELTTHER2
02567                                                                   ELTTHER2
02568      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'B'                    ELTTHER2
02569         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02570         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02571                              NOT = '0'                            ELTTHER2
02572         AND PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02573                              NOT = LOW-VALUES                     ELTTHER2
02574                                                                   ELTTHER2
02575         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02576         MOVE 2 TO WS-CIA                                          ELTTHER2
02577                                                                   ELTTHER2
02578         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02579         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02580         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02581                                                     CMF-CODE-VALUEELTTHER2
02582         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02583                                                                   ELTTHER2
02584         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02585         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02586             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02587             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02588                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02589                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02590         ELSE                                                      ELTTHER2
02591             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02592         END-IF                                                    ELTTHER2
02593                                                                   ELTTHER2
02594         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02595                                                 WS-MUST-BEGIN-DAYSELTTHER2
02596         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02597             ADD 1 TO WS-CIA                                       ELTTHER2
02598             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02599         END-IF                                                    ELTTHER2
02600         ADD 1 TO WS-CIA                                           ELTTHER2
02601         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02602                                                                   ELTTHER2
02603      END-IF.                                                      ELTTHER2
02604 *   *****************                                             ELTTHER2
02605                                                                   ELTTHER2
02606      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'A'                    ELTTHER2
02607         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02608         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02609                              NOT = '0'                            ELTTHER2
02610         AND PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02611                              NOT = LOW-VALUES                     ELTTHER2
02612                                                                   ELTTHER2
02613         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02614         MOVE 2 TO WS-CIA                                          ELTTHER2
02615                                                                   ELTTHER2
02616         MOVE 'BPA'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02617         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02618         MOVE PLA-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02619                                                     CMF-CODE-VALUEELTTHER2
02620         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02621                                                                   ELTTHER2
02622         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02623         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02624             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02625             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02626                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02627                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02628         ELSE                                                      ELTTHER2
02629             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02630         END-IF                                                    ELTTHER2
02631                                                                   ELTTHER2
02632         MOVE PLA-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02633                                                 WS-MUST-BEGIN-DAYSELTTHER2
02634         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02635             ADD 1 TO WS-CIA                                       ELTTHER2
02636             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02637         END-IF                                                    ELTTHER2
02638         ADD 1 TO WS-CIA                                           ELTTHER2
02639         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02640                                                                   ELTTHER2
02641      END-IF.                                                      ELTTHER2
02642 *   *****************                                             ELTTHER2
02643                                                                   ELTTHER2
02644      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)  =  'E'                    ELTTHER2
02645         AND PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES     ELTTHER2
02646         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02647                              NOT = '0'                            ELTTHER2
02648         AND PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)       ELTTHER2
02649                              NOT = LOW-VALUES                     ELTTHER2
02650                                                                   ELTTHER2
02651         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02652         MOVE 2 TO WS-CIA                                          ELTTHER2
02653                                                                   ELTTHER2
02654         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02655         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTTHER2
02656         MOVE PLE-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02657                                                     CMF-CODE-VALUEELTTHER2
02658         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02659                                                                   ELTTHER2
02660         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02661         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02662             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02663             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02664                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02665                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02666         ELSE                                                      ELTTHER2
02667             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02668         END-IF                                                    ELTTHER2
02669                                                                   ELTTHER2
02670         MOVE PLE-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)  TO  ELTTHER2
02671                                                 WS-MUST-BEGIN-DAYSELTTHER2
02672         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02673             ADD 1 TO WS-CIA                                       ELTTHER2
02674             MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02675         END-IF                                                    ELTTHER2
02676         ADD 1 TO WS-CIA                                           ELTTHER2
02677         MOVE WS-AND-MUST-BEGIN  TO  COF-DTL-LINE (WS-CIA)         ELTTHER2
02678                                                                   ELTTHER2
02679      END-IF.                                                      ELTTHER2
02680      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02681         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02682         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02683      END-IF.                                                      ELTTHER2
02684                                                                   ELTTHER2
02685 /*****************************************************************ELTTHER2
02686 *      H O S P I T A L   A D M I S S I O N   R E S T R I C T I O NELTTHER2
02687 *                F O R    'C R P O  B'  P R O V I S I O N         ELTTHER2
02688 **   3501-                                                        ELTTHER2
02689 ******************************************************************ELTTHER2
02690  3501-PRIOR-ADMIS-REQ.                                            ELTTHER2
02691                                                                   ELTTHER2
02692      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02693      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZEROES    ELTTHER2
02694                         AND                                       ELTTHER2
02695         GCG-BC-CARD-REH-PRIOR-ADM  NOT  =  '0'                    ELTTHER2
02696         MOVE 'Y'            TO  CALL-ELUOUTPT-IND                 ELTTHER2
02697         MOVE 2 TO WS-CIA                                          ELTTHER2
02698         MOVE WS-PRIOR-ADM TO COF-DTL-LINE (WS-CIA)                ELTTHER2
02699                                                                   ELTTHER2
02700         MOVE 'GROUP'        TO  CMF-RECORD-PREFIX                 ELTTHER2
02701         MOVE 'BC-CARD-REH-PRIOR-ADM'                              ELTTHER2
02702                             TO  CMF-ELEMENT-SYSTEM-NAME           ELTTHER2
02703         MOVE GCG-BC-CARD-REH-PRIOR-ADM  TO  CMF-CODE-VALUE        ELTTHER2
02704                                                                   ELTTHER2
02705         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02706             ADD 1 TO WS-CIA                                       ELTTHER2
02707             MOVE WS-FOR-BASIC TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02708         END-IF                                                    ELTTHER2
02709                                                                   ELTTHER2
02710         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02711                                                                   ELTTHER2
02712         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02713         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02714             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02715             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02716                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02717                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02718         ELSE                                                      ELTTHER2
02719             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02720         END-IF                                                    ELTTHER2
02721                                                                   ELTTHER2
02722      SET PLT-INDEX2  TO  2.                                       ELTTHER2
02723      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROS      ELTTHER2
02724                         AND                                       ELTTHER2
02725         GCG-MM-CARD-REH-PRIOR-ADM  NOT  =  '0'                    ELTTHER2
02726                         AND                                       ELTTHER2
02727         DISPLAY-BAS-SUP = 'Y'                                     ELTTHER2
02728         IF CALL-ELUOUTPT-IND = 'N'                                ELTTHER2
02729             MOVE 2 TO WS-CIA                                      ELTTHER2
02730             MOVE WS-PRIOR-ADM TO COF-DTL-LINE (WS-CIA)            ELTTHER2
02731         END-IF                                                    ELTTHER2
02732                                                                   ELTTHER2
02733         MOVE 'Y'            TO  CALL-ELUOUTPT-IND                 ELTTHER2
02734         MOVE 'GROUP'        TO  CMF-RECORD-PREFIX                 ELTTHER2
02735         MOVE 'MM-CARD-REH-PRIOR-ADM'                              ELTTHER2
02736                             TO  CMF-ELEMENT-SYSTEM-NAME           ELTTHER2
02737         MOVE GCG-MM-CARD-REH-PRIOR-ADM  TO  CMF-CODE-VALUE        ELTTHER2
02738                                                                   ELTTHER2
02739         ADD 1 TO WS-CIA                                           ELTTHER2
02740         MOVE WS-FOR-SUPP  TO COF-DTL-LINE (WS-CIA)                ELTTHER2
02741                                                                   ELTTHER2
02742         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02743                                                                   ELTTHER2
02744         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02745         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02746             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02747             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02748                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02749                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02750         ELSE                                                      ELTTHER2
02751             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02752         END-IF                                                    ELTTHER2
02753                                                                   ELTTHER2
02754      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02755         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02756         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02757      END-IF.                                                      ELTTHER2
02758                                                                   ELTTHER2
02759 /*****************************************************************ELTTHER2
02760 *        B E N E F I T   S C O P E   I D E N T I F I E R          ELTTHER2
02761 **   3600-                                                        ELTTHER2
02762 ******************************************************************ELTTHER2
02763  3600-BENEFIT-SCOPE-ID.                                           ELTTHER2
02764                                                                   ELTTHER2
02765      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02766      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02767         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E' AND                  ELTTHER2
02768         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTTHER2
02769                                       '0000' AND  NOT =  '00  '   ELTTHER2
02770         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02771         MOVE 2 TO  WS-CIA                                         ELTTHER2
02772         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTTHER2
02773                                                                   ELTTHER2
02774         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02775             ADD 1 TO WS-CIA                                       ELTTHER2
02776             MOVE WS-BASIC  TO COF-DTL-LINE (WS-CIA)               ELTTHER2
02777         END-IF                                                    ELTTHER2
02778                                                                   ELTTHER2
02779         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02780         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTTHER2
02781         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTTHER2
02782                                                    CMF-CODE-VALUE ELTTHER2
02783                                                                   ELTTHER2
02784         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02785                                                                   ELTTHER2
02786         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02787         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02788             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02789             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02790                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02791                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02792         ELSE                                                      ELTTHER2
02793             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02794         END-IF                                                    ELTTHER2
02795                                                                   ELTTHER2
02796      END-IF.                                                      ELTTHER2
02797                                                                   ELTTHER2
02798      SET PLT-INDEX2  TO  2.                                       ELTTHER2
02799      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02800         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E' AND                  ELTTHER2
02801         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTTHER2
02802                                       '0000' AND  NOT =  '00  '   ELTTHER2
02803         IF CALL-ELUOUTPT-IND = 'N'                                ELTTHER2
02804             MOVE 'Y' TO CALL-ELUOUTPT-IND                         ELTTHER2
02805             MOVE 2 TO WS-CIA                                      ELTTHER2
02806             MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)          ELTTHER2
02807         END-IF                                                    ELTTHER2
02808                                                                   ELTTHER2
02809         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
02810             ADD 1 TO WS-CIA                                       ELTTHER2
02811             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
02812         END-IF                                                    ELTTHER2
02813                                                                   ELTTHER2
02814         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTTHER2
02815         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTTHER2
02816         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTTHER2
02817                                                    CMF-CODE-VALUE ELTTHER2
02818                                                                   ELTTHER2
02819         PERFORM 9300-CALL-CODES-MANUAL                            ELTTHER2
02820                                                                   ELTTHER2
02821         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTTHER2
02822         IF FIRST-CHAR-SHOW-AS-IS                                  ELTTHER2
02823             MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)              ELTTHER2
02824             PERFORM 9400-MOVE-TO-COFDTL                           ELTTHER2
02825                  VARYING WS-SUB-CMF FROM 1 BY 1                   ELTTHER2
02826                  UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES           ELTTHER2
02827         ELSE                                                      ELTTHER2
02828             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTTHER2
02829         END-IF                                                    ELTTHER2
02830                                                                   ELTTHER2
02831      END-IF.                                                      ELTTHER2
02832                                                                   ELTTHER2
02833      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02834         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02835         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
02836      END-IF.                                                      ELTTHER2
02837                                                                   ELTTHER2
02838 /*****************************************************************ELTTHER2
02839 *     G E N E R A L   G E T   T A B U L A R   R T N E             ELTTHER2
02840  3700-GENERAL-TABULAR-RTNE.                                       ELTTHER2
02841 ****************************************************************  ELTTHER2
02842 *                  A A R   T A B U L A R                       *  ELTTHER2
02843 ****************************************************************  ELTTHER2
02844      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02845      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02846         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02847                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02848         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02849         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTTHER2
02850         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTTHER2
02851      ELSE                                                         ELTTHER2
02852         SET PLT-INDEX2  TO  2                                     ELTTHER2
02853         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
02854            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
02855                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02856            MOVE 'Y'  TO  CALL-ELUOUTPT-IND                        ELTTHER2
02857            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTTHER2
02858            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)          ELTTHER2
02859         END-IF                                                    ELTTHER2
02860      END-IF.                                                      ELTTHER2
02861                                                                   ELTTHER2
02862      IF YES-CALL-ELUOUTPT                                         ELTTHER2
02863         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
02864         MOVE 1  TO  WS-CIA                                        ELTTHER2
02865         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
02866             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
02867         END-EXEC                                                  ELTTHER2
02868      END-IF.                                                      ELTTHER2
02869 *--------------------------------------------------------------*  ELTTHER2
02870 *                  P P F   T A B U L A R                       *  ELTTHER2
02871 *--------------------------------------------------------------*  ELTTHER2
02872      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02873      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02874         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02875                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02876         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTTHER2
02877                                             KWA-GCTABULR-KEY      ELTTHER2
02878         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHER2
02879         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTTHER2
02880               COMMAREA(DFHCOMMAREA)                               ELTTHER2
02881         END-EXEC                                                  ELTTHER2
02882      ELSE                                                         ELTTHER2
02883         SET PLT-INDEX2  TO  2                                     ELTTHER2
02884         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
02885            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
02886                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02887            MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  ELTTHER2
02888                                          TO KWA-GCTABULR-KEY      ELTTHER2
02889            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHER2
02890            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTTHER2
02891                  COMMAREA(DFHCOMMAREA)                            ELTTHER2
02892            END-EXEC                                               ELTTHER2
02893         END-IF                                                    ELTTHER2
02894      END-IF.                                                      ELTTHER2
02895 *--------------------------------------------------------------*  ELTTHER2
02896 *                  P V E   T A B U L A R                       *  ELTTHER2
02897 *--------------------------------------------------------------*  ELTTHER2
02898                                                                   ELTTHER2
02899      MOVE  +2     TO  WS-CIA.                                     ELTTHER2
02900      MOVE WS-PROVIDER-ELIGIBILITY TO COF-DTL-LINE (2).            ELTTHER2
02901      ADD +1  WS-CIA GIVING COF-NBR-DTL-LINES.                     ELTTHER2
02902      EXEC CICS LINK                                               ELTTHER2
02903                PROGRAM('ELUOUTPT')                                ELTTHER2
02904                COMMAREA (DFHCOMMAREA)                             ELTTHER2
02905      END-EXEC.                                                    ELTTHER2
02906                                                                   ELTTHER2
02907                                                                   ELTTHER2
02908 *--------------------------------------------------------------*  ELTTHER2
02909 *                  A B M   T A B U L A R                       *  ELTTHER2
02910 *--------------------------------------------------------------*  ELTTHER2
02911      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02912      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTTHER2
02913                              AND                                  ELTTHER2
02914         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02915                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02916         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTTHER2
02917                                              KWA-GCTABULR-KEY     ELTTHER2
02918         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHER2
02919         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTTHER2
02920               COMMAREA(DFHCOMMAREA)                               ELTTHER2
02921         END-EXEC                                                  ELTTHER2
02922      ELSE                                                         ELTTHER2
02923         SET PLT-INDEX2  TO  2                                     ELTTHER2
02924         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
02925            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
02926                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02927          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02928                                             KWA-GCTABULR-KEY      ELTTHER2
02929            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHER2
02930            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTTHER2
02931                  COMMAREA(DFHCOMMAREA)                            ELTTHER2
02932            END-EXEC                                               ELTTHER2
02933         END-IF                                                    ELTTHER2
02934      END-IF.                                                      ELTTHER2
02935 *--------------------------------------------------------------*  ELTTHER2
02936 *                  A C L   T A B U L A R                       *  ELTTHER2
02937 *--------------------------------------------------------------*  ELTTHER2
02938      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02939      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02940         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02941                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02942         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHER2
02943                                              KWA-GCTABULR-KEY     ELTTHER2
02944         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHER2
02945         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTTHER2
02946               COMMAREA(DFHCOMMAREA)                               ELTTHER2
02947         END-EXEC                                                  ELTTHER2
02948      ELSE                                                         ELTTHER2
02949         SET PLT-INDEX2  TO  2                                     ELTTHER2
02950         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
02951            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
02952                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02953          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02954                                              KWA-GCTABULR-KEY     ELTTHER2
02955            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHER2
02956            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTTHER2
02957                  COMMAREA(DFHCOMMAREA)                            ELTTHER2
02958            END-EXEC                                               ELTTHER2
02959         END-IF                                                    ELTTHER2
02960      END-IF.                                                      ELTTHER2
02961 *--------------------------------------------------------------*  ELTTHER2
02962 *                  A D L   T A B U L A R                       *  ELTTHER2
02963 *--------------------------------------------------------------*  ELTTHER2
02964      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02965      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02966         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02967                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02968         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHER2
02969                                             KWA-GCTABULR-KEY      ELTTHER2
02970         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHER2
02971         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTTHER2
02972               COMMAREA(DFHCOMMAREA)                               ELTTHER2
02973         END-EXEC                                                  ELTTHER2
02974      ELSE                                                         ELTTHER2
02975         SET PLT-INDEX2  TO  2                                     ELTTHER2
02976         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
02977            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
02978                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02979          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
02980                                               KWA-GCTABULR-KEY    ELTTHER2
02981            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHER2
02982            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTTHER2
02983                  COMMAREA(DFHCOMMAREA)                            ELTTHER2
02984            END-EXEC                                               ELTTHER2
02985         END-IF                                                    ELTTHER2
02986      END-IF.                                                      ELTTHER2
02987 *--------------------------------------------------------------*  ELTTHER2
02988 *                  A O L   T A B U L A R                       *  ELTTHER2
02989 *--------------------------------------------------------------*  ELTTHER2
02990      SET PLT-INDEX2  TO  1.                                       ELTTHER2
02991      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
02992         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTTHER2
02993                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
02994         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTTHER2
02995                                             KWA-GCTABULR-KEY      ELTTHER2
02996         PERFORM 3750-GET-TABULAR-RECORD                           ELTTHER2
02997         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTTHER2
02998               COMMAREA(DFHCOMMAREA)                               ELTTHER2
02999         END-EXEC                                                  ELTTHER2
03000      ELSE                                                         ELTTHER2
03001         SET PLT-INDEX2  TO  2                                     ELTTHER2
03002         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTTHER2
03003            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
03004                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTTHER2
03005          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTTHER2
03006                                              KWA-GCTABULR-KEY     ELTTHER2
03007            PERFORM 3750-GET-TABULAR-RECORD                        ELTTHER2
03008            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTTHER2
03009                  COMMAREA(DFHCOMMAREA)                            ELTTHER2
03010            END-EXEC                                               ELTTHER2
03011         END-IF                                                    ELTTHER2
03012      END-IF.                                                      ELTTHER2
03013                                                                   ELTTHER2
03014 *--------------------------------------------------------------*  ELTTHER2
03015 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTTHER2
03016 *--------------------------------------------------------------*  ELTTHER2
03017                                                                   ELTTHER2
03018      MOVE  +1     TO  WS-CIA.                                     ELTTHER2
03019      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTTHER2
03020      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTTHER2
03021      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTTHER2
03022             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
03023      END-EXEC.                                                    ELTTHER2
03024                                                                   ELTTHER2
03025                                                                   ELTTHER2
03026 *--------------------------------------------------------------*  ELTTHER2
03027 * 3750-GET-TABULAR RECORD                                      *  ELTTHER2
03028 *--------------------------------------------------------------*  ELTTHER2
03029  3750-GET-TABULAR-RECORD.                                         ELTTHER2
03030                                                                   ELTTHER2
03031      SET CIA-GCTABULR-DDN TO TRUE.                                ELTTHER2
03032      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
03033          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTTHER2
03034      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTTHER2
03035      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTTHER2
03036      SET IOP-RD                          TO TRUE.                 ELTTHER2
03037      SET IOP-FCQ-NONE                    TO TRUE.                 ELTTHER2
03038      SET IOP-KVQ-NONE                    TO TRUE.                 ELTTHER2
03039      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTTHER2
03040                                                                   ELTTHER2
03041      EXEC CICS LINK                                               ELTTHER2
03042                PROGRAM ('ELUIOPGM')                               ELTTHER2
03043                COMMAREA (DFHCOMMAREA)                             ELTTHER2
03044      END-EXEC.                                                    ELTTHER2
03045                                                                   ELTTHER2
03046      IF IOP-RC-NOTFND                                             ELTTHER2
03047         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTTHER2
03048         EXEC CICS ABEND                                           ELTTHER2
03049                   ABCODE(CIA-ABCODE)                              ELTTHER2
03050         END-EXEC                                                  ELTTHER2
03051      ELSE                                                         ELTTHER2
03052          IF NOT IOP-RC-OK                                         ELTTHER2
03053             SET CIA-AB-CRITIO TO TRUE                             ELTTHER2
03054             EXEC CICS ABEND                                       ELTTHER2
03055                       ABCODE(CIA-ABCODE)                          ELTTHER2
03056             END-EXEC                                              ELTTHER2
03057          END-IF                                                   ELTTHER2
03058      END-IF.                                                      ELTTHER2
03059                                                                   ELTTHER2
03060 /*****************************************************************ELTTHER2
03061 *        B E N E F I T   M A X I M U M   V I S I T S              ELTTHER2
03062 * 3800-                                                           ELTTHER2
03063 ******************************************************************ELTTHER2
03064  3800-BEN-MAXIMUM-VISITS.                                         ELTTHER2
03065                                                                   ELTTHER2
03066      SET PLT-INDEX2  TO  1.                                       ELTTHER2
03067      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT =  ZERO           ELTTHER2
03068         AND                                                       ELTTHER2
03069         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHER2
03070         AND                                                       ELTTHER2
03071         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  '0'ELTTHER2
03072         AND                                                       ELTTHER2
03073         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)            ELTTHER2
03074                                     NOT = LOW-VALUES              ELTTHER2
03075                                                                   ELTTHER2
03076         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03077         MOVE 2 TO WS-CIA                                          ELTTHER2
03078         MOVE WS-MAX-NUM-OF-VISITS  TO  COF-DTL-LINE(WS-CIA)       ELTTHER2
03079                                                                   ELTTHER2
03080         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
03081             ADD 1 TO WS-CIA                                       ELTTHER2
03082             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTTHER2
03083         END-IF                                                    ELTTHER2
03084         PERFORM 3810-CK-N-MOVE                                    ELTTHER2
03085      END-IF.                                                      ELTTHER2
03086                                                                   ELTTHER2
03087      SET PLT-INDEX2  TO  2.                                       ELTTHER2
03088      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZERO           ELTTHER2
03089         AND                                                       ELTTHER2
03090         PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHER2
03091         AND                                                       ELTTHER2
03092         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  '0'ELTTHER2
03093         AND                                                       ELTTHER2
03094         PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)            ELTTHER2
03095                                     NOT = LOW-VALUES              ELTTHER2
03096                                                                   ELTTHER2
03097         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
03098            MOVE 'Y' TO CALL-ELUOUTPT-IND                          ELTTHER2
03099            ADD 2 TO WS-CIA                                        ELTTHER2
03100            MOVE WS-MAX-NUM-OF-VISITS  TO  COF-DTL-LINE(WS-CIA)    ELTTHER2
03101         END-IF                                                    ELTTHER2
03102                                                                   ELTTHER2
03103                                                                   ELTTHER2
03104         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
03105             ADD 1 TO WS-CIA                                       ELTTHER2
03106             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTTHER2
03107         END-IF                                                    ELTTHER2
03108         PERFORM 3810-CK-N-MOVE                                    ELTTHER2
03109      END-IF.                                                      ELTTHER2
03110                                                                   ELTTHER2
03111      IF YES-CALL-ELUOUTPT                                         ELTTHER2
03112         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03113         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
03114      END-IF.                                                      ELTTHER2
03115                                                                   ELTTHER2
03116                                                                   ELTTHER2
03117 /*****************************************************************ELTTHER2
03118  3810-CK-N-MOVE.                                                  ELTTHER2
03119                                                                   ELTTHER2
03120      MOVE PLE-BEN-MAX-VISITS-IND (PLT-INDEX1, PLT-INDEX2)         ELTTHER2
03121                           TO CMF-CODE-VALUE.                      ELTTHER2
03122      MOVE 'BPE'                     TO  CMF-RECORD-PREFIX.        ELTTHER2
03123      MOVE 'BEN-MAX-VISITS-IND' TO       CMF-ELEMENT-SYSTEM-NAME.  ELTTHER2
03124      PERFORM 9300-CALL-CODES-MANUAL.                              ELTTHER2
03125      ADD 1 TO WS-CIA.                                             ELTTHER2
03126                                                                   ELTTHER2
03127      IF PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1, PLT-INDEX2)          ELTTHER2
03128                                                    = ZEROS        ELTTHER2
03129         MOVE CMF-DESCR-LINE(1) TO COF-DTL-LINE(WS-CIA)            ELTTHER2
03130      ELSE                                                         ELTTHER2
03131         MOVE PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1, PLT-INDEX2)     ELTTHER2
03132              TO WS-DTL-MAX-DAYS                                   ELTTHER2
03133         MOVE CMF-DESCR-LINE(1) TO WS-DTL-MAX-IND                  ELTTHER2
03134         MOVE WS-MAX-DAYS TO COF-DTL-LINE(WS-CIA)                  ELTTHER2
03135      END-IF.                                                      ELTTHER2
03136 /*****************************************************************ELTTHER2
03137 *3900-                                                            ELTTHER2
03138 **************************************************************    ELTTHER2
03139  3900-MAX-AMOUNT-PER-VISIT.                                       ELTTHER2
03140                                                                   ELTTHER2
03141      MOVE SPACES  TO  WS-DTL-BASIC-LONG,  WS-DTL-SUPP-LONG.       ELTTHER2
03142                                                                   ELTTHER2
03143      SET PLT-INDEX2  TO  1.                                       ELTTHER2
03144      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHER2
03145         AND                                                       ELTTHER2
03146         PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTTHER2
03147         AND                                                       ELTTHER2
03148         PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2) NOT = ZERO  ELTTHER2
03149                                                                   ELTTHER2
03150         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03151         MOVE 2 TO WS-CIA                                          ELTTHER2
03152         MOVE WS-MAX-AMT-PER-VISIT  TO  COF-DTL-LINE(WS-CIA)       ELTTHER2
03153                                                                   ELTTHER2
03154         ADD  +1  TO  WS-CIA                                       ELTTHER2
03155         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO    ELTTHER2
03156                                                    WS-BASIC-AMOUNTELTTHER2
03157         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
03158            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHER2
03159         ELSE                                                      ELTTHER2
03160            MOVE WS-DTL-BASIC-LONG TO COF-DTL-LINE(WS-CIA)         ELTTHER2
03161         END-IF                                                    ELTTHER2
03162      END-IF.                                                      ELTTHER2
03163                                                                   ELTTHER2
03164      SET PLT-INDEX2  TO  2.                                       ELTTHER2
03165                                                                   ELTTHER2
03166      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTTHER2
03167         AND                                                       ELTTHER2
03168         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTTHER2
03169         AND                                                       ELTTHER2
03170         PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2) NOT = ZERO  ELTTHER2
03171                                                                   ELTTHER2
03172         IF NOT YES-CALL-ELUOUTPT                                  ELTTHER2
03173             MOVE 'Y' TO CALL-ELUOUTPT-IND                         ELTTHER2
03174             MOVE 2 TO WS-CIA                                      ELTTHER2
03175             MOVE WS-MAX-AMT-PER-VISIT TO COF-DTL-LINE(WS-CIA)     ELTTHER2
03176         END-IF                                                    ELTTHER2
03177                                                                   ELTTHER2
03178         ADD  +1  TO  WS-CIA                                       ELTTHER2
03179         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO    ELTTHER2
03180                                                WS-SUPP-AMOUNT     ELTTHER2
03181         IF DISPLAY-BAS-SUP = 'Y'                                  ELTTHER2
03182            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTTHER2
03183         ELSE                                                      ELTTHER2
03184            MOVE WS-DTL-SUPP-LONG TO COF-DTL-LINE(WS-CIA)          ELTTHER2
03185         END-IF                                                    ELTTHER2
03186      END-IF.                                                      ELTTHER2
03187                                                                   ELTTHER2
03188      IF YES-CALL-ELUOUTPT                                         ELTTHER2
03189         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03190         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTTHER2
03191      END-IF.                                                      ELTTHER2
03192                                                                   ELTTHER2
03193 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTTHER2
03194  4000-CODES-MANUAL-LONG.                                          ELTTHER2
03195                                                                   ELTTHER2
03196      INITIALIZE CMF-RETURN-CODE                                   ELTTHER2
03197                 TCAR-FROM-AREA                                    ELTTHER2
03198                 TCAR-AREA-LENGTH.                                 ELTTHER2
03199                                                                   ELTTHER2
03200      MOVE 1 TO WS-SUB2.                                           ELTTHER2
03201                                                                   ELTTHER2
03202      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTTHER2
03203                       COMMAREA(DFHCOMMAREA)                       ELTTHER2
03204      END-EXEC.                                                    ELTTHER2
03205                                                                   ELTTHER2
03206      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTTHER2
03207      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
03208          ADDRESS OF CMF-DESCR.                                    ELTTHER2
03209                                                                   ELTTHER2
03210      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTTHER2
03211         MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN                        ELTTHER2
03212         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-LINE (WS-SUB2)        ELTTHER2
03213         PERFORM 4010-MOVE-DESCRIPTION-LINES THRU 4010-EXIT        ELTTHER2
03214                VARYING CMF-DESCR-IDX FROM 1 BY 1                  ELTTHER2
03215                UNTIL CMF-DESCR-IDX GREATER THAN                   ELTTHER2
03216                               CMF-NBR-DESCR-LINES                 ELTTHER2
03217         IF  WS-TEMP2-CHARS  NOT =  LOW-VALUES                     ELTTHER2
03218            ADD 1 TO WS-SUB2                                       ELTTHER2
03219            MOVE WS-TEMP-TEXT-AREA2 TO TCAR-FROM-LINE (WS-SUB2)    ELTTHER2
03220            MOVE LOW-VALUES  TO  WS-TEMP-TEXT-AREA2                ELTTHER2
03221         END-IF                                                    ELTTHER2
03222      ELSE                                                         ELTTHER2
03223         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTTHER2
03224         PERFORM 4010-MOVE-DESCRIPTION-LINES THRU 4010-EXIT        ELTTHER2
03225                VARYING CMF-DESCR-IDX FROM 1 BY 1                  ELTTHER2
03226                UNTIL CMF-DESCR-IDX GREATER THAN                   ELTTHER2
03227                               CMF-NBR-DESCR-LINES                 ELTTHER2
03228         IF  WS-TEMP2-CHARS  NOT =  LOW-VALUES                     ELTTHER2
03229            ADD 1 TO WS-SUB2                                       ELTTHER2
03230            MOVE WS-TEMP-TEXT-AREA2 TO TCAR-FROM-LINE (WS-SUB2)    ELTTHER2
03231            MOVE LOW-VALUES  TO  WS-TEMP-TEXT-AREA2                ELTTHER2
03232         END-IF                                                    ELTTHER2
03233      END-IF.                                                      ELTTHER2
03234                                                                   ELTTHER2
03235      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTTHER2
03236                                                                   ELTTHER2
03237      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTTHER2
03238      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTTHER2
03239      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTTHER2
03240      MOVE +79  TO  TCAR-OUTPUT-FIELD-3-LEN.                       ELTTHER2
03241      MOVE +79  TO  TCAR-OUTPUT-FIELD-4-LEN.                       ELTTHER2
03242      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTTHER2
03243                                                                   ELTTHER2
03244      IF WS-MOVE-LINES-TO-CIA                                      ELTTHER2
03245         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTTHER2
03246            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTTHER2
03247                                             WS-TEMP-NOT-USED-CNT  ELTTHER2
03248            PERFORM 4050-CONCATENATE-TO-TEMP-TEXT                  ELTTHER2
03249              VARYING WS-SUB1  FROM  1  BY  1                      ELTTHER2
03250              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTTHER2
03251            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTTHER2
03252            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTTHER2
03253            ADD +1  TO  WS-CIA                                     ELTTHER2
03254         ELSE                                                      ELTTHER2
03255            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTTHER2
03256            ADD +1  TO  WS-CIA.                                    ELTTHER2
03257                                                                   ELTTHER2
03258      IF WS-MOVE-LINES-TO-CIA                                      ELTTHER2
03259         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTTHER2
03260            PERFORM 4020-MOVE-FORMATTED-TEXT THRU 4020-EXIT        ELTTHER2
03261                 VARYING WS-SUB2 FROM 2 BY 1                       ELTTHER2
03262                 UNTIL WS-SUB2 > TCAR-OUTPUT-FIELDS-USED           ELTTHER2
03263         ELSE                                                      ELTTHER2
03264            CONTINUE                                               ELTTHER2
03265      ELSE                                                         ELTTHER2
03266         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTTHER2
03267 /                                                                 ELTTHER2
03268  4010-MOVE-DESCRIPTION-LINES.                                     ELTTHER2
03269      ADD 1 TO WS-SUB2.                                            ELTTHER2
03270      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO TCAR-FROM-LINE        ELTTHER2
03271                                              (WS-SUB2).           ELTTHER2
03272  4010-EXIT.  EXIT.                                                ELTTHER2
03273  4020-MOVE-FORMATTED-TEXT.                                        ELTTHER2
03274      MOVE TCAR-OPF-DATA(WS-SUB2) TO  COF-DTL-LINE(WS-CIA).        ELTTHER2
03275      ADD +1  TO  WS-CIA.                                          ELTTHER2
03276  4020-EXIT.  EXIT.                                                ELTTHER2
03277  4050-CONCATENATE-TO-TEMP-TEXT.                                   ELTTHER2
03278      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTTHER2
03279      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTTHER2
03280                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTTHER2
03281                                                                   ELTTHER2
03282                                                                   ELTTHER2
03283                                                                   ELTTHER2
03284                                                                   ELTTHER2
03285                                                                   ELTTHER2
03286 /**************************************************************** ELTTHER2
03287 *           S A M E   P R O V I D E R   B I L L I N G             ELTTHER2
03288 *     I P   R A D I A T I O N   T H E R A P Y / M E D I C A L     ELTTHER2
03289 *                                                                 ELTTHER2
03290 ***************************************************************** ELTTHER2
03291  4200-SAME-PROV-BILL-THRP-BAS.                                    ELTTHER2
03292                                                                   ELTTHER2
03293      IF GCT-SAME-PROV-IP-RAD-THRPY-MED  NOT =  ZERO               ELTTHER2
03294            MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX                 ELTTHER2
03295            MOVE 'SAME-PROV-IP-RAD-THRPY-MED'  TO                  ELTTHER2
03296                                           CMF-ELEMENT-SYSTEM-NAME ELTTHER2
03297            MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED  TO                ELTTHER2
03298                                                    CMF-CODE-VALUE ELTTHER2
03299            MOVE WS-BASIC  TO  WS-TEMP-TEXT-AREA                   ELTTHER2
03300            MOVE +63  TO  WS-TEMP-NOT-USED-CNT                     ELTTHER2
03301            PERFORM 4000-CODES-MANUAL-LONG                         ELTTHER2
03302      END-IF.                                                      ELTTHER2
03303                                                                   ELTTHER2
03304  4250-SAME-PROV-BILL-THRP-SUP.                                    ELTTHER2
03305      IF GCT-SAME-PROV-IP-RAD-THRPY-MED  NOT =  ZERO               ELTTHER2
03306            MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX                 ELTTHER2
03307            MOVE 'SAME-PROV-IP-RAD-THRPY-MED'  TO                  ELTTHER2
03308                                           CMF-ELEMENT-SYSTEM-NAME ELTTHER2
03309            MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED  TO                ELTTHER2
03310                                                    CMF-CODE-VALUE ELTTHER2
03311            MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                ELTTHER2
03312            MOVE +63  TO  WS-TEMP-NOT-USED-CNT                     ELTTHER2
03313            PERFORM 4000-CODES-MANUAL-LONG                         ELTTHER2
03314      END-IF.                                                      ELTTHER2
03315                                                                   ELTTHER2
03316                                                                   ELTTHER2
03317 /  D A Y S   R E D U C T I O N   B A S I C   &   S E C O N D A R YELTTHER2
03318  4300-DAYS-REDCN-BASIC-SEC.                                       ELTTHER2
03319                                                                   ELTTHER2
03320      SET PLT-INDEX2  TO  1.                                       ELTTHER2
03321      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)      NOT EQUAL 'A'         ELTTHER2
03322            NEXT SENTENCE                                          ELTTHER2
03323                ELSE                                               ELTTHER2
03324      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
03325         PVN-BEN-FMT(PVN-BEN-PROVN-IDX)           =  'A' AND       ELTTHER2
03326         PLA-DAYS-RDCN-RAT-BASIC-APL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTTHER2
03327                                                         ZERO AND  ELTTHER2
03328         PLA-DAYS-RDCN-RAT-BASIC-BASE(PLT-INDEX1, PLT-INDEX2)      ELTTHER2
03329                                                      NOT =  ZERO  ELTTHER2
03330         MOVE PLA-DAYS-RDCN-RAT-BASIC-APL(PLT-INDEX1, PLT-INDEX2)  ELTTHER2
03331                                             TO  WS-BASIC-DAYS-1ST ELTTHER2
03332         MOVE PLA-DAYS-RDCN-RAT-BASIC-BASE(PLT-INDEX1, PLT-INDEX2) ELTTHER2
03333                                         TO  WS-BASIC-DAYS-FOR-1ST ELTTHER2
03334         MOVE WS-BASIC-DAYS-REDUCT-1ST  TO  COF-DTL-LINE(WS-CIA)   ELTTHER2
03335         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03336         ADD  +1  TO  WS-CIA.                                      ELTTHER2
03337                                                                   ELTTHER2
03338      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX)      NOT EQUAL 'A'         ELTTHER2
03339            NEXT SENTENCE                                          ELTTHER2
03340                ELSE                                               ELTTHER2
03341      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTTHER2
03342         PVN-BEN-FMT(PVN-BEN-PROVN-IDX)           =  'A' AND       ELTTHER2
03343         PLA-DAYS-RDCN-RAT-SEC-APL(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTTHER2
03344                                                         ZERO AND  ELTTHER2
03345         PLA-DAYS-RDCN-RAT-SEC-BASE(PLT-INDEX1, PLT-INDEX2)        ELTTHER2
03346                                                      NOT =  ZERO  ELTTHER2
03347         MOVE PLA-DAYS-RDCN-RAT-SEC-APL(PLT-INDEX1, PLT-INDEX2)    ELTTHER2
03348                                             TO  WS-BASIC-DAYS-2ND ELTTHER2
03349         MOVE PLA-DAYS-RDCN-RAT-SEC-BASE(PLT-INDEX1, PLT-INDEX2)   ELTTHER2
03350                                         TO  WS-BASIC-DAYS-FOR-2ND ELTTHER2
03351         MOVE WS-BASIC-DAYS-REDUCT-2ND  TO  COF-DTL-LINE(WS-CIA)   ELTTHER2
03352         MOVE 'Y'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03353         ADD  +1  TO  WS-CIA.                                      ELTTHER2
03354                                                                   ELTTHER2
03355      IF YES-CALL-ELUOUTPT                                         ELTTHER2
03356         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTTHER2
03357         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTTHER2
03358         MOVE 1  TO  WS-CIA                                        ELTTHER2
03359         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
03360             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
03361                                         END-EXEC.                 ELTTHER2
03362                                                                   ELTTHER2
03363      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
03364         SET PLT-INDEX2  TO  2                                     ELTTHER2
03365      ELSE                                                         ELTTHER2
03366         SET PLT-INDEX2  TO  1.                                    ELTTHER2
03367                                                                   ELTTHER2
03368  4399-EXIT.             EXIT.                                     ELTTHER2
03369                                                                   ELTTHER2
03370 /   S H O C K   T H E R A P Y   I P   I N S T I T U T I O N A L   ELTTHER2
03371 ***************************************************************** ELTTHER2
03372 *   S H O C K   T H E R A P Y   I P   I N S T I T U T I O N A L   ELTTHER2
03373 *                                                                 ELTTHER2
03374 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHER2
03375 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHER2
03376 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHER2
03377 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHER2
03378 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHER2
03379 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHER2
03380 *  MODULE.                                                        ELTTHER2
03381 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHER2
03382 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHER2
03383 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHER2
03384 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHER2
03385 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHER2
03386 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHER2
03387 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHER2
03388 *                                                                 ELTTHER2
03389 ***************************************************************** ELTTHER2
03390  5000-SPEECH-THRP-IP-INST-RTNE.                                   ELTTHER2
03391      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
03392                                                                   ELTTHER2
03393      MOVE WS-HDR-2-SPEECH-IP-INST TO  COF-HDR-LINE(2).            ELTTHER2
03394                                                                   ELTTHER2
03395      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
03396      PERFORM WITH TEST BEFORE                                     ELTTHER2
03397              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
03398              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
03399         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
03400         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03401         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03402         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
03403      END-PERFORM.                                                 ELTTHER2
03404      MOVE WS-SPEECH-IP-INST-CNT TO PVN-NBR-BEN-PROVN.             ELTTHER2
03405                                                                   ELTTHER2
03406                                                                   ELTTHER2
03407      PERFORM WITH TEST BEFORE                                     ELTTHER2
03408         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
03409         UNTIL   WS-SUB  >     WS-SPEECH-IP-INST-CNT               ELTTHER2
03410           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
03411           MOVE WS-SPEECH-IP-INST-LIST(WS-SUB)                     ELTTHER2
03412                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
03413            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
03414                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
03415      END-PERFORM.                                                 ELTTHER2
03416                                                                   ELTTHER2
03417      MOVE WS-SPEECH-IP-SERVICES TO  SSB-TOPIC-PHRASE.             ELTTHER2
03418                                                                   ELTTHER2
03419      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
03420      END-EXEC.                                                    ELTTHER2
03421                                                                   ELTTHER2
03422      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
03423      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03424      END-EXEC.                                                    ELTTHER2
03425                                                                   ELTTHER2
03426      IF PVN-COVG-NONE                                             ELTTHER2
03427         GO TO 5099-EXIT.                                          ELTTHER2
03428                                                                   ELTTHER2
03429      MOVE +1  TO  WS-CIA.                                         ELTTHER2
03430      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
03431            PSP-PROVN-PRICING-METHD,                               ELTTHER2
03432            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
03433            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
03434            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
03435            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
03436            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
03437            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
03438            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
03439            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
03440            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
03441            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
03442            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
03443            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
03444                                                                   ELTTHER2
03445      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
03446                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
03447                     END-EXEC.                                     ELTTHER2
03448                                                                   ELTTHER2
03449      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
03450                                                                   ELTTHER2
03451      PERFORM 5030-FIND-FIRST-NONZERO                              ELTTHER2
03452         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
03453         UNTIL WS-SUB  >  WS-SPEECH-IP-INST-CNT.                   ELTTHER2
03454                                                                   ELTTHER2
03455      GO TO 5099-EXIT.                                             ELTTHER2
03456  5030-FIND-FIRST-NONZERO.                                         ELTTHER2
03457      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03458      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
03459         CONTINUE                                                  ELTTHER2
03460      ELSE                                                         ELTTHER2
03461         PERFORM 5040-BUILD-SCREEN-LINES THRU 5040-EXIT.           ELTTHER2
03462                                                                   ELTTHER2
03463  5040-BUILD-SCREEN-LINES.                                         ELTTHER2
03464                                                                   ELTTHER2
03465      SET PLT-INDEX1  TO                                           ELTTHER2
03466                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
03467      IF WS-NOT-FIRST-TIME                                         ELTTHER2
03468         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
03469         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
03470         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
03471             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
03472                                         END-EXEC                  ELTTHER2
03473      ELSE                                                         ELTTHER2
03474         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
03475                                                                   ELTTHER2
03476      MOVE 1  TO  WS-CIA.                                          ELTTHER2
03477      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
03478         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
03479            SET PLT-INDEX2  TO  2                                  ELTTHER2
03480         ELSE                                                      ELTTHER2
03481            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
03482            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
03483            GO TO 5040-EXIT                                        ELTTHER2
03484      ELSE                                                         ELTTHER2
03485         SET PLT-INDEX2  TO  1.                                    ELTTHER2
03486                                                                   ELTTHER2
03487 **---------------------------------------------------------------+ELTTHER2
03488 **                                                               |ELTTHER2
03489 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
03490      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
03491      ADD  +1  TO  WS-CIA.                                         ELTTHER2
03492      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
03493                                                                   ELTTHER2
03494      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
03495                                                                   ELTTHER2
03496      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
03497         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
03498         UNTIL  PVN-BEN-PROVN-IDX > WS-SPEECH-IP-INST-CNT.         ELTTHER2
03499      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03500                                                                   ELTTHER2
03501      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
03502      MOVE +1  TO  WS-CIA                                          ELTTHER2
03503      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03504                                         END-EXEC.                 ELTTHER2
03505 **                                                               |ELTTHER2
03506 **---------------------------------------------------------------+ELTTHER2
03507 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
03508      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
03509 **                                                               |ELTTHER2
03510 **---------------------------------------------------------------+ELTTHER2
03511                                                                   ELTTHER2
03512 **---------------------------------------------------------------+ELTTHER2
03513 **                                                               |ELTTHER2
03514 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
03515 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
03516 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
03517      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
03518 **                                                               |ELTTHER2
03519 **---------------------------------------------------------------+ELTTHER2
03520                                                                   ELTTHER2
03521 **---------------------------------------------------------------+ELTTHER2
03522 **                                                               |ELTTHER2
03523 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHER2
03524 **                  H O S P I T A L   B I L L                    |ELTTHER2
03525      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHER2
03526      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
03527 **                                                               |ELTTHER2
03528 **---------------------------------------------------------------+ELTTHER2
03529                                                                   ELTTHER2
03530 **---------------------------------------------------------------+ELTTHER2
03531 **                                                               |ELTTHER2
03532 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHER2
03533      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
03534 **                                                               |ELTTHER2
03535 **---------------------------------------------------------------+ELTTHER2
03536                                                                   ELTTHER2
03537 **---------------------------------------------------------------+ELTTHER2
03538 **                                                               |ELTTHER2
03539 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
03540      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
03541         THRU 3250-EXIT.                                           ELTTHER2
03542 **                                                               |ELTTHER2
03543 **---------------------------------------------------------------+ELTTHER2
03544                                                                   ELTTHER2
03545 **---------------------------------------------------------------+ELTTHER2
03546 **                                                               |ELTTHER2
03547 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
03548      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
03549 **                                                               |ELTTHER2
03550 **---------------------------------------------------------------+ELTTHER2
03551                                                                   ELTTHER2
03552 **---------------------------------------------------------------+ELTTHER2
03553 **                                                               |ELTTHER2
03554 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
03555 **                         A N D                                 |ELTTHER2
03556 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
03557      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
03558 **                                                               |ELTTHER2
03559 **---------------------------------------------------------------+ELTTHER2
03560                                                                   ELTTHER2
03561 **---------------------------------------------------------------+ELTTHER2
03562 **                                                               |ELTTHER2
03563 **               P E R F O R M   T A B U L A R                   |ELTTHER2
03564      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
03565 **                                                               |ELTTHER2
03566 **---------------------------------------------------------------+ELTTHER2
03567                                                                   ELTTHER2
03568  5040-EXIT.  EXIT.                                                ELTTHER2
03569                                                                   ELTTHER2
03570  5099-EXIT.            EXIT.                                      ELTTHER2
03571                                                                   ELTTHER2
03572 /   S H O C K   T H E R A P Y   O P   I N S T I T U T I O N A L   ELTTHER2
03573  5200-SPEECH-THRP-OP-INST-RTNE.                                   ELTTHER2
03574      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
03575                                                                   ELTTHER2
03576      MOVE WS-HDR-2-SPEECH-OP-INST TO  COF-HDR-LINE(2).            ELTTHER2
03577                                                                   ELTTHER2
03578      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
03579      PERFORM WITH TEST BEFORE                                     ELTTHER2
03580              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
03581              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
03582         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
03583         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03584         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03585         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
03586      END-PERFORM.                                                 ELTTHER2
03587      MOVE WS-SPEECH-OP-INST-CNT TO PVN-NBR-BEN-PROVN.             ELTTHER2
03588                                                                   ELTTHER2
03589                                                                   ELTTHER2
03590      PERFORM WITH TEST BEFORE                                     ELTTHER2
03591         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
03592         UNTIL   WS-SUB  >     WS-SPEECH-OP-INST-CNT               ELTTHER2
03593           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
03594           MOVE WS-SPEECH-OP-INST-LIST(WS-SUB)                     ELTTHER2
03595                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
03596            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
03597                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
03598      END-PERFORM.                                                 ELTTHER2
03599                                                                   ELTTHER2
03600      MOVE WS-SPEECH-OP-SERVICES TO  SSB-TOPIC-PHRASE.             ELTTHER2
03601                                                                   ELTTHER2
03602      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
03603      END-EXEC.                                                    ELTTHER2
03604                                                                   ELTTHER2
03605      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
03606      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03607      END-EXEC.                                                    ELTTHER2
03608                                                                   ELTTHER2
03609                                                                   ELTTHER2
03610      IF PVN-COVG-NONE                                             ELTTHER2
03611         GO TO 5299-EXIT.                                          ELTTHER2
03612                                                                   ELTTHER2
03613      MOVE +1  TO  WS-CIA.                                         ELTTHER2
03614      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
03615            PSP-PROVN-PRICING-METHD,                               ELTTHER2
03616            PSP-TREAT-RESTRN-IND,                                  ELTTHER2
03617            PSP-CERTFN-REQRM-IND,                                  ELTTHER2
03618            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
03619            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
03620            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
03621            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
03622            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
03623            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
03624            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
03625            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
03626            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
03627 *          PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTTHER2
03628 *          PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTTHER2
03629 *          PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTTHER2
03630 *          PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTTHER2
03631            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
03632            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
03633            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHER2
03634            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHER2
03635            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
03636                                                                   ELTTHER2
03637                                                                   ELTTHER2
03638      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
03639                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
03640                     END-EXEC.                                     ELTTHER2
03641                                                                   ELTTHER2
03642      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
03643                                                                   ELTTHER2
03644      PERFORM 5230-FIND-FIRST-NONZERO                              ELTTHER2
03645         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
03646         UNTIL WS-SUB  >  WS-SPEECH-OP-INST-CNT.                   ELTTHER2
03647                                                                   ELTTHER2
03648      GO TO 5299-EXIT.                                             ELTTHER2
03649  5230-FIND-FIRST-NONZERO.                                         ELTTHER2
03650      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03651      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
03652         CONTINUE                                                  ELTTHER2
03653      ELSE                                                         ELTTHER2
03654         PERFORM 5240-BUILD-SCREEN-LINES THRU 5240-EXIT.           ELTTHER2
03655                                                                   ELTTHER2
03656  5240-BUILD-SCREEN-LINES.                                         ELTTHER2
03657                                                                   ELTTHER2
03658      SET PLT-INDEX1  TO                                           ELTTHER2
03659                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
03660      IF WS-NOT-FIRST-TIME                                         ELTTHER2
03661         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
03662         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
03663         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
03664             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
03665                                         END-EXEC                  ELTTHER2
03666      ELSE                                                         ELTTHER2
03667         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
03668                                                                   ELTTHER2
03669      MOVE 1  TO  WS-CIA.                                          ELTTHER2
03670      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
03671         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
03672            SET PLT-INDEX2  TO  2                                  ELTTHER2
03673         ELSE                                                      ELTTHER2
03674            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
03675            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
03676            GO TO 5240-EXIT                                        ELTTHER2
03677      ELSE                                                         ELTTHER2
03678         SET PLT-INDEX2  TO  1.                                    ELTTHER2
03679                                                                   ELTTHER2
03680 **---------------------------------------------------------------+ELTTHER2
03681 **                                                               |ELTTHER2
03682 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
03683      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
03684      ADD  +1  TO  WS-CIA.                                         ELTTHER2
03685      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
03686                                                                   ELTTHER2
03687      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
03688                                                                   ELTTHER2
03689      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
03690         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
03691         UNTIL  PVN-BEN-PROVN-IDX > WS-SPEECH-OP-INST-CNT.         ELTTHER2
03692      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03693                                                                   ELTTHER2
03694      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
03695      MOVE +1  TO  WS-CIA                                          ELTTHER2
03696      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03697                                         END-EXEC.                 ELTTHER2
03698 **                                                               |ELTTHER2
03699 **---------------------------------------------------------------+ELTTHER2
03700 **                                                               |ELTTHER2
03701 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
03702      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
03703 **                                                               |ELTTHER2
03704 **---------------------------------------------------------------+ELTTHER2
03705      PERFORM 3350-TREAT-RESTRN.                                   ELTTHER2
03706      PERFORM 3360-CERT-REQ-IND.                                   ELTTHER2
03707 **---------------------------------------------------------------+ELTTHER2
03708 **                                                               |ELTTHER2
03709 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
03710 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
03711 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
03712      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
03713 **                                                               |ELTTHER2
03714 **---------------------------------------------------------------+ELTTHER2
03715                                                                   ELTTHER2
03716 **---------------------------------------------------------------+ELTTHER2
03717 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHER2
03718 **                    H O S P I T A L    B I L L                 |ELTTHER2
03719      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHER2
03720      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
03721 **                                                               |ELTTHER2
03722 **---------------------------------------------------------------+ELTTHER2
03723      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
03724 **                                                               |ELTTHER2
03725                                                                   ELTTHER2
03726 **---------------------------------------------------------------+ELTTHER2
03727 **                                                               |ELTTHER2
03728 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
03729      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
03730         THRU 3250-EXIT.                                           ELTTHER2
03731 **                                                               |ELTTHER2
03732 **---------------------------------------------------------------+ELTTHER2
03733 *    IF PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX) = 'SPTO B'           ELTTHER2
03734 *       PERFORM 3501-PRIOR-ADMIS-REQ                              ELTTHER2
03735 *    ELSE                                                         ELTTHER2
03736 *       PERFORM 3500-HOSP-ADM-RESTRN.                             ELTTHER2
03737 **                                                               |ELTTHER2
03738 **---------------------------------------------------------------+ELTTHER2
03739 **                                                               |ELTTHER2
03740 **          D A Y S   R E D U C T I O N   R A T I O              |ELTTHER2
03741 **           B A S I C   A N D   S E C O N D A R Y               |ELTTHER2
03742 *    PERFORM 4300-DAYS-REDCN-BASIC-SEC.                           ELTTHER2
03743 **                                                               |ELTTHER2
03744 **---------------------------------------------------------------+ELTTHER2
03745                                                                   ELTTHER2
03746                                                                   ELTTHER2
03747 **---------------------------------------------------------------+ELTTHER2
03748 **                                                               |ELTTHER2
03749 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
03750      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
03751 **                                                               |ELTTHER2
03752 **---------------------------------------------------------------+ELTTHER2
03753 **---------------------------------------------------------------+ELTTHER2
03754 **                                                               |ELTTHER2
03755 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
03756 *    PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHER2
03757 **                                                               |ELTTHER2
03758 **---------------------------------------------------------------+ELTTHER2
03759                                                                   ELTTHER2
03760 **---------------------------------------------------------------+ELTTHER2
03761 **                                                               |ELTTHER2
03762 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
03763 **                         A N D                                 |ELTTHER2
03764 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
03765      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
03766 **                                                               |ELTTHER2
03767 **---------------------------------------------------------------+ELTTHER2
03768                                                                   ELTTHER2
03769 **---------------------------------------------------------------+ELTTHER2
03770 **                                                               |ELTTHER2
03771 **               P E R F O R M   T A B U L A R                   |ELTTHER2
03772      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
03773 **                                                               |ELTTHER2
03774 **---------------------------------------------------------------+ELTTHER2
03775  5240-EXIT.  EXIT.                                                ELTTHER2
03776                                                                   ELTTHER2
03777  5299-EXIT.            EXIT.                                      ELTTHER2
03778                                                                   ELTTHER2
03779 /   S PEECH     T H E R A P Y   I P   P R O F E S S I O N A L     ELTTHER2
03780  5400-SPEECH-THRP-IP-PROF-RTNE.                                   ELTTHER2
03781      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
03782                                                                   ELTTHER2
03783      MOVE WS-HDR-2-SPEECH-IP-PROF TO  COF-HDR-LINE(2).            ELTTHER2
03784                                                                   ELTTHER2
03785      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
03786      PERFORM WITH TEST BEFORE                                     ELTTHER2
03787              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
03788              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
03789         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
03790         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03791         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03792         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
03793      END-PERFORM.                                                 ELTTHER2
03794      MOVE WS-SPEECH-IP-PROF-CNT TO PVN-NBR-BEN-PROVN.             ELTTHER2
03795                                                                   ELTTHER2
03796                                                                   ELTTHER2
03797      PERFORM WITH TEST BEFORE                                     ELTTHER2
03798         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
03799         UNTIL   WS-SUB  >     WS-SPEECH-IP-PROF-CNT               ELTTHER2
03800           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
03801           MOVE WS-SPEECH-IP-PROF-LIST(WS-SUB)                     ELTTHER2
03802                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
03803            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
03804                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
03805      END-PERFORM.                                                 ELTTHER2
03806                                                                   ELTTHER2
03807      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
03808      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03809                                         END-EXEC.                 ELTTHER2
03810                                                                   ELTTHER2
03811      MOVE WS-SPEECH-IP-SERVICES TO  SSB-TOPIC-PHRASE.             ELTTHER2
03812                                                                   ELTTHER2
03813      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
03814                             END-EXEC.                             ELTTHER2
03815                                                                   ELTTHER2
03816      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
03817      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03818                                         END-EXEC.                 ELTTHER2
03819                                                                   ELTTHER2
03820      IF PVN-COVG-NONE                                             ELTTHER2
03821         GO TO 5499-EXIT.                                          ELTTHER2
03822                                                                   ELTTHER2
03823      MOVE +1  TO  WS-CIA.                                         ELTTHER2
03824      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
03825            PSP-PROVN-PRICING-METHD,                               ELTTHER2
03826            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
03827            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
03828            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
03829            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
03830            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
03831            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
03832            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
03833            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
03834            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
03835            PSE-BEN-SCOPE-ID,                                      ELTTHER2
03836            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
03837            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
03838            PSE-MAX-AMT-PER-VISIT.                                 ELTTHER2
03839                                                                   ELTTHER2
03840                                                                   ELTTHER2
03841      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
03842                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
03843                     END-EXEC.                                     ELTTHER2
03844                                                                   ELTTHER2
03845      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
03846                                                                   ELTTHER2
03847      PERFORM 5430-FIND-FIRST-NONZERO                              ELTTHER2
03848         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
03849         UNTIL WS-SUB  >  WS-SPEECH-IP-PROF-CNT.                   ELTTHER2
03850                                                                   ELTTHER2
03851      GO TO 5499-EXIT.                                             ELTTHER2
03852  5430-FIND-FIRST-NONZERO.                                         ELTTHER2
03853      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03854      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
03855         CONTINUE                                                  ELTTHER2
03856      ELSE                                                         ELTTHER2
03857         PERFORM 5440-BUILD-SCREEN-LINES THRU 5440-EXIT.           ELTTHER2
03858                                                                   ELTTHER2
03859  5440-BUILD-SCREEN-LINES.                                         ELTTHER2
03860                                                                   ELTTHER2
03861      SET PLT-INDEX1  TO                                           ELTTHER2
03862                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
03863      IF WS-NOT-FIRST-TIME                                         ELTTHER2
03864         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
03865         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
03866         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
03867             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
03868                                         END-EXEC                  ELTTHER2
03869      ELSE                                                         ELTTHER2
03870         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
03871                                                                   ELTTHER2
03872      MOVE 1  TO  WS-CIA.                                          ELTTHER2
03873      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
03874         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
03875            SET PLT-INDEX2  TO  2                                  ELTTHER2
03876         ELSE                                                      ELTTHER2
03877            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
03878            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
03879            GO TO 5440-EXIT                                        ELTTHER2
03880      ELSE                                                         ELTTHER2
03881         SET PLT-INDEX2  TO  1.                                    ELTTHER2
03882                                                                   ELTTHER2
03883 **---------------------------------------------------------------+ELTTHER2
03884 **                                                               |ELTTHER2
03885 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
03886      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
03887      ADD  +1  TO  WS-CIA.                                         ELTTHER2
03888      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
03889      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
03890         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
03891         UNTIL  PVN-BEN-PROVN-IDX > WS-SPEECH-IP-PROF-CNT.         ELTTHER2
03892      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
03893                                                                   ELTTHER2
03894      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
03895      MOVE +1  TO  WS-CIA                                          ELTTHER2
03896      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03897                                         END-EXEC.                 ELTTHER2
03898 **                                                               |ELTTHER2
03899 **---------------------------------------------------------------+ELTTHER2
03900 **                                                               |ELTTHER2
03901 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
03902      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
03903 **                                                               |ELTTHER2
03904 **---------------------------------------------------------------+ELTTHER2
03905                                                                   ELTTHER2
03906 **---------------------------------------------------------------+ELTTHER2
03907 **                                                               |ELTTHER2
03908 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
03909      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
03910 **                                                               |ELTTHER2
03911 **---------------------------------------------------------------+ELTTHER2
03912                                                                   ELTTHER2
03913 **---------------------------------------------------------------+ELTTHER2
03914 **                                                               |ELTTHER2
03915 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
03916 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
03917 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
03918      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
03919 **                                                               |ELTTHER2
03920 **---------------------------------------------------------------+ELTTHER2
03921                                                                   ELTTHER2
03922 **---------------------------------------------------------------+ELTTHER2
03923 **                                                               |ELTTHER2
03924 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
03925      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
03926         THRU 3250-EXIT.                                           ELTTHER2
03927 **                                                               |ELTTHER2
03928 **---------------------------------------------------------------+ELTTHER2
03929                                                                   ELTTHER2
03930 **---------------------------------------------------------------+ELTTHER2
03931 **                                                               |ELTTHER2
03932 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
03933      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
03934 **                                                               |ELTTHER2
03935 **---------------------------------------------------------------+ELTTHER2
03936                                                                   ELTTHER2
03937 **---------------------------------------------------------------+ELTTHER2
03938 **                                                               |ELTTHER2
03939 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
03940      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
03941 **                                                               |ELTTHER2
03942 **---------------------------------------------------------------+ELTTHER2
03943                                                                   ELTTHER2
03944 **---------------------------------------------------------------+ELTTHER2
03945 **                                                               |ELTTHER2
03946 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
03947 **                         A N D                                 |ELTTHER2
03948 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
03949      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
03950 **                                                               |ELTTHER2
03951 **---------------------------------------------------------------+ELTTHER2
03952                                                                   ELTTHER2
03953 **---------------------------------------------------------------+ELTTHER2
03954 **                                                               |ELTTHER2
03955 **               P E R F O R M   T A B U L A R                   |ELTTHER2
03956      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
03957 **                                                               |ELTTHER2
03958 **---------------------------------------------------------------+ELTTHER2
03959                                                                   ELTTHER2
03960  5440-EXIT.  EXIT.                                                ELTTHER2
03961                                                                   ELTTHER2
03962  5499-EXIT.            EXIT.                                      ELTTHER2
03963                                                                   ELTTHER2
03964 /   S H O C K   T H E R A P Y   O P   P R O F E S S I O N A L     ELTTHER2
03965  5600-SPEECH-THRP-OP-PROF-RTNE.                                   ELTTHER2
03966      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
03967                                                                   ELTTHER2
03968      MOVE WS-HDR-2-SPEECH-OP-PROF TO  COF-HDR-LINE(2).            ELTTHER2
03969                                                                   ELTTHER2
03970      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
03971      PERFORM WITH TEST BEFORE                                     ELTTHER2
03972              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
03973              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
03974         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
03975         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03976         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
03977         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
03978      END-PERFORM.                                                 ELTTHER2
03979      MOVE WS-SPEECH-OP-PROF-CNT TO PVN-NBR-BEN-PROVN.             ELTTHER2
03980                                                                   ELTTHER2
03981                                                                   ELTTHER2
03982      PERFORM WITH TEST BEFORE                                     ELTTHER2
03983         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
03984         UNTIL   WS-SUB  >     WS-SPEECH-OP-PROF-CNT               ELTTHER2
03985           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
03986           MOVE WS-SPEECH-OP-PROF-LIST(WS-SUB)                     ELTTHER2
03987                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
03988            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
03989                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
03990      END-PERFORM.                                                 ELTTHER2
03991                                                                   ELTTHER2
03992      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
03993      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
03994                                         END-EXEC.                 ELTTHER2
03995                                                                   ELTTHER2
03996      MOVE WS-SPEECH-OP-SERVICES TO  SSB-TOPIC-PHRASE.             ELTTHER2
03997                                                                   ELTTHER2
03998      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
03999           END-EXEC.                                               ELTTHER2
04000                                                                   ELTTHER2
04001      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
04002      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04003                                         END-EXEC.                 ELTTHER2
04004                                                                   ELTTHER2
04005      IF PVN-COVG-NONE                                             ELTTHER2
04006         GO TO 5699-EXIT.                                          ELTTHER2
04007                                                                   ELTTHER2
04008      MOVE +1  TO  WS-CIA.                                         ELTTHER2
04009      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
04010            PSP-PROVN-PRICING-METHD,                               ELTTHER2
04011            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
04012            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
04013            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
04014            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
04015            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
04016            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
04017            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
04018            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
04019            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
04020            PSE-BEN-SCOPE-ID,                                      ELTTHER2
04021            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
04022            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
04023            PSE-MAX-AMT-PER-VISIT,                                 ELTTHER2
04024            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHER2
04025            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHER2
04026                                                                   ELTTHER2
04027                                                                   ELTTHER2
04028      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
04029                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
04030                     END-EXEC.                                     ELTTHER2
04031                                                                   ELTTHER2
04032      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
04033                                                                   ELTTHER2
04034      PERFORM 5630-FIND-FIRST-NONZERO                              ELTTHER2
04035         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
04036         UNTIL WS-SUB  >  WS-SPEECH-OP-PROF-CNT.                   ELTTHER2
04037                                                                   ELTTHER2
04038      GO TO 5699-EXIT.                                             ELTTHER2
04039  5630-FIND-FIRST-NONZERO.                                         ELTTHER2
04040      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04041      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
04042         CONTINUE                                                  ELTTHER2
04043      ELSE                                                         ELTTHER2
04044         PERFORM 5640-BUILD-SCREEN-LINES THRU 5640-EXIT.           ELTTHER2
04045                                                                   ELTTHER2
04046  5640-BUILD-SCREEN-LINES.                                         ELTTHER2
04047                                                                   ELTTHER2
04048      SET PLT-INDEX1  TO                                           ELTTHER2
04049                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
04050      IF WS-NOT-FIRST-TIME                                         ELTTHER2
04051         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
04052         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
04053         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
04054             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
04055                                         END-EXEC                  ELTTHER2
04056      ELSE                                                         ELTTHER2
04057         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
04058                                                                   ELTTHER2
04059      MOVE 1  TO  WS-CIA.                                          ELTTHER2
04060      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
04061         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
04062            SET PLT-INDEX2  TO  2                                  ELTTHER2
04063         ELSE                                                      ELTTHER2
04064            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
04065            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
04066            GO TO 5640-EXIT                                        ELTTHER2
04067      ELSE                                                         ELTTHER2
04068         SET PLT-INDEX2  TO  1.                                    ELTTHER2
04069                                                                   ELTTHER2
04070 **---------------------------------------------------------------+ELTTHER2
04071 **                                                               |ELTTHER2
04072 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
04073      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
04074      ADD  +1  TO  WS-CIA.                                         ELTTHER2
04075      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
04076      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
04077         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
04078         UNTIL  PVN-BEN-PROVN-IDX > WS-SPEECH-OP-PROF-CNT.         ELTTHER2
04079      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04080                                                                   ELTTHER2
04081      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
04082      MOVE +1  TO  WS-CIA                                          ELTTHER2
04083      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04084                                         END-EXEC.                 ELTTHER2
04085 **                                                               |ELTTHER2
04086 **---------------------------------------------------------------+ELTTHER2
04087 **                                                               |ELTTHER2
04088 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
04089      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
04090 **                                                               |ELTTHER2
04091 **---------------------------------------------------------------+ELTTHER2
04092                                                                   ELTTHER2
04093 **---------------------------------------------------------------+ELTTHER2
04094 **                                                               |ELTTHER2
04095 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
04096      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
04097 **                                                               |ELTTHER2
04098 **---------------------------------------------------------------+ELTTHER2
04099                                                                   ELTTHER2
04100 **---------------------------------------------------------------+ELTTHER2
04101 **                                                               |ELTTHER2
04102 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
04103 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
04104 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
04105      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
04106 **                                                               |ELTTHER2
04107 **---------------------------------------------------------------+ELTTHER2
04108                                                                   ELTTHER2
04109 **---------------------------------------------------------------+ELTTHER2
04110 **                                                               |ELTTHER2
04111 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
04112      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
04113 **                                                               |ELTTHER2
04114 **---------------------------------------------------------------+ELTTHER2
04115                                                                   ELTTHER2
04116 **---------------------------------------------------------------+ELTTHER2
04117 **                                                               |ELTTHER2
04118 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
04119      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
04120 **                                                               |ELTTHER2
04121 **---------------------------------------------------------------+ELTTHER2
04122                                                                   ELTTHER2
04123 **---------------------------------------------------------------+ELTTHER2
04124 **                                                               |ELTTHER2
04125 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
04126      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
04127         THRU 3250-EXIT.                                           ELTTHER2
04128 **                                                               |ELTTHER2
04129 **---------------------------------------------------------------+ELTTHER2
04130                                                                   ELTTHER2
04131 **---------------------------------------------------------------+ELTTHER2
04132 **                                                               |ELTTHER2
04133 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
04134      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHER2
04135 **                                                               |ELTTHER2
04136 **---------------------------------------------------------------+ELTTHER2
04137                                                                   ELTTHER2
04138 **---------------------------------------------------------------+ELTTHER2
04139 **                                                               |ELTTHER2
04140 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
04141 **                         A N D                                 |ELTTHER2
04142 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
04143      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
04144 **                                                               |ELTTHER2
04145 **---------------------------------------------------------------+ELTTHER2
04146                                                                   ELTTHER2
04147 **---------------------------------------------------------------+ELTTHER2
04148 **                                                               |ELTTHER2
04149 **               P E R F O R M   T A B U L A R                   |ELTTHER2
04150      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
04151 **                                                               |ELTTHER2
04152 **---------------------------------------------------------------+ELTTHER2
04153                                                                   ELTTHER2
04154  5640-EXIT.  EXIT.                                                ELTTHER2
04155                                                                   ELTTHER2
04156  5699-EXIT.            EXIT.                                      ELTTHER2
04157                                                                   ELTTHER2
04158 /    M I S C .   T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHER2
04159 ***************************************************************** ELTTHER2
04160 *    M I S C .   T H E R A P Y   I P   I N S T I T U T I O N A L  ELTTHER2
04161 *                                                                 ELTTHER2
04162 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTTHER2
04163 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTTHER2
04164 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTTHER2
04165 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTTHER2
04166 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTTHER2
04167 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTTHER2
04168 *  MODULE.                                                        ELTTHER2
04169 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTTHER2
04170 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTTHER2
04171 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTTHER2
04172 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTTHER2
04173 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTTHER2
04174 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTTHER2
04175 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTTHER2
04176 *                                                                 ELTTHER2
04177 ***************************************************************** ELTTHER2
04178  6000-CARD-THERP-IP-INST-RTNE.                                    ELTTHER2
04179      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
04180                                                                   ELTTHER2
04181      MOVE WS-HDR-2-CARD-IP-INST  TO  COF-HDR-LINE(2).             ELTTHER2
04182                                                                   ELTTHER2
04183      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
04184      PERFORM WITH TEST BEFORE                                     ELTTHER2
04185              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
04186              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
04187         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
04188         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04189         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04190         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
04191      END-PERFORM.                                                 ELTTHER2
04192      MOVE WS-CARD-IP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHER2
04193                                                                   ELTTHER2
04194                                                                   ELTTHER2
04195      PERFORM WITH TEST BEFORE                                     ELTTHER2
04196         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
04197         UNTIL   WS-SUB  >     WS-CARD-IP-INST-CNT                 ELTTHER2
04198           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
04199           MOVE WS-CARD-IP-INST-LIST (WS-SUB)                      ELTTHER2
04200                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
04201            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
04202                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
04203      END-PERFORM.                                                 ELTTHER2
04204                                                                   ELTTHER2
04205                                                                   ELTTHER2
04206      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
04207      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04208                                         END-EXEC.                 ELTTHER2
04209                                                                   ELTTHER2
04210      MOVE WS-CARD-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHER2
04211                                                                   ELTTHER2
04212      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
04213                               END-EXEC.                           ELTTHER2
04214                                                                   ELTTHER2
04215      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
04216      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04217                                         END-EXEC.                 ELTTHER2
04218                                                                   ELTTHER2
04219      IF PVN-COVG-NONE                                             ELTTHER2
04220         GO TO 6099-EXIT.                                          ELTTHER2
04221                                                                   ELTTHER2
04222      MOVE +1  TO  WS-CIA.                                         ELTTHER2
04223      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
04224            PSP-PROVN-PRICING-METHD,                               ELTTHER2
04225            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
04226            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
04227            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
04228            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
04229            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
04230            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
04231            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
04232            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
04233            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
04234            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
04235            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
04236            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
04237                                                                   ELTTHER2
04238                                                                   ELTTHER2
04239      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
04240                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
04241                     END-EXEC.                                     ELTTHER2
04242                                                                   ELTTHER2
04243      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
04244                                                                   ELTTHER2
04245      PERFORM 6030-FIND-FIRST-NONZERO                              ELTTHER2
04246         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
04247         UNTIL WS-SUB  >  WS-CARD-IP-INST-CNT.                     ELTTHER2
04248                                                                   ELTTHER2
04249      GO TO 6099-EXIT.                                             ELTTHER2
04250  6030-FIND-FIRST-NONZERO.                                         ELTTHER2
04251      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04252      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
04253         CONTINUE                                                  ELTTHER2
04254      ELSE                                                         ELTTHER2
04255         PERFORM 6040-BUILD-SCREEN-LINES THRU 6040-EXIT.           ELTTHER2
04256                                                                   ELTTHER2
04257  6040-BUILD-SCREEN-LINES.                                         ELTTHER2
04258                                                                   ELTTHER2
04259      SET PLT-INDEX1  TO                                           ELTTHER2
04260                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
04261      IF WS-NOT-FIRST-TIME                                         ELTTHER2
04262         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
04263         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
04264         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
04265             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
04266                                         END-EXEC                  ELTTHER2
04267      ELSE                                                         ELTTHER2
04268         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
04269                                                                   ELTTHER2
04270      MOVE 1  TO  WS-CIA.                                          ELTTHER2
04271      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
04272         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
04273            SET PLT-INDEX2  TO  2                                  ELTTHER2
04274         ELSE                                                      ELTTHER2
04275            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
04276            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
04277            GO TO 6040-EXIT                                        ELTTHER2
04278      ELSE                                                         ELTTHER2
04279         SET PLT-INDEX2  TO  1.                                    ELTTHER2
04280                                                                   ELTTHER2
04281 **---------------------------------------------------------------+ELTTHER2
04282 **                                                               |ELTTHER2
04283 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
04284      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
04285      ADD  +1  TO  WS-CIA.                                         ELTTHER2
04286      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
04287                                                                   ELTTHER2
04288      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
04289                                                                   ELTTHER2
04290      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
04291         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
04292         UNTIL  PVN-BEN-PROVN-IDX > WS-CARD-IP-INST-CNT.           ELTTHER2
04293      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04294                                                                   ELTTHER2
04295      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
04296      MOVE +1  TO  WS-CIA                                          ELTTHER2
04297      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04298                                         END-EXEC.                 ELTTHER2
04299 **                                                               |ELTTHER2
04300 **---------------------------------------------------------------+ELTTHER2
04301 **                                                               |ELTTHER2
04302 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
04303      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
04304 **                                                               |ELTTHER2
04305 **---------------------------------------------------------------+ELTTHER2
04306                                                                   ELTTHER2
04307 **---------------------------------------------------------------+ELTTHER2
04308 **                                                               |ELTTHER2
04309 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
04310 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
04311 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
04312      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
04313 **                                                               |ELTTHER2
04314 **---------------------------------------------------------------+ELTTHER2
04315                                                                   ELTTHER2
04316 **---------------------------------------------------------------+ELTTHER2
04317 **                                                               |ELTTHER2
04318 **          P R O F E S S I O N A L   C H A R G E S   O N        |ELTTHER2
04319 **                  H O S P I T A L   B I L L                    |ELTTHER2
04320      MOVE WS-PROF-INPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.         ELTTHER2
04321      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
04322 **                                                               |ELTTHER2
04323 **---------------------------------------------------------------+ELTTHER2
04324                                                                   ELTTHER2
04325 **---------------------------------------------------------------+ELTTHER2
04326 **                                                               |ELTTHER2
04327 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHER2
04328      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
04329 **                                                               |ELTTHER2
04330 **---------------------------------------------------------------+ELTTHER2
04331                                                                   ELTTHER2
04332 **---------------------------------------------------------------+ELTTHER2
04333 **                                                               |ELTTHER2
04334 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
04335      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
04336         THRU 3250-EXIT.                                           ELTTHER2
04337 **                                                               |ELTTHER2
04338 **---------------------------------------------------------------+ELTTHER2
04339                                                                   ELTTHER2
04340 **---------------------------------------------------------------+ELTTHER2
04341 **                                                               |ELTTHER2
04342 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
04343      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
04344 **                                                               |ELTTHER2
04345 **---------------------------------------------------------------+ELTTHER2
04346                                                                   ELTTHER2
04347 **---------------------------------------------------------------+ELTTHER2
04348 **                                                               |ELTTHER2
04349 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
04350 **                         A N D                                 |ELTTHER2
04351 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
04352      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
04353 **                                                               |ELTTHER2
04354 **---------------------------------------------------------------+ELTTHER2
04355                                                                   ELTTHER2
04356 **---------------------------------------------------------------+ELTTHER2
04357 **                                                               |ELTTHER2
04358 **               P E R F O R M   T A B U L A R                   |ELTTHER2
04359      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
04360 **                                                               |ELTTHER2
04361 **---------------------------------------------------------------+ELTTHER2
04362                                                                   ELTTHER2
04363  6040-EXIT.  EXIT.                                                ELTTHER2
04364  6099-EXIT.            EXIT.                                      ELTTHER2
04365                                                                   ELTTHER2
04366 /    M I S C .   T H E R A P Y   O P   I N S T I T U T I O N A L  ELTTHER2
04367  6200-CARD-THERP-OP-INST-RTNE.                                    ELTTHER2
04368      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
04369                                                                   ELTTHER2
04370      MOVE WS-HDR-2-CARD-OP-INST  TO  COF-HDR-LINE(2).             ELTTHER2
04371                                                                   ELTTHER2
04372      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
04373      PERFORM WITH TEST BEFORE                                     ELTTHER2
04374              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
04375              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
04376         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
04377         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04378         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04379         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
04380      END-PERFORM.                                                 ELTTHER2
04381      MOVE WS-CARD-OP-INST-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHER2
04382                                                                   ELTTHER2
04383                                                                   ELTTHER2
04384      PERFORM WITH TEST BEFORE                                     ELTTHER2
04385         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
04386         UNTIL   WS-SUB  >     WS-CARD-OP-INST-CNT                 ELTTHER2
04387           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
04388           MOVE WS-CARD-OP-INST-LIST (WS-SUB)                      ELTTHER2
04389                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
04390            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
04391                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
04392      END-PERFORM.                                                 ELTTHER2
04393                                                                   ELTTHER2
04394                                                                   ELTTHER2
04395      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
04396      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04397                                         END-EXEC.                 ELTTHER2
04398                                                                   ELTTHER2
04399      MOVE WS-CARD-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHER2
04400                                                                   ELTTHER2
04401      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
04402                                         END-EXEC.                 ELTTHER2
04403                                                                   ELTTHER2
04404      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
04405      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04406                                         END-EXEC.                 ELTTHER2
04407                                                                   ELTTHER2
04408      IF PVN-COVG-NONE                                             ELTTHER2
04409         GO TO 6299-EXIT.                                          ELTTHER2
04410                                                                   ELTTHER2
04411      MOVE +1  TO  WS-CIA.                                         ELTTHER2
04412      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
04413            PSP-PROVN-PRICING-METHD,                               ELTTHER2
04414            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
04415            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
04416            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
04417            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
04418            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
04419            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
04420            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
04421            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
04422            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
04423            PSB-ELIG-METHD-OF-TREAT-IND,                           ELTTHER2
04424            PSB-HOSP-COND-RELATSP-IND,                             ELTTHER2
04425            PSB-HOSP-ADM-RESTRN-IND,                               ELTTHER2
04426            PSB-HSP-ADM-RESTRN-DAYS,                               ELTTHER2
04427            PSB-PROF-CHRG-HSP-CLM.                                 ELTTHER2
04428                                                                   ELTTHER2
04429                                                                   ELTTHER2
04430      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
04431                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
04432                     END-EXEC.                                     ELTTHER2
04433                                                                   ELTTHER2
04434      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
04435                                                                   ELTTHER2
04436      PERFORM 6230-FIND-FIRST-NONZERO                              ELTTHER2
04437         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
04438         UNTIL WS-SUB  >  WS-CARD-OP-INST-CNT.                     ELTTHER2
04439                                                                   ELTTHER2
04440      GO TO 6299-EXIT.                                             ELTTHER2
04441  6230-FIND-FIRST-NONZERO.                                         ELTTHER2
04442      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04443      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
04444         CONTINUE                                                  ELTTHER2
04445      ELSE                                                         ELTTHER2
04446         PERFORM 6240-BUILD-SCREEN-LINES THRU 6240-EXIT.           ELTTHER2
04447                                                                   ELTTHER2
04448  6240-BUILD-SCREEN-LINES.                                         ELTTHER2
04449                                                                   ELTTHER2
04450      SET PLT-INDEX1  TO                                           ELTTHER2
04451                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
04452      IF WS-NOT-FIRST-TIME                                         ELTTHER2
04453         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
04454         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
04455         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
04456             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
04457                                         END-EXEC                  ELTTHER2
04458      ELSE                                                         ELTTHER2
04459         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
04460                                                                   ELTTHER2
04461      MOVE 1  TO  WS-CIA.                                          ELTTHER2
04462      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
04463         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
04464            SET PLT-INDEX2  TO  2                                  ELTTHER2
04465         ELSE                                                      ELTTHER2
04466            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
04467            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
04468            GO TO 6240-EXIT                                        ELTTHER2
04469      ELSE                                                         ELTTHER2
04470         SET PLT-INDEX2  TO  1.                                    ELTTHER2
04471                                                                   ELTTHER2
04472 **---------------------------------------------------------------+ELTTHER2
04473 **                                                               |ELTTHER2
04474 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
04475      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
04476      ADD  +1  TO  WS-CIA.                                         ELTTHER2
04477      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
04478                                                                   ELTTHER2
04479      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTTHER2
04480      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
04481         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
04482         UNTIL  PVN-BEN-PROVN-IDX > WS-CARD-OP-INST-CNT.           ELTTHER2
04483      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04484                                                                   ELTTHER2
04485      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
04486      MOVE +1  TO  WS-CIA                                          ELTTHER2
04487      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04488                                         END-EXEC.                 ELTTHER2
04489 **---------------------------------------------------------------+ELTTHER2
04490 **                                                               |ELTTHER2
04491 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
04492      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
04493 **                                                               |ELTTHER2
04494 **---------------------------------------------------------------+ELTTHER2
04495                                                                   ELTTHER2
04496 **---------------------------------------------------------------+ELTTHER2
04497 **                                                               |ELTTHER2
04498 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
04499 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
04500 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
04501      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
04502 **                                                               |ELTTHER2
04503 **---------------------------------------------------------------+ELTTHER2
04504                                                                   ELTTHER2
04505 **---------------------------------------------------------------+ELTTHER2
04506 **          P R O F E S S I O N A L   C H A R G E S    O N       |ELTTHER2
04507 **                    H O S P I T A L    B I L L                 |ELTTHER2
04508      MOVE WS-PROF-OUTPT-CHRGES  TO  WS-HOLD-PROF-CHRG-MSG.        ELTTHER2
04509      PERFORM 3150-PROF-CHGR-HSP-CLM.                              ELTTHER2
04510 **                                                               |ELTTHER2
04511 **---------------------------------------------------------------+ELTTHER2
04512                                                                   ELTTHER2
04513 **---------------------------------------------------------------+ELTTHER2
04514 **                                                               |ELTTHER2
04515 **       E L I G I B L E   M E T H O D   O F   T R E A T M E N T |ELTTHER2
04516      PERFORM 3200-ELIG-METHOD-TREAT.                              ELTTHER2
04517 **                                                               |ELTTHER2
04518 **---------------------------------------------------------------+ELTTHER2
04519                                                                   ELTTHER2
04520 **---------------------------------------------------------------+ELTTHER2
04521 **                                                               |ELTTHER2
04522 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
04523      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
04524         THRU 3250-EXIT.                                           ELTTHER2
04525 **                                                               |ELTTHER2
04526 **---------------------------------------------------------------+ELTTHER2
04527                                                                   ELTTHER2
04528 **---------------------------------------------------------------+ELTTHER2
04529 **                                                               |ELTTHER2
04530 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
04531      IF PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)   =  'CRPO B'        ELTTHER2
04532          PERFORM 3501-PRIOR-ADMIS-REQ                             ELTTHER2
04533      ELSE                                                         ELTTHER2
04534          PERFORM 3500-HOSP-ADM-RESTRN.                            ELTTHER2
04535 **                                                               |ELTTHER2
04536 **---------------------------------------------------------------+ELTTHER2
04537                                                                   ELTTHER2
04538 **---------------------------------------------------------------+ELTTHER2
04539 **                                                               |ELTTHER2
04540 **     H O S P I T A L   C O N D I T I O N   R E L .   I N D .   |ELTTHER2
04541      PERFORM 3300-HOSP-COND-REL-IND.                              ELTTHER2
04542 **                                                               |ELTTHER2
04543 **---------------------------------------------------------------+ELTTHER2
04544                                                                   ELTTHER2
04545 **---------------------------------------------------------------+ELTTHER2
04546 **                                                               |ELTTHER2
04547 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
04548 **                         A N D                                 |ELTTHER2
04549 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
04550      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
04551 **                                                               |ELTTHER2
04552 **---------------------------------------------------------------+ELTTHER2
04553                                                                   ELTTHER2
04554 **---------------------------------------------------------------+ELTTHER2
04555 **                                                               |ELTTHER2
04556 **               P E R F O R M   T A B U L A R                   |ELTTHER2
04557      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
04558 **                                                               |ELTTHER2
04559 **---------------------------------------------------------------+ELTTHER2
04560                                                                   ELTTHER2
04561  6240-EXIT.  EXIT.                                                ELTTHER2
04562                                                                   ELTTHER2
04563  6299-EXIT.            EXIT.                                      ELTTHER2
04564                                                                   ELTTHER2
04565 /    M I S C .   T H E R A P Y   I P   P R O F E S S I O N A L    ELTTHER2
04566  6400-CARD-THERP-IP-PROF-RTNE.                                    ELTTHER2
04567      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
04568                                                                   ELTTHER2
04569      MOVE WS-HDR-2-CARD-IP-PROF  TO  COF-HDR-LINE(2).             ELTTHER2
04570                                                                   ELTTHER2
04571      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
04572      PERFORM WITH TEST BEFORE                                     ELTTHER2
04573              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
04574              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
04575         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
04576         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04577         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04578         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
04579      END-PERFORM.                                                 ELTTHER2
04580      MOVE WS-CARD-IP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHER2
04581                                                                   ELTTHER2
04582                                                                   ELTTHER2
04583      PERFORM WITH TEST BEFORE                                     ELTTHER2
04584         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
04585         UNTIL   WS-SUB  >     WS-CARD-IP-PROF-CNT                 ELTTHER2
04586           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
04587           MOVE WS-CARD-IP-PROF-LIST (WS-SUB)                      ELTTHER2
04588                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
04589            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
04590                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
04591      END-PERFORM.                                                 ELTTHER2
04592                                                                   ELTTHER2
04593                                                                   ELTTHER2
04594      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
04595      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04596                                         END-EXEC.                 ELTTHER2
04597                                                                   ELTTHER2
04598      MOVE WS-CARD-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHER2
04599                                                                   ELTTHER2
04600      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
04601                                         END-EXEC.                 ELTTHER2
04602                                                                   ELTTHER2
04603      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
04604      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04605                                         END-EXEC.                 ELTTHER2
04606                                                                   ELTTHER2
04607      IF PVN-COVG-NONE                                             ELTTHER2
04608         GO TO 6499-EXIT.                                          ELTTHER2
04609                                                                   ELTTHER2
04610      MOVE +1  TO  WS-CIA.                                         ELTTHER2
04611      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
04612            PSP-PROVN-PRICING-METHD,                               ELTTHER2
04613            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
04614            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
04615            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
04616            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
04617            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
04618            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
04619            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
04620            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
04621            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
04622            PSE-BEN-SCOPE-ID,                                      ELTTHER2
04623            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
04624            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
04625            PSE-MAX-AMT-PER-VISIT.                                 ELTTHER2
04626                                                                   ELTTHER2
04627                                                                   ELTTHER2
04628      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
04629                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
04630                     END-EXEC.                                     ELTTHER2
04631                                                                   ELTTHER2
04632      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
04633                                                                   ELTTHER2
04634      PERFORM 6430-FIND-FIRST-NONZERO                              ELTTHER2
04635         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
04636         UNTIL WS-SUB  >  WS-CARD-IP-PROF-CNT.                     ELTTHER2
04637                                                                   ELTTHER2
04638      GO TO 6499-EXIT.                                             ELTTHER2
04639  6430-FIND-FIRST-NONZERO.                                         ELTTHER2
04640      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04641      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
04642         CONTINUE                                                  ELTTHER2
04643      ELSE                                                         ELTTHER2
04644         PERFORM 6440-BUILD-SCREEN-LINES THRU 6440-EXIT.           ELTTHER2
04645                                                                   ELTTHER2
04646  6440-BUILD-SCREEN-LINES.                                         ELTTHER2
04647                                                                   ELTTHER2
04648      SET PLT-INDEX1  TO                                           ELTTHER2
04649                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
04650      IF WS-NOT-FIRST-TIME                                         ELTTHER2
04651         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
04652         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
04653         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
04654             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
04655                                         END-EXEC                  ELTTHER2
04656      ELSE                                                         ELTTHER2
04657         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
04658                                                                   ELTTHER2
04659      MOVE 1  TO  WS-CIA.                                          ELTTHER2
04660      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
04661         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
04662            SET PLT-INDEX2  TO  2                                  ELTTHER2
04663         ELSE                                                      ELTTHER2
04664            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
04665            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
04666            GO TO 6440-EXIT                                        ELTTHER2
04667      ELSE                                                         ELTTHER2
04668         SET PLT-INDEX2  TO  1.                                    ELTTHER2
04669                                                                   ELTTHER2
04670 **---------------------------------------------------------------+ELTTHER2
04671 **                                                               |ELTTHER2
04672 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
04673      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
04674      ADD  +1  TO  WS-CIA.                                         ELTTHER2
04675      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
04676      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
04677         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
04678         UNTIL  PVN-BEN-PROVN-IDX > WS-CARD-IP-PROF-CNT.           ELTTHER2
04679      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04680                                                                   ELTTHER2
04681      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
04682      MOVE +1  TO  WS-CIA                                          ELTTHER2
04683      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04684                                         END-EXEC.                 ELTTHER2
04685 **                                                               |ELTTHER2
04686 **---------------------------------------------------------------+ELTTHER2
04687 **                                                               |ELTTHER2
04688 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
04689      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
04690 **                                                               |ELTTHER2
04691 **---------------------------------------------------------------+ELTTHER2
04692                                                                   ELTTHER2
04693 **---------------------------------------------------------------+ELTTHER2
04694 **                                                               |ELTTHER2
04695 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
04696      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
04697 **                                                               |ELTTHER2
04698 **---------------------------------------------------------------+ELTTHER2
04699                                                                   ELTTHER2
04700 **---------------------------------------------------------------+ELTTHER2
04701 **                                                               |ELTTHER2
04702 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
04703 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
04704 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
04705      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
04706 **                                                               |ELTTHER2
04707 **---------------------------------------------------------------+ELTTHER2
04708                                                                   ELTTHER2
04709 **---------------------------------------------------------------+ELTTHER2
04710 **                                                               |ELTTHER2
04711 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
04712      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
04713         THRU 3250-EXIT.                                           ELTTHER2
04714 **                                                               |ELTTHER2
04715 **---------------------------------------------------------------+ELTTHER2
04716                                                                   ELTTHER2
04717 **---------------------------------------------------------------+ELTTHER2
04718 **                                                               |ELTTHER2
04719 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
04720      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
04721 **                                                               |ELTTHER2
04722 **---------------------------------------------------------------+ELTTHER2
04723                                                                   ELTTHER2
04724 **---------------------------------------------------------------+ELTTHER2
04725 **                                                               |ELTTHER2
04726 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
04727      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
04728 **                                                               |ELTTHER2
04729 **---------------------------------------------------------------+ELTTHER2
04730                                                                   ELTTHER2
04731 **---------------------------------------------------------------+ELTTHER2
04732 **                                                               |ELTTHER2
04733 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
04734 **                         A N D                                 |ELTTHER2
04735 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
04736      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
04737 **                                                               |ELTTHER2
04738 **---------------------------------------------------------------+ELTTHER2
04739                                                                   ELTTHER2
04740 **---------------------------------------------------------------+ELTTHER2
04741 **                                                               |ELTTHER2
04742 **               P E R F O R M   T A B U L A R                   |ELTTHER2
04743      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
04744 **                                                               |ELTTHER2
04745 **---------------------------------------------------------------+ELTTHER2
04746  6440-EXIT.  EXIT.                                                ELTTHER2
04747                                                                   ELTTHER2
04748                                                                   ELTTHER2
04749  6499-EXIT.            EXIT.                                      ELTTHER2
04750                                                                   ELTTHER2
04751 /    M I S C .   T H E R A P Y   O P   P R O F E S S I O N A L    ELTTHER2
04752  6600-CARD-THERP-OP-PROF-RTNE.                                    ELTTHER2
04753                                                                   ELTTHER2
04754      PERFORM 3000-COMMON-HEADER-RTNE.                             ELTTHER2
04755                                                                   ELTTHER2
04756      MOVE WS-HDR-2-CARD-OP-PROF  TO  COF-HDR-LINE(2).             ELTTHER2
04757                                                                   ELTTHER2
04758      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTTHER2
04759      PERFORM WITH TEST BEFORE                                     ELTTHER2
04760              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTTHER2
04761              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTTHER2
04762         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTTHER2
04763         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04764         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTTHER2
04765         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTTHER2
04766      END-PERFORM.                                                 ELTTHER2
04767      MOVE WS-CARD-OP-PROF-CNT  TO  PVN-NBR-BEN-PROVN.             ELTTHER2
04768                                                                   ELTTHER2
04769                                                                   ELTTHER2
04770      PERFORM WITH TEST BEFORE                                     ELTTHER2
04771         VARYING WS-SUB FROM +1 BY +1                              ELTTHER2
04772         UNTIL   WS-SUB  >     WS-CARD-OP-PROF-CNT                 ELTTHER2
04773           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTTHER2
04774           MOVE WS-CARD-OP-PROF-LIST (WS-SUB)                      ELTTHER2
04775                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTTHER2
04776            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTTHER2
04777                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTTHER2
04778      END-PERFORM.                                                 ELTTHER2
04779                                                                   ELTTHER2
04780      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTTHER2
04781      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04782                                         END-EXEC.                 ELTTHER2
04783                                                                   ELTTHER2
04784      MOVE WS-CARD-SERVICES  TO  SSB-TOPIC-PHRASE.                 ELTTHER2
04785                                                                   ELTTHER2
04786      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTTHER2
04787                                         END-EXEC.                 ELTTHER2
04788                                                                   ELTTHER2
04789      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTTHER2
04790      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04791                                         END-EXEC.                 ELTTHER2
04792                                                                   ELTTHER2
04793      IF PVN-COVG-NONE                                             ELTTHER2
04794         GO TO 6699-EXIT.                                          ELTTHER2
04795                                                                   ELTTHER2
04796      MOVE +1  TO  WS-CIA.                                         ELTTHER2
04797      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTTHER2
04798            PSP-PROVN-PRICING-METHD,                               ELTTHER2
04799            PSP-TRANSF-OTHER-RESP-IND,                             ELTTHER2
04800            PSP-SPILL-OVER-COINS-APL-IND,                          ELTTHER2
04801            PSP-SPILL-OVER-DED-APL-IND,                            ELTTHER2
04802            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTTHER2
04803            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTTHER2
04804            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTTHER2
04805            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTTHER2
04806            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTTHER2
04807            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTTHER2
04808            PSE-BEN-SCOPE-ID,                                      ELTTHER2
04809            PSE-BEN-MAX-VISITS-IND,                                ELTTHER2
04810            PSE-BEN-MAX-VISITS-DAYS,                               ELTTHER2
04811            PSE-MAX-AMT-PER-VISIT,                                 ELTTHER2
04812            PSE-HOSP-ADM-RESTRN-IND,                               ELTTHER2
04813            PSE-HSP-ADM-RESTRN-DAYS.                               ELTTHER2
04814                                                                   ELTTHER2
04815                                                                   ELTTHER2
04816      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTTHER2
04817                     COMMAREA (DFHCOMMAREA)                        ELTTHER2
04818                     END-EXEC.                                     ELTTHER2
04819                                                                   ELTTHER2
04820      PERFORM 0020-SET-ADR-OF-PLT-PAY-LVL.                         ELTTHER2
04821                                                                   ELTTHER2
04822      PERFORM 6630-FIND-FIRST-NONZERO                              ELTTHER2
04823         VARYING WS-SUB  FROM  +1  BY  +1                          ELTTHER2
04824         UNTIL WS-SUB  >  WS-CARD-OP-PROF-CNT.                     ELTTHER2
04825                                                                   ELTTHER2
04826      GO TO 6699-EXIT.                                             ELTTHER2
04827  6630-FIND-FIRST-NONZERO.                                         ELTTHER2
04828      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04829      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTTHER2
04830         CONTINUE                                                  ELTTHER2
04831      ELSE                                                         ELTTHER2
04832         PERFORM 6640-BUILD-SCREEN-LINES THRU 6640-EXIT.           ELTTHER2
04833                                                                   ELTTHER2
04834  6640-BUILD-SCREEN-LINES.                                         ELTTHER2
04835                                                                   ELTTHER2
04836      SET PLT-INDEX1  TO                                           ELTTHER2
04837                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTTHER2
04838      IF WS-NOT-FIRST-TIME                                         ELTTHER2
04839         MOVE 'P'  TO  COF-FUNCTION                                ELTTHER2
04840         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTTHER2
04841         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTTHER2
04842             COMMAREA(DFHCOMMAREA)                                 ELTTHER2
04843                                         END-EXEC                  ELTTHER2
04844      ELSE                                                         ELTTHER2
04845         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTTHER2
04846                                                                   ELTTHER2
04847      MOVE 1  TO  WS-CIA.                                          ELTTHER2
04848      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTTHER2
04849         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTTHER2
04850            SET PLT-INDEX2  TO  2                                  ELTTHER2
04851         ELSE                                                      ELTTHER2
04852            MOVE TABLE-MAX TO WS-SUB                               ELTTHER2
04853            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTTHER2
04854            GO TO 6640-EXIT                                        ELTTHER2
04855      ELSE                                                         ELTTHER2
04856         SET PLT-INDEX2  TO  1.                                    ELTTHER2
04857                                                                   ELTTHER2
04858 **---------------------------------------------------------------+ELTTHER2
04859 **                                                               |ELTTHER2
04860 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTTHER2
04861      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTTHER2
04862      ADD  +1  TO  WS-CIA.                                         ELTTHER2
04863      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTTHER2
04864      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTTHER2
04865         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTTHER2
04866         UNTIL  PVN-BEN-PROVN-IDX > WS-CARD-OP-PROF-CNT.           ELTTHER2
04867      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTTHER2
04868                                                                   ELTTHER2
04869      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTTHER2
04870      MOVE +1  TO  WS-CIA                                          ELTTHER2
04871      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTTHER2
04872                                         END-EXEC.                 ELTTHER2
04873 **---------------------------------------------------------------+ELTTHER2
04874 **                                                               |ELTTHER2
04875 **        P L A C E   O F   T R E A T M E N T                    |ELTTHER2
04876      PERFORM 3050-PLACE-TREAT.                                    ELTTHER2
04877 **                                                               |ELTTHER2
04878 **---------------------------------------------------------------+ELTTHER2
04879                                                                   ELTTHER2
04880 **---------------------------------------------------------------+ELTTHER2
04881 **                                                               |ELTTHER2
04882 **       B E N E F I T   S C O P E   I D E N T I F I E R         |ELTTHER2
04883      PERFORM 3600-BENEFIT-SCOPE-ID.                               ELTTHER2
04884 **                                                               |ELTTHER2
04885 **---------------------------------------------------------------+ELTTHER2
04886                                                                   ELTTHER2
04887 **---------------------------------------------------------------+ELTTHER2
04888 **                                                               |ELTTHER2
04889 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTTHER2
04890 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTTHER2
04891 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTTHER2
04892      PERFORM 3100-PROV-PRICING-METHD.                             ELTTHER2
04893 **                                                               |ELTTHER2
04894 **---------------------------------------------------------------+ELTTHER2
04895                                                                   ELTTHER2
04896 **---------------------------------------------------------------+ELTTHER2
04897 **                                                               |ELTTHER2
04898 **         B E N E F I T   M A X I M U M   V I S I T S           |ELTTHER2
04899      PERFORM 3800-BEN-MAXIMUM-VISITS.                             ELTTHER2
04900 **                                                               |ELTTHER2
04901 **---------------------------------------------------------------+ELTTHER2
04902                                                                   ELTTHER2
04903 **---------------------------------------------------------------+ELTTHER2
04904 **                                                               |ELTTHER2
04905 **  T R A N S F E R  T O  O T H E R  R E S P O N S I B I L I T Y |ELTTHER2
04906      PERFORM 3250-TRANSFER-OTHER-RESPON-IND                       ELTTHER2
04907         THRU 3250-EXIT.                                           ELTTHER2
04908 **                                                               |ELTTHER2
04909 **---------------------------------------------------------------+ELTTHER2
04910                                                                   ELTTHER2
04911 **---------------------------------------------------------------+ELTTHER2
04912 **                                                               |ELTTHER2
04913 **        M A X I M U M   A M O U N T   P E R   V I S I T        |ELTTHER2
04914      PERFORM 3900-MAX-AMOUNT-PER-VISIT.                           ELTTHER2
04915 **                                                               |ELTTHER2
04916 **---------------------------------------------------------------+ELTTHER2
04917                                                                   ELTTHER2
04918 **---------------------------------------------------------------+ELTTHER2
04919 **                                                               |ELTTHER2
04920 **  H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N  |ELTTHER2
04921      PERFORM 3500-HOSP-ADM-RESTRN.                                ELTTHER2
04922 **                                                               |ELTTHER2
04923 **---------------------------------------------------------------+ELTTHER2
04924                                                                   ELTTHER2
04925 **---------------------------------------------------------------+ELTTHER2
04926 **                                                               |ELTTHER2
04927 **          S P I L L   O V E R   C O I N S U R A N C E          |ELTTHER2
04928 **                         A N D                                 |ELTTHER2
04929 **          D E D U C T I B L E   A P P L I C A T I O N          |ELTTHER2
04930      PERFORM 3400-SPILLOVER-COINS-N-DEDBL.                        ELTTHER2
04931 **                                                               |ELTTHER2
04932 **---------------------------------------------------------------+ELTTHER2
04933                                                                   ELTTHER2
04934 **---------------------------------------------------------------+ELTTHER2
04935 **                                                               |ELTTHER2
04936 **               P E R F O R M   T A B U L A R                   |ELTTHER2
04937      PERFORM 3700-GENERAL-TABULAR-RTNE.                           ELTTHER2
04938 **                                                               |ELTTHER2
04939 **---------------------------------------------------------------+ELTTHER2
04940                                                                   ELTTHER2
04941  6640-EXIT.  EXIT.                                                ELTTHER2
04942                                                                   ELTTHER2
04943  6699-EXIT.           EXIT.                                       ELTTHER2
04944                                                                   ELTTHER2
04945      COPY ELSTCOMP.                                               ELTTHER2
04946 ***************************************************************** ELTTHER2
04947 **          PRINT THE TEXT INFORMATION FOR THE SCREEN           * ELTTHER2
04948 ***************************************************************** ELTTHER2
04949  9200-TEXT-OUTPUT-REQUEST.                                        ELTTHER2
04950                                                                   ELTTHER2
04951       MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                         ELTTHER2
04952       MOVE +0      TO  COF-NBR-HDR-LINES.                         ELTTHER2
04953       MOVE ' '     TO  COF-FUNCTION.                              ELTTHER2
04954                                                                   ELTTHER2
04955       EXEC CICS LINK PROGRAM ('ELUOUTPT')                         ELTTHER2
04956                      COMMAREA (DFHCOMMAREA)                       ELTTHER2
04957                      END-EXEC.                                    ELTTHER2
04958                                                                   ELTTHER2
04959                                                                   ELTTHER2
04960 ****************************************************************  ELTTHER2
04961 *          C A L L   C O D E S   M A N U A L                   *  ELTTHER2
04962 ****************************************************************  ELTTHER2
04963  9300-CALL-CODES-MANUAL.                                          ELTTHER2
04964      INITIALIZE CMF-RETURN-CODE,                                  ELTTHER2
04965                 TCAR-FROM-AREA.                                   ELTTHER2
04966                                                                   ELTTHER2
04967      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTTHER2
04968                       COMMAREA(DFHCOMMAREA)                       ELTTHER2
04969      END-EXEC.                                                    ELTTHER2
04970                                                                   ELTTHER2
04971      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTTHER2
04972      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTTHER2
04973          ADDRESS OF CMF-DESCR.                                    ELTTHER2
04974                                                                   ELTTHER2
04975 ****************************************************************  ELTTHER2
04976 * 9400-MOVE-TO-COFDTL                                          *  ELTTHER2
04977 * THE PURPOSE OF THIS PARAGRAPH IS TO MOVE EACH LINE RETURNED  *  ELTTHER2
04978 * FROM THE CODES MANUAL TO THE ARRAY INTERFACE FOR PROGRAM     *  ELTTHER2
04979 * ELUOUTPT.  WS-CIA IS THE SUBSRIPT.                           *  ELTTHER2
04980 * RGO. 10/12/95.                                               *  ELTTHER2
04981 ****************************************************************  ELTTHER2
04982  9400-MOVE-TO-COFDTL.                                             ELTTHER2
04983                                                                   ELTTHER2
04984                                                                   ELTTHER2
04985      ADD +1 TO WS-CIA.                                            ELTTHER2
04986      MOVE CMF-DESCR-LINE (WS-SUB-CMF) TO                          ELTTHER2
04987           COF-DTL-LINE (WS-CIA).                                  ELTTHER2
04988                                                                   ELTTHER2
04989      IF WS-CIA > 20 OR = 20                                       ELTTHER2
04990          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTTHER2
04991          MOVE ' ' TO COF-FUNCTION                                 ELTTHER2
04992          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTTHER2
04993                         COMMAREA (DFHCOMMAREA)                    ELTTHER2
04994                         END-EXEC                                  ELTTHER2
04995          MOVE +1 TO WS-CIA                                        ELTTHER2
04996      END-IF.                                                      ELTTHER2
04997                                                                   ELTTHER2
04998 ****************************************************************  ELTTHER2
04999 * 9400-COMPRESS-STRING-MOVE                                    *  ELTTHER2
05000 * THE PURPOSE OF THIS PARAGRAPH IS TO COMPRESS WHAT IS RETURNED*  ELTTHER2
05001 * FROM THE CODES MANUAL, STRING IT INTO 79 CHARACTER LINES,    *  ELTTHER2
05002 * AND DISPLAY IT STARTING ON A NEW LINE.                       *  ELTTHER2
05003 * RGO. 11/09/95.                                               *  ELTTHER2
05004 ****************************************************************  ELTTHER2
05005  9400-COMPRESS-STRING-MOVE.                                       ELTTHER2
05006      MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN.                          ELTTHER2
05007      STRING CMF-DESCR-LINE(1), ' '                                ELTTHER2
05008         CMF-DESCR-LINE(2), ' '                                    ELTTHER2
05009         CMF-DESCR-LINE(3), ' '                                    ELTTHER2
05010         CMF-DESCR-LINE(4), ' '                                    ELTTHER2
05011         CMF-DESCR-LINE(5), ' '                                    ELTTHER2
05012         CMF-DESCR-LINE(6), ' '                                    ELTTHER2
05013         CMF-DESCR-LINE(7), ' '                                    ELTTHER2
05014         CMF-DESCR-LINE(8), ' '                                    ELTTHER2
05015         CMF-DESCR-LINE(9), ' '                                    ELTTHER2
05016         CMF-DESCR-LINE(10), ' '                                   ELTTHER2
05017         CMF-DESCR-LINE(11), ' '                                   ELTTHER2
05018         CMF-DESCR-LINE(12), ' '                                   ELTTHER2
05019         CMF-DESCR-LINE(13), ' '                                   ELTTHER2
05020         CMF-DESCR-LINE(14), ' '                                   ELTTHER2
05021         CMF-DESCR-LINE(15), ' '                                   ELTTHER2
05022         DELIMITED BY SIZE INTO TCAR-FROM-AREA.                    ELTTHER2
05023                                                                   ELTTHER2
05024      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTTHER2
05025      MOVE +15              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTTHER2
05026      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN,           ELTTHER2
05027                                TCAR-OUTPUT-FIELD-3-LEN,           ELTTHER2
05028                                TCAR-OUTPUT-FIELD-4-LEN,           ELTTHER2
05029                                TCAR-OUTPUT-FIELD-5-LEN,           ELTTHER2
05030                                TCAR-OUTPUT-FIELD-6-LEN,           ELTTHER2
05031                                TCAR-OUTPUT-FIELD-7-LEN,           ELTTHER2
05032                                TCAR-OUTPUT-FIELD-8-LEN,           ELTTHER2
05033                                TCAR-OUTPUT-FIELD-9-LEN,           ELTTHER2
05034                                TCAR-OUTPUT-FIELD-10-LEN,          ELTTHER2
05035                                TCAR-OUTPUT-FIELD-11-LEN,          ELTTHER2
05036                                TCAR-OUTPUT-FIELD-12-LEN,          ELTTHER2
05037                                TCAR-OUTPUT-FIELD-13-LEN,          ELTTHER2
05038                                TCAR-OUTPUT-FIELD-14-LEN,          ELTTHER2
05039                                TCAR-OUTPUT-FIELD-15-LEN.          ELTTHER2
05040      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTTHER2
05041                                                                   ELTTHER2
05042      IF WS-MOVE-LINES-TO-CIA                                      ELTTHER2
05043         PERFORM 9450-MOVE-STRUNG                                  ELTTHER2
05044            VARYING WS-SUB1 FROM 1 BY 1                            ELTTHER2
05045            UNTIL WS-SUB1 > TCAR-OUTPUT-FIELDS-USED                ELTTHER2
05046      END-IF.                                                      ELTTHER2
05047                                                                   ELTTHER2
05048  9450-MOVE-STRUNG.                                                ELTTHER2
05049      ADD 1 TO WS-CIA.                                             ELTTHER2
05050      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTTHER2
05051                                                                   ELTTHER2
05052      IF WS-CIA > 20 OR = 20                                       ELTTHER2
05053          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTTHER2
05054          MOVE ' ' TO COF-FUNCTION                                 ELTTHER2
05055          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTTHER2
05056                         COMMAREA (DFHCOMMAREA)                    ELTTHER2
05057                         END-EXEC                                  ELTTHER2
05058          MOVE +1 TO WS-CIA                                        ELTTHER2
05059      END-IF.                                                      ELTTHER2
05060                                                                   ELTTHER2
