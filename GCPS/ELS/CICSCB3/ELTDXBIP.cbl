00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTDXBIP.                                            ELTDXBIP
00003  AUTHOR. ANNE KEFFER KING.                                           LV001
00004  DATE-WRITTEN.   09/26/95.                                        ELTDXBIP
00005  DATE-COMPILED.                                                   ELTDXBIP
00006      SKIP3                                                        ELTDXBIP
00007 ******************************************************************ELTDXBIP
00008 *@>ELTDIAGS                                                       ELTDXBIP
00009 *@¬                                                               ELTDXBIP
00010 *                        PROGRAM ABSTRACT                         ELTDXBIP
00011 *                                                                 ELTDXBIP
00012 *@¬ PROGRAM NAME:   E.L.S. BASIC INPATIENT DIAGNOSTIC SERVICES    ELTDXBIP
00013 *@¬                                                               ELTDXBIP
00014 *@¬ PROGRAM I.D.:   ELTDXBIP.                                     ELTDXBIP
00015 *@¬                                                               ELTDXBIP
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDXBIP
00017 *@¬            DIAGNOSTIC --BASIC INPATIENT SERVICES.             ELTDXBIP
00018 *@¬                                                               ELTDXBIP
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DIAGNOSTIC        ELTDXBIP
00020 *@¬            PRROCEDURES IS AFFORD A MEMBER BY HIS GROUP.       ELTDXBIP
00021 *@¬            THIS INFORMATION IS                                ELTDXBIP
00022 *@¬            GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTDXBIP
00023 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTDXBIP
00024 *@¬            RANGE OF DATES.                                    ELTDXBIP
00025 *@¬                                                               ELTDXBIP
00026 *@¬ RECORDS                                                       ELTDXBIP
00027 *@¬ ACCESSED:  VARIOUS BENEFIT PROVISION, AND A                   ELTDXBIP
00028 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTDXBIP
00029 *@¬                                                               ELTDXBIP
00030 *@¬ PROCESSING                                                    ELTDXBIP
00031 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTDXBIP
00032 *@¬                                                               ELTDXBIP
00033 *@¬                                                               ELTDXBIP
00034 ***************************************************************** ELTDXBIP
00035      SKIP3                                                        ELTDXBIP
00036 ***************************************************************** ELTDXBIP
00037 *                    U P D A T E   H I S T O R Y                * ELTDXBIP
00038 *                                                               * ELTDXBIP
00039 *   DATE    PGM  DESCRIPTION  (MOST CURRENT AT TOP)             * ELTDXBIP
00040 * --------  ---  ---------------------------------------------- * ELTDXBIP
00041 *                                                               * ELTDXBIP
00042 * 09/26/95  AKK  CREATED.  CLONED FROM ELTDIAGS.                * ELTDXBIP
00043 *                                                               * ELTDXBIP
00044 * 11/29/95  AKK  CHANGED DUE TO CUSTOMER REQUEST FOR CHANGE    *  ELTDXBIP
00045 *                IN OUTPUT METHOD.                              * ELTDXBIP
00046 ***************************************************************** ELTDXBIP
00047 *                                                               * ELTDXBIP
00048 ***************************************************************** ELTDXBIP
00049 /                                                                 ELTDXBIP
00050  ENVIRONMENT DIVISION.                                            ELTDXBIP
00051      SKIP3                                                        ELTDXBIP
00052  DATA DIVISION.                                                   ELTDXBIP
00053  WORKING-STORAGE SECTION.                                         ELTDXBIP
00054  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDXBIP
00055      '***ELTDXIP WS BEGINS***'.                                   ELTDXBIP
00056  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTDXBIP
00057                                                                   ELTDXBIP
00058  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTDXBIP
00059                                                                   ELTDXBIP
00060 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDXBIP
00061  01  WS-WORK-FIELDS.                                              ELTDXBIP
00062      05  WS-CHAR-0                     PIC X.                     ELTDXBIP
00063      05  WS-HOLD1                      PIC X(10).                 ELTDXBIP
00064      05  WS-HOLD2                      PIC X(10).                 ELTDXBIP
00065      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTDXBIP
00066      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTDXBIP
00067      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTDXBIP
00068      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTDXBIP
00069      05  WS-DTL-PP                     PIC X(50).                 ELTDXBIP
00070      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTDXBIP
00071      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTDXBIP
00072      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTDXBIP
00073      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTDXBIP
00074      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTDXBIP
00075      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTDXBIP
00076      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDXBIP
00077      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTDXBIP
00078      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTDXBIP
00079      05  WS-FIRSTTIME-IND              PIC X.                     ELTDXBIP
00080          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTDXBIP
00081      05  WS-ADD-A-BLANK-IND            PIC  X(01) VALUE 'N'.      ELTDXBIP
00082          88  WS-ADD-A-BLANK-LINE                  VALUE 'Y'.      ELTDXBIP
00083                                                                   ELTDXBIP
00084      05  WS-POT-SWITCH                 PIC  X(01) VALUE SPACE.    ELTDXBIP
00085          88  WS-PROCESS-POT                       VALUE 'P'.      ELTDXBIP
00086                                                                   ELTDXBIP
00087      05 WS-SINGLE-QUOTE                PIC X  VALUE ''''.         ELTDXBIP
00088      05 WS-BASIC-LINE                  PIC X.                     ELTDXBIP
00089         88  WS-BSC-LINE                       VALUE '1' '2' '3'.  ELTDXBIP
00090                                                                   ELTDXBIP
00091      05  WS-TEST-LINE.                                            ELTDXBIP
00092          10  WS-TEST-CHAR              PIC X.                     ELTDXBIP
00093          10  WS-TEST-DATA              PIC X(78).                 ELTDXBIP
00094                                                                   ELTDXBIP
00095      05  WS-LOB-SWITCH                 PIC X   VALUE SPACE.       ELTDXBIP
00096          88  WS-BASIC-LOB                      VALUE 'B'.         ELTDXBIP
00097          88  WS-NOT-BASIC-LOB                  VALUE SPACE.       ELTDXBIP
00098                                                                   ELTDXBIP
00099      05  WS-BASIC-SUPP-SWITCH          PIC X   VALUE SPACE.       ELTDXBIP
00100          88  PROCESSING-BASIC-INFO             VALUE 'I'.         ELTDXBIP
00101          88  PROCESSING-SUPPLEMENTAL           VALUE 'P'.         ELTDXBIP
00102                                                                   ELTDXBIP
00103      05  WS-PROCESSING-SWITCH          PIC X   VALUE SPACE.       ELTDXBIP
00104          88  PROCESSING-INSTITUTIONAL          VALUE 'I'.         ELTDXBIP
00105          88  PROCESSING-PROFESSIONAL           VALUE 'P'.         ELTDXBIP
00106                                                                   ELTDXBIP
00107 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTDXBIP
00108  01  WS-BEN-PROV-ID.                                              ELTDXBIP
00109      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +14.ELTDXBIP
00110      05  WS-INST-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDXBIP
00111      05  WS-PROF-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDXBIP
00112                                                                   ELTDXBIP
00113      05  WS-INST-CNT                   PIC S9(4) COMP  VALUE +14. ELTDXBIP
00114      05  WS-INST-TAB.                                             ELTDXBIP
00115        10  FILLER                      PIC X(6)  VALUE 'ATI  B'.  ELTDXBIP
00116        10  FILLER                      PIC X(6)  VALUE 'ATII B'.  ELTDXBIP
00117        10  FILLER                      PIC X(6)  VALUE 'DMPI B'.  ELTDXBIP
00118        10  FILLER                      PIC X(6)  VALUE 'HTEI B'.  ELTDXBIP
00119        10  FILLER                      PIC X(6)  VALUE 'LABI B'.  ELTDXBIP
00120        10  FILLER                      PIC X(6)  VALUE 'PAPI B'.  ELTDXBIP
00121        10  FILLER                      PIC X(6)  VALUE 'PAT  B'.  ELTDXBIP
00122        10  FILLER                      PIC X(6)  VALUE 'PPFI B'.  ELTDXBIP
00123        10  FILLER                      PIC X(6)  VALUE 'PSI  B'.  ELTDXBIP
00124        10  FILLER                      PIC X(6)  VALUE 'RCXI B'.  ELTDXBIP
00125        10  FILLER                      PIC X(6)  VALUE 'RII  B'.  ELTDXBIP
00126        10  FILLER                      PIC X(6)  VALUE 'RPFI B'.  ELTDXBIP
00127        10  FILLER                      PIC X(6)  VALUE 'VTI  B'.  ELTDXBIP
00128        10  FILLER                      PIC X(6)  VALUE 'XRYI B'.  ELTDXBIP
00129      05  WS-INST-LIST     REDEFINES    WS-INST-TAB                ELTDXBIP
00130                                        PIC X(6)  OCCURS 14 TIMES. ELTDXBIP
00131                                                                   ELTDXBIP
00132      05  WS-PROF-CNT                   PIC S9(4) COMP  VALUE +13. ELTDXBIP
00133      05  WS-PROF-TAB.                                             ELTDXBIP
00134        10  FILLER                      PIC X(6)  VALUE 'ATII E'.  ELTDXBIP
00135        10  FILLER                      PIC X(6)  VALUE 'ATSI E'.  ELTDXBIP
00136        10  FILLER                      PIC X(6)  VALUE 'DMPI E'.  ELTDXBIP
00137        10  FILLER                      PIC X(6)  VALUE 'HTEI E'.  ELTDXBIP
00138        10  FILLER                      PIC X(6)  VALUE 'LABI E'.  ELTDXBIP
00139        10  FILLER                      PIC X(6)  VALUE 'PAPI E'.  ELTDXBIP
00140        10  FILLER                      PIC X(6)  VALUE 'PAT  E'.  ELTDXBIP
00141        10  FILLER                      PIC X(6)  VALUE 'PSI  D'.  ELTDXBIP
00142        10  FILLER                      PIC X(6)  VALUE 'PTI  E'.  ELTDXBIP
00143        10  FILLER                      PIC X(6)  VALUE 'PXR  E'.  ELTDXBIP
00144        10  FILLER                      PIC X(6)  VALUE 'RII  E'.  ELTDXBIP
00145        10  FILLER                      PIC X(6)  VALUE 'VTI  E'.  ELTDXBIP
00146        10  FILLER                      PIC X(6)  VALUE 'XRYI E'.  ELTDXBIP
00147      05  WS-PROF-LIST        REDEFINES    WS-PROF-TAB             ELTDXBIP
00148                                        PIC X(6)  OCCURS 13 TIMES. ELTDXBIP
00149                                                                   ELTDXBIP
00150 /                L I T E R A L S                                  ELTDXBIP
00151  01  WS-PROGRAM-LITERALS.                                         ELTDXBIP
00152    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTDXBIP
00153    05  DAYS                        PIC X(04) VALUE 'DAYS'.        ELTDXBIP
00154    05  WS-NO                       PIC X     VALUE 'N'.           ELTDXBIP
00155    05  WS-YES                      PIC X     VALUE 'Y'.           ELTDXBIP
00156    05  WS-BASIC-LIT                PIC X(08) VALUE                ELTDXBIP
00157        'BASIC: '.                                                 ELTDXBIP
00158    05  WS-SECONDARY-LIT            PIC X(12) VALUE                ELTDXBIP
00159        'SECONDARY: '.                                             ELTDXBIP
00160    05  WS-SUPPLEMENTAL-LIT         PIC X(15) VALUE                ELTDXBIP
00161        'SUPPLEMENTAL: '.                                          ELTDXBIP
00162    05  WS-DAYS-REDUCED             PIC X(44) VALUE                ELTDXBIP
00163          ' OUTPATIENT DIALYSIS TREATMENTS REDUCE DAYS '.          ELTDXBIP
00164    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTDXBIP
00165    05  WS-PAYMNT-BASED             PIC X(20)                      ELTDXBIP
00166          VALUE 'PAYMENT IS BASED ON:'.                            ELTDXBIP
00167    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTDXBIP
00168          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTDXBIP
00169    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTDXBIP
00170          VALUE 'SPILLOVER COINSURANCE: '.                         ELTDXBIP
00171    05  WS-SERVICES-RENDERED        PIC X(26)                      ELTDXBIP
00172          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTDXBIP
00173                                                                   ELTDXBIP
00174 /            D I S P L A Y   L I N E S                            ELTDXBIP
00175  01  WS-ELS-DISPLAY-LINES.                                        ELTDXBIP
00176    05  WS-HDR-1.                                                  ELTDXBIP
00177      10  FILLER                    PIC X(12) VALUE 'SECTION NO: '.ELTDXBIP
00178      10  WS-HDR1-SECT-NO           PIC X(5)  VALUE SPACES.        ELTDXBIP
00179      10  FILLER                    PIC X(22)                      ELTDXBIP
00180          VALUE '      EFFECTIVE DATE: '.                          ELTDXBIP
00181      10  WS-HDR1-DATE              PIC X(8).                      ELTDXBIP
00182      10  FILLER                    PIC X(28)                      ELTDXBIP
00183          VALUE '      FAMILY RELATIONSHIP: '.                     ELTDXBIP
00184      10  WS-HDR1-FRL               PIC X.                         ELTDXBIP
00185      10  FILLER                    PIC X(4) VALUE LOW-VALUES.     ELTDXBIP
00186                                                                   ELTDXBIP
00187    05  WS-HDR-2-INST-IP.                                          ELTDXBIP
00188      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBIP
00189      10  FILLER                    PIC X(43)                      ELTDXBIP
00190         VALUE 'INPATIENT DIAGNOSTIC SERVICES INSTITUTIONAL'.      ELTDXBIP
00191      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBIP
00192                                                                   ELTDXBIP
00193    05  WS-HDR-2-PROF.                                             ELTDXBIP
00194      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBIP
00195      10  FILLER                    PIC X(42)                      ELTDXBIP
00196        VALUE 'INPATIENT DIAGNOSTIC SERVICES PROFESSIONAL'.        ELTDXBIP
00197      10  FILLER                    PIC X(19) VALUE SPACES.        ELTDXBIP
00198                                                                   ELTDXBIP
00199    05  WS-INPATIENT-DX-ARE.                                       ELTDXBIP
00200      10  FILLER                    PIC X(30) VALUE                ELTDXBIP
00201          'INPATIENT DIAGNOSTIC SERVICES'.                         ELTDXBIP
00202      10  FILLER                    PIC X(49) VALUE SPACES.        ELTDXBIP
00203                                                                   ELTDXBIP
00204    05  WS-FOLLOW-BENEFIT.                                         ELTDXBIP
00205      10  FILLER                    PIC X(79) VALUE                ELTDXBIP
00206          'COVERED SERVICES ARE:'.                                 ELTDXBIP
00207                                                                   ELTDXBIP
00208    05  WS-PAY-CONSDR-TEXT1.                                       ELTDXBIP
00209      10  FILLER                    PIC  X(45)                     ELTDXBIP
00210        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDXBIP
00211                                                                   ELTDXBIP
00212    05  WS-PAY-CONSDR-TEXT2.                                       ELTDXBIP
00213      10  FILLER                    PIC X(45)                      ELTDXBIP
00214        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDXBIP
00215                                                                   ELTDXBIP
00216    05  WS-PROF-INPT-CHRGES         PIC  X(61) VALUE               ELTDXBIP
00217            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTDXBIP
00218 -          'EPORT: '.                                             ELTDXBIP
00219                                                                   ELTDXBIP
00220    05  WS-SERVICES-2ND.                                           ELTDXBIP
00221      10  FILLER                    PIC X(21) VALUE SPACES.        ELTDXBIP
00222      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTDXBIP
00223      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTDXBIP
00224                                                                   ELTDXBIP
00225    05  WS-SERVICES-PAYABLE.                                       ELTDXBIP
00226      10  FILLER                    PIC X(45)                      ELTDXBIP
00227          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTDXBIP
00228      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDXBIP
00229                                                                   ELTDXBIP
00230    05  WS-BASIC                    PIC X(08) VALUE 'BASIC: '.     ELTDXBIP
00231    05  WS-BASIC-A.                                                ELTDXBIP
00232      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXBIP
00233      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDXBIP
00234      10  WS-DTL-BASIC-A            PIC X(63) VALUE SPACES.        ELTDXBIP
00235                                                                   ELTDXBIP
00236    05  WS-SUPPLEMENTAL.                                           ELTDXBIP
00237      10  FILLER                    PIC X(16)                      ELTDXBIP
00238          VALUE '  SUPPLEMENTAL: '.                                ELTDXBIP
00239      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTDXBIP
00240                                                                   ELTDXBIP
00241    05  WS-BASIC-PERCENT.                                          ELTDXBIP
00242      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXBIP
00243      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDXBIP
00244      10  WS-DTL-BASIC-A-PER        PIC X(63) VALUE SPACES.        ELTDXBIP
00245                                                                   ELTDXBIP
00246    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTDXBIP
00247      10  FILLER                    PIC X(16)                      ELTDXBIP
00248          VALUE '  SUPPLEMENTAL: '.                                ELTDXBIP
00249      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTDXBIP
00250                                                                   ELTDXBIP
00251    05  WS-CONTACT-CONTRACT.                                       ELTDXBIP
00252      10  FILLER                    PIC X(50)                      ELTDXBIP
00253        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTDXBIP
00254      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDXBIP
00255                                                                   ELTDXBIP
00256    05  WS-CONTRACT-RELATED.                                       ELTDXBIP
00257      10  FILLER                    PIC X(49)                      ELTDXBIP
00258        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTDXBIP
00259      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDXBIP
00260                                                                   ELTDXBIP
00261    05  WS-PVE-TEXT.                                               ELTDXBIP
00262      10  FILLER                    PIC X(44) VALUE                ELTDXBIP
00263        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTDXBIP
00264                                                                   ELTDXBIP
00265  01  WS-END                            PIC X(16)  VALUE           ELTDXBIP
00266      '*** W/S ENDS ***'.                                          ELTDXBIP
00267 /             L I N K A G E   S E C T I O N                       ELTDXBIP
00268  LINKAGE SECTION.                                                 ELTDXBIP
00269  01  DFHCOMMAREA.                                                 ELTDXBIP
00270      COPY ELSCOMMC.                                               ELTDXBIP
00271 /  *** CIA  AREA ***                                              ELTDXBIP
00272      COPY ELSCIA2C.                                               ELTDXBIP
00273 /  *** IO PARM AREA ***                                           ELTDXBIP
00274      COPY ELSIOPMC.                                               ELTDXBIP
00275 /  *** KEY AREA ***                                               ELTDXBIP
00276      COPY ELSKEYSC.                                               ELTDXBIP
00277 /  *** OUTPUT TEXT AREA ***                                       ELTDXBIP
00278      COPY ELSOUTPC.                                               ELTDXBIP
00279 /  *** TOPIC SELECTION AREA ***                                   ELTDXBIP
00280      COPY ELSSSCBC.                                               ELTDXBIP
00281 /  *** CODE MANUAL INTERFACE ***                                  ELTDXBIP
00282      COPY ELSCMIFC.                                               ELTDXBIP
00283 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDXBIP
00284      COPY ELSCMDSC.                                               ELTDXBIP
00285 /  *** BENEFIT PROVISION TABLE ***                                ELTDXBIP
00286      COPY ELSPRVNC.                                               ELTDXBIP
00287 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDXBIP
00288      COPY ELSTCWAC.                                               ELTDXBIP
00289 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDXBIP
00290      COPY ELSPLGSW.                                               ELTDXBIP
00291                                                                   ELTDXBIP
00292 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDXBIP
00293      COPY ELSPLGTB.                                               ELTDXBIP
00294                                                                   ELTDXBIP
00295  01 CONTRACT-RECORD.                                              ELTDXBIP
00296      COPY GCCONTRC.                                               ELTDXBIP
00297 /                  M A I N L I N E                                ELTDXBIP
00298  PROCEDURE DIVISION.                                              ELTDXBIP
00299                                                                   ELTDXBIP
00300 ******************************************************************ELTDXBIP
00301 *                                                                 ELTDXBIP
00302 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDXBIP
00303 *                                                                 ELTDXBIP
00304 ******************************************************************ELTDXBIP
00305  0000-MAINLINE.                                                   ELTDXBIP
00306                                                                   ELTDXBIP
00307      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDXBIP
00308         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDXBIP
00309                                                                   ELTDXBIP
00310      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDXBIP
00311                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELTDXBIP
00312                                                                   ELTDXBIP
00313      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDXBIP
00314      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00315          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDXBIP
00316      IF NOT CIA-RC-OK                                             ELTDXBIP
00317          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00318                                                                   ELTDXBIP
00319      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDXBIP
00320      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00321          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDXBIP
00322      IF NOT CIA-RC-OK                                             ELTDXBIP
00323          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00324                                                                   ELTDXBIP
00325      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDXBIP
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00327          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDXBIP
00328      IF NOT CIA-RC-OK                                             ELTDXBIP
00329          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00330                                                                   ELTDXBIP
00331      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDXBIP
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00333          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDXBIP
00334      IF NOT CIA-RC-OK                                             ELTDXBIP
00335          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00336                                                                   ELTDXBIP
00337      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDXBIP
00338      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00339          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDXBIP
00340      IF NOT CIA-RC-OK                                             ELTDXBIP
00341          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00342                                                                   ELTDXBIP
00343      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDXBIP
00344      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00345          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDXBIP
00346      IF NOT CIA-RC-OK                                             ELTDXBIP
00347          PERFORM 9998-INVALID-PTR.                                ELTDXBIP
00348                                                                   ELTDXBIP
00349      MOVE '0'  TO  WS-CHAR-0.                                     ELTDXBIP
00350                                                                   ELTDXBIP
00351      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDXBIP
00352                                                                   ELTDXBIP
00353      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDXBIP
00354              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDXBIP
00355                                                                   ELTDXBIP
00356      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXBIP
00357                                                                   ELTDXBIP
00358      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDXBIP
00359                                                                   ELTDXBIP
00360      EXEC CICS LINK                                               ELTDXBIP
00361                PROGRAM('ELUSTGMG')                                ELTDXBIP
00362                COMMAREA(DFHCOMMAREA)                              ELTDXBIP
00363      END-EXEC.                                                    ELTDXBIP
00364                                                                   ELTDXBIP
00365      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXBIP
00366      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00367          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDXBIP
00368                                                                   ELTDXBIP
00369      PERFORM 9999-CHECK-CONTRACT.                                 ELTDXBIP
00370                                                                   ELTDXBIP
00371      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTDXBIP
00372         PERFORM 1000-INSTITUTIONAL-RTNE.                          ELTDXBIP
00373                                                                   ELTDXBIP
00374      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTDXBIP
00375         PERFORM 2000-PROFESSIONAL-RTNE.                           ELTDXBIP
00376                                                                   ELTDXBIP
00377      IF (SSB-PROV-CLASS-INST  OR                                  ELTDXBIP
00378            SSB-PROV-CLASS-PROF  OR                                ELTDXBIP
00379            SSB-PROV-CLASS-BOTH)                                   ELTDXBIP
00380               CONTINUE                                            ELTDXBIP
00381      ELSE                                                         ELTDXBIP
00382         SET CIA-AB-UNDEF TO TRUE                                  ELTDXBIP
00383         EXEC CICS ABEND                                           ELTDXBIP
00384                   ABCODE(CIA-ABCODE)                              ELTDXBIP
00385         END-EXEC                                                  ELTDXBIP
00386      END-IF.                                                      ELTDXBIP
00387 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTDXBIP
00388       MOVE +0  TO  COF-NBR-HDR-LINES.                             ELTDXBIP
00389       MOVE +0  TO  COF-NBR-DTL-LINES.                             ELTDXBIP
00390       MOVE 'E' TO  COF-FUNCTION.                                  ELTDXBIP
00391                                                                   ELTDXBIP
00392      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBIP
00393      END-EXEC.                                                    ELTDXBIP
00394                                                                   ELTDXBIP
00395  0099-RETURN.                                                     ELTDXBIP
00396      GOBACK.                                                      ELTDXBIP
00397                                                                   ELTDXBIP
00398 /        I N S T I T U T I O N A L  R T N E                       ELTDXBIP
00399 ***************************************************************** ELTDXBIP
00400 *        I N S T I T U T I O N A L  R T N E                       ELTDXBIP
00401 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDXBIP
00402 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDXBIP
00403 ***************************************************************** ELTDXBIP
00404  1000-INSTITUTIONAL-RTNE.                                         ELTDXBIP
00405      MOVE '1000'  TO  WS-PARA-ID.                                 ELTDXBIP
00406      MOVE WS-HDR-2-INST-IP TO  COF-HDR-LINE(2).                   ELTDXBIP
00407                                                                   ELTDXBIP
00408      MOVE WS-INST-CNT TO PVN-NBR-BEN-PROVN,                       ELTDXBIP
00409                                 WS-INST-COUNTER.                  ELTDXBIP
00410      PERFORM 6000-MOVE-IN-INST                                    ELTDXBIP
00411         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXBIP
00412         UNTIL WS-SUB  >  WS-INST-CNT.                             ELTDXBIP
00413                                                                   ELTDXBIP
00414      MOVE WS-INPATIENT-DX-ARE TO SSB-TOPIC-PHRASE.                ELTDXBIP
00415      PERFORM 8000-CALL-COVERAGE.                                  ELTDXBIP
00416      IF PVN-COVG-NONE                                             ELTDXBIP
00417         NEXT SENTENCE                                             ELTDXBIP
00418      ELSE                                                         ELTDXBIP
00419         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDXBIP
00420                                                                   ELTDXBIP
00421                                                                   ELTDXBIP
00422 /        I N S T I T U T I O N A L   C O M M O N  R T N E         ELTDXBIP
00423  1600-INSTITUTIONAL-COMMON.                                       ELTDXBIP
00424      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDXBIP
00425                                                                   ELTDXBIP
00426      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXBIP
00427                    PSP-PROVN-PRICING-METHD,                       ELTDXBIP
00428                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBIP
00429                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDXBIP
00430                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDXBIP
00431                    PSB-PROF-CHRG-HSP-CLM.                         ELTDXBIP
00432                                                                   ELTDXBIP
00433      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXBIP
00434      END-EXEC.                                                    ELTDXBIP
00435                                                                   ELTDXBIP
00436      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXBIP
00437      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00438          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDXBIP
00439                                                                   ELTDXBIP
00440      PERFORM 1630-FIND-FIRST-NONZERO                              ELTDXBIP
00441         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXBIP
00442         UNTIL WS-SUB  >  WS-INST-COUNTER.                         ELTDXBIP
00443                                                                   ELTDXBIP
00444                                                                   ELTDXBIP
00445  1630-FIND-FIRST-NONZERO.                                         ELTDXBIP
00446      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDXBIP
00447      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDXBIP
00448         NEXT SENTENCE                                             ELTDXBIP
00449      ELSE                                                         ELTDXBIP
00450         PERFORM 1640-BUILD-SCREEN-LINES.                          ELTDXBIP
00451                                                                   ELTDXBIP
00452  1640-BUILD-SCREEN-LINES.                                         ELTDXBIP
00453      MOVE '1640'  TO  WS-PARA-ID.                                 ELTDXBIP
00454                                                                   ELTDXBIP
00455      SET PLT-INDEX1 TO                                            ELTDXBIP
00456         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDXBIP
00457                                                                   ELTDXBIP
00458      IF WS-NOT-FIRST-TIME                                         ELTDXBIP
00459         MOVE 'P' TO COF-FUNCTION                                  ELTDXBIP
00460 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDXBIP
00461 ******** REB ===> 12/15/87.                                       ELTDXBIP
00462 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDXBIP
00463         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDXBIP
00464         EXEC CICS LINK                                            ELTDXBIP
00465                   PROGRAM('ELUOUTPT')                             ELTDXBIP
00466                   COMMAREA(DFHCOMMAREA)                           ELTDXBIP
00467         END-EXEC                                                  ELTDXBIP
00468      ELSE                                                         ELTDXBIP
00469        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDXBIP
00470                                                                   ELTDXBIP
00471      MOVE +1  TO  WS-CIA.                                         ELTDXBIP
00472      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXBIP
00473         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXBIP
00474            SET PLT-INDEX2  TO  2                                  ELTDXBIP
00475         ELSE                                                      ELTDXBIP
00476            PERFORM 1690-PROBLEM-WITH-INDICES                      ELTDXBIP
00477            PERFORM 3000-OUTPUT-TEXT                               ELTDXBIP
00478      ELSE                                                         ELTDXBIP
00479         SET PLT-INDEX2  TO  1.                                    ELTDXBIP
00480      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDXBIP
00481                                                                   ELTDXBIP
00482      ADD +1                 TO WS-CIA.                            ELTDXBIP
00483      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDXBIP
00484      ADD +1                 TO WS-CIA.                            ELTDXBIP
00485                                                                   ELTDXBIP
00486      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTDXBIP
00487                                                                   ELTDXBIP
00488      PERFORM 1650-ZERO-ALL-WITH-SAME-NO                           ELTDXBIP
00489         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDXBIP
00490         UNTIL WS-SUB2  >  WS-INST-COUNTER.                        ELTDXBIP
00491                                                                   ELTDXBIP
00492      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXBIP
00493               NOT = '0' AND NOT = LOW-VALUES                      ELTDXBIP
00494           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDXBIP
00495                                                                   ELTDXBIP
00496      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBIP
00497 *    ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00498      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDXBIP
00499      ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00500                                                                   ELTDXBIP
00501      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00502         SET  PLT-INDEX2           TO  1                           ELTDXBIP
00503         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDXBIP
00504                                                                   ELTDXBIP
00505      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBIP
00506         SET  PLT-INDEX2             TO  2                         ELTDXBIP
00507         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDXBIP
00508                                                                   ELTDXBIP
00509      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00510         SET  PLT-INDEX2           TO  1                           ELTDXBIP
00511                  IF WS-BASIC-LOB                                  ELTDXBIP
00512                     SET PROCESSING-BASIC-INFO TO TRUE             ELTDXBIP
00513                  END-IF                                           ELTDXBIP
00514         PERFORM 4500-TRANS-OTHER-RESP-IND                         ELTDXBIP
00515      ELSE                                                         ELTDXBIP
00516         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDXBIP
00517            SET  PLT-INDEX2             TO  2                      ELTDXBIP
00518           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDXBIP
00519                                                                   ELTDXBIP
00520      PERFORM 4250-PROF-CHGR-HSP-CLM.                              ELTDXBIP
00521                                                                   ELTDXBIP
00522      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00523        SET   PLT-INDEX2          TO  1                            ELTDXBIP
00524        PERFORM 4300-SPILLOVER-COINS.                              ELTDXBIP
00525                                                                   ELTDXBIP
00526      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00527        SET   PLT-INDEX2          TO  1                            ELTDXBIP
00528        PERFORM 4400-SPILLOVER-DEDUCT.                             ELTDXBIP
00529 *      PERFORM 3000-OUTPUT-TEXT.                                  ELTDXBIP
00530                                                                   ELTDXBIP
00531      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00532        SET   PLT-INDEX2          TO  2                            ELTDXBIP
00533        PERFORM 4400-SPILLOVER-DEDUCT.                             ELTDXBIP
00534 *      PERFORM 3000-OUTPUT-TEXT.                                  ELTDXBIP
00535                                                                   ELTDXBIP
00536      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXBIP
00537      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXBIP
00538                                                                   ELTDXBIP
00539  1650-ZERO-ALL-WITH-SAME-NO.                                      ELTDXBIP
00540      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDXBIP
00541                                                                   ELTDXBIP
00542      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTDXBIP
00543          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXBIP
00544              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTDXBIP
00545                                                                   ELTDXBIP
00546      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDXBIP
00547         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXBIP
00548         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXBIP
00549         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXBIP
00550                                                   CMF-CODE-VALUE  ELTDXBIP
00551         PERFORM 8500-CALL-CODES-MANUAL                            ELTDXBIP
00552         STRING CMF-DESCR-LINE(1) ' '                              ELTDXBIP
00553                CMF-DESCR-LINE(2) ' '                              ELTDXBIP
00554                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDXBIP
00555         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDXBIP
00556                                                                   ELTDXBIP
00557         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDXBIP
00558         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDXBIP
00559         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDXBIP
00560         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDXBIP
00561         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDXBIP
00562         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDXBIP
00563         IF WS-CIA  <  20                                          ELTDXBIP
00564            ADD +1  TO  WS-CIA                                     ELTDXBIP
00565            MOVE ZERO  TO                                          ELTDXBIP
00566                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXBIP
00567         ELSE                                                      ELTDXBIP
00568            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXBIP
00569                COMMAREA(DFHCOMMAREA)                              ELTDXBIP
00570            END-EXEC                                               ELTDXBIP
00571            MOVE +1  TO  WS-CIA                                    ELTDXBIP
00572            MOVE ZERO  TO                                          ELTDXBIP
00573                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDXBIP
00574                                                                   ELTDXBIP
00575  1690-PROBLEM-WITH-INDICES.                                       ELTDXBIP
00576                                                                   ELTDXBIP
00577      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXBIP
00578      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXBIP
00579                                                                   ELTDXBIP
00580      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBIP
00581      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBIP
00582                                                                   ELTDXBIP
00583      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBIP
00584      END-EXEC.                                                    ELTDXBIP
00585                                                                   ELTDXBIP
00586                                                                   ELTDXBIP
00587 /        P R O F E S S I O N A L   L A B   R T N E                ELTDXBIP
00588 ***************************************************************** ELTDXBIP
00589 *        P R O F E S S I O N A L   L A B   R T N E                ELTDXBIP
00590 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDXBIP
00591 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDXBIP
00592 ***************************************************************** ELTDXBIP
00593  2000-PROFESSIONAL-RTNE.                                          ELTDXBIP
00594      MOVE '2000'  TO  WS-PARA-ID.                                 ELTDXBIP
00595      MOVE WS-HDR-2-PROF TO  COF-HDR-LINE(2).                      ELTDXBIP
00596                                                                   ELTDXBIP
00597      MOVE WS-PROF-CNT TO PVN-NBR-BEN-PROVN,                       ELTDXBIP
00598                                 WS-PROF-COUNTER.                  ELTDXBIP
00599      PERFORM 7000-MOVE-IN-PROF                                    ELTDXBIP
00600         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXBIP
00601         UNTIL WS-SUB  >  WS-PROF-CNT.                             ELTDXBIP
00602                                                                   ELTDXBIP
00603      MOVE WS-INPATIENT-DX-ARE TO SSB-TOPIC-PHRASE.                ELTDXBIP
00604      PERFORM 8000-CALL-COVERAGE.                                  ELTDXBIP
00605      IF PVN-COVG-NONE                                             ELTDXBIP
00606         NEXT SENTENCE                                             ELTDXBIP
00607      ELSE                                                         ELTDXBIP
00608         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDXBIP
00609                                                                   ELTDXBIP
00610 /                                                                 ELTDXBIP
00611  2600-PROFESSIONAL-COMMON.                                        ELTDXBIP
00612      MOVE '2600'  TO WS-PARA-ID.                                  ELTDXBIP
00613                                                                   ELTDXBIP
00614      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDXBIP
00615                                                                   ELTDXBIP
00616      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXBIP
00617                    PSP-PROVN-PRICING-METHD,                       ELTDXBIP
00618                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBIP
00619                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBIP
00620                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDXBIP
00621                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDXBIP
00622                    PSE-BEN-SCOPE-ID.                              ELTDXBIP
00623                                                                   ELTDXBIP
00624      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXBIP
00625      END-EXEC.                                                    ELTDXBIP
00626                                                                   ELTDXBIP
00627      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXBIP
00628      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
00629          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDXBIP
00630                                                                   ELTDXBIP
00631      PERFORM 2630-FIND-FIRST-NONZERO                              ELTDXBIP
00632         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXBIP
00633         UNTIL WS-SUB  >  WS-PROF-COUNTER.                         ELTDXBIP
00634                                                                   ELTDXBIP
00635                                                                   ELTDXBIP
00636  2630-FIND-FIRST-NONZERO.                                         ELTDXBIP
00637      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDXBIP
00638      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTDXBIP
00639         NEXT SENTENCE                                             ELTDXBIP
00640      ELSE                                                         ELTDXBIP
00641         PERFORM 2640-BUILD-SCREEN-LINES.                          ELTDXBIP
00642                                                                   ELTDXBIP
00643  2640-BUILD-SCREEN-LINES.                                         ELTDXBIP
00644      MOVE '2640'  TO  WS-PARA-ID.                                 ELTDXBIP
00645                                                                   ELTDXBIP
00646      SET PLT-INDEX1 TO                                            ELTDXBIP
00647         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDXBIP
00648                                                                   ELTDXBIP
00649      IF WS-NOT-FIRST-TIME                                         ELTDXBIP
00650         MOVE 'P' TO COF-FUNCTION                                  ELTDXBIP
00651 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDXBIP
00652 ******** REB ===> 12/15/87.                                       ELTDXBIP
00653 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDXBIP
00654         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDXBIP
00655         EXEC CICS LINK                                            ELTDXBIP
00656                   PROGRAM('ELUOUTPT')                             ELTDXBIP
00657                   COMMAREA(DFHCOMMAREA)                           ELTDXBIP
00658         END-EXEC                                                  ELTDXBIP
00659      ELSE                                                         ELTDXBIP
00660        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDXBIP
00661                                                                   ELTDXBIP
00662      MOVE +1  TO  WS-CIA.                                         ELTDXBIP
00663      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXBIP
00664         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXBIP
00665            SET PLT-INDEX2  TO  2                                  ELTDXBIP
00666         ELSE                                                      ELTDXBIP
00667            PERFORM 2690-PROBLEM-WITH-INDICES                      ELTDXBIP
00668            PERFORM 3000-OUTPUT-TEXT                               ELTDXBIP
00669      ELSE                                                         ELTDXBIP
00670         SET PLT-INDEX2  TO  1.                                    ELTDXBIP
00671      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDXBIP
00672                                                                   ELTDXBIP
00673      ADD +1                 TO WS-CIA.                            ELTDXBIP
00674      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDXBIP
00675      ADD +1                 TO WS-CIA.                            ELTDXBIP
00676      PERFORM 2650-ZERO-ALL-WITH-SAME-NO                           ELTDXBIP
00677         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDXBIP
00678         UNTIL WS-SUB2  >  WS-PROF-COUNTER.                        ELTDXBIP
00679                                                                   ELTDXBIP
00680      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXBIP
00681               NOT = '0' AND NOT = LOW-VALUES                      ELTDXBIP
00682           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDXBIP
00683                                                                   ELTDXBIP
00684      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBIP
00685      MOVE WS-NO      TO  WS-DISPLAY-PAYMNT-BASED-TEXT.            ELTDXBIP
00686                                                                   ELTDXBIP
00687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00688       SET  PLT-INDEX2       TO  1                                 ELTDXBIP
00689       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBIP
00690        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBIP
00691                 '0000' AND NOT = '00  '                           ELTDXBIP
00692              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDXBIP
00693                                                                   ELTDXBIP
00694      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBIP
00695       SET PLT-INDEX2        TO 2                                  ELTDXBIP
00696       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBIP
00697        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBIP
00698                 '0000' AND NOT = '00  '                           ELTDXBIP
00699              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDXBIP
00700                                                                   ELTDXBIP
00701      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTDXBIP
00702          ADD  +1               TO  WS-CIA                         ELTDXBIP
00703          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTDXBIP
00704          ADD  +1               TO  WS-CIA.                        ELTDXBIP
00705                                                                   ELTDXBIP
00706      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00707       SET  PLT-INDEX2       TO  1                                 ELTDXBIP
00708       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBIP
00709        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBIP
00710                 '0000' AND NOT = '00  '                           ELTDXBIP
00711         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDXBIP
00712         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDXBIP
00713                               CMF-CODE-VALUE                      ELTDXBIP
00714         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDXBIP
00715                                      CMF-CODE-VALUE               ELTDXBIP
00716         IF WS-BASIC-LOB                                           ELTDXBIP
00717            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBIP
00718         END-IF                                                    ELTDXBIP
00719         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXBIP
00720         PERFORM 9100-DETERMINE-OUTPUT-METHOD.                     ELTDXBIP
00721         INITIALIZE TCAR-FROM-AREA.                                ELTDXBIP
00722                                                                   ELTDXBIP
00723      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBIP
00724       SET PLT-INDEX2        TO 2                                  ELTDXBIP
00725       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBIP
00726        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBIP
00727                 '0000' AND NOT = '00  '                           ELTDXBIP
00728         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDXBIP
00729         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDXBIP
00730                               CMF-CODE-VALUE                      ELTDXBIP
00731         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDXBIP
00732      IF WS-BASIC-LOB                                              ELTDXBIP
00733         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBIP
00734      END-IF                                                       ELTDXBIP
00735      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
00736      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
00737      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
00738                                                                   ELTDXBIP
00739 *    ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00740      ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00741      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDXBIP
00742      ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00743                                                                   ELTDXBIP
00744      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00745         SET  PLT-INDEX2           TO  1                           ELTDXBIP
00746         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDXBIP
00747                                                                   ELTDXBIP
00748      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBIP
00749         SET  PLT-INDEX2             TO  2                         ELTDXBIP
00750         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDXBIP
00751                                                                   ELTDXBIP
00752      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00753         IF WS-BASIC-LOB                                           ELTDXBIP
00754            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBIP
00755         END-IF                                                    ELTDXBIP
00756         SET  PLT-INDEX2           TO  1                           ELTDXBIP
00757         PERFORM 4500-TRANS-OTHER-RESP-IND                         ELTDXBIP
00758      ELSE                                                         ELTDXBIP
00759         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDXBIP
00760            IF WS-BASIC-LOB                                        ELTDXBIP
00761               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBIP
00762            END-IF                                                 ELTDXBIP
00763            SET  PLT-INDEX2             TO  2                      ELTDXBIP
00764           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDXBIP
00765                                                                   ELTDXBIP
00766      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBIP
00767         SET  PLT-INDEX2          TO  2                            ELTDXBIP
00768         PERFORM 4300-SPILLOVER-COINS                              ELTDXBIP
00769         PERFORM 4400-SPILLOVER-DEDUCT                             ELTDXBIP
00770         PERFORM 3000-OUTPUT-TEXT.                                 ELTDXBIP
00771                                                                   ELTDXBIP
00772      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXBIP
00773      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXBIP
00774                                                                   ELTDXBIP
00775  2650-ZERO-ALL-WITH-SAME-NO.                                      ELTDXBIP
00776      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDXBIP
00777                                                                   ELTDXBIP
00778      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDXBIP
00779         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXBIP
00780         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXBIP
00781         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXBIP
00782                                                   CMF-CODE-VALUE  ELTDXBIP
00783         PERFORM 8500-CALL-CODES-MANUAL                            ELTDXBIP
00784         STRING CMF-DESCR-LINE(1) ' '                              ELTDXBIP
00785                CMF-DESCR-LINE(2) ' '                              ELTDXBIP
00786                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDXBIP
00787         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDXBIP
00788                                                                   ELTDXBIP
00789         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDXBIP
00790         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDXBIP
00791         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDXBIP
00792         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDXBIP
00793         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDXBIP
00794         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDXBIP
00795         IF WS-CIA  <  20                                          ELTDXBIP
00796            ADD +1  TO  WS-CIA                                     ELTDXBIP
00797            MOVE ZERO  TO                                          ELTDXBIP
00798                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXBIP
00799         ELSE                                                      ELTDXBIP
00800            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXBIP
00801                COMMAREA(DFHCOMMAREA)                              ELTDXBIP
00802            END-EXEC                                               ELTDXBIP
00803            MOVE +1  TO  WS-CIA                                    ELTDXBIP
00804            MOVE ZERO  TO                                          ELTDXBIP
00805                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDXBIP
00806                                                                   ELTDXBIP
00807  2690-PROBLEM-WITH-INDICES.                                       ELTDXBIP
00808                                                                   ELTDXBIP
00809      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXBIP
00810      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXBIP
00811                                                                   ELTDXBIP
00812      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBIP
00813      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBIP
00814                                                                   ELTDXBIP
00815      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBIP
00816      END-EXEC.                                                    ELTDXBIP
00817                                                                   ELTDXBIP
00818  2699-EXIT.   EXIT.                                               ELTDXBIP
00819                                                                   ELTDXBIP
00820 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTDXBIP
00821  3000-OUTPUT-TEXT.                                                ELTDXBIP
00822      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBIP
00823      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDXBIP
00824      MOVE ' '  TO  COF-FUNCTION.                                  ELTDXBIP
00825                                                                   ELTDXBIP
00826      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDXBIP
00827                       COMMAREA(DFHCOMMAREA)                       ELTDXBIP
00828      END-EXEC.                                                    ELTDXBIP
00829      MOVE +1   TO WS-CIA.                                         ELTDXBIP
00830      MOVE +1   TO TCAR-FROM-SUB.                                  ELTDXBIP
00831      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
00832                                                                   ELTDXBIP
00833 /                                                                 ELTDXBIP
00834  4000-PLACE-OF-TREATMENT.                                         ELTDXBIP
00835      SET WS-PROCESS-POT TO TRUE.                                  ELTDXBIP
00836      ADD +1     TO  WS-CIA.                                       ELTDXBIP
00837      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXBIP
00838      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTDXBIP
00839      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXBIP
00840                                               TO  CMF-CODE-VALUE. ELTDXBIP
00841      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXBIP
00842      ADD  +1                   TO  WS-CIA.                        ELTDXBIP
00843      MOVE WS-SERVICES-RENDERED                                    ELTDXBIP
00844           TO COF-DTL-LINE(WS-CIA).                                ELTDXBIP
00845      ADD 1 TO WS-CIA.                                             ELTDXBIP
00846      PERFORM 8500-CALL-CODES-MANUAL.                              ELTDXBIP
00847      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
00848      INITIALIZE WS-POT-SWITCH                                     ELTDXBIP
00849                 TCAR-FROM-AREA.                                   ELTDXBIP
00850      MOVE 1 TO WS-CIA.                                            ELTDXBIP
00851                                                                   ELTDXBIP
00852 /                                                                 ELTDXBIP
00853  4100-PAYABLE-AS-BASIC.                                           ELTDXBIP
00854      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDXBIP
00855          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDXBIP
00856          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTDXBIP
00857      ELSE                                                         ELTDXBIP
00858         MOVE 'BP'                  TO  CMF-RECORD-PREFIX          ELTDXBIP
00859         MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBIP
00860         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXBIP
00861                                                CMF-CODE-VALUE     ELTDXBIP
00862      IF WS-BASIC-LOB                                              ELTDXBIP
00863         SET PROCESSING-BASIC-INFO TO TRUE                         ELTDXBIP
00864      END-IF.                                                      ELTDXBIP
00865      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
00866      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
00867      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
00868                                                                   ELTDXBIP
00869  4100-OUTPUT-TEXT.                                                ELTDXBIP
00870        PERFORM 3000-OUTPUT-TEXT.                                  ELTDXBIP
00871                                                                   ELTDXBIP
00872 /                                                                 ELTDXBIP
00873  4200-PAYABLE-AS-SUPP.                                            ELTDXBIP
00874      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDXBIP
00875          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDXBIP
00876          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTDXBIP
00877      ELSE                                                         ELTDXBIP
00878         MOVE 'BP'                  TO  CMF-RECORD-PREFIX          ELTDXBIP
00879         MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBIP
00880         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXBIP
00881                                                CMF-CODE-VALUE     ELTDXBIP
00882      END-IF.                                                      ELTDXBIP
00883      IF WS-BASIC-LOB                                              ELTDXBIP
00884         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBIP
00885      END-IF.                                                      ELTDXBIP
00886      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
00887      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
00888      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
00889                                                                   ELTDXBIP
00890  4200-OUTPUT-TEXT.                                                ELTDXBIP
00891      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBIP
00892 /                                                                 ELTDXBIP
00893  4250-PROF-CHGR-HSP-CLM.                                          ELTDXBIP
00894      MOVE 'N'        TO  WS-ADD-A-BLANK-IND.                      ELTDXBIP
00895      SET PLT-INDEX2  TO  1.                                       ELTDXBIP
00896      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXBIP
00897      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXBIP
00898      SET PLT-INDEX2 TO 1.                                         ELTDXBIP
00899                                                                   ELTDXBIP
00900      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTDXBIP
00901          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDXBIP
00902              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
00903                          NOT  =  '0'  AND  NOT  =  LOW-VALUES     ELTDXBIP
00904 *                ADD  +1         TO  WS-CIA                       ELTDXBIP
00905                  MOVE +1         TO  WS-CIA                       ELTDXBIP
00906                  MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)        ELTDXBIP
00907                  MOVE WS-PROF-INPT-CHRGES                         ELTDXBIP
00908                                  TO  COF-DTL-LINE (WS-CIA)        ELTDXBIP
00909                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND           ELTDXBIP
00910                  ADD  +1         TO  WS-CIA.                      ELTDXBIP
00911                                                                   ELTDXBIP
00912      SET PLT-INDEX2  TO  2.                                       ELTDXBIP
00913                                                                   ELTDXBIP
00914      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTDXBIP
00915          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDXBIP
00916                       AND                                         ELTDXBIP
00917             NOT  WS-ADD-A-BLANK-LINE                              ELTDXBIP
00918              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
00919                          NOT  =  '0'  AND  NOT  =  LOW-VALUES     ELTDXBIP
00920 *                ADD  +1         TO  WS-CIA                       ELTDXBIP
00921                  MOVE  +1         TO  WS-CIA                      ELTDXBIP
00922                  MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)        ELTDXBIP
00923                  ADD  +1         TO  WS-CIA                       ELTDXBIP
00924                  MOVE WS-PROF-INPT-CHRGES                         ELTDXBIP
00925                                  TO  COF-DTL-LINE (WS-CIA)        ELTDXBIP
00926                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND.          ELTDXBIP
00927                                                                   ELTDXBIP
00928      SET PLT-INDEX2  TO  1.                                       ELTDXBIP
00929                                                                   ELTDXBIP
00930      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  ZERO       ELTDXBIP
00931          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDXBIP
00932              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
00933                      NOT  =  '0'  AND  NOT  =  LOW-VALUES         ELTDXBIP
00934                  IF WS-BASIC-LOB                                  ELTDXBIP
00935                     SET PROCESSING-BASIC-INFO TO TRUE             ELTDXBIP
00936                  END-IF                                           ELTDXBIP
00937                 MOVE 'BPB'         TO  CMF-RECORD-PREFIX          ELTDXBIP
00938                 MOVE 'PROF-CHRG-HSP-CLM'                          ELTDXBIP
00939                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBIP
00940                 MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)ELTDXBIP
00941                                    TO  CMF-CODE-VALUE             ELTDXBIP
00942 *               PERFORMM4251-TRANSLATE                            ELTDXBIP
00943                 PERFORM 9000-CALL-CODES-MANUAL                    ELTDXBIP
00944                 PERFORM 9100-DETERMINE-OUTPUT-METHOD.             ELTDXBIP
00945 *    INITIALIZE WS-LOB-SWITCH                                     ELTDXBIP
00946      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXBIP
00947                                                                   ELTDXBIP
00948      SET PLT-INDEX2  TO  2.                                       ELTDXBIP
00949                                                                   ELTDXBIP
00950      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  ZERO        ELTDXBIP
00951          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDXBIP
00952              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
00953                      NOT  =  '0'  AND  NOT  =  LOW-VALUES         ELTDXBIP
00954                 MOVE 'BPB'         TO  CMF-RECORD-PREFIX          ELTDXBIP
00955                 MOVE 'PROF-CHRG-HSP-CLM'                          ELTDXBIP
00956                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBIP
00957                 MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)ELTDXBIP
00958                                    TO  CMF-CODE-VALUE             ELTDXBIP
00959                  IF WS-BASIC-LOB                                  ELTDXBIP
00960                     SET PROCESSING-SUPPLEMENTAL TO TRUE           ELTDXBIP
00961                  END-IF                                           ELTDXBIP
00962                 PERFORM 9000-CALL-CODES-MANUAL                    ELTDXBIP
00963                 PERFORM 9100-DETERMINE-OUTPUT-METHOD.             ELTDXBIP
00964 *    INITIALIZE WS-LOB-SWITCH                                     ELTDXBIP
00965      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXBIP
00966 *              PERFORM 4251-TRANSLATE.                            ELTDXBIP
00967                                                                   ELTDXBIP
00968 /                                                                 ELTDXBIP
00969  4300-SPILLOVER-COINS.                                            ELTDXBIP
00970      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBIP
00971         AND                                                       ELTDXBIP
00972         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXBIP
00973            NOT = '0'                                              ELTDXBIP
00974         MOVE 2 TO WS-CIA                                          ELTDXBIP
00975         MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)         ELTDXBIP
00976         PERFORM 4301-SPILLOVER-CONTD                              ELTDXBIP
00977      ELSE                                                         ELTDXBIP
00978         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO   ELTDXBIP
00979            AND                                                    ELTDXBIP
00980            PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTDXBIP
00981               NOT = '0'                                           ELTDXBIP
00982         MOVE 2 TO WS-CIA                                          ELTDXBIP
00983         MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)         ELTDXBIP
00984         PERFORM 4302-SPILLOVER-CONTD                              ELTDXBIP
00985      END-IF.                                                      ELTDXBIP
00986                                                                   ELTDXBIP
00987  4301-SPILLOVER-CONTD.                                            ELTDXBIP
00988      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBIP
00989      ADD +1     TO  WS-CIA.                                       ELTDXBIP
00990      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXBIP
00991      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDXBIP
00992      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
00993                       TO CMF-CODE-VALUE.                          ELTDXBIP
00994      IF WS-BASIC-LOB                                              ELTDXBIP
00995         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBIP
00996      END-IF.                                                      ELTDXBIP
00997      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
00998      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
00999      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
01000      MOVE 1 TO WS-CIA.                                            ELTDXBIP
01001                                                                   ELTDXBIP
01002  4302-SPILLOVER-CONTD.                                            ELTDXBIP
01003      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBIP
01004      ADD +1     TO  WS-CIA.                                       ELTDXBIP
01005      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXBIP
01006      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDXBIP
01007      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDXBIP
01008                       TO CMF-CODE-VALUE.                          ELTDXBIP
01009      IF WS-BASIC-LOB                                              ELTDXBIP
01010         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBIP
01011      END-IF.                                                      ELTDXBIP
01012      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
01013      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
01014      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
01015      MOVE 1 TO WS-CIA.                                            ELTDXBIP
01016                                                                   ELTDXBIP
01017 /                                                                 ELTDXBIP
01018  4400-SPILLOVER-DEDUCT.                                           ELTDXBIP
01019      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXBIP
01020            = '0'   OR LOW-VALUES                                  ELTDXBIP
01021         NEXT SENTENCE                                             ELTDXBIP
01022      ELSE                                                         ELTDXBIP
01023         PERFORM 4401-SPILLOVER-DEDUCT-CONTD.                      ELTDXBIP
01024                                                                   ELTDXBIP
01025  4401-SPILLOVER-DEDUCT-CONTD.                                     ELTDXBIP
01026 *    ADD +1     TO  WS-CIA.                                       ELTDXBIP
01027      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBIP
01028      MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA).           ELTDXBIP
01029      ADD  +1                   TO  WS-CIA.                        ELTDXBIP
01030      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXBIP
01031      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXBIP
01032      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXBIP
01033                       TO CMF-CODE-VALUE.                          ELTDXBIP
01034      IF WS-BASIC-LOB                                              ELTDXBIP
01035         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBIP
01036      END-IF.                                                      ELTDXBIP
01037      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBIP
01038      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
01039      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
01040                                                                   ELTDXBIP
01041 /                                                                 ELTDXBIP
01042  4500-TRANS-OTHER-RESP-IND.                                       ELTDXBIP
01043      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTDXBIP
01044            = ZEROS OR LOW-VALUES                                  ELTDXBIP
01045          CONTINUE                                                 ELTDXBIP
01046      ELSE                                                         ELTDXBIP
01047         PERFORM 4501-TRANS-OTHER-RESP-CONTD.                      ELTDXBIP
01048                                                                   ELTDXBIP
01049  4501-TRANS-OTHER-RESP-CONTD.                                     ELTDXBIP
01050      SET WS-PROCESS-POT TO TRUE.                                  ELTDXBIP
01051 *    ADD +1     TO  WS-CIA.                                       ELTDXBIP
01052      ADD +1     TO  WS-CIA.                                       ELTDXBIP
01053      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXBIP
01054      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXBIP
01055      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDXBIP
01056                       TO CMF-CODE-VALUE                           ELTDXBIP
01057      PERFORM 8500-CALL-CODES-MANUAL                               ELTDXBIP
01058      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBIP
01059      INITIALIZE WS-POT-SWITCH.                                    ELTDXBIP
01060 /                                                                 ELTDXBIP
01061  4675-PAY-CONSID-TEXT.                                            ELTDXBIP
01062      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBIP
01063      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDXBIP
01064             WS-PAY-CONSDR-TEXT2                                   ELTDXBIP
01065                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDXBIP
01066      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXBIP
01067      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDXBIP
01068      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDXBIP
01069                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDXBIP
01070      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXBIP
01071      IF WS-CIA > 17                                               ELTDXBIP
01072            PERFORM 3000-OUTPUT-TEXT                               ELTDXBIP
01073            MOVE +1            TO WS-CIA.                          ELTDXBIP
01074      ADD +1                TO  WS-CIA.                            ELTDXBIP
01075      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDXBIP
01076      ADD +1                TO  WS-CIA.                            ELTDXBIP
01077      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDXBIP
01078      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBIP
01079                                                                   ELTDXBIP
01080  4900-BEN-TAB-PVE.                                                ELTDXBIP
01081      MOVE +1 TO WS-CIA.                                           ELTDXBIP
01082      MOVE SPACES TO COF-DTL-LINE(WS-CIA).                         ELTDXBIP
01083      ADD  +1         TO WS-CIA.                                   ELTDXBIP
01084      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDXBIP
01085      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBIP
01086 /                                                                 ELTDXBIP
01087  6000-MOVE-IN-INST.                                               ELTDXBIP
01088      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXBIP
01089      MOVE WS-INST-LIST(WS-SUB)  TO                                ELTDXBIP
01090                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXBIP
01091      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDXBIP
01092                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDXBIP
01093                                                                   ELTDXBIP
01094 /                                                                 ELTDXBIP
01095  7000-MOVE-IN-PROF.                                               ELTDXBIP
01096      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXBIP
01097      MOVE WS-PROF-LIST(WS-SUB)  TO                                ELTDXBIP
01098                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXBIP
01099      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDXBIP
01100                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDXBIP
01101                                                                   ELTDXBIP
01102 /                                                                 ELTDXBIP
01103  8000-CALL-COVERAGE.                                              ELTDXBIP
01104      MOVE '8000'            TO  WS-PARA-ID.                       ELTDXBIP
01105 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDXBIP
01106      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBIP
01107      MOVE WS-YES TO WS-FIRSTTIME-IND.                             ELTDXBIP
01108      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDXBIP
01109      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDXBIP
01110                                                                   ELTDXBIP
01111      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBIP
01112      END-EXEC.                                                    ELTDXBIP
01113                                                                   ELTDXBIP
01114      PERFORM 8300-DISPLAY-COVERAGE.                               ELTDXBIP
01115                                                                   ELTDXBIP
01116 *                                                                 ELTDXBIP
01117  8300-DISPLAY-COVERAGE.                                           ELTDXBIP
01118 ********************************************************          ELTDXBIP
01119 ***** REARRANGING THIS PARAGRAPH SO THAT YOU ARE NOT ***          ELTDXBIP
01120 ***** FORCED TO DO A LINK FOR A NEW PAGE WHEN YOU    ***          ELTDXBIP
01121 ***** DO NOT NEED ONE.     12/11/87 ==> REB.         ***          ELTDXBIP
01122 ********************************************************          ELTDXBIP
01123      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDXBIP
01124      END-EXEC.                                                    ELTDXBIP
01125                                                                   ELTDXBIP
01126 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDXBIP
01127      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBIP
01128      MOVE ' '  TO  COF-FUNCTION.                                  ELTDXBIP
01129      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBIP
01130      END-EXEC.                                                    ELTDXBIP
01131 *4/15 END OF TEMPORARY CODE                                       ELTDXBIP
01132                                                                   ELTDXBIP
01133 /  C O D E S   M A N U A L   C A L L                              ELTDXBIP
01134  8500-CALL-CODES-MANUAL.                                          ELTDXBIP
01135      INITIALIZE CMF-RETURN-CODE.                                  ELTDXBIP
01136      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXBIP
01137               COMMAREA(DFHCOMMAREA)                               ELTDXBIP
01138      END-EXEC.                                                    ELTDXBIP
01139                                                                   ELTDXBIP
01140      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXBIP
01141      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01142          ADDRESS OF CMF-DESCR.                                    ELTDXBIP
01143                                                                   ELTDXBIP
01144  9000-CALL-CODES-MANUAL.                                          ELTDXBIP
01145      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXBIP
01146                       COMMAREA(DFHCOMMAREA)                       ELTDXBIP
01147      END-EXEC.                                                    ELTDXBIP
01148      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXBIP
01149      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01150                      ADDRESS OF CMF-DESCR.                        ELTDXBIP
01151                                                                   ELTDXBIP
01152  9100-DETERMINE-OUTPUT-METHOD.                                    ELTDXBIP
01153      MOVE CMF-DESCR-LINE(1) TO WS-TEST-LINE.                      ELTDXBIP
01154      IF WS-TEST-CHAR = WS-SINGLE-QUOTE                            ELTDXBIP
01155         MOVE SPACE TO WS-TEST-CHAR                                ELTDXBIP
01156         MOVE WS-TEST-DATA TO CMF-DESCR-LINE(1)                    ELTDXBIP
01157         PERFORM 9200-DIRECT-OUTPUT                                ELTDXBIP
01158      ELSE                                                         ELTDXBIP
01159         PERFORM 9300-DISPLAY-CODE-VALUES                          ELTDXBIP
01160      END-IF.                                                      ELTDXBIP
01161                                                                   ELTDXBIP
01162  9200-DIRECT-OUTPUT.                                              ELTDXBIP
01163      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXBIP
01164        EVALUATE TRUE                                              ELTDXBIP
01165            WHEN PROCESSING-BASIC-INFO                             ELTDXBIP
01166               IF WS-PROCESS-POT                                   ELTDXBIP
01167                  CONTINUE                                         ELTDXBIP
01168               ELSE                                                ELTDXBIP
01169                  MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)            ELTDXBIP
01170                  ADD 1 TO WS-CIA                                  ELTDXBIP
01171               END-IF                                              ELTDXBIP
01172            WHEN PROCESSING-SUPPLEMENTAL                           ELTDXBIP
01173               IF WS-PROCESS-POT                                   ELTDXBIP
01174                  CONTINUE                                         ELTDXBIP
01175               ELSE                                                ELTDXBIP
01176                  MOVE WS-SUPPLEMENTAL-LIT                         ELTDXBIP
01177                      TO COF-DTL-LINE(WS-CIA)                      ELTDXBIP
01178                  ADD 1 TO WS-CIA                                  ELTDXBIP
01179               END-IF                                              ELTDXBIP
01180            WHEN OTHER                                             ELTDXBIP
01181               CONTINUE                                            ELTDXBIP
01182        END-EVALUATE.                                              ELTDXBIP
01183      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXBIP
01184         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXBIP
01185        MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                      ELTDXBIP
01186           COF-DTL-LINE(WS-CIA)                                    ELTDXBIP
01187        ADD 1 TO WS-CIA                                            ELTDXBIP
01188      END-PERFORM.                                                 ELTDXBIP
01189      IF WS-PROCESS-POT OR NOT PROCESSING-BASIC-INFO               ELTDXBIP
01190         ADD 1 TO WS-CIA                                           ELTDXBIP
01191         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXBIP
01192      END-IF.                                                      ELTDXBIP
01193      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXBIP
01194      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXBIP
01195                            DFHCOMMAREA.                           ELTDXBIP
01196      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXBIP
01197                WS-CIA                                             ELTDXBIP
01198                TCAR-FROM-SUB.                                     ELTDXBIP
01199                                                                   ELTDXBIP
01200  9300-DISPLAY-CODE-VALUES.                                        ELTDXBIP
01201      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXBIP
01202         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXBIP
01203         MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                     ELTDXBIP
01204            TCAR-FROM-LINE(TCAR-FROM-SUB)                          ELTDXBIP
01205         ADD 1 TO TCAR-FROM-SUB                                    ELTDXBIP
01206      END-PERFORM.                                                 ELTDXBIP
01207      COMPUTE TCAR-FROM-LENGTH = TCAR-FROM-SUB * 79.               ELTDXBIP
01208      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXBIP
01209      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXBIP
01210      PERFORM 9610-UNSTRING-TEXT.                                  ELTDXBIP
01211      PERFORM UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED        ELTDXBIP
01212 *          ADD 1 TO WS-CIA                                        ELTDXBIP
01213            IF TCAR-FROM-SUB = 1 OR                                ELTDXBIP
01214               (WS-BASIC-SUPP-SWITCH  NOT = SPACES)                ELTDXBIP
01215               EVALUATE TRUE                                       ELTDXBIP
01216                 WHEN PROCESSING-BASIC-INFO                        ELTDXBIP
01217                   IF WS-PROCESS-POT                               ELTDXBIP
01218                     CONTINUE                                      ELTDXBIP
01219                   ELSE                                            ELTDXBIP
01220                      MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)        ELTDXBIP
01221                      ADD +1 TO WS-CIA                             ELTDXBIP
01222                   END-IF                                          ELTDXBIP
01223                  WHEN PROCESSING-SUPPLEMENTAL                     ELTDXBIP
01224                   IF WS-PROCESS-POT                               ELTDXBIP
01225                      CONTINUE                                     ELTDXBIP
01226                   ELSE                                            ELTDXBIP
01227                      MOVE WS-SUPPLEMENTAL-LIT                     ELTDXBIP
01228                         TO COF-DTL-LINE(WS-CIA)                   ELTDXBIP
01229                      ADD +1 TO WS-CIA                             ELTDXBIP
01230                   END-IF                                          ELTDXBIP
01231                  WHEN OTHER                                       ELTDXBIP
01232                      CONTINUE                                     ELTDXBIP
01233               END-EVALUATE                                        ELTDXBIP
01234 *             ADD 1 TO WS-CIA                                     ELTDXBIP
01235               MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)       ELTDXBIP
01236            ELSE                                                   ELTDXBIP
01237               ADD 1 TO WS-CIA                                     ELTDXBIP
01238               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                ELTDXBIP
01239                   COF-DTL-LINE(WS-CIA)                            ELTDXBIP
01240            END-IF                                                 ELTDXBIP
01241            ADD 1 TO TCAR-FROM-SUB                                 ELTDXBIP
01242      END-PERFORM.                                                 ELTDXBIP
01243      IF WS-PROCESS-POT OR NOT PROCESSING-BASIC-INFO               ELTDXBIP
01244         ADD 1 TO WS-CIA                                           ELTDXBIP
01245         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXBIP
01246      END-IF.                                                      ELTDXBIP
01247      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXBIP
01248      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXBIP
01249                            DFHCOMMAREA.                           ELTDXBIP
01250      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXBIP
01251                WS-CIA                                             ELTDXBIP
01252                TCAR-FROM-SUB.                                     ELTDXBIP
01253                                                                   ELTDXBIP
01254  9610-UNSTRING-TEXT.                                              ELTDXBIP
01255      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDXBIP
01256      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDXBIP
01257      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDXBIP
01258      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTDXBIP
01259      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTDXBIP
01260      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTDXBIP
01261      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTDXBIP
01262      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTDXBIP
01263      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTDXBIP
01264      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTDXBIP
01265      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTDXBIP
01266      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTDXBIP
01267      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTDXBIP
01268      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTDXBIP
01269      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTDXBIP
01270      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTDXBIP
01271      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTDXBIP
01272      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTDXBIP
01273      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTDXBIP
01274      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTDXBIP
01275      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTDXBIP
01276      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXBIP
01277                                                                   ELTDXBIP
01278 / C O M P R E S S   A N D   E X P A N D   S U B R O U T I N E S   ELTDXBIP
01279  9999-DUMMEY.                                                     ELTDXBIP
01280      COPY ELSTCOMP.                                               ELTDXBIP
01281                                                                   ELTDXBIP
01282  9999-CHECK-CONTRACT.                                             ELTDXBIP
01283      INITIALIZE WS-LOB-SWITCH.                                    ELTDXBIP
01284      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDXBIP
01285      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01286                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBIP
01287      IF CIA-RC-PTR-NULL                                           ELTDXBIP
01288         CONTINUE                                                  ELTDXBIP
01289      ELSE                                                         ELTDXBIP
01290         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBIP
01291         IF WS-BSC-LINE                                            ELTDXBIP
01292            SET WS-BASIC-LOB TO TRUE                               ELTDXBIP
01293         END-IF                                                    ELTDXBIP
01294      END-IF.                                                      ELTDXBIP
01295      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDXBIP
01296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01297                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBIP
01298      IF CIA-RC-PTR-NULL                                           ELTDXBIP
01299         CONTINUE                                                  ELTDXBIP
01300      ELSE                                                         ELTDXBIP
01301         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBIP
01302         IF WS-BSC-LINE                                            ELTDXBIP
01303            SET WS-BASIC-LOB TO TRUE                               ELTDXBIP
01304         END-IF                                                    ELTDXBIP
01305      END-IF.                                                      ELTDXBIP
01306      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTDXBIP
01307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01308                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBIP
01309      IF CIA-RC-PTR-NULL                                           ELTDXBIP
01310         CONTINUE                                                  ELTDXBIP
01311      ELSE                                                         ELTDXBIP
01312         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBIP
01313         IF WS-BSC-LINE                                            ELTDXBIP
01314            SET WS-BASIC-LOB TO TRUE                               ELTDXBIP
01315         END-IF                                                    ELTDXBIP
01316      END-IF.                                                      ELTDXBIP
01317      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTDXBIP
01318      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBIP
01319                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBIP
01320      IF CIA-RC-PTR-NULL                                           ELTDXBIP
01321         CONTINUE                                                  ELTDXBIP
01322      ELSE                                                         ELTDXBIP
01323         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBIP
01324         IF WS-BSC-LINE                                            ELTDXBIP
01325            SET WS-BASIC-LOB TO TRUE                               ELTDXBIP
01326         END-IF                                                    ELTDXBIP
01327      END-IF.                                                      ELTDXBIP
01328 /   C O M P R E S S I O N  A N D  U N S T R I N G   R O U T I N E ELTDXBIP
01329  8600-ELSTCOMP.                                                   ELTDXBIP
01330 ****                                                              ELTDXBIP
01331 **** 8600-ELSTCOMP SECTION REQUIRED TO END PREV SECTION           ELTDXBIP
01332 ****                                                              ELTDXBIP
01333 *COPY ELSTCOMP.                                                   ELTDXBIP
01334 /              A B E N D                                          ELTDXBIP
01335 ******************************************************************ELTDXBIP
01336 *                        A B E N D                                ELTDXBIP
01337 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTDXBIP
01338 *                                                                 ELTDXBIP
01339 ******************************************************************ELTDXBIP
01340  9998-INVALID-PTR.                                                ELTDXBIP
01341      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTDXBIP
01342      EXEC CICS ABEND   ABCODE(CIA-ABCODE) END-EXEC.               ELTDXBIP
01343                                                                   ELTDXBIP
01344                                                                   ELTDXBIP
01345      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTDXBIP
01346                                                                   ELTDXBIP
01347      MOVE +79             TO TCAR-OUTPUT-FIELD-20-LEN.            ELTDXBIP
01348      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXBIP
01349                                                                   ELTDXBIP
