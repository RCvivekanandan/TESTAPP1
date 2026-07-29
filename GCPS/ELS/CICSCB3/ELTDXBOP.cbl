00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTDXBOP.                                            ELTDXBOP
00003  AUTHOR. ANNE KEFFER KING.                                           LV001
00004  DATE-WRITTEN.   09/26/95.                                        ELTDXBOP
00005  DATE-COMPILED.                                                   ELTDXBOP
00006      SKIP3                                                        ELTDXBOP
00007 ******************************************************************ELTDXBOP
00008 *@>ELTDIAGS                                                       ELTDXBOP
00009 *@¬                                                               ELTDXBOP
00010 *                        PROGRAM ABSTRACT                         ELTDXBOP
00011 *                                                                 ELTDXBOP
00012 *@¬ PROGRAM NAME:   E.L.S. BASIC OUTPATIENT DIAGNOSTIC SERVICES   ELTDXBOP
00013 *@¬                                                               ELTDXBOP
00014 *@¬ PROGRAM I.D.:   ELTDXBOP.                                     ELTDXBOP
00015 *@¬                                                               ELTDXBOP
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDXBOP
00017 *@¬            DIAGNOSTIC --BASIC OUTPATIENT SERVICES.            ELTDXBOP
00018 *@¬                                                               ELTDXBOP
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DIAGNOSTIC        ELTDXBOP
00020 *@¬            PRROCEDURES IS AFFORD A MEMBER BY HIS GROUP.       ELTDXBOP
00021 *@¬            THIS INFORMATION IS                                ELTDXBOP
00022 *@¬            GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTDXBOP
00023 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTDXBOP
00024 *@¬            RANGE OF DATES.                                    ELTDXBOP
00025 *@¬                                                               ELTDXBOP
00026 *@¬ RECORDS                                                       ELTDXBOP
00027 *@¬ ACCESSED:  VARIOUS BENEFIT PROVISION, AND A                   ELTDXBOP
00028 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTDXBOP
00029 *@¬                                                               ELTDXBOP
00030 *@¬ PROCESSING                                                    ELTDXBOP
00031 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTDXBOP
00032 *@¬                                                               ELTDXBOP
00033 *@¬                                                               ELTDXBOP
00034 ***************************************************************** ELTDXBOP
00035      SKIP3                                                        ELTDXBOP
00036 ***************************************************************** ELTDXBOP
00037 *                    U P D A T E   H I S T O R Y                * ELTDXBOP
00038 *                                                               * ELTDXBOP
00039 *   DATE    PGM  DESCRIPTION  (MOST CURRENT AT TOP)             * ELTDXBOP
00040 * --------  ---  ---------------------------------------------- * ELTDXBOP
00041 *                                                               * ELTDXBOP
00042 * 09/26/95  AKK  CREATED.  CLONED FROM ELTDIAGS.                * ELTDXBOP
00043 *                                                               * ELTDXBOP
00044 * 12/28/95  AKK  CHANGED DUE TO CUSTOMER REQUEST FOR CHANGE    *  ELTDXBOP
00045 *                IN OUTPUT METHOD.                              * ELTDXBOP
00046 ***************************************************************** ELTDXBOP
00047 *                                                               * ELTDXBOP
00048 ***************************************************************** ELTDXBOP
00049 /                                                                 ELTDXBOP
00050  ENVIRONMENT DIVISION.                                            ELTDXBOP
00051      SKIP3                                                        ELTDXBOP
00052  DATA DIVISION.                                                   ELTDXBOP
00053  WORKING-STORAGE SECTION.                                         ELTDXBOP
00054  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDXBOP
00055      '***ELTDXIP WS BEGINS***'.                                   ELTDXBOP
00056  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTDXBOP
00057                                                                   ELTDXBOP
00058  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTDXBOP
00059                                                                   ELTDXBOP
00060 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDXBOP
00061  01  WS-WORK-FIELDS.                                              ELTDXBOP
00062      05  WS-CHAR-0                     PIC X.                     ELTDXBOP
00063      05  WS-HOLD1                      PIC X(10).                 ELTDXBOP
00064      05  WS-HOLD2                      PIC X(10).                 ELTDXBOP
00065      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTDXBOP
00066      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTDXBOP
00067      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTDXBOP
00068      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTDXBOP
00069      05  WS-DTL-PP                     PIC X(50).                 ELTDXBOP
00070      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTDXBOP
00071      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTDXBOP
00072      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTDXBOP
00073      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTDXBOP
00074      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTDXBOP
00075      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTDXBOP
00076      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDXBOP
00077      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTDXBOP
00078      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTDXBOP
00079      05  WS-FIRSTTIME-IND              PIC X.                     ELTDXBOP
00080          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTDXBOP
00081      05  WS-ADD-A-BLANK-IND            PIC  X(01) VALUE 'N'.      ELTDXBOP
00082          88  WS-ADD-A-BLANK-LINE                  VALUE 'Y'.      ELTDXBOP
00083                                                                   ELTDXBOP
00084      05  WS-POT-SWITCH                 PIC  X(01) VALUE SPACE.    ELTDXBOP
00085          88  WS-PROCESS-POT                       VALUE 'P'.      ELTDXBOP
00086                                                                   ELTDXBOP
00087      05 WS-SINGLE-QUOTE                PIC X  VALUE ''''.         ELTDXBOP
00088      05 WS-BASIC-LINE                  PIC X.                     ELTDXBOP
00089         88  WS-BSC-LINE                       VALUE '1' '2' '3'.  ELTDXBOP
00090                                                                   ELTDXBOP
00091      05  WS-TEST-LINE.                                            ELTDXBOP
00092          10  WS-TEST-CHAR              PIC X.                     ELTDXBOP
00093          10  WS-TEST-DATA              PIC X(78).                 ELTDXBOP
00094                                                                   ELTDXBOP
00095      05  WS-LOB-SWITCH                 PIC X   VALUE SPACE.       ELTDXBOP
00096          88  WS-BASIC-LOB                      VALUE 'B'.         ELTDXBOP
00097          88  WS-NOT-BASIC-LOB                  VALUE SPACE.       ELTDXBOP
00098                                                                   ELTDXBOP
00099      05  WS-BASIC-SUPP-SWITCH          PIC X   VALUE SPACE.       ELTDXBOP
00100          88  PROCESSING-BASIC-INFO             VALUE 'I'.         ELTDXBOP
00101          88  PROCESSING-SUPPLEMENTAL           VALUE 'P'.         ELTDXBOP
00102                                                                   ELTDXBOP
00103      05  WS-PROCESSING-SWITCH          PIC X   VALUE SPACE.       ELTDXBOP
00104          88  PROCESSING-INSTITUTIONAL          VALUE 'I'.         ELTDXBOP
00105          88  PROCESSING-PROFESSIONAL           VALUE 'P'.         ELTDXBOP
00106                                                                   ELTDXBOP
00107 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTDXBOP
00108  01  WS-BEN-PROV-ID.                                              ELTDXBOP
00109      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +13.ELTDXBOP
00110      05  WS-INST-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDXBOP
00111      05  WS-PROF-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDXBOP
00112                                                                   ELTDXBOP
00113      05  WS-INST-CNT                   PIC S9(4) COMP  VALUE +13. ELTDXBOP
00114      05  WS-INST-TAB.                                             ELTDXBOP
00115        10  FILLER                      PIC X(6)  VALUE 'ATO  B'.  ELTDXBOP
00116        10  FILLER                      PIC X(6)  VALUE 'DMPO B'.  ELTDXBOP
00117        10  FILLER                      PIC X(6)  VALUE 'HTEO B'.  ELTDXBOP
00118        10  FILLER                      PIC X(6)  VALUE 'LABO B'.  ELTDXBOP
00119        10  FILLER                      PIC X(6)  VALUE 'PAPO B'.  ELTDXBOP
00120        10  FILLER                      PIC X(6)  VALUE 'PAT  B'.  ELTDXBOP
00121        10  FILLER                      PIC X(6)  VALUE 'PPFO B'.  ELTDXBOP
00122        10  FILLER                      PIC X(6)  VALUE 'PSO  B'.  ELTDXBOP
00123        10  FILLER                      PIC X(6)  VALUE 'RCXO B'.  ELTDXBOP
00124        10  FILLER                      PIC X(6)  VALUE 'RIO  B'.  ELTDXBOP
00125        10  FILLER                      PIC X(6)  VALUE 'RPFO B'.  ELTDXBOP
00126        10  FILLER                      PIC X(6)  VALUE 'VTO  B'.  ELTDXBOP
00127        10  FILLER                      PIC X(6)  VALUE 'XRYO B'.  ELTDXBOP
00128      05  WS-INST-LIST     REDEFINES    WS-INST-TAB                ELTDXBOP
00129                                        PIC X(6)  OCCURS 13 TIMES. ELTDXBOP
00130                                                                   ELTDXBOP
00131      05  WS-PROF-CNT                   PIC S9(4) COMP  VALUE +13. ELTDXBOP
00132      05  WS-PROF-TAB.                                             ELTDXBOP
00133        10  FILLER                      PIC X(6)  VALUE 'ATO  E'.  ELTDXBOP
00134        10  FILLER                      PIC X(6)  VALUE 'DMPO E'.  ELTDXBOP
00135        10  FILLER                      PIC X(6)  VALUE 'HTEO E'.  ELTDXBOP
00136        10  FILLER                      PIC X(6)  VALUE 'LABO E'.  ELTDXBOP
00137        10  FILLER                      PIC X(6)  VALUE 'PAPO E'.  ELTDXBOP
00138        10  FILLER                      PIC X(6)  VALUE 'PAT  E'.  ELTDXBOP
00139        10  FILLER                      PIC X(6)  VALUE 'PREH E'.  ELTDXBOP
00140        10  FILLER                      PIC X(6)  VALUE 'PSO  D'.  ELTDXBOP
00141        10  FILLER                      PIC X(6)  VALUE 'PTO  E'.  ELTDXBOP
00142        10  FILLER                      PIC X(6)  VALUE 'PXH  E'.  ELTDXBOP
00143        10  FILLER                      PIC X(6)  VALUE 'RIO  E'.  ELTDXBOP
00144        10  FILLER                      PIC X(6)  VALUE 'VTO  E'.  ELTDXBOP
00145        10  FILLER                      PIC X(6)  VALUE 'XRYO E'.  ELTDXBOP
00146      05  WS-PROF-LIST        REDEFINES    WS-PROF-TAB             ELTDXBOP
00147                                        PIC X(6)  OCCURS 13 TIMES. ELTDXBOP
00148                                                                   ELTDXBOP
00149 /                L I T E R A L S                                  ELTDXBOP
00150  01  WS-PROGRAM-LITERALS.                                         ELTDXBOP
00151    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTDXBOP
00152    05  DAYS                        PIC X(04) VALUE 'DAYS'.        ELTDXBOP
00153    05  WS-NO                       PIC X     VALUE 'N'.           ELTDXBOP
00154    05  WS-YES                      PIC X     VALUE 'Y'.           ELTDXBOP
00155    05  WS-BASIC-LIT                PIC X(08) VALUE                ELTDXBOP
00156        'BASIC: '.                                                 ELTDXBOP
00157    05  WS-SECONDARY-LIT            PIC X(12) VALUE                ELTDXBOP
00158        'SECONDARY: '.                                             ELTDXBOP
00159    05  WS-SUPPLEMENTAL-LIT         PIC X(15) VALUE                ELTDXBOP
00160        'SUPPLEMENTAL: '.                                          ELTDXBOP
00161    05  WS-DAYS-REDUCED             PIC X(44) VALUE                ELTDXBOP
00162          ' OUTPATIENT DIALYSIS TREATMENTS REDUCE DAYS '.          ELTDXBOP
00163    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTDXBOP
00164    05  WS-PAYMNT-BASED             PIC X(20)                      ELTDXBOP
00165          VALUE 'PAYMENT IS BASED ON:'.                            ELTDXBOP
00166    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTDXBOP
00167          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTDXBOP
00168    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTDXBOP
00169          VALUE 'SPILLOVER COINSURANCE: '.                         ELTDXBOP
00170    05  WS-SERVICES-RENDERED        PIC X(26)                      ELTDXBOP
00171          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTDXBOP
00172                                                                   ELTDXBOP
00173 /            D I S P L A Y   L I N E S                            ELTDXBOP
00174  01  WS-ELS-DISPLAY-LINES.                                        ELTDXBOP
00175    05  WS-HDR-1.                                                  ELTDXBOP
00176      10  FILLER                    PIC X(12) VALUE 'SECTION NO: '.ELTDXBOP
00177      10  WS-HDR1-SECT-NO           PIC X(5)  VALUE SPACES.        ELTDXBOP
00178      10  FILLER                    PIC X(22)                      ELTDXBOP
00179          VALUE '      EFFECTIVE DATE: '.                          ELTDXBOP
00180      10  WS-HDR1-DATE              PIC X(8).                      ELTDXBOP
00181      10  FILLER                    PIC X(28)                      ELTDXBOP
00182          VALUE '      FAMILY RELATIONSHIP: '.                     ELTDXBOP
00183      10  WS-HDR1-FRL               PIC X.                         ELTDXBOP
00184      10  FILLER                    PIC X(4) VALUE LOW-VALUES.     ELTDXBOP
00185                                                                   ELTDXBOP
00186    05  WS-HDR-2-INST-IP.                                          ELTDXBOP
00187      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBOP
00188      10  FILLER                    PIC X(44)                      ELTDXBOP
00189         VALUE 'OUTPATIENT DIAGNOSTIC SERVICES INSTITUTIONAL'.     ELTDXBOP
00190      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBOP
00191                                                                   ELTDXBOP
00192    05  WS-HDR-2-PROF.                                             ELTDXBOP
00193      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDXBOP
00194      10  FILLER                    PIC X(43)                      ELTDXBOP
00195        VALUE 'OUTPATIENT DIAGNOSTIC SERVICES PROFESSIONAL'.       ELTDXBOP
00196      10  FILLER                    PIC X(19) VALUE SPACES.        ELTDXBOP
00197                                                                   ELTDXBOP
00198    05  WS-OUTPATIENT-DX-ARE.                                      ELTDXBOP
00199      10  FILLER                    PIC X(30) VALUE                ELTDXBOP
00200          'OUTPATIENT DIAGNOSTIC SERVICES'.                        ELTDXBOP
00201      10  FILLER                    PIC X(49) VALUE SPACES.        ELTDXBOP
00202                                                                   ELTDXBOP
00203    05  WS-FOLLOW-BENEFIT.                                         ELTDXBOP
00204      10  FILLER                    PIC X(79) VALUE                ELTDXBOP
00205          'COVERED SERVICES ARE:'.                                 ELTDXBOP
00206                                                                   ELTDXBOP
00207    05  WS-PAY-CONSDR-TEXT1.                                       ELTDXBOP
00208      10  FILLER                    PIC  X(45)                     ELTDXBOP
00209        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDXBOP
00210                                                                   ELTDXBOP
00211    05  WS-PAY-CONSDR-TEXT2.                                       ELTDXBOP
00212      10  FILLER                    PIC X(45)                      ELTDXBOP
00213        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDXBOP
00214                                                                   ELTDXBOP
00215    05  WS-SERVICES-2ND.                                           ELTDXBOP
00216      10  FILLER                    PIC X(21) VALUE SPACES.        ELTDXBOP
00217      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTDXBOP
00218      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTDXBOP
00219                                                                   ELTDXBOP
00220    05  WS-SERVICES-PAYABLE.                                       ELTDXBOP
00221      10  FILLER                    PIC X(45)                      ELTDXBOP
00222          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTDXBOP
00223      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDXBOP
00224                                                                   ELTDXBOP
00225    05  WS-BASIC                    PIC X(08) VALUE 'BASIC: '.     ELTDXBOP
00226    05  WS-BASIC-A.                                                ELTDXBOP
00227      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXBOP
00228      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDXBOP
00229      10  WS-DTL-BASIC-A            PIC X(63) VALUE SPACES.        ELTDXBOP
00230                                                                   ELTDXBOP
00231    05  WS-SUPPLEMENTAL.                                           ELTDXBOP
00232      10  FILLER                    PIC X(16)                      ELTDXBOP
00233          VALUE '  SUPPLEMENTAL: '.                                ELTDXBOP
00234      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTDXBOP
00235                                                                   ELTDXBOP
00236    05  WS-BASIC-PERCENT.                                          ELTDXBOP
00237      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXBOP
00238      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDXBOP
00239      10  WS-DTL-BASIC-A-PER        PIC X(63) VALUE SPACES.        ELTDXBOP
00240                                                                   ELTDXBOP
00241    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTDXBOP
00242      10  FILLER                    PIC X(16)                      ELTDXBOP
00243          VALUE '  SUPPLEMENTAL: '.                                ELTDXBOP
00244      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTDXBOP
00245                                                                   ELTDXBOP
00246    05  WS-CONTACT-CONTRACT.                                       ELTDXBOP
00247      10  FILLER                    PIC X(50)                      ELTDXBOP
00248        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTDXBOP
00249      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDXBOP
00250                                                                   ELTDXBOP
00251    05  WS-CONTRACT-RELATED.                                       ELTDXBOP
00252      10  FILLER                    PIC X(49)                      ELTDXBOP
00253        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTDXBOP
00254      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDXBOP
00255                                                                   ELTDXBOP
00256    05  WS-PVE-TEXT.                                               ELTDXBOP
00257      10  FILLER                    PIC X(44) VALUE                ELTDXBOP
00258        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTDXBOP
00259                                                                   ELTDXBOP
00260  01  WS-END                            PIC X(16)  VALUE           ELTDXBOP
00261      '*** W/S ENDS ***'.                                          ELTDXBOP
00262 /             L I N K A G E   S E C T I O N                       ELTDXBOP
00263  LINKAGE SECTION.                                                 ELTDXBOP
00264  01  DFHCOMMAREA.                                                 ELTDXBOP
00265      COPY ELSCOMMC.                                               ELTDXBOP
00266 /  *** CIA  AREA ***                                              ELTDXBOP
00267      COPY ELSCIA2C.                                               ELTDXBOP
00268 /  *** IO PARM AREA ***                                           ELTDXBOP
00269      COPY ELSIOPMC.                                               ELTDXBOP
00270 /  *** KEY AREA ***                                               ELTDXBOP
00271      COPY ELSKEYSC.                                               ELTDXBOP
00272 /  *** OUTPUT TEXT AREA ***                                       ELTDXBOP
00273      COPY ELSOUTPC.                                               ELTDXBOP
00274 /  *** TOPIC SELECTION AREA ***                                   ELTDXBOP
00275      COPY ELSSSCBC.                                               ELTDXBOP
00276 /  *** CODE MANUAL INTERFACE ***                                  ELTDXBOP
00277      COPY ELSCMIFC.                                               ELTDXBOP
00278 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDXBOP
00279      COPY ELSCMDSC.                                               ELTDXBOP
00280 /  *** BENEFIT PROVISION TABLE ***                                ELTDXBOP
00281      COPY ELSPRVNC.                                               ELTDXBOP
00282 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDXBOP
00283      COPY ELSTCWAC.                                               ELTDXBOP
00284 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDXBOP
00285      COPY ELSPLGSW.                                               ELTDXBOP
00286                                                                   ELTDXBOP
00287 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDXBOP
00288      COPY ELSPLGTB.                                               ELTDXBOP
00289                                                                   ELTDXBOP
00290  01 CONTRACT-RECORD.                                              ELTDXBOP
00291      COPY GCCONTRC.                                               ELTDXBOP
00292 /                  M A I N L I N E                                ELTDXBOP
00293  PROCEDURE DIVISION.                                              ELTDXBOP
00294                                                                   ELTDXBOP
00295 ******************************************************************ELTDXBOP
00296 *                                                                 ELTDXBOP
00297 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDXBOP
00298 *                                                                 ELTDXBOP
00299 ******************************************************************ELTDXBOP
00300  0000-MAINLINE.                                                   ELTDXBOP
00301                                                                   ELTDXBOP
00302      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDXBOP
00303         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDXBOP
00304                                                                   ELTDXBOP
00305      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDXBOP
00306                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELTDXBOP
00307                                                                   ELTDXBOP
00308      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDXBOP
00309      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00310          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDXBOP
00311      IF NOT CIA-RC-OK                                             ELTDXBOP
00312          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00313                                                                   ELTDXBOP
00314      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDXBOP
00315      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00316          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDXBOP
00317      IF NOT CIA-RC-OK                                             ELTDXBOP
00318          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00319                                                                   ELTDXBOP
00320      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDXBOP
00321      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00322          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDXBOP
00323      IF NOT CIA-RC-OK                                             ELTDXBOP
00324          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00325                                                                   ELTDXBOP
00326      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDXBOP
00327      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00328          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDXBOP
00329      IF NOT CIA-RC-OK                                             ELTDXBOP
00330          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00331                                                                   ELTDXBOP
00332      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDXBOP
00333      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00334          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDXBOP
00335      IF NOT CIA-RC-OK                                             ELTDXBOP
00336          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00337                                                                   ELTDXBOP
00338      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDXBOP
00339      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00340          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDXBOP
00341      IF NOT CIA-RC-OK                                             ELTDXBOP
00342          PERFORM 9998-INVALID-PTR.                                ELTDXBOP
00343                                                                   ELTDXBOP
00344      MOVE '0'  TO  WS-CHAR-0.                                     ELTDXBOP
00345                                                                   ELTDXBOP
00346      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDXBOP
00347                                                                   ELTDXBOP
00348      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDXBOP
00349              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDXBOP
00350                                                                   ELTDXBOP
00351      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXBOP
00352                                                                   ELTDXBOP
00353      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDXBOP
00354                                                                   ELTDXBOP
00355      EXEC CICS LINK                                               ELTDXBOP
00356                PROGRAM('ELUSTGMG')                                ELTDXBOP
00357                COMMAREA(DFHCOMMAREA)                              ELTDXBOP
00358      END-EXEC.                                                    ELTDXBOP
00359                                                                   ELTDXBOP
00360      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXBOP
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00362          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDXBOP
00363                                                                   ELTDXBOP
00364      PERFORM 9999-CHECK-CONTRACT.                                 ELTDXBOP
00365                                                                   ELTDXBOP
00366      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTDXBOP
00367         PERFORM 1000-INSTITUTIONAL-RTNE.                          ELTDXBOP
00368                                                                   ELTDXBOP
00369      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTDXBOP
00370         PERFORM 2000-PROFESSIONAL-RTNE.                           ELTDXBOP
00371                                                                   ELTDXBOP
00372      IF (SSB-PROV-CLASS-INST  OR                                  ELTDXBOP
00373            SSB-PROV-CLASS-PROF  OR                                ELTDXBOP
00374            SSB-PROV-CLASS-BOTH)                                   ELTDXBOP
00375               CONTINUE                                            ELTDXBOP
00376      ELSE                                                         ELTDXBOP
00377         SET CIA-AB-UNDEF TO TRUE                                  ELTDXBOP
00378         EXEC CICS ABEND                                           ELTDXBOP
00379                   ABCODE(CIA-ABCODE)                              ELTDXBOP
00380         END-EXEC                                                  ELTDXBOP
00381      END-IF.                                                      ELTDXBOP
00382 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTDXBOP
00383       MOVE +0  TO  COF-NBR-HDR-LINES.                             ELTDXBOP
00384       MOVE +0  TO  COF-NBR-DTL-LINES.                             ELTDXBOP
00385       MOVE 'E' TO  COF-FUNCTION.                                  ELTDXBOP
00386                                                                   ELTDXBOP
00387      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBOP
00388      END-EXEC.                                                    ELTDXBOP
00389                                                                   ELTDXBOP
00390  0099-RETURN.                                                     ELTDXBOP
00391      GOBACK.                                                      ELTDXBOP
00392                                                                   ELTDXBOP
00393 /        I N S T I T U T I O N A L  R T N E                       ELTDXBOP
00394 ***************************************************************** ELTDXBOP
00395 *        I N S T I T U T I O N A L  R T N E                       ELTDXBOP
00396 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDXBOP
00397 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDXBOP
00398 ***************************************************************** ELTDXBOP
00399  1000-INSTITUTIONAL-RTNE.                                         ELTDXBOP
00400      MOVE '1000'  TO  WS-PARA-ID.                                 ELTDXBOP
00401      MOVE WS-HDR-2-INST-IP TO  COF-HDR-LINE(2).                   ELTDXBOP
00402                                                                   ELTDXBOP
00403      MOVE WS-INST-CNT TO PVN-NBR-BEN-PROVN,                       ELTDXBOP
00404                                 WS-INST-COUNTER.                  ELTDXBOP
00405      PERFORM 6000-MOVE-IN-INST                                    ELTDXBOP
00406         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXBOP
00407         UNTIL WS-SUB  >  WS-INST-CNT.                             ELTDXBOP
00408                                                                   ELTDXBOP
00409      MOVE WS-OUTPATIENT-DX-ARE TO SSB-TOPIC-PHRASE.               ELTDXBOP
00410      PERFORM 8000-CALL-COVERAGE.                                  ELTDXBOP
00411      IF PVN-COVG-NONE                                             ELTDXBOP
00412         NEXT SENTENCE                                             ELTDXBOP
00413      ELSE                                                         ELTDXBOP
00414         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDXBOP
00415                                                                   ELTDXBOP
00416                                                                   ELTDXBOP
00417 /        I N S T I T U T I O N A L   C O M M O N  R T N E         ELTDXBOP
00418  1600-INSTITUTIONAL-COMMON.                                       ELTDXBOP
00419      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDXBOP
00420                                                                   ELTDXBOP
00421      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXBOP
00422                    PSP-PROVN-PRICING-METHD,                       ELTDXBOP
00423                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBOP
00424                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDXBOP
00425                    PSP-SPILL-OVER-DED-APL-IND.                    ELTDXBOP
00426 *                  PSB-PROF-CHRG-HSP-CLM.                         ELTDXBOP
00427                                                                   ELTDXBOP
00428      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXBOP
00429      END-EXEC.                                                    ELTDXBOP
00430                                                                   ELTDXBOP
00431      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXBOP
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00433          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDXBOP
00434                                                                   ELTDXBOP
00435      PERFORM 1630-FIND-FIRST-NONZERO                              ELTDXBOP
00436         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXBOP
00437         UNTIL WS-SUB  >  WS-INST-COUNTER.                         ELTDXBOP
00438                                                                   ELTDXBOP
00439                                                                   ELTDXBOP
00440  1630-FIND-FIRST-NONZERO.                                         ELTDXBOP
00441      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDXBOP
00442      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDXBOP
00443         NEXT SENTENCE                                             ELTDXBOP
00444      ELSE                                                         ELTDXBOP
00445         PERFORM 1640-BUILD-SCREEN-LINES.                          ELTDXBOP
00446                                                                   ELTDXBOP
00447  1640-BUILD-SCREEN-LINES.                                         ELTDXBOP
00448      MOVE '1640'  TO  WS-PARA-ID.                                 ELTDXBOP
00449                                                                   ELTDXBOP
00450      SET PLT-INDEX1 TO                                            ELTDXBOP
00451         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDXBOP
00452                                                                   ELTDXBOP
00453      IF WS-NOT-FIRST-TIME                                         ELTDXBOP
00454         MOVE 'P' TO COF-FUNCTION                                  ELTDXBOP
00455 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDXBOP
00456 ******** REB ===> 12/15/87.                                       ELTDXBOP
00457 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDXBOP
00458         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDXBOP
00459         EXEC CICS LINK                                            ELTDXBOP
00460                   PROGRAM('ELUOUTPT')                             ELTDXBOP
00461                   COMMAREA(DFHCOMMAREA)                           ELTDXBOP
00462         END-EXEC                                                  ELTDXBOP
00463      ELSE                                                         ELTDXBOP
00464        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDXBOP
00465                                                                   ELTDXBOP
00466      MOVE +1  TO  WS-CIA.                                         ELTDXBOP
00467      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXBOP
00468         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXBOP
00469            SET PLT-INDEX2  TO  2                                  ELTDXBOP
00470         ELSE                                                      ELTDXBOP
00471            PERFORM 1690-PROBLEM-WITH-INDICES                      ELTDXBOP
00472            PERFORM 3000-OUTPUT-TEXT                               ELTDXBOP
00473      ELSE                                                         ELTDXBOP
00474         SET PLT-INDEX2  TO  1.                                    ELTDXBOP
00475      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDXBOP
00476                                                                   ELTDXBOP
00477      ADD +1                 TO WS-CIA.                            ELTDXBOP
00478      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDXBOP
00479      ADD +1                 TO WS-CIA.                            ELTDXBOP
00480                                                                   ELTDXBOP
00481      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTDXBOP
00482                                                                   ELTDXBOP
00483      PERFORM 1650-ZERO-ALL-WITH-SAME-NO                           ELTDXBOP
00484         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDXBOP
00485         UNTIL WS-SUB2  >  WS-INST-COUNTER.                        ELTDXBOP
00486                                                                   ELTDXBOP
00487      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXBOP
00488               NOT = '0' AND NOT = LOW-VALUES                      ELTDXBOP
00489           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDXBOP
00490                                                                   ELTDXBOP
00491      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBOP
00492 *    ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00493      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDXBOP
00494      ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00495                                                                   ELTDXBOP
00496      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00497         SET  PLT-INDEX2           TO  1                           ELTDXBOP
00498         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDXBOP
00499                                                                   ELTDXBOP
00500      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBOP
00501         SET  PLT-INDEX2             TO  2                         ELTDXBOP
00502         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDXBOP
00503                                                                   ELTDXBOP
00504      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00505         SET  PLT-INDEX2           TO  1                           ELTDXBOP
00506                  IF WS-BASIC-LOB                                  ELTDXBOP
00507                     SET PROCESSING-BASIC-INFO TO TRUE             ELTDXBOP
00508                  END-IF                                           ELTDXBOP
00509         PERFORM 4500-TRANS-OTHER-RESP-IND.                        ELTDXBOP
00510                                                                   ELTDXBOP
00511      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBOP
00512            SET  PLT-INDEX2             TO  2                      ELTDXBOP
00513           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDXBOP
00514                                                                   ELTDXBOP
00515      SET   PLT-INDEX2          TO  1.                             ELTDXBOP
00516      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00517         MOVE 1 TO WS-CIA                                          ELTDXBOP
00518         MOVE SPACES  TO  COF-DTL-LINE(WS-CIA)                     ELTDXBOP
00519         MOVE 1 TO WS-CIA                                          ELTDXBOP
00520         MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)         ELTDXBOP
00521         ADD  +1                   TO  WS-CIA                      ELTDXBOP
00522         IF WS-BASIC-LOB                                           ELTDXBOP
00523            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00524         END-IF                                                    ELTDXBOP
00525         PERFORM 4300-SPILLOVER-COINS                              ELTDXBOP
00526      ELSE                                                         ELTDXBOP
00527        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00528        IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO       ELTDXBOP
00529            MOVE 1 TO WS-CIA                                       ELTDXBOP
00530            MOVE SPACES  TO  COF-DTL-LINE(WS-CIA)                  ELTDXBOP
00531            MOVE 1 TO WS-CIA                                       ELTDXBOP
00532            MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)      ELTDXBOP
00533            ADD +1                   TO  WS-CIA                    ELTDXBOP
00534            IF WS-BASIC-LOB                                        ELTDXBOP
00535               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBOP
00536            END-IF                                                 ELTDXBOP
00537           PERFORM 4300-SPILLOVER-COINS.                           ELTDXBOP
00538        MOVE 1 TO WS-CIA.                                          ELTDXBOP
00539        MOVE SPACES TO COF-DTL-LINE(WS-CIA).                       ELTDXBOP
00540                                                                   ELTDXBOP
00541 *      SET   PLT-INDEX2          TO  2                            ELTDXBOP
00542                                                                   ELTDXBOP
00543      SET   PLT-INDEX2          TO  1.                             ELTDXBOP
00544      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00545         MOVE 1 TO WS-CIA                                          ELTDXBOP
00546         MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA)         ELTDXBOP
00547         ADD  +1                   TO  WS-CIA                      ELTDXBOP
00548         IF WS-BASIC-LOB                                           ELTDXBOP
00549            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00550         END-IF                                                    ELTDXBOP
00551         PERFORM 4400-SPILLOVER-DEDUCT                             ELTDXBOP
00552      ELSE                                                         ELTDXBOP
00553        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00554        IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO       ELTDXBOP
00555            MOVE 1 TO WS-CIA                                       ELTDXBOP
00556            MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA)      ELTDXBOP
00557            ADD +1                   TO  WS-CIA                    ELTDXBOP
00558            IF WS-BASIC-LOB                                        ELTDXBOP
00559               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBOP
00560            END-IF                                                 ELTDXBOP
00561           PERFORM 4400-SPILLOVER-DEDUCT.                          ELTDXBOP
00562        MOVE 1 TO WS-CIA.                                          ELTDXBOP
00563        MOVE SPACES TO COF-DTL-LINE(WS-CIA).                       ELTDXBOP
00564                                                                   ELTDXBOP
00565        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00566 *      PERFORM 3000-OUTPUT-TEXT.                                  ELTDXBOP
00567                                                                   ELTDXBOP
00568      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXBOP
00569      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXBOP
00570                                                                   ELTDXBOP
00571  1650-ZERO-ALL-WITH-SAME-NO.                                      ELTDXBOP
00572      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDXBOP
00573                                                                   ELTDXBOP
00574      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTDXBOP
00575          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXBOP
00576              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTDXBOP
00577                                                                   ELTDXBOP
00578      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDXBOP
00579         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXBOP
00580         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXBOP
00581         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXBOP
00582                                                   CMF-CODE-VALUE  ELTDXBOP
00583         PERFORM 8500-CALL-CODES-MANUAL                            ELTDXBOP
00584         STRING CMF-DESCR-LINE(1) ' '                              ELTDXBOP
00585                CMF-DESCR-LINE(2) ' '                              ELTDXBOP
00586                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDXBOP
00587         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDXBOP
00588                                                                   ELTDXBOP
00589         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDXBOP
00590         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDXBOP
00591         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDXBOP
00592         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDXBOP
00593         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDXBOP
00594         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDXBOP
00595         IF WS-CIA  <  20                                          ELTDXBOP
00596            ADD +1  TO  WS-CIA                                     ELTDXBOP
00597            MOVE ZERO  TO                                          ELTDXBOP
00598                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXBOP
00599         ELSE                                                      ELTDXBOP
00600            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXBOP
00601                COMMAREA(DFHCOMMAREA)                              ELTDXBOP
00602            END-EXEC                                               ELTDXBOP
00603            MOVE +1  TO  WS-CIA                                    ELTDXBOP
00604            MOVE ZERO  TO                                          ELTDXBOP
00605                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDXBOP
00606                                                                   ELTDXBOP
00607  1690-PROBLEM-WITH-INDICES.                                       ELTDXBOP
00608                                                                   ELTDXBOP
00609      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXBOP
00610      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXBOP
00611                                                                   ELTDXBOP
00612      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBOP
00613      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBOP
00614                                                                   ELTDXBOP
00615      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBOP
00616      END-EXEC.                                                    ELTDXBOP
00617                                                                   ELTDXBOP
00618                                                                   ELTDXBOP
00619 /        P R O F E S S I O N A L   L A B   R T N E                ELTDXBOP
00620 ***************************************************************** ELTDXBOP
00621 *        P R O F E S S I O N A L   L A B   R T N E                ELTDXBOP
00622 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDXBOP
00623 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDXBOP
00624 ***************************************************************** ELTDXBOP
00625  2000-PROFESSIONAL-RTNE.                                          ELTDXBOP
00626      MOVE '2000'  TO  WS-PARA-ID.                                 ELTDXBOP
00627      MOVE WS-HDR-2-PROF TO  COF-HDR-LINE(2).                      ELTDXBOP
00628                                                                   ELTDXBOP
00629      MOVE WS-PROF-CNT TO PVN-NBR-BEN-PROVN,                       ELTDXBOP
00630                                 WS-PROF-COUNTER.                  ELTDXBOP
00631      PERFORM 7000-MOVE-IN-PROF                                    ELTDXBOP
00632         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXBOP
00633         UNTIL WS-SUB  >  WS-PROF-CNT.                             ELTDXBOP
00634                                                                   ELTDXBOP
00635      MOVE WS-OUTPATIENT-DX-ARE TO SSB-TOPIC-PHRASE.               ELTDXBOP
00636      PERFORM 8000-CALL-COVERAGE.                                  ELTDXBOP
00637      IF PVN-COVG-NONE                                             ELTDXBOP
00638         NEXT SENTENCE                                             ELTDXBOP
00639      ELSE                                                         ELTDXBOP
00640         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDXBOP
00641                                                                   ELTDXBOP
00642 /                                                                 ELTDXBOP
00643  2600-PROFESSIONAL-COMMON.                                        ELTDXBOP
00644      MOVE '2600'  TO WS-PARA-ID.                                  ELTDXBOP
00645                                                                   ELTDXBOP
00646      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDXBOP
00647                                                                   ELTDXBOP
00648      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXBOP
00649                    PSP-PROVN-PRICING-METHD,                       ELTDXBOP
00650                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBOP
00651                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDXBOP
00652                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDXBOP
00653                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDXBOP
00654                    PSE-BEN-SCOPE-ID.                              ELTDXBOP
00655                                                                   ELTDXBOP
00656      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXBOP
00657      END-EXEC.                                                    ELTDXBOP
00658                                                                   ELTDXBOP
00659      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXBOP
00660      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
00661          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDXBOP
00662                                                                   ELTDXBOP
00663      PERFORM 2630-FIND-FIRST-NONZERO                              ELTDXBOP
00664         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXBOP
00665         UNTIL WS-SUB  >  WS-PROF-COUNTER.                         ELTDXBOP
00666                                                                   ELTDXBOP
00667                                                                   ELTDXBOP
00668  2630-FIND-FIRST-NONZERO.                                         ELTDXBOP
00669      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDXBOP
00670      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTDXBOP
00671         NEXT SENTENCE                                             ELTDXBOP
00672      ELSE                                                         ELTDXBOP
00673         PERFORM 2640-BUILD-SCREEN-LINES.                          ELTDXBOP
00674                                                                   ELTDXBOP
00675  2640-BUILD-SCREEN-LINES.                                         ELTDXBOP
00676      MOVE '2640'  TO  WS-PARA-ID.                                 ELTDXBOP
00677                                                                   ELTDXBOP
00678      SET PLT-INDEX1 TO                                            ELTDXBOP
00679         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDXBOP
00680                                                                   ELTDXBOP
00681      IF WS-NOT-FIRST-TIME                                         ELTDXBOP
00682         MOVE 'P' TO COF-FUNCTION                                  ELTDXBOP
00683 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDXBOP
00684 ******** REB ===> 12/15/87.                                       ELTDXBOP
00685 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDXBOP
00686         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDXBOP
00687         EXEC CICS LINK                                            ELTDXBOP
00688                   PROGRAM('ELUOUTPT')                             ELTDXBOP
00689                   COMMAREA(DFHCOMMAREA)                           ELTDXBOP
00690         END-EXEC                                                  ELTDXBOP
00691      ELSE                                                         ELTDXBOP
00692        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDXBOP
00693                                                                   ELTDXBOP
00694      MOVE +1  TO  WS-CIA.                                         ELTDXBOP
00695      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXBOP
00696         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXBOP
00697            SET PLT-INDEX2  TO  2                                  ELTDXBOP
00698         ELSE                                                      ELTDXBOP
00699            PERFORM 2690-PROBLEM-WITH-INDICES                      ELTDXBOP
00700            PERFORM 3000-OUTPUT-TEXT                               ELTDXBOP
00701      ELSE                                                         ELTDXBOP
00702         SET PLT-INDEX2  TO  1.                                    ELTDXBOP
00703      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDXBOP
00704                                                                   ELTDXBOP
00705      ADD +1                 TO WS-CIA.                            ELTDXBOP
00706      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDXBOP
00707      ADD +1                 TO WS-CIA.                            ELTDXBOP
00708      PERFORM 2650-ZERO-ALL-WITH-SAME-NO                           ELTDXBOP
00709         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDXBOP
00710         UNTIL WS-SUB2  >  WS-PROF-COUNTER.                        ELTDXBOP
00711                                                                   ELTDXBOP
00712      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXBOP
00713               NOT = '0' AND NOT = LOW-VALUES                      ELTDXBOP
00714           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDXBOP
00715                                                                   ELTDXBOP
00716      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBOP
00717      MOVE WS-NO      TO  WS-DISPLAY-PAYMNT-BASED-TEXT.            ELTDXBOP
00718                                                                   ELTDXBOP
00719      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00720       SET  PLT-INDEX2       TO  1                                 ELTDXBOP
00721       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBOP
00722        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBOP
00723                 '0000' AND NOT = '00  '                           ELTDXBOP
00724              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDXBOP
00725                                                                   ELTDXBOP
00726      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBOP
00727       SET PLT-INDEX2        TO 2                                  ELTDXBOP
00728       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBOP
00729        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBOP
00730                 '0000' AND NOT = '00  '                           ELTDXBOP
00731              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDXBOP
00732                                                                   ELTDXBOP
00733      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTDXBOP
00734          ADD  +1               TO  WS-CIA                         ELTDXBOP
00735          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTDXBOP
00736          ADD  +1               TO  WS-CIA.                        ELTDXBOP
00737                                                                   ELTDXBOP
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00739       SET  PLT-INDEX2       TO  1                                 ELTDXBOP
00740       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBOP
00741        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBOP
00742                 '0000' AND NOT = '00  '                           ELTDXBOP
00743         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDXBOP
00744         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDXBOP
00745                               CMF-CODE-VALUE                      ELTDXBOP
00746         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDXBOP
00747         IF WS-BASIC-LOB                                           ELTDXBOP
00748            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00749         END-IF                                                    ELTDXBOP
00750         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXBOP
00751         PERFORM 9100-DETERMINE-OUTPUT-METHOD.                     ELTDXBOP
00752         INITIALIZE TCAR-FROM-AREA.                                ELTDXBOP
00753                                                                   ELTDXBOP
00754      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBOP
00755       SET PLT-INDEX2        TO 2                                  ELTDXBOP
00756       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDXBOP
00757        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDXBOP
00758                 '0000' AND NOT = '00  '                           ELTDXBOP
00759         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDXBOP
00760         MOVE 'BEN-SCOPE-ID' TO                                    ELTDXBOP
00761                   CMF-ELEMENT-SYSTEM-NAME                         ELTDXBOP
00762         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)             ELTDXBOP
00763                         TO  CMF-CODE-VALUE                        ELTDXBOP
00764      IF WS-BASIC-LOB                                              ELTDXBOP
00765         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBOP
00766      END-IF                                                       ELTDXBOP
00767      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBOP
00768      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
00769      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
00770                                                                   ELTDXBOP
00771 *    ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00772      ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00773      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDXBOP
00774      ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00775                                                                   ELTDXBOP
00776      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00777         SET  PLT-INDEX2           TO  1                           ELTDXBOP
00778         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDXBOP
00779                                                                   ELTDXBOP
00780      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXBOP
00781         SET  PLT-INDEX2             TO  2                         ELTDXBOP
00782         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDXBOP
00783                                                                   ELTDXBOP
00784      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00785         IF WS-BASIC-LOB                                           ELTDXBOP
00786            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00787         END-IF                                                    ELTDXBOP
00788         SET  PLT-INDEX2           TO  1                           ELTDXBOP
00789         PERFORM 4500-TRANS-OTHER-RESP-IND                         ELTDXBOP
00790      ELSE                                                         ELTDXBOP
00791         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDXBOP
00792            IF WS-BASIC-LOB                                        ELTDXBOP
00793               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBOP
00794            END-IF                                                 ELTDXBOP
00795            SET  PLT-INDEX2             TO  2                      ELTDXBOP
00796           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDXBOP
00797                                                                   ELTDXBOP
00798                                                                   ELTDXBOP
00799      SET   PLT-INDEX2          TO  1.                             ELTDXBOP
00800      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00801         MOVE 1 TO WS-CIA                                          ELTDXBOP
00802         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXBOP
00803         ADD +1 TO WS-CIA                                          ELTDXBOP
00804         MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)         ELTDXBOP
00805         ADD  +1                   TO  WS-CIA                      ELTDXBOP
00806         IF WS-BASIC-LOB                                           ELTDXBOP
00807            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00808         END-IF                                                    ELTDXBOP
00809         PERFORM 4300-SPILLOVER-COINS                              ELTDXBOP
00810      ELSE                                                         ELTDXBOP
00811        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00812        IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO       ELTDXBOP
00813            MOVE 1 TO WS-CIA                                       ELTDXBOP
00814            MOVE WS-SPILLOVER-COINS  TO  COF-DTL-LINE(WS-CIA)      ELTDXBOP
00815            ADD +1                   TO  WS-CIA                    ELTDXBOP
00816            IF WS-BASIC-LOB                                        ELTDXBOP
00817               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBOP
00818            END-IF                                                 ELTDXBOP
00819           PERFORM 4300-SPILLOVER-COINS.                           ELTDXBOP
00820           MOVE 1 TO WS-CIA.                                       ELTDXBOP
00821           MOVE SPACES  TO  COF-DTL-LINE(WS-CIA).                  ELTDXBOP
00822                                                                   ELTDXBOP
00823 *      SET   PLT-INDEX2          TO  2                            ELTDXBOP
00824                                                                   ELTDXBOP
00825      SET   PLT-INDEX2          TO  1.                             ELTDXBOP
00826      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXBOP
00827         MOVE 1 TO WS-CIA                                          ELTDXBOP
00828         MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA)         ELTDXBOP
00829         ADD  +1                   TO  WS-CIA                      ELTDXBOP
00830         IF WS-BASIC-LOB                                           ELTDXBOP
00831            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXBOP
00832         END-IF                                                    ELTDXBOP
00833         PERFORM 4400-SPILLOVER-DEDUCT                             ELTDXBOP
00834      ELSE                                                         ELTDXBOP
00835        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00836        IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO       ELTDXBOP
00837            MOVE 1 TO WS-CIA                                       ELTDXBOP
00838            MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA)      ELTDXBOP
00839            ADD +1                   TO  WS-CIA                    ELTDXBOP
00840            IF WS-BASIC-LOB                                        ELTDXBOP
00841               SET PROCESSING-SUPPLEMENTAL TO TRUE                 ELTDXBOP
00842            END-IF                                                 ELTDXBOP
00843           PERFORM 4400-SPILLOVER-DEDUCT.                          ELTDXBOP
00844           MOVE 1 TO WS-CIA.                                       ELTDXBOP
00845           MOVE SPACES TO COF-DTL-LINE(WS-CIA).                    ELTDXBOP
00846                                                                   ELTDXBOP
00847        SET   PLT-INDEX2          TO  2                            ELTDXBOP
00848                                                                   ELTDXBOP
00849      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXBOP
00850      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXBOP
00851                                                                   ELTDXBOP
00852  2650-ZERO-ALL-WITH-SAME-NO.                                      ELTDXBOP
00853      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDXBOP
00854                                                                   ELTDXBOP
00855      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDXBOP
00856         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXBOP
00857         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXBOP
00858         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXBOP
00859                                                   CMF-CODE-VALUE  ELTDXBOP
00860         PERFORM 8500-CALL-CODES-MANUAL                            ELTDXBOP
00861         STRING CMF-DESCR-LINE(1) ' '                              ELTDXBOP
00862                CMF-DESCR-LINE(2) ' '                              ELTDXBOP
00863                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDXBOP
00864         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDXBOP
00865                                                                   ELTDXBOP
00866         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDXBOP
00867         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDXBOP
00868         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDXBOP
00869         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDXBOP
00870         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDXBOP
00871         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDXBOP
00872         IF WS-CIA  <  20                                          ELTDXBOP
00873            ADD +1  TO  WS-CIA                                     ELTDXBOP
00874            MOVE ZERO  TO                                          ELTDXBOP
00875                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXBOP
00876         ELSE                                                      ELTDXBOP
00877            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXBOP
00878                COMMAREA(DFHCOMMAREA)                              ELTDXBOP
00879            END-EXEC                                               ELTDXBOP
00880            MOVE +1  TO  WS-CIA                                    ELTDXBOP
00881            MOVE ZERO  TO                                          ELTDXBOP
00882                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDXBOP
00883                                                                   ELTDXBOP
00884  2690-PROBLEM-WITH-INDICES.                                       ELTDXBOP
00885                                                                   ELTDXBOP
00886      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXBOP
00887      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXBOP
00888                                                                   ELTDXBOP
00889      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBOP
00890      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBOP
00891                                                                   ELTDXBOP
00892      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBOP
00893      END-EXEC.                                                    ELTDXBOP
00894                                                                   ELTDXBOP
00895  2699-EXIT.   EXIT.                                               ELTDXBOP
00896                                                                   ELTDXBOP
00897 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTDXBOP
00898  3000-OUTPUT-TEXT.                                                ELTDXBOP
00899      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBOP
00900      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDXBOP
00901      MOVE ' '  TO  COF-FUNCTION.                                  ELTDXBOP
00902                                                                   ELTDXBOP
00903      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDXBOP
00904                       COMMAREA(DFHCOMMAREA)                       ELTDXBOP
00905      END-EXEC.                                                    ELTDXBOP
00906      MOVE +1   TO WS-CIA.                                         ELTDXBOP
00907      MOVE +1   TO TCAR-FROM-SUB.                                  ELTDXBOP
00908      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
00909                                                                   ELTDXBOP
00910 /                                                                 ELTDXBOP
00911  4000-PLACE-OF-TREATMENT.                                         ELTDXBOP
00912      SET WS-PROCESS-POT TO TRUE.                                  ELTDXBOP
00913      ADD +1     TO  WS-CIA.                                       ELTDXBOP
00914      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXBOP
00915      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTDXBOP
00916      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXBOP
00917                                               TO  CMF-CODE-VALUE. ELTDXBOP
00918      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXBOP
00919      ADD  +1                   TO  WS-CIA.                        ELTDXBOP
00920      MOVE WS-SERVICES-RENDERED                                    ELTDXBOP
00921           TO COF-DTL-LINE(WS-CIA).                                ELTDXBOP
00922      ADD 1 TO WS-CIA.                                             ELTDXBOP
00923      PERFORM 8500-CALL-CODES-MANUAL.                              ELTDXBOP
00924      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
00925      INITIALIZE WS-POT-SWITCH                                     ELTDXBOP
00926                 TCAR-FROM-AREA.                                   ELTDXBOP
00927      MOVE 1 TO WS-CIA.                                            ELTDXBOP
00928                                                                   ELTDXBOP
00929 /                                                                 ELTDXBOP
00930  4100-PAYABLE-AS-BASIC.                                           ELTDXBOP
00931      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDXBOP
00932          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDXBOP
00933          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTDXBOP
00934      ELSE                                                         ELTDXBOP
00935         MOVE 'BP'                  TO  CMF-RECORD-PREFIX          ELTDXBOP
00936         MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBOP
00937         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXBOP
00938                                                CMF-CODE-VALUE     ELTDXBOP
00939      IF WS-BASIC-LOB                                              ELTDXBOP
00940         SET PROCESSING-BASIC-INFO TO TRUE                         ELTDXBOP
00941      END-IF.                                                      ELTDXBOP
00942      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBOP
00943      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
00944      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
00945                                                                   ELTDXBOP
00946  4100-OUTPUT-TEXT.                                                ELTDXBOP
00947        PERFORM 3000-OUTPUT-TEXT.                                  ELTDXBOP
00948                                                                   ELTDXBOP
00949 /                                                                 ELTDXBOP
00950  4200-PAYABLE-AS-SUPP.                                            ELTDXBOP
00951      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDXBOP
00952          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDXBOP
00953          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTDXBOP
00954      ELSE                                                         ELTDXBOP
00955         MOVE 'BP'                  TO  CMF-RECORD-PREFIX          ELTDXBOP
00956         MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME    ELTDXBOP
00957         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXBOP
00958                                                CMF-CODE-VALUE     ELTDXBOP
00959      END-IF.                                                      ELTDXBOP
00960      IF WS-BASIC-LOB                                              ELTDXBOP
00961         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXBOP
00962      END-IF.                                                      ELTDXBOP
00963      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBOP
00964      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
00965      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
00966                                                                   ELTDXBOP
00967  4200-OUTPUT-TEXT.                                                ELTDXBOP
00968      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBOP
00969 /                                                                 ELTDXBOP
00970                                                                   ELTDXBOP
00971 /                                                                 ELTDXBOP
00972  4300-SPILLOVER-COINS.                                            ELTDXBOP
00973      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXBOP
00974            = '0'                                                  ELTDXBOP
00975         CONTINUE                                                  ELTDXBOP
00976      ELSE                                                         ELTDXBOP
00977         PERFORM 4301-SPILLOVER-CONTD.                             ELTDXBOP
00978                                                                   ELTDXBOP
00979  4301-SPILLOVER-CONTD.                                            ELTDXBOP
00980      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBOP
00981 *    ADD +1     TO  WS-CIA.                                       ELTDXBOP
00982      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXBOP
00983      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDXBOP
00984      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDXBOP
00985                       TO CMF-CODE-VALUE.                          ELTDXBOP
00986      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBOP
00987      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
00988      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
00989                                                                   ELTDXBOP
00990 /                                                                 ELTDXBOP
00991  4400-SPILLOVER-DEDUCT.                                           ELTDXBOP
00992      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXBOP
00993            = '0'                                                  ELTDXBOP
00994         NEXT SENTENCE                                             ELTDXBOP
00995      ELSE                                                         ELTDXBOP
00996         PERFORM 4401-SPILLOVER-DEDUCT-CONTD.                      ELTDXBOP
00997                                                                   ELTDXBOP
00998  4401-SPILLOVER-DEDUCT-CONTD.                                     ELTDXBOP
00999 *    ADD +1     TO  WS-CIA.                                       ELTDXBOP
01000      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXBOP
01001 *    MOVE WS-SPILLOVER-DEDBL  TO  COF-DTL-LINE(WS-CIA).           ELTDXBOP
01002 *    ADD  +1                   TO  WS-CIA.                        ELTDXBOP
01003      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXBOP
01004      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXBOP
01005      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXBOP
01006                       TO CMF-CODE-VALUE.                          ELTDXBOP
01007      PERFORM 9000-CALL-CODES-MANUAL                               ELTDXBOP
01008      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
01009      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
01010                                                                   ELTDXBOP
01011 /                                                                 ELTDXBOP
01012  4500-TRANS-OTHER-RESP-IND.                                       ELTDXBOP
01013      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTDXBOP
01014            = ZEROS OR LOW-VALUES                                  ELTDXBOP
01015          CONTINUE                                                 ELTDXBOP
01016      ELSE                                                         ELTDXBOP
01017         PERFORM 4501-TRANS-OTHER-RESP-CONTD.                      ELTDXBOP
01018                                                                   ELTDXBOP
01019  4501-TRANS-OTHER-RESP-CONTD.                                     ELTDXBOP
01020      SET WS-PROCESS-POT TO TRUE.                                  ELTDXBOP
01021 *    ADD +1     TO  WS-CIA.                                       ELTDXBOP
01022      ADD +1     TO  WS-CIA.                                       ELTDXBOP
01023      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXBOP
01024      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXBOP
01025      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDXBOP
01026                       TO CMF-CODE-VALUE                           ELTDXBOP
01027      PERFORM 8500-CALL-CODES-MANUAL                               ELTDXBOP
01028      PERFORM 9100-DETERMINE-OUTPUT-METHOD.                        ELTDXBOP
01029      INITIALIZE WS-POT-SWITCH.                                    ELTDXBOP
01030 /                                                                 ELTDXBOP
01031  4675-PAY-CONSID-TEXT.                                            ELTDXBOP
01032      INITIALIZE TCAR-FROM-AREA.                                   ELTDXBOP
01033      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDXBOP
01034             WS-PAY-CONSDR-TEXT2                                   ELTDXBOP
01035                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDXBOP
01036      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXBOP
01037      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDXBOP
01038      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDXBOP
01039                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDXBOP
01040      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXBOP
01041      IF WS-CIA > 17                                               ELTDXBOP
01042            PERFORM 3000-OUTPUT-TEXT                               ELTDXBOP
01043            MOVE +1            TO WS-CIA.                          ELTDXBOP
01044      ADD +1                TO  WS-CIA.                            ELTDXBOP
01045      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDXBOP
01046      ADD +1                TO  WS-CIA.                            ELTDXBOP
01047      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDXBOP
01048      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBOP
01049                                                                   ELTDXBOP
01050  4900-BEN-TAB-PVE.                                                ELTDXBOP
01051      MOVE +1 TO WS-CIA.                                           ELTDXBOP
01052      MOVE SPACES TO COF-DTL-LINE(WS-CIA).                         ELTDXBOP
01053      ADD  +1         TO WS-CIA.                                   ELTDXBOP
01054      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDXBOP
01055      PERFORM 3000-OUTPUT-TEXT.                                    ELTDXBOP
01056 /                                                                 ELTDXBOP
01057  6000-MOVE-IN-INST.                                               ELTDXBOP
01058      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXBOP
01059      MOVE WS-INST-LIST(WS-SUB)  TO                                ELTDXBOP
01060                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXBOP
01061      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDXBOP
01062                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDXBOP
01063                                                                   ELTDXBOP
01064 /                                                                 ELTDXBOP
01065  7000-MOVE-IN-PROF.                                               ELTDXBOP
01066      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXBOP
01067      MOVE WS-PROF-LIST(WS-SUB)  TO                                ELTDXBOP
01068                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXBOP
01069      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDXBOP
01070                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDXBOP
01071                                                                   ELTDXBOP
01072 /                                                                 ELTDXBOP
01073  8000-CALL-COVERAGE.                                              ELTDXBOP
01074      MOVE '8000'            TO  WS-PARA-ID.                       ELTDXBOP
01075 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDXBOP
01076      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXBOP
01077      MOVE WS-YES TO WS-FIRSTTIME-IND.                             ELTDXBOP
01078      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDXBOP
01079      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDXBOP
01080                                                                   ELTDXBOP
01081      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBOP
01082      END-EXEC.                                                    ELTDXBOP
01083                                                                   ELTDXBOP
01084      PERFORM 8300-DISPLAY-COVERAGE.                               ELTDXBOP
01085                                                                   ELTDXBOP
01086 *                                                                 ELTDXBOP
01087  8300-DISPLAY-COVERAGE.                                           ELTDXBOP
01088 ********************************************************          ELTDXBOP
01089 ***** REARRANGING THIS PARAGRAPH SO THAT YOU ARE NOT ***          ELTDXBOP
01090 ***** FORCED TO DO A LINK FOR A NEW PAGE WHEN YOU    ***          ELTDXBOP
01091 ***** DO NOT NEED ONE.     12/11/87 ==> REB.         ***          ELTDXBOP
01092 ********************************************************          ELTDXBOP
01093      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDXBOP
01094      END-EXEC.                                                    ELTDXBOP
01095                                                                   ELTDXBOP
01096 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDXBOP
01097      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXBOP
01098      MOVE ' '  TO  COF-FUNCTION.                                  ELTDXBOP
01099      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXBOP
01100      END-EXEC.                                                    ELTDXBOP
01101 *4/15 END OF TEMPORARY CODE                                       ELTDXBOP
01102                                                                   ELTDXBOP
01103 /  C O D E S   M A N U A L   C A L L                              ELTDXBOP
01104  8500-CALL-CODES-MANUAL.                                          ELTDXBOP
01105      INITIALIZE CMF-RETURN-CODE.                                  ELTDXBOP
01106      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXBOP
01107               COMMAREA(DFHCOMMAREA)                               ELTDXBOP
01108      END-EXEC.                                                    ELTDXBOP
01109                                                                   ELTDXBOP
01110      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXBOP
01111      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01112          ADDRESS OF CMF-DESCR.                                    ELTDXBOP
01113                                                                   ELTDXBOP
01114  9000-CALL-CODES-MANUAL.                                          ELTDXBOP
01115      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXBOP
01116                       COMMAREA(DFHCOMMAREA)                       ELTDXBOP
01117      END-EXEC.                                                    ELTDXBOP
01118      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXBOP
01119      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01120                      ADDRESS OF CMF-DESCR.                        ELTDXBOP
01121                                                                   ELTDXBOP
01122  9100-DETERMINE-OUTPUT-METHOD.                                    ELTDXBOP
01123      MOVE CMF-DESCR-LINE(1) TO WS-TEST-LINE.                      ELTDXBOP
01124      IF WS-TEST-CHAR = WS-SINGLE-QUOTE                            ELTDXBOP
01125         MOVE SPACE TO WS-TEST-CHAR                                ELTDXBOP
01126         MOVE WS-TEST-DATA TO CMF-DESCR-LINE(1)                    ELTDXBOP
01127         PERFORM 9200-DIRECT-OUTPUT                                ELTDXBOP
01128      ELSE                                                         ELTDXBOP
01129         PERFORM 9300-DISPLAY-CODE-VALUES                          ELTDXBOP
01130      END-IF.                                                      ELTDXBOP
01131                                                                   ELTDXBOP
01132  9200-DIRECT-OUTPUT.                                              ELTDXBOP
01133      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXBOP
01134        EVALUATE TRUE                                              ELTDXBOP
01135            WHEN PROCESSING-BASIC-INFO                             ELTDXBOP
01136               IF WS-PROCESS-POT                                   ELTDXBOP
01137                  CONTINUE                                         ELTDXBOP
01138               ELSE                                                ELTDXBOP
01139                  MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)            ELTDXBOP
01140                  ADD 1 TO WS-CIA                                  ELTDXBOP
01141               END-IF                                              ELTDXBOP
01142            WHEN PROCESSING-SUPPLEMENTAL                           ELTDXBOP
01143               IF WS-PROCESS-POT                                   ELTDXBOP
01144                  CONTINUE                                         ELTDXBOP
01145               ELSE                                                ELTDXBOP
01146                  MOVE WS-SUPPLEMENTAL-LIT                         ELTDXBOP
01147                      TO COF-DTL-LINE(WS-CIA)                      ELTDXBOP
01148                  ADD 1 TO WS-CIA                                  ELTDXBOP
01149               END-IF                                              ELTDXBOP
01150            WHEN OTHER                                             ELTDXBOP
01151               CONTINUE                                            ELTDXBOP
01152        END-EVALUATE.                                              ELTDXBOP
01153      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXBOP
01154         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXBOP
01155        MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                      ELTDXBOP
01156           COF-DTL-LINE(WS-CIA)                                    ELTDXBOP
01157        ADD 1 TO WS-CIA                                            ELTDXBOP
01158      END-PERFORM.                                                 ELTDXBOP
01159      IF WS-PROCESS-POT OR NOT PROCESSING-BASIC-INFO               ELTDXBOP
01160         ADD 1 TO WS-CIA                                           ELTDXBOP
01161         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXBOP
01162      END-IF.                                                      ELTDXBOP
01163      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXBOP
01164      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXBOP
01165                            DFHCOMMAREA.                           ELTDXBOP
01166      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXBOP
01167                WS-CIA                                             ELTDXBOP
01168                TCAR-FROM-SUB.                                     ELTDXBOP
01169                                                                   ELTDXBOP
01170  9300-DISPLAY-CODE-VALUES.                                        ELTDXBOP
01171      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXBOP
01172         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXBOP
01173         MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                     ELTDXBOP
01174            TCAR-FROM-LINE(TCAR-FROM-SUB)                          ELTDXBOP
01175         ADD 1 TO TCAR-FROM-SUB                                    ELTDXBOP
01176      END-PERFORM.                                                 ELTDXBOP
01177      COMPUTE TCAR-FROM-LENGTH = TCAR-FROM-SUB * 79.               ELTDXBOP
01178      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXBOP
01179      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXBOP
01180      PERFORM 9610-UNSTRING-TEXT.                                  ELTDXBOP
01181      PERFORM UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED        ELTDXBOP
01182 *          ADD 1 TO WS-CIA                                        ELTDXBOP
01183            IF TCAR-FROM-SUB = 1                                   ELTDXBOP
01184                OR (WS-BASIC-SUPP-SWITCH NOT = SPACE)              ELTDXBOP
01185               EVALUATE TRUE                                       ELTDXBOP
01186                 WHEN PROCESSING-BASIC-INFO                        ELTDXBOP
01187                   IF WS-PROCESS-POT                               ELTDXBOP
01188                     CONTINUE                                      ELTDXBOP
01189                   ELSE                                            ELTDXBOP
01190                      MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)        ELTDXBOP
01191                      ADD +1 TO WS-CIA                             ELTDXBOP
01192                   END-IF                                          ELTDXBOP
01193                  WHEN PROCESSING-SUPPLEMENTAL                     ELTDXBOP
01194                   IF WS-PROCESS-POT                               ELTDXBOP
01195                      CONTINUE                                     ELTDXBOP
01196                   ELSE                                            ELTDXBOP
01197                      MOVE WS-SUPPLEMENTAL-LIT                     ELTDXBOP
01198                         TO COF-DTL-LINE(WS-CIA)                   ELTDXBOP
01199                      ADD +1 TO WS-CIA                             ELTDXBOP
01200                   END-IF                                          ELTDXBOP
01201                  WHEN OTHER                                       ELTDXBOP
01202                      CONTINUE                                     ELTDXBOP
01203               END-EVALUATE                                        ELTDXBOP
01204 *             ADD 1 TO WS-CIA                                     ELTDXBOP
01205               MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)       ELTDXBOP
01206            ELSE                                                   ELTDXBOP
01207               ADD 1 TO WS-CIA                                     ELTDXBOP
01208               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                ELTDXBOP
01209                   COF-DTL-LINE(WS-CIA)                            ELTDXBOP
01210            END-IF                                                 ELTDXBOP
01211            ADD 1 TO TCAR-FROM-SUB                                 ELTDXBOP
01212      END-PERFORM.                                                 ELTDXBOP
01213      IF WS-PROCESS-POT OR NOT PROCESSING-BASIC-INFO               ELTDXBOP
01214         ADD 1 TO WS-CIA                                           ELTDXBOP
01215         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXBOP
01216      END-IF.                                                      ELTDXBOP
01217      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXBOP
01218      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXBOP
01219                            DFHCOMMAREA.                           ELTDXBOP
01220      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXBOP
01221                WS-CIA                                             ELTDXBOP
01222                TCAR-FROM-SUB.                                     ELTDXBOP
01223                                                                   ELTDXBOP
01224  9610-UNSTRING-TEXT.                                              ELTDXBOP
01225      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDXBOP
01226      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDXBOP
01227      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDXBOP
01228      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTDXBOP
01229      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTDXBOP
01230      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTDXBOP
01231      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTDXBOP
01232      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTDXBOP
01233      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTDXBOP
01234      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTDXBOP
01235      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTDXBOP
01236      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTDXBOP
01237      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTDXBOP
01238      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTDXBOP
01239      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTDXBOP
01240      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTDXBOP
01241      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTDXBOP
01242      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTDXBOP
01243      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTDXBOP
01244      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTDXBOP
01245      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTDXBOP
01246      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXBOP
01247                                                                   ELTDXBOP
01248 / C O M P R E S S   A N D   E X P A N D   S U B R O U T I N E S   ELTDXBOP
01249  9999-DUMMEY.                                                     ELTDXBOP
01250      COPY ELSTCOMP.                                               ELTDXBOP
01251                                                                   ELTDXBOP
01252  9999-CHECK-CONTRACT.                                             ELTDXBOP
01253      INITIALIZE WS-LOB-SWITCH.                                    ELTDXBOP
01254      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDXBOP
01255      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01256                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBOP
01257      IF CIA-RC-PTR-NULL                                           ELTDXBOP
01258         CONTINUE                                                  ELTDXBOP
01259      ELSE                                                         ELTDXBOP
01260         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBOP
01261         IF WS-BSC-LINE                                            ELTDXBOP
01262            SET WS-BASIC-LOB TO TRUE                               ELTDXBOP
01263         END-IF                                                    ELTDXBOP
01264      END-IF.                                                      ELTDXBOP
01265      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDXBOP
01266      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01267                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBOP
01268      IF CIA-RC-PTR-NULL                                           ELTDXBOP
01269         CONTINUE                                                  ELTDXBOP
01270      ELSE                                                         ELTDXBOP
01271         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBOP
01272         IF WS-BSC-LINE                                            ELTDXBOP
01273            SET WS-BASIC-LOB TO TRUE                               ELTDXBOP
01274         END-IF                                                    ELTDXBOP
01275      END-IF.                                                      ELTDXBOP
01276      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTDXBOP
01277      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01278                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBOP
01279      IF CIA-RC-PTR-NULL                                           ELTDXBOP
01280         CONTINUE                                                  ELTDXBOP
01281      ELSE                                                         ELTDXBOP
01282         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBOP
01283         IF WS-BSC-LINE                                            ELTDXBOP
01284            SET WS-BASIC-LOB TO TRUE                               ELTDXBOP
01285         END-IF                                                    ELTDXBOP
01286      END-IF.                                                      ELTDXBOP
01287      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTDXBOP
01288      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXBOP
01289                      ADDRESS OF CONTRACT-RECORD.                  ELTDXBOP
01290      IF CIA-RC-PTR-NULL                                           ELTDXBOP
01291         CONTINUE                                                  ELTDXBOP
01292      ELSE                                                         ELTDXBOP
01293         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXBOP
01294         IF WS-BSC-LINE                                            ELTDXBOP
01295            SET WS-BASIC-LOB TO TRUE                               ELTDXBOP
01296         END-IF                                                    ELTDXBOP
01297      END-IF.                                                      ELTDXBOP
01298 /   C O M P R E S S I O N  A N D  U N S T R I N G   R O U T I N E ELTDXBOP
01299  8600-ELSTCOMP.                                                   ELTDXBOP
01300 ****                                                              ELTDXBOP
01301 **** 8600-ELSTCOMP SECTION REQUIRED TO END PREV SECTION           ELTDXBOP
01302 ****                                                              ELTDXBOP
01303 *COPY ELSTCOMP.                                                   ELTDXBOP
01304 /              A B E N D                                          ELTDXBOP
01305 ******************************************************************ELTDXBOP
01306 *                        A B E N D                                ELTDXBOP
01307 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTDXBOP
01308 *                                                                 ELTDXBOP
01309 ******************************************************************ELTDXBOP
01310  9998-INVALID-PTR.                                                ELTDXBOP
01311      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTDXBOP
01312      EXEC CICS ABEND   ABCODE(CIA-ABCODE) END-EXEC.               ELTDXBOP
01313                                                                   ELTDXBOP
01314                                                                   ELTDXBOP
01315      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTDXBOP
01316                                                                   ELTDXBOP
01317      MOVE +79             TO TCAR-OUTPUT-FIELD-20-LEN.            ELTDXBOP
01318      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXBOP
01319                                                                   ELTDXBOP
