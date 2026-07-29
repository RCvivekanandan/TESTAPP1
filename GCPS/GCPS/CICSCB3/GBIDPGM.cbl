00001  IDENTIFICATION DIVISION.                                         09/29/04
00002  PROGRAM-ID.     GBIDPGM.                                         GBIDPGM 
00003  AUTHOR.         G. PEREZ.                                           LV002
00004  DATE-WRITTEN.   02/02/01.                                        GBIDPGM 
00005  DATE-COMPILED.                                                   GBIDPGM 
00006 ******************************************************************GBIDPGM 
00007 *    GBIDPGM   BENEFIT HIGHLIGHTS PROGRAM                        *GBIDPGM 
00008 *                                                                *GBIDPGM 
00009 *    THIS PROGRAM WILL LINK TO GBIFPGM WHICH WILL DETERMINE RULE *GBIDPGM 
00010 *    VALUES AND RETURN THEM IN COPYBOOK GCBENHLC.                *GBIDPGM 
00011 *    THE VALUES IN THE COPYBOOK WILL THEN BE USED TO BUILD       *GBIDPGM 
00012 *    THE TS QUEUE PAGES.                                         *GBIDPGM 
00013 *    THE FIRST TIME THROUGH, THE FIRST TS QUEUE PAGE WILL BE     *GBIDPGM 
00014 *    DISPLAY. THEN THE USER WILL DETERMINE THROUGH PF KEYS       *GBIDPGM 
00015 *    HOW TO PAGE.                                                *GBIDPGM 
00016 *                                                                *GBIDPGM 
00017 *   FUNC CODE: GBID                                              *GBIDPGM 
00018 *      MAPSET: GBIDSET                                           *GBIDPGM 
00019 * INPUT FILES: GROUP FILE                                        *GBIDPGM 
00020 *              CONTRACT FILE                                     *GBIDPGM 
00021 *              TABULAR FILE                                      *GBIDPGM 
00022 *                                                                *GBIDPGM 
00023 ******************************************************************GBIDPGM 
00024 *                                                                *GBIDPGM 
00025 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBIDPGM 
00026 *       *-*         U P D A T E   H I S T O R Y         *-*      *GBIDPGM 
00027 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBIDPGM 
00028 *                                                                *GBIDPGM 
00029 **-CHG-NUM-* *-DATE-* *WHO* *-----DESCRIPTION--------*            GBIDPGM 
00030 *                                                                *GBIDPGM 
00031 *    XXXX    02/02/01  GSP  CREATED SKELETON PROGRAM.            *GBIDPGM 
00032 *                                                                 GBIDPGM 
00033 *    XXXX    06/05/01  GSP  CHANGED ALL REFERENCES OF GBIA TO     GBIDPGM 
00034 *                           GHIL (TRANSACTION WAS RENAMED).       GBIDPGM 
00035 *                                                                 GBIDPGM 
00036 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GBIDPGM 
00037 *                                                                *GBIDPGM 
00038 *            06-06-03   MQ    CHANGE WS-RULE-4330-VALUE FROM     *GBIDPGM 
00039 *                             'OTHER COVERED SERVICES PAYMENT    *GBIDPGM 
00040 *                              LEVEL' TO 'UNSOLICITED PROVIDERS' *GBIDPGM 
00041 *                                                                *GBIDPGM 
00042 *            10-23-03   KDM   RECOMPILE FOR PRIME                *GBIDPGM 
00043 *                                                                *GBIDPGM 
00044 *            11-12-03   MQ    ADD LOGIC FOR NEW RULE VALUES      *GBIDPGM 
00045 *                                                                *GBIDPGM 
00046 *            12-03-03   MQ    ADD LOGIC FOR HOSPITAL/MEDICAL     *GBIDPGM 
00047 *                             SURGICAL PAYMENT LEVEL-RULE 4215   *GBIDPGM 
00048 *                                                                *GBIDPGM 
00049 *            03-02-04   MQ    CHANGE LOGIC IN PARA 4100- TO      *GBIDPGM 
00050 *                             EVALUATE DISCOUNT PRODUCT TYPE     *GBIDPGM 
00051 *                                                                *GBIDPGM 
00052 *            04-07-04   MQ    ADD LOGIC FOR VISION HARDWARE      *GBIDPGM 
00053 *                             BENEFIT PERIOD MAX - RULE 4398     *GBIDPGM 
00054 *                                                                *GBIDPGM 
00055 *            05-13-04   MQ    FIX SUBSTANCE ABUSE LIFETIME MAX   *GBIDPGM 
00056 *                                                                *GBIDPGM 
00057 *            11-09-04   MQ    ADD BLUE CHOICE SELECT PARA 4100-  *GBIDPGM 
00058 *                                                                *GBIDPGM 
00059 *            01-20-05   MQ    ADD NEW RULES FOR TEXAS ACCUMS     *GBIDPGM 
00060 *                                                                *GBIDPGM 
00061 *            01-24-05   MQ    ADD TX DED/OOP STAND ALONE ACCUMS  *GBIDPGM 
00062 *                                                                *GBIDPGM 
00063 *            05-04-05   MQ    ADD NEW RULE 4115 PER ADM DEDL MAX *GBIDPGM 
00064 *                                                                *GBIDPGM 
00065 *            05-11-05   MQ    ADD LOGIC FOR TX ERS/HMO ACCUMS    *GBIDPGM 
00066 *                             COMMENT OUT LOGIC FOR RULE 4930-   *GBIDPGM 
00056 *                                                                *GBIDPGM 
00056 *            08-15-24   NSK   ADDED BACK MISSING FIELDS FROM 2004*GBIDPGM 
00056 *                             TO 2005 TO FIX BBDA-5950 DEFECT TO *GBIDPGM 
00056 *                             FIELDS CORRECTLY IN GHIL SCREEN.   *GBIDPGM 
00056 *                                                                *GBIDPGM 
00057 ******************************************************************GBIDPGM 
00058 /                                                                 GBIDPGM 
00059  ENVIRONMENT DIVISION.                                            GBIDPGM 
00060  DATA DIVISION.                                                   GBIDPGM 
00061  WORKING-STORAGE SECTION.                                         GBIDPGM 
00062  77  PAN-VALET PICTURE X(24) VALUE '154GBIDPGMTS 02/02/01'.       GBIDPGM 
00063  77  PAN-DSN   PICTURE  X(44) VALUE                               GBIDPGM 
00064      'HCMSGEN.TEST.PANLIB                         '.              GBIDPGM 
00065  01  WS-DIAGNOSTICS.                                              GBIDPGM 
00066      05  WS-BEGIN                PIC X(20) VALUE                  GBIDPGM 
00067      '**GBIDPGM WS BEGIN**'.                                      GBIDPGM 
00068      05  WS-PARA-ID              PIC X(4)  VALUE 'GBID'.          GBIDPGM 
00069      05  WS-ABEND-CODE           PIC X(4)  VALUE 'GBID'.          GBIDPGM 
00070                                                                   GBIDPGM 
00071 ** WORKFIELDS AND SWITCHES **                                     GBIDPGM 
00072  01  WS-WORK-FIELDS.                                              GBIDPGM 
00073      05  WS-PROCESS-CON-GRP-SW   PIC X     VALUE SPACE.           GBIDPGM 
00074          88  WS-PROCESS-CON                VALUE 'C'.             GBIDPGM 
00075          88  WS-PROCESS-GRP                VALUE 'G'.             GBIDPGM 
00076      05  WS-HEADING-SW           PIC X     VALUE 'N'.             GBIDPGM 
00077          88  WS-HEADING-NOT-WRITTEN        VALUE 'N'.             GBIDPGM 
00078      05  WS-RULE-WRITTEN-SW      PIC X     VALUE 'N'.             GBIDPGM 
00079          88  WS-RULE-WRITTEN               VALUE 'Y'.             GBIDPGM 
00080      05  WS-GRP-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00081          88  WS-GRP-ABM-FOUND              VALUE 'Y'.             GBIDPGM 
00082      05  WS-GRP-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00083          88  WS-GRP-ACL-FOUND              VALUE 'Y'.             GBIDPGM 
00084      05  WS-GRP-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00085          88  WS-GRP-ACP-FOUND              VALUE 'Y'.             GBIDPGM 
00086      05  WS-GRP-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00087          88  WS-GRP-ADL-FOUND              VALUE 'Y'.             GBIDPGM 
00088      05  WS-GRP-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00089          88  WS-GRP-AOL-FOUND              VALUE 'Y'.             GBIDPGM 
00090      05  WS-CON-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00091          88  WS-CON-ABM-FOUND              VALUE 'Y'.             GBIDPGM 
00092      05  WS-CON-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00093          88  WS-CON-ACL-FOUND              VALUE 'Y'.             GBIDPGM 
00094      05  WS-CON-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00095          88  WS-CON-ACP-FOUND              VALUE 'Y'.             GBIDPGM 
00096      05  WS-CON-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00097          88  WS-CON-ADL-FOUND              VALUE 'Y'.             GBIDPGM 
00098      05  WS-CON-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBIDPGM 
00099          88  WS-CON-AOL-FOUND              VALUE 'Y'.             GBIDPGM 
00100      05  WS-HEX-00               PIC X.                           GBIDPGM 
00101                                                                   GBIDPGM 
00102      05  WS-VALUE-LIMIT          PIC  Z(6)9.99 VALUE ZEROS.       GBIDPGM 
00103      05  WS-VALUE-LIMIT-S REDEFINES WS-VALUE-LIMIT                GBIDPGM 
00104                                  PIC  $(6)9.99.                   GBIDPGM 
00105                                                                   GBIDPGM 
00106      05  WS-PERCENT.                                              GBIDPGM 
00107          10  FILLER           PIC X(6)  VALUE SPACES.             GBIDPGM 
00108          10  WS-PERCENT-VAL   PIC Z(2)9 VALUE ZEROS.              GBIDPGM 
00109          10  WS-PERCENT-SIGN  PIC X     VALUE '%'.                GBIDPGM 
00110                                                                   GBIDPGM 
00111      05  WS-AGE-FIELD.                                            GBIDPGM 
00112          10  FILLER           PIC X(7)  VALUE SPACES.             GBIDPGM 
00113          10  WS-AGE           PIC Z(2)9 VALUE ZEROS.              GBIDPGM 
00114                                                                   GBIDPGM 
00115                                                                   GBIDPGM 
00116      05  WS-HOLD-SECTION-LINE.                                    GBIDPGM 
00117          10  WS-HOLD-SECTION-IND  PIC X(03) VALUE SPACES.         GBIDPGM 
00118          10  FILLER               PIC X(76) VALUE SPACES.         GBIDPGM 
00119                                                                   GBIDPGM 
00120      05  WS-HOLD-WAITING-PERD-LINE.                               GBIDPGM 
00121          10  FILLER               PIC X(03) VALUE SPACES.         GBIDPGM 
00122          10  WS-HOLD-WPL-DESC     PIC X(15) VALUE                 GBIDPGM 
00123                    'WAITING PERIODS'.                             GBIDPGM 
00124          10  FILLER               PIC X(05) VALUE SPACES.         GBIDPGM 
00125          10  FILLER               PIC X(08) VALUE 'MEMBER: '.     GBIDPGM 
00126          10  WS-WPL-MEM-DAYS      PIC X(03) VALUE SPACES.         GBIDPGM 
00127          10  FILLER               PIC X(05) VALUE SPACES.         GBIDPGM 
00128          10  FILLER               PIC X(08) VALUE 'SPOUSE: '.     GBIDPGM 
00129          10  WS-WPL-SPS-DAYS      PIC X(03) VALUE SPACES.         GBIDPGM 
00130          10  FILLER               PIC X(05) VALUE SPACES.         GBIDPGM 
00131          10  FILLER               PIC X(11) VALUE 'DEPENDENT: '.  GBIDPGM 
00132          10  WS-WPL-DEP-DAYS      PIC X(03) VALUE SPACES.         GBIDPGM 
00133          10  FILLER               PIC X(10) VALUE SPACES.         GBIDPGM 
00134                                                                   GBIDPGM 
00135      05  WS-SECTION-LINE.                                         GBIDPGM 
00136       10  WS-SECTION-HDG       PIC X(43) VALUE SPACES.            GBIDPGM 
00137        88  WS-SECTION-HDG-PLAN-SUM                                GBIDPGM 
00138               VALUE 'PLAN SUMMARY                               '.GBIDPGM 
00139        88  WS-SECTION-HDG-GEN-BEN                                 GBIDPGM 
00140               VALUE 'GENERAL BENEFITS                           '.GBIDPGM 
00141        88  WS-SECTION-HDG-ADM-BEN                                 GBIDPGM 
00142               VALUE 'ADMINISTRATIVE BENEFITS                    '.GBIDPGM 
00143        88  WS-SECTION-HDG-COST-CONT                               GBIDPGM 
00144               VALUE 'COST CONTAINMENT                           '.GBIDPGM 
00145        88  WS-SECTION-HDG-MH-MAX-IP                               GBIDPGM 
00146               VALUE 'MENTAL HEALTH MAXIMUMS INPATIENT           '.GBIDPGM 
00147        88  WS-SECTION-HDG-SA-MAX-IP                               GBIDPGM 
00148               VALUE 'SUBSTANCE ABUSE MAXIMUMS INPATIENT         '.GBIDPGM 
00149        88  WS-SECTION-HDG-COMB-MAX-IP                             GBIDPGM 
00150               VALUE 'COMBINED M/H AND S/A MAXIMUMS INPATIENT    '.GBIDPGM 
00151        88  WS-SECTION-HDG-MH-MAX-OP                               GBIDPGM 
00152               VALUE 'MENTAL HEALTH MAXIMUMS OUTPATIENT          '.GBIDPGM 
00153        88  WS-SECTION-HDG-SA-MAX-OP                               GBIDPGM 
00154               VALUE 'SUBSTANCE ABUSE MAXIMUMS OUTPATIENT        '.GBIDPGM 
00155        88  WS-SECTION-HDG-COMB-MAX-OP                             GBIDPGM 
00156               VALUE 'COMBINED M/H AND S/A MAXIMUMS OUTPATIENT   '.GBIDPGM 
00157        88  WS-SECTION-HDG-MH-PMT-LVL-IP                           GBIDPGM 
00158               VALUE 'MENTAL HEALTH PAYMENT LEVEL INPATIENT      '.GBIDPGM 
00159        88  WS-SECTION-HDG-SA-PMT-LVL-IP                           GBIDPGM 
00160               VALUE 'SUBSTANCE ABUSE PAYMENT LEVEL INPATIENT    '.GBIDPGM 
00161        88  WS-SECTION-HDG-COMB-PMT-LVL-IP                         GBIDPGM 
00162               VALUE 'COMBINED M/H AND S/A PAYMENT LVL INPATIENT '.GBIDPGM 
00163        88  WS-SECTION-HDG-MH-PMT-LVL-OP                           GBIDPGM 
00164               VALUE 'MENTAL HEALTH PAYMENT LEVEL OUTPATIENT     '.GBIDPGM 
00165        88  WS-SECTION-HDG-SA-PMT-LVL-OP                           GBIDPGM 
00166               VALUE 'SUBSTANCE ABUSE PAYMENT LEVEL OUTPATIENT   '.GBIDPGM 
00167        88  WS-SECTION-HDG-COMB-PMT-LVL-OP                         GBIDPGM 
00168               VALUE 'COMBINED M/H AND S/A PAYMENT LVL OUTPATIENT'.GBIDPGM 
00180 *MQ 11/03    - CHANGE BEGIN                                       GBIDPGM 
00170        88  WS-SECTION-HDG-SA-MA-ALL-POT                           GBIDPGM 
00171               VALUE 'SUBSTANCE ABUSE MAX-ALL PLACES OF TREATMENT'.GBIDPGM 
00173        88  WS-SECTION-HDG-CMB-PMT-LVL-POT                         GBIDPGM 
00174               VALUE 'COMB M/H S/A PMT LVL-ALL PLACES OF TREATMNT'.GBIDPGM 
00176        88  WS-SECTION-HDG-MH-PMT-LVL-POT                          GBIDPGM 
00177               VALUE 'MENTAL HLTH PMT LVL-ALL PLACES OF TREATMENT'.GBIDPGM 
00179        88  WS-SECTION-HDG-SA-PMT-LVL-POT                          GBIDPGM 
00180               VALUE 'SUBST ABUSE PMT LVL-ALL PLACES OF TREATMENT'.GBIDPGM 
00189 *MQ 11/03    - CHANGE END                                         GBIDPGM 
00181 *MQ 05/04                                                         GBIDPGM 
00182        88  WS-SECTION-HDG-SA-LIFE-CONF-MX                         GBIDPGM 
00183               VALUE 'SUBSTANCE ABUSE LIFETIME CONFINEMENT MAX   '.GBIDPGM 
00193 *MQ 01/20/05 - CHANGE BEGIN                                       GBIDPGM 
00194        88  WS-SECTION-HDG-MH-BP-MX-POT                            GBIDPGM 
00195               VALUE 'MENTAL HEALTH BENEFIT PERIOD MAX - ALL POT '.GBIDPGM 
00196        88  WS-SECTION-HDG-MH-LIFE-MX-POT                          GBIDPGM 
00197               VALUE 'MENTAL HEALTH LIFETIME MAX - ALL POT       '.GBIDPGM 
00198        88  WS-SECTION-HDG-SMI-BP-MAX                              GBIDPGM 
00199               VALUE 'SERIOUS MENTAL ILLNESS BENEFIT PERIOD MAX  '.GBIDPGM 
00200 *MQ 01/20/05 - CHANGE END                                         GBIDPGM 
00184   10  FILLER               PIC X(36) VALUE SPACES.                GBIDPGM 
00185                                                                   GBIDPGM 
00186                                                                   GBIDPGM 
00187      05  WS-RULE-4020-VALUE      PIC X(39)                        GBIDPGM 
00188             VALUE 'DEDUCTIBLE PER INDIVIDUAL              '.      GBIDPGM 
00206 *MQ 01/24/05 - ADD NEW RULES: 4022, 4032, 4042,4052               GBIDPGM 
00207      05  WS-RULE-4022-VALUE      PIC X(39)                        GBIDPGM 
00208             VALUE 'INDIVIDUAL STAND ALONE DEDUCTIBLE      '.      GBIDPGM 
00209 *MQ 01/20/05 - ADD NEW RULES: 4025, 4035, 4045,4055               GBIDPGM 
00210      05  WS-RULE-4025-VALUE      PIC X(39)                        GBIDPGM 
00211             VALUE 'COMBINED INDIVIDUAL DEDUCTIBLE         '.      GBIDPGM 
00189      05  WS-RULE-4030-VALUE      PIC X(39)                        GBIDPGM 
00190             VALUE 'DEDUCTIBLE PER FAMILY                  '.      GBIDPGM 
00214      05  WS-RULE-4032-VALUE      PIC X(39)                        GBIDPGM 
00215             VALUE 'FAMILY STAND ALONE DEDUCTIBLE          '.      GBIDPGM 
00216      05  WS-RULE-4035-VALUE      PIC X(39)                        GBIDPGM 
00217             VALUE 'COMBINED FAMILY DEDUCTIBLE             '.      GBIDPGM 
00191      05  WS-RULE-4040-VALUE      PIC X(39)                        GBIDPGM 
00192             VALUE 'OUT OF POCKET PER INDIVIDUAL           '.      GBIDPGM 
00220      05  WS-RULE-4042-VALUE      PIC X(39)                        GBIDPGM 
00221             VALUE 'INDIVIDUAL STAND ALONE OUT OF POCKET   '.      GBIDPGM 
00222      05  WS-RULE-4045-VALUE      PIC X(39)                        GBIDPGM 
00223             VALUE 'COMBINED INDIVIDUAL OUT OF POCKET      '.      GBIDPGM 
00193      05  WS-RULE-4050-VALUE      PIC X(39)                        GBIDPGM 
00194             VALUE 'OUT OF POCKET PER FAMILY               '.      GBIDPGM 
00226      05  WS-RULE-4052-VALUE      PIC X(39)                        GBIDPGM 
00227             VALUE 'FAMILY STAND ALONE OUT OF POCKET       '.      GBIDPGM 
00228      05  WS-RULE-4055-VALUE      PIC X(39)                        GBIDPGM 
00229             VALUE 'COMBINED FAMILY OUT OF POCKET          '.      GBIDPGM 
00195      05  WS-RULE-4060-VALUE      PIC X(39)                        GBIDPGM 
00196             VALUE 'LIFETIME MAXIMUM                       '.      GBIDPGM 
00197      05  WS-RULE-4070-VALUE      PIC X(39)                        GBIDPGM 
00198             VALUE 'COPAY - EMERGENCY ROOM                 '.      GBIDPGM 
00234 *MQ 05/11/05 - CHANGE BEGIN                                       GBIDPGM 
00235      05  WS-RULE-4071-VALUE      PIC X(39)                        GBIDPGM 
00236             VALUE 'COPAY - AMBULANCE                      '.      GBIDPGM 
00237      05  WS-RULE-4072-VALUE      PIC X(39)                        GBIDPGM 
00238             VALUE 'COPAY - SKILLED NURSING FACILITY       '.      GBIDPGM 
00239      05  WS-RULE-4073-VALUE      PIC X(39)                        GBIDPGM 
00240             VALUE 'COPAY - OUTPATIENT PSYCH VISIT         '.      GBIDPGM 
00241      05  WS-RULE-4074-VALUE      PIC X(39)                        GBIDPGM 
00242             VALUE 'COPAY - INPATIENT PER ADMIT            '.      GBIDPGM 
00243      05  WS-RULE-4075-VALUE      PIC X(39)                        GBIDPGM 
00244             VALUE 'COPAY - HOME HEALTH VISIT              '.      GBIDPGM 
00245      05  WS-RULE-4076-VALUE      PIC X(39)                        GBIDPGM 
00246             VALUE 'ALLERGY TESTING/INJECTION/SERUM PMT LVL'.      GBIDPGM 
00247 *MQ 05/11/05 - CHANGE END                                         GBIDPGM 
00199      05  WS-RULE-4080-VALUE      PIC X(39)                        GBIDPGM 
00200             VALUE 'COPAY - OFFICE VISIT                   '.      GBIDPGM 
00201      05  WS-RULE-4085-VALUE      PIC X(39)                        GBIDPGM 
00202             VALUE 'OFFICE VISIT PAYMENT LEVEL             '.      GBIDPGM 
00203      05  WS-RULE-4086-VALUE      PIC X(39)                        GBIDPGM 
00204             VALUE 'OFFICE SURGERY PAYMENT LEVEL           '.      GBIDPGM 
00205      05  WS-RULE-4090-VALUE      PIC X(39)                        GBIDPGM 
00206             VALUE 'COPAY - WELL CARE                      '.      GBIDPGM 
00207      05  WS-RULE-4091-VALUE      PIC X(39)                        GBIDPGM 
00208             VALUE 'COPAY - SPECIALIST OFFICE VISIT        '.      GBIDPGM 
00209      05  WS-RULE-4092-VALUE      PIC X(39)                        GBIDPGM 
00210             VALUE 'COPAY - OUTPATIENT SURGERY             '.      GBIDPGM 
00211      05  WS-RULE-4093-VALUE      PIC X(39)                        GBIDPGM 
00212             VALUE 'COPAY - OUTPATIENT MENTAL/SUB ABUSE    '.      GBIDPGM 
00213      05  WS-RULE-4094-VALUE      PIC X(39)                        GBIDPGM 
00214             VALUE 'COPAY - URGENT CARE FACILITY           '.      GBIDPGM 
00215      05  WS-RULE-4095-VALUE      PIC X(39)                        GBIDPGM 
00216             VALUE 'COPAY - OUTPATIENT HOSPITAL            '.      GBIDPGM 
00217      05  WS-RULE-4096-VALUE      PIC X(39)                        GBIDPGM 
00218             VALUE 'COPAY - URGENT CARE PROFESSIONAL       '.      GBIDPGM 
00219      05  WS-RULE-4100-VALUE      PIC X(39)                        GBIDPGM 
00220             VALUE 'HOSPITAL PAYMENT LEVEL                 '.      GBIDPGM 
00221      05  WS-RULE-4110-VALUE      PIC X(39)                        GBIDPGM 
00222             VALUE 'PER ADMISSION DEDUCTIBLE               '.      GBIDPGM 
00272 *MQ 05/04/05                                                      GBIDPGM 
00273      05  WS-RULE-4115-VALUE      PIC X(39)                        GBIDPGM 
00274             VALUE 'PER ADMISSION DEDUCTIBLE MAXIMUM       '.      GBIDPGM 
00223      05  WS-RULE-4120-VALUE      PIC X(39)                        GBIDPGM 
00224             VALUE 'OUTPATIENT SURGERY HOSPITAL PMT LEVEL  '.      GBIDPGM 
00225      05  WS-RULE-4130-VALUE      PIC X(39)                        GBIDPGM 
00226             VALUE 'OUTPATIENT SURGERY PROFESSIONAL PMT LVL'.      GBIDPGM 
00227      05  WS-RULE-4135-VALUE      PIC X(39)                        GBIDPGM 
00228             VALUE 'OUTPATIENT SURGERY B/C AND B/S PMT LVL'.       GBIDPGM 
00229      05  WS-RULE-4140-VALUE      PIC X(39)                        GBIDPGM 
00230             VALUE 'OUTPATIENT DIAGNOSTIC HOSPITAL PMT LVL '.      GBIDPGM 
00231      05  WS-RULE-4150-VALUE      PIC X(39)                        GBIDPGM 
00232             VALUE 'OUTPATIENT DIAGNOSTIC PROF PAYMENT LVL '.      GBIDPGM 
00233      05  WS-RULE-4160-VALUE      PIC X(39)                        GBIDPGM 
00234             VALUE 'EMERGENCY ACCIDENT CARE HOSP PMT LEVEL '.      GBIDPGM 
00235      05  WS-RULE-4170-VALUE      PIC X(39)                        GBIDPGM 
00236             VALUE 'EMERGENCY ACCIDENT CARE PROF PMT LEVEL '.      GBIDPGM 
00237      05  WS-RULE-4175-VALUE      PIC X(39)                        GBIDPGM 
00238             VALUE 'EAC B/C AND B/S PAYMENT LEVEL          '.      GBIDPGM 
00239      05  WS-RULE-4180-VALUE      PIC X(39)                        GBIDPGM 
00240             VALUE 'EMERGENCY MEDICAL  CARE HOSP PMT LEVEL '.      GBIDPGM 
00241      05  WS-RULE-4190-VALUE      PIC X(39)                        GBIDPGM 
00242             VALUE 'EMERGENCY MEDICAL  CARE PROF PMT LEVEL '.      GBIDPGM 
00243      05  WS-RULE-4195-VALUE      PIC X(39)                        GBIDPGM 
00244             VALUE 'EMC B/C AND B/S PAYMENT LEVEL          '.      GBIDPGM 
00245      05  WS-RULE-4196-VALUE      PIC X(39)                        GBIDPGM 
00246             VALUE 'EAC/EMC B/C PAYMENT LEVEL              '.      GBIDPGM 
00247      05  WS-RULE-4197-VALUE      PIC X(39)                        GBIDPGM 
00248             VALUE 'EAC/EMC B/S PAYMENT LEVEL              '.      GBIDPGM 
00249      05  WS-RULE-4198-VALUE      PIC X(39)                        GBIDPGM 
00250             VALUE 'EAC/EMC B/C AND B/S PAYMENT LEVEL      '.      GBIDPGM 
00251      05  WS-RULE-4200-VALUE      PIC X(39)                        GBIDPGM 
00252             VALUE 'SUPPLEMENTAL ACCIDENT CARE             '.      GBIDPGM 
00253      05  WS-RULE-4210-VALUE      PIC X(39)                        GBIDPGM 
00254             VALUE 'MEDICAL SURGICAL PAYMENT LEVEL         '.      GBIDPGM 
00255 *MQ 12/03/03                                                      GBIDPGM 
00256      05  WS-RULE-4215-VALUE      PIC X(39)                        GBIDPGM 
00257             VALUE 'HOSPITAL/MEDICAL SURGICAL PAYMENT LEVEL'.      GBIDPGM 
00258      05  WS-RULE-4220-VALUE      PIC X(39)                        GBIDPGM 
00259             VALUE 'THERAPY MAXIMUM COMBINED (PT, OT, ST)  '.      GBIDPGM 
00260      05  WS-RULE-4230-VALUE      PIC X(39)                        GBIDPGM 
00261             VALUE 'FUNCTIONAL OCCUPATIONAL THERAPY MAXIMUM'.      GBIDPGM 
00262      05  WS-RULE-4240-VALUE      PIC X(39)                        GBIDPGM 
00263             VALUE 'PHYSICAL / MECHANO THERAPY MAXIMUM     '.      GBIDPGM 
00264      05  WS-RULE-4250-VALUE      PIC X(39)                        GBIDPGM 
00265             VALUE 'SPEECH THERAPY MAXIMUM                 '.      GBIDPGM 
00266      05  WS-RULE-4260-VALUE      PIC X(39)                        GBIDPGM 
00267             VALUE 'TMJ LIFETIME MAXIMUM                   '.      GBIDPGM 
00268      05  WS-RULE-4270-VALUE      PIC X(39)                        GBIDPGM 
00269             VALUE 'PRIVATE DUTY NURSING MAXIMUM           '.      GBIDPGM 
00270      05  WS-RULE-4275-VALUE      PIC X(39)                        GBIDPGM 
00271             VALUE 'SKILLED NURSING BENEFIT PERIOD MAXIMUM '.      GBIDPGM 
00272      05  WS-RULE-4280-VALUE      PIC X(39)                        GBIDPGM 
00273             VALUE 'CHIROPRACTIC SERVICES MAXIMUM          '.      GBIDPGM 
00274      05  WS-RULE-4290-VALUE      PIC X(39)                        GBIDPGM 
00275             VALUE 'CHIROPRACTIC PROVIDER MAXIMUM          '.      GBIDPGM 
00276      05  WS-RULE-4300-VALUE      PIC X(39)                        GBIDPGM 
00277             VALUE 'WELL CARE PAYMENT LEVEL                '.      GBIDPGM 
00278      05  WS-RULE-4305-VALUE      PIC X(39)                        GBIDPGM 
00279             VALUE 'WELL ADULT CARE PAYMENT LEVEL          '.      GBIDPGM 
00280      05  WS-RULE-4310-VALUE      PIC X(39)                        GBIDPGM 
00281             VALUE 'WELL CARE MAXIMUM                      '.      GBIDPGM 
00282      05  WS-RULE-4315-VALUE      PIC X(39)                        GBIDPGM 
00283             VALUE 'WELL ADULT CARE BENEFIT PERIOD MAXIMUM '.      GBIDPGM 
00284      05  WS-RULE-4320-VALUE      PIC X(39)                        GBIDPGM 
00285             VALUE 'WELL CHILD CARE MAXIMUM                '.      GBIDPGM 
00286      05  WS-RULE-4325-VALUE      PIC X(39)                        GBIDPGM 
00287             VALUE 'WELL CHILD CARE PAYMENT LEVEL          '.      GBIDPGM 
00288      05  WS-RULE-4330-VALUE      PIC X(39)                        GBIDPGM 
00289             VALUE 'UNSOLICITED PROVIDERS PAYMENT LEVEL    '.      GBIDPGM 
00290      05  WS-RULE-4340-VALUE      PIC X(39)                        GBIDPGM 
00291             VALUE 'MSA SANCTION COINSURANCE               '.      GBIDPGM 
00292      05  WS-RULE-4350-VALUE      PIC X(39)                        GBIDPGM 
00293             VALUE 'MSA SANCTION DEDUCTIBLE                '.      GBIDPGM 
00294      05  WS-RULE-4360-VALUE      PIC X(39)                        GBIDPGM 
00295             VALUE 'IP MENTAL/SUBSTANCE ABUSE PMT LEVEL    '.      GBIDPGM 
00296      05  WS-RULE-4370-VALUE      PIC X(39)                        GBIDPGM 
00297             VALUE 'HEARING AID BENEFIT PERIOD MAXIMUM     '.      GBIDPGM 
00298      05  WS-RULE-4380-VALUE      PIC X(39)                        GBIDPGM 
00299             VALUE 'NON PLAN PAYMENT LEVEL                 '.      GBIDPGM 
00300      05  WS-RULE-4390-VALUE      PIC X(39)                        GBIDPGM 
00301             VALUE 'CONTACT LENSES BENEFIT PERIOD MAXIMUM  '.      GBIDPGM 
00302      05  WS-RULE-4392-VALUE      PIC X(39)                        GBIDPGM 
00303             VALUE 'FRAMES BENEFIT PERIOD MAXIMUM          '.      GBIDPGM 
00304      05  WS-RULE-4394-VALUE      PIC X(39)                        GBIDPGM 
00305             VALUE 'VISION EXAM BENEFIT PERIOD MAXIMUM     '.      GBIDPGM 
00306      05  WS-RULE-4396-VALUE      PIC X(39)                        GBIDPGM 
00307             VALUE 'LENSES BENEFIT PERIOD MAXIMUM          '.      GBIDPGM 
00308      05  WS-RULE-4398-VALUE      PIC X(39)                        GBIDPGM 
00309             VALUE 'VISION HARDWARE BENEFIT PERIOD MAXIMUM '.      GBIDPGM 
00310      05  WS-RULE-4400-VALUE      PIC X(39)                        GBIDPGM 
00311             VALUE 'IP MENTAL HEALTH LIFETIME MAXIMUM      '.      GBIDPGM 
00312      05  WS-RULE-4410-VALUE      PIC X(39)                        GBIDPGM 
00313             VALUE 'IP SUBSTANCE ABUSE BENEFIT PERIOD MAX  '.      GBIDPGM 
00314      05  WS-RULE-4420-VALUE      PIC X(39)                        GBIDPGM 
00315             VALUE 'IP SUBSTANCE ABUSE LIFETIME MAXIMUM    '.      GBIDPGM 
00316      05  WS-RULE-4440-VALUE      PIC X(39)                        GBIDPGM 
00317             VALUE 'OP MENTAL/SUBSTANCE BENEFIT PERIOD MAX '.      GBIDPGM 
00318      05  WS-RULE-4460-VALUE      PIC X(39)                        GBIDPGM 
00319             VALUE 'OP MENTAL HEALTH BENEFIT PERIOD MAXIMUM'.      GBIDPGM 
00320      05  WS-RULE-4470-VALUE      PIC X(39)                        GBIDPGM 
00321             VALUE 'OP MENTAL HEALTH LIFETIME MAXIMUM      '.      GBIDPGM 
00322      05  WS-RULE-4490-VALUE      PIC X(39)                        GBIDPGM 
00323             VALUE 'OP SUBSTANCE ABUSE LIFETIME MAXIMUM    '.      GBIDPGM 
00324      05  WS-RULE-4335-VALUE      PIC X(39)                        GBIDPGM 
00325             VALUE 'DEPENDENT AGE                          '.      GBIDPGM 
00326      05  WS-RULE-4336-VALUE      PIC X(39)                        GBIDPGM 
00327             VALUE 'STUDENT AGE                            '.      GBIDPGM 
00328      05  WS-RULE-4337-VALUE      PIC X(39)                        GBIDPGM 
00329             VALUE 'WAITING PERIODS                        '.      GBIDPGM 
00330      05  WS-RULE-4500-VALUE      PIC X(39)                        GBIDPGM 
00331             VALUE 'B/C MENTAL / SUBSTANCE ABUSE PMT LEVEL '.      GBIDPGM 
00332      05  WS-RULE-4510-VALUE      PIC X(39)                        GBIDPGM 
00333             VALUE 'B/S MENTAL / SUBSTANCE ABUSE PMT LEVEL '.      GBIDPGM 
00334      05  WS-RULE-4520-VALUE      PIC X(39)                        GBIDPGM 
00335             VALUE 'B/C AND B/S MENTAL/SUB ABUSE PMT LEVEL '.      GBIDPGM 
00336      05  WS-RULE-4530-VALUE      PIC X(39)                        GBIDPGM 
00337             VALUE 'B/C MENTAL AND S/A COMBINED BP MAXIMUM '.      GBIDPGM 
00338      05  WS-RULE-4540-VALUE      PIC X(39)                        GBIDPGM 
00339             VALUE 'B/S MENTAL AND S/A COMBINED BP MAXIMUM '.      GBIDPGM 
00340      05  WS-RULE-4550-VALUE      PIC X(39)                        GBIDPGM 
00341             VALUE 'B/C AND B/S MENTAL S/A COMBINED BP MAX '.      GBIDPGM 
00342      05  WS-RULE-4560-VALUE      PIC X(39)                        GBIDPGM 
00343             VALUE 'B/C MENTAL/SUBSTANCE ABUSE LIFETIME MAX'.      GBIDPGM 
00344      05  WS-RULE-4570-VALUE      PIC X(39)                        GBIDPGM 
00345             VALUE 'B/S MENTAL/SUBSTANCE ABUSE LIFETIME MAX'.      GBIDPGM 
00346      05  WS-RULE-4580-VALUE      PIC X(39)                        GBIDPGM 
00347             VALUE 'B/S AND B/S MENTAL / SA LIFETIME MAX   '.      GBIDPGM 
00348      05  WS-RULE-4590-VALUE      PIC X(39)                        GBIDPGM 
00349             VALUE 'B/C MENTAL HEALTH PAYMENT LEVEL        '.      GBIDPGM 
00350      05  WS-RULE-4600-VALUE      PIC X(39)                        GBIDPGM 
00351             VALUE 'B/S MENTAL HEALTH PAYMENT LEVEL        '.      GBIDPGM 
00352      05  WS-RULE-4610-VALUE      PIC X(39)                        GBIDPGM 
00353             VALUE 'B/C AND B/S MENTAL HEALTH PAYMENT LEVEL'.      GBIDPGM 
00354      05  WS-RULE-4620-VALUE      PIC X(39)                        GBIDPGM 
00355             VALUE 'B/C MENTAL HEALTH BENEFIT PERIOD MAX   '.      GBIDPGM 
00408 *MQ 01/20/05 - ADD NEW RULES: 4625, 4635, 4645,4655, 4665, 4675   GBIDPGM 
00409      05  WS-RULE-4625-VALUE      PIC X(39)                        GBIDPGM 
00410             VALUE 'B/C MENTAL HEALTH BEN PER MAX-ALL POT  '.      GBIDPGM 
00356      05  WS-RULE-4630-VALUE      PIC X(39)                        GBIDPGM 
00357             VALUE 'B/S MENTAL HEALTH BENEFIT PERIOD MAX   '.      GBIDPGM 
00413      05  WS-RULE-4635-VALUE      PIC X(39)                        GBIDPGM 
00414             VALUE 'B/S MENTAL HEALTH BEN PER MAX-ALL POT  '.      GBIDPGM 
00358      05  WS-RULE-4640-VALUE      PIC X(39)                        GBIDPGM 
00359             VALUE 'B/C AND B/S MENTAL HEALTH BEN PERD MAX '.      GBIDPGM 
00417      05  WS-RULE-4645-VALUE      PIC X(39)                        GBIDPGM 
00418             VALUE 'B/C & B/S MENTAL HLTH BENPER MAX-ALLPOT'.      GBIDPGM 
00360      05  WS-RULE-4650-VALUE      PIC X(39)                        GBIDPGM 
00361             VALUE 'B/C MENTAL HEALTH LIFETIME MAXIMUM     '.      GBIDPGM 
00421      05  WS-RULE-4655-VALUE      PIC X(39)                        GBIDPGM 
00422             VALUE 'B/C MENTAL HEALTH LIFETIME MAX-ALL POT '.      GBIDPGM 
00362      05  WS-RULE-4660-VALUE      PIC X(39)                        GBIDPGM 
00363             VALUE 'B/S MENTAL HEALTH LIFETIME MAXIMUM     '.      GBIDPGM 
00425      05  WS-RULE-4665-VALUE      PIC X(39)                        GBIDPGM 
00426             VALUE 'B/S MENTAL HEALTH LIFETIME MAX-ALL POT '.      GBIDPGM 
00364      05  WS-RULE-4670-VALUE      PIC X(39)                        GBIDPGM 
00365             VALUE 'B/C AND B/S MENTAL HEALTH LIFETIME MAX '.      GBIDPGM 
00429      05  WS-RULE-4675-VALUE      PIC X(39)                        GBIDPGM 
00430             VALUE 'B/C & B/S MENTAL HLTH LIFE MAX-ALL POT '.      GBIDPGM 
00366      05  WS-RULE-4680-VALUE      PIC X(39)                        GBIDPGM 
00367             VALUE 'B/C SUBSTANCE ABUSE PAYMENT LEVEL      '.      GBIDPGM 
00368      05  WS-RULE-4690-VALUE      PIC X(39)                        GBIDPGM 
00369             VALUE 'B/S SUBSTANCE ABUSE PAYMENT LEVEL      '.      GBIDPGM 
00370      05  WS-RULE-4700-VALUE      PIC X(39)                        GBIDPGM 
00371             VALUE 'B/C AND B/S SUBSTANCE ABUSE PMT LEVEL  '.      GBIDPGM 
00372      05  WS-RULE-4710-VALUE      PIC X(39)                        GBIDPGM 
00373             VALUE 'B/C SUBSTANCE ABUSE BENEFIT PERIOD MAX '.      GBIDPGM 
00374      05  WS-RULE-4715-VALUE      PIC X(39)                        GBIDPGM 
00375             VALUE 'B/C SUB/ABUSE BENEFIT PER MAX-ALL POT  '.      GBIDPGM 
00376      05  WS-RULE-4720-VALUE      PIC X(39)                        GBIDPGM 
00377             VALUE 'B/S SUBSTANCE ABUSE BENEFIT PERIOD MAX '.      GBIDPGM 
00378      05  WS-RULE-4725-VALUE      PIC X(39)                        GBIDPGM 
00379             VALUE 'B/S SUB/ABUSE BENEFIT PER MAX-ALL POT  '.      GBIDPGM 
00380      05  WS-RULE-4730-VALUE      PIC X(39)                        GBIDPGM 
00381             VALUE 'B/C AND B/S SUBSTANCE ABUSE BP MAXIMUM '.      GBIDPGM 
00382      05  WS-RULE-4735-VALUE      PIC X(39)                        GBIDPGM 
00383             VALUE 'B/C AND B/S SUB/ABUSE BP MAX-ALL POT   '.      GBIDPGM 
00384      05  WS-RULE-4740-VALUE      PIC X(39)                        GBIDPGM 
00385             VALUE 'B/C SUBSTANCE ABUSE LIFETIME MAXIMUM-IP'.      GBIDPGM 
00386      05  WS-RULE-4745-VALUE      PIC X(39)                        GBIDPGM 
00387             VALUE 'B/C SUB/ABUSE LIFETIME MAX-ALL POT     '.      GBIDPGM 
00388      05  WS-RULE-4750-VALUE      PIC X(39)                        GBIDPGM 
00389             VALUE 'B/S SUBSTANCE ABUSE LIFETIME MAXIMUM-IP'.      GBIDPGM 
00390      05  WS-RULE-4755-VALUE      PIC X(39)                        GBIDPGM 
00391             VALUE 'B/S SUB/ABUSE LIFETIME MAXIMUM-ALL POT '.      GBIDPGM 
00392      05  WS-RULE-4760-VALUE      PIC X(39)                        GBIDPGM 
00393             VALUE 'B/C AND B/S SUBSTANCEABUSE LIFETIME MAX'.      GBIDPGM 
00394      05  WS-RULE-4762-VALUE      PIC X(39)                        GBIDPGM 
00395             VALUE 'SUBSTANCE ABUSE LIFETIME CONFINE MAX   '.      GBIDPGM 
00396      05  WS-RULE-4765-VALUE      PIC X(39)                        GBIDPGM 
00397             VALUE 'B/C AND B/S SUB ABUSE LIFE MAX-ALL POT '.      GBIDPGM 
00398      05  WS-RULE-4770-VALUE      PIC X(39)                        GBIDPGM 
00399             VALUE 'B/C MENTAL / SUBSTANCE PAYMENT LEVEL   '.      GBIDPGM 
00400      05  WS-RULE-4775-VALUE      PIC X(39)                        GBIDPGM 
00401             VALUE 'B/C MENTAL /SUBSTANCE PMT LEVEL-ALL POT'.      GBIDPGM 
00402      05  WS-RULE-4780-VALUE      PIC X(39)                        GBIDPGM 
00403             VALUE 'B/S MENTAL / SUBSTANCE PAYMENT LEVEL   '.      GBIDPGM 
00404      05  WS-RULE-4785-VALUE      PIC X(39)                        GBIDPGM 
00405             VALUE 'B/S MENTAL /SUBSTANCE PMT LEVEL-ALL POT'.      GBIDPGM 
00406      05  WS-RULE-4790-VALUE      PIC X(39)                        GBIDPGM 
00407             VALUE 'B/C AND B/S MENTAL / SA PAYMENT LEVEL  '.      GBIDPGM 
00408      05  WS-RULE-4795-VALUE      PIC X(39)                        GBIDPGM 
00409             VALUE 'B/C AND B/S MENTAL/SA PMT LEVEL-ALL POT'.      GBIDPGM 
00410      05  WS-RULE-4800-VALUE      PIC X(39)                        GBIDPGM 
00411             VALUE 'B/C MENTAL / SA BENEFIT PERIOD MAXIMUM '.      GBIDPGM 
00412      05  WS-RULE-4810-VALUE      PIC X(39)                        GBIDPGM 
00413             VALUE 'B/S MENTAL / SA BENEFIT PERIOD MAXIMUM '.      GBIDPGM 
00414      05  WS-RULE-4820-VALUE      PIC X(39)                        GBIDPGM 
00415             VALUE 'B/C AND B/S MENTAL/SA BENEFIT PERD MAX '.      GBIDPGM 
00416      05  WS-RULE-4822-VALUE      PIC X(39)                        GBIDPGM 
00417             VALUE 'BCBS OP MH/SA BENEFIT PERIOD MAX-CHILD '.      GBIDPGM 
00418      05  WS-RULE-4825-VALUE      PIC X(39)                        GBIDPGM 
00419             VALUE 'BCBS OP MH/SA BENEFIT PERIOD MAX-ADULT '.      GBIDPGM 
00420      05  WS-RULE-4830-VALUE      PIC X(39)                        GBIDPGM 
00421             VALUE 'B/C MENTAL / SA LIFETIME MAXIMUM       '.      GBIDPGM 
00422      05  WS-RULE-4840-VALUE      PIC X(39)                        GBIDPGM 
00423             VALUE 'B/S MENTAL / SA LIFETIME MAXIMUM       '.      GBIDPGM 
00424      05  WS-RULE-4850-VALUE      PIC X(39)                        GBIDPGM 
00425             VALUE 'B/C AND B/S MENTAL / SA LIFETIME MAX   '.      GBIDPGM 
00426      05  WS-RULE-4860-VALUE      PIC X(39)                        GBIDPGM 
00427             VALUE 'B/C MENTAL HEALTH PAYMENT LEVEL        '.      GBIDPGM 
00428      05  WS-RULE-4865-VALUE      PIC X(39)                        GBIDPGM 
00429             VALUE 'B/C MENTAL HEALTH PAYMENT LEVEL-ALL POT'.      GBIDPGM 
00430      05  WS-RULE-4870-VALUE      PIC X(39)                        GBIDPGM 
00431             VALUE 'B/S MENTAL HEALTH PAYMENT LEVEL        '.      GBIDPGM 
00432      05  WS-RULE-4875-VALUE      PIC X(39)                        GBIDPGM 
00433             VALUE 'B/S MENTAL HEALTH PAYMENT LEVEL-ALL POT'.      GBIDPGM 
00434      05  WS-RULE-4880-VALUE      PIC X(39)                        GBIDPGM 
00435             VALUE 'B/C AND B/S MENTAL HEALTH PAYMENT LEVEL'.      GBIDPGM 
00436      05  WS-RULE-4885-VALUE      PIC X(39)                        GBIDPGM 
00437             VALUE 'B/C AND B/S MENTAL PAYMENT LVL-ALL POT '.      GBIDPGM 
00438      05  WS-RULE-4890-VALUE      PIC X(39)                        GBIDPGM 
00439             VALUE 'B/C MENTAL HEALTH BENEFIT PERIOD MAX   '.      GBIDPGM 
00440      05  WS-RULE-4900-VALUE      PIC X(39)                        GBIDPGM 
00441             VALUE 'B/S MENTAL HEALTH BENEFIT PERIOD MAX   '.      GBIDPGM 
00442      05  WS-RULE-4910-VALUE      PIC X(39)                        GBIDPGM 
00443             VALUE 'B/C AND B/S MENTAL HEALTH BEN PERD MAX '.      GBIDPGM 
00444      05  WS-RULE-4911-VALUE      PIC X(39)                        GBIDPGM 
00445             VALUE 'BCBS OP MENTAL HEALTH BEN PER MAX-CHILD'.      GBIDPGM 
00446      05  WS-RULE-4915-VALUE      PIC X(39)                        GBIDPGM 
00447             VALUE 'BCBS OP MENTAL HEALTH BEN PER MAX-ADULT'.      GBIDPGM 
00448      05  WS-RULE-4920-VALUE      PIC X(39)                        GBIDPGM 
00449             VALUE 'B/C MENTAL HEALTH LIFETIME MAXIMUM     '.      GBIDPGM 
00515 *    05  WS-RULE-4930-VALUE      PIC X(39)                        GBIDPGM 
00516 *           VALUE 'B/S MENTAL HEALTH LIFETIME MAXIMUM     '.      GBIDPGM 
00452      05  WS-RULE-4940-VALUE      PIC X(39)                        GBIDPGM 
00453             VALUE 'B/C AND B/S MENTAL HEALTH LIFETIME MAX '.      GBIDPGM 
00454      05  WS-RULE-4950-VALUE      PIC X(39)                        GBIDPGM 
00455             VALUE 'B/C SUBSTANCE ABUSE PAYMENT LEVEL      '.      GBIDPGM 
00456      05  WS-RULE-4955-VALUE      PIC X(39)                        GBIDPGM 
00457             VALUE 'B/C SUBSTANCE ABUSE PAYMENT LVL-ALL POT'.      GBIDPGM 
00458      05  WS-RULE-4960-VALUE      PIC X(39)                        GBIDPGM 
00459             VALUE 'B/S SUBSTANCE ABUSE PAYMENT LEVEL      '.      GBIDPGM 
00460      05  WS-RULE-4965-VALUE      PIC X(39)                        GBIDPGM 
00461             VALUE 'B/S SUBSTANCE ABUSE PAYMENT LVL-ALL POT'.      GBIDPGM 
00462      05  WS-RULE-4970-VALUE      PIC X(39)                        GBIDPGM 
00463             VALUE 'B/C AND B/S SUBSTANCE ABUSE PAYMENT LVL'.      GBIDPGM 
00464      05  WS-RULE-4975-VALUE      PIC X(39)                        GBIDPGM 
00465             VALUE 'BCBS SUBSTANCE ABUSE PMT LVL-ALL POT   '.      GBIDPGM 
00466      05  WS-RULE-4980-VALUE      PIC X(39)                        GBIDPGM 
00467             VALUE 'B/C SUBSTANCE ABUSE BENEFIT PERIOD MAX '.      GBIDPGM 
00468      05  WS-RULE-4990-VALUE      PIC X(39)                        GBIDPGM 
00469             VALUE 'B/S SUBSTANCE ABUSE BENEFIT PERIOD MAX '.      GBIDPGM 
00470      05  WS-RULE-5000-VALUE      PIC X(39)                        GBIDPGM 
00471             VALUE 'B/C AND B/S SUBSTANCE ABUSE BP PERD MAX'.      GBIDPGM 
00472      05  WS-RULE-5002-VALUE      PIC X(39)                        GBIDPGM 
00473             VALUE 'BCBS OP SUB ABUSE BEN PER MAX-CHILD    '.      GBIDPGM 
00474      05  WS-RULE-5005-VALUE      PIC X(39)                        GBIDPGM 
00475             VALUE 'BCBS OP SUB ABUSE BEN PER MAX-ADULT    '.      GBIDPGM 
00476      05  WS-RULE-5010-VALUE      PIC X(39)                        GBIDPGM 
00477             VALUE 'B/C SUBSTANCE ABUSE LIFETIME MAXIMUM   '.      GBIDPGM 
00478      05  WS-RULE-5020-VALUE      PIC X(39)                        GBIDPGM 
00479             VALUE 'B/S SUBSTANCE ABUSE LIFETIME MAXIMUM   '.      GBIDPGM 
00480      05  WS-RULE-5030-VALUE      PIC X(39)                        GBIDPGM 
00481             VALUE 'B/C AND B/S SUBSTANCE ABUSE LIFE MAX   '.      GBIDPGM 
00482      05  WS-RULE-5040-VALUE      PIC X(39)                        GBIDPGM 
00483             VALUE 'HOSPICE CARE BENEFIT PERIOD MAXIMUM    '.      GBIDPGM 
00484      05  WS-RULE-5050-VALUE      PIC X(39)                        GBIDPGM 
00485             VALUE 'COORDINATED HOME CARE BEN PER MAXIMUM  '.      GBIDPGM 
00551 *MQ 05/11/05                                                      GBIDPGM 
00552      05  WS-RULE-5052-VALUE      PIC X(39)                        GBIDPGM 
00553             VALUE 'COORDINATED HOME CARE BEN DAY MAXIMUM  '.      GBIDPGM 
00554 *MQ 01/20/05 - ADD NEW RULES: 5060, 5070                          GBIDPGM 
00555      05  WS-RULE-5060-VALUE      PIC X(39)                        GBIDPGM 
00556             VALUE 'SERIOUS MENTAL ILLNESS BEN PER MAX- IP '.      GBIDPGM 
00557      05  WS-RULE-5070-VALUE      PIC X(39)                        GBIDPGM 
00558             VALUE 'SERIOUS MENTAL ILLNESS BEN PER MAX- OP '.      GBIDPGM 
00486                                                                   GBIDPGM 
00487 /                                                                 GBIDPGM 
00488  01  WS-TEMP-STORAGE.                                             GBIDPGM 
00489      05  WS-TS-LINE OCCURS 19 TIMES.                              GBIDPGM 
00490          10  WS-TS-VALUE                PIC X(79) VALUE SPACES.   GBIDPGM 
00491                                                                   GBIDPGM 
00492  01  WS-TEMP-STORAGE-P.                                           GBIDPGM 
00493      05  WS-TS-LST-PAGE                 PIC 9(01) VALUE ZEROS.    GBIDPGM 
00494      05  WS-TS-TOT-PAGE                 PIC 9(01) VALUE ZEROS.    GBIDPGM 
00495      05  WS-TS-SUBSCRIBER               PIC X(12) VALUE SPACES.   GBIDPGM 
00496      05  WS-TS-GROUP                    PIC X(09) VALUE SPACES.   GBIDPGM 
00497      05  WS-TS-SECTION                  PIC X(05) VALUE SPACES.   GBIDPGM 
00498      05  WS-TS-EFFDT                    PIC X(06) VALUE SPACES.   GBIDPGM 
00499      05  WS-TS-PRGRAM                   PIC X(30) VALUE SPACES.   GBIDPGM 
00500                                                                   GBIDPGM 
00501 /                                                                 GBIDPGM 
00502  01  WS-TS-WORK-FIELDS.                                           GBIDPGM 
00503      05  WS-TS-LINE-SUB          PIC 9(03) VALUE ZEROS.           GBIDPGM 
00504      05  WS-TS-LINE-SUB-X REDEFINES WS-TS-LINE-SUB                GBIDPGM 
00505                                  PIC  X(03).                      GBIDPGM 
00506                                                                   GBIDPGM 
00507      05  WS-TS-QNAME.                                             GBIDPGM 
00508          10 FILLER               PIC X(04)  VALUE 'GBID'.         GBIDPGM 
00509          10 WS-TS-TRM            PIC X(04)  VALUE 'XXXX'.         GBIDPGM 
00510      05  WS-TS-QITEM             PIC S9(04) COMP VALUE ZERO.      GBIDPGM 
00511      05  WS-TS-NUMITEMS          PIC S9(04) COMP VALUE ZERO.      GBIDPGM 
00512      05  WS-TS-QLENGTH           PIC S9(04) COMP VALUE +1501.     GBIDPGM 
00513      05  WS-TS-QLENGTH-P         PIC S9(04) COMP VALUE +64.       GBIDPGM 
00514      05  WS-TS-LAST-PAGE         PIC S9(04) COMP VALUE +1.        GBIDPGM 
00515                                                                   GBIDPGM 
00516      05  WS-PAGE-NUM             PIC  9 VALUE ZERO.               GBIDPGM 
00517      05  WS-PAGE-NUM-X         REDEFINES                          GBIDPGM 
00518          WS-PAGE-NUM             PIC  X.                          GBIDPGM 
00519                                                                   GBIDPGM 
00520  01  WS-TO-PAGE-AREA.                                             GBIDPGM 
00521      05  WS-TPAGE-NUM            PIC  9 VALUE ZERO.               GBIDPGM 
00522      05  WS-TPAGE-NUM-X         REDEFINES                         GBIDPGM 
00523          WS-TPAGE-NUM            PIC  X.                          GBIDPGM 
00524                                                                   GBIDPGM 
00525  01  WS-01-ABEND-AREA.                                            GBIDPGM 
00526      05  FILLER                   PIC X(16)  VALUE                GBIDPGM 
00527          '** ABEND AREA **'.                                      GBIDPGM 
00528                                                                   GBIDPGM 
00529      05  WS-01-ABEND-CODES-AND-MSG.                               GBIDPGM 
00530          10  FILLER      PIC X(11)  VALUE  'ABEND-CODE='.         GBIDPGM 
00531          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. GBIDPGM 
00532          10  FILLER      PIC X(10)  VALUE  'ABEND-MSG='.          GBIDPGM 
00533          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. GBIDPGM 
00534          10  FILLER      PIC X(12)  VALUE  'ABEND-MSG-2='.        GBIDPGM 
00535          10  WS-01-ABCODE-MSG-2         PIC X(100) VALUE  SPACES. GBIDPGM 
00536          10  FILLER      PIC X(20)  VALUE  '** ABEND AREA END **'.GBIDPGM 
00537                                                                   GBIDPGM 
00538          10  WS-01-ABCODE-1BS1          PIC X(04)  VALUE  '1BS1'. GBIDPGM 
00539          10  WS-01-ABCODE-1BS1-MSG      PIC X(44)  VALUE          GBIDPGM 
00540             'READQ (INVALID REQUEST)                  '.          GBIDPGM 
00541                                                                   GBIDPGM 
00542          10  WS-01-ABCODE-1BS2          PIC X(04)  VALUE  '1BS2'. GBIDPGM 
00543          10  WS-01-ABCODE-1BS2-MSG      PIC X(44)  VALUE          GBIDPGM 
00544             'READQ (I/O ERROR)                        '.          GBIDPGM 
00545                                                                   GBIDPGM 
00546          10  WS-01-ABCODE-1BS3          PIC X(04)  VALUE  '1BS3'. GBIDPGM 
00547          10  WS-01-ABCODE-1BS3-MSG      PIC X(44)  VALUE          GBIDPGM 
00548             'READQ (LENGTH ERROR)                     '.          GBIDPGM 
00549                                                                   GBIDPGM 
00550          10  WS-01-ABCODE-1BS4          PIC X(04)  VALUE  '1BS4'. GBIDPGM 
00551          10  WS-01-ABCODE-1BS4-MSG      PIC X(44)  VALUE          GBIDPGM 
00552             'READQ (QUEUE ID ERROR)                   '.          GBIDPGM 
00553                                                                   GBIDPGM 
00554          10  WS-01-ABCODE-1BS5          PIC X(04)  VALUE  '1BS5'. GBIDPGM 
00555          10  WS-01-ABCODE-1BS5-MSG      PIC X(44)  VALUE          GBIDPGM 
00556             'WRITEQ (INVALID REQUEST)                 '.          GBIDPGM 
00557                                                                   GBIDPGM 
00558          10  WS-01-ABCODE-1BS6          PIC X(04)  VALUE  '1BS6'. GBIDPGM 
00559          10  WS-01-ABCODE-1BS6-MSG      PIC X(44)  VALUE          GBIDPGM 
00560             'WRITEQ (I/O ERROR)                       '.          GBIDPGM 
00561                                                                   GBIDPGM 
00562          10  WS-01-ABCODE-1BS7          PIC X(04)  VALUE  '1BS7'. GBIDPGM 
00563          10  WS-01-ABCODE-1BS7-MSG      PIC X(44)  VALUE          GBIDPGM 
00564             'WRITEQ (NO SPACE)                        '.          GBIDPGM 
00565                                                                   GBIDPGM 
00566          10  WS-01-ABCODE-1BS8          PIC X(04)  VALUE  '1BS8'. GBIDPGM 
00567          10  WS-01-ABCODE-1BS8-MSG      PIC X(44)  VALUE          GBIDPGM 
00568             'WRITEQ (QUEUE ID ERROR)                  '.          GBIDPGM 
00569                                                                   GBIDPGM 
00570          10  WS-01-ABCODE-1BS9          PIC X(04)  VALUE  '1BS9'. GBIDPGM 
00571          10  WS-01-ABCODE-1BS9-MSG      PIC X(44)  VALUE          GBIDPGM 
00572             'READQ  (UNKNOWN RETURN CODE)             '.          GBIDPGM 
00573                                                                   GBIDPGM 
00574 /                                                                 GBIDPGM 
00575  01  WS-LIT-RETURN-CODE-VALUES.                                   GBIDPGM 
00576      05  WS-LIT-RETURN-CODE-CLEAN        PIC X(02) VALUE '00'.    GBIDPGM 
00577      05  WS-LIT-RETURN-REC-NOT-FOUND     PIC X(02) VALUE '01'.    GBIDPGM 
00578      05  WS-LIT-RETURN-EOF-BROWSE        PIC X(02) VALUE '02'.    GBIDPGM 
00579      05  WS-LIT-RETURN-PAGE-QUEUE-EOF    PIC X(02) VALUE '03'.    GBIDPGM 
00580      05  WS-LIT-RETURN-PAGE-IS-FULL      PIC X(02) VALUE '04'.    GBIDPGM 
00581                                                                   GBIDPGM 
00582  01  WS-RETURN-CODE                      PIC X(02) VALUE '00'.    GBIDPGM 
00583      88  WS-RETURN-CODE-CLEAN                      VALUE '00'.    GBIDPGM 
00584      88  WS-RETURN-REC-NOT-FOUND                   VALUE '01'.    GBIDPGM 
00585      88  WS-RETURN-EOF-BROWSE                      VALUE '02'.    GBIDPGM 
00586      88  WS-RETURN-PAGE-QUEUE-EOF                  VALUE '03'.    GBIDPGM 
00587      88  WS-RETURN-PAGE-IS-FULL                    VALUE '04'.    GBIDPGM 
00588                                                                   GBIDPGM 
00589 /                                                                 GBIDPGM 
00590 ******************************************************************GBIDPGM 
00591 **  ACCUM HOLD AREAS                                            **GBIDPGM 
00592 ******************************************************************GBIDPGM 
00593                                                                   GBIDPGM 
00594  01  WS-LOOP-ACL-HOLD.                                            GBIDPGM 
00595  COPY GCTACLC.                                                    GBIDPGM 
00596                                                                   GBIDPGM 
00597  01  WS-LOOP-ACP-HOLD.                                            GBIDPGM 
00598  COPY GCTACPC.                                                    GBIDPGM 
00599                                                                   GBIDPGM 
00600  01  WS-LOOP-ADL-HOLD.                                            GBIDPGM 
00601  COPY GCTADLC.                                                    GBIDPGM 
00602                                                                   GBIDPGM 
00603  01  WS-LOOP-AOL-HOLD.                                            GBIDPGM 
00604  COPY GCTAOLC.                                                    GBIDPGM 
00605                                                                   GBIDPGM 
00606  01  WS-CON-ABM-HOLD.                                             GBIDPGM 
00607  COPY GCTABM2.                                                    GBIDPGM 
00608                                                                   GBIDPGM 
00609  01  WS-CON-ACL-HOLD.                                             GBIDPGM 
00610  COPY GCTACL2.                                                    GBIDPGM 
00611                                                                   GBIDPGM 
00612  01  WS-CON-ACP-HOLD.                                             GBIDPGM 
00613  COPY GCTACP2.                                                    GBIDPGM 
00614                                                                   GBIDPGM 
00615  01  WS-CON-ADL-HOLD.                                             GBIDPGM 
00616  COPY GCTADL2.                                                    GBIDPGM 
00617                                                                   GBIDPGM 
00618  01  WS-CON-AOL-HOLD.                                             GBIDPGM 
00619  COPY GCTAOL2.                                                    GBIDPGM 
00620                                                                   GBIDPGM 
00621  01  WS-GRP-ABM-HOLD.                                             GBIDPGM 
00622  COPY GCTABM3.                                                    GBIDPGM 
00623                                                                   GBIDPGM 
00624  01  WS-GRP-ACL-HOLD.                                             GBIDPGM 
00625  COPY GCTACL3.                                                    GBIDPGM 
00626                                                                   GBIDPGM 
00627  01  WS-GRP-ACP-HOLD.                                             GBIDPGM 
00628  COPY GCTACP3.                                                    GBIDPGM 
00629                                                                   GBIDPGM 
00630  01  WS-GRP-ADL-HOLD.                                             GBIDPGM 
00631  COPY GCTADL3.                                                    GBIDPGM 
00632                                                                   GBIDPGM 
00633  01  WS-GRP-AOL-HOLD.                                             GBIDPGM 
00634  COPY GCTAOL3.                                                    GBIDPGM 
00635                                                                   GBIDPGM 
00636 /                                                                 GBIDPGM 
00637 ******************************************************************GBIDPGM 
00638 ** WORK DETAIL LINES.                                             GBIDPGM 
00639 ******************************************************************GBIDPGM 
00640  01  WS-DETAIL-LINES.                                             GBIDPGM 
00641      05  WS-WRITE-LINE           PIC X(79) VALUE SPACES.          GBIDPGM 
00642      05  WS-WRITE-LINE-GROUP  REDEFINES WS-WRITE-LINE             GBIDPGM 
00643                                  PIC X(79).                       GBIDPGM 
00644      05  WS-WRITE-LINE-DETAIL REDEFINES WS-WRITE-LINE.            GBIDPGM 
00645          10  FILLER              PIC X(03).                       GBIDPGM 
00646          10  WS-WLD-DESCRIPTION  PIC X(39).                       GBIDPGM 
00647          10  FILLER              PIC X(01).                       GBIDPGM 
00648          10  WS-WLD-IN-VALUE     PIC X(11).                       GBIDPGM 
00649          10  FILLER              PIC X(01).                       GBIDPGM 
00650          10  WS-WLD-OUT-VALUE    PIC X(11).                       GBIDPGM 
00651          10  FILLER              PIC X(01).                       GBIDPGM 
00652          10  WS-WLD-OTHER-VALUE  PIC X(11).                       GBIDPGM 
00653          10  FILLER              PIC X(01).                       GBIDPGM 
00654                                                                   GBIDPGM 
00655 *************** GBIFPGM COMMAREA *********************************GBIDPGM 
00656  01  WS-GBIF-COMMAREA.                                            GBIDPGM 
00657  COPY GICOMKEC.                                                   GBIDPGM 
00658  COPY GCBENHLC.                                                   GBIDPGM 
00659                                                                   GBIDPGM 
00660                                                                   GBIDPGM 
00661 *************** DATE ROUTINE COMMAREA ****************************GBIDPGM 
00662  01  HGADATES-COMMAREA.                                           GBIDPGM 
00663  COPY HGCDAT01.                                                   GBIDPGM 
00664                                                                   GBIDPGM 
00665  01  WS-REC-LENGTHS.                                              GBIDPGM 
00666  COPY GCCDRLEN.                                                   GBIDPGM 
00667                                                                   GBIDPGM 
00668  01  WS-CONSTANTS.                                                GBIDPGM 
00669      05  WS-GCCONTRC-KEYLEN      PIC S9(4) COMP VALUE +36.        GBIDPGM 
00670      05  WS-IO-PARM-CONTRACT-LEN PIC S9(4) COMP VALUE +0.         GBIDPGM 
00671      05  WS-IO-PARM-GROUPSPC-LEN PIC S9(4) COMP VALUE +0.         GBIDPGM 
00672      05  WS-IO-PARM-TABULAR-LEN  PIC S9(4) COMP VALUE +0.         GBIDPGM 
00673      05  WS-GCCOMKEC-LENGTH      PIC S9(4) COMP VALUE +150.       GBIDPGM 
00674 /                                                                 GBIDPGM 
00675 ******************************************************************GBIDPGM 
00676 **  GROUPSPC FILE AND I/O PARM AREA.                            **GBIDPGM 
00677 ******************************************************************GBIDPGM 
00678  01  IO-PARM-GROUPSPC-AREA-1.                                     GBIDPGM 
00679      COPY  GCIOPRM1.                                              GBIDPGM 
00680      COPY  GCGROUPC.                                              GBIDPGM 
00681                                                                   GBIDPGM 
00682 /                                                                 GBIDPGM 
00683 ******************************************************************GBIDPGM 
00684 **  CONTRACT FILE AND I/O PARM AREA.                            **GBIDPGM 
00685 ******************************************************************GBIDPGM 
00686  01  IO-PARM-CONTRACT-AREA-1.                                     GBIDPGM 
00687      COPY  GCIOPRM3.                                              GBIDPGM 
00688      COPY  GCCONTRC.                                              GBIDPGM 
00689                                                                   GBIDPGM 
00690 /                                                                 GBIDPGM 
00691 ******************************************************************GBIDPGM 
00692 **  TABULAR FILE AND I/O PARM AREA.                             **GBIDPGM 
00693 ******************************************************************GBIDPGM 
00694  01  IO-PARM-TABULAR-AREA-1.                                      GBIDPGM 
00695  COPY GCIOPRM5.                                                   GBIDPGM 
00696  COPY GCTABMC.                                                    GBIDPGM 
00697                                                                   GBIDPGM 
00698                                                                   GBIDPGM 
00699  TITLE 'MAP I/O AREAS'.                                           GBIDPGM 
00700  01  WS-I-O-MAP-AREA             PIC X(32)  VALUE                 GBIDPGM 
00701      '***  I/O MAPAREA ***'.                                      GBIDPGM 
00702  01  WS-GBIDSETC                 PIC X(1400) VALUE LOW-VALUES.    GBIDPGM 
00703  01  GBIDI01I REDEFINES WS-GBIDSETC.                              GBIDPGM 
00704  COPY GBIDSETC REPLACING == 01  GBIDI01I. ==                      GBIDPGM 
00705                       BY ==               ==.                     GBIDPGM 
00706                                                                   GBIDPGM 
00707 /                                                                 GBIDPGM 
00708 ******************************************************************GBIDPGM 
00709 ** ATTRIBUTE BYTE SETTINGS                                      **GBIDPGM 
00710 ******************************************************************GBIDPGM 
00711  COPY DFHBMSCA.                                                   GBIDPGM 
00712      02  DFHBMABF                PIC X VALUE '9'.                 GBIDPGM 
00713                                                                   GBIDPGM 
00714 /                                                                 GBIDPGM 
00715 ******************************************************************GBIDPGM 
00716 ** ATTENTION IDENTIFIERS                                        **GBIDPGM 
00717 ******************************************************************GBIDPGM 
00718  COPY DFHAID.                                                     GBIDPGM 
00719                                                                   GBIDPGM 
00720 /                                                                 GBIDPGM 
00721 ******************************************************************GBIDPGM 
00722 ** HEX VALUES FOR MILL DATES.                                   **GBIDPGM 
00723 ******************************************************************GBIDPGM 
00724  COPY HEXCOBOL.                                                   GBIDPGM 
00725                                                                   GBIDPGM 
00726  01  WS-END                      PIC X(16)  VALUE                 GBIDPGM 
00727      '*** W/S ENDS ***'.                                          GBIDPGM 
00728 /                                                                 GBIDPGM 
00729 ******************************************************************GBIDPGM 
00730 ** THIS PROGRAM RECEIVES AN INCOMING DFHCOMMAREA ONLY WHEN IT   **GBIDPGM 
00731 ** IS INVOKED BY XCTL FROM GBIBPGM.                             **GBIDPGM 
00732 ******************************************************************GBIDPGM 
00733  LINKAGE SECTION.                                                 GBIDPGM 
00734                                                                   GBIDPGM 
00735  01  DFHCOMMAREA.                                                 GBIDPGM 
00736      02  GICOMKE2.                                                GBIDPGM 
00737  COPY GICOMKE2.                                                   GBIDPGM 
00738      10   GI2-REDF-AREA REDEFINES GI2-FILLER.                     GBIDPGM 
00739           15  GI2-SUBSCRIBER     PIC X(09).                       GBIDPGM 
00740           15  GI2-LAST-NAME      PIC X(15).                       GBIDPGM 
00741           15  GI2-FIRST-NAME     PIC X(09).                       GBIDPGM 
00742           15  GI2-REDF-FILLER    PIC X(03).                       GBIDPGM 
00743 **- ALSO ADD AREA FOR RECEIVING THE SUBSCRIBER ID ---------------*GBIDPGM 
00744                                                                   GBIDPGM 
00745 /                                                                 GBIDPGM 
00746  PROCEDURE DIVISION.                                              GBIDPGM 
00747 ******************************************************************GBIDPGM 
00748 **                    M A I N L I N E                            *GBIDPGM 
00749 **                                                               *GBIDPGM 
00750 ** THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONS *GBIDPGM 
00751 ** TAKEN BY THE OPERATOR.                                        *GBIDPGM 
00752 **                                                               *GBIDPGM 
00753 ******************************************************************GBIDPGM 
00754  0000-MAINLINE.                                                   GBIDPGM 
00755                                                                   GBIDPGM 
00756      MOVE '0000' TO WS-PARA-ID.                                   GBIDPGM 
00757      MOVE LOW-VALUES TO WS-HEX-00.                                GBIDPGM 
00758      MOVE EIBTRMID TO WS-TS-TRM.                                  GBIDPGM 
00759                                                                   GBIDPGM 
00760      EXEC CICS HANDLE CONDITION                                   GBIDPGM 
00761           MAPFAIL(0000-SECURITY-VIOLATION)                        GBIDPGM 
00762           END-EXEC.                                               GBIDPGM 
00763                                                                   GBIDPGM 
00764 ******************************************************************GBIDPGM 
00765 *                                                                 GBIDPGM 
00766 *    CHANGE BELOW 'IF' STATEMENT TO CHECK GI-MULT-PATH-ID         GBIDPGM 
00767 *    FOR TRANSACTION RATHER THAN EIBTRNID.                        GBIDPGM 
00768 *                                                                 GBIDPGM 
00769 ******************************************************************GBIDPGM 
00770      IF EIBAID = DFHPF3 OR = DFHPF15                              GBIDPGM 
00771          PERFORM 8300-000-DELETE-PAGE-QUEUE THRU 8300-900-EXIT    GBIDPGM 
00772          IF EIBTRNID = 'GHIL' OR 'GBID'                           GBIDPGM 
00773          EXEC CICS XCTL                                           GBIDPGM 
00774               PROGRAM('GHILPGM')                                  GBIDPGM 
00775               END-EXEC                                            GBIDPGM 
00776          ELSE                                                     GBIDPGM 
00777          IF EIBTRNID = 'GBIB' OR 'GBIE' OR 'GBIG'                 GBIDPGM 
00778          EXEC CICS XCTL                                           GBIDPGM 
00779               PROGRAM('GBIBPGM')                                  GBIDPGM 
00780               COMMAREA (COMMAREA-KEY-RECORD)                      GBIDPGM 
00781               LENGTH   (LENGTH OF COMMAREA-KEY-RECORD)            GBIDPGM 
00782               END-EXEC.                                           GBIDPGM 
00783                                                                   GBIDPGM 
00784      IF EIBTRNID = 'GHIL' OR                                      GBIDPGM 
00785         EIBTRNID = 'GBIB' OR                                      GBIDPGM 
00786         EIBTRNID = 'GBIE' OR                                      GBIDPGM 
00787         EIBTRNID = 'GBIG'                                         GBIDPGM 
00788         IF EIBAID = DFHENTER OR = DFHPF9                          GBIDPGM 
00789            PERFORM 1000-DISPLAY-FIRST-SCREEN.                     GBIDPGM 
00790                                                                   GBIDPGM 
00791      IF EIBTRNID = 'GBID'                                         GBIDPGM 
00792         IF EIBAID = DFHPF7 OR = DFHPF19                           GBIDPGM 
00793            PERFORM 5100-000-DISPLAY-NEXT-PAGE THRU 5100-900-EXIT. GBIDPGM 
00794                                                                   GBIDPGM 
00795      IF EIBTRNID = 'GBID'                                         GBIDPGM 
00796         IF EIBAID = DFHPF8 OR = DFHPF20                           GBIDPGM 
00797            PERFORM 5000-000-DISPLAY-NEXT-PAGE THRU 5000-900-EXIT. GBIDPGM 
00798                                                                   GBIDPGM 
00799      IF EIBTRNID = 'GBID'                                         GBIDPGM 
00800         IF EIBAID = DFHPF10 OR = DFHPF22                          GBIDPGM 
00801            PERFORM 5200-000-DISPLAY-LAST-PAGE THRU 5200-900-EXIT. GBIDPGM 
00802                                                                   GBIDPGM 
00803      IF EIBTRNID = 'GBID'                                         GBIDPGM 
00804         IF EIBAID = DFHPF11 OR = DFHPF23                          GBIDPGM 
00805            PERFORM 5300-000-DISPLAY-FIRST-PAGE THRU 5300-900-EXIT.GBIDPGM 
00806                                                                   GBIDPGM 
00807      PERFORM 0099-RETURN.                                         GBIDPGM 
00808                                                                   GBIDPGM 
00809 /                                                                 GBIDPGM 
00810 ******************************************************************GBIDPGM 
00811 ** IF WE HAVE REACHED THIS POINT, THEN THE TERMINAL USER KEYED  **GBIDPGM 
00812 ** THE TRANSID 'GBID' ON A CLEARED SCREEN OR IN THE CORNER OF   **GBIDPGM 
00813 ** SOME OTHER MAP.  TERMINAL USERS SHOULD INVOKE THIS PROGRAM   **GBIDPGM 
00814 ** ONLY BY GOING THROUGH APPROPRIATE HIGHER-LEVEL PROGRAMS.     **GBIDPGM 
00815 ******************************************************************GBIDPGM 
00816  0000-SECURITY-VIOLATION.                                         GBIDPGM 
00817                                                                   GBIDPGM 
00818      MOVE '0000' TO WS-PARA-ID.                                   GBIDPGM 
00819                                                                   GBIDPGM 
00820      EXEC CICS XCTL                                               GBIDPGM 
00821           PROGRAM('GCS1PGM')                                      GBIDPGM 
00822           END-EXEC.                                               GBIDPGM 
00823                                                                   GBIDPGM 
00824  0000-MAINLINE-EXIT.                                              GBIDPGM 
00825      EXIT.                                                        GBIDPGM 
00826 /                                                                 GBIDPGM 
00827 ******************************************************************GBIDPGM 
00828 *                                                                 GBIDPGM 
00829 ******************************************************************GBIDPGM 
00830  0099-RETURN.                                                     GBIDPGM 
00831                                                                   GBIDPGM 
00832      MOVE '0099' TO WS-PARA-ID.                                   GBIDPGM 
00833                                                                   GBIDPGM 
00834      EXEC CICS                                                    GBIDPGM 
00835           RETURN                                                  GBIDPGM 
00836           END-EXEC.                                               GBIDPGM 
00837                                                                   GBIDPGM 
00838  0099-EXIT.                                                       GBIDPGM 
00839      EXIT.                                                        GBIDPGM 
00840 /                                                                 GBIDPGM 
00841 ******************************************************************GBIDPGM 
00842 *                                                                 GBIDPGM 
00843 *  DISPLAY FIRST SCREEN OF BENEFIT HIGHLIGHTS                     GBIDPGM 
00844 *                                                                 GBIDPGM 
00845 ******************************************************************GBIDPGM 
00846  1000-DISPLAY-FIRST-SCREEN.                                       GBIDPGM 
00847                                                                   GBIDPGM 
00848      MOVE '1000' TO WS-PARA-ID.                                   GBIDPGM 
00849                                                                   GBIDPGM 
00850      PERFORM 8300-000-DELETE-PAGE-QUEUE THRU 8300-900-EXIT.       GBIDPGM 
00851                                                                   GBIDPGM 
00852      PERFORM 8800-INIT-TS-LINES                                   GBIDPGM 
00853        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
00854          FROM 1 BY 1                                              GBIDPGM 
00855            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
00856                                                                   GBIDPGM 
00857      MOVE +1 TO WS-TS-LINE-SUB.                                   GBIDPGM 
00858                                                                   GBIDPGM 
00859      MOVE GIC2-CONTRACT-ID       TO GCT-CONTRACT-ID.              GBIDPGM 
00860      MOVE GIG2-GROUP-SPECIFIC-ID TO GCG-GRP-SPECIF-ID.            GBIDPGM 
00861                                                                   GBIDPGM 
00862      PERFORM 8000-READ-GROUPSPC.                                  GBIDPGM 
00863      PERFORM 8100-READ-CONTRACT.                                  GBIDPGM 
00864      PERFORM 6000-MOVE-KEY-TO-SCREEN.                             GBIDPGM 
00865                                                                   GBIDPGM 
00866 ******************************************************************GBIDPGM 
00867 **** MOVE GROUP AND CONTRACT INFO TO GICOMKEC                  ***GBIDPGM 
00868 **** AND THEN LINK TO GBIFPGM.                                 ***GBIDPGM 
00869 ******************************************************************GBIDPGM 
00870      EXEC CICS GETMAIN  SET (WS-GBIF-COMMAREA)                    GBIDPGM 
00871                         INITIMG(WS-HEX-00)                        GBIDPGM 
00872                         LENGTH (LENGTH OF WS-GBIF-COMMAREA)       GBIDPGM 
00873                         END-EXEC.                                 GBIDPGM 
00874                                                                   GBIDPGM 
00875      MOVE GCT-CONTRACT-ID    TO GIC-CONTRACT-ID.                  GBIDPGM 
00876      MOVE GCG-GRP-SPECIF-ID  TO GIG-GROUP-SPECIFIC-ID.            GBIDPGM 
00877      EXEC CICS  LINK  PROGRAM ('GBIFPGM')                         GBIDPGM 
00878                       COMMAREA(WS-GBIF-COMMAREA)                  GBIDPGM 
00879                       LENGTH  (LENGTH OF WS-GBIF-COMMAREA)        GBIDPGM 
00880                       END-EXEC.                                   GBIDPGM 
00881                                                                   GBIDPGM 
00882                                                                   GBIDPGM 
00883      PERFORM 6100-STORE-TS-HEADING.                               GBIDPGM 
00884                                                                   GBIDPGM 
00885      PERFORM 2000-BUILD-TS-QUEUE-PAGES THRU 2000-EXIT.            GBIDPGM 
00886                                                                   GBIDPGM 
00887      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
00888      MOVE +1           TO WS-TS-LST-PAGE,                         GBIDPGM 
00889                           WS-PAGE-NUM.                            GBIDPGM 
00890      MOVE WS-TPAGE-NUM TO WS-TS-TOT-PAGE.                         GBIDPGM 
00891      PERFORM 7400-000-REWRITE-TS-QUEUE THRU 7400-900-EXIT.        GBIDPGM 
00892                                                                   GBIDPGM 
00893      MOVE WS-TPAGE-NUM    TO TPAGEO.                              GBIDPGM 
00894                                                                   GBIDPGM 
00895      MOVE +1  TO WS-PAGE-NUM.                                     GBIDPGM 
00896      MOVE +2  TO WS-TS-QITEM.                                     GBIDPGM 
00897      PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT          GBIDPGM 
00898      MOVE WS-PAGE-NUM     TO CPAGEO,                              GBIDPGM 
00899                              WS-TS-LST-PAGE.                      GBIDPGM 
00900                                                                   GBIDPGM 
00901      PERFORM 6200-FILL-MAP-DETAIL                                 GBIDPGM 
00902        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
00903          FROM 1 BY 1                                              GBIDPGM 
00904            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
00905                                                                   GBIDPGM 
00906      PERFORM 8700-SEND-PAGE-AND-RETURN.                           GBIDPGM 
00907                                                                   GBIDPGM 
00908  1000-EXIT.                                                       GBIDPGM 
00909      EXIT.                                                        GBIDPGM 
00910 /                                                                 GBIDPGM 
00911 ******************************************************************GBIDPGM 
00912 *                                                                 GBIDPGM 
00913 ******************************************************************GBIDPGM 
00914  2000-BUILD-TS-QUEUE-PAGES.                                       GBIDPGM 
00915                                                                   GBIDPGM 
00916      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
00917                                                                   GBIDPGM 
00918      PERFORM 4100-DETERMINE-PRODUCT-NAME.                         GBIDPGM 
00919                                                                   GBIDPGM 
00920  2000-4020.                                                       GBIDPGM 
00921      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
00922         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
00923         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
00924                                                                   GBIDPGM 
00925      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
00926      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
00927                                                                   GBIDPGM 
00928      IF GCBH-DED-PER-IND-LN NOT = SPACES                          GBIDPGM 
00929         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
00930         MOVE WS-RULE-4020-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
00931         MOVE GCBH-DED-PER-IND-IN   TO WS-WLD-IN-VALUE             GBIDPGM 
00932         MOVE GCBH-DED-PER-IND-OUT  TO WS-WLD-OUT-VALUE            GBIDPGM 
00933         MOVE GCBH-DED-PER-IND-OTH  TO WS-WLD-OTHER-VALUE          GBIDPGM 
00934         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
00935         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
00936                                                                   GBIDPGM 
01010 *MQ 01/24/05                                                      GBIDPGM 
01011  2000-4022.                                                       GBIDPGM 
01012      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01013         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01014         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01015                                                                   GBIDPGM 
01016      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01017      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01018                                                                   GBIDPGM 
01019      IF GCBH-IND-ALONE-DED-LN NOT = SPACES                        GBIDPGM 
01020         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01021         MOVE WS-RULE-4022-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01022         MOVE GCBH-IND-ALONE-DED-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01023         MOVE GCBH-IND-ALONE-DED-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01024         MOVE GCBH-IND-ALONE-DED-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01025         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01026         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01027                                                                   GBIDPGM 
01028                                                                   GBIDPGM 
01029 *MQ 01/20/05                                                      GBIDPGM 
01030  2000-4025.                                                       GBIDPGM 
01031      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01032         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01033         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01034                                                                   GBIDPGM 
01035      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01036      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01037                                                                   GBIDPGM 
01038      IF GCBH-COMB-IND-DED-LN NOT = SPACES                         GBIDPGM 
01039         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01040         MOVE WS-RULE-4025-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01041         MOVE GCBH-COMB-IND-DED-IN  TO WS-WLD-IN-VALUE             GBIDPGM 
01042         MOVE GCBH-COMB-IND-DED-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01043         MOVE GCBH-COMB-IND-DED-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01044         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01045         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01046                                                                   GBIDPGM 
00937                                                                   GBIDPGM 
00938  2000-4030.                                                       GBIDPGM 
00939      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
00940         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
00941         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
00942                                                                   GBIDPGM 
00943      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
00944      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
00945                                                                   GBIDPGM 
00946      IF GCBH-DED-PER-FAM-LN NOT = SPACES                          GBIDPGM 
00947         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
00948         MOVE WS-RULE-4030-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
00949         MOVE GCBH-DED-PER-FAM-IN   TO WS-WLD-IN-VALUE             GBIDPGM 
00950         MOVE GCBH-DED-PER-FAM-OUT  TO WS-WLD-OUT-VALUE            GBIDPGM 
00951         MOVE GCBH-DED-PER-FAM-OTH  TO WS-WLD-OTHER-VALUE          GBIDPGM 
00952         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
00953         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
00954                                                                   GBIDPGM 
00955                                                                   GBIDPGM 
01066 *MQ 01/24/05                                                      GBIDPGM 
01067  2000-4032.                                                       GBIDPGM 
01068      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01069         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01070         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01071                                                                   GBIDPGM 
01072      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01073      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01074                                                                   GBIDPGM 
01075      IF GCBH-FAM-ALONE-DED-LN NOT = SPACES                        GBIDPGM 
01076         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01077         MOVE WS-RULE-4032-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01078         MOVE GCBH-FAM-ALONE-DED-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01079         MOVE GCBH-FAM-ALONE-DED-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01080         MOVE GCBH-FAM-ALONE-DED-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01081         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01082         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01083                                                                   GBIDPGM 
01084                                                                   GBIDPGM 
01085 *MQ 01/20/05                                                      GBIDPGM 
01086  2000-4035.                                                       GBIDPGM 
01087      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01088         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01089         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01090                                                                   GBIDPGM 
01091      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01092      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01093                                                                   GBIDPGM 
01094      IF GCBH-COMB-FAM-DED-LN NOT = SPACES                         GBIDPGM 
01095         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01096         MOVE WS-RULE-4035-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01097         MOVE GCBH-COMB-FAM-DED-IN  TO WS-WLD-IN-VALUE             GBIDPGM 
01098         MOVE GCBH-COMB-FAM-DED-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01099         MOVE GCBH-COMB-FAM-DED-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01100         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01101         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01102                                                                   GBIDPGM 
01103                                                                   GBIDPGM 
00956  2000-4040.                                                       GBIDPGM 
00957      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
00958         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
00959         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
00960                                                                   GBIDPGM 
00961      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
00962      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
00963                                                                   GBIDPGM 
00964      IF GCBH-OOP-PER-IND-LN NOT = SPACES                          GBIDPGM 
00965         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
00966         MOVE WS-RULE-4040-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
00967         MOVE GCBH-OOP-PER-IND-IN   TO WS-WLD-IN-VALUE             GBIDPGM 
00968         MOVE GCBH-OOP-PER-IND-OUT  TO WS-WLD-OUT-VALUE            GBIDPGM 
00969         MOVE GCBH-OOP-PER-IND-OTH  TO WS-WLD-OTHER-VALUE          GBIDPGM 
00970         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
00971         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
00972                                                                   GBIDPGM 
00973                                                                   GBIDPGM 
01122 *MQ 01/24/05                                                      GBIDPGM 
01123  2000-4042.                                                       GBIDPGM 
01124      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01125         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01126         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01127                                                                   GBIDPGM 
01128      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01129      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01130                                                                   GBIDPGM 
01131      IF GCBH-IND-ALONE-OOP-LN NOT = SPACES                        GBIDPGM 
01132         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01133         MOVE WS-RULE-4042-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01134         MOVE GCBH-IND-ALONE-OOP-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01135         MOVE GCBH-IND-ALONE-OOP-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01136         MOVE GCBH-IND-ALONE-OOP-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01137         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01138         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01139                                                                   GBIDPGM 
01140                                                                   GBIDPGM 
01141 *MQ 01/20/05                                                      GBIDPGM 
01142  2000-4045.                                                       GBIDPGM 
01143      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01144         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01145         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01146                                                                   GBIDPGM 
01147      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01148      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01149                                                                   GBIDPGM 
01150      IF GCBH-COMB-IND-OOP-LN NOT = SPACES                         GBIDPGM 
01151         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01152         MOVE WS-RULE-4045-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01153         MOVE GCBH-COMB-IND-OOP-IN  TO WS-WLD-IN-VALUE             GBIDPGM 
01154         MOVE GCBH-COMB-IND-OOP-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01155         MOVE GCBH-COMB-IND-OOP-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01156         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01157         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01158                                                                   GBIDPGM 
01159                                                                   GBIDPGM 
00974  2000-4050.                                                       GBIDPGM 
00975      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
00976         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
00977         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
00978                                                                   GBIDPGM 
00979      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
00980      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
00981                                                                   GBIDPGM 
00982      IF GCBH-OOP-PER-FAM-LN NOT = SPACES                          GBIDPGM 
00983         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
00984         MOVE WS-RULE-4050-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
00985         MOVE GCBH-OOP-PER-FAM-IN   TO WS-WLD-IN-VALUE             GBIDPGM 
00986         MOVE GCBH-OOP-PER-FAM-OUT  TO WS-WLD-OUT-VALUE            GBIDPGM 
00987         MOVE GCBH-OOP-PER-FAM-OTH  TO WS-WLD-OTHER-VALUE          GBIDPGM 
00988         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
00989         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
00990                                                                   GBIDPGM 
00991                                                                   GBIDPGM 
01178 *MQ 01/24/05                                                      GBIDPGM 
01179  2000-4052.                                                       GBIDPGM 
01180      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01181         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01182         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01183                                                                   GBIDPGM 
01184      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01185      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01186                                                                   GBIDPGM 
01187      IF GCBH-FAM-ALONE-OOP-LN NOT = SPACES                        GBIDPGM 
01188         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01189         MOVE WS-RULE-4052-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01190         MOVE GCBH-FAM-ALONE-OOP-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01191         MOVE GCBH-FAM-ALONE-OOP-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01192         MOVE GCBH-FAM-ALONE-OOP-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01193         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01194         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01195                                                                   GBIDPGM 
01196                                                                   GBIDPGM 
01197 *MQ 01/20/05                                                      GBIDPGM 
01198  2000-4055.                                                       GBIDPGM 
01199      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01200         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01201         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01202                                                                   GBIDPGM 
01203      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01204      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01205                                                                   GBIDPGM 
01206      IF GCBH-COMB-FAM-OOP-LN NOT = SPACES                         GBIDPGM 
01207         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01208         MOVE WS-RULE-4055-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01209         MOVE GCBH-COMB-FAM-OOP-IN  TO WS-WLD-IN-VALUE             GBIDPGM 
01210         MOVE GCBH-COMB-FAM-OOP-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01211         MOVE GCBH-COMB-FAM-OOP-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01212         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01213         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01214                                                                   GBIDPGM 
01215                                                                   GBIDPGM 
00992  2000-4060.                                                       GBIDPGM 
00993      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
00994         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
00995         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
00996                                                                   GBIDPGM 
00997      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
00998      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
00999                                                                   GBIDPGM 
01000      IF GCBH-LIFE-MAX-LN NOT = SPACES                             GBIDPGM 
01001         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01002         MOVE WS-RULE-4060-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01003         MOVE GCBH-LIFE-MAX-IN      TO WS-WLD-IN-VALUE             GBIDPGM 
01004         MOVE GCBH-LIFE-MAX-OUT     TO WS-WLD-OUT-VALUE            GBIDPGM 
01005         MOVE GCBH-LIFE-MAX-OTH     TO WS-WLD-OTHER-VALUE          GBIDPGM 
01006         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01007         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01008                                                                   GBIDPGM 
01009                                                                   GBIDPGM 
01010  2000-4070.                                                       GBIDPGM 
01011      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01012         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01013         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01014                                                                   GBIDPGM 
01015      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01016      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01017                                                                   GBIDPGM 
01018      IF GCBH-EMER-RM-COPAY-LN NOT = SPACES                        GBIDPGM 
01019         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01020         MOVE WS-RULE-4070-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01021         MOVE GCBH-EMER-RM-COPAY-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01022         MOVE GCBH-EMER-RM-COPAY-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01023         MOVE GCBH-EMER-RM-COPAY-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01024         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01025         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01026                                                                   GBIDPGM 
01251 *MQ 05/11/05  CHANGE BEGIN                                        GBIDPGM 
01252  2000-4071.                                                       GBIDPGM 
01253      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01254         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01255         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01256                                                                   GBIDPGM 
01257      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01258      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01259                                                                   GBIDPGM 
01260      IF GCBH-AMB-COPAY-LN     NOT = SPACES                        GBIDPGM 
01261         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01262         MOVE WS-RULE-4071-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01263         MOVE GCBH-AMB-COPAY-IN     TO WS-WLD-IN-VALUE             GBIDPGM 
01264         MOVE GCBH-AMB-COPAY-OUT     TO WS-WLD-OUT-VALUE           GBIDPGM 
01265         MOVE GCBH-AMB-COPAY-OTH     TO WS-WLD-OTHER-VALUE         GBIDPGM 
01266         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01267         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01268                                                                   GBIDPGM 
01269                                                                   GBIDPGM 
01270  2000-4072.                                                       GBIDPGM 
01271      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01272         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01273         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01274                                                                   GBIDPGM 
01275      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01276      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01277                                                                   GBIDPGM 
01278      IF GCBH-SKILL-NUR-FAC-COPAY-LN NOT = SPACES                  GBIDPGM 
01279         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01280         MOVE WS-RULE-4072-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01281         MOVE GCBH-SKILL-NUR-FAC-COPAY-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01282         MOVE GCBH-SKILL-NUR-FAC-COPAY-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01283         MOVE GCBH-SKILL-NUR-FAC-COPAY-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01284         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01285         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01286                                                                   GBIDPGM 
01287                                                                   GBIDPGM 
01288  2000-4073.                                                       GBIDPGM 
01289      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01290         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01291         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01292                                                                   GBIDPGM 
01293      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01294      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01295                                                                   GBIDPGM 
01296      IF GCBH-OP-PSYC-VISIT-COPAY-LN  NOT = SPACES                 GBIDPGM 
01297         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01298         MOVE WS-RULE-4073-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01299         MOVE GCBH-OP-PSYC-VISIT-COPAY-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01300         MOVE GCBH-OP-PSYC-VISIT-COPAY-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01301         MOVE GCBH-OP-PSYC-VISIT-COPAY-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01302         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01303         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01304                                                                   GBIDPGM 
01305                                                                   GBIDPGM 
01306  2000-4074.                                                       GBIDPGM 
01307      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01308         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01309         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01310                                                                   GBIDPGM 
01311      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01312      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01313                                                                   GBIDPGM 
01314      IF GCBH-IP-PER-ADM-COPAY-LN     NOT = SPACES                 GBIDPGM 
01315         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01316         MOVE WS-RULE-4074-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01317         MOVE GCBH-IP-PER-ADM-COPAY-IN     TO WS-WLD-IN-VALUE      GBIDPGM 
01318         MOVE GCBH-IP-PER-ADM-COPAY-OUT    TO WS-WLD-OUT-VALUE     GBIDPGM 
01319         MOVE GCBH-IP-PER-ADM-COPAY-OTH    TO WS-WLD-OTHER-VALUE   GBIDPGM 
01320         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01321         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01322                                                                   GBIDPGM 
01323  2000-4075.                                                       GBIDPGM 
01324      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01325         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01326         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01327                                                                   GBIDPGM 
01328      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01329      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01330                                                                   GBIDPGM 
01331      IF GCBH-HOME-HLTH-VST-COPAY-LN  NOT = SPACES                 GBIDPGM 
01332         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01333         MOVE WS-RULE-4075-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01334         MOVE GCBH-HOME-HLTH-VST-COPAY-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01335         MOVE GCBH-HOME-HLTH-VST-COPAY-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01336         MOVE GCBH-HOME-HLTH-VST-COPAY-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01337         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01338         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01339                                                                   GBIDPGM 
01340                                                                   GBIDPGM 
01341  2000-4076.                                                       GBIDPGM 
01342      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01343         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01344         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01345                                                                   GBIDPGM 
01346      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01347      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01348                                                                   GBIDPGM 
01349      IF GCBH-ALLERGY-TST-PMT-LVL-LN  NOT = SPACES                 GBIDPGM 
01350         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01351         MOVE WS-RULE-4076-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01352         MOVE GCBH-ALLERGY-TST-PMT-LVL-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01353         MOVE GCBH-ALLERGY-TST-PMT-LVL-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01354         MOVE GCBH-ALLERGY-TST-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01355         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01356         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01357 *MQ 05/11/05  CHANGE END                                          GBIDPGM 
01027                                                                   GBIDPGM 
01028  2000-4080.                                                       GBIDPGM 
01029      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01030         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01031         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01032                                                                   GBIDPGM 
01033      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01034      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01035                                                                   GBIDPGM 
01036      IF GCBH-OFF-VISIT-COPAY-LN NOT = SPACES                      GBIDPGM 
01037         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01038         MOVE WS-RULE-4080-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01039         MOVE GCBH-OFF-VISIT-COPAY-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01040         MOVE GCBH-OFF-VISIT-COPAY-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01041         MOVE GCBH-OFF-VISIT-COPAY-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01042         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01043         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01044                                                                   GBIDPGM 
01045                                                                   GBIDPGM 
01046  2000-4085.                                                       GBIDPGM 
01047      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01048         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01049         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01050                                                                   GBIDPGM 
01051      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01052      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01053                                                                   GBIDPGM 
01054      IF GCBH-OFF-VISIT-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
01055         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01056         MOVE WS-RULE-4085-VALUE         TO WS-WLD-DESCRIPTION     GBIDPGM 
01057         MOVE GCBH-OFF-VISIT-PMT-LVL-IN  TO WS-WLD-IN-VALUE        GBIDPGM 
01058         MOVE GCBH-OFF-VISIT-PMT-LVL-OUT TO WS-WLD-OUT-VALUE       GBIDPGM 
01059         MOVE GCBH-OFF-VISIT-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE     GBIDPGM 
01060         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01061         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01062                                                                   GBIDPGM 
01063 *MQ 11/03                                                         GBIDPGM 
01064  2000-4086.                                                       GBIDPGM 
01065      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01066         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01067         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01068                                                                   GBIDPGM 
01069      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01070      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01071                                                                   GBIDPGM 
01072      IF GCBH-OFF-SURG-PMT-LVL-LN  NOT = SPACES                    GBIDPGM 
01073         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01074         MOVE WS-RULE-4086-VALUE         TO WS-WLD-DESCRIPTION     GBIDPGM 
01075         MOVE GCBH-OFF-SURG-PMT-LVL-IN   TO WS-WLD-IN-VALUE        GBIDPGM 
01076         MOVE GCBH-OFF-SURG-PMT-LVL-OUT  TO WS-WLD-OUT-VALUE       GBIDPGM 
01077         MOVE GCBH-OFF-SURG-PMT-LVL-OTH  TO WS-WLD-OTHER-VALUE     GBIDPGM 
01078         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01079         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01080                                                                   GBIDPGM 
01081                                                                   GBIDPGM 
01082  2000-4090.                                                       GBIDPGM 
01083      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01084         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01085         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01086                                                                   GBIDPGM 
01087      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01088      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01089                                                                   GBIDPGM 
01090      IF GCBH-WELL-CARE-COPAY-LN NOT = SPACES                      GBIDPGM 
01091         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01092         MOVE WS-RULE-4090-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01093         MOVE GCBH-WELL-CARE-COPAY-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01094         MOVE GCBH-WELL-CARE-COPAY-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01095         MOVE GCBH-WELL-CARE-COPAY-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01096         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01097         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01098                                                                   GBIDPGM 
01099                                                                   GBIDPGM 
01100 *MQ 11/03                                                         GBIDPGM 
01101  2000-4091.                                                       GBIDPGM 
01102      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01103         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01104         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01105                                                                   GBIDPGM 
01106      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01107      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01108                                                                   GBIDPGM 
01109      IF GCBH-SPEC-OFF-VISIT-COPAY-LN  NOT = SPACES                GBIDPGM 
01110         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01111         MOVE WS-RULE-4091-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01112         MOVE GCBH-SPEC-OFF-VISIT-COPAY-IN  TO WS-WLD-IN-VALUE     GBIDPGM 
01113         MOVE GCBH-SPEC-OFF-VISIT-COPAY-OUT TO WS-WLD-OUT-VALUE    GBIDPGM 
01114         MOVE GCBH-SPEC-OFF-VISIT-COPAY-OTH TO WS-WLD-OTHER-VALUE  GBIDPGM 
01115         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01116         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01117                                                                   GBIDPGM 
01118                                                                   GBIDPGM 
01119 *MQ 11/03                                                         GBIDPGM 
01120  2000-4092.                                                       GBIDPGM 
01121      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01122         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01123         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01124                                                                   GBIDPGM 
01125      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01126      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01127                                                                   GBIDPGM 
01128      IF GCBH-OP-SURG-COPAY-LN         NOT = SPACES                GBIDPGM 
01129         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01130         MOVE WS-RULE-4092-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01131         MOVE GCBH-OP-SURG-COPAY-IN         TO WS-WLD-IN-VALUE     GBIDPGM 
01132         MOVE GCBH-OP-SURG-COPAY-OUT        TO WS-WLD-OUT-VALUE    GBIDPGM 
01133         MOVE GCBH-OP-SURG-COPAY-OTH        TO WS-WLD-OTHER-VALUE  GBIDPGM 
01134         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01135         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01136                                                                   GBIDPGM 
01137                                                                   GBIDPGM 
01138 *MQ 11/03                                                         GBIDPGM 
01139  2000-4093.                                                       GBIDPGM 
01140      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01141         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01142         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01143                                                                   GBIDPGM 
01144      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01145      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01146                                                                   GBIDPGM 
01147      IF GCBH-OP-MSA-COPAY-LN          NOT = SPACES                GBIDPGM 
01148         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01149         MOVE WS-RULE-4093-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01150         MOVE GCBH-OP-MSA-COPAY-IN          TO WS-WLD-IN-VALUE     GBIDPGM 
01151         MOVE GCBH-OP-MSA-COPAY-OUT         TO WS-WLD-OUT-VALUE    GBIDPGM 
01152         MOVE GCBH-OP-MSA-COPAY-OTH         TO WS-WLD-OTHER-VALUE  GBIDPGM 
01153         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01154         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01155                                                                   GBIDPGM 
01156                                                                   GBIDPGM 
01157 *MQ 11/03                                                         GBIDPGM 
01158  2000-4094.                                                       GBIDPGM 
01159      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01160         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01161         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01162                                                                   GBIDPGM 
01163      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01164      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01165                                                                   GBIDPGM 
01166      IF GCBH-UCF-COPAY-LN             NOT = SPACES                GBIDPGM 
01167         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01168         MOVE WS-RULE-4094-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01169         MOVE GCBH-UCF-COPAY-IN             TO WS-WLD-IN-VALUE     GBIDPGM 
01170         MOVE GCBH-UCF-COPAY-OUT            TO WS-WLD-OUT-VALUE    GBIDPGM 
01171         MOVE GCBH-UCF-COPAY-OTH            TO WS-WLD-OTHER-VALUE  GBIDPGM 
01172         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01173         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01174                                                                   GBIDPGM 
01175                                                                   GBIDPGM 
01176 *MQ 11/03                                                         GBIDPGM 
01177  2000-4095.                                                       GBIDPGM 
01178      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01179         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01180         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01181                                                                   GBIDPGM 
01182      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01183      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01184                                                                   GBIDPGM 
01185      IF GCBH-OP-HOSP-COPAY-LN         NOT = SPACES                GBIDPGM 
01186         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01187         MOVE WS-RULE-4095-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01188         MOVE GCBH-OP-HOSP-COPAY-IN         TO WS-WLD-IN-VALUE     GBIDPGM 
01189         MOVE GCBH-OP-HOSP-COPAY-OUT        TO WS-WLD-OUT-VALUE    GBIDPGM 
01190         MOVE GCBH-OP-HOSP-COPAY-OTH        TO WS-WLD-OTHER-VALUE  GBIDPGM 
01191         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01192         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01193                                                                   GBIDPGM 
01194                                                                   GBIDPGM 
01195 *MQ 11/03                                                         GBIDPGM 
01196  2000-4096.                                                       GBIDPGM 
01197      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01198         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01199         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01200                                                                   GBIDPGM 
01201      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01202      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01203                                                                   GBIDPGM 
01204      IF GCBH-UCP-COPAY-LN             NOT = SPACES                GBIDPGM 
01205         PERFORM 4001-PLAN-SUM-HEADING-CHECK                       GBIDPGM 
01206         MOVE WS-RULE-4096-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01207         MOVE GCBH-UCP-COPAY-IN             TO WS-WLD-IN-VALUE     GBIDPGM 
01208         MOVE GCBH-UCP-COPAY-OUT            TO WS-WLD-OUT-VALUE    GBIDPGM 
01209         MOVE GCBH-UCP-COPAY-OTH            TO WS-WLD-OTHER-VALUE  GBIDPGM 
01210         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01211         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01212                                                                   GBIDPGM 
01213                                                                   GBIDPGM 
01214  2000-4100.                                                       GBIDPGM 
01215 **** RESET HEADING SWITCH ****************************************GBIDPGM 
01216      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
01217                                                                   GBIDPGM 
01218      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01219         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01220         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01221                                                                   GBIDPGM 
01222      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01223      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01224                                                                   GBIDPGM 
01225      IF GCBH-HOSP-PMT-LVL-LN NOT = SPACES                         GBIDPGM 
01226         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01227         MOVE WS-RULE-4100-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01228         MOVE GCBH-HOSP-PMT-LVL-IN TO WS-WLD-IN-VALUE              GBIDPGM 
01229         MOVE GCBH-HOSP-PMT-LVL-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01230         MOVE GCBH-HOSP-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01231         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01232         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01233                                                                   GBIDPGM 
01234  2000-4110.                                                       GBIDPGM 
01235      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01236         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01237         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01238                                                                   GBIDPGM 
01239      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01240      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01241                                                                   GBIDPGM 
01242      IF GCBH-PER-ADM-DED-LN NOT = SPACES                          GBIDPGM 
01243         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01244         MOVE WS-RULE-4110-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01245         MOVE GCBH-PER-ADM-DED-IN TO WS-WLD-IN-VALUE               GBIDPGM 
01246         MOVE GCBH-PER-ADM-DED-OUT TO WS-WLD-OUT-VALUE             GBIDPGM 
01247         MOVE GCBH-PER-ADM-DED-OTH TO WS-WLD-OTHER-VALUE           GBIDPGM 
01579         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01580         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01581                                                                   GBIDPGM 
01582  2000-4115.                                                       GBIDPGM 
01583      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01584         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01585         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01586                                                                   GBIDPGM 
01587      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01588      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01589                                                                   GBIDPGM 
01590      IF GCBH-PER-ADM-DED-MAX-LN NOT = SPACES                      GBIDPGM 
01591         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01592         MOVE WS-RULE-4115-VALUE       TO WS-WLD-DESCRIPTION       GBIDPGM 
01593         MOVE GCBH-PER-ADM-DED-MAX-IN  TO WS-WLD-IN-VALUE          GBIDPGM 
01594         MOVE GCBH-PER-ADM-DED-MAX-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01595         MOVE GCBH-PER-ADM-DED-MAX-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01248         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01249         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01250                                                                   GBIDPGM 
01251  2000-4120.                                                       GBIDPGM 
01252      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01253         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01254         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01255                                                                   GBIDPGM 
01256      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01257      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01258                                                                   GBIDPGM 
01259      IF GCBH-OS-HOSP-PMT-LVL-LN NOT = SPACES                      GBIDPGM 
01260         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01261         MOVE WS-RULE-4120-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01262         MOVE GCBH-OS-HOSP-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01263         MOVE GCBH-OS-HOSP-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01264         MOVE GCBH-OS-HOSP-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01265         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01266         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01267                                                                   GBIDPGM 
01268  2000-4130.                                                       GBIDPGM 
01269      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01270         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01271         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01272                                                                   GBIDPGM 
01273      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01274      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01275                                                                   GBIDPGM 
01276      IF GCBH-OS-PROF-PMT-LVL-LN NOT = SPACES                      GBIDPGM 
01277         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01278         MOVE WS-RULE-4130-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01279         MOVE GCBH-OS-PROF-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01280         MOVE GCBH-OS-PROF-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01281         MOVE GCBH-OS-PROF-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01282         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01283         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01284                                                                   GBIDPGM 
01285 *MQ 11/03                                                         GBIDPGM 
01286  2000-4135.                                                       GBIDPGM 
01287      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01288         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01289         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01290                                                                   GBIDPGM 
01291      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01292      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01293                                                                   GBIDPGM 
01294      IF GCBH-OS-BCBS-PMT-LVL-LN  NOT = SPACES                     GBIDPGM 
01295         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01296         MOVE WS-RULE-4135-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01297         MOVE GCBH-OS-BCBS-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01298         MOVE GCBH-OS-BCBS-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01299         MOVE GCBH-OS-BCBS-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01300         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01301         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01302                                                                   GBIDPGM 
01303  2000-4140.                                                       GBIDPGM 
01304      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01305         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01306         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01307                                                                   GBIDPGM 
01308      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01309      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01310                                                                   GBIDPGM 
01311      IF GCBH-OD-HOSP-PMT-LVL-LN NOT = SPACES                      GBIDPGM 
01312         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01313         MOVE WS-RULE-4140-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01314         MOVE GCBH-OD-HOSP-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01315         MOVE GCBH-OD-HOSP-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01316         MOVE GCBH-OD-HOSP-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01317         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01318         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01319                                                                   GBIDPGM 
01320  2000-4150.                                                       GBIDPGM 
01321      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01322         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01323         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01324                                                                   GBIDPGM 
01325      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01326      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01327                                                                   GBIDPGM 
01328      IF GCBH-OD-PROF-PMT-LVL-LN NOT = SPACES                      GBIDPGM 
01329         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01330         MOVE WS-RULE-4150-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01331         MOVE GCBH-OD-PROF-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01332         MOVE GCBH-OD-PROF-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01333         MOVE GCBH-OD-PROF-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01334         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01335         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01336                                                                   GBIDPGM 
01337  2000-4160.                                                       GBIDPGM 
01338      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01339         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01340         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01341                                                                   GBIDPGM 
01342      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01343      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01344                                                                   GBIDPGM 
01345      IF GCBH-EAC-HOSP-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01346         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01347         MOVE WS-RULE-4160-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01348         MOVE GCBH-EAC-HOSP-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01349         MOVE GCBH-EAC-HOSP-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01350         MOVE GCBH-EAC-HOSP-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01351         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01352         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01353                                                                   GBIDPGM 
01354  2000-4170.                                                       GBIDPGM 
01355      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01356         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01357         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01358                                                                   GBIDPGM 
01359      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01360      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01361                                                                   GBIDPGM 
01362      IF GCBH-EAC-PROF-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01363         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01364         MOVE WS-RULE-4170-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01365         MOVE GCBH-EAC-PROF-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01366         MOVE GCBH-EAC-PROF-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01367         MOVE GCBH-EAC-PROF-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01368         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01369         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01370                                                                   GBIDPGM 
01371 *MQ 11/03                                                         GBIDPGM 
01372  2000-4175.                                                       GBIDPGM 
01373      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01374         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01375         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01376                                                                   GBIDPGM 
01377      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01378      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01379                                                                   GBIDPGM 
01380      IF GCBH-EAC-BCBS-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01381         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01382         MOVE WS-RULE-4175-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01383         MOVE GCBH-EAC-BCBS-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01384         MOVE GCBH-EAC-BCBS-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01385         MOVE GCBH-EAC-BCBS-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01386         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01387         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01388                                                                   GBIDPGM 
01389  2000-4180.                                                       GBIDPGM 
01390      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01391         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01392         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01393                                                                   GBIDPGM 
01394      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01395      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01396                                                                   GBIDPGM 
01397      IF GCBH-EMC-HOSP-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01398         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01399         MOVE WS-RULE-4180-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01400         MOVE GCBH-EMC-HOSP-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01401         MOVE GCBH-EMC-HOSP-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01402         MOVE GCBH-EMC-HOSP-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01403         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01404         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01405                                                                   GBIDPGM 
01406  2000-4190.                                                       GBIDPGM 
01407      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01408         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01409         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01410                                                                   GBIDPGM 
01411      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01412      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01413                                                                   GBIDPGM 
01414      IF GCBH-EMC-PROF-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01415         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01416         MOVE WS-RULE-4190-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01417         MOVE GCBH-EMC-PROF-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01418         MOVE GCBH-EMC-PROF-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01419         MOVE GCBH-EMC-PROF-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01420         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01421         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01422                                                                   GBIDPGM 
01423 *MQ 11/03                                                         GBIDPGM 
01424  2000-4195.                                                       GBIDPGM 
01425      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01426         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01427         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01428                                                                   GBIDPGM 
01429      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01430      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01431                                                                   GBIDPGM 
01432      IF GCBH-EMC-BCBS-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01433         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01434         MOVE WS-RULE-4195-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01435         MOVE GCBH-EMC-BCBS-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01436         MOVE GCBH-EMC-BCBS-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01437         MOVE GCBH-EMC-BCBS-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01438         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01439         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01440                                                                   GBIDPGM 
01441 *MQ 11/03                                                         GBIDPGM 
01442  2000-4196.                                                       GBIDPGM 
01443      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01444         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01445         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01446                                                                   GBIDPGM 
01447      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01448      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01449                                                                   GBIDPGM 
01450      IF GCBH-EAC-EMC-BC-PMT-LVL-LN   NOT = SPACES                 GBIDPGM 
01451         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01452         MOVE WS-RULE-4196-VALUE          TO WS-WLD-DESCRIPTION    GBIDPGM 
01453         MOVE GCBH-EAC-EMC-BC-PMT-LVL-IN  TO WS-WLD-IN-VALUE       GBIDPGM 
01454         MOVE GCBH-EAC-EMC-BC-PMT-LVL-OUT TO WS-WLD-OUT-VALUE      GBIDPGM 
01455         MOVE GCBH-EAC-EMC-BC-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE    GBIDPGM 
01456         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01457         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01458                                                                   GBIDPGM 
01459 *MQ 11/03                                                         GBIDPGM 
01460  2000-4197.                                                       GBIDPGM 
01461      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01462         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01463         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01464                                                                   GBIDPGM 
01465      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01466      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01467                                                                   GBIDPGM 
01468      IF GCBH-EAC-EMC-BS-PMT-LVL-LN   NOT = SPACES                 GBIDPGM 
01469         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01470         MOVE WS-RULE-4197-VALUE          TO WS-WLD-DESCRIPTION    GBIDPGM 
01471         MOVE GCBH-EAC-EMC-BS-PMT-LVL-IN  TO WS-WLD-IN-VALUE       GBIDPGM 
01472         MOVE GCBH-EAC-EMC-BS-PMT-LVL-OUT TO WS-WLD-OUT-VALUE      GBIDPGM 
01473         MOVE GCBH-EAC-EMC-BS-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE    GBIDPGM 
01474         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01475         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01476                                                                   GBIDPGM 
01477 *MQ 11/03                                                         GBIDPGM 
01478  2000-4198.                                                       GBIDPGM 
01479      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01480         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01481         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01482                                                                   GBIDPGM 
01483      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01484      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01485                                                                   GBIDPGM 
01486      IF GCBH-EAC-EMC-BCBS-PMT-LVL-LN NOT = SPACES                 GBIDPGM 
01487         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01488         MOVE WS-RULE-4198-VALUE           TO WS-WLD-DESCRIPTION   GBIDPGM 
01489         MOVE GCBH-EAC-EMC-BCBS-PMT-LVL-IN  TO WS-WLD-IN-VALUE     GBIDPGM 
01490         MOVE GCBH-EAC-EMC-BCBS-PMT-LVL-OUT TO WS-WLD-OUT-VALUE    GBIDPGM 
01491         MOVE GCBH-EAC-EMC-BCBS-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE  GBIDPGM 
01492         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01493         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01494                                                                   GBIDPGM 
01495  2000-4200.                                                       GBIDPGM 
01496      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01497         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01498         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01499                                                                   GBIDPGM 
01500      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01501      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01502                                                                   GBIDPGM 
01503      IF GCBH-SUPP-ACC-CARE-LN NOT = SPACES                        GBIDPGM 
01504         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01505         MOVE WS-RULE-4200-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01506         MOVE GCBH-SUPP-ACC-CARE-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01507         MOVE GCBH-SUPP-ACC-CARE-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01508         MOVE GCBH-SUPP-ACC-CARE-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01509         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01510         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01511                                                                   GBIDPGM 
01512  2000-4210.                                                       GBIDPGM 
01513      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01514         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01515         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01516                                                                   GBIDPGM 
01517      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01518      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01519                                                                   GBIDPGM 
01520      IF GCBH-MED-SURG-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
01521         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01522         MOVE WS-RULE-4210-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01523         MOVE GCBH-MED-SURG-PMT-LVL-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01524         MOVE GCBH-MED-SURG-PMT-LVL-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01525         MOVE GCBH-MED-SURG-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01526         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01527         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01528                                                                   GBIDPGM 
01529 *MQ 12/03/03                                                      GBIDPGM 
01530  2000-4215.                                                       GBIDPGM 
01531      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01532         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01533         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01534                                                                   GBIDPGM 
01535      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01536      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01537                                                                   GBIDPGM 
01538      IF GCBH-HOSP-MEDSURG-PMT-LVL-LN  NOT = SPACES                GBIDPGM 
01539         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01540         MOVE WS-RULE-4215-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01541         MOVE GCBH-HOSP-MEDSURG-PMT-LVL-IN  TO WS-WLD-IN-VALUE     GBIDPGM 
01542         MOVE GCBH-HOSP-MEDSURG-PMT-LVL-OUT TO WS-WLD-OUT-VALUE    GBIDPGM 
01543         MOVE GCBH-HOSP-MEDSURG-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE  GBIDPGM 
01544         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01545         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01546                                                                   GBIDPGM 
01547  2000-4220.                                                       GBIDPGM 
01548      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01549         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01550         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01551                                                                   GBIDPGM 
01552      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01553      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01554                                                                   GBIDPGM 
01555      IF GCBH-THER-MAX-COMB-LN NOT = SPACES                        GBIDPGM 
01556         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01557         MOVE WS-RULE-4220-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01558         MOVE GCBH-THER-MAX-COMB-IN TO WS-WLD-IN-VALUE             GBIDPGM 
01559         MOVE GCBH-THER-MAX-COMB-OUT TO WS-WLD-OUT-VALUE           GBIDPGM 
01560         MOVE GCBH-THER-MAX-COMB-OTH TO WS-WLD-OTHER-VALUE         GBIDPGM 
01561         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01562         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01563                                                                   GBIDPGM 
01564  2000-4230.                                                       GBIDPGM 
01565      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01566         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01567         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01568                                                                   GBIDPGM 
01569      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01570      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01571                                                                   GBIDPGM 
01572      IF GCBH-FUNC-OC-THER-MAX-LN NOT = SPACES                     GBIDPGM 
01573         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01574         MOVE WS-RULE-4230-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01575         MOVE GCBH-FUNC-OC-THER-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01576         MOVE GCBH-FUNC-OC-THER-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01577         MOVE GCBH-FUNC-OC-THER-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01578         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01579         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01580                                                                   GBIDPGM 
01581  2000-4240.                                                       GBIDPGM 
01582      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01583         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01584         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01585                                                                   GBIDPGM 
01586      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01587      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01588                                                                   GBIDPGM 
01589      IF GCBH-PHYS-ME-THER-MAX-LN NOT = SPACES                     GBIDPGM 
01590         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01591         MOVE WS-RULE-4240-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01592         MOVE GCBH-PHYS-ME-THER-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01593         MOVE GCBH-PHYS-ME-THER-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01594         MOVE GCBH-PHYS-ME-THER-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01595         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01596         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01597                                                                   GBIDPGM 
01598  2000-4250.                                                       GBIDPGM 
01599      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01600         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01601         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01602                                                                   GBIDPGM 
01603      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01604      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01605                                                                   GBIDPGM 
01606      IF GCBH-SPEECH-THER-MAX-LN NOT = SPACES                      GBIDPGM 
01607         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01608         MOVE WS-RULE-4250-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01609         MOVE GCBH-SPEECH-THER-MAX-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01610         MOVE GCBH-SPEECH-THER-MAX-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01611         MOVE GCBH-SPEECH-THER-MAX-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01612         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01613         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01614                                                                   GBIDPGM 
01615  2000-4260.                                                       GBIDPGM 
01616      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01617         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01618         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01619                                                                   GBIDPGM 
01620      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01621      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01622                                                                   GBIDPGM 
01623      IF GCBH-TMJ-LIFE-MAX-LN NOT = SPACES                         GBIDPGM 
01624         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01625         MOVE WS-RULE-4260-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01626         MOVE GCBH-TMJ-LIFE-MAX-IN TO WS-WLD-IN-VALUE              GBIDPGM 
01627         MOVE GCBH-TMJ-LIFE-MAX-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
01628         MOVE GCBH-TMJ-LIFE-MAX-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
01629         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01630         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01631                                                                   GBIDPGM 
01632  2000-4270.                                                       GBIDPGM 
01633      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01634         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01635         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01636                                                                   GBIDPGM 
01637      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01638      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01639                                                                   GBIDPGM 
01640      IF GCBH-PRI-DUTY-NUR-MAX-LN NOT = SPACES                     GBIDPGM 
01641         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01642         MOVE WS-RULE-4270-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01643         MOVE GCBH-PRI-DUTY-NUR-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01644         MOVE GCBH-PRI-DUTY-NUR-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01645         MOVE GCBH-PRI-DUTY-NUR-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01646         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01647         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01648                                                                   GBIDPGM 
01649 *MQ 11/03                                                         GBIDPGM 
01650  2000-4275.                                                       GBIDPGM 
01651      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01652         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01653         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01654                                                                   GBIDPGM 
01655      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01656      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01657                                                                   GBIDPGM 
01658      IF GCBH-SKILL-NUR-BP-MAX-LN NOT = SPACES                     GBIDPGM 
01659         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01660         MOVE WS-RULE-4275-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01661         MOVE GCBH-SKILL-NUR-BP-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01662         MOVE GCBH-SKILL-NUR-BP-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01663         MOVE GCBH-SKILL-NUR-BP-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01664         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01665         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01666                                                                   GBIDPGM 
01667  2000-4280.                                                       GBIDPGM 
01668      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01669         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01670         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01671                                                                   GBIDPGM 
01672      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01673      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01674                                                                   GBIDPGM 
01675      IF GCBH-CHIRO-SERV-MAX-LN NOT = SPACES                       GBIDPGM 
01676         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01677         MOVE WS-RULE-4280-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01678         MOVE GCBH-CHIRO-SERV-MAX-IN TO WS-WLD-IN-VALUE            GBIDPGM 
01679         MOVE GCBH-CHIRO-SERV-MAX-OUT TO WS-WLD-OUT-VALUE          GBIDPGM 
01680         MOVE GCBH-CHIRO-SERV-MAX-OTH TO WS-WLD-OTHER-VALUE        GBIDPGM 
01681         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01682         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01683                                                                   GBIDPGM 
01684  2000-4290.                                                       GBIDPGM 
01685      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01686         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01687         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01688                                                                   GBIDPGM 
01689      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01690      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01691                                                                   GBIDPGM 
01692      IF GCBH-CHIRO-PROV-MAX-LN NOT = SPACES                       GBIDPGM 
01693         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01694         MOVE WS-RULE-4290-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01695         MOVE GCBH-CHIRO-PROV-MAX-IN TO WS-WLD-IN-VALUE            GBIDPGM 
01696         MOVE GCBH-CHIRO-PROV-MAX-OUT TO WS-WLD-OUT-VALUE          GBIDPGM 
01697         MOVE GCBH-CHIRO-PROV-MAX-OTH TO WS-WLD-OTHER-VALUE        GBIDPGM 
01698         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01699         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01700                                                                   GBIDPGM 
01701  2000-4300.                                                       GBIDPGM 
01702      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01703         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01704         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01705                                                                   GBIDPGM 
01706      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01707      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01708                                                                   GBIDPGM 
01709      IF GCBH-WELL-CARE-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
01710         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01711         MOVE WS-RULE-4300-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01712         MOVE GCBH-WELL-CARE-PMT-LVL-IN TO WS-WLD-IN-VALUE         GBIDPGM 
01713         MOVE GCBH-WELL-CARE-PMT-LVL-OUT TO WS-WLD-OUT-VALUE       GBIDPGM 
01714         MOVE GCBH-WELL-CARE-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE     GBIDPGM 
01715         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01716         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01717                                                                   GBIDPGM 
01718 *MQ 11/03                                                         GBIDPGM 
01719  2000-4305.                                                       GBIDPGM 
01720      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01721         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01722         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01723                                                                   GBIDPGM 
01724      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01725      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01726                                                                   GBIDPGM 
01727      IF GCBH-WELL-AD-CARE-PMT-LVL-LN  NOT = SPACES                GBIDPGM 
01728         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01729         MOVE WS-RULE-4305-VALUE           TO WS-WLD-DESCRIPTION   GBIDPGM 
01730         MOVE GCBH-WELL-AD-CARE-PMT-LVL-IN   TO WS-WLD-IN-VALUE    GBIDPGM 
01731         MOVE GCBH-WELL-AD-CARE-PMT-LVL-OUT  TO WS-WLD-OUT-VALUE   GBIDPGM 
01732         MOVE GCBH-WELL-AD-CARE-PMT-LVL-OTH  TO WS-WLD-OTHER-VALUE GBIDPGM 
01733         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01734         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01735                                                                   GBIDPGM 
01736  2000-4310.                                                       GBIDPGM 
01737      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01738         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01739         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01740                                                                   GBIDPGM 
01741      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01742      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01743                                                                   GBIDPGM 
01744      IF GCBH-WELL-AD-CARE-MAX-LN NOT = SPACES                     GBIDPGM 
01745         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01746         MOVE WS-RULE-4310-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01747         MOVE GCBH-WELL-AD-CARE-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01748         MOVE GCBH-WELL-AD-CARE-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01749         MOVE GCBH-WELL-AD-CARE-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01750         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01751         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01752                                                                   GBIDPGM 
01753 *MQ 11/03                                                         GBIDPGM 
01754  2000-4315.                                                       GBIDPGM 
01755      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01756         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01757         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01758                                                                   GBIDPGM 
01759      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01760      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01761                                                                   GBIDPGM 
01762      IF GCBH-WELL-AD-CARE-BP-MAX-LN  NOT = SPACES                 GBIDPGM 
01763         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01764         MOVE WS-RULE-4315-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01765         MOVE GCBH-WELL-AD-CARE-BP-MAX-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01766         MOVE GCBH-WELL-AD-CARE-BP-MAX-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01767         MOVE GCBH-WELL-AD-CARE-BP-MAX-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01768         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01769         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01770                                                                   GBIDPGM 
01771  2000-4320.                                                       GBIDPGM 
01772      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01773         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01774         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01775                                                                   GBIDPGM 
01776      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01777      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01778                                                                   GBIDPGM 
01779      IF GCBH-WELL-CH-CARE-MAX-LN NOT = SPACES                     GBIDPGM 
01780         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01781         MOVE WS-RULE-4320-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01782         MOVE GCBH-WELL-CH-CARE-MAX-IN TO WS-WLD-IN-VALUE          GBIDPGM 
01783         MOVE GCBH-WELL-CH-CARE-MAX-OUT TO WS-WLD-OUT-VALUE        GBIDPGM 
01784         MOVE GCBH-WELL-CH-CARE-MAX-OTH TO WS-WLD-OTHER-VALUE      GBIDPGM 
01785         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01786         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01787                                                                   GBIDPGM 
01788  2000-4325.                                                       GBIDPGM 
01789      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01790         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01791         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01792                                                                   GBIDPGM 
01793      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01794      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01795                                                                   GBIDPGM 
01796      IF GCBH-WELL-CH-CARE-PMT-LVL-LN NOT = SPACES                 GBIDPGM 
01797         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01798         MOVE WS-RULE-4325-VALUE            TO WS-WLD-DESCRIPTION  GBIDPGM 
01799         MOVE GCBH-WELL-CH-CARE-PMT-LVL-IN  TO WS-WLD-IN-VALUE     GBIDPGM 
01800         MOVE GCBH-WELL-CH-CARE-PMT-LVL-OUT TO WS-WLD-OUT-VALUE    GBIDPGM 
01801         MOVE GCBH-WELL-CH-CARE-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE  GBIDPGM 
01802         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01803         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01804                                                                   GBIDPGM 
01805  2000-4330.                                                       GBIDPGM 
01806      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01807         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01808         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01809                                                                   GBIDPGM 
01810      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01811      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01812                                                                   GBIDPGM 
01813      IF GCBH-OTH-COV-PMT-LVL-LN NOT = SPACES                      GBIDPGM 
01814         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01815         MOVE WS-RULE-4330-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01816         MOVE GCBH-OTH-COV-PMT-LVL-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01817         MOVE GCBH-OTH-COV-PMT-LVL-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01818         MOVE GCBH-OTH-COV-PMT-LVL-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01819         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01820         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01821                                                                   GBIDPGM 
01822 *MQ 11/03                                                         GBIDPGM 
01823  2000-4370.                                                       GBIDPGM 
01824      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01825         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01826         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01827                                                                   GBIDPGM 
01828      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01829      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01830                                                                   GBIDPGM 
01831      IF GCBH-HEAR-AID-BP-MAX-LN NOT = SPACES                      GBIDPGM 
01832         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01833         MOVE WS-RULE-4370-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01834         MOVE GCBH-HEAR-AID-BP-MAX-IN TO WS-WLD-IN-VALUE           GBIDPGM 
01835         MOVE GCBH-HEAR-AID-BP-MAX-OUT TO WS-WLD-OUT-VALUE         GBIDPGM 
01836         MOVE GCBH-HEAR-AID-BP-MAX-OTH TO WS-WLD-OTHER-VALUE       GBIDPGM 
01837         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01838         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01839                                                                   GBIDPGM 
01840 *MQ 11/03                                                         GBIDPGM 
01841  2000-4380.                                                       GBIDPGM 
01842      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01843         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01844         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01845                                                                   GBIDPGM 
01846      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01847      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01848                                                                   GBIDPGM 
01849      IF GCBH-NON-PLAN-PMT-LVL-LN  NOT = SPACES                    GBIDPGM 
01850         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01851         MOVE WS-RULE-4380-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01852         MOVE GCBH-NON-PLAN-PMT-LVL-IN   TO WS-WLD-IN-VALUE        GBIDPGM 
01853         MOVE GCBH-NON-PLAN-PMT-LVL-OUT  TO WS-WLD-OUT-VALUE       GBIDPGM 
01854         MOVE GCBH-NON-PLAN-PMT-LVL-OTH  TO WS-WLD-OTHER-VALUE     GBIDPGM 
01855         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01856         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01857                                                                   GBIDPGM 
01858 *MQ 11/03                                                         GBIDPGM 
01859  2000-4390.                                                       GBIDPGM 
01860      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01861         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01862         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01863                                                                   GBIDPGM 
01864      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01865      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01866                                                                   GBIDPGM 
01867      IF GCBH-CON-LENS-BP-MAX-LN   NOT = SPACES                    GBIDPGM 
01868         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01869         MOVE WS-RULE-4390-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01870         MOVE GCBH-CON-LENS-BP-MAX-IN    TO WS-WLD-IN-VALUE        GBIDPGM 
01871         MOVE GCBH-CON-LENS-BP-MAX-OUT   TO WS-WLD-OUT-VALUE       GBIDPGM 
01872         MOVE GCBH-CON-LENS-BP-MAX-OTH   TO WS-WLD-OTHER-VALUE     GBIDPGM 
01873         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01874         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01875                                                                   GBIDPGM 
01876 *MQ 11/03                                                         GBIDPGM 
01877  2000-4392.                                                       GBIDPGM 
01878      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01879         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01880         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01881                                                                   GBIDPGM 
01882      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01883      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01884                                                                   GBIDPGM 
01885      IF GCBH-FRAME-BP-MAX-LN      NOT = SPACES                    GBIDPGM 
01886         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01887         MOVE WS-RULE-4392-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01888         MOVE GCBH-FRAME-BP-MAX-IN       TO WS-WLD-IN-VALUE        GBIDPGM 
01889         MOVE GCBH-FRAME-BP-MAX-OUT      TO WS-WLD-OUT-VALUE       GBIDPGM 
01890         MOVE GCBH-FRAME-BP-MAX-OTH      TO WS-WLD-OTHER-VALUE     GBIDPGM 
01891         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01892         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01893                                                                   GBIDPGM 
01894 *MQ 11/03                                                         GBIDPGM 
01895  2000-4394.                                                       GBIDPGM 
01896      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01897         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01898         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01899                                                                   GBIDPGM 
01900      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01901      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01902                                                                   GBIDPGM 
01903      IF GCBH-VIS-EX-BP-MAX-LN     NOT = SPACES                    GBIDPGM 
01904         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01905         MOVE WS-RULE-4394-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01906         MOVE GCBH-VIS-EX-BP-MAX-IN      TO WS-WLD-IN-VALUE        GBIDPGM 
01907         MOVE GCBH-VIS-EX-BP-MAX-OUT     TO WS-WLD-OUT-VALUE       GBIDPGM 
01908         MOVE GCBH-VIS-EX-BP-MAX-OTH     TO WS-WLD-OTHER-VALUE     GBIDPGM 
01909         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01910         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01911                                                                   GBIDPGM 
01912 *MQ 11/03                                                         GBIDPGM 
01913  2000-4396.                                                       GBIDPGM 
01914      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01915         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01916         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01917                                                                   GBIDPGM 
01918      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01919      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01920                                                                   GBIDPGM 
01921      IF GCBH-LENS-BP-MAX-LN       NOT = SPACES                    GBIDPGM 
01922         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01923         MOVE WS-RULE-4396-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01924         MOVE GCBH-LENS-BP-MAX-IN        TO WS-WLD-IN-VALUE        GBIDPGM 
01925         MOVE GCBH-LENS-BP-MAX-OUT       TO WS-WLD-OUT-VALUE       GBIDPGM 
01926         MOVE GCBH-LENS-BP-MAX-OTH       TO WS-WLD-OTHER-VALUE     GBIDPGM 
01927         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01928         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01929                                                                   GBIDPGM 
01930 *MQ 04/07/04                                                      GBIDPGM 
01931  2000-4398.                                                       GBIDPGM 
01932      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01933         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01934         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01935                                                                   GBIDPGM 
01936      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01937      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01938                                                                   GBIDPGM 
01939      IF GCBH-VIS-HW-BP-MAX-LN     NOT = SPACES                    GBIDPGM 
01940         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01941         MOVE WS-RULE-4398-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01942         MOVE GCBH-VIS-HW-BP-MAX-IN      TO WS-WLD-IN-VALUE        GBIDPGM 
01943         MOVE GCBH-VIS-HW-BP-MAX-OUT     TO WS-WLD-OUT-VALUE       GBIDPGM 
01944         MOVE GCBH-VIS-HW-BP-MAX-OTH     TO WS-WLD-OTHER-VALUE     GBIDPGM 
01945         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01946         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01947                                                                   GBIDPGM 
01948                                                                   GBIDPGM 
01949 *MQ 11/03                                                         GBIDPGM 
01950  2000-5040.                                                       GBIDPGM 
01951      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01952         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01953         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01954                                                                   GBIDPGM 
01955      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01956      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01957                                                                   GBIDPGM 
01958      IF GCBH-HOSP-CARE-BP-MAX-LN  NOT = SPACES                    GBIDPGM 
01959         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01960         MOVE WS-RULE-5040-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01961         MOVE GCBH-HOSP-CARE-BP-MAX-IN   TO WS-WLD-IN-VALUE        GBIDPGM 
01962         MOVE GCBH-HOSP-CARE-BP-MAX-OUT  TO WS-WLD-OUT-VALUE       GBIDPGM 
01963         MOVE GCBH-HOSP-CARE-BP-MAX-OTH  TO WS-WLD-OTHER-VALUE     GBIDPGM 
01964         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01965         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01966                                                                   GBIDPGM 
01967                                                                   GBIDPGM 
01968 *MQ 11/03                                                         GBIDPGM 
01969  2000-5050.                                                       GBIDPGM 
01970      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01971         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01972         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01973                                                                   GBIDPGM 
01974      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01975      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01976                                                                   GBIDPGM 
01977      IF GCBH-COOR-HM-CARE-BP-MAX-LN  NOT = SPACES                 GBIDPGM 
01978         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
01979         MOVE WS-RULE-5050-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
01980         MOVE GCBH-COOR-HM-CARE-BP-MAX-IN  TO WS-WLD-IN-VALUE      GBIDPGM 
01981         MOVE GCBH-COOR-HM-CARE-BP-MAX-OUT TO WS-WLD-OUT-VALUE     GBIDPGM 
01982         MOVE GCBH-COOR-HM-CARE-BP-MAX-OTH TO WS-WLD-OTHER-VALUE   GBIDPGM 
01983         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
01984         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
01985                                                                   GBIDPGM 
02334 *MQ 05/11/05                                                      GBIDPGM 
02335  2000-5052.                                                       GBIDPGM 
02336      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02337         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02338         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02339                                                                   GBIDPGM 
02340      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02341      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02342                                                                   GBIDPGM 
02343      IF GCBH-COOR-HM-CARE-DAY-MAX-LN NOT = SPACES                 GBIDPGM 
02344         PERFORM 4002-GEN-BEN-HEADING-CHECK                        GBIDPGM 
02345         MOVE WS-RULE-5052-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
02346         MOVE GCBH-COOR-HM-CARE-DAY-MAX-IN  TO WS-WLD-IN-VALUE     GBIDPGM 
02347         MOVE GCBH-COOR-HM-CARE-DAY-MAX-OUT TO WS-WLD-OUT-VALUE    GBIDPGM 
02348         MOVE GCBH-COOR-HM-CARE-DAY-MAX-OTH TO WS-WLD-OTHER-VALUE  GBIDPGM 
02349         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02350         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02351                                                                   GBIDPGM 
01986                                                                   GBIDPGM 
01987  2000-4335.                                                       GBIDPGM 
01988 **** RESET HEADING SWITCH ****************************************GBIDPGM 
01989      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
01990                                                                   GBIDPGM 
01991      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
01992         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
01993         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
01994                                                                   GBIDPGM 
01995      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
01996      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
01997                                                                   GBIDPGM 
01998      IF GCBH-DEPENDENT-MAX-AGE NOT = SPACES                       GBIDPGM 
01999         PERFORM 4004-ADMIN-BENEFITS-CHECK                         GBIDPGM 
02000         MOVE WS-RULE-4335-VALUE     TO WS-WLD-DESCRIPTION         GBIDPGM 
02001         MOVE GCBH-DEPENDENT-MAX-AGE TO WS-AGE                     GBIDPGM 
02002         MOVE WS-AGE-FIELD          TO WS-WLD-OTHER-VALUE          GBIDPGM 
02003         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02004         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02005                                                                   GBIDPGM 
02006  2000-4336.                                                       GBIDPGM 
02007      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02008         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02009         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02010                                                                   GBIDPGM 
02011      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02012      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02013                                                                   GBIDPGM 
02014      IF GCBH-STUDENT-MAX-AGE NOT = SPACES                         GBIDPGM 
02015         PERFORM 4004-ADMIN-BENEFITS-CHECK                         GBIDPGM 
02016         MOVE WS-RULE-4336-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
02017         MOVE GCBH-STUDENT-MAX-AGE  TO WS-AGE                      GBIDPGM 
02018         MOVE WS-AGE-FIELD          TO WS-WLD-OTHER-VALUE          GBIDPGM 
02019         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02020         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02021                                                                   GBIDPGM 
02022  2000-4337.                                                       GBIDPGM 
02023      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02024         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02025         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02026                                                                   GBIDPGM 
02027      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02028      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02029                                                                   GBIDPGM 
02030      IF GCBH-WAITING-PER-LN NOT = SPACES                          GBIDPGM 
02031         PERFORM 4004-ADMIN-BENEFITS-CHECK                         GBIDPGM 
02032         MOVE WS-RULE-4337-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
02033         MOVE GCG-BC-WAITG-PERD-MEM-DAYS TO WS-WPL-MEM-DAYS        GBIDPGM 
02034         MOVE GCG-BC-WAITG-PERD-SPS-DAYS TO WS-WPL-SPS-DAYS        GBIDPGM 
02035         MOVE GCG-BC-WAITG-PERD-DEP-DAYS TO WS-WPL-DEP-DAYS        GBIDPGM 
02036         MOVE WS-HOLD-WAITING-PERD-LINE                            GBIDPGM 
02037                                    TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02038         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02039                                                                   GBIDPGM 
02040  2000-4340.                                                       GBIDPGM 
02041 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02042      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02043                                                                   GBIDPGM 
02044      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02045         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02046         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02047                                                                   GBIDPGM 
02048      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02049      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02050                                                                   GBIDPGM 
02051      IF GCBH-MSA-SANC-COINS-LN NOT = SPACES                       GBIDPGM 
02052         PERFORM 4005-COST-CONTAINMENT                             GBIDPGM 
02053         MOVE WS-RULE-4340-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
02054         MOVE GCBH-MSA-SANC-COINS-IN TO WS-WLD-IN-VALUE            GBIDPGM 
02055         MOVE GCBH-MSA-SANC-COINS-OUT TO WS-WLD-OUT-VALUE          GBIDPGM 
02056         MOVE GCBH-MSA-SANC-COINS-OTH TO WS-WLD-OTHER-VALUE        GBIDPGM 
02057         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02058         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02059                                                                   GBIDPGM 
02060  2000-4350.                                                       GBIDPGM 
02061      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02062         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02063         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02064                                                                   GBIDPGM 
02065      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02066      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02067                                                                   GBIDPGM 
02068      IF GCBH-MSA-SANC-DED-LN NOT = SPACES                         GBIDPGM 
02069         PERFORM 4005-COST-CONTAINMENT                             GBIDPGM 
02070         MOVE WS-RULE-4350-VALUE    TO WS-WLD-DESCRIPTION          GBIDPGM 
02071         MOVE GCBH-MSA-SANC-DED-IN TO WS-WLD-IN-VALUE              GBIDPGM 
02072         MOVE GCBH-MSA-SANC-DED-OUT TO WS-WLD-OUT-VALUE            GBIDPGM 
02073         MOVE GCBH-MSA-SANC-DED-OTH TO WS-WLD-OTHER-VALUE          GBIDPGM 
02074         MOVE WS-WRITE-LINE         TO WS-TS-VALUE (WS-TS-LINE-SUB)GBIDPGM 
02075         ADD  +1                    TO WS-TS-LINE-SUB.             GBIDPGM 
02076                                                                   GBIDPGM 
02077  2000-4650.                                                       GBIDPGM 
02078 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02079      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02080                                                                   GBIDPGM 
02081      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02082         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02083         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02084                                                                   GBIDPGM 
02085      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02086      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02087                                                                   GBIDPGM 
02088      IF GCBH-IP-BC-MH-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02089         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02090         MOVE WS-RULE-4650-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02091         MOVE GCBH-IP-BC-MH-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02092         MOVE GCBH-IP-BC-MH-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02093         MOVE GCBH-IP-BC-MH-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02094         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02095         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02096                                                                   GBIDPGM 
02097  2000-4660.                                                       GBIDPGM 
02098      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02099         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02100         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02101                                                                   GBIDPGM 
02102      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02103      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02104                                                                   GBIDPGM 
02105      IF GCBH-IP-BS-MH-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02106         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02107         MOVE WS-RULE-4660-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02108         MOVE GCBH-IP-BS-MH-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02109         MOVE GCBH-IP-BS-MH-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02110         MOVE GCBH-IP-BS-MH-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02111         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02112         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02113                                                                   GBIDPGM 
02114  2000-4670.                                                       GBIDPGM 
02115      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02116         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02117         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02118                                                                   GBIDPGM 
02119      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02120      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02121                                                                   GBIDPGM 
02122      IF GCBH-IP-BCBS-MH-LIFE-MAX-LN NOT = SPACES                  GBIDPGM 
02123         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02124         MOVE WS-RULE-4670-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02125         MOVE GCBH-IP-BCBS-MH-LIFE-MAX-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
02126         MOVE GCBH-IP-BCBS-MH-LIFE-MAX-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
02127         MOVE GCBH-IP-BCBS-MH-LIFE-MAX-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
02128         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02129         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02130                                                                   GBIDPGM 
02131  2000-4620.                                                       GBIDPGM 
02132      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02133         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02134         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02135                                                                   GBIDPGM 
02136      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02137      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02138                                                                   GBIDPGM 
02139      IF GCBH-IP-BC-MH-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02140         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02141         MOVE WS-RULE-4620-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02142         MOVE GCBH-IP-BC-MH-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02143         MOVE GCBH-IP-BC-MH-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02144         MOVE GCBH-IP-BC-MH-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02145         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02146         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02147                                                                   GBIDPGM 
02148  2000-4630.                                                       GBIDPGM 
02149      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02150         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02151         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02152                                                                   GBIDPGM 
02153      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02154      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02155                                                                   GBIDPGM 
02156      IF GCBH-IP-BS-MH-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02157         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02158         MOVE WS-RULE-4630-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02159         MOVE GCBH-IP-BS-MH-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02160         MOVE GCBH-IP-BS-MH-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02161         MOVE GCBH-IP-BS-MH-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02162         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02163         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02164                                                                   GBIDPGM 
02165  2000-4640.                                                       GBIDPGM 
02166      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02167         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02168         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02169                                                                   GBIDPGM 
02170      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02171      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02172                                                                   GBIDPGM 
02173      IF GCBH-IP-BCBS-MH-BP-MAX-LN NOT = SPACES                    GBIDPGM 
02174         PERFORM 4020-HDG-MH-MAX-IP                                GBIDPGM 
02175         MOVE WS-RULE-4640-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02176         MOVE GCBH-IP-BCBS-MH-BP-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02177         MOVE GCBH-IP-BCBS-MH-BP-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02178         MOVE GCBH-IP-BCBS-MH-BP-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02179         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02180         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02181                                                                   GBIDPGM 
02182  2000-4740.                                                       GBIDPGM 
02183 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02184      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02185                                                                   GBIDPGM 
02186      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02187         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02188         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02189                                                                   GBIDPGM 
02190      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02191      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02192                                                                   GBIDPGM 
02193      IF GCBH-IP-BC-SA-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02194         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02195         MOVE WS-RULE-4740-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02196         MOVE GCBH-IP-BC-SA-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02197         MOVE GCBH-IP-BC-SA-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02198         MOVE GCBH-IP-BC-SA-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02199         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02200         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02201                                                                   GBIDPGM 
02202  2000-4750.                                                       GBIDPGM 
02203      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02204         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02205         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02206                                                                   GBIDPGM 
02207      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02208      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02209                                                                   GBIDPGM 
02210      IF GCBH-IP-BS-SA-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02211         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02212         MOVE WS-RULE-4750-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02213         MOVE GCBH-IP-BS-SA-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02214         MOVE GCBH-IP-BS-SA-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02215         MOVE GCBH-IP-BS-SA-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02216         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02217         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02218                                                                   GBIDPGM 
02219  2000-4760.                                                       GBIDPGM 
02220      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02221         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02222         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02223                                                                   GBIDPGM 
02224      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02225      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02226                                                                   GBIDPGM 
02227      IF GCBH-IP-BCBS-SA-LIFE-MAX-LN NOT = SPACES                  GBIDPGM 
02228         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02229         MOVE WS-RULE-4760-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02230         MOVE GCBH-IP-BCBS-SA-LIFE-MAX-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
02231         MOVE GCBH-IP-BCBS-SA-LIFE-MAX-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
02232         MOVE GCBH-IP-BCBS-SA-LIFE-MAX-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
02233         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02234         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02235                                                                   GBIDPGM 
02236  2000-4710.                                                       GBIDPGM 
02237      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02238         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02239         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02240                                                                   GBIDPGM 
02241      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02242      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02243                                                                   GBIDPGM 
02244      IF GCBH-IP-BC-SA-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02245         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02246         MOVE WS-RULE-4710-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02247         MOVE GCBH-IP-BC-SA-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02248         MOVE GCBH-IP-BC-SA-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02249         MOVE GCBH-IP-BC-SA-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02250         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02251         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02252                                                                   GBIDPGM 
02253  2000-4720.                                                       GBIDPGM 
02254      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02255         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02256         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02257                                                                   GBIDPGM 
02258      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02259      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02260                                                                   GBIDPGM 
02261      IF GCBH-IP-BS-SA-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02262         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02263         MOVE WS-RULE-4720-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02264         MOVE GCBH-IP-BS-SA-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02265         MOVE GCBH-IP-BS-SA-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02266         MOVE GCBH-IP-BS-SA-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02267         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02268         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02269                                                                   GBIDPGM 
02270  2000-4730.                                                       GBIDPGM 
02271      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02272         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02273         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02274                                                                   GBIDPGM 
02275      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02276      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02277                                                                   GBIDPGM 
02278      IF GCBH-IP-BCBS-SA-BP-MAX-LN NOT = SPACES                    GBIDPGM 
02279         PERFORM 4021-HDG-SA-MAX-IP                                GBIDPGM 
02280         MOVE WS-RULE-4730-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02281         MOVE GCBH-IP-BCBS-SA-BP-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02282         MOVE GCBH-IP-BCBS-SA-BP-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02283         MOVE GCBH-IP-BCBS-SA-BP-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02284         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02285         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02286                                                                   GBIDPGM 
02287  2000-4560.                                                       GBIDPGM 
02288 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02289      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02290                                                                   GBIDPGM 
02291      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02292         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02293         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02294                                                                   GBIDPGM 
02295      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02296      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02297                                                                   GBIDPGM 
02298      IF GCBH-IP-BC-MSA-LIFE-MAX-LN NOT = SPACES                   GBIDPGM 
02299         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02300         MOVE WS-RULE-4560-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02301         MOVE GCBH-IP-BC-MSA-LIFE-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02302         MOVE GCBH-IP-BC-MSA-LIFE-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02303         MOVE GCBH-IP-BC-MSA-LIFE-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02304         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02305         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02306                                                                   GBIDPGM 
02307  2000-4570.                                                       GBIDPGM 
02308      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02309         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02310         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02311                                                                   GBIDPGM 
02312      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02313      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02314                                                                   GBIDPGM 
02315      IF GCBH-IP-BS-MSA-LIFE-MAX-LN NOT = SPACES                   GBIDPGM 
02316         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02317         MOVE WS-RULE-4570-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02318         MOVE GCBH-IP-BS-MSA-LIFE-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02319         MOVE GCBH-IP-BS-MSA-LIFE-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02320         MOVE GCBH-IP-BS-MSA-LIFE-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02321         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02322         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02323                                                                   GBIDPGM 
02324  2000-4580.                                                       GBIDPGM 
02325      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02326         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02327         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02328                                                                   GBIDPGM 
02329      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02330      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02331                                                                   GBIDPGM 
02332      IF GCBH-IP-BCBS-MSA-LIFE-MAX-LN NOT = SPACES                 GBIDPGM 
02333         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02334         MOVE WS-RULE-4580-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02335         MOVE GCBH-IP-BCBS-MSA-LIFE-MAX-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02336         MOVE GCBH-IP-BCBS-MSA-LIFE-MAX-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02337         MOVE GCBH-IP-BCBS-MSA-LIFE-MAX-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02338         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02339         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02340                                                                   GBIDPGM 
02341  2000-4530.                                                       GBIDPGM 
02342      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02343         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02344         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02345                                                                   GBIDPGM 
02346      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02347      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02348                                                                   GBIDPGM 
02349      IF GCBH-IP-BC-MSA-BP-MAX-LN NOT = SPACES                     GBIDPGM 
02350         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02351         MOVE WS-RULE-4530-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02352         MOVE GCBH-IP-BC-MSA-BP-MAX-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02353         MOVE GCBH-IP-BC-MSA-BP-MAX-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02354         MOVE GCBH-IP-BC-MSA-BP-MAX-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02355         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02356         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02357                                                                   GBIDPGM 
02358  2000-4540.                                                       GBIDPGM 
02359      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02360         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02361         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02362                                                                   GBIDPGM 
02363      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02364      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02365                                                                   GBIDPGM 
02366      IF GCBH-IP-BS-MSA-BP-MAX-LN NOT = SPACES                     GBIDPGM 
02367         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02368         MOVE WS-RULE-4540-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02369         MOVE GCBH-IP-BS-MSA-BP-MAX-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02370         MOVE GCBH-IP-BS-MSA-BP-MAX-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02371         MOVE GCBH-IP-BS-MSA-BP-MAX-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02372         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02373         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02374                                                                   GBIDPGM 
02375  2000-4550.                                                       GBIDPGM 
02376      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02377         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02378         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02379                                                                   GBIDPGM 
02380      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02381      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02382                                                                   GBIDPGM 
02383      IF GCBH-IP-BCBS-MSA-BP-MAX-LN NOT = SPACES                   GBIDPGM 
02384         PERFORM 4022-HDG-COMB-MAX-IP                              GBIDPGM 
02385         MOVE WS-RULE-4550-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02386         MOVE GCBH-IP-BCBS-MSA-BP-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02387         MOVE GCBH-IP-BCBS-MSA-BP-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02388         MOVE GCBH-IP-BCBS-MSA-BP-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02389         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02390         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02391                                                                   GBIDPGM 
02392  2000-4920.                                                       GBIDPGM 
02393 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02394      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02395                                                                   GBIDPGM 
02396      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02397         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02398         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02399                                                                   GBIDPGM 
02400      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02401      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02402                                                                   GBIDPGM 
02403      IF GCBH-OP-BC-MH-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02404         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02405         MOVE WS-RULE-4920-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02406         MOVE GCBH-OP-BC-MH-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02407         MOVE GCBH-OP-BC-MH-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02408         MOVE GCBH-OP-BC-MH-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02409         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02410         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02777 *                                                                 GBIDPGM 
02778 *2000-4930.                                                       GBIDPGM 
02779 *    IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02780 *       PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02781 *       MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02782 *                                                                 GBIDPGM 
02783 *    MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02784 *    MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02785 *                                                                 GBIDPGM 
02786 *    IF GCBH-OP-BS-MH-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02787 *       PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02788 *       MOVE WS-RULE-4930-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02789 *       MOVE GCBH-OP-BS-MH-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02790 *       MOVE GCBH-OP-BS-MH-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02791 *       MOVE GCBH-OP-BS-MH-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02792 *       MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02793 *       ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02428                                                                   GBIDPGM 
02429  2000-4940.                                                       GBIDPGM 
02430      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02431         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02432         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02433                                                                   GBIDPGM 
02434      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02435      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02436                                                                   GBIDPGM 
02437      IF GCBH-OP-BCBS-MH-LIFE-MAX-LN NOT = SPACES                  GBIDPGM 
02438         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02439         MOVE WS-RULE-4940-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02440         MOVE GCBH-OP-BCBS-MH-LIFE-MAX-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
02441         MOVE GCBH-OP-BCBS-MH-LIFE-MAX-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
02442         MOVE GCBH-OP-BCBS-MH-LIFE-MAX-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
02443         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02444         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02445                                                                   GBIDPGM 
02446  2000-4890.                                                       GBIDPGM 
02447      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02448         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02449         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02450                                                                   GBIDPGM 
02451      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02452      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02453                                                                   GBIDPGM 
02454      IF GCBH-OP-BC-MH-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02455         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02456         MOVE WS-RULE-4890-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02457         MOVE GCBH-OP-BC-MH-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02458         MOVE GCBH-OP-BC-MH-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02459         MOVE GCBH-OP-BC-MH-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02460         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02461         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02462                                                                   GBIDPGM 
02463  2000-4900.                                                       GBIDPGM 
02464      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02465         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02466         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02467                                                                   GBIDPGM 
02468      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02469      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02470                                                                   GBIDPGM 
02471      IF GCBH-OP-BS-MH-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02472         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02473         MOVE WS-RULE-4900-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02474         MOVE GCBH-OP-BS-MH-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02475         MOVE GCBH-OP-BS-MH-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02476         MOVE GCBH-OP-BS-MH-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02477         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02478         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02479                                                                   GBIDPGM 
02480  2000-4910.                                                       GBIDPGM 
02481      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02482         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02483         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02484                                                                   GBIDPGM 
02485      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02486      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02487                                                                   GBIDPGM 
02488      IF GCBH-OP-BCBS-MH-BP-MAX-LN NOT = SPACES                    GBIDPGM 
02489         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02490         MOVE WS-RULE-4910-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02491         MOVE GCBH-OP-BCBS-MH-BP-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02492         MOVE GCBH-OP-BCBS-MH-BP-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02493         MOVE GCBH-OP-BCBS-MH-BP-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02494         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02495         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02496                                                                   GBIDPGM 
02497 *MQ 11/03                                                         GBIDPGM 
02498  2000-4911.                                                       GBIDPGM 
02499      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02500         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02501         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02502                                                                   GBIDPGM 
02503      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02504      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02505                                                                   GBIDPGM 
02506      IF GCBH-BCBS-OP-MH-BP-MAX-CH-LN  NOT = SPACES                GBIDPGM 
02507         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02508         MOVE WS-RULE-4911-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02509         MOVE GCBH-BCBS-OP-MH-BP-MAX-CH-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02510         MOVE GCBH-BCBS-OP-MH-BP-MAX-CH-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02511         MOVE GCBH-BCBS-OP-MH-BP-MAX-CH-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02512         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02513         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02514                                                                   GBIDPGM 
02515 *MQ 11/03                                                         GBIDPGM 
02516  2000-4915.                                                       GBIDPGM 
02517      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02518         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02519         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02520                                                                   GBIDPGM 
02521      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02522      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02523                                                                   GBIDPGM 
02524      IF GCBH-BCBS-OP-MH-BP-MAX-AD-LN  NOT = SPACES                GBIDPGM 
02525         PERFORM 4023-HDG-MH-MAX-OP                                GBIDPGM 
02526         MOVE WS-RULE-4915-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02527         MOVE GCBH-BCBS-OP-MH-BP-MAX-AD-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02528         MOVE GCBH-BCBS-OP-MH-BP-MAX-AD-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02529         MOVE GCBH-BCBS-OP-MH-BP-MAX-AD-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02530         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02531         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02532                                                                   GBIDPGM 
02533  2000-5010.                                                       GBIDPGM 
02534 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02535      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02536                                                                   GBIDPGM 
02537      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02538         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02539         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02540                                                                   GBIDPGM 
02541      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02542      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02543                                                                   GBIDPGM 
02544      IF GCBH-OP-BC-SA-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02545         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02546         MOVE WS-RULE-5010-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02547         MOVE GCBH-OP-BC-SA-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02548         MOVE GCBH-OP-BC-SA-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02549         MOVE GCBH-OP-BC-SA-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02550         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02551         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02552                                                                   GBIDPGM 
02553  2000-5020.                                                       GBIDPGM 
02554      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02555         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02556         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02557                                                                   GBIDPGM 
02558      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02559      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02560                                                                   GBIDPGM 
02561      IF GCBH-OP-BS-SA-LIFE-MAX-LN NOT = SPACES                    GBIDPGM 
02562         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02563         MOVE WS-RULE-5020-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02564         MOVE GCBH-OP-BS-SA-LIFE-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02565         MOVE GCBH-OP-BS-SA-LIFE-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02566         MOVE GCBH-OP-BS-SA-LIFE-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02567         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02568         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02569                                                                   GBIDPGM 
02570  2000-5030.                                                       GBIDPGM 
02571      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02572         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02573         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02574                                                                   GBIDPGM 
02575      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02576      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02577                                                                   GBIDPGM 
02578      IF GCBH-OP-BCBS-SA-LIFE-MAX-LN NOT = SPACES                  GBIDPGM 
02579         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02580         MOVE WS-RULE-5030-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02581         MOVE GCBH-OP-BCBS-SA-LIFE-MAX-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
02582         MOVE GCBH-OP-BCBS-SA-LIFE-MAX-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
02583         MOVE GCBH-OP-BCBS-SA-LIFE-MAX-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
02584         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02585         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02586                                                                   GBIDPGM 
02587  2000-4980.                                                       GBIDPGM 
02588      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02589         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02590         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02591                                                                   GBIDPGM 
02592      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02593      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02594                                                                   GBIDPGM 
02595      IF GCBH-OP-BC-SA-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02596         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02597         MOVE WS-RULE-4980-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02598         MOVE GCBH-OP-BC-SA-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02599         MOVE GCBH-OP-BC-SA-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02600         MOVE GCBH-OP-BC-SA-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02601         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02602         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02603                                                                   GBIDPGM 
02604  2000-4990.                                                       GBIDPGM 
02605      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02606         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02607         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02608                                                                   GBIDPGM 
02609      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02610      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02611                                                                   GBIDPGM 
02612      IF GCBH-OP-BS-SA-BP-MAX-LN NOT = SPACES                      GBIDPGM 
02613         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02614         MOVE WS-RULE-4990-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02615         MOVE GCBH-OP-BS-SA-BP-MAX-IN         TO WS-WLD-IN-VALUE   GBIDPGM 
02616         MOVE GCBH-OP-BS-SA-BP-MAX-OUT        TO WS-WLD-OUT-VALUE  GBIDPGM 
02617         MOVE GCBH-OP-BS-SA-BP-MAX-OTH        TO WS-WLD-OTHER-VALUEGBIDPGM 
02618         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02619         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02620                                                                   GBIDPGM 
02621  2000-5000.                                                       GBIDPGM 
02622      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02623         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02624         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02625                                                                   GBIDPGM 
02626      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02627      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02628                                                                   GBIDPGM 
02629      IF GCBH-OP-BCBS-SA-BP-MAX-LN NOT = SPACES                    GBIDPGM 
02630         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02631         MOVE WS-RULE-5000-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02632         MOVE GCBH-OP-BCBS-SA-BP-MAX-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02633         MOVE GCBH-OP-BCBS-SA-BP-MAX-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02634         MOVE GCBH-OP-BCBS-SA-BP-MAX-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02635         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02636         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02637                                                                   GBIDPGM 
02638 *MQ 11/03                                                         GBIDPGM 
02639  2000-5002.                                                       GBIDPGM 
02640      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02641         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02642         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02643                                                                   GBIDPGM 
02644      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02645      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02646                                                                   GBIDPGM 
02647      IF GCBH-BCBS-OP-SA-BP-MAX-CH-LN NOT = SPACES                 GBIDPGM 
02648         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02649         MOVE WS-RULE-5002-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02650         MOVE GCBH-BCBS-OP-SA-BP-MAX-CH-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02651         MOVE GCBH-BCBS-OP-SA-BP-MAX-CH-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02652         MOVE GCBH-BCBS-OP-SA-BP-MAX-CH-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02653         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02654         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02655                                                                   GBIDPGM 
02656 *MQ 11/03                                                         GBIDPGM 
02657  2000-5005.                                                       GBIDPGM 
02658      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02659         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02660         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02661                                                                   GBIDPGM 
02662      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02663      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02664                                                                   GBIDPGM 
02665      IF GCBH-BCBS-OP-SA-BP-MAX-AD-LN NOT = SPACES                 GBIDPGM 
02666         PERFORM 4024-HDG-SA-MAX-OP                                GBIDPGM 
02667         MOVE WS-RULE-5005-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02668         MOVE GCBH-BCBS-OP-SA-BP-MAX-AD-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02669         MOVE GCBH-BCBS-OP-SA-BP-MAX-AD-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02670         MOVE GCBH-BCBS-OP-SA-BP-MAX-AD-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02671         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02672         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02673                                                                   GBIDPGM 
02674  2000-4830.                                                       GBIDPGM 
02675 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02676      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02677                                                                   GBIDPGM 
02678      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02679         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02680         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02681                                                                   GBIDPGM 
02682      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02683      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02684                                                                   GBIDPGM 
02685      IF GCBH-OP-BC-MSA-LIFE-MAX-LN NOT = SPACES                   GBIDPGM 
02686         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02687         MOVE WS-RULE-4830-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02688         MOVE GCBH-OP-BC-MSA-LIFE-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02689         MOVE GCBH-OP-BC-MSA-LIFE-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02690         MOVE GCBH-OP-BC-MSA-LIFE-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02691         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02692         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02693                                                                   GBIDPGM 
02694  2000-4840.                                                       GBIDPGM 
02695      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02696         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02697         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02698                                                                   GBIDPGM 
02699      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02700      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02701                                                                   GBIDPGM 
02702      IF GCBH-OP-BS-MSA-LIFE-MAX-LN NOT = SPACES                   GBIDPGM 
02703         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02704         MOVE WS-RULE-4840-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02705         MOVE GCBH-OP-BS-MSA-LIFE-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02706         MOVE GCBH-OP-BS-MSA-LIFE-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02707         MOVE GCBH-OP-BS-MSA-LIFE-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02708         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02709         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02710                                                                   GBIDPGM 
02711  2000-4850.                                                       GBIDPGM 
02712      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02713         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02714         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02715                                                                   GBIDPGM 
02716      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02717      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02718                                                                   GBIDPGM 
02719      IF GCBH-OP-BCBS-MSA-LIFE-MAX-LN NOT = SPACES                 GBIDPGM 
02720         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02721         MOVE WS-RULE-4850-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02722         MOVE GCBH-OP-BCBS-MSA-LIFE-MAX-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
02723         MOVE GCBH-OP-BCBS-MSA-LIFE-MAX-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
02724         MOVE GCBH-OP-BCBS-MSA-LIFE-MAX-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
02725         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02726         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02727                                                                   GBIDPGM 
02728  2000-4800.                                                       GBIDPGM 
02729      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02730         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02731         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02732                                                                   GBIDPGM 
02733      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02734      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02735                                                                   GBIDPGM 
02736      IF GCBH-OP-BC-MSA-BP-MAX-LN NOT = SPACES                     GBIDPGM 
02737         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02738         MOVE WS-RULE-4800-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02739         MOVE GCBH-OP-BC-MSA-BP-MAX-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02740         MOVE GCBH-OP-BC-MSA-BP-MAX-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02741         MOVE GCBH-OP-BC-MSA-BP-MAX-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02742         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02743         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02744                                                                   GBIDPGM 
02745  2000-4810.                                                       GBIDPGM 
02746      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02747         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02748         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02749                                                                   GBIDPGM 
02750      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02751      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02752                                                                   GBIDPGM 
02753      IF GCBH-OP-BS-MSA-BP-MAX-LN NOT = SPACES                     GBIDPGM 
02754         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02755         MOVE WS-RULE-4810-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02756         MOVE GCBH-OP-BS-MSA-BP-MAX-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02757         MOVE GCBH-OP-BS-MSA-BP-MAX-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02758         MOVE GCBH-OP-BS-MSA-BP-MAX-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02759         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02760         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02761                                                                   GBIDPGM 
02762  2000-4820.                                                       GBIDPGM 
02763      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02764         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02765         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02766                                                                   GBIDPGM 
02767      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02768      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02769                                                                   GBIDPGM 
02770      IF GCBH-OP-BCBS-MSA-BP-MAX-LN NOT = SPACES                   GBIDPGM 
02771         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02772         MOVE WS-RULE-4820-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02773         MOVE GCBH-OP-BCBS-MSA-BP-MAX-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02774         MOVE GCBH-OP-BCBS-MSA-BP-MAX-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02775         MOVE GCBH-OP-BCBS-MSA-BP-MAX-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02776         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02777         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02778                                                                   GBIDPGM 
02779 *MQ 11/03                                                         GBIDPGM 
02780  2000-4822.                                                       GBIDPGM 
02781      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02782         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02783         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02784                                                                   GBIDPGM 
02785      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02786      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02787                                                                   GBIDPGM 
02788      IF GCBH-BCBS-OP-MSA-BP-MAX-CH-LN  NOT = SPACES               GBIDPGM 
02789         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02790         MOVE WS-RULE-4822-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02791         MOVE GCBH-BCBS-OP-MSA-BP-MAX-CH-IN   TO WS-WLD-IN-VALUE   GBIDPGM 
02792         MOVE GCBH-BCBS-OP-MSA-BP-MAX-CH-OUT  TO WS-WLD-OUT-VALUE  GBIDPGM 
02793         MOVE GCBH-BCBS-OP-MSA-BP-MAX-CH-OTH  TO WS-WLD-OTHER-VALUEGBIDPGM 
02794         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02795         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02796                                                                   GBIDPGM 
02797 *MQ 11/03                                                         GBIDPGM 
02798  2000-4825.                                                       GBIDPGM 
02799      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02800         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02801         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02802                                                                   GBIDPGM 
02803      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02804      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02805                                                                   GBIDPGM 
02806      IF GCBH-BCBS-OP-MSA-BP-MAX-AD-LN  NOT = SPACES               GBIDPGM 
02807         PERFORM 4025-HDG-COMB-MAX-OP                              GBIDPGM 
02808         MOVE WS-RULE-4825-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02809         MOVE GCBH-BCBS-OP-MSA-BP-MAX-AD-IN   TO WS-WLD-IN-VALUE   GBIDPGM 
02810         MOVE GCBH-BCBS-OP-MSA-BP-MAX-AD-OUT  TO WS-WLD-OUT-VALUE  GBIDPGM 
02811         MOVE GCBH-BCBS-OP-MSA-BP-MAX-AD-OTH  TO WS-WLD-OTHER-VALUEGBIDPGM 
02812         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02813         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02814                                                                   GBIDPGM 
02815  2000-4590.                                                       GBIDPGM 
02816 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02817      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02818                                                                   GBIDPGM 
02819      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02820         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02821         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02822                                                                   GBIDPGM 
02823      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02824      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02825                                                                   GBIDPGM 
02826      IF GCBH-IP-BC-MH-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
02827         PERFORM 4026-HDG-MH-PMT-LVL-IP                            GBIDPGM 
02828         MOVE WS-RULE-4590-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02829         MOVE GCBH-IP-BC-MH-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02830         MOVE GCBH-IP-BC-MH-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02831         MOVE GCBH-IP-BC-MH-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02832         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02833         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02834                                                                   GBIDPGM 
02835  2000-4600.                                                       GBIDPGM 
02836      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02837         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02838         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02839                                                                   GBIDPGM 
02840      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02841      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02842                                                                   GBIDPGM 
02843      IF GCBH-IP-BS-MH-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
02844         PERFORM 4026-HDG-MH-PMT-LVL-IP                            GBIDPGM 
02845         MOVE WS-RULE-4600-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02846         MOVE GCBH-IP-BS-MH-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02847         MOVE GCBH-IP-BS-MH-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02848         MOVE GCBH-IP-BS-MH-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02849         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02850         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02851                                                                   GBIDPGM 
02852  2000-4610.                                                       GBIDPGM 
02853      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02854         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02855         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02856                                                                   GBIDPGM 
02857      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02858      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02859                                                                   GBIDPGM 
02860      IF GCBH-IP-BCBS-MH-PMT-LVL-LN NOT = SPACES                   GBIDPGM 
02861         PERFORM 4026-HDG-MH-PMT-LVL-IP                            GBIDPGM 
02862         MOVE WS-RULE-4610-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02863         MOVE GCBH-IP-BCBS-MH-PMT-LVL-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02864         MOVE GCBH-IP-BCBS-MH-PMT-LVL-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02865         MOVE GCBH-IP-BCBS-MH-PMT-LVL-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02866         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02867         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02868                                                                   GBIDPGM 
02869  2000-4680.                                                       GBIDPGM 
02870 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02871      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02872                                                                   GBIDPGM 
02873      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02874         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02875         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02876                                                                   GBIDPGM 
02877      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02878      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02879                                                                   GBIDPGM 
02880      IF GCBH-IP-BC-SA-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
02881         PERFORM 4027-HDG-SA-PMT-LVL-IP                            GBIDPGM 
02882         MOVE WS-RULE-4680-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02883         MOVE GCBH-IP-BC-SA-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02884         MOVE GCBH-IP-BC-SA-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02885         MOVE GCBH-IP-BC-SA-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02886         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02887         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02888                                                                   GBIDPGM 
02889  2000-4690.                                                       GBIDPGM 
02890      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02891         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02892         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02893                                                                   GBIDPGM 
02894      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02895      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02896                                                                   GBIDPGM 
02897      IF GCBH-IP-BS-SA-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
02898         PERFORM 4027-HDG-SA-PMT-LVL-IP                            GBIDPGM 
02899         MOVE WS-RULE-4690-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02900         MOVE GCBH-IP-BS-SA-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02901         MOVE GCBH-IP-BS-SA-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02902         MOVE GCBH-IP-BS-SA-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02903         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02904         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02905                                                                   GBIDPGM 
02906  2000-4700.                                                       GBIDPGM 
02907      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02908         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02909         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02910                                                                   GBIDPGM 
02911      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02912      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02913                                                                   GBIDPGM 
02914      IF GCBH-IP-BCBS-SA-PMT-LVL-LN NOT = SPACES                   GBIDPGM 
02915         PERFORM 4027-HDG-SA-PMT-LVL-IP                            GBIDPGM 
02916         MOVE WS-RULE-4700-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02917         MOVE GCBH-IP-BCBS-SA-PMT-LVL-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
02918         MOVE GCBH-IP-BCBS-SA-PMT-LVL-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
02919         MOVE GCBH-IP-BCBS-SA-PMT-LVL-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
02920         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02921         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02922                                                                   GBIDPGM 
02923  2000-4500.                                                       GBIDPGM 
02924 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02925      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02926                                                                   GBIDPGM 
02927      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02928         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02929         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02930                                                                   GBIDPGM 
02931      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02932      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02933                                                                   GBIDPGM 
02934      IF GCBH-IP-BC-MSA-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
02935         PERFORM 4028-HDG-COMB-PMT-LVL-IP                          GBIDPGM 
02936         MOVE WS-RULE-4500-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02937         MOVE GCBH-IP-BC-MSA-PMT-LVL-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02938         MOVE GCBH-IP-BC-MSA-PMT-LVL-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02939         MOVE GCBH-IP-BC-MSA-PMT-LVL-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02940         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02941         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02942                                                                   GBIDPGM 
02943  2000-4510.                                                       GBIDPGM 
02944      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02945         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02946         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02947                                                                   GBIDPGM 
02948      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02949      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02950                                                                   GBIDPGM 
02951      IF GCBH-IP-BS-MSA-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
02952         PERFORM 4028-HDG-COMB-PMT-LVL-IP                          GBIDPGM 
02953         MOVE WS-RULE-4510-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02954         MOVE GCBH-IP-BS-MSA-PMT-LVL-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
02955         MOVE GCBH-IP-BS-MSA-PMT-LVL-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
02956         MOVE GCBH-IP-BS-MSA-PMT-LVL-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
02957         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02958         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02959                                                                   GBIDPGM 
02960  2000-4520.                                                       GBIDPGM 
02961      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02962         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02963         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02964                                                                   GBIDPGM 
02965      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02966      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02967                                                                   GBIDPGM 
02968      IF GCBH-IP-BCBS-MSA-PMT-LVL-LN NOT = SPACES                  GBIDPGM 
02969         PERFORM 4028-HDG-COMB-PMT-LVL-IP                          GBIDPGM 
02970         MOVE WS-RULE-4520-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02971         MOVE GCBH-IP-BCBS-MSA-PMT-LVL-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
02972         MOVE GCBH-IP-BCBS-MSA-PMT-LVL-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
02973         MOVE GCBH-IP-BCBS-MSA-PMT-LVL-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
02974         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02975         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02976                                                                   GBIDPGM 
02977  2000-4860.                                                       GBIDPGM 
02978 **** RESET HEADING SWITCH ****************************************GBIDPGM 
02979      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
02980                                                                   GBIDPGM 
02981      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02982         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
02983         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
02984                                                                   GBIDPGM 
02985      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
02986      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
02987                                                                   GBIDPGM 
02988      IF GCBH-OP-BC-MH-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
02989         PERFORM 4029-HDG-MH-PMT-LVL-OP                            GBIDPGM 
02990         MOVE WS-RULE-4860-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
02991         MOVE GCBH-OP-BC-MH-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
02992         MOVE GCBH-OP-BC-MH-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
02993         MOVE GCBH-OP-BC-MH-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
02994         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
02995         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
02996                                                                   GBIDPGM 
02997  2000-4870.                                                       GBIDPGM 
02998      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
02999         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03000         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03001                                                                   GBIDPGM 
03002      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03003      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03004                                                                   GBIDPGM 
03005      IF GCBH-OP-BS-MH-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
03006         PERFORM 4029-HDG-MH-PMT-LVL-OP                            GBIDPGM 
03007         MOVE WS-RULE-4870-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03008         MOVE GCBH-OP-BS-MH-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03009         MOVE GCBH-OP-BS-MH-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03010         MOVE GCBH-OP-BS-MH-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03011         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03012         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03013                                                                   GBIDPGM 
03014  2000-4880.                                                       GBIDPGM 
03015      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03016         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03017         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03018                                                                   GBIDPGM 
03019      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03020      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03021                                                                   GBIDPGM 
03022      IF GCBH-OP-BCBS-MH-PMT-LVL-LN NOT = SPACES                   GBIDPGM 
03023         PERFORM 4029-HDG-MH-PMT-LVL-OP                            GBIDPGM 
03024         MOVE WS-RULE-4880-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03025         MOVE GCBH-OP-BCBS-MH-PMT-LVL-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03026         MOVE GCBH-OP-BCBS-MH-PMT-LVL-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03027         MOVE GCBH-OP-BCBS-MH-PMT-LVL-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03028         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03029         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03030                                                                   GBIDPGM 
03031  2000-4950.                                                       GBIDPGM 
03032 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03033      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03034                                                                   GBIDPGM 
03035      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03036         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03037         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03038                                                                   GBIDPGM 
03039      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03040      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03041                                                                   GBIDPGM 
03042      IF GCBH-OP-BC-SA-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
03043         PERFORM 4030-HDG-SA-PMT-LVL-OP                            GBIDPGM 
03044         MOVE WS-RULE-4950-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03045         MOVE GCBH-OP-BC-SA-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03046         MOVE GCBH-OP-BC-SA-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03047         MOVE GCBH-OP-BC-SA-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03048         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03049         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03050                                                                   GBIDPGM 
03051  2000-4960.                                                       GBIDPGM 
03052      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03053         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03054         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03055                                                                   GBIDPGM 
03056      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03057      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03058                                                                   GBIDPGM 
03059      IF GCBH-OP-BS-SA-PMT-LVL-LN NOT = SPACES                     GBIDPGM 
03060         PERFORM 4030-HDG-SA-PMT-LVL-OP                            GBIDPGM 
03061         MOVE WS-RULE-4960-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03062         MOVE GCBH-OP-BS-SA-PMT-LVL-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03063         MOVE GCBH-OP-BS-SA-PMT-LVL-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03064         MOVE GCBH-OP-BS-SA-PMT-LVL-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03065         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03066         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03067                                                                   GBIDPGM 
03068  2000-4970.                                                       GBIDPGM 
03069      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03070         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03071         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03072                                                                   GBIDPGM 
03073      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03074      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03075                                                                   GBIDPGM 
03076      IF GCBH-OP-BCBS-SA-PMT-LVL-LN NOT = SPACES                   GBIDPGM 
03077         PERFORM 4030-HDG-SA-PMT-LVL-OP                            GBIDPGM 
03078         MOVE WS-RULE-4970-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03079         MOVE GCBH-OP-BCBS-SA-PMT-LVL-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03080         MOVE GCBH-OP-BCBS-SA-PMT-LVL-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03081         MOVE GCBH-OP-BCBS-SA-PMT-LVL-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03082         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03083         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03084                                                                   GBIDPGM 
03085  2000-4770.                                                       GBIDPGM 
03086 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03087      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03088                                                                   GBIDPGM 
03089      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03090         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03091         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03092                                                                   GBIDPGM 
03093      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03094      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03095                                                                   GBIDPGM 
03096      IF GCBH-OP-BC-MSA-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
03097         PERFORM 4031-HDG-COMB-PMT-LVL-OP                          GBIDPGM 
03098         MOVE WS-RULE-4770-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03099         MOVE GCBH-OP-BC-MSA-PMT-LVL-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03100         MOVE GCBH-OP-BC-MSA-PMT-LVL-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03101         MOVE GCBH-OP-BC-MSA-PMT-LVL-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03102         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03103         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03104                                                                   GBIDPGM 
03105  2000-4780.                                                       GBIDPGM 
03106      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03107         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03108         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03109                                                                   GBIDPGM 
03110      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03111      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03112                                                                   GBIDPGM 
03113      IF GCBH-OP-BS-MSA-PMT-LVL-LN NOT = SPACES                    GBIDPGM 
03114         PERFORM 4031-HDG-COMB-PMT-LVL-OP                          GBIDPGM 
03115         MOVE WS-RULE-4780-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03116         MOVE GCBH-OP-BS-MSA-PMT-LVL-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03117         MOVE GCBH-OP-BS-MSA-PMT-LVL-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03118         MOVE GCBH-OP-BS-MSA-PMT-LVL-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03119         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03120         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03121                                                                   GBIDPGM 
03122  2000-4790.                                                       GBIDPGM 
03123      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03124         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03125         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03126                                                                   GBIDPGM 
03127      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03128      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03129                                                                   GBIDPGM 
03130      IF GCBH-OP-BCBS-MSA-PMT-LVL-LN NOT = SPACES                  GBIDPGM 
03131         PERFORM 4031-HDG-COMB-PMT-LVL-OP                          GBIDPGM 
03132         MOVE WS-RULE-4790-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03133         MOVE GCBH-OP-BCBS-MSA-PMT-LVL-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
03134         MOVE GCBH-OP-BCBS-MSA-PMT-LVL-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
03135         MOVE GCBH-OP-BCBS-MSA-PMT-LVL-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
03136         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03137         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03138                                                                   GBIDPGM 
03139 *MQ 11/03                                                         GBIDPGM 
03140  2000-4715.                                                       GBIDPGM 
03141 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03142      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03143                                                                   GBIDPGM 
03144      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03145         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03146         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03147                                                                   GBIDPGM 
03148      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03149      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03150                                                                   GBIDPGM 
03151      IF GCBH-BC-SA-BP-MAX-POT-LN  NOT = SPACES                    GBIDPGM 
03152         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03153         MOVE WS-RULE-4715-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03154         MOVE GCBH-BC-SA-BP-MAX-POT-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03155         MOVE GCBH-BC-SA-BP-MAX-POT-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03156         MOVE GCBH-BC-SA-BP-MAX-POT-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03157         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03158         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03159                                                                   GBIDPGM 
03160 *MQ 11/03                                                         GBIDPGM 
03161  2000-4725.                                                       GBIDPGM 
03162                                                                   GBIDPGM 
03163      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03164         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03165         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03166                                                                   GBIDPGM 
03167      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03168      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03169                                                                   GBIDPGM 
03170      IF GCBH-BS-SA-BP-MAX-POT-LN  NOT = SPACES                    GBIDPGM 
03171         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03172         MOVE WS-RULE-4725-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03173         MOVE GCBH-BS-SA-BP-MAX-POT-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03174         MOVE GCBH-BS-SA-BP-MAX-POT-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03175         MOVE GCBH-BS-SA-BP-MAX-POT-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03176         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03177         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03178                                                                   GBIDPGM 
03179 *MQ 11/03                                                         GBIDPGM 
03180  2000-4735.                                                       GBIDPGM 
03181                                                                   GBIDPGM 
03182      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03183         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03184         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03185                                                                   GBIDPGM 
03186      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03187      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03188                                                                   GBIDPGM 
03189      IF GCBH-BCBS-SA-BP-MAX-POT-LN  NOT = SPACES                  GBIDPGM 
03190         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03191         MOVE WS-RULE-4735-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03192         MOVE GCBH-BCBS-SA-BP-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03193         MOVE GCBH-BCBS-SA-BP-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03194         MOVE GCBH-BCBS-SA-BP-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03195         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03196         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03197                                                                   GBIDPGM 
03198                                                                   GBIDPGM 
03199 *MQ 11/03                                                         GBIDPGM 
03200  2000-4745.                                                       GBIDPGM 
03201                                                                   GBIDPGM 
03202      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03203         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03204         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03205                                                                   GBIDPGM 
03206      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03207      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03208                                                                   GBIDPGM 
03209      IF GCBH-BC-SA-LIFE-MAX-POT-LN  NOT = SPACES                  GBIDPGM 
03210         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03211         MOVE WS-RULE-4745-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03212         MOVE GCBH-BC-SA-LIFE-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03213         MOVE GCBH-BC-SA-LIFE-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03214         MOVE GCBH-BC-SA-LIFE-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03215         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03216         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03217                                                                   GBIDPGM 
03218                                                                   GBIDPGM 
03219 *MQ 11/03                                                         GBIDPGM 
03220  2000-4755.                                                       GBIDPGM 
03221                                                                   GBIDPGM 
03222      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03223         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03224         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03225                                                                   GBIDPGM 
03226      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03227      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03228                                                                   GBIDPGM 
03229      IF GCBH-BS-SA-LIFE-MAX-POT-LN  NOT = SPACES                  GBIDPGM 
03230         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03231         MOVE WS-RULE-4755-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03232         MOVE GCBH-BS-SA-LIFE-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03233         MOVE GCBH-BS-SA-LIFE-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03234         MOVE GCBH-BS-SA-LIFE-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03235         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03236         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03237                                                                   GBIDPGM 
03238                                                                   GBIDPGM 
03239                                                                   GBIDPGM 
03240 *MQ 11/03                                                         GBIDPGM 
03241  2000-4765.                                                       GBIDPGM 
03242                                                                   GBIDPGM 
03243      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03244         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03245         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03246                                                                   GBIDPGM 
03247      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03248      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03249                                                                   GBIDPGM 
03250      IF GCBH-BCBS-SA-LIFE-MAX-POT-LN  NOT = SPACES                GBIDPGM 
03251         PERFORM 4032-HDG-SA-MA-ALL-POT                            GBIDPGM 
03252         MOVE WS-RULE-4765-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03253         MOVE GCBH-BCBS-SA-LIFE-MAX-POT-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
03254         MOVE GCBH-BCBS-SA-LIFE-MAX-POT-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
03255         MOVE GCBH-BCBS-SA-LIFE-MAX-POT-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
03256         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03257         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03258                                                                   GBIDPGM 
03259 *MQ 11/03                                                         GBIDPGM 
03260  2000-4775.                                                       GBIDPGM 
03261 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03262      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03263                                                                   GBIDPGM 
03264      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03265         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03266         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03267                                                                   GBIDPGM 
03268      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03269      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03270                                                                   GBIDPGM 
03271      IF GCBH-BC-MSA-PMT-LVL-POT-LN  NOT = SPACES                  GBIDPGM 
03272         PERFORM 4033-HDG-CMB-PMT-LVL-POT                          GBIDPGM 
03273         MOVE WS-RULE-4775-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03274         MOVE GCBH-BC-MSA-PMT-LVL-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03275         MOVE GCBH-BC-MSA-PMT-LVL-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03276         MOVE GCBH-BC-MSA-PMT-LVL-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03277         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03278         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03279                                                                   GBIDPGM 
03280 *MQ 11/03                                                         GBIDPGM 
03281  2000-4785.                                                       GBIDPGM 
03282                                                                   GBIDPGM 
03283      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03284         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03285         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03286                                                                   GBIDPGM 
03287      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03288      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03289                                                                   GBIDPGM 
03290      IF GCBH-BS-MSA-PMT-LVL-POT-LN  NOT = SPACES                  GBIDPGM 
03291         PERFORM 4033-HDG-CMB-PMT-LVL-POT                          GBIDPGM 
03292         MOVE WS-RULE-4785-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03293         MOVE GCBH-BS-MSA-PMT-LVL-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03294         MOVE GCBH-BS-MSA-PMT-LVL-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03295         MOVE GCBH-BS-MSA-PMT-LVL-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03296         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03297         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03298                                                                   GBIDPGM 
03299                                                                   GBIDPGM 
03300 *MQ 11/03                                                         GBIDPGM 
03301  2000-4795.                                                       GBIDPGM 
03302                                                                   GBIDPGM 
03303      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03304         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03305         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03306                                                                   GBIDPGM 
03307      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03308      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03309                                                                   GBIDPGM 
03310      IF GCBH-BCBS-MSA-PMT-LVL-POT-LN  NOT = SPACES                GBIDPGM 
03311         PERFORM 4033-HDG-CMB-PMT-LVL-POT                          GBIDPGM 
03312         MOVE WS-RULE-4795-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03313         MOVE GCBH-BCBS-MSA-PMT-LVL-POT-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
03314         MOVE GCBH-BCBS-MSA-PMT-LVL-POT-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
03315         MOVE GCBH-BCBS-MSA-PMT-LVL-POT-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
03316         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03317         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03318                                                                   GBIDPGM 
03319                                                                   GBIDPGM 
03320 *MQ 11/03                                                         GBIDPGM 
03321  2000-4865.                                                       GBIDPGM 
03322 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03323      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03324                                                                   GBIDPGM 
03325      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03326         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03327         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03328                                                                   GBIDPGM 
03329      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03330      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03331                                                                   GBIDPGM 
03332      IF GCBH-BC-MH-PMT-LVL-POT-LN   NOT = SPACES                  GBIDPGM 
03333         PERFORM 4034-HDG-MH-PMT-LVL-POT                           GBIDPGM 
03334         MOVE WS-RULE-4865-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03335         MOVE GCBH-BC-MH-PMT-LVL-POT-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03336         MOVE GCBH-BC-MH-PMT-LVL-POT-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03337         MOVE GCBH-BC-MH-PMT-LVL-POT-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03338         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03339         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03340                                                                   GBIDPGM 
03341                                                                   GBIDPGM 
03342 *MQ 11/03                                                         GBIDPGM 
03343  2000-4875.                                                       GBIDPGM 
03344                                                                   GBIDPGM 
03345      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03346         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03347         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03348                                                                   GBIDPGM 
03349      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03350      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03351                                                                   GBIDPGM 
03352      IF GCBH-BS-MH-PMT-LVL-POT-LN   NOT = SPACES                  GBIDPGM 
03353         PERFORM 4034-HDG-MH-PMT-LVL-POT                           GBIDPGM 
03354         MOVE WS-RULE-4875-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03355         MOVE GCBH-BS-MH-PMT-LVL-POT-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03356         MOVE GCBH-BS-MH-PMT-LVL-POT-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03357         MOVE GCBH-BS-MH-PMT-LVL-POT-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03358         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03359         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03360                                                                   GBIDPGM 
03361                                                                   GBIDPGM 
03362 *MQ 11/03                                                         GBIDPGM 
03363  2000-4885.                                                       GBIDPGM 
03364                                                                   GBIDPGM 
03365      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03366         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03367         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03368                                                                   GBIDPGM 
03369      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03370      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03371                                                                   GBIDPGM 
03372      IF GCBH-BCBS-MH-PMT-LVL-POT-LN NOT = SPACES                  GBIDPGM 
03373         PERFORM 4034-HDG-MH-PMT-LVL-POT                           GBIDPGM 
03374         MOVE WS-RULE-4885-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03375         MOVE GCBH-BCBS-MH-PMT-LVL-POT-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
03376         MOVE GCBH-BCBS-MH-PMT-LVL-POT-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
03377         MOVE GCBH-BCBS-MH-PMT-LVL-POT-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
03378         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03379         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03380                                                                   GBIDPGM 
03381                                                                   GBIDPGM 
03382 *MQ 11/03                                                         GBIDPGM 
03383  2000-4955.                                                       GBIDPGM 
03384 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03385      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03386                                                                   GBIDPGM 
03387      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03388         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03389         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03390                                                                   GBIDPGM 
03391      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03392      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03393                                                                   GBIDPGM 
03394      IF GCBH-BC-SA-PMT-LVL-POT-LN   NOT = SPACES                  GBIDPGM 
03395         PERFORM 4035-HDG-SA-PMT-LVL-POT                           GBIDPGM 
03396         MOVE WS-RULE-4955-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03397         MOVE GCBH-BC-SA-PMT-LVL-POT-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03398         MOVE GCBH-BC-SA-PMT-LVL-POT-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03399         MOVE GCBH-BC-SA-PMT-LVL-POT-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03400         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03401         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03402                                                                   GBIDPGM 
03403                                                                   GBIDPGM 
03404 *MQ 11/03                                                         GBIDPGM 
03405  2000-4965.                                                       GBIDPGM 
03406                                                                   GBIDPGM 
03407      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03408         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03409         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03410                                                                   GBIDPGM 
03411      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03412      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03413                                                                   GBIDPGM 
03414      IF GCBH-BS-SA-PMT-LVL-POT-LN   NOT = SPACES                  GBIDPGM 
03415         PERFORM 4035-HDG-SA-PMT-LVL-POT                           GBIDPGM 
03416         MOVE WS-RULE-4965-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03417         MOVE GCBH-BS-SA-PMT-LVL-POT-IN       TO WS-WLD-IN-VALUE   GBIDPGM 
03418         MOVE GCBH-BS-SA-PMT-LVL-POT-OUT      TO WS-WLD-OUT-VALUE  GBIDPGM 
03419         MOVE GCBH-BS-SA-PMT-LVL-POT-OTH      TO WS-WLD-OTHER-VALUEGBIDPGM 
03420         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03421         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03422                                                                   GBIDPGM 
03423                                                                   GBIDPGM 
03424 *MQ 11/03                                                         GBIDPGM 
03425  2000-4975.                                                       GBIDPGM 
03426                                                                   GBIDPGM 
03427      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03428         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03429         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03430                                                                   GBIDPGM 
03431      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03432      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03433                                                                   GBIDPGM 
03434      IF GCBH-BCBS-SA-PMT-LVL-POT-LN NOT = SPACES                  GBIDPGM 
03435         PERFORM 4035-HDG-SA-PMT-LVL-POT                           GBIDPGM 
03436         MOVE WS-RULE-4975-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03437         MOVE GCBH-BCBS-SA-PMT-LVL-POT-IN     TO WS-WLD-IN-VALUE   GBIDPGM 
03438         MOVE GCBH-BCBS-SA-PMT-LVL-POT-OUT    TO WS-WLD-OUT-VALUE  GBIDPGM 
03439         MOVE GCBH-BCBS-SA-PMT-LVL-POT-OTH    TO WS-WLD-OTHER-VALUEGBIDPGM 
03440         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03441         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03442                                                                   GBIDPGM 
03444 *MQ 05/04                                                         GBIDPGM 
03445  2000-4762.                                                       GBIDPGM 
03446                                                                   GBIDPGM 
03447 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03448      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03449                                                                   GBIDPGM 
03450      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03451         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03452         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03453                                                                   GBIDPGM 
03454      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03455      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03456                                                                   GBIDPGM 
03457      IF GCBH-SA-LIFE-CONF-MAX-LN    NOT = SPACES                  GBIDPGM 
03458         PERFORM 4036-HDG-SA-LIFE-CONF-MX                          GBIDPGM 
03459         MOVE WS-RULE-4762-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03460         MOVE GCBH-SA-LIFE-CONF-MAX-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03461         MOVE GCBH-SA-LIFE-CONF-MAX-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03462         MOVE GCBH-SA-LIFE-CONF-MAX-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03463         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03464         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03465                                                                   GBIDPGM 
03831                                                                   GBIDPGM 
03832 *MQ 01/20/05 - CHANGE BEGIN                                       GBIDPGM 
03833  2000-4625.                                                       GBIDPGM 
03834                                                                   GBIDPGM 
03835 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03836      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03837                                                                   GBIDPGM 
03838      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03839         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03840         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03841                                                                   GBIDPGM 
03842      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03843      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03844                                                                   GBIDPGM 
03845      IF GCBH-BC-MH-BP-MAX-POT-LN  NOT = SPACES                    GBIDPGM 
03846         PERFORM 4037-HDG-MH-BP-MAX-ALL-POT                        GBIDPGM 
03847         MOVE WS-RULE-4625-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03848         MOVE GCBH-BC-MH-BP-MAX-POT-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03849         MOVE GCBH-BC-MH-BP-MAX-POT-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03850         MOVE GCBH-BC-MH-BP-MAX-POT-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03851         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03852         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03853                                                                   GBIDPGM 
03854                                                                   GBIDPGM 
03855  2000-4635.                                                       GBIDPGM 
03856                                                                   GBIDPGM 
03857      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03858         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03859         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03860                                                                   GBIDPGM 
03861      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03862      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03863                                                                   GBIDPGM 
03864      IF GCBH-BS-MH-BP-MAX-POT-LN  NOT = SPACES                    GBIDPGM 
03865         PERFORM 4037-HDG-MH-BP-MAX-ALL-POT                        GBIDPGM 
03866         MOVE WS-RULE-4635-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03867         MOVE GCBH-BS-MH-BP-MAX-POT-IN        TO WS-WLD-IN-VALUE   GBIDPGM 
03868         MOVE GCBH-BS-MH-BP-MAX-POT-OUT       TO WS-WLD-OUT-VALUE  GBIDPGM 
03869         MOVE GCBH-BS-MH-BP-MAX-POT-OTH       TO WS-WLD-OTHER-VALUEGBIDPGM 
03870         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03871         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03872                                                                   GBIDPGM 
03873                                                                   GBIDPGM 
03874  2000-4645.                                                       GBIDPGM 
03875                                                                   GBIDPGM 
03876      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03877         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03878         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03879                                                                   GBIDPGM 
03880      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03881      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03882                                                                   GBIDPGM 
03883      IF GCBH-BCBS-MH-BP-MAX-POT-LN NOT = SPACES                   GBIDPGM 
03884         PERFORM 4037-HDG-MH-BP-MAX-ALL-POT                        GBIDPGM 
03885         MOVE WS-RULE-4645-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03886         MOVE GCBH-BCBS-MH-BP-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03887         MOVE GCBH-BCBS-MH-BP-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03888         MOVE GCBH-BCBS-MH-BP-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03889         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03890         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03891                                                                   GBIDPGM 
03892                                                                   GBIDPGM 
03893  2000-4655.                                                       GBIDPGM 
03894                                                                   GBIDPGM 
03895 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03896      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03897                                                                   GBIDPGM 
03898      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03899         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03900         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03901                                                                   GBIDPGM 
03902      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03903      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03904                                                                   GBIDPGM 
03905      IF GCBH-BC-MH-LIFE-MAX-POT-LN NOT = SPACES                   GBIDPGM 
03906         PERFORM 4038-HDG-MH-LIFE-MAX-ALL-POT                      GBIDPGM 
03907         MOVE WS-RULE-4655-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03908         MOVE GCBH-BC-MH-LIFE-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03909         MOVE GCBH-BC-MH-LIFE-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03910         MOVE GCBH-BC-MH-LIFE-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03911         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03912         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03913                                                                   GBIDPGM 
03914  2000-4665.                                                       GBIDPGM 
03915                                                                   GBIDPGM 
03916      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03917         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03918         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03919                                                                   GBIDPGM 
03920      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03921      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03922                                                                   GBIDPGM 
03923      IF GCBH-BS-MH-LIFE-MAX-POT-LN   NOT = SPACES                 GBIDPGM 
03924         PERFORM 4038-HDG-MH-LIFE-MAX-ALL-POT                      GBIDPGM 
03925         MOVE WS-RULE-4665-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03926         MOVE GCBH-BS-MH-LIFE-MAX-POT-IN      TO WS-WLD-IN-VALUE   GBIDPGM 
03927         MOVE GCBH-BS-MH-LIFE-MAX-POT-OUT     TO WS-WLD-OUT-VALUE  GBIDPGM 
03928         MOVE GCBH-BS-MH-LIFE-MAX-POT-OTH     TO WS-WLD-OTHER-VALUEGBIDPGM 
03929         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03930         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03931                                                                   GBIDPGM 
03932                                                                   GBIDPGM 
03933  2000-4675.                                                       GBIDPGM 
03934                                                                   GBIDPGM 
03935      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03936         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03937         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03938                                                                   GBIDPGM 
03939      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03940      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03941                                                                   GBIDPGM 
03942      IF GCBH-BCBS-MH-LIFE-MAX-POT-LN NOT = SPACES                 GBIDPGM 
03943         PERFORM 4038-HDG-MH-LIFE-MAX-ALL-POT                      GBIDPGM 
03944         MOVE WS-RULE-4675-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03945         MOVE GCBH-BCBS-MH-LIFE-MAX-POT-IN    TO WS-WLD-IN-VALUE   GBIDPGM 
03946         MOVE GCBH-BCBS-MH-LIFE-MAX-POT-OUT   TO WS-WLD-OUT-VALUE  GBIDPGM 
03947         MOVE GCBH-BCBS-MH-LIFE-MAX-POT-OTH   TO WS-WLD-OTHER-VALUEGBIDPGM 
03948         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03949         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03950                                                                   GBIDPGM 
03951                                                                   GBIDPGM 
03952  2000-5060.                                                       GBIDPGM 
03953                                                                   GBIDPGM 
03954 **** RESET HEADING SWITCH ****************************************GBIDPGM 
03955      MOVE 'N'   TO WS-HEADING-SW.                                 GBIDPGM 
03956                                                                   GBIDPGM 
03957      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03958         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03959         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03960                                                                   GBIDPGM 
03961      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03962      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03963                                                                   GBIDPGM 
03964      IF GCBH-SMI-BP-MAX-IP-LN NOT = SPACES                        GBIDPGM 
03965         PERFORM 4039-HDG-SMI-BP-MAX                               GBIDPGM 
03966         MOVE WS-RULE-5060-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03967         MOVE GCBH-SMI-BP-MAX-IP-IN           TO WS-WLD-IN-VALUE   GBIDPGM 
03968         MOVE GCBH-SMI-BP-MAX-IP-OUT          TO WS-WLD-OUT-VALUE  GBIDPGM 
03969         MOVE GCBH-SMI-BP-MAX-IP-OTH          TO WS-WLD-OTHER-VALUEGBIDPGM 
03970         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03971         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03972                                                                   GBIDPGM 
03973                                                                   GBIDPGM 
03974  2000-5070.                                                       GBIDPGM 
03975                                                                   GBIDPGM 
03976      IF WS-TS-LINE-SUB > 19                                       GBIDPGM 
03977         PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT      GBIDPGM 
03978         MOVE +001 TO WS-TS-LINE-SUB.                              GBIDPGM 
03979                                                                   GBIDPGM 
03980      MOVE 'N'             TO WS-RULE-WRITTEN-SW.                  GBIDPGM 
03981      MOVE SPACES          TO WS-WRITE-LINE.                       GBIDPGM 
03982                                                                   GBIDPGM 
03983      IF GCBH-SMI-BP-MAX-OP-LN NOT = SPACES                        GBIDPGM 
03984         PERFORM 4039-HDG-SMI-BP-MAX                               GBIDPGM 
03985         MOVE WS-RULE-5070-VALUE              TO WS-WLD-DESCRIPTIONGBIDPGM 
03986         MOVE GCBH-SMI-BP-MAX-OP-IN           TO WS-WLD-IN-VALUE   GBIDPGM 
03987         MOVE GCBH-SMI-BP-MAX-OP-OUT          TO WS-WLD-OUT-VALUE  GBIDPGM 
03988         MOVE GCBH-SMI-BP-MAX-OP-OTH          TO WS-WLD-OTHER-VALUEGBIDPGM 
03989         MOVE WS-WRITE-LINE       TO  WS-TS-VALUE (WS-TS-LINE-SUB) GBIDPGM 
03990         ADD  +1                  TO  WS-TS-LINE-SUB.              GBIDPGM 
03991                                                                   GBIDPGM 
03992 *MQ 01/20/05 - CHANGE END                                         GBIDPGM 
03466                                                                   GBIDPGM 
03467 ******************************************************************GBIDPGM 
03468 **** WRITE OUT LAST LINES ****************************************GBIDPGM 
03469 ******************************************************************GBIDPGM 
03470      PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT         GBIDPGM 
03471      MOVE +001 TO WS-TS-LINE-SUB.                                 GBIDPGM 
03472                                                                   GBIDPGM 
03473  2000-EXIT.                                                       GBIDPGM 
03474      EXIT.                                                        GBIDPGM 
03475 /                                                                 GBIDPGM 
03476 ******************************************************************GBIDPGM 
03477 *                                                                 GBIDPGM 
03478 *  IF THE \
03479 *  DETAIL LINES WILL BE WRITTEN FOR THIS SECTION.                 GBIDPGM 
03480 *                                                                 GBIDPGM 
03481 ******************************************************************GBIDPGM 
03482  4001-PLAN-SUM-HEADING-CHECK.                                     GBIDPGM 
03483                                                                   GBIDPGM 
03484      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03485         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03486            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03487            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03488         END-IF                                                    GBIDPGM 
03489         MOVE SPACES            TO WS-WRITE-LINE                   GBIDPGM 
03490         SET  WS-SECTION-HDG-PLAN-SUM TO TRUE                      GBIDPGM 
03491         MOVE WS-SECTION-LINE   TO WS-TS-VALUE (WS-TS-LINE-SUB)    GBIDPGM 
03492         MOVE SPACES            TO WS-WRITE-LINE                   GBIDPGM 
03493         ADD  +1                TO WS-TS-LINE-SUB                  GBIDPGM 
03494         MOVE 'Y'               TO WS-HEADING-SW.                  GBIDPGM 
03495                                                                   GBIDPGM 
03496  4001-EXIT.                                                       GBIDPGM 
03497      EXIT.                                                        GBIDPGM 
03498 /                                                                 GBIDPGM 
03499 ******************************************************************GBIDPGM 
03500 *                                                                 GBIDPGM 
03501 *  IF THE \
03502 *  DETAIL LINES WILL BE WRITTEN FOR THIS SECTION.                 GBIDPGM 
03503 *                                                                 GBIDPGM 
03504 ******************************************************************GBIDPGM 
03505  4002-GEN-BEN-HEADING-CHECK.                                      GBIDPGM 
03506                                                                   GBIDPGM 
03507      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03508         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03509            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03510            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03511         END-IF                                                    GBIDPGM 
03512         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03513         SET  WS-SECTION-HDG-GEN-BEN TO TRUE                       GBIDPGM 
03514         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03515         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03516         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03517         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03518                                                                   GBIDPGM 
03519  4002-EXIT.                                                       GBIDPGM 
03520      EXIT.                                                        GBIDPGM 
03521 /                                                                 GBIDPGM 
03522 ******************************************************************GBIDPGM 
03523 *                                                                 GBIDPGM 
03524 *  IF THE \
03525 *  WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN                GBIDPGM 
03526 *  FOR THIS SECTION.                                              GBIDPGM 
03527 *                                                                 GBIDPGM 
03528 ******************************************************************GBIDPGM 
03529  4004-ADMIN-BENEFITS-CHECK.                                       GBIDPGM 
03530                                                                   GBIDPGM 
03531      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03532         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03533            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03534            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03535         END-IF                                                    GBIDPGM 
03536         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03537         SET  WS-SECTION-HDG-ADM-BEN TO TRUE                       GBIDPGM 
03538         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03539         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03540         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03541         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03542                                                                   GBIDPGM 
03543  4004-EXIT.                                                       GBIDPGM 
03544      EXIT.                                                        GBIDPGM 
03545 /                                                                 GBIDPGM 
03546 ******************************************************************GBIDPGM 
03547 *                                                                 GBIDPGM 
03548 *  IF THE \
03549 *  WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN                GBIDPGM 
03550 *  FOR THIS SECTION.                                              GBIDPGM 
03551 *                                                                 GBIDPGM 
03552 ******************************************************************GBIDPGM 
03553  4005-COST-CONTAINMENT.                                           GBIDPGM 
03554                                                                   GBIDPGM 
03555      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03556         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03557            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03558            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03559         END-IF                                                    GBIDPGM 
03560         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03561         SET  WS-SECTION-HDG-COST-CONT TO TRUE                     GBIDPGM 
03562         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03563         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03564         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03565         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03566                                                                   GBIDPGM 
03567  4005-EXIT.                                                       GBIDPGM 
03568      EXIT.                                                        GBIDPGM 
03569 /                                                                 GBIDPGM 
03570 ******************************************************************GBIDPGM 
03571 *                                                                 GBIDPGM 
03572 *  IF THE \
03573 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03574 *  FOR THIS SECTION.                                              GBIDPGM 
03575 *                                                                 GBIDPGM 
03576 ******************************************************************GBIDPGM 
03577  4020-HDG-MH-MAX-IP.                                              GBIDPGM 
03578                                                                   GBIDPGM 
03579      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03580         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03581            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03582            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03583         END-IF                                                    GBIDPGM 
03584         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03585         SET  WS-SECTION-HDG-MH-MAX-IP TO TRUE                     GBIDPGM 
03586         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03587         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03588         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03589         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03590                                                                   GBIDPGM 
03591  4020-EXIT.                                                       GBIDPGM 
03592      EXIT.                                                        GBIDPGM 
03593 /                                                                 GBIDPGM 
03594 ******************************************************************GBIDPGM 
03595 *                                                                 GBIDPGM 
03596 *  IF THE \
03597 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03598 *  FOR THIS SECTION.                                              GBIDPGM 
03599 *                                                                 GBIDPGM 
03600 ******************************************************************GBIDPGM 
03601  4021-HDG-SA-MAX-IP.                                              GBIDPGM 
03602                                                                   GBIDPGM 
03603      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03604         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03605            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03606            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03607         END-IF                                                    GBIDPGM 
03608         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03609         SET  WS-SECTION-HDG-SA-MAX-IP TO TRUE                     GBIDPGM 
03610         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03611         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03612         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03613         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03614                                                                   GBIDPGM 
03615  4021-EXIT.                                                       GBIDPGM 
03616      EXIT.                                                        GBIDPGM 
03617 /                                                                 GBIDPGM 
03618 ******************************************************************GBIDPGM 
03619 *                                                                 GBIDPGM 
03620 *  IF THE \
03621 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03622 *  FOR THIS SECTION.                                              GBIDPGM 
03623 *                                                                 GBIDPGM 
03624 ******************************************************************GBIDPGM 
03625  4022-HDG-COMB-MAX-IP.                                            GBIDPGM 
03626                                                                   GBIDPGM 
03627      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03628         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03629            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03630            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03631         END-IF                                                    GBIDPGM 
03632         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03633         SET  WS-SECTION-HDG-COMB-MAX-IP TO TRUE                   GBIDPGM 
03634         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03635         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03636         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03637         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03638                                                                   GBIDPGM 
03639  4022-EXIT.                                                       GBIDPGM 
03640      EXIT.                                                        GBIDPGM 
03641 /                                                                 GBIDPGM 
03642 ******************************************************************GBIDPGM 
03643 *                                                                 GBIDPGM 
03644 *  IF THE \
03645 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03646 *  FOR THIS SECTION.                                              GBIDPGM 
03647 *                                                                 GBIDPGM 
03648 ******************************************************************GBIDPGM 
03649  4023-HDG-MH-MAX-OP.                                              GBIDPGM 
03650                                                                   GBIDPGM 
03651      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03652         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03653            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03654            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03655         END-IF                                                    GBIDPGM 
03656         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03657         SET  WS-SECTION-HDG-MH-MAX-OP TO TRUE                     GBIDPGM 
03658         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03659         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03660         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03661         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03662                                                                   GBIDPGM 
03663  4023-EXIT.                                                       GBIDPGM 
03664      EXIT.                                                        GBIDPGM 
03665 /                                                                 GBIDPGM 
03666 ******************************************************************GBIDPGM 
03667 *                                                                 GBIDPGM 
03668 *  IF THE \
03669 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03670 *  FOR THIS SECTION.                                              GBIDPGM 
03671 *                                                                 GBIDPGM 
03672 ******************************************************************GBIDPGM 
03673  4024-HDG-SA-MAX-OP.                                              GBIDPGM 
03674                                                                   GBIDPGM 
03675      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03676         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03677            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03678            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03679         END-IF                                                    GBIDPGM 
03680         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03681         SET  WS-SECTION-HDG-SA-MAX-OP TO TRUE                     GBIDPGM 
03682         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03683         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03684         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03685         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03686                                                                   GBIDPGM 
03687  4024-EXIT.                                                       GBIDPGM 
03688      EXIT.                                                        GBIDPGM 
03689 /                                                                 GBIDPGM 
03690 ******************************************************************GBIDPGM 
03691 *                                                                 GBIDPGM 
03692 *  IF THE \
03693 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03694 *  FOR THIS SECTION.                                              GBIDPGM 
03695 *                                                                 GBIDPGM 
03696 ******************************************************************GBIDPGM 
03697  4025-HDG-COMB-MAX-OP.                                            GBIDPGM 
03698                                                                   GBIDPGM 
03699      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03700         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03701            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03702            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03703         END-IF                                                    GBIDPGM 
03704         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03705         SET  WS-SECTION-HDG-COMB-MAX-OP TO TRUE                   GBIDPGM 
03706         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03707         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03708         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03709         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03710                                                                   GBIDPGM 
03711  4025-EXIT.                                                       GBIDPGM 
03712      EXIT.                                                        GBIDPGM 
03713 /                                                                 GBIDPGM 
03714 ******************************************************************GBIDPGM 
03715 *                                                                 GBIDPGM 
03716 *  IF THE \
03717 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03718 *  FOR THIS SECTION.                                              GBIDPGM 
03719 *                                                                 GBIDPGM 
03720 ******************************************************************GBIDPGM 
03721  4026-HDG-MH-PMT-LVL-IP.                                          GBIDPGM 
03722                                                                   GBIDPGM 
03723      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03724         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03725            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03726            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03727         END-IF                                                    GBIDPGM 
03728         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03729         SET  WS-SECTION-HDG-MH-PMT-LVL-IP TO TRUE                 GBIDPGM 
03730         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03731         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03732         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03733         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03734                                                                   GBIDPGM 
03735  4026-EXIT.                                                       GBIDPGM 
03736      EXIT.                                                        GBIDPGM 
03737 /                                                                 GBIDPGM 
03738 ******************************************************************GBIDPGM 
03739 *                                                                 GBIDPGM 
03740 *  IF THE \
03741 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03742 *  FOR THIS SECTION.                                              GBIDPGM 
03743 *                                                                 GBIDPGM 
03744 ******************************************************************GBIDPGM 
03745  4027-HDG-SA-PMT-LVL-IP.                                          GBIDPGM 
03746                                                                   GBIDPGM 
03747      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03748         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03749            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03750            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03751         END-IF                                                    GBIDPGM 
03752         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03753         SET  WS-SECTION-HDG-SA-PMT-LVL-IP TO TRUE                 GBIDPGM 
03754         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03755         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03756         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03757         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03758                                                                   GBIDPGM 
03759  4027-EXIT.                                                       GBIDPGM 
03760      EXIT.                                                        GBIDPGM 
03761 /                                                                 GBIDPGM 
03762 ******************************************************************GBIDPGM 
03763 *                                                                 GBIDPGM 
03764 *  IF THE \
03765 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03766 *  FOR THIS SECTION.                                              GBIDPGM 
03767 *                                                                 GBIDPGM 
03768 ******************************************************************GBIDPGM 
03769  4028-HDG-COMB-PMT-LVL-IP.                                        GBIDPGM 
03770                                                                   GBIDPGM 
03771      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03772         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03773            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03774            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03775         END-IF                                                    GBIDPGM 
03776         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03777         SET  WS-SECTION-HDG-COMB-PMT-LVL-IP TO TRUE               GBIDPGM 
03778         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03779         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03780         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03781         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03782                                                                   GBIDPGM 
03783  4028-EXIT.                                                       GBIDPGM 
03784      EXIT.                                                        GBIDPGM 
03785 /                                                                 GBIDPGM 
03786 ******************************************************************GBIDPGM 
03787 *                                                                 GBIDPGM 
03788 *  IF THE \
03789 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03790 *  FOR THIS SECTION.                                              GBIDPGM 
03791 *                                                                 GBIDPGM 
03792 ******************************************************************GBIDPGM 
03793  4029-HDG-MH-PMT-LVL-OP.                                          GBIDPGM 
03794                                                                   GBIDPGM 
03795      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03796         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03797            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03798            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03799         END-IF                                                    GBIDPGM 
03800         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03801         SET  WS-SECTION-HDG-MH-PMT-LVL-OP TO TRUE                 GBIDPGM 
03802         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03803         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03804         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03805         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03806                                                                   GBIDPGM 
03807  4029-EXIT.                                                       GBIDPGM 
03808      EXIT.                                                        GBIDPGM 
03809 /                                                                 GBIDPGM 
03810 ******************************************************************GBIDPGM 
03811 *                                                                 GBIDPGM 
03812 *  IF THE \
03813 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03814 *  FOR THIS SECTION.                                              GBIDPGM 
03815 *                                                                 GBIDPGM 
03816 ******************************************************************GBIDPGM 
03817  4030-HDG-SA-PMT-LVL-OP.                                          GBIDPGM 
03818                                                                   GBIDPGM 
03819      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03820         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03821            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03822            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03823         END-IF                                                    GBIDPGM 
03824         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03825         SET  WS-SECTION-HDG-SA-PMT-LVL-OP TO TRUE                 GBIDPGM 
03826         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03827         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03828         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03829         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03830                                                                   GBIDPGM 
03831  4030-EXIT.                                                       GBIDPGM 
03832      EXIT.                                                        GBIDPGM 
03833 /                                                                 GBIDPGM 
03834 ******************************************************************GBIDPGM 
03835 *                                                                 GBIDPGM 
03836 *  IF THE \
03837 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03838 *  FOR THIS SECTION.                                              GBIDPGM 
03839 *                                                                 GBIDPGM 
03840 ******************************************************************GBIDPGM 
03841  4031-HDG-COMB-PMT-LVL-OP.                                        GBIDPGM 
03842                                                                   GBIDPGM 
03843      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03844         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03845            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03846            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03847         END-IF                                                    GBIDPGM 
03848         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03849         SET  WS-SECTION-HDG-COMB-PMT-LVL-OP TO TRUE               GBIDPGM 
03850         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03851         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03852         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03853         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03854                                                                   GBIDPGM 
03855  4031-EXIT.                                                       GBIDPGM 
03856      EXIT.                                                        GBIDPGM 
03857 /                                                                 GBIDPGM 
03858 *MQ 11/03                                                         GBIDPGM 
03859 ******************************************************************GBIDPGM 
03860 *                                                                 GBIDPGM 
03861 *  IF THE \
03862 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03863 *  FOR THIS SECTION.                                              GBIDPGM 
03864 *                                                                 GBIDPGM 
03865 ******************************************************************GBIDPGM 
03866  4032-HDG-SA-MA-ALL-POT.                                          GBIDPGM 
03867                                                                   GBIDPGM 
03868      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03869         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03870            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03871            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03872         END-IF                                                    GBIDPGM 
03873         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03874         SET  WS-SECTION-HDG-SA-MA-ALL-POT  TO TRUE                GBIDPGM 
03875         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03876         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03877         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03878         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03879                                                                   GBIDPGM 
03880  4032-EXIT.                                                       GBIDPGM 
03881      EXIT.                                                        GBIDPGM 
03882 /                                                                 GBIDPGM 
03883 *MQ 11/03                                                         GBIDPGM 
03884 ******************************************************************GBIDPGM 
03885 *                                                                 GBIDPGM 
03886 *  IF THE \
03887 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03888 *  FOR THIS SECTION.                                              GBIDPGM 
03889 *                                                                 GBIDPGM 
03890 ******************************************************************GBIDPGM 
03891  4033-HDG-CMB-PMT-LVL-POT.                                        GBIDPGM 
03892                                                                   GBIDPGM 
03893      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03894         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03895            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03896            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03897         END-IF                                                    GBIDPGM 
03898         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03899         SET  WS-SECTION-HDG-CMB-PMT-LVL-POT   TO TRUE             GBIDPGM 
03900         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03901         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03902         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03903         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03904                                                                   GBIDPGM 
03905  4033-EXIT.                                                       GBIDPGM 
03906      EXIT.                                                        GBIDPGM 
03907 /                                                                 GBIDPGM 
03908 *MQ 11/03                                                         GBIDPGM 
03909 ******************************************************************GBIDPGM 
03910 *                                                                 GBIDPGM 
03911 *  IF THE \
03912 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03913 *  FOR THIS SECTION.                                              GBIDPGM 
03914 *                                                                 GBIDPGM 
03915 ******************************************************************GBIDPGM 
03916  4034-HDG-MH-PMT-LVL-POT.                                         GBIDPGM 
03917                                                                   GBIDPGM 
03918      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03919         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03920            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03921            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03922         END-IF                                                    GBIDPGM 
03923         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03924         SET  WS-SECTION-HDG-MH-PMT-LVL-POT  TO TRUE               GBIDPGM 
03925         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03926         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03927         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03928         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03929                                                                   GBIDPGM 
03930  4034-EXIT.                                                       GBIDPGM 
03931      EXIT.                                                        GBIDPGM 
03932 /                                                                 GBIDPGM 
03933 *MQ 11/03                                                         GBIDPGM 
03934 ******************************************************************GBIDPGM 
03935 *                                                                 GBIDPGM 
03936 *  IF THE \
03937 *  WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE WRITTEN       GBIDPGM 
03938 *  FOR THIS SECTION.                                              GBIDPGM 
03939 *                                                                 GBIDPGM 
03940 ******************************************************************GBIDPGM 
03941  4035-HDG-SA-PMT-LVL-POT.                                         GBIDPGM 
03942                                                                   GBIDPGM 
03943      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03944         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03945            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03946            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03947         END-IF                                                    GBIDPGM 
03948         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03949         SET  WS-SECTION-HDG-SA-PMT-LVL-POT   TO TRUE              GBIDPGM 
03950         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03951         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03952         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03953         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03954                                                                   GBIDPGM 
03955  4035-EXIT.                                                       GBIDPGM 
03956      EXIT.                                                        GBIDPGM 
03957 /                                                                 GBIDPGM 
03958 *MQ 05/04                                                         GBIDPGM 
03959 ******************************************************************GBIDPGM 
03960 *                                                                 GBIDPGM 
03961 *  IF THE \
03962 *  IS NOT WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE        GBIDPGM 
03963 *  WRITTEN FOR THIS SECTION.                                      GBIDPGM 
03964 *                                                                 GBIDPGM 
03965 ******************************************************************GBIDPGM 
03966  4036-HDG-SA-LIFE-CONF-MX.                                        GBIDPGM 
03967                                                                   GBIDPGM 
03968      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
03969         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
03970            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
03971            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
03972         END-IF                                                    GBIDPGM 
03973         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03974         SET  WS-SECTION-HDG-SA-LIFE-CONF-MX  TO TRUE              GBIDPGM 
03975         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
03976         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
03977         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
03978         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
03979                                                                   GBIDPGM 
03980  4036-EXIT.                                                       GBIDPGM 
03981      EXIT.                                                        GBIDPGM 
03982 /                                                                 GBIDPGM 
04510 *MQ 01/20/05 - CHANGE BEGIN                                       GBIDPGM 
03983 ******************************************************************GBIDPGM 
03984 *                                                                 GBIDPGM 
04513 *  IF THE \
04514 *  IS NOT WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE        GBIDPGM 
04515 *  WRITTEN FOR THIS SECTION.                                      GBIDPGM 
04516 *                                                                 GBIDPGM 
04517 ******************************************************************GBIDPGM 
04518  4037-HDG-MH-BP-MAX-ALL-POT.                                      GBIDPGM 
04519                                                                   GBIDPGM 
04520      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
04521         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
04522            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
04523            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
04524         END-IF                                                    GBIDPGM 
04525         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04526         SET  WS-SECTION-HDG-MH-BP-MX-POT     TO TRUE              GBIDPGM 
04527         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
04528         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
04529         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04530         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
04531                                                                   GBIDPGM 
04532  4037-EXIT.                                                       GBIDPGM 
04533      EXIT.                                                        GBIDPGM 
04534 /                                                                 GBIDPGM 
04535 ******************************************************************GBIDPGM 
04536 *                                                                 GBIDPGM 
04537 *  IF THE \
04538 *  IS NOT WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE        GBIDPGM 
04539 *  WRITTEN FOR THIS SECTION.                                      GBIDPGM 
04540 *                                                                 GBIDPGM 
04541 ******************************************************************GBIDPGM 
04542  4038-HDG-MH-LIFE-MAX-ALL-POT.                                    GBIDPGM 
04543                                                                   GBIDPGM 
04544      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
04545         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
04546            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
04547            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
04548         END-IF                                                    GBIDPGM 
04549         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04550         SET  WS-SECTION-HDG-MH-LIFE-MX-POT   TO TRUE              GBIDPGM 
04551         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
04552         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
04553         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04554         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
04555                                                                   GBIDPGM 
04556  4038-EXIT.                                                       GBIDPGM 
04557      EXIT.                                                        GBIDPGM 
04558 /                                                                 GBIDPGM 
04559 ******************************************************************GBIDPGM 
04560 *                                                                 GBIDPGM 
04561 *  IF THE \
04562 *  IS NOT WRITTEN, WRITE IT SINCE THE DETAIL LINES WILL BE        GBIDPGM 
04563 *  WRITTEN FOR THIS SECTION.                                      GBIDPGM 
04564 *                                                                 GBIDPGM 
04565 ******************************************************************GBIDPGM 
04566  4039-HDG-SMI-BP-MAX.                                             GBIDPGM 
04567                                                                   GBIDPGM 
04568      IF WS-HEADING-NOT-WRITTEN                                    GBIDPGM 
04569         IF WS-TS-LINE-SUB > 18                                    GBIDPGM 
04570            PERFORM 7300-000-WRITE-TS-QUEUE-L THRU 7300-900-EXIT   GBIDPGM 
04571            MOVE +001 TO WS-TS-LINE-SUB                            GBIDPGM 
04572         END-IF                                                    GBIDPGM 
04573         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04574         SET  WS-SECTION-HDG-SMI-BP-MAX       TO TRUE              GBIDPGM 
04575         MOVE WS-SECTION-LINE    TO WS-TS-VALUE (WS-TS-LINE-SUB)   GBIDPGM 
04576         ADD  +1                 TO WS-TS-LINE-SUB                 GBIDPGM 
04577         MOVE SPACES             TO WS-WRITE-LINE                  GBIDPGM 
04578         MOVE 'Y'                TO WS-HEADING-SW.                 GBIDPGM 
04579                                                                   GBIDPGM 
04580  4039-EXIT.                                                       GBIDPGM 
04581      EXIT.                                                        GBIDPGM 
04582 /                                                                 GBIDPGM 
04583 *MQ 01/20/05 - CHANGE END                                         GBIDPGM 
04584 ******************************************************************GBIDPGM 
04585 *                                                                 GBIDPGM 
03985 *  LOOKS AT GROUP SPECIFIC FIELDS TO DETERMINE THE PRODUCT/PROGRAMGBIDPGM 
03986 *  NAME TO BE DISPLAYED IN THE HEADING.                           GBIDPGM 
03987 *                                                                 GBIDPGM 
03988 ******************************************************************GBIDPGM 
03989  4100-DETERMINE-PRODUCT-NAME.                                     GBIDPGM 
03990                                                                   GBIDPGM 
03991 *MQ  03/02/04                                                     GBIDPGM 
03992      EVALUATE GCG-DISCOUNT-PRODUCT-TYPE                           GBIDPGM 
03993         WHEN  'BAP'                                               GBIDPGM 
03994            MOVE 'BASE PLUS                     ' TO PRGRAMO       GBIDPGM 
04596                                                  WS-TS-PRGRAM     GBIDPGM 
04597         WHEN  'BCS'                                               GBIDPGM 
04598            MOVE 'BLUE CHOICE SELECT            ' TO PRGRAMO       GBIDPGM 
03995                                                  WS-TS-PRGRAM     GBIDPGM 
03996         WHEN  'CMM'                                               GBIDPGM 
03997            MOVE 'COMPREHENSIVE MAJOR MEDICAL   ' TO PRGRAMO       GBIDPGM 
03998                                                  WS-TS-PRGRAM     GBIDPGM 
03999         WHEN  'CPO'                                               GBIDPGM 
04000            MOVE 'COMMUNITY PARTICIPATING OPTION' TO PRGRAMO       GBIDPGM 
04001                                                  WS-TS-PRGRAM     GBIDPGM 
04002         WHEN  'HMO'                                               GBIDPGM 
04003            MOVE 'HEALTH MAINTENANCE ORGANIZATIO' TO PRGRAMO       GBIDPGM 
04004                                                  WS-TS-PRGRAM     GBIDPGM 
04005         WHEN  'PON'                                               GBIDPGM 
04006            MOVE 'POINT OF SERVICE / BLUE CHOICE' TO PRGRAMO       GBIDPGM 
04007                                                  WS-TS-PRGRAM     GBIDPGM 
04008         WHEN  'PPO'                                               GBIDPGM 
04009            MOVE 'PPO HOSPITAL ONLY             ' TO PRGRAMO       GBIDPGM 
04010                                                  WS-TS-PRGRAM     GBIDPGM 
04011         WHEN  'PPN'                                               GBIDPGM 
04012            MOVE 'PPO PLUS (PPO+)               ' TO PRGRAMO       GBIDPGM 
04013                                                  WS-TS-PRGRAM     GBIDPGM 
04014         WHEN  'RPO'                                               GBIDPGM 
04015            MOVE 'RESTRICTED PROVIDER OPTION    ' TO PRGRAMO       GBIDPGM 
04016                                                  WS-TS-PRGRAM     GBIDPGM 
04017         WHEN OTHER                                                GBIDPGM 
04018            MOVE '** CAN NOT BE DETERMINED **   ' TO PRGRAMO       GBIDPGM 
04019                                                  WS-TS-PRGRAM.    GBIDPGM 
04020                                                                   GBIDPGM 
04021                                                                   GBIDPGM 
04022  4100-EXIT.                                                       GBIDPGM 
04023      EXIT.                                                        GBIDPGM 
04024 /                                                                 GBIDPGM 
04025 ******************************************************************GBIDPGM 
04026 *                                                                *GBIDPGM 
04027 *  5000   D I S P L A Y   N E X T   P A G E                      *GBIDPGM 
04028 *                                                                *GBIDPGM 
04029 *  THE OPERATOR HAS JUST PRESSED THE PF8  OR PF20 KEY AND        *GBIDPGM 
04030 *  REQUESTS THE NEXT PAGE OF OUTPUT TO BE DISPLAYED.             *GBIDPGM 
04031 *  IF THE LAST PAGE WAS JUST DISPLAYED, SEND THE FIRST PAGE.     *GBIDPGM 
04032 *                                                                *GBIDPGM 
04033 ******************************************************************GBIDPGM 
04034  5000-000-DISPLAY-NEXT-PAGE   SECTION.                            GBIDPGM 
04035  5000-010.                                                        GBIDPGM 
04036                                                                   GBIDPGM 
04037      MOVE '5000' TO WS-PARA-ID.                                   GBIDPGM 
04038                                                                   GBIDPGM 
04039 *--- READ QUEUE THAT STORES PAGE NUMBERS ------------------------*GBIDPGM 
04040      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04041                                                                   GBIDPGM 
04042                                                                   GBIDPGM 
04043 *--- READ PAGE AFTER THE LAST ONE DISPLAYED IF END NOT REACHED --*GBIDPGM 
04044 *--- OTHERWISE, GO BACK AND READ THE FIRST PAGE -----------------*GBIDPGM 
04045      IF WS-TS-LST-PAGE < WS-TS-TOT-PAGE                           GBIDPGM 
04046         ADD +1 TO WS-TS-LST-PAGE GIVING WS-PAGE-NUM               GBIDPGM 
04047         ADD +1 TO WS-PAGE-NUM    GIVING WS-TS-QITEM               GBIDPGM 
04048         PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT       GBIDPGM 
04049      ELSE                                                         GBIDPGM 
04050         MOVE +1 TO WS-PAGE-NUM                                    GBIDPGM 
04051         MOVE +2 TO WS-TS-QITEM                                    GBIDPGM 
04052         PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT.      GBIDPGM 
04053                                                                   GBIDPGM 
04054                                                                   GBIDPGM 
04055 *--- IF BAD READ OF TEMP STORAGE, ABEND -------------------------*GBIDPGM 
04056      IF  NOT WS-RETURN-CODE-CLEAN                                 GBIDPGM 
04057          MOVE WS-01-ABCODE-1BS9       TO WS-01-ABCODE             GBIDPGM 
04058          MOVE WS-01-ABCODE-1BS9-MSG   TO WS-01-ABCODE-MSG         GBIDPGM 
04059          MOVE WS-01-ABCODE-1BS9-MSG   TO ERRMSGO                  GBIDPGM 
04060          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIDPGM 
04061                                                                   GBIDPGM 
04062                                                                   GBIDPGM 
04063 *--- CLEAR DETAIL LINES ON MAP ----------------------------------*GBIDPGM 
04064      PERFORM 8900-INIT-MAP-LINES                                  GBIDPGM 
04065        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04066          FROM 1 BY 1                                              GBIDPGM 
04067            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04068                                                                   GBIDPGM 
04069                                                                   GBIDPGM 
04070 *--- MOVE HEADING ELEMENTS --------------------------------------*GBIDPGM 
04071      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04072                                                                   GBIDPGM 
04073      PERFORM 6050-MOVE-TS-HEADING.                                GBIDPGM 
04074      PERFORM 6200-FILL-MAP-DETAIL                                 GBIDPGM 
04075        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04076          FROM 1 BY 1                                              GBIDPGM 
04077            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04078                                                                   GBIDPGM 
04079 *--- UPDATE TEMP STORAGE PAGE -----------------------------------*GBIDPGM 
04080      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04081      MOVE WS-PAGE-NUM  TO WS-TS-LST-PAGE.                         GBIDPGM 
04082      PERFORM 7400-000-REWRITE-TS-QUEUE THRU 7400-900-EXIT.        GBIDPGM 
04083                                                                   GBIDPGM 
04084      PERFORM 8700-SEND-PAGE-AND-RETURN.                           GBIDPGM 
04085                                                                   GBIDPGM 
04086  5000-900-EXIT.                                                   GBIDPGM 
04087      EXIT.                                                        GBIDPGM 
04088 /                                                                 GBIDPGM 
04089 ******************************************************************GBIDPGM 
04090 *                                                                *GBIDPGM 
04091 *  5100   D I S P L A Y   P R E V   P A G E                      *GBIDPGM 
04092 *                                                                *GBIDPGM 
04093 *  THE OPERATOR HAS JUST PRESSED THE PF7  OR PF19 KEY AND        *GBIDPGM 
04094 *  REQUESTS THE PREVIOUS PAGE OF OUTPUT TO BE DISPLAYED.         *GBIDPGM 
04095 *  IF THE FIRST PAGE WAS JUST DISPLAYED, SEND THE LAST PAGE.     *GBIDPGM 
04096 *                                                                *GBIDPGM 
04097 ******************************************************************GBIDPGM 
04098  5100-000-DISPLAY-NEXT-PAGE   SECTION.                            GBIDPGM 
04099  5100-010.                                                        GBIDPGM 
04100                                                                   GBIDPGM 
04101      MOVE '5100' TO WS-PARA-ID.                                   GBIDPGM 
04102                                                                   GBIDPGM 
04103 *--- READ QUEUE THAT STORES PAGE NUMBERS ------------------------*GBIDPGM 
04104      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04105                                                                   GBIDPGM 
04106                                                                   GBIDPGM 
04107 *--- READ PAGE BEFORE THE LAST ONE DISPLAYED IF END NOT REACHED -*GBIDPGM 
04108 *--- OTHERWISE, GO BACK AND READ THE LAST  PAGE -----------------*GBIDPGM 
04109      IF WS-TS-LST-PAGE = +1                                       GBIDPGM 
04110         MOVE WS-TS-TOT-PAGE TO WS-PAGE-NUM                        GBIDPGM 
04111         ADD +1 TO WS-PAGE-NUM GIVING WS-TS-QITEM                  GBIDPGM 
04112         PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT       GBIDPGM 
04113      ELSE                                                         GBIDPGM 
04114         SUBTRACT  1 FROM WS-TS-LST-PAGE GIVING WS-PAGE-NUM        GBIDPGM 
04115         MOVE WS-TS-LST-PAGE TO WS-TS-QITEM                        GBIDPGM 
04116         PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT.      GBIDPGM 
04117                                                                   GBIDPGM 
04118                                                                   GBIDPGM 
04119 *--- IF BAD READ OF TEMP STORAGE, ABEND -------------------------*GBIDPGM 
04120      IF  NOT WS-RETURN-CODE-CLEAN                                 GBIDPGM 
04121          MOVE WS-01-ABCODE-1BS9       TO WS-01-ABCODE             GBIDPGM 
04122          MOVE WS-01-ABCODE-1BS9-MSG   TO WS-01-ABCODE-MSG         GBIDPGM 
04123          MOVE WS-01-ABCODE-1BS9-MSG   TO ERRMSGO                  GBIDPGM 
04124          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIDPGM 
04125                                                                   GBIDPGM 
04126                                                                   GBIDPGM 
04127 *--- CLEAR DETAIL LINES ON MAP ----------------------------------*GBIDPGM 
04128      PERFORM 8900-INIT-MAP-LINES                                  GBIDPGM 
04129        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04130          FROM 1 BY 1                                              GBIDPGM 
04131            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04132                                                                   GBIDPGM 
04133                                                                   GBIDPGM 
04134 *--- MOVE HEADING ELEMENTS --------------------------------------*GBIDPGM 
04135      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04136                                                                   GBIDPGM 
04137      PERFORM 6050-MOVE-TS-HEADING.                                GBIDPGM 
04138      PERFORM 6200-FILL-MAP-DETAIL                                 GBIDPGM 
04139        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04140          FROM 1 BY 1                                              GBIDPGM 
04141            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04142                                                                   GBIDPGM 
04143 *--- UPDATE TEMP STORAGE PAGE -----------------------------------*GBIDPGM 
04144      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04145      MOVE WS-PAGE-NUM  TO WS-TS-LST-PAGE.                         GBIDPGM 
04146      PERFORM 7400-000-REWRITE-TS-QUEUE THRU 7400-900-EXIT.        GBIDPGM 
04147                                                                   GBIDPGM 
04148      PERFORM 8700-SEND-PAGE-AND-RETURN.                           GBIDPGM 
04149                                                                   GBIDPGM 
04150  5100-900-EXIT.                                                   GBIDPGM 
04151      EXIT.                                                        GBIDPGM 
04152 /                                                                 GBIDPGM 
04153 ******************************************************************GBIDPGM 
04154 *                                                                *GBIDPGM 
04155 *  5200   G O   T O   L A S T   P A G E                          *GBIDPGM 
04156 *                                                                *GBIDPGM 
04157 *  THE OPERATOR HAS JUST PRESSED THE PF10 OR PF22 KEY AND        *GBIDPGM 
04158 *  REQUIRES THE LAST PAGE OF OUTPUT TO BE DISPLAYED.             *GBIDPGM 
04159 *                                                                *GBIDPGM 
04160 ******************************************************************GBIDPGM 
04161  5200-000-DISPLAY-LAST-PAGE   SECTION.                            GBIDPGM 
04162  5200-010.                                                        GBIDPGM 
04163                                                                   GBIDPGM 
04164      MOVE '5200' TO WS-PARA-ID.                                   GBIDPGM 
04165                                                                   GBIDPGM 
04166 *--- READ QUEUE THAT STORES PAGE NUMBERS ------------------------*GBIDPGM 
04167      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04168                                                                   GBIDPGM 
04169                                                                   GBIDPGM 
04170 *--- READ LAST PAGE IN TEMPORARY STORAGE QUEUE ------------------*GBIDPGM 
04171      MOVE WS-TS-TOT-PAGE TO WS-PAGE-NUM.                          GBIDPGM 
04172      ADD +1 TO WS-PAGE-NUM GIVING WS-TS-QITEM .                   GBIDPGM 
04173      PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT.         GBIDPGM 
04174                                                                   GBIDPGM 
04175                                                                   GBIDPGM 
04176 *--- IF BAD READ OF TEMP STORAGE, ABEND -------------------------*GBIDPGM 
04177      IF  NOT WS-RETURN-CODE-CLEAN                                 GBIDPGM 
04178          MOVE WS-01-ABCODE-1BS9       TO WS-01-ABCODE             GBIDPGM 
04179          MOVE WS-01-ABCODE-1BS9-MSG   TO WS-01-ABCODE-MSG         GBIDPGM 
04180          MOVE WS-01-ABCODE-1BS9-MSG   TO ERRMSGO                  GBIDPGM 
04181          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIDPGM 
04182                                                                   GBIDPGM 
04183                                                                   GBIDPGM 
04184 *--- CLEAR DETAIL LINES ON MAP ----------------------------------*GBIDPGM 
04185      PERFORM 8900-INIT-MAP-LINES                                  GBIDPGM 
04186        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04187          FROM 1 BY 1                                              GBIDPGM 
04188            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04189                                                                   GBIDPGM 
04190                                                                   GBIDPGM 
04191 *--- MOVE HEADING ELEMENTS --------------------------------------*GBIDPGM 
04192      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04193                                                                   GBIDPGM 
04194      PERFORM 6050-MOVE-TS-HEADING.                                GBIDPGM 
04195      PERFORM 6200-FILL-MAP-DETAIL                                 GBIDPGM 
04196        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04197          FROM 1 BY 1                                              GBIDPGM 
04198            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04199                                                                   GBIDPGM 
04200 *--- UPDATE TEMP STORAGE PAGE -----------------------------------*GBIDPGM 
04201      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04202      MOVE WS-PAGE-NUM  TO WS-TS-LST-PAGE.                         GBIDPGM 
04203      PERFORM 7400-000-REWRITE-TS-QUEUE THRU 7400-900-EXIT.        GBIDPGM 
04204                                                                   GBIDPGM 
04205      PERFORM 8700-SEND-PAGE-AND-RETURN.                           GBIDPGM 
04206                                                                   GBIDPGM 
04207  5200-900-EXIT.                                                   GBIDPGM 
04208      EXIT.                                                        GBIDPGM 
04209 /                                                                 GBIDPGM 
04210 ******************************************************************GBIDPGM 
04211 *                                                                *GBIDPGM 
04212 *  5300   D I S P L A Y   F I R S T   P A G E                    *GBIDPGM 
04213 *                                                                *GBIDPGM 
04214 *  THE OPERATOR HAS JUST PRESSED THE PF11 OR PF23 KEY AND        *GBIDPGM 
04215 *  REQUIRES THE FIRST PAGE OF OUTPUT TO BE DISPLAYED.            *GBIDPGM 
04216 *                                                                *GBIDPGM 
04217 ******************************************************************GBIDPGM 
04218  5300-000-DISPLAY-FIRST-PAGE  SECTION.                            GBIDPGM 
04219  5300-010.                                                        GBIDPGM 
04220                                                                   GBIDPGM 
04221      MOVE '5300' TO WS-PARA-ID.                                   GBIDPGM 
04222                                                                   GBIDPGM 
04223 *--- READ QUEUE THAT STORES PAGE NUMBERS ------------------------*GBIDPGM 
04224      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04225                                                                   GBIDPGM 
04226                                                                   GBIDPGM 
04227 *--- READ FIRST PAGE --------------------------------------------*GBIDPGM 
04228      MOVE +1 TO WS-PAGE-NUM.                                      GBIDPGM 
04229      ADD  +1 TO WS-PAGE-NUM GIVING WS-TS-QITEM.                   GBIDPGM 
04230      PERFORM 7200-000-READ-TS-QUEUE-L THRU 7200-900-EXIT.         GBIDPGM 
04231                                                                   GBIDPGM 
04232                                                                   GBIDPGM 
04233 *--- IF BAD READ OF TEMP STORAGE, ABEND -------------------------*GBIDPGM 
04234      IF  NOT WS-RETURN-CODE-CLEAN                                 GBIDPGM 
04235          MOVE WS-01-ABCODE-1BS9       TO WS-01-ABCODE             GBIDPGM 
04236          MOVE WS-01-ABCODE-1BS9-MSG   TO WS-01-ABCODE-MSG         GBIDPGM 
04237          MOVE WS-01-ABCODE-1BS9-MSG   TO ERRMSGO                  GBIDPGM 
04238          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIDPGM 
04239                                                                   GBIDPGM 
04240                                                                   GBIDPGM 
04241 *--- CLEAR DETAIL LINES ON MAP ----------------------------------*GBIDPGM 
04242      PERFORM 8900-INIT-MAP-LINES                                  GBIDPGM 
04243        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04244          FROM 1 BY 1                                              GBIDPGM 
04245            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04246                                                                   GBIDPGM 
04247                                                                   GBIDPGM 
04248 *--- MOVE HEADING ELEMENTS --------------------------------------*GBIDPGM 
04249      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04250                                                                   GBIDPGM 
04251      PERFORM 6050-MOVE-TS-HEADING.                                GBIDPGM 
04252      PERFORM 6200-FILL-MAP-DETAIL                                 GBIDPGM 
04253        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04254          FROM 1 BY 1                                              GBIDPGM 
04255            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04256                                                                   GBIDPGM 
04257 *--- UPDATE TEMP STORAGE PAGE -----------------------------------*GBIDPGM 
04258      PERFORM 7250-000-READ-TS-QUEUE-P THRU 7250-900-EXIT.         GBIDPGM 
04259      MOVE WS-PAGE-NUM  TO WS-TS-LST-PAGE.                         GBIDPGM 
04260      PERFORM 7400-000-REWRITE-TS-QUEUE THRU 7400-900-EXIT.        GBIDPGM 
04261                                                                   GBIDPGM 
04262      PERFORM 8700-SEND-PAGE-AND-RETURN.                           GBIDPGM 
04263                                                                   GBIDPGM 
04264  5300-900-EXIT.                                                   GBIDPGM 
04265      EXIT.                                                        GBIDPGM 
04266 /                                                                 GBIDPGM 
04267 ******************************************************************GBIDPGM 
04268 *                                                                 GBIDPGM 
04269 *  MOVES GROUP SPECIFIC FIELDS TO HEADING.                        GBIDPGM 
04270 *                                                                 GBIDPGM 
04271 ******************************************************************GBIDPGM 
04272  6000-MOVE-KEY-TO-SCREEN.                                         GBIDPGM 
04273                                                                   GBIDPGM 
04274 **************************************************************    GBIDPGM 
04275 *--- MOVE SUBSCRIBER NUMBER ALSO ----------------------------*    GBIDPGM 
04276 **************************************************************    GBIDPGM 
04277                                                                   GBIDPGM 
04278      MOVE GI2-SUBSCRIBER  TO SUBIDO.                              GBIDPGM 
04279                                                                   GBIDPGM 
04280      MOVE GCG-GROUP-NUM   TO GROUPO.                              GBIDPGM 
04281      MOVE GCG-SECTION-NUM TO SECTNO.                              GBIDPGM 
04282                                                                   GBIDPGM 
04283      MOVE GCG-EFF-DT    TO HGADATE-JULIAN1.                       GBIDPGM 
04284      PERFORM 9200-JUL-TO-GREG-DATE.                               GBIDPGM 
04285      MOVE HGADATE-DATE2 TO EFFDTO.                                GBIDPGM 
04286                                                                   GBIDPGM 
04287  6000-EXIT.                                                       GBIDPGM 
04288      EXIT.                                                        GBIDPGM 
04289 /                                                                 GBIDPGM 
04290 ******************************************************************GBIDPGM 
04291 *                                                                 GBIDPGM 
04292 *  MOVE HEADING INFO IN TEMPORARY STORAGE TO MAP.                 GBIDPGM 
04293 *                                                                 GBIDPGM 
04294 ******************************************************************GBIDPGM 
04295  6050-MOVE-TS-HEADING.                                            GBIDPGM 
04296                                                                   GBIDPGM 
04297      MOVE WS-PAGE-NUM      TO CPAGEO.                             GBIDPGM 
04298      MOVE WS-TS-TOT-PAGE   TO TPAGEO.                             GBIDPGM 
04299      MOVE WS-TS-SUBSCRIBER TO SUBIDO.                             GBIDPGM 
04300      MOVE WS-TS-GROUP      TO GROUPO.                             GBIDPGM 
04301      MOVE WS-TS-SECTION    TO SECTNO.                             GBIDPGM 
04302      MOVE WS-TS-EFFDT      TO EFFDTO.                             GBIDPGM 
04303      MOVE WS-TS-PRGRAM     TO PRGRAMO.                            GBIDPGM 
04304                                                                   GBIDPGM 
04305  6050-EXIT.                                                       GBIDPGM 
04306      EXIT.                                                        GBIDPGM 
04307 /                                                                 GBIDPGM 
04308 ******************************************************************GBIDPGM 
04309 *                                                                 GBIDPGM 
04310 *  MOVE HEADING INFO TO TEMPORARY STORAGE AREA.                   GBIDPGM 
04311 *  THIS PARAGRAPH SHOULD ONLY BE PERFORMED ONCE BEFORE ALL THE    GBIDPGM 
04312 *  OTHER TEMPORARY STORAGE QUEUES ARE BUILT. HOWEVER, THIS ITEM   GBIDPGM 
04313 *  CAN LATER BE UPDATED.                                          GBIDPGM 
04314 *                                                                 GBIDPGM 
04315 ******************************************************************GBIDPGM 
04316  6100-STORE-TS-HEADING.                                           GBIDPGM 
04317                                                                   GBIDPGM 
04318      MOVE GI2-SUBSCRIBER   TO WS-TS-SUBSCRIBER.                   GBIDPGM 
04319      MOVE GCG-GROUP-NUM    TO WS-TS-GROUP.                        GBIDPGM 
04320      MOVE GCG-SECTION-NUM  TO WS-TS-SECTION.                      GBIDPGM 
04321                                                                   GBIDPGM 
04322      MOVE GCG-EFF-DT    TO HGADATE-JULIAN1.                       GBIDPGM 
04323      PERFORM 9200-JUL-TO-GREG-DATE.                               GBIDPGM 
04324      MOVE HGADATE-DATE2 TO WS-TS-EFFDT.                           GBIDPGM 
04325                                                                   GBIDPGM 
04326 *--- PRODUCT NAME IS DETERMINED IN 4100-DETERMINE-PRODUCT-NAME --*GBIDPGM 
04327      PERFORM 4100-DETERMINE-PRODUCT-NAME.                         GBIDPGM 
04328                                                                   GBIDPGM 
04329      PERFORM 7350-000-WRITE-TS-QUEUE-P THRU 7350-900-EXIT.        GBIDPGM 
04330                                                                   GBIDPGM 
04331  6100-EXIT.                                                       GBIDPGM 
04332      EXIT.                                                        GBIDPGM 
04333 /                                                                 GBIDPGM 
04334 ******************************************************************GBIDPGM 
04335 *                                                                 GBIDPGM 
04336 *  MOVES TEMP STORAGE LINES TO MAP.                               GBIDPGM 
04337 *                                                                 GBIDPGM 
04338 ******************************************************************GBIDPGM 
04339  6200-FILL-MAP-DETAIL.                                            GBIDPGM 
04340                                                                   GBIDPGM 
04341      MOVE WS-TS-VALUE (WS-TS-LINE-SUB)                            GBIDPGM 
04342        TO DETLNO (WS-TS-LINE-SUB),                                GBIDPGM 
04343           WS-HOLD-SECTION-LINE.                                   GBIDPGM 
04344                                                                   GBIDPGM 
04345      IF WS-HOLD-SECTION-IND NOT = SPACES                          GBIDPGM 
04346         MOVE DFHBMABF TO DETLNA (WS-TS-LINE-SUB).                 GBIDPGM 
04347                                                                   GBIDPGM 
04348  6200-EXIT.                                                       GBIDPGM 
04349      EXIT.                                                        GBIDPGM 
04350 ******************************************************************GBIDPGM 
04351 *                                                                *GBIDPGM 
04352 *  7200   R E A D   P A G E   Q U E U E                          *GBIDPGM 
04353 *                                                                *GBIDPGM 
04354 *  READ A RECORD FROM TEMPORARY STORAGE QUEUE.                   *GBIDPGM 
04355 *                                                                *GBIDPGM 
04356 ******************************************************************GBIDPGM 
04357  7200-000-READ-TS-QUEUE-L       SECTION.                          GBIDPGM 
04358  7200-010.                                                        GBIDPGM 
04359                                                                   GBIDPGM 
04360      MOVE '7200' TO WS-PARA-ID.                                   GBIDPGM 
04361                                                                   GBIDPGM 
04362      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04363                                                                   GBIDPGM 
04364      EXEC CICS HANDLE CONDITION  INVREQ   (7200-020-INVREQ)       GBIDPGM 
04365                                  IOERR    (7200-030-IOERR)        GBIDPGM 
04366                                  ITEMERR  (7200-040-EOF)          GBIDPGM 
04367                                  LENGERR  (7200-050-LENGERR)      GBIDPGM 
04368                                  QIDERR   (7200-060-QIDERR)       GBIDPGM 
04369                                  END-EXEC.                        GBIDPGM 
04370                                                                   GBIDPGM 
04371      EXEC CICS READQ TS QUEUE    (WS-TS-QNAME)                    GBIDPGM 
04372                         INTO     (WS-TEMP-STORAGE)                GBIDPGM 
04373                         ITEM     (WS-TS-QITEM)                    GBIDPGM 
04374                         LENGTH   (WS-TS-QLENGTH)                  GBIDPGM 
04375                         END-EXEC.                                 GBIDPGM 
04376                                                                   GBIDPGM 
04377      GO TO 7200-900-EXIT.                                         GBIDPGM 
04378                                                                   GBIDPGM 
04379  7200-020-INVREQ.                                                 GBIDPGM 
04380      MOVE WS-01-ABCODE-1BS1       TO WS-01-ABCODE.                GBIDPGM 
04381      MOVE WS-01-ABCODE-1BS1-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04382      MOVE WS-01-ABCODE-1BS1-MSG   TO ERRMSGO.                     GBIDPGM 
04383      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04384                                                                   GBIDPGM 
04385  7200-030-IOERR.                                                  GBIDPGM 
04386      MOVE WS-01-ABCODE-1BS2       TO WS-01-ABCODE.                GBIDPGM 
04387      MOVE WS-01-ABCODE-1BS2-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04388      MOVE WS-01-ABCODE-1BS2-MSG   TO ERRMSGO.                     GBIDPGM 
04389      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04390                                                                   GBIDPGM 
04391  7200-040-EOF.                                                    GBIDPGM 
04392      MOVE WS-LIT-RETURN-PAGE-QUEUE-EOF TO WS-RETURN-CODE.         GBIDPGM 
04393      GO TO 7200-900-EXIT.                                         GBIDPGM 
04394                                                                   GBIDPGM 
04395  7200-050-LENGERR.                                                GBIDPGM 
04396      MOVE WS-01-ABCODE-1BS3       TO WS-01-ABCODE.                GBIDPGM 
04397      MOVE WS-01-ABCODE-1BS3-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04398      MOVE WS-01-ABCODE-1BS3-MSG   TO ERRMSGO.                     GBIDPGM 
04399      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04400                                                                   GBIDPGM 
04401  7200-060-QIDERR.                                                 GBIDPGM 
04402      MOVE WS-01-ABCODE-1BS4       TO WS-01-ABCODE.                GBIDPGM 
04403      MOVE WS-01-ABCODE-1BS4-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04404      MOVE WS-01-ABCODE-1BS4-MSG   TO ERRMSGO.                     GBIDPGM 
04405      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04406                                                                   GBIDPGM 
04407  7200-900-EXIT.                                                   GBIDPGM 
04408      EXIT.                                                        GBIDPGM 
04409                                                                   GBIDPGM 
04410 ******************************************************************GBIDPGM 
04411 *                                                                *GBIDPGM 
04412 *  7250   R E A D   P A G E   Q U E U E                          *GBIDPGM 
04413 *                                                                *GBIDPGM 
04414 *  READ A RECORD FROM THE PAGE QUEUE.                            *GBIDPGM 
04415 *                                                                *GBIDPGM 
04416 ******************************************************************GBIDPGM 
04417  7250-000-READ-TS-QUEUE-P       SECTION.                          GBIDPGM 
04418  7250-010.                                                        GBIDPGM 
04419                                                                   GBIDPGM 
04420      MOVE '7250' TO WS-PARA-ID.                                   GBIDPGM 
04421                                                                   GBIDPGM 
04422      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04423                                                                   GBIDPGM 
04424      MOVE +1 TO WS-TS-QITEM.                                      GBIDPGM 
04425                                                                   GBIDPGM 
04426      EXEC CICS HANDLE CONDITION  INVREQ   (7250-020-INVREQ)       GBIDPGM 
04427                                  IOERR    (7250-030-IOERR)        GBIDPGM 
04428                                  ITEMERR  (7250-040-EOF)          GBIDPGM 
04429                                  LENGERR  (7250-050-LENGERR)      GBIDPGM 
04430                                  QIDERR   (7250-060-QIDERR)       GBIDPGM 
04431                                  END-EXEC.                        GBIDPGM 
04432                                                                   GBIDPGM 
04433      EXEC CICS READQ TS QUEUE    (WS-TS-QNAME)                    GBIDPGM 
04434                         INTO     (WS-TEMP-STORAGE-P)              GBIDPGM 
04435                         ITEM     (WS-TS-QITEM)                    GBIDPGM 
04436                         LENGTH   (WS-TS-QLENGTH-P)                GBIDPGM 
04437                         END-EXEC.                                 GBIDPGM 
04438                                                                   GBIDPGM 
04439      GO TO 7250-900-EXIT.                                         GBIDPGM 
04440                                                                   GBIDPGM 
04441  7250-020-INVREQ.                                                 GBIDPGM 
04442      MOVE WS-01-ABCODE-1BS1       TO WS-01-ABCODE.                GBIDPGM 
04443      MOVE WS-01-ABCODE-1BS1-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04444      MOVE WS-01-ABCODE-1BS1-MSG   TO ERRMSGO.                     GBIDPGM 
04445      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04446                                                                   GBIDPGM 
04447  7250-030-IOERR.                                                  GBIDPGM 
04448      MOVE WS-01-ABCODE-1BS2       TO WS-01-ABCODE.                GBIDPGM 
04449      MOVE WS-01-ABCODE-1BS2-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04450      MOVE WS-01-ABCODE-1BS2-MSG   TO ERRMSGO.                     GBIDPGM 
04451      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04452                                                                   GBIDPGM 
04453  7250-040-EOF.                                                    GBIDPGM 
04454      MOVE WS-LIT-RETURN-PAGE-QUEUE-EOF TO WS-RETURN-CODE.         GBIDPGM 
04455      GO TO 7250-900-EXIT.                                         GBIDPGM 
04456                                                                   GBIDPGM 
04457  7250-050-LENGERR.                                                GBIDPGM 
04458      MOVE WS-01-ABCODE-1BS3       TO WS-01-ABCODE.                GBIDPGM 
04459      MOVE WS-01-ABCODE-1BS3-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04460      MOVE WS-01-ABCODE-1BS3-MSG   TO ERRMSGO.                     GBIDPGM 
04461      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04462                                                                   GBIDPGM 
04463  7250-060-QIDERR.                                                 GBIDPGM 
04464      MOVE WS-01-ABCODE-1BS4       TO WS-01-ABCODE.                GBIDPGM 
04465      MOVE WS-01-ABCODE-1BS4-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04466      MOVE WS-01-ABCODE-1BS4-MSG   TO ERRMSGO.                     GBIDPGM 
04467      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04468                                                                   GBIDPGM 
04469  7250-900-EXIT.                                                   GBIDPGM 
04470      EXIT.                                                        GBIDPGM 
04471                                                                   GBIDPGM 
04472 ******************************************************************GBIDPGM 
04473 *                                                                *GBIDPGM 
04474 *  7300   W R I T E   P A G E   Q U E U E                        *GBIDPGM 
04475 *                                                                *GBIDPGM 
04476 *  WRITE A RECORD TO THE TEMPORARY STORAGE QUEUE.                *GBIDPGM 
04477 *                                                                *GBIDPGM 
04478 ******************************************************************GBIDPGM 
04479  7300-000-WRITE-TS-QUEUE-L       SECTION.                         GBIDPGM 
04480  7300-010.                                                        GBIDPGM 
04481      MOVE '7300' TO WS-PARA-ID.                                   GBIDPGM 
04482                                                                   GBIDPGM 
04483      ADD +1 TO WS-TS-QITEM,                                       GBIDPGM 
04484                WS-PAGE-NUM,                                       GBIDPGM 
04485                WS-TPAGE-NUM.                                      GBIDPGM 
04486                                                                   GBIDPGM 
04487      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04488                                                                   GBIDPGM 
04489      EXEC CICS HANDLE CONDITION  INVREQ   (7300-020-INVREQ)       GBIDPGM 
04490                                  IOERR    (7300-030-IOERR)        GBIDPGM 
04491                                  NOSPACE  (7300-040-NOSPACE)      GBIDPGM 
04492                                  QIDERR   (7300-050-QIDERR)       GBIDPGM 
04493                                  END-EXEC.                        GBIDPGM 
04494                                                                   GBIDPGM 
04495      EXEC CICS WRITEQ TS QUEUE    (WS-TS-QNAME)                   GBIDPGM 
04496                          FROM     (WS-TEMP-STORAGE)               GBIDPGM 
04497                          LENGTH   (WS-TS-QLENGTH)                 GBIDPGM 
04498                          NUMITEMS (WS-TS-LAST-PAGE)               GBIDPGM 
04499                          END-EXEC.                                GBIDPGM 
04500                                                                   GBIDPGM 
04501      PERFORM 8800-INIT-TS-LINES                                   GBIDPGM 
04502        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04503          FROM 1 BY 1                                              GBIDPGM 
04504            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04505                                                                   GBIDPGM 
04506      GO TO 7300-900-EXIT.                                         GBIDPGM 
04507                                                                   GBIDPGM 
04508  7300-020-INVREQ.                                                 GBIDPGM 
04509      MOVE WS-01-ABCODE-1BS5       TO WS-01-ABCODE.                GBIDPGM 
04510      MOVE WS-01-ABCODE-1BS5-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04511      MOVE WS-01-ABCODE-1BS5-MSG   TO ERRMSGO.                     GBIDPGM 
04512      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04513                                                                   GBIDPGM 
04514  7300-030-IOERR.                                                  GBIDPGM 
04515      MOVE WS-01-ABCODE-1BS6       TO WS-01-ABCODE.                GBIDPGM 
04516      MOVE WS-01-ABCODE-1BS6-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04517      MOVE WS-01-ABCODE-1BS6-MSG   TO ERRMSGO.                     GBIDPGM 
04518      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04519                                                                   GBIDPGM 
04520  7300-040-NOSPACE.                                                GBIDPGM 
04521      MOVE WS-01-ABCODE-1BS7       TO WS-01-ABCODE.                GBIDPGM 
04522      MOVE WS-01-ABCODE-1BS7-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04523      MOVE WS-01-ABCODE-1BS7-MSG   TO ERRMSGO.                     GBIDPGM 
04524      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04525                                                                   GBIDPGM 
04526  7300-050-QIDERR.                                                 GBIDPGM 
04527      MOVE WS-01-ABCODE-1BS8       TO WS-01-ABCODE.                GBIDPGM 
04528      MOVE WS-01-ABCODE-1BS8-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04529      MOVE WS-01-ABCODE-1BS8-MSG   TO ERRMSGO.                     GBIDPGM 
04530      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04531                                                                   GBIDPGM 
04532  7300-900-EXIT.                                                   GBIDPGM 
04533      EXIT.                                                        GBIDPGM 
04534                                                                   GBIDPGM 
04535 ******************************************************************GBIDPGM 
04536 *                                                                *GBIDPGM 
04537 *  7350   W R I T E   P A G E   Q U E U E                        *GBIDPGM 
04538 *                                                                *GBIDPGM 
04539 *  WRITE A RECORD TO THE PAGE QUEUE.                             *GBIDPGM 
04540 *                                                                *GBIDPGM 
04541 ******************************************************************GBIDPGM 
04542  7350-000-WRITE-TS-QUEUE-P       SECTION.                         GBIDPGM 
04543  7350-010.                                                        GBIDPGM 
04544                                                                   GBIDPGM 
04545      MOVE '7350' TO WS-PARA-ID.                                   GBIDPGM 
04546                                                                   GBIDPGM 
04547      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04548                                                                   GBIDPGM 
04549      MOVE +1 TO WS-TS-QITEM,                                      GBIDPGM 
04550                 WS-PAGE-NUM.                                      GBIDPGM 
04551                                                                   GBIDPGM 
04552      EXEC CICS HANDLE CONDITION  INVREQ   (7350-020-INVREQ)       GBIDPGM 
04553                                  IOERR    (7350-030-IOERR)        GBIDPGM 
04554                                  NOSPACE  (7350-040-NOSPACE)      GBIDPGM 
04555                                  QIDERR   (7350-050-QIDERR)       GBIDPGM 
04556                                  END-EXEC.                        GBIDPGM 
04557                                                                   GBIDPGM 
04558      EXEC CICS WRITEQ TS QUEUE    (WS-TS-QNAME)                   GBIDPGM 
04559                          FROM     (WS-TEMP-STORAGE-P)             GBIDPGM 
04560                          LENGTH   (WS-TS-QLENGTH-P)               GBIDPGM 
04561                          ITEM     (WS-TS-QITEM)                   GBIDPGM 
04562                          END-EXEC.                                GBIDPGM 
04563                                                                   GBIDPGM 
04564      GO TO 7350-900-EXIT.                                         GBIDPGM 
04565                                                                   GBIDPGM 
04566  7350-020-INVREQ.                                                 GBIDPGM 
04567      MOVE WS-01-ABCODE-1BS5       TO WS-01-ABCODE.                GBIDPGM 
04568      MOVE WS-01-ABCODE-1BS5-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04569      MOVE WS-01-ABCODE-1BS5-MSG   TO ERRMSGO.                     GBIDPGM 
04570      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04571                                                                   GBIDPGM 
04572  7350-030-IOERR.                                                  GBIDPGM 
04573      MOVE WS-01-ABCODE-1BS6       TO WS-01-ABCODE.                GBIDPGM 
04574      MOVE WS-01-ABCODE-1BS6-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04575      MOVE WS-01-ABCODE-1BS6-MSG   TO ERRMSGO.                     GBIDPGM 
04576      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04577                                                                   GBIDPGM 
04578  7350-040-NOSPACE.                                                GBIDPGM 
04579      MOVE WS-01-ABCODE-1BS7       TO WS-01-ABCODE.                GBIDPGM 
04580      MOVE WS-01-ABCODE-1BS7-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04581      MOVE WS-01-ABCODE-1BS7-MSG   TO ERRMSGO.                     GBIDPGM 
04582      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04583                                                                   GBIDPGM 
04584  7350-050-QIDERR.                                                 GBIDPGM 
04585      MOVE WS-01-ABCODE-1BS8       TO WS-01-ABCODE.                GBIDPGM 
04586      MOVE WS-01-ABCODE-1BS8-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04587      MOVE WS-01-ABCODE-1BS8-MSG   TO ERRMSGO.                     GBIDPGM 
04588      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04589                                                                   GBIDPGM 
04590  7350-900-EXIT.                                                   GBIDPGM 
04591      EXIT.                                                        GBIDPGM 
04592                                                                   GBIDPGM 
04593 ******************************************************************GBIDPGM 
04594 *                                                                *GBIDPGM 
04595 *  7400   W R I T E   P A G E   Q U E U E                        *GBIDPGM 
04596 *                                                                *GBIDPGM 
04597 *  RE-WRITE A RECORD TO THE PAGE QUEUE.                          *GBIDPGM 
04598 *                                                                *GBIDPGM 
04599 ******************************************************************GBIDPGM 
04600  7400-000-REWRITE-TS-QUEUE       SECTION.                         GBIDPGM 
04601  7400-010.                                                        GBIDPGM 
04602      MOVE '7400' TO WS-PARA-ID.                                   GBIDPGM 
04603                                                                   GBIDPGM 
04604      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04605                                                                   GBIDPGM 
04606      MOVE +1 TO WS-TS-QITEM.                                      GBIDPGM 
04607                                                                   GBIDPGM 
04608      EXEC CICS HANDLE CONDITION  INVREQ   (7400-020-INVREQ)       GBIDPGM 
04609                                  IOERR    (7400-030-IOERR)        GBIDPGM 
04610                                  NOSPACE  (7400-040-NOSPACE)      GBIDPGM 
04611                                  QIDERR   (7400-050-QIDERR)       GBIDPGM 
04612                                  END-EXEC.                        GBIDPGM 
04613                                                                   GBIDPGM 
04614      EXEC CICS WRITEQ TS QUEUE    (WS-TS-QNAME)                   GBIDPGM 
04615                          FROM     (WS-TEMP-STORAGE-P)             GBIDPGM 
04616                          LENGTH   (WS-TS-QLENGTH-P)               GBIDPGM 
04617                          ITEM     (WS-TS-QITEM)                   GBIDPGM 
04618                          REWRITE                                  GBIDPGM 
04619                          END-EXEC.                                GBIDPGM 
04620                                                                   GBIDPGM 
04621      PERFORM 8800-INIT-TS-LINES                                   GBIDPGM 
04622        VARYING WS-TS-LINE-SUB                                     GBIDPGM 
04623          FROM 1 BY 1                                              GBIDPGM 
04624            UNTIL WS-TS-LINE-SUB > 19.                             GBIDPGM 
04625                                                                   GBIDPGM 
04626      GO TO 7400-900-EXIT.                                         GBIDPGM 
04627                                                                   GBIDPGM 
04628  7400-020-INVREQ.                                                 GBIDPGM 
04629      MOVE WS-01-ABCODE-1BS5       TO WS-01-ABCODE.                GBIDPGM 
04630      MOVE WS-01-ABCODE-1BS5-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04631      MOVE WS-01-ABCODE-1BS5-MSG   TO ERRMSGO.                     GBIDPGM 
04632      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04633                                                                   GBIDPGM 
04634  7400-030-IOERR.                                                  GBIDPGM 
04635      MOVE WS-01-ABCODE-1BS6       TO WS-01-ABCODE.                GBIDPGM 
04636      MOVE WS-01-ABCODE-1BS6-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04637      MOVE WS-01-ABCODE-1BS6-MSG   TO ERRMSGO.                     GBIDPGM 
04638      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04639                                                                   GBIDPGM 
04640  7400-040-NOSPACE.                                                GBIDPGM 
04641      MOVE WS-01-ABCODE-1BS7       TO WS-01-ABCODE.                GBIDPGM 
04642      MOVE WS-01-ABCODE-1BS7-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04643      MOVE WS-01-ABCODE-1BS7-MSG   TO ERRMSGO.                     GBIDPGM 
04644      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04645                                                                   GBIDPGM 
04646  7400-050-QIDERR.                                                 GBIDPGM 
04647      MOVE WS-01-ABCODE-1BS8       TO WS-01-ABCODE.                GBIDPGM 
04648      MOVE WS-01-ABCODE-1BS8-MSG   TO WS-01-ABCODE-MSG.            GBIDPGM 
04649      MOVE WS-01-ABCODE-1BS8-MSG   TO ERRMSGO.                     GBIDPGM 
04650      PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                       GBIDPGM 
04651                                                                   GBIDPGM 
04652  7400-900-EXIT.                                                   GBIDPGM 
04653      EXIT.                                                        GBIDPGM 
04654                                                                   GBIDPGM 
04655 /                                                                 GBIDPGM 
04656 ******************************************************************GBIDPGM 
04657 *                                                                 GBIDPGM 
04658 *  READ THE GROUP SPECIFIC RECORD.                                GBIDPGM 
04659 *                                                                 GBIDPGM 
04660 ******************************************************************GBIDPGM 
04661  8000-READ-GROUPSPC.                                              GBIDPGM 
04662                                                                   GBIDPGM 
04663      COMPUTE  WS-IO-PARM-GROUPSPC-LEN      =                      GBIDPGM 
04664               GC-GCIOPARM-LEN              +                      GBIDPGM 
04665               GC-GCGRPSPC-FIXED-LEN        +                      GBIDPGM 
04666             ( GC-GCGRPSPC-VARY-MAX-OCUR    *                      GBIDPGM 
04667               GC-GCGRPSPC-VARY-LEN ).                             GBIDPGM 
04668                                                                   GBIDPGM 
04669      EXEC CICS GETMAIN  SET (IO-PARM-GROUPSPC-AREA-1)             GBIDPGM 
04670                         INITIMG(WS-HEX-00)                        GBIDPGM 
04671                         LENGTH (WS-IO-PARM-GROUPSPC-LEN)          GBIDPGM 
04672                         END-EXEC.                                 GBIDPGM 
04673                                                                   GBIDPGM 
04674      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GBIDPGM 
04675                                   TO GCG-COUNT-TAB-PROVN-POINTERS.GBIDPGM 
04676      MOVE 'RD '                   TO GCIO-FILE-ACCESS-CODE.       GBIDPGM 
04677      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBIDPGM 
04678      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBIDPGM 
04679      MOVE GCG-GRP-SPECIF-ID       TO GCIO-FILE-KEY.               GBIDPGM 
04680                                                                   GBIDPGM 
04681      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GBIDPGM 
04682                       COMMAREA(IO-PARM-GROUPSPC-AREA-1)           GBIDPGM 
04683                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBIDPGM 
04684                       END-EXEC.                                   GBIDPGM 
04685                                                                   GBIDPGM 
04686  8000-EXIT.                                                       GBIDPGM 
04687      EXIT.                                                        GBIDPGM 
04688 /                                                                 GBIDPGM 
04689 ******************************************************************GBIDPGM 
04690 *                                                                 GBIDPGM 
04691 *  READ THE CONTRACT RECORD.                                      GBIDPGM 
04692 *                                                                 GBIDPGM 
04693 ******************************************************************GBIDPGM 
04694  8100-READ-CONTRACT.                                              GBIDPGM 
04695                                                                   GBIDPGM 
04696      COMPUTE  WS-IO-PARM-CONTRACT-LEN      =                      GBIDPGM 
04697               GC-GCIOPARM-LEN              +                      GBIDPGM 
04698               GC-GCCONTR-FIXED-LEN         +                      GBIDPGM 
04699             ( GC-GCCONTR-VARY-MAX-OCUR     *                      GBIDPGM 
04700               GC-GCCONTR-VARY-LEN ).                              GBIDPGM 
04701                                                                   GBIDPGM 
04702      EXEC CICS GETMAIN  SET (IO-PARM-CONTRACT-AREA-1)             GBIDPGM 
04703                         INITIMG(WS-HEX-00)                        GBIDPGM 
04704                         LENGTH (WS-IO-PARM-CONTRACT-LEN)          GBIDPGM 
04705                         END-EXEC.                                 GBIDPGM 
04706                                                                   GBIDPGM 
04707      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GBIDPGM 
04708                                   TO GCT-COUNT-BEN-PROVN-POINTERS.GBIDPGM 
04709      MOVE 'RD '                   TO GCIO3-FILE-ACCESS-CODE.      GBIDPGM 
04710      MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBIDPGM 
04711      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBIDPGM 
04712      MOVE GCT-CONTRACT-ID         TO GCIO3-FILE-KEY.              GBIDPGM 
04713                                                                   GBIDPGM 
04714      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIDPGM 
04715                       COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBIDPGM 
04716                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBIDPGM 
04717                       END-EXEC.                                   GBIDPGM 
04718                                                                   GBIDPGM 
04719  8100-EXIT.                                                       GBIDPGM 
04720      EXIT.                                                        GBIDPGM 
04721 /                                                                 GBIDPGM 
04722 ******************************************************************GBIDPGM 
04723 *                                                                 GBIDPGM 
04724 *  READ THE TABULAR RECORD.                                       GBIDPGM 
04725 *                                                                 GBIDPGM 
04726 ******************************************************************GBIDPGM 
04727  8200-READ-TABULAR.                                               GBIDPGM 
04728                                                                   GBIDPGM 
04729      COMPUTE   WS-IO-PARM-TABULAR-LEN     =                       GBIDPGM 
04730                GC-GCIOPARM-LEN            +                       GBIDPGM 
04731                GC-GCTABULR-ABM-FIXED-LEN  +                       GBIDPGM 
04732               (GC-GCTABULR-ABM-VARY-LEN   *                       GBIDPGM 
04733                GC-GCTABULR-ABM-VARY-MAX-OCUR).                    GBIDPGM 
04734                                                                   GBIDPGM 
04735      EXEC CICS GETMAIN                                            GBIDPGM 
04736                SET(IO-PARM-TABULAR-AREA-1)                        GBIDPGM 
04737                INITIMG(WS-HEX-00)                                 GBIDPGM 
04738                LENGTH(WS-IO-PARM-TABULAR-LEN)                     GBIDPGM 
04739      END-EXEC.                                                    GBIDPGM 
04740                                                                   GBIDPGM 
04741      MOVE 'RD '                    TO GCIO5-FILE-ACCESS-CODE.     GBIDPGM 
04742      MOVE 'GCTABULR'               TO GCIO5-FILE-DDNAME.          GBIDPGM 
04743      MOVE '1'                      TO GCIO5-IO-AREA-TO-USE.       GBIDPGM 
04744      MOVE GAA-TABULAR-PROVISION-ID TO GCIO5-FILE-KEY.             GBIDPGM 
04745                                                                   GBIDPGM 
04746      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GBIDPGM 
04747        TO GAA-ENTRY-COUNT.                                        GBIDPGM 
04748                                                                   GBIDPGM 
04749      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GBIDPGM 
04750         COMMAREA(IO-PARM-TABULAR-AREA-1)                          GBIDPGM 
04751         LENGTH(WS-IO-PARM-TABULAR-LEN) END-EXEC.                  GBIDPGM 
04752                                                                   GBIDPGM 
04753      IF NOT GCIO5-GOOD-RETURN                                     GBIDPGM 
04754         MOVE '1BF3'  TO  WS-ABEND-CODE                            GBIDPGM 
04755         MOVE '*** ERROR READING ALL LEVEL TABULAR ***'            GBIDPGM 
04756           TO  ERRMSGO                                             GBIDPGM 
04757         PERFORM 9999-ABEND.                                       GBIDPGM 
04758                                                                   GBIDPGM 
04759  8200-EXIT.                                                       GBIDPGM 
04760      EXIT.                                                        GBIDPGM 
04761 ******************************************************************GBIDPGM 
04762 *                                                                *GBIDPGM 
04763 *  8300   D E L E T E    P A G E   Q U E U E                     *GBIDPGM 
04764 *                                                                *GBIDPGM 
04765 *  DELETE THE PAGE QUEUE.                                        *GBIDPGM 
04766 *                                                                *GBIDPGM 
04767 ******************************************************************GBIDPGM 
04768  8300-000-DELETE-PAGE-QUEUE       SECTION.                        GBIDPGM 
04769  8300-010.                                                        GBIDPGM 
04770                                                                   GBIDPGM 
04771      MOVE '8300' TO WS-PARA-ID.                                   GBIDPGM 
04772                                                                   GBIDPGM 
04773      MOVE WS-LIT-RETURN-CODE-CLEAN TO WS-RETURN-CODE.             GBIDPGM 
04774                                                                   GBIDPGM 
04775      EXEC CICS HANDLE CONDITION ERROR (8300-900-EXIT) END-EXEC.   GBIDPGM 
04776                                                                   GBIDPGM 
04777      EXEC CICS DELETEQ TS QUEUE (WS-TS-QNAME) END-EXEC.           GBIDPGM 
04778                                                                   GBIDPGM 
04779  8300-900-EXIT.                                                   GBIDPGM 
04780      EXIT.                                                        GBIDPGM 
04781 /                                                                 GBIDPGM 
04782 ******************************************************************GBIDPGM 
04783 *                                                                *GBIDPGM 
04784 *  SEND PAGE AND RETURN.                                         *GBIDPGM 
04785 *                                                                *GBIDPGM 
04786 ******************************************************************GBIDPGM 
04787  8700-SEND-PAGE-AND-RETURN.                                       GBIDPGM 
04788                                                                   GBIDPGM 
04789      EXEC CICS SEND                                               GBIDPGM 
04790           MAP('GBIDI01')                                          GBIDPGM 
04791           MAPSET('GBIDSET')                                       GBIDPGM 
04792           ERASE                                                   GBIDPGM 
04793           FROM(GBIDI01O)                                          GBIDPGM 
04794           END-EXEC.                                               GBIDPGM 
04795                                                                   GBIDPGM 
04796      EXEC CICS RETURN TRANSID  ('GBID')                           GBIDPGM 
04797                       END-EXEC.                                   GBIDPGM 
04798                                                                   GBIDPGM 
04799  8700-EXIT.                                                       GBIDPGM 
04800      EXIT.                                                        GBIDPGM 
04801 /                                                                 GBIDPGM 
04802 ******************************************************************GBIDPGM 
04803 *                                                                 GBIDPGM 
04804 *  USED TO BLANK OUT TEMPORARY STORAGE LINES.                     GBIDPGM 
04805 *                                                                 GBIDPGM 
04806 ******************************************************************GBIDPGM 
04807  8800-INIT-TS-LINES.                                              GBIDPGM 
04808                                                                   GBIDPGM 
04809      MOVE SPACES TO WS-TS-VALUE (WS-TS-LINE-SUB).                 GBIDPGM 
04810                                                                   GBIDPGM 
04811  8800-EXIT.                                                       GBIDPGM 
04812      EXIT.                                                        GBIDPGM 
04813 /                                                                 GBIDPGM 
04814 ******************************************************************GBIDPGM 
04815 *                                                                 GBIDPGM 
04816 *  USED TO BLANK OUT DETAIL LINES ON MAP.                         GBIDPGM 
04817 *                                                                 GBIDPGM 
04818 ******************************************************************GBIDPGM 
04819  8900-INIT-MAP-LINES.                                             GBIDPGM 
04820                                                                   GBIDPGM 
04821      MOVE SPACES TO DETLNO (WS-TS-LINE-SUB),                      GBIDPGM 
04822                     ERRMSGO.                                      GBIDPGM 
04823                                                                   GBIDPGM 
04824  8900-EXIT.                                                       GBIDPGM 
04825      EXIT.                                                        GBIDPGM 
04826 /                                                                 GBIDPGM 
04827 ******************************************************************GBIDPGM 
04828 *                                                                 GBIDPGM 
04829 *  CONVERT JULIAN DATE INTO GREGORIAN FORMAT.                     GBIDPGM 
04830 *                                                                 GBIDPGM 
04831 ******************************************************************GBIDPGM 
04832  9200-JUL-TO-GREG-DATE.                                           GBIDPGM 
04833                                                                   GBIDPGM 
04834      MOVE '9200' TO WS-PARA-ID.                                   GBIDPGM 
04835                                                                   GBIDPGM 
04836      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIDPGM 
04837      MOVE 'J'   TO  HGADATE-FORM1.                                GBIDPGM 
04838      MOVE 'M'   TO  HGADATE-FORM2.                                GBIDPGM 
04839      MOVE ZEROS TO  HGADATE-RETURN                                GBIDPGM 
04840                     HGADATE-AMOUNT.                               GBIDPGM 
04841      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIDPGM 
04842                     COMMAREA(HGADATES-COMMAREA)                   GBIDPGM 
04843                     LENGTH  (24)                                  GBIDPGM 
04844                     END-EXEC.                                     GBIDPGM 
04845                                                                   GBIDPGM 
04846  9200-EXIT.                                                       GBIDPGM 
04847      EXIT.                                                        GBIDPGM 
04848 ******************************************************************GBIDPGM 
04849 *                                                                *GBIDPGM 
04850 * 9400    RECEIVE SCREEN                                         *GBIDPGM 
04851 *                                                                *GBIDPGM 
04852 ******************************************************************GBIDPGM 
04853  9400-000-RECEIVE-SCR  SECTION.                                   GBIDPGM 
04854                                                                   GBIDPGM 
04855 *        +----------------------------------------+               GBIDPGM 
04856 *        +  RECEIVE THE CURRENT SCREEN            +               GBIDPGM 
04857 *        +----------------------------------------+               GBIDPGM 
04858                                                                   GBIDPGM 
04859      EXEC CICS RECEIVE   MAP   ('GBIDI01')                        GBIDPGM 
04860                          MAPSET('GBIDSET')                        GBIDPGM 
04861                          INTO  (GBIDI01I)                         GBIDPGM 
04862                          END-EXEC.                                GBIDPGM 
04863                                                                   GBIDPGM 
04864                                                                   GBIDPGM 
04865  9400-900-EXIT.                                                   GBIDPGM 
04866           EXIT.                                                   GBIDPGM 
04867 ******************************************************************GBIDPGM 
04868 *                                                                *GBIDPGM 
04869 * 9900    ISSUE ERROR MESSAGE THEN ABEND                         *GBIDPGM 
04870 *                                                                *GBIDPGM 
04871 ******************************************************************GBIDPGM 
04872  9900-000-ERROR-MSG-THEN-ABEND SECTION.                           GBIDPGM 
04873  9900-010.                                                        GBIDPGM 
04874                                                                   GBIDPGM 
04875      EXEC CICS SEND MAP   ('GBIDI01')                             GBIDPGM 
04876                     MAPSET('GBIDSET')                             GBIDPGM 
04877                     FROM  (GBIDI01O)                              GBIDPGM 
04878                     ERASE                                         GBIDPGM 
04879                     WAIT                                          GBIDPGM 
04880                     END-EXEC.                                     GBIDPGM 
04881                                                                   GBIDPGM 
04882      EXEC CICS ABEND  ABCODE(WS-01-ABCODE) END-EXEC.              GBIDPGM 
04883                                                                   GBIDPGM 
04884  9900-000-EXIT.                                                   GBIDPGM 
04885      EXIT.                                                        GBIDPGM 
04886 /                                                                 GBIDPGM 
04887 ******************************************************************GBIDPGM 
04888 * THIS CATCHES THE SITUATION IN WHICH PROGRAM EXECUTION SEQUENCE *GBIDPGM 
04889 * 'FALLS THROUGH' THE BOTTOM OF THE PROGRAM.                     *GBIDPGM 
04890 ******************************************************************GBIDPGM 
04891  9999-FALL-THRU-TRAP.                                             GBIDPGM 
04892      MOVE 'L104' TO WS-ABEND-CODE.                                GBIDPGM 
04893                                                                   GBIDPGM 
04894 /                                                                 GBIDPGM 
04895 ******************************************************************GBIDPGM 
04896 * THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTS    *GBIDPGM 
04897 * THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE        *GBIDPGM 
04898 * DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.                 *GBIDPGM 
04899 ******************************************************************GBIDPGM 
04900  9999-ABEND.                                                      GBIDPGM 
04901                                                                   GBIDPGM 
04902      MOVE '9999' TO WS-PARA-ID.                                   GBIDPGM 
04903                                                                   GBIDPGM 
04904      EXEC CICS ABEND  ABCODE(WS-ABEND-CODE)                       GBIDPGM 
04905                       END-EXEC.                                   GBIDPGM 
04906                                                                   GBIDPGM 
04907  9999-EXIT.                                                       GBIDPGM 
04908      EXIT.                                                        GBIDPGM 
