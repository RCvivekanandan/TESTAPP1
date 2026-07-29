00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTROOMB.                                            ELTROOMB
00003  AUTHOR. JOHN CURIN - KEANE, INC.                                    LV001
00004  DATE-WRITTEN.   4/08/86.                                         ELTROOMB
00005  DATE-COMPILED.                                                   ELTROOMB
00006      SKIP3                                                        ELTROOMB
00007 ******************************************************************ELTROOMB
00008 *@>ELTROOMB                                                       ELTROOMB
00009 *@¬                                                               ELTROOMB
00010 *                        PROGRAM ABSTRACT                         ELTROOMB
00011 *                                                                 ELTROOMB
00012 *@¬ PROGRAM NAME:   E.L.S. ROOM AND BOARD TOPIC                   ELTROOMB
00013 *@¬                                                               ELTROOMB
00014 *@¬ PROGRAM I.D.:   ELTROOMB                                      ELTROOMB
00015 *@¬                                                               ELTROOMB
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE ROOM ANDELTROOMB
00017 *@¬            BOARD COVERAGE GIVEN A MEMBER.                     ELTROOMB
00018 *@¬                                                               ELTROOMB
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF ROOM AND BOARD COVELTROOMB
00020 *@¬            AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTROOMB
00021 *@¬            GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTROOMB
00022 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTROOMB
00023 *@¬            RANGE OF DATES.                                    ELTROOMB
00024 *@¬                                                               ELTROOMB
00025 *@¬ RECORDS                                                       ELTROOMB
00026 *@¬ ACCESSED:  GROUP SPECIFIC, VARIOUS BENEFIT PROVISION, AND A   ELTROOMB
00027 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTROOMB
00028 *@¬                                                               ELTROOMB
00029 *@¬ PROCESSING                                                    ELTROOMB
00030 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTROOMB
00031 *@¬                                                               ELTROOMB
00032 *@¬                                                               ELTROOMB
00033 ***************************************************************** ELTROOMB
00034 *                                                                 ELTROOMB
00035 *                                                                 ELTROOMB
00036 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELTROOMB
00037 *        *-*     U P D A T E   H I S T O R Y         *-*          ELTROOMB
00038 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELTROOMB
00039 *                                                                 ELTROOMB
00040 **-CHG NUM-* *-DATE-* *WHO* *-----DESCRIPTION-------------------  ELTROOMB
00041 *    XXXX    04/28/86  JTC   ORIGINAL IMPLEMENTATION              ELTROOMB
00042 *    0001    08/12/86  JTC   REVISE THE PER DIEM PROCESSING OF    ELTROOMB
00043 *                            PROVISION PRICING METHOD.  ALSO,     ELTROOMB
00044 *                            REMOVE THE SETUP OF HEADING LINE 1   ELTROOMB
00045 *                            WS-HDR-1.                            ELTROOMB
00046 *    XXXX    10/06/86  NAC   VS COBOL II CONVERSION.              ELTROOMB
00047 *    XXXX    11/25/86  LET   CHANGED CODE TO ACCOMMODATE THE      ELTROOMB
00048 *                            MOVING OF THE CERTIFICATION REQUIRE- ELTROOMB
00049 *                            MENT INDICATOR FROM THE TYPE FORMAT  ELTROOMB
00050 *                            SECTION TO THE COMMON SECTION        ELTROOMB
00051 *    0004    10/21/87  EGL   CHANGED FIXED TEXT.                  ELTROOMB
00052 *    0005    03/22/89  GEM   STORAGE MANAGEMENT ENHANCEMENTS      ELTROOMB
00053 *    0006    08/24/90  GEM   ADDED BENEFIT PROVISION IDS 'ABUI A'.ELTROOMB
00054 ***************************************************************** ELTROOMB
00055 /                                                                 ELTROOMB
00056  ENVIRONMENT DIVISION.                                            ELTROOMB
00057      SKIP3                                                        ELTROOMB
00058  DATA DIVISION.                                                   ELTROOMB
00059  WORKING-STORAGE SECTION.                                         ELTROOMB
00060  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTROOMB
00061      '***ELTROOMB WS BEGINS***'.                                  ELTROOMB
00062 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTROOMB
00063  01  WS-WORK-FIELDS.                                              ELTROOMB
00064      05  WS-HEX-00                     PIC X.                     ELTROOMB
00065      05  WS-CHAR-0                     PIC X.                     ELTROOMB
00066      05  WS-TEST-FOR-PER-DIEM          PIC XX.                    ELTROOMB
00067          88  FLAT-RATE              VALUE '04'.                   ELTROOMB
00068          88  FLAT-RATE-PLUS-PERCENT VALUES ARE '14', '21',        ELTROOMB
00069                                                '22'.              ELTROOMB
00070      05  WS-HOLD1                      PIC X(10).                 ELTROOMB
00071      05  WS-HOLD2                      PIC X(10).                 ELTROOMB
00072      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTROOMB
00073      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTROOMB
00074      05  WS-DTL-PP                     PIC X(63).                 ELTROOMB
00075      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTROOMB
00076      05  WS-DTL-ALLOW                  PIC X(63).                 ELTROOMB
00077      05  WS-DTL-ALLOW-AMT              PIC $$$9.99.               ELTROOMB
00078      05  WS-DTL-PER-D                  PIC X(63).                 ELTROOMB
00079      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTROOMB
00080      05  WS-DTL-CERT-REQ-ID            PIC X(79) VALUE SPACES.    ELTROOMB
00081      05  WS-DTL-CERT-REQ-IND           PIC X(79) VALUE SPACES.    ELTROOMB
00082      05  WS-FIRSTTIME-IND              PIC X.                     ELTROOMB
00083          88  WS-NOT-FIRST-TIME         VALUE 'N'.                 ELTROOMB
00084      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTROOMB
00085      05  WS-DISPLAY-CERTN-TEXT         PIC X.                     ELTROOMB
00086      05  WS-DISPLAY-DAY-PSYCH-TEXT     PIC X.                     ELTROOMB
00087      05  WS-DISPLAY-NIGHT-PSYCH-TEXT   PIC X.                     ELTROOMB
00088      05  WS-CIA                        PIC S999 COMP VALUE +0.    ELTROOMB
00089      05  WS-SUB                        PIC S999 COMP VALUE +0.    ELTROOMB
00090      05  WS-SUB2                       PIC S999 COMP VALUE +0.    ELTROOMB
00091      05  WS-SUB3                       PIC S999 COMP VALUE +0.    ELTROOMB
00092      05  WS-DESC-CTR                   PIC S999 COMP VALUE +0.    ELTROOMB
00093                                                                   ELTROOMB
00094 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTROOMB
00095  01  TABLE-MAX                   PIC S9(03) VALUE +12 COMP.       ELTROOMB
00096 * 11 REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTROOMB
00097                                                                   ELTROOMB
00098  01  WS-BEN-PROV-ID.                                              ELTROOMB
00099      05  WS-INST-IP-CNT                PIC S999 COMP    VALUE +12.ELTROOMB
00100      05  WS-INST-IP-TAB.                                          ELTROOMB
00101        10  FILLER                      PIC X(6)  VALUE 'ABUI A'.  ELTROOMB
00102        10  FILLER                      PIC X(6)  VALUE 'DRB  A'.  ELTROOMB
00103        10  FILLER                      PIC X(6)  VALUE 'NRB  A'.  ELTROOMB
00104        10  FILLER                      PIC X(6)  VALUE 'PVTR A'.  ELTROOMB
00105        10  FILLER                      PIC X(6)  VALUE 'PVTA A'.  ELTROOMB
00106        10  FILLER                      PIC X(6)  VALUE 'ICUN A'.  ELTROOMB
00107        10  FILLER                      PIC X(6)  VALUE 'ICUR A'.  ELTROOMB
00108        10  FILLER                      PIC X(6)  VALUE 'DTOX A'.  ELTROOMB
00109        10  FILLER                      PIC X(6)  VALUE 'DPSY A'.  ELTROOMB
00110        10  FILLER                      PIC X(6)  VALUE 'NPSY A'.  ELTROOMB
00111        10  FILLER                      PIC X(6)  VALUE 'PASS A'.  ELTROOMB
00112        10  FILLER                      PIC X(6)  VALUE 'REHB A'.  ELTROOMB
00113      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTROOMB
00114                                        PIC X(6)  OCCURS 12 TIMES. ELTROOMB
00115                                                                   ELTROOMB
00116 /                L I T E R A L S                                  ELTROOMB
00117  01  WS-PROGRAM-LITERALS.                                         ELTROOMB
00118    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTROOMB
00119    05  DAYS                        PIC X(04) VALUE 'DAYS'.        ELTROOMB
00120    05  WS-NO                       PIC X     VALUE 'N'.           ELTROOMB
00121    05  WS-YES                      PIC X     VALUE 'Y'.           ELTROOMB
00122    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTROOMB
00123    05  WS-IS                       PIC X(02) VALUE 'IS'.          ELTROOMB
00124    05  WS-DAY-PSYCH                PIC X(48)                      ELTROOMB
00125         VALUE 'DAY PSYCHIATRIC PRIOR ADMISSION REQUIREMENT IS: '. ELTROOMB
00126    05  WS-NIGHT-PSYCH              PIC X(50) VALUE                ELTROOMB
00127          'NIGHT PSYCHIATRIC PRIOR ADMISSION REQUIREMENT IS: '.    ELTROOMB
00128    05  WS-FLAT-RATE-IS             PIC X(17) VALUE                ELTROOMB
00129        'THE FLAT RATE IS '.                                       ELTROOMB
00130    05  WS-PERCENT-REMAINDER        PIC X(23) VALUE                ELTROOMB
00131        'THE % ON REMAINDER IS '.                                  ELTROOMB
00132    05  WS-BASIC-LIT                PIC X(05) VALUE 'BASIC'.       ELTROOMB
00133    05  WS-SECONDARY                PIC X(09) VALUE 'SECONDARY'.   ELTROOMB
00134    05  WS-DAYS-REDUCED             PIC X(22) VALUE                ELTROOMB
00135          ' DAYS REDUCTION RATIO '.                                ELTROOMB
00136    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTROOMB
00137          VALUE 'SPILLOVER COINSURANCE: '.                         ELTROOMB
00138    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTROOMB
00139          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTROOMB
00140    05  WS-SPILLOVER-FL-RT-PER-D    PIC X(29)                      ELTROOMB
00141          VALUE 'SPILLOVER FLAT RATE PER DIEM '.                   ELTROOMB
00142    05  WS-SERVICES-RENDERED.                                      ELTROOMB
00143      10  FILLER                    PIC X(26)                      ELTROOMB
00144          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTROOMB
00145                                                                   ELTROOMB
00146 /            D I S P L A Y   L I N E S                            ELTROOMB
00147  01  WS-ELS-DISPLAY-LINES.                                        ELTROOMB
00148    05  WS-HDR-2-INST-IP.                                          ELTROOMB
00149      10  FILLER                    PIC X(26) VALUE SPACES.        ELTROOMB
00150      10  FILLER                    PIC X(23)                      ELTROOMB
00151          VALUE 'ROOM AND BOARD SERVICES'.                         ELTROOMB
00152      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTROOMB
00153                                                                   ELTROOMB
00154    05  WS-ROOM-BOARD-ARE.                                         ELTROOMB
00155      10  FILLER                    PIC X(28)                      ELTROOMB
00156          VALUE 'ROOM AND BOARD SERVICES     '.                    ELTROOMB
00157      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTROOMB
00158                                                                   ELTROOMB
00159    05  WS-FOLLOW-BENEFIT.                                         ELTROOMB
00160      10  FILLER                  PIC  X(22) VALUE                 ELTROOMB
00161            'COVERED SERVICES ARE: '.                              ELTROOMB
00162                                                                   ELTROOMB
00163    05  WS-SERVICES-2ND.                                           ELTROOMB
00164      10  FILLER                    PIC X(21) VALUE SPACES.        ELTROOMB
00165      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTROOMB
00166      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTROOMB
00167                                                                   ELTROOMB
00168    05  WS-SERVICES-PAYABLE.                                       ELTROOMB
00169      10  FILLER                  PIC  X(40) VALUE                 ELTROOMB
00170          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTROOMB
00171                                                                   ELTROOMB
00172                                                                   ELTROOMB
00173    05  WS-CERTIFCATION-REQ.                                       ELTROOMB
00174      10  FILLER                    PIC X(26)                      ELTROOMB
00175          VALUE 'CERTIFICATION REQUIREMENT:'.                      ELTROOMB
00176      10  FILLER                    PIC X(53) VALUE LOW-VALUES.    ELTROOMB
00177                                                                   ELTROOMB
00178    05  WS-BASIC.                                                  ELTROOMB
00179      10  FILLER                    PIC X(09) VALUE SPACES.        ELTROOMB
00180      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTROOMB
00181      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTROOMB
00182                                                                   ELTROOMB
00183    05  WS-SUPPLEMENTAL.                                           ELTROOMB
00184      10  FILLER                    PIC X(16)                      ELTROOMB
00185          VALUE '  SUPPLEMENTAL: '.                                ELTROOMB
00186      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTROOMB
00187                                                                   ELTROOMB
00188                                                                   ELTROOMB
00189    05  WS-BASIC-PERCENT.                                          ELTROOMB
00190      10  FILLER                    PIC X(09) VALUE SPACES.        ELTROOMB
00191      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTROOMB
00192      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTROOMB
00193                                                                   ELTROOMB
00194    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTROOMB
00195      10  FILLER                    PIC X(16)                      ELTROOMB
00196          VALUE '  SUPPLEMENTAL: '.                                ELTROOMB
00197      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTROOMB
00198                                                                   ELTROOMB
00199                                                                   ELTROOMB
00200    05  WS-SUPP-PER-D.                                             ELTROOMB
00201      10  FILLER                    PIC X(16)                      ELTROOMB
00202          VALUE '  SUPPLEMENTAL: '.                                ELTROOMB
00203      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTROOMB
00204      10  FILLER                    PIC X     VALUE SPACE.         ELTROOMB
00205      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTROOMB
00206      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTROOMB
00207                                                                   ELTROOMB
00208    05  WS-CONTACT-CONTRACT.                                       ELTROOMB
00209      10  FILLER                    PIC X(51)                      ELTROOMB
00210       VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING.'.ELTROOMB
00211      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTROOMB
00212                                                                   ELTROOMB
00213    05  WS-CONTRACT-RELATED.                                       ELTROOMB
00214      10  FILLER                    PIC X(50)                      ELTROOMB
00215        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.ELTROOMB
00216      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTROOMB
00217                                                                   ELTROOMB
00218    05  WS-PVE-TEXT.                                               ELTROOMB
00219      10  FILLER                    PIC X(44)      VALUE           ELTROOMB
00220        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTROOMB
00221                                                                   ELTROOMB
00222    05  WS-ACCUM-MSG1.                                             ELTROOMB
00223      10  FILLER                  PIC  X(79) VALUE                 ELTROOMB
00224      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTROOMB
00225 -    'CONSIDERATIONS.'.                                           ELTROOMB
00226                                                                   ELTROOMB
00227    05  WS-ROOM-BOARD-NOT-PROF      PIC X(55)      VALUE           ELTROOMB
00228        'ROOM AND BOARD IS NOT COVERED AS A PROFESSIONAL CHARGE.'. ELTROOMB
00229                                                                   ELTROOMB
00230  01  WS-END                            PIC X(16)  VALUE           ELTROOMB
00231      '*** W/S ENDS ***'.                                          ELTROOMB
00232 /             L I N K A G E   S E C T I O N                       ELTROOMB
00233  LINKAGE SECTION.                                                 ELTROOMB
00234  01  DFHCOMMAREA.                                                 ELTROOMB
00235      COPY ELSCOMMC.                                               ELTROOMB
00236 /                                                                 ELTROOMB
00237      COPY ELSCIA2C.                                               ELTROOMB
00238 /                                                                 ELTROOMB
00239      COPY ELSIOPMC.                                               ELTROOMB
00240 /                                                                 ELTROOMB
00241      COPY ELSKEYSC.                                               ELTROOMB
00242 /                                                                 ELTROOMB
00243      COPY ELSOUTPC.                                               ELTROOMB
00244 /                                                                 ELTROOMB
00245      COPY ELSSSCBC.                                               ELTROOMB
00246 /                                                                 ELTROOMB
00247      COPY ELSCMIFC.                                               ELTROOMB
00248 /                                                                 ELTROOMB
00249      COPY ELSCMDSC.                                               ELTROOMB
00250 /                                                                 ELTROOMB
00251      COPY ELSPRVNC.                                               ELTROOMB
00252 /                                                                 ELTROOMB
00253 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTROOMB
00254      COPY ELSPLGSW.                                               ELTROOMB
00255 *** BENEFIT PROVISION TABLE OF FLDS                               ELTROOMB
00256      COPY ELSPLGTB.                                               ELTROOMB
00257 /                                                                 ELTROOMB
00258      COPY ELSTCWAC.                                               ELTROOMB
00259  01  GROUP-SPECIFIC-RECORD.                                       ELTROOMB
00260      COPY GCGROUPC.                                               ELTROOMB
00261 /                  M A I N L I N E                                ELTROOMB
00262  PROCEDURE DIVISION.                                              ELTROOMB
00263                                                                   ELTROOMB
00264 ******************************************************************ELTROOMB
00265 *                                                                 ELTROOMB
00266 *   PERFORM THE MAINLINE OPERATIONS.                              ELTROOMB
00267 *                                                                 ELTROOMB
00268 ******************************************************************ELTROOMB
00269  0000-MAINLINE.                                                   ELTROOMB
00270                                                                   ELTROOMB
00271 ****************************************************************  ELTROOMB
00272 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTROOMB
00273 ****************************************************************  ELTROOMB
00274                                                                   ELTROOMB
00275      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTROOMB
00276          EXEC CICS ABEND                                          ELTROOMB
00277                    ABCODE ('EL01')                                ELTROOMB
00278          END-EXEC                                                 ELTROOMB
00279      END-IF.                                                      ELTROOMB
00280                                                                   ELTROOMB
00281 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTROOMB
00282                                                                   ELTROOMB
00283      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTROOMB
00284          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTROOMB
00285                                                                   ELTROOMB
00286      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTROOMB
00287      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00288          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTROOMB
00289                                                                   ELTROOMB
00290      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTROOMB
00291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00292          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTROOMB
00293                                                                   ELTROOMB
00294      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTROOMB
00295      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00296          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTROOMB
00297                                                                   ELTROOMB
00298      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTROOMB
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00300          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTROOMB
00301                                                                   ELTROOMB
00302      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTROOMB
00303      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00304          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTROOMB
00305                                                                   ELTROOMB
00306      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTROOMB
00307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00308          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTROOMB
00309                                                                   ELTROOMB
00310      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTROOMB
00311      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00312          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTROOMB
00313                                                                   ELTROOMB
00314      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTROOMB
00315                                                                   ELTROOMB
00316      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTROOMB
00317              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTROOMB
00318                                                                   ELTROOMB
00319      SET CIA-STG-GETMAIN  TO TRUE.                                ELTROOMB
00320      EXEC CICS LINK                                               ELTROOMB
00321                PROGRAM('ELUSTGMG')                                ELTROOMB
00322                COMMAREA(DFHCOMMAREA)                              ELTROOMB
00323      END-EXEC.                                                    ELTROOMB
00324                                                                   ELTROOMB
00325      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTROOMB
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00327          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTROOMB
00328                                                                   ELTROOMB
00329      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTROOMB
00330                                                                   ELTROOMB
00331      IF (SSB-PROV-CLASS-INST  OR  SSB-PROV-CLASS-BOTH)            ELTROOMB
00332         PERFORM 1000-INSTITUTIONAL-IP-RTNE THRU 1000-EXIT.        ELTROOMB
00333                                                                   ELTROOMB
00334      IF SSB-PROV-CLASS-PROF                                       ELTROOMB
00335         PERFORM 2000-PROFESSIONAL-IP-RTNE THRU 2000-EXIT.         ELTROOMB
00336                                                                   ELTROOMB
00337      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTROOMB
00338                                                                   ELTROOMB
00339      SET CIA-STG-FREEMAIN TO TRUE.                                ELTROOMB
00340      EXEC CICS LINK                                               ELTROOMB
00341                PROGRAM('ELUSTGMG')                                ELTROOMB
00342                COMMAREA(DFHCOMMAREA)                              ELTROOMB
00343      END-EXEC.                                                    ELTROOMB
00344                                                                   ELTROOMB
00345 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTROOMB
00346       MOVE +0  TO  COF-NBR-HDR-LINES.                             ELTROOMB
00347       MOVE +0  TO  COF-NBR-DTL-LINES.                             ELTROOMB
00348       MOVE 'E' TO  COF-FUNCTION.                                  ELTROOMB
00349                                                                   ELTROOMB
00350      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTROOMB
00351                                         END-EXEC.                 ELTROOMB
00352                                                                   ELTROOMB
00353  0099-RETURN.                                                     ELTROOMB
00354      EXEC CICS RETURN   END-EXEC.                                 ELTROOMB
00355                                                                   ELTROOMB
00356      GOBACK.                                                      ELTROOMB
00357                                                                   ELTROOMB
00358 /        I N S T I T U T I O N A L  I P   R T N E                 ELTROOMB
00359 ***************************************************************** ELTROOMB
00360 *        I N S T I T U T I O N A L  I P   R T N E                 ELTROOMB
00361 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTROOMB
00362 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTROOMB
00363 ***************************************************************** ELTROOMB
00364  1000-INSTITUTIONAL-IP-RTNE.                                      ELTROOMB
00365                                                                   ELTROOMB
00366      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTROOMB
00367                                                                   ELTROOMB
00368      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTROOMB
00369      PERFORM WITH TEST BEFORE                                     ELTROOMB
00370              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTROOMB
00371              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTROOMB
00372         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTROOMB
00373         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTROOMB
00374         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTROOMB
00375         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTROOMB
00376      END-PERFORM.                                                 ELTROOMB
00377      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTROOMB
00378                                                                   ELTROOMB
00379      PERFORM WITH TEST BEFORE                                     ELTROOMB
00380         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTROOMB
00381         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTROOMB
00382            SET  PVN-BEN-PROVN-IDX TO WS-SUB                       ELTROOMB
00383            MOVE WS-INST-IP-LIST(WS-SUB)  TO                       ELTROOMB
00384                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX)    ELTROOMB
00385            MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      ELTROOMB
00386                            PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)    ELTROOMB
00387      END-PERFORM.                                                 ELTROOMB
00388                                                                   ELTROOMB
00389                                                                   ELTROOMB
00390      MOVE WS-HDR-2-INST-IP    TO  COF-HDR-LINE(2).                ELTROOMB
00391 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTROOMB
00392      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTROOMB
00393      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTROOMB
00394      MOVE 'P'  TO  COF-FUNCTION.                                  ELTROOMB
00395                                                                   ELTROOMB
00396      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTROOMB
00397              END-EXEC.                                            ELTROOMB
00398 ****************************************************              ELTROOMB
00399                                                                   ELTROOMB
00400      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTROOMB
00401      MOVE WS-ROOM-BOARD-ARE   TO  SSB-TOPIC-PHRASE.               ELTROOMB
00402                                                                   ELTROOMB
00403      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTROOMB
00404          END-EXEC.                                                ELTROOMB
00405                                                                   ELTROOMB
00406 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTROOMB
00407      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTROOMB
00408      MOVE ' '  TO  COF-FUNCTION.                                  ELTROOMB
00409      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTROOMB
00410              END-EXEC.                                            ELTROOMB
00411 *4/15 END OF TEMPORARY CODE                                       ELTROOMB
00412                                                                   ELTROOMB
00413      IF PVN-COVG-NONE                                             ELTROOMB
00414         GO TO 1000-EXIT.                                          ELTROOMB
00415                                                                   ELTROOMB
00416      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTROOMB
00417            PSP-PROVN-PRICING-METHD,                               ELTROOMB
00418            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTROOMB
00419            PSP-TRANSF-OTHER-RESP-IND,                             ELTROOMB
00420            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTROOMB
00421            PSP-SPILL-OVER-COINS-APL-IND,                          ELTROOMB
00422            PSP-SPILL-OVER-DED-APL-IND,                            ELTROOMB
00423            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTROOMB
00424            PSP-CERTFN-REQRM-IND,                                  ELTROOMB
00425            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTROOMB
00426            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTROOMB
00427            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTROOMB
00428            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTROOMB
00429            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTROOMB
00430            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTROOMB
00431            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTROOMB
00432            PSA-DAYS-RDCN-RAT-IND,                                 ELTROOMB
00433            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTROOMB
00434            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTROOMB
00435            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTROOMB
00436            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTROOMB
00437            PSA-FLAT-RATE-PDM-AMT.                                 ELTROOMB
00438                                                                   ELTROOMB
00439                                                                   ELTROOMB
00440      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTROOMB
00441                     COMMAREA (DFHCOMMAREA)                        ELTROOMB
00442      END-EXEC.                                                    ELTROOMB
00443                                                                   ELTROOMB
00444      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTROOMB
00445      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00446          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTROOMB
00447                                                                   ELTROOMB
00448      PERFORM WITH TEST BEFORE                                     ELTROOMB
00449         VARYING WS-SUB  FROM  +1  BY  +1                          ELTROOMB
00450         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTROOMB
00451             SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB          ELTROOMB
00452             IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) NOT = ZERO     ELTROOMB
00453                  PERFORM 1040-BUILD-SCREEN-LINES THRU 1040-EXIT   ELTROOMB
00454             END-IF                                                ELTROOMB
00455      END-PERFORM.                                                 ELTROOMB
00456                                                                   ELTROOMB
00457  1000-EXIT. EXIT.                                                 ELTROOMB
00458 /                                                                 ELTROOMB
00459                                                                   ELTROOMB
00460  1040-BUILD-SCREEN-LINES.                                         ELTROOMB
00461                                                                   ELTROOMB
00462      SET PLT-INDEX1 TO                                            ELTROOMB
00463         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTROOMB
00464                                                                   ELTROOMB
00465      IF WS-NOT-FIRST-TIME                                         ELTROOMB
00466         MOVE 'P' TO COF-FUNCTION                                  ELTROOMB
00467         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTROOMB
00468         EXEC CICS LINK  PROGRAM('ELUOUTPT')                       ELTROOMB
00469                         COMMAREA(DFHCOMMAREA)                     ELTROOMB
00470                          END-EXEC                                 ELTROOMB
00471      ELSE                                                         ELTROOMB
00472         MOVE 'N' TO WS-FIRSTTIME-IND.                             ELTROOMB
00473                                                                   ELTROOMB
00474      MOVE +1  TO  WS-CIA.                                         ELTROOMB
00475      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTROOMB
00476         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTROOMB
00477            SET PLT-INDEX2  TO  2                                  ELTROOMB
00478         ELSE                                                      ELTROOMB
00479            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTROOMB
00480            GO TO 1040-EXIT                                        ELTROOMB
00481      ELSE                                                         ELTROOMB
00482         SET PLT-INDEX2  TO  1.                                    ELTROOMB
00483      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTROOMB
00484                                                                   ELTROOMB
00485      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTROOMB
00486      ADD +1                 TO WS-CIA.                            ELTROOMB
00487      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTROOMB
00488      ADD +1                 TO WS-CIA.                            ELTROOMB
00489      MOVE WS-NO             TO WS-DISPLAY-DAY-PSYCH-TEXT,         ELTROOMB
00490                                WS-DISPLAY-NIGHT-PSYCH-TEXT.       ELTROOMB
00491      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTROOMB
00492         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTROOMB
00493         UNTIL WS-SUB2  >  WS-INST-IP-CNT.                         ELTROOMB
00494                                                                   ELTROOMB
00495      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTROOMB
00496               NOT = '0' OR LOW-VALUES                             ELTROOMB
00497           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTROOMB
00498                                                                   ELTROOMB
00499      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTROOMB
00500      MOVE LOW-VALUES           TO  COF-DTL-LINE(WS-CIA).          ELTROOMB
00501      ADD  +1                   TO  WS-CIA.                        ELTROOMB
00502      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTROOMB
00503      ADD  +1                   TO  WS-CIA.                        ELTROOMB
00504                                                                   ELTROOMB
00505      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00506         SET  PLT-INDEX2           TO  1                           ELTROOMB
00507         PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT.             ELTROOMB
00508                                                                   ELTROOMB
00509      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTROOMB
00510         SET  PLT-INDEX2             TO  2                         ELTROOMB
00511         PERFORM 4200-PAYABLE-AS-SUPP THRU 4200-EXIT.              ELTROOMB
00512                                                                   ELTROOMB
00513      MOVE 'Y' TO WS-DISPLAY-CERTN-TEXT.                           ELTROOMB
00514                                                                   ELTROOMB
00515      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00516        SET  PLT-INDEX2          TO  1                             ELTROOMB
00517        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00518                NOT = ZEROES         AND NOT = LOW-VALUES          ELTROOMB
00519                        PERFORM 3500-CERTIFICATION-REQ.            ELTROOMB
00520                                                                   ELTROOMB
00521      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTROOMB
00522        SET  PLT-INDEX2          TO  2                             ELTROOMB
00523        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00524                NOT = ZEROES    AND NOT = LOW-VALUES               ELTROOMB
00525                        PERFORM 3500-CERTIFICATION-REQ.            ELTROOMB
00526                                                                   ELTROOMB
00527      IF WS-DISPLAY-DAY-PSYCH-TEXT = WS-YES                        ELTROOMB
00528       IF GCG-DAY-PSYCH-PRIOR-ADM-CD NOT = '0'                     ELTROOMB
00529         ADD +1                         TO  WS-CIA                 ELTROOMB
00530         MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX      ELTROOMB
00531         MOVE 'DAY-PSYCH-PRIOR-ADM-CD'  TO  CMF-ELEMENT-SYSTEM-NAMEELTROOMB
00532         MOVE GCG-DAY-PSYCH-PRIOR-ADM-CD                           ELTROOMB
00533                               TO  CMF-CODE-VALUE                  ELTROOMB
00534         EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA) ELTROOMB
00535                END-EXEC                                           ELTROOMB
00536         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00537         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00538             ADDRESS OF CMF-DESCR                                  ELTROOMB
00539         MOVE SPACES          TO TCAR-FROM-AREA                    ELTROOMB
00540         STRING WS-DAY-PSYCH                                       ELTROOMB
00541                CMF-DESCR-LINE(1) ' '                              ELTROOMB
00542                CMF-DESCR-LINE(2) ' '                              ELTROOMB
00543                  DELIMITED BY SIZE INTO TCAR-FROM-AREA            ELTROOMB
00544         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTROOMB
00545         MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT           ELTROOMB
00546         MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN           ELTROOMB
00547         MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN           ELTROOMB
00548         PERFORM TCPR-000-TEXT-UNSTRING                            ELTROOMB
00549         MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)             ELTROOMB
00550         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTROOMB
00551            ADD +1                TO WS-CIA                        ELTROOMB
00552            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTROOMB
00553            PERFORM 3000-OUTPUT-TEXT                               ELTROOMB
00554         ELSE                                                      ELTROOMB
00555          PERFORM 3000-OUTPUT-TEXT.                                ELTROOMB
00556                                                                   ELTROOMB
00557      IF WS-DISPLAY-NIGHT-PSYCH-TEXT = WS-YES                      ELTROOMB
00558       IF GCG-NIGHT-PSYCH-PRIOR-ADM-CD NOT = '0'                   ELTROOMB
00559        ADD +1                         TO  WS-CIA                  ELTROOMB
00560        MOVE 'GROUP'                    TO  CMF-RECORD-PREFIX      ELTROOMB
00561        MOVE 'NIGHT-PSYCH-PRIOR-ADM-CD' TO  CMF-ELEMENT-SYSTEM-NAMEELTROOMB
00562        MOVE GCG-NIGHT-PSYCH-PRIOR-ADM-CD                          ELTROOMB
00563                             TO CMF-CODE-VALUE                     ELTROOMB
00564        EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)  ELTROOMB
00565               END-EXEC                                            ELTROOMB
00566         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00567         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00568             ADDRESS OF CMF-DESCR                                  ELTROOMB
00569        MOVE SPACES          TO TCAR-FROM-AREA                     ELTROOMB
00570        STRING WS-NIGHT-PSYCH                                      ELTROOMB
00571               CMF-DESCR-LINE(1) ' '                               ELTROOMB
00572               CMF-DESCR-LINE(2) ' '                               ELTROOMB
00573                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTROOMB
00574        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTROOMB
00575        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTROOMB
00576        MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN            ELTROOMB
00577        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTROOMB
00578        PERFORM TCPR-000-TEXT-UNSTRING                             ELTROOMB
00579        MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)              ELTROOMB
00580        IF TCAR-OUTPUT-FIELDS-USED > 1                             ELTROOMB
00581           ADD +1                TO WS-CIA                         ELTROOMB
00582           MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)           ELTROOMB
00583           PERFORM 3000-OUTPUT-TEXT                                ELTROOMB
00584        ELSE                                                       ELTROOMB
00585          PERFORM 3000-OUTPUT-TEXT.                                ELTROOMB
00586                                                                   ELTROOMB
00587      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00588        SET  PLT-INDEX2          TO  1                             ELTROOMB
00589        IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00590                     NOT = '0'                                     ELTROOMB
00591         IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTROOMB
00592                     NOT = LOW-VALUES                              ELTROOMB
00593          ADD +1                   TO WS-CIA                       ELTROOMB
00594          MOVE 'BPA'               TO  CMF-RECORD-PREFIX           ELTROOMB
00595          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTROOMB
00596          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTROOMB
00597                                   TO  CMF-CODE-VALUE              ELTROOMB
00598          EXEC CICS  LINK  PROGRAM('ELUCMIF')                      ELTROOMB
00599                 COMMAREA(DFHCOMMAREA)                             ELTROOMB
00600                 END-EXEC                                          ELTROOMB
00601         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00602         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00603             ADDRESS OF CMF-DESCR                                  ELTROOMB
00604          MOVE SPACES          TO TCAR-FROM-AREA                   ELTROOMB
00605          STRING CMF-DESCR-LINE(1) ' '                             ELTROOMB
00606                 CMF-DESCR-LINE(2) ' '                             ELTROOMB
00607                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTROOMB
00608          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTROOMB
00609          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTROOMB
00610          MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN          ELTROOMB
00611          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTROOMB
00612          PERFORM TCPR-000-TEXT-UNSTRING                           ELTROOMB
00613          MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)            ELTROOMB
00614          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTROOMB
00615             ADD +1                TO WS-CIA                       ELTROOMB
00616             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00617             PERFORM 3000-OUTPUT-TEXT                              ELTROOMB
00618          ELSE                                                     ELTROOMB
00619           PERFORM 3000-OUTPUT-TEXT.                               ELTROOMB
00620                                                                   ELTROOMB
00621      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00622        SET  PLT-INDEX2          TO  1                             ELTROOMB
00623        IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00624                         = '0' OR LOW-VALUES                       ELTROOMB
00625               ADD +1 TO  WS-CIA.                                  ELTROOMB
00626                                                                   ELTROOMB
00627      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00628        SET  PLT-INDEX2          TO  1                             ELTROOMB
00629        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTROOMB
00630                     NOT = ZEROS  AND                              ELTROOMB
00631            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTROOMB
00632                      NOT = ZEROS                                  ELTROOMB
00633             MOVE                                                  ELTROOMB
00634              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTROOMB
00635                    TO WS-DTL-DAYS-REDUCED-APL                     ELTROOMB
00636             MOVE                                                  ELTROOMB
00637              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTROOMB
00638                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTROOMB
00639             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
00640             STRING WS-DAYS-REDUCED,                               ELTROOMB
00641                    WS-BASIC-LIT,                                  ELTROOMB
00642                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTROOMB
00643                    WS-FOR, ' '                                    ELTROOMB
00644                    WS-DTL-DAYS-REDUCED-BASE,                      ELTROOMB
00645                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
00646             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
00647             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
00648             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
00649             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
00650             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
00651             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00652             PERFORM 3000-OUTPUT-TEXT.                             ELTROOMB
00653                                                                   ELTROOMB
00654      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00655        SET  PLT-INDEX2          TO  1                             ELTROOMB
00656        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTROOMB
00657                         = ZEROS  AND                              ELTROOMB
00658            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTROOMB
00659                          = ZEROS                                  ELTROOMB
00660                              ADD +1  TO  WS-CIA.                  ELTROOMB
00661                                                                   ELTROOMB
00662      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00663        SET  PLT-INDEX2          TO  1                             ELTROOMB
00664        IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTROOMB
00665                     NOT = ZEROS  AND                              ELTROOMB
00666            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTROOMB
00667                      NOT = ZEROS                                  ELTROOMB
00668             MOVE                                                  ELTROOMB
00669              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTROOMB
00670                    TO WS-DTL-DAYS-REDUCED-APL                     ELTROOMB
00671             MOVE                                                  ELTROOMB
00672              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTROOMB
00673                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTROOMB
00674             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
00675             STRING WS-DAYS-REDUCED,                               ELTROOMB
00676                    WS-SECONDARY,                                  ELTROOMB
00677                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTROOMB
00678                    WS-FOR, ' '                                    ELTROOMB
00679                    WS-DTL-DAYS-REDUCED-BASE,                      ELTROOMB
00680                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
00681             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
00682             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
00683             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
00684             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
00685             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
00686             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00687             PERFORM 3000-OUTPUT-TEXT.                             ELTROOMB
00688                                                                   ELTROOMB
00689      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTROOMB
00690         SET  PLT-INDEX2          TO  2                            ELTROOMB
00691         PERFORM 4300-SPILLOVER-COINS THRU 4300-EXIT               ELTROOMB
00692         PERFORM 4400-SPILLOVER-DEDUCT THRU 4400-EXIT              ELTROOMB
00693         IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTROOMB
00694                     NOT = '0'                                     ELTROOMB
00695          IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTROOMB
00696                       NOT = LOW-VALUE                             ELTROOMB
00697           ADD +1     TO WS-CIA                                    ELTROOMB
00698           MOVE 'BP'  TO  CMF-RECORD-PREFIX                        ELTROOMB
00699           MOVE 'SPILL-OVR-RM-F-RT-APL-IND'                        ELTROOMB
00700                       TO CMF-ELEMENT-SYSTEM-NAME                  ELTROOMB
00701           MOVE                                                    ELTROOMB
00702            PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTROOMB
00703                       TO CMF-CODE-VALUE                           ELTROOMB
00704           EXEC CICS  LINK  PROGRAM('ELUCMIF')                     ELTROOMB
00705                            COMMAREA(DFHCOMMAREA)                  ELTROOMB
00706                END-EXEC                                           ELTROOMB
00707         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00708         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00709             ADDRESS OF CMF-DESCR                                  ELTROOMB
00710           MOVE SPACES          TO TCAR-FROM-AREA                  ELTROOMB
00711           STRING WS-SPILLOVER-FL-RT-PER-D                         ELTROOMB
00712                  CMF-DESCR-LINE(1) ' '                            ELTROOMB
00713                  CMF-DESCR-LINE(2) ' '                            ELTROOMB
00714                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTROOMB
00715           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTROOMB
00716           MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT         ELTROOMB
00717           MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN         ELTROOMB
00718           MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN         ELTROOMB
00719           PERFORM TCPR-000-TEXT-UNSTRING                          ELTROOMB
00720           MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)           ELTROOMB
00721           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTROOMB
00722             ADD +1                TO WS-CIA                       ELTROOMB
00723             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00724             PERFORM 3000-OUTPUT-TEXT                              ELTROOMB
00725           ELSE                                                    ELTROOMB
00726             PERFORM 3000-OUTPUT-TEXT.                             ELTROOMB
00727                                                                   ELTROOMB
00728      PERFORM 4600-SCAN-TAB.                                       ELTROOMB
00729                                                                   ELTROOMB
00730  1040-EXIT.  EXIT.                                                ELTROOMB
00731 /                                                                 ELTROOMB
00732  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTROOMB
00733      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTROOMB
00734                                                                   ELTROOMB
00735      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTROOMB
00736         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'DPSY '                ELTROOMB
00737               MOVE WS-YES TO WS-DISPLAY-DAY-PSYCH-TEXT.           ELTROOMB
00738                                                                   ELTROOMB
00739      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTROOMB
00740         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'NPSY '                ELTROOMB
00741               MOVE WS-YES TO WS-DISPLAY-NIGHT-PSYCH-TEXT.         ELTROOMB
00742                                                                   ELTROOMB
00743      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTROOMB
00744         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTROOMB
00745         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTROOMB
00746         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTROOMB
00747                                                   CMF-CODE-VALUE  ELTROOMB
00748         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTROOMB
00749                COMMAREA(DFHCOMMAREA)                              ELTROOMB
00750                END-EXEC                                           ELTROOMB
00751         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00752         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00753             ADDRESS OF CMF-DESCR                                  ELTROOMB
00754         MOVE SPACES          TO TCAR-FROM-AREA                    ELTROOMB
00755         STRING CMF-DESCR-LINE(1) ' '                              ELTROOMB
00756                CMF-DESCR-LINE(2) ' '                              ELTROOMB
00757                  DELIMITED BY SIZE INTO TCAR-FROM-AREA            ELTROOMB
00758         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTROOMB
00759         MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT           ELTROOMB
00760         MOVE +55             TO TCAR-OUTPUT-FIELD-1-LEN           ELTROOMB
00761         MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN           ELTROOMB
00762         PERFORM TCPR-000-TEXT-UNSTRING                            ELTROOMB
00763         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTROOMB
00764         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTROOMB
00765         IF WS-CIA  <  20                                          ELTROOMB
00766            ADD +1  TO  WS-CIA                                     ELTROOMB
00767            MOVE ZERO  TO                                          ELTROOMB
00768                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTROOMB
00769         ELSE                                                      ELTROOMB
00770            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTROOMB
00771                COMMAREA(DFHCOMMAREA)                              ELTROOMB
00772                END-EXEC                                           ELTROOMB
00773            MOVE +1  TO  WS-CIA                                    ELTROOMB
00774            MOVE ZERO  TO                                          ELTROOMB
00775                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTROOMB
00776                                                                   ELTROOMB
00777                                                                   ELTROOMB
00778      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTROOMB
00779        SET  PLT-INDEX2          TO  1                             ELTROOMB
00780        IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTROOMB
00781                NOT = ZEROES         AND NOT = LOW-VALUES          ELTROOMB
00782                        PERFORM 3600-TRANS-OTHER-RESP-IND.         ELTROOMB
00783                                                                   ELTROOMB
00784      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTROOMB
00785        SET  PLT-INDEX2          TO  2                             ELTROOMB
00786        IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTROOMB
00787                NOT = ZEROES    AND NOT = LOW-VALUES               ELTROOMB
00788                        PERFORM 3600-TRANS-OTHER-RESP-IND.         ELTROOMB
00789                                                                   ELTROOMB
00790  1090-PROBLEM-WITH-INDICES.                                       ELTROOMB
00791                                                                   ELTROOMB
00792      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTROOMB
00793      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTROOMB
00794                                                                   ELTROOMB
00795      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTROOMB
00796      MOVE 'P'  TO  COF-FUNCTION.                                  ELTROOMB
00797                                                                   ELTROOMB
00798      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTROOMB
00799             END-EXEC.                                             ELTROOMB
00800                                                                   ELTROOMB
00801  1099-EXIT.            EXIT.                                      ELTROOMB
00802                                                                   ELTROOMB
00803 /        P R O F E S S I O N A L   I P   R T N E                  ELTROOMB
00804 ***************************************************************** ELTROOMB
00805 *        P R O F E S S I O N A L   I P   R T N E                  ELTROOMB
00806 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTROOMB
00807 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTROOMB
00808 ***************************************************************** ELTROOMB
00809  2000-PROFESSIONAL-IP-RTNE.                                       ELTROOMB
00810                                                                   ELTROOMB
00811      MOVE 'P'  TO  COF-FUNCTION.                                  ELTROOMB
00812      MOVE WS-HDR-2-INST-IP    TO  COF-HDR-LINE(2).                ELTROOMB
00813      MOVE LOW-VALUES             TO COF-DTL-LINE(1).              ELTROOMB
00814      MOVE WS-ROOM-BOARD-NOT-PROF TO COF-DTL-LINE(2)               ELTROOMB
00815      MOVE +2   TO COF-NBR-DTL-LINES.                              ELTROOMB
00816 **********CALL OUTPUT FOR NEW PAGE WITH TEXT*********             ELTROOMB
00817      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTROOMB
00818      MOVE 'P'  TO  COF-FUNCTION.                                  ELTROOMB
00819                                                                   ELTROOMB
00820      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTROOMB
00821              END-EXEC.                                            ELTROOMB
00822 ****************************************************              ELTROOMB
00823  2000-EXIT.  EXIT.                                                ELTROOMB
00824                                                                   ELTROOMB
00825 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTROOMB
00826  3000-OUTPUT-TEXT.                                                ELTROOMB
00827      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTROOMB
00828      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTROOMB
00829      MOVE ' '  TO  COF-FUNCTION.                                  ELTROOMB
00830                                                                   ELTROOMB
00831      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTROOMB
00832                       COMMAREA(DFHCOMMAREA)                       ELTROOMB
00833                        END-EXEC.                                  ELTROOMB
00834      MOVE +1   TO WS-CIA.                                         ELTROOMB
00835  3000-EXIT.  EXIT.                                                ELTROOMB
00836 /                                                                 ELTROOMB
00837  3500-CERTIFICATION-REQ.                                          ELTROOMB
00838      IF WS-DISPLAY-CERTN-TEXT = 'Y'                               ELTROOMB
00839          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00840          ADD +1                   TO WS-CIA                       ELTROOMB
00841          MOVE WS-CERTIFCATION-REQ TO COF-DTL-LINE(WS-CIA)         ELTROOMB
00842          ADD +1                   TO WS-CIA                       ELTROOMB
00843          MOVE 'N' TO WS-DISPLAY-CERTN-TEXT.                       ELTROOMB
00844      MOVE 'BP'  TO  CMF-RECORD-PREFIX                             ELTROOMB
00845      MOVE 'CERTFN-REQRM-IND'   TO  CMF-ELEMENT-SYSTEM-NAME        ELTROOMB
00846      MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00847                   TO CMF-CODE-VALUE.                              ELTROOMB
00848      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTROOMB
00849                           COMMAREA(DFHCOMMAREA)                   ELTROOMB
00850                           END-EXEC                                ELTROOMB
00851         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTROOMB
00852         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00853             ADDRESS OF CMF-DESCR                                  ELTROOMB
00854      STRING WS-DTL-CERT-REQ-ID ' '                                ELTROOMB
00855             CMF-DESCR-LINE(1) ' '                                 ELTROOMB
00856             CMF-DESCR-LINE(2)                                     ELTROOMB
00857               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTROOMB
00858      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTROOMB
00859      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT.             ELTROOMB
00860      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTROOMB
00861      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTROOMB
00862      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTROOMB
00863      IF PLT-INDEX2 = 1                                            ELTROOMB
00864          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTROOMB
00865          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTROOMB
00866      ELSE                                                         ELTROOMB
00867       MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL                ELTROOMB
00868       MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA).              ELTROOMB
00869      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
00870            ADD +1                TO WS-CIA                        ELTROOMB
00871            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTROOMB
00872            PERFORM 3000-OUTPUT-TEXT                               ELTROOMB
00873      ELSE                                                         ELTROOMB
00874         PERFORM 3000-OUTPUT-TEXT.                                 ELTROOMB
00875  3500-EXIT.    EXIT.                                              ELTROOMB
00876 /                                                                 ELTROOMB
00877  3600-TRANS-OTHER-RESP-IND.                                       ELTROOMB
00878                                                                   ELTROOMB
00879      ADD +1     TO  WS-CIA.                                       ELTROOMB
00880      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTROOMB
00881      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTROOMB
00882      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTROOMB
00883           TO  CMF-CODE-VALUE.                                     ELTROOMB
00884      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTROOMB
00885                       COMMAREA(DFHCOMMAREA)                       ELTROOMB
00886      END-EXEC.                                                    ELTROOMB
00887      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTROOMB
00888      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00889             ADDRESS OF CMF-DESCR.                                 ELTROOMB
00890      MOVE SPACES          TO TCAR-FROM-AREA                       ELTROOMB
00891      STRING WS-SERVICES-RENDERED ' '                              ELTROOMB
00892             CMF-DESCR-LINE(1) ' '                                 ELTROOMB
00893             CMF-DESCR-LINE(2) ' '                                 ELTROOMB
00894               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTROOMB
00895      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTROOMB
00896      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTROOMB
00897      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTROOMB
00898      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTROOMB
00899      PERFORM TCPR-000-TEXT-UNSTRING                               ELTROOMB
00900      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTROOMB
00901      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
00902         ADD +1                TO WS-CIA                           ELTROOMB
00903         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTROOMB
00904         PERFORM 3000-OUTPUT-TEXT                                  ELTROOMB
00905      ELSE                                                         ELTROOMB
00906       PERFORM 3000-OUTPUT-TEXT.                                   ELTROOMB
00907  3600-EXIT.  EXIT.                                                ELTROOMB
00908 /                                                                 ELTROOMB
00909  4000-PLACE-OF-TREATMENT.                                         ELTROOMB
00910      ADD +1     TO  WS-CIA.                                       ELTROOMB
00911      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTROOMB
00912      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTROOMB
00913      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTROOMB
00914                                               TO  CMF-CODE-VALUE. ELTROOMB
00915      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTROOMB
00916              END-EXEC.                                            ELTROOMB
00917         SET CIA-ELSCMDSC-DDN TO TRUE.                             ELTROOMB
00918         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTROOMB
00919             ADDRESS OF CMF-DESCR.                                 ELTROOMB
00920      MOVE SPACES          TO TCAR-FROM-AREA                       ELTROOMB
00921      STRING WS-SERVICES-RENDERED ' '                              ELTROOMB
00922             CMF-DESCR-LINE(1) ' '                                 ELTROOMB
00923             CMF-DESCR-LINE(2) ' '                                 ELTROOMB
00924               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTROOMB
00925      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTROOMB
00926      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTROOMB
00927      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTROOMB
00928      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTROOMB
00929      PERFORM TCPR-000-TEXT-UNSTRING                               ELTROOMB
00930      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTROOMB
00931      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
00932         ADD +1                TO WS-CIA                           ELTROOMB
00933         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTROOMB
00934         PERFORM 3000-OUTPUT-TEXT                                  ELTROOMB
00935      ELSE                                                         ELTROOMB
00936       PERFORM 3000-OUTPUT-TEXT.                                   ELTROOMB
00937  4000-EXIT.  EXIT.                                                ELTROOMB
00938 /                                                                 ELTROOMB
00939  4100-PAYABLE-AS-BASIC.                                           ELTROOMB
00940      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTROOMB
00941          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTROOMB
00942          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTROOMB
00943          GO TO 4100-OUTPUT-TEXT.                                  ELTROOMB
00944      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTROOMB
00945      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTROOMB
00946      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTROOMB
00947                                          CMF-CODE-VALUE,          ELTROOMB
00948                                          WS-TEST-FOR-PER-DIEM.    ELTROOMB
00949      EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)    ELTROOMB
00950              END-EXEC.                                            ELTROOMB
00951      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTROOMB
00952      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
00953          ADDRESS OF CMF-DESCR.                                    ELTROOMB
00954      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTROOMB
00955      STRING CMF-DESCR-LINE(1) ' '                                 ELTROOMB
00956             CMF-DESCR-LINE(2)                                     ELTROOMB
00957                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTROOMB
00958      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTROOMB
00959      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTROOMB
00960      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTROOMB
00961      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTROOMB
00962      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTROOMB
00963      IF FLAT-RATE                                                 ELTROOMB
00964        IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
00965                                        NOT =  ZEROS               ELTROOMB
00966            MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D                  ELTROOMB
00967            MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)     ELTROOMB
00968                   TO WS-DTL-PER-D-AMT                             ELTROOMB
00969            MOVE SPACES          TO TCAR-FROM-AREA                 ELTROOMB
00970            STRING WS-DTL-PER-D, ' ' 'OF' ' '                      ELTROOMB
00971                   WS-DTL-PER-D-AMT,                               ELTROOMB
00972                     DELIMITED BY SIZE INTO TCAR-FROM-AREA         ELTROOMB
00973            PERFORM TCPR-000-TEXT-COMPRESSION                      ELTROOMB
00974            MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT        ELTROOMB
00975            MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN        ELTROOMB
00976            MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN        ELTROOMB
00977            PERFORM TCPR-000-TEXT-UNSTRING                         ELTROOMB
00978            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                  ELTROOMB
00979            MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)          ELTROOMB
00980            GO TO 4100-OUTPUT-TEXT.                                ELTROOMB
00981      IF FLAT-RATE-PLUS-PERCENT                                    ELTROOMB
00982       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTROOMB
00983                                    NOT =  ZEROS AND               ELTROOMB
00984         PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2) NOT = ZEROS ELTROOMB
00985          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTROOMB
00986          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTROOMB
00987                                        TO  WS-DTL-PERCENT         ELTROOMB
00988          MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTROOMB
00989                                        TO WS-DTL-PER-D-AMT        ELTROOMB
00990          MOVE SPACES          TO TCAR-FROM-AREA                   ELTROOMB
00991          STRING WS-DTL-PP ' ' ';' ' '                             ELTROOMB
00992                 WS-FLAT-RATE-IS, ' ' WS-DTL-PER-D-AMT ' ' 'AND'   ELTROOMB
00993                 ' ' WS-PERCENT-REMAINDER                          ELTROOMB
00994                 WS-DTL-PERCENT WS-PERCENT                         ELTROOMB
00995                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTROOMB
00996          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTROOMB
00997          MOVE +13             TO TCAR-OUTPUT-FIELD-COUNT          ELTROOMB
00998          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTROOMB
00999          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTROOMB
01000          PERFORM TCPR-000-TEXT-UNSTRING                           ELTROOMB
01001          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTROOMB
01002          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTROOMB
01003          GO TO 4100-OUTPUT-TEXT.                                  ELTROOMB
01004      IF FLAT-RATE                                                 ELTROOMB
01005        IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
01006                                         NOT =  ZEROS              ELTROOMB
01007             MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D                 ELTROOMB
01008             MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)    ELTROOMB
01009                    TO WS-DTL-PER-D-AMT                            ELTROOMB
01010             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
01011             STRING WS-DTL-PER-D, ' ' 'OF' ' '                     ELTROOMB
01012                    WS-DTL-PER-D-AMT,                              ELTROOMB
01013                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
01014             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
01015             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
01016             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
01017             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
01018             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
01019             MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                 ELTROOMB
01020             MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)         ELTROOMB
01021          GO TO 4100-OUTPUT-TEXT.                                  ELTROOMB
01022      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01023                                         =  ZEROS                  ELTROOMB
01024        IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01025                                         =  ZEROS                  ELTROOMB
01026                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTROOMB
01027                 MOVE WS-BASIC               TO                    ELTROOMB
01028                         COF-DTL-LINE(WS-CIA)                      ELTROOMB
01029        ELSE                                                       ELTROOMB
01030            MOVE TCAR-OPF-DATA(1) TO WS-DTL-ALLOW                  ELTROOMB
01031            MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)ELTROOMB
01032                   TO WS-DTL-ALLOW-AMT                             ELTROOMB
01033             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
01034             STRING WS-DTL-ALLOW, ' '                              ELTROOMB
01035                    WS-DTL-ALLOW-AMT,                              ELTROOMB
01036                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
01037             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
01038             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
01039             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
01040             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
01041             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
01042             MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                 ELTROOMB
01043             MOVE WS-BASIC         TO  COF-DTL-LINE(WS-CIA)        ELTROOMB
01044      ELSE                                                         ELTROOMB
01045        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTROOMB
01046        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTROOMB
01047                                     TO WS-DTL-PERCENT             ELTROOMB
01048        MOVE SPACES          TO TCAR-FROM-AREA                     ELTROOMB
01049        STRING WS-DTL-PP,                                          ELTROOMB
01050               WS-DTL-PERCENT,                                     ELTROOMB
01051               WS-PERCENT,                                         ELTROOMB
01052                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTROOMB
01053        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTROOMB
01054        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTROOMB
01055        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTROOMB
01056        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTROOMB
01057        PERFORM TCPR-000-TEXT-UNSTRING                             ELTROOMB
01058        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTROOMB
01059        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTROOMB
01060  4100-OUTPUT-TEXT.                                                ELTROOMB
01061      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
01062            ADD +1                TO WS-CIA                        ELTROOMB
01063            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTROOMB
01064            PERFORM 3000-OUTPUT-TEXT                               ELTROOMB
01065      ELSE                                                         ELTROOMB
01066         PERFORM 3000-OUTPUT-TEXT.                                 ELTROOMB
01067  4100-EXIT.  EXIT.                                                ELTROOMB
01068                                                                   ELTROOMB
01069 /                                                                 ELTROOMB
01070  4200-PAYABLE-AS-SUPP.                                            ELTROOMB
01071      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTROOMB
01072          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTROOMB
01073          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTROOMB
01074          GO TO 4200-OUTPUT-TEXT.                                  ELTROOMB
01075      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTROOMB
01076      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTROOMB
01077      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTROOMB
01078                                                CMF-CODE-VALUE     ELTROOMB
01079                                          WS-TEST-FOR-PER-DIEM.    ELTROOMB
01080      EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)    ELTROOMB
01081              END-EXEC.                                            ELTROOMB
01082      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTROOMB
01083      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
01084          ADDRESS OF CMF-DESCR.                                    ELTROOMB
01085      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTROOMB
01086      STRING CMF-DESCR-LINE(1) ' '                                 ELTROOMB
01087             CMF-DESCR-LINE(2)                                     ELTROOMB
01088                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTROOMB
01089      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTROOMB
01090      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTROOMB
01091      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTROOMB
01092      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTROOMB
01093      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTROOMB
01094      IF FLAT-RATE                                                 ELTROOMB
01095        IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
01096                                        NOT =  ZEROS               ELTROOMB
01097            MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D                  ELTROOMB
01098            MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)     ELTROOMB
01099                   TO WS-DTL-PER-D-AMT                             ELTROOMB
01100            MOVE SPACES          TO TCAR-FROM-AREA                 ELTROOMB
01101            STRING WS-DTL-PER-D, ' ' 'OF' ' '                      ELTROOMB
01102                   WS-DTL-PER-D-AMT,                               ELTROOMB
01103                     DELIMITED BY SIZE INTO TCAR-FROM-AREA         ELTROOMB
01104            PERFORM TCPR-000-TEXT-COMPRESSION                      ELTROOMB
01105            MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT        ELTROOMB
01106            MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN        ELTROOMB
01107            MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN        ELTROOMB
01108            PERFORM TCPR-000-TEXT-UNSTRING                         ELTROOMB
01109            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL           ELTROOMB
01110            MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)          ELTROOMB
01111            GO TO 4200-OUTPUT-TEXT.                                ELTROOMB
01112      IF FLAT-RATE-PLUS-PERCENT                                    ELTROOMB
01113       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTROOMB
01114                                    NOT =  ZEROS AND               ELTROOMB
01115         PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2) NOT = ZEROS ELTROOMB
01116          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTROOMB
01117          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTROOMB
01118                                        TO  WS-DTL-PERCENT         ELTROOMB
01119          MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTROOMB
01120                                        TO WS-DTL-PER-D-AMT        ELTROOMB
01121          MOVE SPACES          TO TCAR-FROM-AREA                   ELTROOMB
01122          STRING WS-DTL-PP ' ' ';' ' '                             ELTROOMB
01123                 WS-FLAT-RATE-IS, ' ' WS-DTL-PER-D-AMT ' ' 'AND'   ELTROOMB
01124                 ' ' WS-PERCENT-REMAINDER                          ELTROOMB
01125                 WS-DTL-PERCENT WS-PERCENT                         ELTROOMB
01126                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTROOMB
01127          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTROOMB
01128          MOVE +12             TO TCAR-OUTPUT-FIELD-COUNT          ELTROOMB
01129          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTROOMB
01130          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTROOMB
01131          PERFORM TCPR-000-TEXT-UNSTRING                           ELTROOMB
01132          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTROOMB
01133          MOVE WS-SUPPLEMENTAL-PERCENT TO COF-DTL-LINE(WS-CIA)     ELTROOMB
01134          GO TO 4200-OUTPUT-TEXT.                                  ELTROOMB
01135      IF FLAT-RATE                                                 ELTROOMB
01136        IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTROOMB
01137                                         NOT =  ZEROS              ELTROOMB
01138             MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D                 ELTROOMB
01139             MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)    ELTROOMB
01140                    TO WS-DTL-PER-D-AMT                            ELTROOMB
01141             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
01142             STRING WS-DTL-PER-D, ' ' 'OF' ' '                     ELTROOMB
01143                    WS-DTL-PER-D-AMT,                              ELTROOMB
01144                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
01145             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
01146             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
01147             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
01148             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
01149             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
01150             MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                 ELTROOMB
01151             MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)         ELTROOMB
01152          GO TO 4200-OUTPUT-TEXT.                                  ELTROOMB
01153      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01154                                         =  ZEROS                  ELTROOMB
01155        IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01156                                         =  ZEROS                  ELTROOMB
01157                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTROOMB
01158                MOVE WS-SUPPLEMENTAL        TO                     ELTROOMB
01159                         COF-DTL-LINE(WS-CIA)                      ELTROOMB
01160        ELSE                                                       ELTROOMB
01161            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-ALLOW             ELTROOMB
01162            MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)ELTROOMB
01163                   TO WS-DTL-ALLOW-AMT                             ELTROOMB
01164             MOVE SPACES          TO TCAR-FROM-AREA                ELTROOMB
01165             STRING WS-DTL-ALLOW, ' '                              ELTROOMB
01166                    WS-DTL-ALLOW-AMT,                              ELTROOMB
01167                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTROOMB
01168             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTROOMB
01169             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTROOMB
01170             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTROOMB
01171             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTROOMB
01172             PERFORM TCPR-000-TEXT-UNSTRING                        ELTROOMB
01173             MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL          ELTROOMB
01174             MOVE WS-SUPPLEMENTAL  TO  COF-DTL-LINE(WS-CIA)        ELTROOMB
01175      ELSE                                                         ELTROOMB
01176        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTROOMB
01177        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTROOMB
01178                                    TO WS-DTL-PERCENT              ELTROOMB
01179        MOVE SPACES          TO TCAR-FROM-AREA                     ELTROOMB
01180        STRING WS-DTL-PP,                                          ELTROOMB
01181               WS-DTL-PERCENT,                                     ELTROOMB
01182               WS-PERCENT,                                         ELTROOMB
01183                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTROOMB
01184        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTROOMB
01185        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTROOMB
01186        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTROOMB
01187        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTROOMB
01188        PERFORM TCPR-000-TEXT-UNSTRING                             ELTROOMB
01189        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTROOMB
01190        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTROOMB
01191  4200-OUTPUT-TEXT.                                                ELTROOMB
01192      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
01193            ADD +1                TO WS-CIA                        ELTROOMB
01194            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTROOMB
01195            PERFORM 3000-OUTPUT-TEXT                               ELTROOMB
01196      ELSE                                                         ELTROOMB
01197         PERFORM 3000-OUTPUT-TEXT.                                 ELTROOMB
01198  4200-EXIT.  EXIT.                                                ELTROOMB
01199 /                                                                 ELTROOMB
01200  4300-SPILLOVER-COINS.                                            ELTROOMB
01201      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01202            = '0' OR LOW-VALUES                                    ELTROOMB
01203           GO TO 4300-EXIT.                                        ELTROOMB
01204      ADD +1     TO  WS-CIA.                                       ELTROOMB
01205      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTROOMB
01206      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTROOMB
01207      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTROOMB
01208                       TO CMF-CODE-VALUE.                          ELTROOMB
01209      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTROOMB
01210              END-EXEC.                                            ELTROOMB
01211      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTROOMB
01212      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
01213          ADDRESS OF CMF-DESCR.                                    ELTROOMB
01214      MOVE SPACES          TO TCAR-FROM-AREA                       ELTROOMB
01215      STRING WS-SPILLOVER-COINS                                    ELTROOMB
01216             CMF-DESCR-LINE(1) ' '                                 ELTROOMB
01217             CMF-DESCR-LINE(2) ' '                                 ELTROOMB
01218               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTROOMB
01219      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTROOMB
01220      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTROOMB
01221      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTROOMB
01222      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTROOMB
01223      PERFORM TCPR-000-TEXT-UNSTRING                               ELTROOMB
01224      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTROOMB
01225      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
01226         ADD +1                TO WS-CIA                           ELTROOMB
01227         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTROOMB
01228         PERFORM 3000-OUTPUT-TEXT                                  ELTROOMB
01229      ELSE                                                         ELTROOMB
01230       PERFORM 3000-OUTPUT-TEXT.                                   ELTROOMB
01231  4300-EXIT.  EXIT.                                                ELTROOMB
01232 /                                                                 ELTROOMB
01233  4400-SPILLOVER-DEDUCT.                                           ELTROOMB
01234      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01235            = '0' OR LOW-VALUES                                    ELTROOMB
01236           GO TO 4400-EXIT.                                        ELTROOMB
01237      ADD +1     TO  WS-CIA.                                       ELTROOMB
01238      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTROOMB
01239      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTROOMB
01240      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTROOMB
01241                       TO CMF-CODE-VALUE                           ELTROOMB
01242      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTROOMB
01243              END-EXEC.                                            ELTROOMB
01244      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTROOMB
01245      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
01246          ADDRESS OF CMF-DESCR.                                    ELTROOMB
01247      MOVE SPACES          TO TCAR-FROM-AREA                       ELTROOMB
01248      STRING WS-SPILLOVER-DEDBL                                    ELTROOMB
01249             CMF-DESCR-LINE(1) ' '                                 ELTROOMB
01250             CMF-DESCR-LINE(2) ' '                                 ELTROOMB
01251               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTROOMB
01252      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTROOMB
01253      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTROOMB
01254      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTROOMB
01255      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTROOMB
01256      PERFORM TCPR-000-TEXT-UNSTRING                               ELTROOMB
01257      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTROOMB
01258      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTROOMB
01259         ADD +1                TO WS-CIA                           ELTROOMB
01260         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTROOMB
01261         PERFORM 3000-OUTPUT-TEXT                                  ELTROOMB
01262      ELSE                                                         ELTROOMB
01263       PERFORM 3000-OUTPUT-TEXT.                                   ELTROOMB
01264  4400-EXIT.  EXIT.                                                ELTROOMB
01265 /                                                                 ELTROOMB
01266  4600-SCAN-TAB.                                                   ELTROOMB
01267                                                                   ELTROOMB
01268      PERFORM 4700-BEN-TAB-AAR THRU 4700-EXIT.                     ELTROOMB
01269      PERFORM 4800-BEN-TAB-PPF THRU 4800-EXIT.                     ELTROOMB
01270      ADD +1            TO WS-CIA.                                 ELTROOMB
01271      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTROOMB
01272      ADD +1            TO WS-CIA.                                 ELTROOMB
01273      PERFORM 3000-OUTPUT-TEXT THRU 3000-EXIT.                     ELTROOMB
01274      PERFORM 5000-BEN-TAB-ADL THRU 5000-EXIT.                     ELTROOMB
01275      PERFORM 5100-BEN-TAB-ABM THRU 5100-EXIT.                     ELTROOMB
01276      PERFORM 5200-BEN-TAB-ACL THRU 5200-EXIT.                     ELTROOMB
01277      PERFORM 5300-BEN-TAB-AOL THRU 5300-EXIT.                     ELTROOMB
01278                                                                   ELTROOMB
01279      ADD   +2     TO  WS-CIA.                                     ELTROOMB
01280      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTROOMB
01281      PERFORM 3000-OUTPUT-TEXT THRU 3000-EXIT.                     ELTROOMB
01282                                                                   ELTROOMB
01283 /                                                                 ELTROOMB
01284  4700-BEN-TAB-AAR.                                                ELTROOMB
01285      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTROOMB
01286      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01287                                                                   ELTROOMB
01288      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01289          NOT = LOW-VALUES                                         ELTROOMB
01290       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01291          NOT = SPACE                                              ELTROOMB
01292                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTROOMB
01293                                                                   ELTROOMB
01294      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01295                                                                   ELTROOMB
01296      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01297          NOT = LOW-VALUES                                         ELTROOMB
01298       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01299          NOT = SPACE                                              ELTROOMB
01300                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTROOMB
01301                                                                   ELTROOMB
01302      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTROOMB
01303             MOVE +1                  TO WS-CIA                    ELTROOMB
01304             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTROOMB
01305             ADD  +1                  TO WS-CIA                    ELTROOMB
01306             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTROOMB
01307             PERFORM 3000-OUTPUT-TEXT.                             ELTROOMB
01308  4700-EXIT.  EXIT.                                                ELTROOMB
01309 /                                                                 ELTROOMB
01310  4800-BEN-TAB-PPF.                                                ELTROOMB
01311      MOVE ZEROS   TO  WS-HOLD1,                                   ELTROOMB
01312                       WS-HOLD2.                                   ELTROOMB
01313      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01314      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01315          NOT = LOW-VALUES                                         ELTROOMB
01316       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01317          NOT = SPACE                                              ELTROOMB
01318             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01319                        TO  WS-HOLD1.                              ELTROOMB
01320                                                                   ELTROOMB
01321      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01322      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01323          NOT = LOW-VALUES                                         ELTROOMB
01324       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01325          NOT = SPACE                                              ELTROOMB
01326             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01327                        TO  WS-HOLD2.                              ELTROOMB
01328                                                                   ELTROOMB
01329      IF WS-HOLD1 = WS-HOLD2                                       ELTROOMB
01330         IF WS-HOLD1 = ZEROS                                       ELTROOMB
01331                 GO TO 4800-EXIT                                   ELTROOMB
01332         ELSE                                                      ELTROOMB
01333             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTROOMB
01334             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01335               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTROOMB
01336                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01337               END-EXEC                                            ELTROOMB
01338               GO TO 4800-EXIT.                                    ELTROOMB
01339                                                                   ELTROOMB
01340      IF WS-HOLD1 = ZEROS                                          ELTROOMB
01341             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01342             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01343               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTROOMB
01344                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01345               END-EXEC                                            ELTROOMB
01346      ELSE                                                         ELTROOMB
01347       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTROOMB
01348       PERFORM 5900-GET-TAB-REC                                    ELTROOMB
01349          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTROOMB
01350                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01351          END-EXEC                                                 ELTROOMB
01352          IF WS-HOLD2 = ZEROS                                      ELTROOMB
01353            GO TO 4800-EXIT                                        ELTROOMB
01354          ELSE                                                     ELTROOMB
01355             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01356             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01357               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTROOMB
01358                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01359               END-EXEC.                                           ELTROOMB
01360  4800-EXIT.    EXIT.                                              ELTROOMB
01361 /                                                                 ELTROOMB
01362  5000-BEN-TAB-ADL.                                                ELTROOMB
01363      MOVE ZEROS   TO  WS-HOLD1,                                   ELTROOMB
01364                       WS-HOLD2.                                   ELTROOMB
01365      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01366      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01367          NOT = LOW-VALUES                                         ELTROOMB
01368       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01369          NOT = SPACE                                              ELTROOMB
01370             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01371                        TO  WS-HOLD1.                              ELTROOMB
01372                                                                   ELTROOMB
01373      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01374      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01375          NOT = LOW-VALUES                                         ELTROOMB
01376       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01377          NOT = SPACE                                              ELTROOMB
01378             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01379                        TO  WS-HOLD2.                              ELTROOMB
01380                                                                   ELTROOMB
01381      IF WS-HOLD1 = WS-HOLD2                                       ELTROOMB
01382         IF WS-HOLD1 = ZEROS                                       ELTROOMB
01383                 GO TO 5000-EXIT                                   ELTROOMB
01384         ELSE                                                      ELTROOMB
01385             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTROOMB
01386             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01387               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTROOMB
01388                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01389               END-EXEC                                            ELTROOMB
01390               GO TO 5000-EXIT.                                    ELTROOMB
01391                                                                   ELTROOMB
01392      IF WS-HOLD1 = ZEROS                                          ELTROOMB
01393          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTROOMB
01394          PERFORM 5900-GET-TAB-REC                                 ELTROOMB
01395          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTROOMB
01396                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01397          END-EXEC                                                 ELTROOMB
01398      ELSE                                                         ELTROOMB
01399          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTROOMB
01400          PERFORM 5900-GET-TAB-REC                                 ELTROOMB
01401          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTROOMB
01402                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01403          END-EXEC                                                 ELTROOMB
01404          IF WS-HOLD2 = ZEROS                                      ELTROOMB
01405            GO TO 5000-EXIT                                        ELTROOMB
01406          ELSE                                                     ELTROOMB
01407             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01408             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01409               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTROOMB
01410                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01411               END-EXEC.                                           ELTROOMB
01412  5000-EXIT.     EXIT.                                             ELTROOMB
01413 /                                                                 ELTROOMB
01414  5100-BEN-TAB-ABM.                                                ELTROOMB
01415      MOVE ZEROS   TO  WS-HOLD1,                                   ELTROOMB
01416                       WS-HOLD2.                                   ELTROOMB
01417      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01418      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01419          NOT = LOW-VALUES                                         ELTROOMB
01420       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01421          NOT = SPACE                                              ELTROOMB
01422             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01423                        TO  WS-HOLD1.                              ELTROOMB
01424                                                                   ELTROOMB
01425      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01426      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01427          NOT = LOW-VALUES                                         ELTROOMB
01428       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01429          NOT = SPACE                                              ELTROOMB
01430             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01431                        TO  WS-HOLD2.                              ELTROOMB
01432                                                                   ELTROOMB
01433      IF WS-HOLD1 = WS-HOLD2                                       ELTROOMB
01434         IF WS-HOLD1 = ZEROS                                       ELTROOMB
01435                 GO TO 5100-EXIT                                   ELTROOMB
01436         ELSE                                                      ELTROOMB
01437             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTROOMB
01438             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01439               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTROOMB
01440                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01441               END-EXEC                                            ELTROOMB
01442               GO TO 5100-EXIT.                                    ELTROOMB
01443                                                                   ELTROOMB
01444      IF WS-HOLD1 = ZEROS                                          ELTROOMB
01445             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01446             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01447               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTROOMB
01448                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01449               END-EXEC                                            ELTROOMB
01450      ELSE                                                         ELTROOMB
01451       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTROOMB
01452       PERFORM 5900-GET-TAB-REC                                    ELTROOMB
01453          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTROOMB
01454                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01455          END-EXEC                                                 ELTROOMB
01456          IF WS-HOLD2 = ZEROS                                      ELTROOMB
01457            GO TO 5100-EXIT                                        ELTROOMB
01458          ELSE                                                     ELTROOMB
01459             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01460             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01461               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTROOMB
01462                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01463               END-EXEC.                                           ELTROOMB
01464  5100-EXIT.     EXIT.                                             ELTROOMB
01465 /                                                                 ELTROOMB
01466  5200-BEN-TAB-ACL.                                                ELTROOMB
01467      MOVE ZEROS   TO  WS-HOLD1,                                   ELTROOMB
01468                       WS-HOLD2.                                   ELTROOMB
01469      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01470      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01471          NOT = LOW-VALUES                                         ELTROOMB
01472       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01473          NOT = SPACE                                              ELTROOMB
01474             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01475                        TO  WS-HOLD1.                              ELTROOMB
01476                                                                   ELTROOMB
01477      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01478      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01479          NOT = LOW-VALUES                                         ELTROOMB
01480       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01481          NOT = SPACE                                              ELTROOMB
01482             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01483                        TO  WS-HOLD2.                              ELTROOMB
01484                                                                   ELTROOMB
01485      IF WS-HOLD1 = WS-HOLD2                                       ELTROOMB
01486         IF WS-HOLD1 = ZEROS                                       ELTROOMB
01487                 GO TO 5200-EXIT                                   ELTROOMB
01488         ELSE                                                      ELTROOMB
01489             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTROOMB
01490             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01491               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTROOMB
01492                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01493               END-EXEC                                            ELTROOMB
01494               GO TO 5200-EXIT.                                    ELTROOMB
01495                                                                   ELTROOMB
01496      IF WS-HOLD1 = ZEROS                                          ELTROOMB
01497             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01498             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01499               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTROOMB
01500                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01501               END-EXEC                                            ELTROOMB
01502      ELSE                                                         ELTROOMB
01503       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTROOMB
01504       PERFORM 5900-GET-TAB-REC                                    ELTROOMB
01505          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTROOMB
01506                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01507          END-EXEC                                                 ELTROOMB
01508          IF WS-HOLD2 = ZEROS                                      ELTROOMB
01509            GO TO 5200-EXIT                                        ELTROOMB
01510          ELSE                                                     ELTROOMB
01511             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01512             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01513               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTROOMB
01514                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01515               END-EXEC.                                           ELTROOMB
01516  5200-EXIT.     EXIT.                                             ELTROOMB
01517 /                                                                 ELTROOMB
01518  5300-BEN-TAB-AOL.                                                ELTROOMB
01519      MOVE ZEROS   TO  WS-HOLD1,                                   ELTROOMB
01520                       WS-HOLD2.                                   ELTROOMB
01521      SET PLT-INDEX2 TO 1.                                         ELTROOMB
01522      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01523          NOT = LOW-VALUES                                         ELTROOMB
01524       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01525          NOT = SPACE                                              ELTROOMB
01526             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01527                        TO  WS-HOLD1.                              ELTROOMB
01528                                                                   ELTROOMB
01529      SET PLT-INDEX2 TO 2.                                         ELTROOMB
01530      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTROOMB
01531          NOT = LOW-VALUES                                         ELTROOMB
01532       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTROOMB
01533          NOT = SPACE                                              ELTROOMB
01534             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTROOMB
01535                        TO  WS-HOLD2.                              ELTROOMB
01536                                                                   ELTROOMB
01537      IF WS-HOLD1 = WS-HOLD2                                       ELTROOMB
01538         IF WS-HOLD1 = ZEROS                                       ELTROOMB
01539                 GO TO 5300-EXIT                                   ELTROOMB
01540         ELSE                                                      ELTROOMB
01541             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTROOMB
01542             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01543               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTROOMB
01544                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01545               END-EXEC                                            ELTROOMB
01546               GO TO 5300-EXIT.                                    ELTROOMB
01547                                                                   ELTROOMB
01548      IF WS-HOLD1 = ZEROS                                          ELTROOMB
01549             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01550             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01551               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTROOMB
01552                             COMMAREA(DFHCOMMAREA)                 ELTROOMB
01553               END-EXEC                                            ELTROOMB
01554      ELSE                                                         ELTROOMB
01555       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTROOMB
01556       PERFORM 5900-GET-TAB-REC                                    ELTROOMB
01557          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTROOMB
01558                          COMMAREA(DFHCOMMAREA)                    ELTROOMB
01559          END-EXEC                                                 ELTROOMB
01560          IF WS-HOLD2 = ZEROS                                      ELTROOMB
01561            GO TO 5300-EXIT                                        ELTROOMB
01562          ELSE                                                     ELTROOMB
01563             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTROOMB
01564             PERFORM 5900-GET-TAB-REC                              ELTROOMB
01565               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTROOMB
01566                              COMMAREA(DFHCOMMAREA)                ELTROOMB
01567               END-EXEC.                                           ELTROOMB
01568  5300-EXIT.     EXIT.                                             ELTROOMB
01569 /                                                                 ELTROOMB
01570  5900-GET-TAB-REC.                                                ELTROOMB
01571      SET CIA-GCTABULR-DDN TO TRUE.                                ELTROOMB
01572      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTROOMB
01573          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTROOMB
01574      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTROOMB
01575      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTROOMB
01576      SET IOP-RD                          TO TRUE.                 ELTROOMB
01577      SET IOP-FCQ-NONE                    TO TRUE.                 ELTROOMB
01578      SET IOP-KVQ-NONE                    TO TRUE.                 ELTROOMB
01579      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTROOMB
01580                                                                   ELTROOMB
01581      EXEC CICS LINK                                               ELTROOMB
01582                PROGRAM ('ELUIOPGM')                               ELTROOMB
01583                COMMAREA (DFHCOMMAREA)                             ELTROOMB
01584      END-EXEC.                                                    ELTROOMB
01585                                                                   ELTROOMB
01586      IF IOP-RC-NOTFND                                             ELTROOMB
01587         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTROOMB
01588         EXEC CICS ABEND                                           ELTROOMB
01589                   ABCODE(CIA-ABCODE)                              ELTROOMB
01590         END-EXEC                                                  ELTROOMB
01591      ELSE                                                         ELTROOMB
01592          IF NOT IOP-RC-OK                                         ELTROOMB
01593             SET CIA-AB-CRITIO TO TRUE                             ELTROOMB
01594             EXEC CICS ABEND                                       ELTROOMB
01595                       ABCODE(CIA-ABCODE)                          ELTROOMB
01596             END-EXEC                                              ELTROOMB
01597          END-IF                                                   ELTROOMB
01598      END-IF.                                                      ELTROOMB
01599  5900-EXIT.                                                       ELTROOMB
01600 /                                                                 ELTROOMB
01601 /   C O M P R E S S I O N   A N D  U N S T R I N G   R O U T I N EELTROOMB
01602     COPY ELSTCOMP.                                                ELTROOMB
