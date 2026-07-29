00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTCOOHC.                                            ELTCOOHC
00003  AUTHOR. JOHN CURIN - KEANE.                                         LV002
00004  DATE-WRITTEN.   5/23/86.                                         ELTCOOHC
00005  DATE-COMPILED.                                                   ELTCOOHC
00006      SKIP3                                                        ELTCOOHC
00007 ******************************************************************ELTCOOHC
00008 *@>ELTCOOHC                                                       ELTCOOHC
00009 *@¬                                                               ELTCOOHC
00010 *                        PROGRAM ABSTRACT                         ELTCOOHC
00011 *                                                                 ELTCOOHC
00012 *@¬ PROGRAM NAME:   E.L.S. COORDINATED HOME CARE TOPIC            ELTCOOHC
00013 *@¬                                                               ELTCOOHC
00014 *@¬ PROGRAM I.D.:   ELTCOOHC                                      ELTCOOHC
00015 *@¬                                                               ELTCOOHC
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTCOOHC
00017 *@¬            COORDINATED HOME CARE                              ELTCOOHC
00018 *@¬            BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTCOOHC
00019 *@¬                                                               ELTCOOHC
00020 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF COORDINATED HOME  ELTCOOHC
00021 *@¬            CARE SERVICES AFFORDED A MEMBER BY HIS GROUP.      ELTCOOHC
00022 *@¬            THIS INFORMATION IS GOTTEN BY INTEROGATING THE     ELTCOOHC
00023 *@¬            BENEFIT PROVISIONS FOR THE GROUP WITHIN THE        ELTCOOHC
00024 *@¬            CONTRACT FOR A PARTICULAR RANGE OF DATES.          ELTCOOHC
00025 *@¬                                                               ELTCOOHC
00026 *@¬ RECORDS                                                       ELTCOOHC
00027 *@¬ ACCESSED:  CONTRACT, GROUP SPECIFIC, VARIOUS BENEFIT          ELTCOOHC
00028 *@¬            PROVISION, AND A LARGE NUMBER OF DATA ELEMENT      ELTCOOHC
00029 *@¬            AND CODE VALUE RECORDS.                            ELTCOOHC
00030 *@¬                                                               ELTCOOHC
00031 *@¬ PROCESSING                                                    ELTCOOHC
00032 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTCOOHC
00033 *@¬                                                               ELTCOOHC
00034 *@¬ UPDATE HISTORY                                                ELTCOOHC
00035 *@¬                                                               ELTCOOHC
00036 *@¬ 07/24/86  JTC  CHANGED THE PICTURE OF WS-DTL-MAX-AMOUNT       ELTCOOHC
00037 *@¬                FROM $$$9 TO ZZ9.99-.                          ELTCOOHC
00038 *@¬                ALSO CHANGED THE DISPLAY OF THE MAXIMUM AMOUNT.ELTCOOHC
00039 *@¬                THERE WAS AN ERROR. A SUPPLEMENTAL MAX AMOUNT, ELTCOOHC
00040 *@¬                WOULD HAVE BEEN DISPLAY AS A BASIC MAX AMOUNT. ELTCOOHC
00041 *@¬                                                               ELTCOOHC
00042 *@¬                ALSO REMOVED THE UNNEEDED REFERENCES TO FORMAT ELTCOOHC
00043 *@¬                E FIELDS                                       ELTCOOHC
00044 *@¬                                                               ELTCOOHC
00045 *@¬ 08/13/86 JTC  REVISED THE PER DIEM PROCESSING OF THE          ELTCOOHC
00046 *@¬               PROVISION PRICING METHOD FIELD.  ALSO, REMOVED  ELTCOOHC
00047 *@¬               THE SETTING UP OF THE FIRST HEADER LINE         ELTCOOHC
00048 *@¬               WS-HDR-1.                                       ELTCOOHC
00049 *@¬                                                               ELTCOOHC
00050 *@¬ 10/02/86 JTC  VS COBOL II CONVERSION                          ELTCOOHC
00051 *@¬                                                               ELTCOOHC
00052 *@¬   11/25/86 LET  REVISED CODE DUE TO THE MOVING OF THE         ELTCOOHC
00053 *@¬                 CERTIFICATION REQUIREMENT INDICATOR FROM THE  ELTCOOHC
00054 *@¬                 TYPE FORMAT SECTION TO THE COMMON SECTION     ELTCOOHC
00055 *@¬                                                               ELTCOOHC
00056 *@¬   10/19/87 NAC  REWORD PHRASE FOR COVERED BENEFITS.           ELTCOOHC
00057 *@¬                                                               ELTCOOHC
00058 *@¬ 03/17/89 GEM  STORAGE MAMNAGEMENT ENHANCEMENTS                ELTCOOHC
00059 *@¬                                                               ELTCOOHC
00060 *@¬ 10/03/90 GEM  ADDED BENEFIT PROVISION ID AND ANCILLARY MESSAGEELTCOOHC
00061 *@¬               DELETED 'ADD +1 TO WS-CIA.' SENTENCES IN PARAGRAELTCOOHC
00062 *@¬               '6000-SCAN-TAB'.                                ELTCOOHC
00063 *@¬                                                               ELTCOOHC
00064 *@¬ 11/09/90 GEM  IN PARAGRAPH 5900-TRANSF-OTHER-RESP:            ELTCOOHC
00065 *@¬                  CHANGED 'NOT = 0' TO 'NOT = ZERO'.           ELTCOOHC
00066 *@¬                                                               ELTCOOHC
00067 *@¬ 03/01/91 GEM  ADDED LOGIC TO COMPENSATE FOR ASRA WHENEVER     ELTCOOHC
00068 *@¬               'B' FORMAT BENEFIT PROVISIONS OR PROCESSED.     ELTCOOHC
00069 *@¬                                                               ELTCOOHC
00070 *@¬ 05/16/91 JPB  CHANGED BEN-MAX-VISITS-IND TO BEN-MAX-VISIT-IND ELTCOOHC
00071 *@¬               FOR BPD FORMAT.                                 ELTCOOHC
00072 *@¬                                                               ELTCOOHC
00073 **       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       ELTCOOHC
00074 ***************************************************************** ELTCOOHC
00075 /                                                                 ELTCOOHC
00076  ENVIRONMENT DIVISION.                                            ELTCOOHC
00077      SKIP3                                                        ELTCOOHC
00078  DATA DIVISION.                                                   ELTCOOHC
00079  WORKING-STORAGE SECTION.                                         ELTCOOHC
00080  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTCOOHC
00081      '***ELTCOOHC WS BEGINS***'.                                  ELTCOOHC
00082  01  WS-PARA-COMMENTS.                                            ELTCOOHC
00083    05  WS-PARA-ID1               PIC X(4) VALUE 'XXXX'.           ELTCOOHC
00084    05  WS-PARA-ID2               PIC X(4) VALUE 'XXXX'.           ELTCOOHC
00085                                                                   ELTCOOHC
00086  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTCOOHC
00087                                                                   ELTCOOHC
00088 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTCOOHC
00089  01  WS-WORK-FIELDS.                                              ELTCOOHC
00090      05  WS-CHAR-0                     PIC X.                     ELTCOOHC
00091      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTCOOHC
00092      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTCOOHC
00093      05  WS-DTL-CERT-REQ-ID            PIC X(79) VALUE SPACES.    ELTCOOHC
00094      05  WS-DTL-CERT-REQ-IND           PIC X(79) VALUE SPACES.    ELTCOOHC
00095      05  WS-HOLD1                      PIC X(10).                 ELTCOOHC
00096      05  WS-HOLD2                      PIC X(10).                 ELTCOOHC
00097      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTCOOHC
00098      05  WS-DISPLAY-CERT-REQ           PIC X.                     ELTCOOHC
00099      05  WS-DISPLAY-MAX-AMT-TEXT       PIC X.                     ELTCOOHC
00100      05  WS-DISPLAY-MAX-VISITS-TEXT    PIC X.                     ELTCOOHC
00101      05  WS-YES                        PIC X     VALUE 'Y'.       ELTCOOHC
00102      05  WS-NO                         PIC X     VALUE 'N'.       ELTCOOHC
00103      05  WS-PER-DIEM                   PIC $$$$$9.99.             ELTCOOHC
00104      05  WS-ALLOW                      PIC $$$9.99.               ELTCOOHC
00105      05  WS-DTL-MAX-AMOUNT             PIC ZZ9.99-.               ELTCOOHC
00106      05  WS-DTL-MAX-DAYS               PIC ZZ9.                   ELTCOOHC
00107      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTCOOHC
00108      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTCOOHC
00109      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTCOOHC
00110      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTCOOHC
00111      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTCOOHC
00112      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTCOOHC
00113      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTCOOHC
00114      05  WS-HGADATES-LEN               PIC S9(4) COMP VALUE +24.  ELTCOOHC
00115      05  WS-FIRSTTIME-IND              PIC X.                     ELTCOOHC
00116        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTCOOHC
00117      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTCOOHC
00118        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTCOOHC
00119      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTCOOHC
00120        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTCOOHC
00121      05  WS-INDENT-IND                 PIC X.                     ELTCOOHC
00122        88  WS-INDENT-ON                    VALUE 'Y'.             ELTCOOHC
00123      05  WS-INDENT-FOUR-IND            PIC X.                     ELTCOOHC
00124        88  WS-INDENT-FOUR-ON               VALUE 'Y'.             ELTCOOHC
00125      05  WS-TEST-FOR-PER-DIEM          PIC XX.                    ELTCOOHC
00126        88  FLAT-RATE                       VALUE '04'.            ELTCOOHC
00127        88  FLAT-RATE-PLUS-PERCENT          VALUE '14', '21',      ELTCOOHC
00128                                                  '22'.            ELTCOOHC
00129                                                                   ELTCOOHC
00130      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTCOOHC
00131      05  WS-PRCNT-PERDM-ALLOW          PIC X(9).                  ELTCOOHC
00132      05  WS-PERCENT-FLD.                                          ELTCOOHC
00133        10  WS-PERCENTAGE               PIC ZZ9.                   ELTCOOHC
00134        10  WS-PERCENT-SIGN             PIC X.                     ELTCOOHC
00135                                                                   ELTCOOHC
00136 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTCOOHC
00137  01  WS-BEN-PROV-ID.                                              ELTCOOHC
00138      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +2. ELTCOOHC
00139      05  WS-INST-IP-CNT                PIC S9(4) COMP  VALUE +02. ELTCOOHC
00140      05  WS-INST-IP-TAB.                                          ELTCOOHC
00141        10  FILLER                      PIC X(6)  VALUE 'CHC  W'.  ELTCOOHC
00142        10  FILLER                      PIC X(6)  VALUE 'AIDE B'.  ELTCOOHC
00143      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTCOOHC
00144                                        PIC X(6)  OCCURS 2 TIMES.  ELTCOOHC
00145                                                                   ELTCOOHC
00146      05  WS-PROF-IP-CNT                PIC S9(4) COMP  VALUE +01. ELTCOOHC
00147      05  WS-PROF-IP-TAB.                                          ELTCOOHC
00148        10  FILLER                      PIC X(6)  VALUE 'CHCV D'.  ELTCOOHC
00149      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTCOOHC
00150                                        PIC X(6)  OCCURS 1 TIMES.  ELTCOOHC
00151                                                                   ELTCOOHC
00152 /            D I S P L A Y   L I N E S                            ELTCOOHC
00153  01  WS-ELS-DISPLAY-LINES.                                        ELTCOOHC
00154    05  WS-HDR-2-PROF-IP.                                          ELTCOOHC
00155      10  FILLER                    PIC X(16) VALUE SPACES.        ELTCOOHC
00156      10  FILLER                    PIC X(44)                      ELTCOOHC
00157          VALUE 'COORDINATED HOME CARE SERVICES PROFESSIONAL'.     ELTCOOHC
00158      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTCOOHC
00159                                                                   ELTCOOHC
00160    05  WS-HDR-2-INST-IP.                                          ELTCOOHC
00161      10  FILLER                    PIC X(16) VALUE SPACES.        ELTCOOHC
00162      10  FILLER                    PIC X(45)                      ELTCOOHC
00163          VALUE 'COORDINATED HOME CARE SERVICES INSTITUTIONAL'.    ELTCOOHC
00164      10  FILLER                    PIC X(18) VALUE LOW-VALUES.    ELTCOOHC
00165                                                                   ELTCOOHC
00166    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTCOOHC
00167          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTCOOHC
00168                                                                   ELTCOOHC
00169    05  WS-FOLLOWING-BEN.                                          ELTCOOHC
00170      10  FILLER                    PIC X(21) VALUE                ELTCOOHC
00171          'COVERED SERVICES ARE:'.                                 ELTCOOHC
00172                                                                   ELTCOOHC
00173    05  WS-PAY-CONSDR-TEXT1.                                       ELTCOOHC
00174      10  FILLER                    PIC X(45)                      ELTCOOHC
00175        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTCOOHC
00176    05  WS-PAY-CONSDR-TEXT2.                                       ELTCOOHC
00177      10  FILLER                    PIC X(44)                      ELTCOOHC
00178        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTCOOHC
00179                                                                   ELTCOOHC
00180    05  WS-ANCILLARY-TEXT1.                                        ELTCOOHC
00181      10  FILLER                    PIC X(37)                      ELTCOOHC
00182        VALUE  'ANCILLARY SERVICES COVERED UNDER THIS'.            ELTCOOHC
00183    05  WS-ANCILLARY-TEXT2.                                        ELTCOOHC
00184      10  FILLER                    PIC X(26)                      ELTCOOHC
00185        VALUE  'CONTRACT ARE ALSO ELIGIBLE'.                       ELTCOOHC
00186    05  WS-ANCILLARY-TEXT3.                                        ELTCOOHC
00187      10  FILLER                    PIC X(40)                      ELTCOOHC
00188        VALUE  'UNDER THE COORDINATED HOME CARE PROGRAM.'.         ELTCOOHC
00189                                                                   ELTCOOHC
00190    05  WS-PAYMNT-BASED.                                           ELTCOOHC
00191      10  FILLER                    PIC X(20)                      ELTCOOHC
00192          VALUE 'PAYMENT IS BASED ON:'.                            ELTCOOHC
00193      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTCOOHC
00194                                                                   ELTCOOHC
00195    05  WS-BASIC.                                                  ELTCOOHC
00196      10  WS-BASIC-LIT              PIC X(16)                      ELTCOOHC
00197          VALUE '         BASIC: '.                                ELTCOOHC
00198      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTCOOHC
00199                                                                   ELTCOOHC
00200    05  WS-SUPPLEMENTAL.                                           ELTCOOHC
00201      10  WS-SUPP-LIT               PIC X(16)                      ELTCOOHC
00202          VALUE '  SUPPLEMENTAL: '.                                ELTCOOHC
00203      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTCOOHC
00204                                                                   ELTCOOHC
00205    05  WS-MAX-LEAVES.                                             ELTCOOHC
00206      10  FILLER                    PIC X(44) VALUE                ELTCOOHC
00207          'THE MAXIMUM NUMBER OF LEAVES PER ADMISSION: '.          ELTCOOHC
00208      10  WS-DTL-MAX-LEAVES         PIC ZZ9.                       ELTCOOHC
00209                                                                   ELTCOOHC
00210    05  WS-INDENTED.                                               ELTCOOHC
00211      10  FILLER                    PIC X(16) VALUE SPACES.        ELTCOOHC
00212      10  WS-DTL-INDENTED           PIC X(63) VALUE SPACES.        ELTCOOHC
00213                                                                   ELTCOOHC
00214    05  WS-INDENTED-FOUR.                                          ELTCOOHC
00215      10  FILLER                    PIC X(04) VALUE SPACES.        ELTCOOHC
00216      10  WS-DTL-INDENTED-FOUR      PIC X(75) VALUE SPACES.        ELTCOOHC
00217                                                                   ELTCOOHC
00218    05  WS-PAYABLE-AS.                                             ELTCOOHC
00219      10  FILLER                    PIC X(40) VALUE                ELTCOOHC
00220          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTCOOHC
00221      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTCOOHC
00222                                                                   ELTCOOHC
00223    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTCOOHC
00224        'SPILLOVER COINSURANCE: '.                                 ELTCOOHC
00225                                                                   ELTCOOHC
00226    05  WS-SPILLOVER-DEDUCT         PIC X(22)  VALUE               ELTCOOHC
00227        'SPILLOVER DEDUCTIBLE: '.                                  ELTCOOHC
00228                                                                   ELTCOOHC
00229    05  WS-SPILLOVER-FL-RT-PER-D    PIC X(30)  VALUE               ELTCOOHC
00230        'SPILLOVER FLAT RATE PER DIEM: '.                          ELTCOOHC
00231                                                                   ELTCOOHC
00232    05  WS-MAX-VISITS               PIC X(34)  VALUE               ELTCOOHC
00233        'THE MAXIMUM NUMBER OF VISITS ARE: '.                      ELTCOOHC
00234                                                                   ELTCOOHC
00235    05  WS-MAX-AMOUNT               PIC X(42)  VALUE               ELTCOOHC
00236        'THE MAXIMUM AMOUNT ELIGIBLE PER VISIT IS: '.              ELTCOOHC
00237                                                                   ELTCOOHC
00238    05  WS-SECONDARY                PIC X(10) VALUE 'SECONDARY:'.  ELTCOOHC
00239                                                                   ELTCOOHC
00240    05  WS-DAYS-REDUCED             PIC X(22) VALUE                ELTCOOHC
00241          ' DAYS REDUCTION RATIO '.                                ELTCOOHC
00242                                                                   ELTCOOHC
00243    05  WS-PER                      PIC X(03) VALUE 'PER'.         ELTCOOHC
00244                                                                   ELTCOOHC
00245    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTCOOHC
00246                                                                   ELTCOOHC
00247    05  WS-PRIOR-ADM-REQUIREMENT    PIC X(56)  VALUE               ELTCOOHC
00248        'COORDINATED HOME CARE PRIOR ADMISSION REQUIREMENT IS: '.  ELTCOOHC
00249                                                                   ELTCOOHC
00250    05  WS-CERT-REQUIREMENT         PIC X(26)  VALUE               ELTCOOHC
00251        'CERTIFICATION REQUIREMENT:'.                              ELTCOOHC
00252                                                                   ELTCOOHC
00253    05  WS-CONTRACT-RELATED.                                       ELTCOOHC
00254      10  FILLER                    PIC X(50)                      ELTCOOHC
00255        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.ELTCOOHC
00256      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTCOOHC
00257                                                                   ELTCOOHC
00258    05  WS-PVE-TEXT.                                               ELTCOOHC
00259      10  FILLER                    PIC X(44)                      ELTCOOHC
00260        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTCOOHC
00261      10  FILLER                    PIC X(35) VALUE LOW-VALUES.    ELTCOOHC
00262                                                                   ELTCOOHC
00263    05  WS-FLAT-RATE-IS             PIC X(17)  VALUE               ELTCOOHC
00264        'THE FLAT RATE IS '.                                       ELTCOOHC
00265    05  WS-PERCENT-REMAINDER        PIC X(23)  VALUE               ELTCOOHC
00266        'THE % ON REMAINDER IS '.                                  ELTCOOHC
00267                                                                   ELTCOOHC
00268    05  WS-NO-TABULAR1.                                            ELTCOOHC
00269      10  FILLER                    PIC X(51)  VALUE               ELTCOOHC
00270         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTCOOHC
00271      10  FILLER                    PIC X(22)  VALUE               ELTCOOHC
00272         'GOING FROM BENEFIT ***'.                                 ELTCOOHC
00273                                                                   ELTCOOHC
00274    05  WS-NO-TABULAR2.                                            ELTCOOHC
00275      10  FILLER                    PIC X(15)  VALUE               ELTCOOHC
00276         '*** PROVISION: '.                                        ELTCOOHC
00277      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTCOOHC
00278      10  FILLER                    PIC X VALUE SPACE.             ELTCOOHC
00279      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTCOOHC
00280      10  FILLER                    PIC X(13)  VALUE               ELTCOOHC
00281         ' TO TABULAR: '.                                          ELTCOOHC
00282      10  WS-NO-TAB-ID              PIC X(6).                      ELTCOOHC
00283      10  FILLER                    PIC X VALUE SPACE.             ELTCOOHC
00284      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTCOOHC
00285      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTCOOHC
00286                                                                   ELTCOOHC
00287    05  WS-PGM-ERROR.                                              ELTCOOHC
00288      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTCOOHC
00289      10  FILLER                    PIC X(35)  VALUE               ELTCOOHC
00290         '***  P R O G R A M   E R R O R  ***'.                    ELTCOOHC
00291      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTCOOHC
00292                                                                   ELTCOOHC
00293    05  WS-BAD-INST-PROF-SEL.                                      ELTCOOHC
00294      10  FILLER                    PIC XX VALUE SPACE.            ELTCOOHC
00295      10  FILLER                    PIC X(47) VALUE                ELTCOOHC
00296         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTCOOHC
00297      10  FILLER                    PIC X(48) VALUE                ELTCOOHC
00298         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTCOOHC
00299      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTCOOHC
00300                                                                   ELTCOOHC
00301    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTCOOHC
00302       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTCOOHC
00303                                                                   ELTCOOHC
00304    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTCOOHC
00305    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTCOOHC
00306      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTCOOHC
00307                                                                   ELTCOOHC
00308  01  WS-END                            PIC X(16)  VALUE           ELTCOOHC
00309      '*** W/S ENDS ***'.                                          ELTCOOHC
00310 /             L I N K A G E   S E C T I O N                       ELTCOOHC
00311  LINKAGE SECTION.                                                 ELTCOOHC
00312  01  DFHCOMMAREA.                                                 ELTCOOHC
00313      COPY ELSCOMMC.                                               ELTCOOHC
00314 /  *** CIA  AREA ***                                              ELTCOOHC
00315      COPY ELSCIA2C.                                               ELTCOOHC
00316 /  *** IO PARM AREA ***                                           ELTCOOHC
00317      COPY ELSIOPMC.                                               ELTCOOHC
00318 /  *** KEY AREA ***                                               ELTCOOHC
00319      COPY ELSKEYSC.                                               ELTCOOHC
00320 /  *** OUTPUT TEXT AREA ***                                       ELTCOOHC
00321      COPY ELSOUTPC.                                               ELTCOOHC
00322 /  *** TOPIC SELECTION AREA ***                                   ELTCOOHC
00323      COPY ELSSSCBC.                                               ELTCOOHC
00324 /  *** CODE MANUAL INTERFACE ***                                  ELTCOOHC
00325      COPY ELSCMIFC.                                               ELTCOOHC
00326 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTCOOHC
00327      COPY ELSCMDSC.                                               ELTCOOHC
00328 /  *** BENEFIT PROVISION TABLE ***                                ELTCOOHC
00329      COPY ELSPRVNC.                                               ELTCOOHC
00330 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTCOOHC
00331      COPY ELSTCWAC.                                               ELTCOOHC
00332 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTCOOHC
00333      COPY ELSPLGSW.                                               ELTCOOHC
00334                                                                   ELTCOOHC
00335 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTCOOHC
00336      COPY ELSPLGTB.                                               ELTCOOHC
00337 /        G R O U P   S P E C I F I C   R E C O R D                ELTCOOHC
00338  01  GROUP-SPECIFIC-RECORD.                                       ELTCOOHC
00339      COPY GCGROUPC.                                               ELTCOOHC
00340 /                  M A I N L I N E                                ELTCOOHC
00341  PROCEDURE DIVISION.                                              ELTCOOHC
00342                                                                   ELTCOOHC
00343 ******************************************************************ELTCOOHC
00344 *                                                                 ELTCOOHC
00345 *   PERFORM THE MAINLINE OPERATIONS.                              ELTCOOHC
00346 *                                                                 ELTCOOHC
00347 ******************************************************************ELTCOOHC
00348  0000-MAINLINE.                                                   ELTCOOHC
00349                                                                   ELTCOOHC
00350      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTCOOHC
00351         EXEC CICS ABEND                                           ELTCOOHC
00352                   ABCODE ('EL01')                                 ELTCOOHC
00353         END-EXEC.                                                 ELTCOOHC
00354                                                                   ELTCOOHC
00355      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCOOHC
00356          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCOOHC
00357                                                                   ELTCOOHC
00358      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCOOHC
00359      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00360          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCOOHC
00361                                                                   ELTCOOHC
00362      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCOOHC
00363      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00364          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCOOHC
00365                                                                   ELTCOOHC
00366      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCOOHC
00367      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00368          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTCOOHC
00369                                                                   ELTCOOHC
00370      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCOOHC
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00372          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCOOHC
00373                                                                   ELTCOOHC
00374      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCOOHC
00375      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00376          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTCOOHC
00377                                                                   ELTCOOHC
00378      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTCOOHC
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00380          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTCOOHC
00381                                                                   ELTCOOHC
00382      MOVE 'N'  TO WS-INDENT-IND,                                  ELTCOOHC
00383                   WS-INDENT-FOUR-IND.                             ELTCOOHC
00384      MOVE '0'  TO  WS-CHAR-0.                                     ELTCOOHC
00385                                                                   ELTCOOHC
00386      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCOOHC
00387      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00388          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTCOOHC
00389                                                                   ELTCOOHC
00390      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTCOOHC
00391                                                                   ELTCOOHC
00392      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTCOOHC
00393              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTCOOHC
00394                                                                   ELTCOOHC
00395      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTCOOHC
00396                                                                   ELTCOOHC
00397      SET CIA-STG-GETMAIN  TO TRUE.                                ELTCOOHC
00398                                                                   ELTCOOHC
00399      EXEC CICS LINK                                               ELTCOOHC
00400                PROGRAM('ELUSTGMG')                                ELTCOOHC
00401                COMMAREA(DFHCOMMAREA)                              ELTCOOHC
00402      END-EXEC.                                                    ELTCOOHC
00403                                                                   ELTCOOHC
00404      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTCOOHC
00405      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00406          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTCOOHC
00407                                                                   ELTCOOHC
00408      IF (SSB-PROV-CLASS-INST OR  SSB-PROV-CLASS-BOTH)             ELTCOOHC
00409         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTCOOHC
00410                                                                   ELTCOOHC
00411      IF (SSB-PROV-CLASS-PROF OR  SSB-PROV-CLASS-BOTH)             ELTCOOHC
00412         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTCOOHC
00413                                                                   ELTCOOHC
00414      IF NOT SSB-PROV-CLASS-INST AND  NOT SSB-PROV-CLASS-PROF      ELTCOOHC
00415                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTCOOHC
00416         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTCOOHC
00417         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTCOOHC
00418         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTCOOHC
00419         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTCOOHC
00420         MOVE SPACE  TO  COF-FUNCTION                              ELTCOOHC
00421         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOOHC
00422               COMMAREA(DFHCOMMAREA)                               ELTCOOHC
00423         END-EXEC.                                                 ELTCOOHC
00424                                                                   ELTCOOHC
00425      MOVE 'E'  TO  COF-FUNCTION.                                  ELTCOOHC
00426      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOOHC
00427                     COF-NBR-DTL-LINES.                            ELTCOOHC
00428      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00429      END-EXEC.                                                    ELTCOOHC
00430                                                                   ELTCOOHC
00431                                                                   ELTCOOHC
00432  0099-RETURN.                                                     ELTCOOHC
00433      EXEC CICS RETURN   END-EXEC.                                 ELTCOOHC
00434                                                                   ELTCOOHC
00435      GOBACK.                                                      ELTCOOHC
00436 /        I N S T I T U T I O N A L   I P   R T N E                ELTCOOHC
00437 ***************************************************************** ELTCOOHC
00438 *        I N S T I T U T I O N A L   I P   R T N E                ELTCOOHC
00439 *                                                                 ELTCOOHC
00440 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCOOHC
00441 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCOOHC
00442 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCOOHC
00443 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCOOHC
00444 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCOOHC
00445 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCOOHC
00446 *  MODULE.                                                        ELTCOOHC
00447 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCOOHC
00448 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCOOHC
00449 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCOOHC
00450 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCOOHC
00451 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCOOHC
00452 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCOOHC
00453 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCOOHC
00454 *                                                                 ELTCOOHC
00455 ***************************************************************** ELTCOOHC
00456  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTCOOHC
00457      MOVE '1000'  TO  WS-PARA-ID1.                                ELTCOOHC
00458                                                                   ELTCOOHC
00459      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOOHC
00460      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCOOHC
00461      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOOHC
00462                     COF-NBR-DTL-LINES.                            ELTCOOHC
00463      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00464      END-EXEC.                                                    ELTCOOHC
00465      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCOOHC
00466      MOVE WS-HDR-2-INST-IP  TO  COF-HDR-LINE(2).                  ELTCOOHC
00467                                                                   ELTCOOHC
00468      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTCOOHC
00469      PERFORM 1010-MOVE-IN-INST-IP                                 ELTCOOHC
00470         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCOOHC
00471         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTCOOHC
00472                                                                   ELTCOOHC
00473      GO TO 1020-CALL-COVERAGE.                                    ELTCOOHC
00474  1010-MOVE-IN-INST-IP.                                            ELTCOOHC
00475      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCOOHC
00476      MOVE WS-INST-IP-LIST(WS-SUB)  TO                             ELTCOOHC
00477                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCOOHC
00478      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCOOHC
00479                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCOOHC
00480                                                                   ELTCOOHC
00481  1020-CALL-COVERAGE.                                              ELTCOOHC
00482      MOVE '1020'  TO  WS-PARA-ID1.                                ELTCOOHC
00483      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOOHC
00484      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00485      END-EXEC.                                                    ELTCOOHC
00486                                                                   ELTCOOHC
00487      MOVE 'COORDINATED HOME CARE SERVICES ARE '                   ELTCOOHC
00488                TO SSB-TOPIC-PHRASE.                               ELTCOOHC
00489                                                                   ELTCOOHC
00490      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCOOHC
00491      END-EXEC.                                                    ELTCOOHC
00492                                                                   ELTCOOHC
00493      MOVE COF-NBR-DTL-LINES     TO  WS-CIA.                       ELTCOOHC
00494      ADD  +1  TO  WS-CIA.                                         ELTCOOHC
00495      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTCOOHC
00496      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00497      END-EXEC.                                                    ELTCOOHC
00498                                                                   ELTCOOHC
00499      IF PVN-COVG-NONE                                             ELTCOOHC
00500         GO TO 1099-EXIT.                                          ELTCOOHC
00501                                                                   ELTCOOHC
00502      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTCOOHC
00503                                                                   ELTCOOHC
00504      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCOOHC
00505            PSP-PROVN-PRICING-METHD,                               ELTCOOHC
00506            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCOOHC
00507            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCOOHC
00508            PSP-TRANSF-OTHER-RESP-IND,                             ELTCOOHC
00509            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCOOHC
00510            PSP-SPILL-OVER-DED-APL-IND,                            ELTCOOHC
00511            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTCOOHC
00512            PSP-CERTFN-REQRM-IND,                                  ELTCOOHC
00513            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCOOHC
00514            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCOOHC
00515            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCOOHC
00516            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCOOHC
00517            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCOOHC
00518            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCOOHC
00519            PSW-ADDN-ALLOW-AMT-PER-DAY,                            ELTCOOHC
00520            PSW-DAYS-RDCN-RAT-IND,                                 ELTCOOHC
00521            PSW-DAYS-RDCN-RAT-BASIC-APL,                           ELTCOOHC
00522            PSW-DAYS-RDCN-RAT-BASIC-BASE,                          ELTCOOHC
00523            PSW-DAYS-RDCN-RAT-SEC-APL,                             ELTCOOHC
00524            PSW-DAYS-RDCN-RAT-SEC-BASE,                            ELTCOOHC
00525            PSW-FLAT-RATE-PDM-AMT.                                 ELTCOOHC
00526                                                                   ELTCOOHC
00527      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCOOHC
00528      END-EXEC.                                                    ELTCOOHC
00529                                                                   ELTCOOHC
00530      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCOOHC
00531      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
00532          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCOOHC
00533                                                                   ELTCOOHC
00534      PERFORM 1030-FIND-FIRST-NONZERO                              ELTCOOHC
00535         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCOOHC
00536         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTCOOHC
00537                                                                   ELTCOOHC
00538      GO TO 1099-EXIT.                                             ELTCOOHC
00539  1030-FIND-FIRST-NONZERO.                                         ELTCOOHC
00540      SET PVN-BEN-PROVN-IDX, PLT-INDEX1 TO WS-SUB.                 ELTCOOHC
00541      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCOOHC
00542         NEXT SENTENCE                                             ELTCOOHC
00543      ELSE                                                         ELTCOOHC
00544         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTCOOHC
00545                                                                   ELTCOOHC
00546  1040-BUILD-SCREEN-LINES.                                         ELTCOOHC
00547      MOVE '1040'  TO  WS-PARA-ID1.                                ELTCOOHC
00548                                                                   ELTCOOHC
00549      SET PLT-INDEX1  TO                                           ELTCOOHC
00550                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTCOOHC
00551      IF WS-NOT-FIRST-TIME                                         ELTCOOHC
00552         MOVE 'P'  TO  COF-FUNCTION                                ELTCOOHC
00553         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTCOOHC
00554         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOOHC
00555             COMMAREA(DFHCOMMAREA)                                 ELTCOOHC
00556         END-EXEC                                                  ELTCOOHC
00557      ELSE                                                         ELTCOOHC
00558         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCOOHC
00559                                                                   ELTCOOHC
00560      MOVE 1  TO  WS-CIA.                                          ELTCOOHC
00561      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOOHC
00562         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTCOOHC
00563            SET PLT-INDEX2  TO  2                                  ELTCOOHC
00564         ELSE                                                      ELTCOOHC
00565            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTCOOHC
00566            GO TO 1099-EXIT                                        ELTCOOHC
00567      ELSE                                                         ELTCOOHC
00568         SET PLT-INDEX2  TO  1.                                    ELTCOOHC
00569                                                                   ELTCOOHC
00570 **---------------------------------------------------------------+ELTCOOHC
00571 **                                                               |ELTCOOHC
00572 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCOOHC
00573      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCOOHC
00574      ADD  +1  TO  WS-CIA.                                         ELTCOOHC
00575      MOVE ZERO  TO  WS-SUB2.                                      ELTCOOHC
00576      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCOOHC
00577      MOVE '1050'  TO  WS-PARA-ID1.                                ELTCOOHC
00578                                                                   ELTCOOHC
00579      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTCOOHC
00580         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTCOOHC
00581         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.                ELTCOOHC
00582      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOOHC
00583                                                                   ELTCOOHC
00584      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCOOHC
00585      MOVE +1  TO  WS-CIA                                          ELTCOOHC
00586      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00587      END-EXEC.                                                    ELTCOOHC
00588 **                                                               |ELTCOOHC
00589 **---------------------------------------------------------------+ELTCOOHC
00590                                                                   ELTCOOHC
00591      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
00592      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
00593        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00594               NOT = ZERO                                          ELTCOOHC
00595         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTCOOHC
00596         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
00597         ADD +1  TO  WS-CIA.                                       ELTCOOHC
00598                                                                   ELTCOOHC
00599      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
00600      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00601       AND  NOT WS-ADD-A-BLANK-LINE                                ELTCOOHC
00602        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00603               NOT = ZERO                                          ELTCOOHC
00604         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTCOOHC
00605         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
00606         ADD +1  TO  WS-CIA.                                       ELTCOOHC
00607                                                                   ELTCOOHC
00608      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
00609             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTCOOHC
00610                                                                   ELTCOOHC
00611      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00612             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTCOOHC
00613                                                                   ELTCOOHC
00614      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
00615            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTCOOHC
00616            ADD  +1   TO  WS-CIA                                   ELTCOOHC
00617            PERFORM 8000-OUTPUT-TEXT.                              ELTCOOHC
00618                                                                   ELTCOOHC
00619 **---------------------------------------------------------------+ELTCOOHC
00620 **                                                               |ELTCOOHC
00621 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCOOHC
00622 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCOOHC
00623 **     A D D I T I O N A L   P R I C I N G   P E R C E N T   O R |ELTCOOHC
00624 **     F L A T  R A T E  P E R  D I E M                      O R |ELTCOOHC
00625 **     A D D I T I O N A L   A L L O W A N C E  A M O U N T      |ELTCOOHC
00626      MOVE ZEROS       TO WS-PER-DIEM,                             ELTCOOHC
00627                          WS-PERCENTAGE.                           ELTCOOHC
00628      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
00629      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
00630         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
00631                                                              '19' ELTCOOHC
00632         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOOHC
00633         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
00634         ADD +1  TO  WS-CIA.                                       ELTCOOHC
00635                                                                   ELTCOOHC
00636      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
00637      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOOHC
00638         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
00639                                                        '19' AND   ELTCOOHC
00640         NOT WS-ADD-A-BLANK-LINE                                   ELTCOOHC
00641         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOOHC
00642         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
00643         ADD +1  TO  WS-CIA.                                       ELTCOOHC
00644                                                                   ELTCOOHC
00645      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
00646      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
00647         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOOHC
00648                            AND                                    ELTCOOHC
00649         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00650         SET  PLT-INDEX2  TO  2                                    ELTCOOHC
00651         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOOHC
00652                                                             ZERO  ELTCOOHC
00653            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCOOHC
00654            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCOOHC
00655            ADD +1  TO  WS-CIA.                                    ELTCOOHC
00656                                                                   ELTCOOHC
00657      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
00658      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
00659         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOOHC
00660                            AND                                    ELTCOOHC
00661         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTCOOHC
00662         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTCOOHC
00663         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTCOOHC
00664         ADD +1  TO  WS-CIA.                                       ELTCOOHC
00665                                                                   ELTCOOHC
00666      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTCOOHC
00667         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00668         SET  PLT-INDEX2  TO  2                                    ELTCOOHC
00669         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOOHC
00670                                                             ZERO  ELTCOOHC
00671            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCOOHC
00672            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCOOHC
00673            ADD +1  TO  WS-CIA.                                    ELTCOOHC
00674                                                                   ELTCOOHC
00675      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
00676      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
00677         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00678                                                            =  ZEROELTCOOHC
00679            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00680                                                            =  ZEROELTCOOHC
00681               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTCOOHC
00682            ELSE                                                   ELTCOOHC
00683               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOOHC
00684          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00685                                                  TO  WS-PERCENTAGEELTCOOHC
00686          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTCOOHC
00687         ELSE                                                      ELTCOOHC
00688          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTCOOHC
00689          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00690                                                 TO  WS-PERCENTAGE ELTCOOHC
00691          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTCOOHC
00692                                                                   ELTCOOHC
00693      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00694       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)           NOT =  ZEROELTCOOHC
00695        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
00696                                                            =  ZEROELTCOOHC
00697         IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00698                                                            =  ZEROELTCOOHC
00699         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTCOOHC
00700                                                            =  ZEROELTCOOHC
00701            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00702                                                            =  ZEROELTCOOHC
00703               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTCOOHC
00704            ELSE                                                   ELTCOOHC
00705              NEXT SENTENCE                                        ELTCOOHC
00706         ELSE                                                      ELTCOOHC
00707          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00708                                                 TO  WS-ALLOW      ELTCOOHC
00709          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTCOOHC
00710                                                                   ELTCOOHC
00711      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00712        IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO    ELTCOOHC
00713          IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
00714                                                    NOT =  ZERO    ELTCOOHC
00715             MOVE                                                  ELTCOOHC
00716               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00717                                                  TO  WS-PER-DIEM. ELTCOOHC
00718                                                                   ELTCOOHC
00719      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
00720         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
00721                                             ZERO AND  NOT =  '19' ELTCOOHC
00722         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
00723         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOOHC
00724         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOOHC
00725                                        CMF-CODE-VALUE             ELTCOOHC
00726                                        WS-TEST-FOR-PER-DIEM       ELTCOOHC
00727         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCOOHC
00728         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
00729         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTCOOHC
00730                                                                   ELTCOOHC
00731      MOVE ZEROS       TO WS-PER-DIEM,                             ELTCOOHC
00732                          WS-PERCENTAGE.                           ELTCOOHC
00733      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
00734      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00735         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCOOHC
00736                                                               ZEROELTCOOHC
00737            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00738                                                            =  ZEROELTCOOHC
00739               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTCOOHC
00740            ELSE                                                   ELTCOOHC
00741               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOOHC
00742          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00743                                                  TO  WS-PERCENTAGEELTCOOHC
00744          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTCOOHC
00745         ELSE                                                      ELTCOOHC
00746            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOOHC
00747          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00748                                                 TO  WS-PERCENTAGE ELTCOOHC
00749          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTCOOHC
00750                                                                   ELTCOOHC
00751      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00752       IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)          NOT =  ZERO ELTCOOHC
00753        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
00754                                                           =  ZERO ELTCOOHC
00755        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
00756                                                           =  ZERO ELTCOOHC
00757         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTCOOHC
00758                                                           =  ZERO ELTCOOHC
00759            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00760                                                            =  ZEROELTCOOHC
00761               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTCOOHC
00762            ELSE                                                   ELTCOOHC
00763               NEXT SENTENCE                                       ELTCOOHC
00764         ELSE                                                      ELTCOOHC
00765          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00766                                                 TO  WS-ALLOW      ELTCOOHC
00767          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTCOOHC
00768                                                                   ELTCOOHC
00769      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00770        IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO     ELTCOOHC
00771          IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
00772                                                   NOT =  ZERO     ELTCOOHC
00773             MOVE                                                  ELTCOOHC
00774               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00775                                                  TO  WS-PER-DIEM. ELTCOOHC
00776                                                                   ELTCOOHC
00777      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOOHC
00778         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
00779                                             ZERO AND  NOT =  '19' ELTCOOHC
00780         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
00781         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOOHC
00782         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOOHC
00783                                        CMF-CODE-VALUE             ELTCOOHC
00784                                        WS-TEST-FOR-PER-DIEM       ELTCOOHC
00785         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCOOHC
00786         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
00787         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTCOOHC
00788                                                                   ELTCOOHC
00789      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
00790         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
00791         ADD  +1   TO  WS-CIA                                      ELTCOOHC
00792         PERFORM 8000-OUTPUT-TEXT                                  ELTCOOHC
00793      ELSE                                                         ELTCOOHC
00794       PERFORM 8000-OUTPUT-TEXT.                                   ELTCOOHC
00795 **                                                               |ELTCOOHC
00796 **---------------------------------------------------------------+ELTCOOHC
00797                                                                   ELTCOOHC
00798      MOVE WS-NO TO WS-DISPLAY-CERT-REQ.                           ELTCOOHC
00799                                                                   ELTCOOHC
00800      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
00801        SET  PLT-INDEX2          TO  1                             ELTCOOHC
00802        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
00803                          NOT = ZEROES                             ELTCOOHC
00804                           MOVE WS-YES TO WS-DISPLAY-CERT-REQ.     ELTCOOHC
00805                                                                   ELTCOOHC
00806      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00807        SET  PLT-INDEX2          TO  2                             ELTCOOHC
00808        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
00809                     NOT = ZEROES                                  ELTCOOHC
00810                           MOVE WS-YES TO WS-DISPLAY-CERT-REQ.     ELTCOOHC
00811                                                                   ELTCOOHC
00812      IF WS-DISPLAY-CERT-REQ = WS-YES                              ELTCOOHC
00813         ADD +1                   TO WS-CIA                        ELTCOOHC
00814         MOVE WS-CERT-REQUIREMENT TO COF-DTL-LINE(WS-CIA)          ELTCOOHC
00815         ADD +1                   TO WS-CIA.                       ELTCOOHC
00816                                                                   ELTCOOHC
00817      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
00818        SET  PLT-INDEX2          TO  1                             ELTCOOHC
00819        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
00820                          NOT = ZEROES                             ELTCOOHC
00821                        PERFORM 5500-CERTIFICATION-REQ.            ELTCOOHC
00822                                                                   ELTCOOHC
00823      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00824        SET  PLT-INDEX2          TO  2                             ELTCOOHC
00825        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
00826                     NOT = ZEROES                                  ELTCOOHC
00827                        PERFORM 5500-CERTIFICATION-REQ.            ELTCOOHC
00828                                                                   ELTCOOHC
00829      IF WS-DISPLAY-CERT-REQ = WS-YES                              ELTCOOHC
00830         PERFORM 8000-OUTPUT-TEXT.                                 ELTCOOHC
00831 **---------------------------------------------------------------+ELTCOOHC
00832 **                                                               |ELTCOOHC
00833 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTCOOHC
00834                                                                   ELTCOOHC
00835      SET  PLT-INDEX2          TO  1.                              ELTCOOHC
00836      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00837       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTCOOHC
00838        IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
00839                     NOT = '0'                                     ELTCOOHC
00840         IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTCOOHC
00841                     NOT = LOW-VALUES                              ELTCOOHC
00842          ADD +1                   TO WS-CIA                       ELTCOOHC
00843          MOVE 'BPW'               TO  CMF-RECORD-PREFIX           ELTCOOHC
00844          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTCOOHC
00845          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
00846                                   TO  CMF-CODE-VALUE              ELTCOOHC
00847          MOVE WS-YES              TO  WS-ADD-A-BLANK-IND          ELTCOOHC
00848          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTCOOHC
00849                                                                   ELTCOOHC
00850      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00851       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTCOOHC
00852        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00853                     NOT = ZEROS  AND                              ELTCOOHC
00854            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00855                      NOT = ZEROS                                  ELTCOOHC
00856             MOVE                                                  ELTCOOHC
00857              PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTCOOHC
00858                    TO WS-DTL-DAYS-REDUCED-APL                     ELTCOOHC
00859             MOVE                                                  ELTCOOHC
00860              PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTCOOHC
00861                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTCOOHC
00862             MOVE SPACES          TO TCAR-FROM-AREA                ELTCOOHC
00863             STRING WS-DAYS-REDUCED,                               ELTCOOHC
00864                    WS-BASIC-LIT,                                  ELTCOOHC
00865                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTCOOHC
00866                    WS-FOR, ' '                                    ELTCOOHC
00867                    WS-DTL-DAYS-REDUCED-BASE,                      ELTCOOHC
00868                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTCOOHC
00869             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTCOOHC
00870             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTCOOHC
00871             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTCOOHC
00872             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTCOOHC
00873             PERFORM TCPR-000-TEXT-UNSTRING                        ELTCOOHC
00874             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTCOOHC
00875             MOVE WS-YES          TO WS-ADD-A-BLANK-IND.           ELTCOOHC
00876                                                                   ELTCOOHC
00877      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00878       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTCOOHC
00879        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00880                     NOT = ZEROS  AND                              ELTCOOHC
00881            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00882                     NOT  = ZEROS                                  ELTCOOHC
00883                              ADD +1  TO  WS-CIA.                  ELTCOOHC
00884                                                                   ELTCOOHC
00885      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                      ELTCOOHC
00886       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTCOOHC
00887        IF  PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTCOOHC
00888                     NOT = ZEROS  AND                              ELTCOOHC
00889            PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
00890                      NOT = ZEROS                                  ELTCOOHC
00891             MOVE                                                  ELTCOOHC
00892              PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00893                    TO WS-DTL-DAYS-REDUCED-APL                     ELTCOOHC
00894             MOVE                                                  ELTCOOHC
00895              PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00896                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTCOOHC
00897             MOVE SPACES          TO TCAR-FROM-AREA                ELTCOOHC
00898             STRING WS-DAYS-REDUCED,                               ELTCOOHC
00899                    WS-SECONDARY, ' '                              ELTCOOHC
00900                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTCOOHC
00901                    WS-FOR, ' '                                    ELTCOOHC
00902                    WS-DTL-DAYS-REDUCED-BASE,                      ELTCOOHC
00903                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTCOOHC
00904             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTCOOHC
00905             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTCOOHC
00906             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTCOOHC
00907             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTCOOHC
00908             PERFORM TCPR-000-TEXT-UNSTRING                        ELTCOOHC
00909             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTCOOHC
00910             MOVE WS-YES          TO WS-ADD-A-BLANK-IND.           ELTCOOHC
00911                                                                   ELTCOOHC
00912      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
00913         MOVE WS-NO  TO  WS-ADD-A-BLANK-IND                        ELTCOOHC
00914         ADD +1      TO  WS-CIA                                    ELTCOOHC
00915         PERFORM  8000-OUTPUT-TEXT.                                ELTCOOHC
00916 **                                                               |ELTCOOHC
00917 **---------------------------------------------------------------+ELTCOOHC
00918                                                                   ELTCOOHC
00919 **---------------------------------------------------------------+ELTCOOHC
00920 **                                                               |ELTCOOHC
00921 **   E X T E N D E D  C A R E  P R I O R  A D M  R E Q U I R E   |ELTCOOHC
00922      IF GCG-CHC-PRIOR-ADMISSION NOT = ZEROS                       ELTCOOHC
00923          ADD +1                       TO WS-CIA                   ELTCOOHC
00924          MOVE WS-PRIOR-ADM-REQUIREMENT TO COF-DTL-LINE(WS-CIA)    ELTCOOHC
00925          ADD +1                       TO WS-CIA                   ELTCOOHC
00926          MOVE 'GROUP'                 TO  CMF-RECORD-PREFIX       ELTCOOHC
00927          MOVE 'CHC-PRIOR-ADMISSION'   TO  CMF-ELEMENT-SYSTEM-NAME ELTCOOHC
00928          MOVE GCG-CHC-PRIOR-ADMISSION                             ELTCOOHC
00929                                   TO  CMF-CODE-VALUE              ELTCOOHC
00930          MOVE ZERO    TO  WS-TEMP-NOT-USED-CNT                    ELTCOOHC
00931          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTCOOHC
00932          MOVE 'Y'  TO  WS-INDENT-FOUR-IND                         ELTCOOHC
00933          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTCOOHC
00934 **                                                               |ELTCOOHC
00935 **---------------------------------------------------------------+ELTCOOHC
00936                                                                   ELTCOOHC
00937      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
00938      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
00939        PERFORM 7000-SPILLOVER-COINS                               ELTCOOHC
00940        PERFORM 7200-SPILLOVER-DEDUCT                              ELTCOOHC
00941        IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
00942          NOT = '0' AND NOT = LOW-VALUES                           ELTCOOHC
00943           ADD +1  TO  WS-CIA                                      ELTCOOHC
00944           MOVE 'BP' TO CMF-RECORD-PREFIX                          ELTCOOHC
00945           MOVE 'SPILL-OVR-RM-F-RT-APL-IND' TO                     ELTCOOHC
00946                        CMF-ELEMENT-SYSTEM-NAME                    ELTCOOHC
00947           MOVE                                                    ELTCOOHC
00948            PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
00949             TO CMF-CODE-VALUE                                     ELTCOOHC
00950           MOVE WS-SPILLOVER-FL-RT-PER-D TO WS-TEMP-TEXT-AREA      ELTCOOHC
00951           MOVE 50 TO WS-TEMP-NOT-USED-CNT                         ELTCOOHC
00952           PERFORM 2100-CALL-CODES-MANUAL-LONG                     ELTCOOHC
00953           PERFORM 8000-OUTPUT-TEXT                                ELTCOOHC
00954        ELSE                                                       ELTCOOHC
00955          PERFORM 8000-OUTPUT-TEXT.                                ELTCOOHC
00956                                                                   ELTCOOHC
00957      PERFORM 5900-TRANSF-OTHER-RESP.                              ELTCOOHC
00958      PERFORM 6000-SCAN-TAB.                                       ELTCOOHC
00959      PERFORM 5700-ANCILLARY-TEXT.                                 ELTCOOHC
00960      PERFORM 5600-PAY-CONSID-TEXT.                                ELTCOOHC
00961                                                                   ELTCOOHC
00962  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTCOOHC
00963                                                                   ELTCOOHC
00964      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCOOHC
00965         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
00966         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCOOHC
00967         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCOOHC
00968                                                   CMF-CODE-VALUE  ELTCOOHC
00969         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTCOOHC
00970         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
00971         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
00972         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCOOHC
00973         ADD  1  TO  WS-SUB2                                       ELTCOOHC
00974         IF WS-CIA  >  20 OR  =  20                                ELTCOOHC
00975            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTCOOHC
00976            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCOOHC
00977                COMMAREA(DFHCOMMAREA)                              ELTCOOHC
00978            END-EXEC                                               ELTCOOHC
00979            MOVE +1  TO  WS-CIA.                                   ELTCOOHC
00980                                                                   ELTCOOHC
00981  1090-PROBLEM-WITH-INDICES.                                       ELTCOOHC
00982                                                                   ELTCOOHC
00983      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCOOHC
00984      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOOHC
00985                                                                   ELTCOOHC
00986      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCOOHC
00987      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOOHC
00988                                                                   ELTCOOHC
00989      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
00990      END-EXEC.                                                    ELTCOOHC
00991                                                                   ELTCOOHC
00992  1099-EXIT.            EXIT.                                      ELTCOOHC
00993 /        P R O F E S S I O N A L   I P   R T N E                  ELTCOOHC
00994 ***************************************************************** ELTCOOHC
00995 *        P R O F E S S I O N A L   I P   R T N E                  ELTCOOHC
00996 *                                                                 ELTCOOHC
00997 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCOOHC
00998 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCOOHC
00999 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCOOHC
01000 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCOOHC
01001 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCOOHC
01002 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCOOHC
01003 *  MODULE.                                                        ELTCOOHC
01004 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCOOHC
01005 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCOOHC
01006 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCOOHC
01007 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCOOHC
01008 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCOOHC
01009 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCOOHC
01010 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCOOHC
01011 *                                                                 ELTCOOHC
01012 ***************************************************************** ELTCOOHC
01013  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTCOOHC
01014      MOVE '2000'  TO  WS-PARA-ID1.                                ELTCOOHC
01015                                                                   ELTCOOHC
01016      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOOHC
01017      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCOOHC
01018      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOOHC
01019                     COF-NBR-DTL-LINES.                            ELTCOOHC
01020      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
01021      END-EXEC.                                                    ELTCOOHC
01022      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCOOHC
01023      MOVE WS-HDR-2-PROF-IP  TO  COF-HDR-LINE(2).                  ELTCOOHC
01024                                                                   ELTCOOHC
01025      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTCOOHC
01026      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTCOOHC
01027         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCOOHC
01028         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCOOHC
01029                                                                   ELTCOOHC
01030      GO TO 2020-CALL-COVERAGE.                                    ELTCOOHC
01031  2010-MOVE-IN-PROF-IP.                                            ELTCOOHC
01032      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCOOHC
01033      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTCOOHC
01034                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCOOHC
01035      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCOOHC
01036                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCOOHC
01037                                                                   ELTCOOHC
01038  2020-CALL-COVERAGE.                                              ELTCOOHC
01039      MOVE '2020'  TO  WS-PARA-ID1.                                ELTCOOHC
01040      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOOHC
01041      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
01042      END-EXEC.                                                    ELTCOOHC
01043                                                                   ELTCOOHC
01044      MOVE 'COORDINATED HOME CARE SERVICES ARE '                   ELTCOOHC
01045                TO SSB-TOPIC-PHRASE.                               ELTCOOHC
01046                                                                   ELTCOOHC
01047      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCOOHC
01048      END-EXEC.                                                    ELTCOOHC
01049                                                                   ELTCOOHC
01050      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTCOOHC
01051      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
01052      END-EXEC.                                                    ELTCOOHC
01053                                                                   ELTCOOHC
01054      IF PVN-COVG-NONE                                             ELTCOOHC
01055         GO TO 2099-EXIT.                                          ELTCOOHC
01056                                                                   ELTCOOHC
01057      MOVE +1  TO  WS-CIA.                                         ELTCOOHC
01058                                                                   ELTCOOHC
01059      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCOOHC
01060                                                                   ELTCOOHC
01061      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCOOHC
01062            PSP-PROVN-PRICING-METHD,                               ELTCOOHC
01063            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCOOHC
01064            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCOOHC
01065            PSP-TRANSF-OTHER-RESP-IND,                             ELTCOOHC
01066            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCOOHC
01067            PSP-SPILL-OVER-DED-APL-IND,                            ELTCOOHC
01068            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCOOHC
01069            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCOOHC
01070            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCOOHC
01071            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCOOHC
01072            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCOOHC
01073            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCOOHC
01074            PSD-BEN-SCOPE-ID,                                      ELTCOOHC
01075            PSD-DAYS-RDCN-RAT-IND,                                 ELTCOOHC
01076            PSD-DAYS-RDCN-RAT-BASIC-APL,                           ELTCOOHC
01077            PSD-DAYS-RDCN-RAT-BASIC-BASE,                          ELTCOOHC
01078            PSD-DAYS-RDCN-RAT-SEC-APL,                             ELTCOOHC
01079            PSD-DAYS-RDCN-RAT-SEC-BASE,                            ELTCOOHC
01080            PSD-FLAT-RATE-PDM-AMT,                                 ELTCOOHC
01081            PSD-MAX-AMT-PER-VISIT,                                 ELTCOOHC
01082            PSD-BEN-MAX-VISIT-IND,                                 ELTCOOHC
01083            PSD-BEN-MAX-VISIT-DAYS.                                ELTCOOHC
01084                                                                   ELTCOOHC
01085      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCOOHC
01086      END-EXEC.                                                    ELTCOOHC
01087                                                                   ELTCOOHC
01088      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCOOHC
01089      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
01090          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCOOHC
01091                                                                   ELTCOOHC
01092      PERFORM 2030-FIND-FIRST-NONZERO                              ELTCOOHC
01093         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCOOHC
01094         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCOOHC
01095                                                                   ELTCOOHC
01096      GO TO 2099-EXIT.                                             ELTCOOHC
01097  2030-FIND-FIRST-NONZERO.                                         ELTCOOHC
01098      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOOHC
01099      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCOOHC
01100         NEXT SENTENCE                                             ELTCOOHC
01101      ELSE                                                         ELTCOOHC
01102         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTCOOHC
01103                                                                   ELTCOOHC
01104  2040-BUILD-SCREEN-LINES.                                         ELTCOOHC
01105      MOVE '2040'  TO  WS-PARA-ID1.                                ELTCOOHC
01106                                                                   ELTCOOHC
01107      SET PLT-INDEX1   TO                                          ELTCOOHC
01108                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTCOOHC
01109      IF WS-NOT-FIRST-TIME                                         ELTCOOHC
01110         MOVE 'P'  TO  COF-FUNCTION                                ELTCOOHC
01111         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOOHC
01112             COMMAREA(DFHCOMMAREA)                                 ELTCOOHC
01113         END-EXEC                                                  ELTCOOHC
01114      ELSE                                                         ELTCOOHC
01115         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCOOHC
01116                                                                   ELTCOOHC
01117      MOVE +1  TO  WS-CIA.                                         ELTCOOHC
01118      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOOHC
01119         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTCOOHC
01120            SET PLT-INDEX2  TO  2                                  ELTCOOHC
01121         ELSE                                                      ELTCOOHC
01122            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTCOOHC
01123            GO TO 2099-EXIT                                        ELTCOOHC
01124      ELSE                                                         ELTCOOHC
01125         SET PLT-INDEX2  TO  1.                                    ELTCOOHC
01126                                                                   ELTCOOHC
01127 **---------------------------------------------------------------+ELTCOOHC
01128 **                                                               |ELTCOOHC
01129 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCOOHC
01130      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCOOHC
01131      ADD  +1  TO  WS-CIA.                                         ELTCOOHC
01132      MOVE ZERO  TO  WS-SUB2.                                      ELTCOOHC
01133      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCOOHC
01134      MOVE '2050'  TO  WS-PARA-ID1.                                ELTCOOHC
01135                                                                   ELTCOOHC
01136      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTCOOHC
01137         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTCOOHC
01138         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTCOOHC
01139      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOOHC
01140                                                                   ELTCOOHC
01141      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCOOHC
01142      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
01143      END-EXEC.                                                    ELTCOOHC
01144      MOVE +1  TO  WS-CIA.                                         ELTCOOHC
01145 **                                                               |ELTCOOHC
01146 **---------------------------------------------------------------+ELTCOOHC
01147                                                                   ELTCOOHC
01148      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01149      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01150        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01151               NOT = ZERO                                          ELTCOOHC
01152         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTCOOHC
01153         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01154         ADD +1  TO  WS-CIA.                                       ELTCOOHC
01155                                                                   ELTCOOHC
01156      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
01157      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01158       AND  NOT WS-ADD-A-BLANK-LINE                                ELTCOOHC
01159        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01160               NOT = ZERO                                          ELTCOOHC
01161         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTCOOHC
01162         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01163         ADD +1  TO  WS-CIA.                                       ELTCOOHC
01164                                                                   ELTCOOHC
01165      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01166             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTCOOHC
01167                                                                   ELTCOOHC
01168      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01169             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTCOOHC
01170                                                                   ELTCOOHC
01171      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
01172            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTCOOHC
01173            ADD  +1   TO  WS-CIA                                   ELTCOOHC
01174            PERFORM 8000-OUTPUT-TEXT.                              ELTCOOHC
01175                                                                   ELTCOOHC
01176 **---------------------------------------------------------------+ELTCOOHC
01177 **                                                               |ELTCOOHC
01178 **            B E N E F I T   S C O P E   I D                    |ELTCOOHC
01179      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01180      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01181         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTCOOHC
01182          AND NOT WS-ADD-A-BLANK-LINE                              ELTCOOHC
01183           AND  PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01184                                         '0000' AND  NOT =  '00  ' ELTCOOHC
01185               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTCOOHC
01186               ADD  +1  TO  WS-CIA                                 ELTCOOHC
01187               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTCOOHC
01188                                                                   ELTCOOHC
01189      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
01190      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01191         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTCOOHC
01192          AND NOT WS-ADD-A-BLANK-LINE                              ELTCOOHC
01193            AND PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01194                                         '0000' AND  NOT =  '00  ' ELTCOOHC
01195               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTCOOHC
01196               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTCOOHC
01197               ADD  +1  TO  WS-CIA.                                ELTCOOHC
01198                                                                   ELTCOOHC
01199      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01200      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01201             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01202                                         '0000' AND  NOT =  '00  ' ELTCOOHC
01203               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTCOOHC
01204               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOOHC
01205               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOOHC
01206                                                    CMF-CODE-VALUE ELTCOOHC
01207               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTCOOHC
01208               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTCOOHC
01209               MOVE 'Y'  TO  WS-INDENT-IND                         ELTCOOHC
01210               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTCOOHC
01211                                                                   ELTCOOHC
01212                                                                   ELTCOOHC
01213      SET PLT-INDEX2  TO  2.                                       ELTCOOHC
01214      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01215             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01216                                         '0000' AND  NOT =  '00  ' ELTCOOHC
01217               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTCOOHC
01218               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOOHC
01219               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOOHC
01220                                                    CMF-CODE-VALUE ELTCOOHC
01221               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTCOOHC
01222               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTCOOHC
01223               MOVE 'Y'  TO  WS-INDENT-IND                         ELTCOOHC
01224               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTCOOHC
01225                                                                   ELTCOOHC
01226      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
01227         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01228         ADD  +1   TO  WS-CIA                                      ELTCOOHC
01229         PERFORM 8000-OUTPUT-TEXT.                                 ELTCOOHC
01230 **                                                               |ELTCOOHC
01231 **---------------------------------------------------------------+ELTCOOHC
01232                                                                   ELTCOOHC
01233 **---------------------------------------------------------------+ELTCOOHC
01234 **                                                               |ELTCOOHC
01235 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCOOHC
01236 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCOOHC
01237 **     A D D I T I O N A L   P R I C I N G   P E R C E N T       |ELTCOOHC
01238      MOVE ZEROS       TO WS-PER-DIEM,                             ELTCOOHC
01239                          WS-PERCENTAGE.                           ELTCOOHC
01240      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01241      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
01242         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01243                                                              '19' ELTCOOHC
01244         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOOHC
01245         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01246         ADD +1  TO  WS-CIA.                                       ELTCOOHC
01247                                                                   ELTCOOHC
01248      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
01249      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOOHC
01250         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01251                                                        '19' AND   ELTCOOHC
01252         NOT WS-ADD-A-BLANK-LINE                                   ELTCOOHC
01253         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOOHC
01254         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01255         ADD +1  TO  WS-CIA.                                       ELTCOOHC
01256                                                                   ELTCOOHC
01257      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01258      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
01259         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOOHC
01260                            AND                                    ELTCOOHC
01261         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01262         SET  PLT-INDEX2  TO  2                                    ELTCOOHC
01263         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOOHC
01264                                                             ZERO  ELTCOOHC
01265            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCOOHC
01266            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCOOHC
01267            ADD +1  TO  WS-CIA.                                    ELTCOOHC
01268                                                                   ELTCOOHC
01269      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01270      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
01271         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOOHC
01272                            AND                                    ELTCOOHC
01273         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTCOOHC
01274         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTCOOHC
01275         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTCOOHC
01276         ADD +1  TO  WS-CIA.                                       ELTCOOHC
01277                                                                   ELTCOOHC
01278      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTCOOHC
01279         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01280         SET  PLT-INDEX2  TO  2                                    ELTCOOHC
01281         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOOHC
01282                                                             ZERO  ELTCOOHC
01283            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCOOHC
01284            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCOOHC
01285            ADD +1  TO  WS-CIA.                                    ELTCOOHC
01286                                                                   ELTCOOHC
01287      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01288      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01289         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01290                                                            =  ZEROELTCOOHC
01291            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01292                                                            =  ZEROELTCOOHC
01293               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTCOOHC
01294            ELSE                                                   ELTCOOHC
01295               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOOHC
01296          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01297                                                  TO  WS-PERCENTAGEELTCOOHC
01298          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTCOOHC
01299         ELSE                                                      ELTCOOHC
01300          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTCOOHC
01301          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01302                                                 TO  WS-PERCENTAGE ELTCOOHC
01303          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTCOOHC
01304                                                                   ELTCOOHC
01305      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01306         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01307                                                            =  ZEROELTCOOHC
01308          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
01309                                                            =  ZEROELTCOOHC
01310            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01311                                                            =  ZEROELTCOOHC
01312               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW.              ELTCOOHC
01313                                                                   ELTCOOHC
01314      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01315        IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
01316                                                       NOT  =  ZEROELTCOOHC
01317             MOVE                                                  ELTCOOHC
01318               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01319                                                  TO  WS-PER-DIEM  ELTCOOHC
01320             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTCOOHC
01321                                                                   ELTCOOHC
01322      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOOHC
01323         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01324                                             ZERO AND  NOT =  '19' ELTCOOHC
01325         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
01326         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOOHC
01327         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOOHC
01328                                     CMF-CODE-VALUE                ELTCOOHC
01329                                     WS-TEST-FOR-PER-DIEM          ELTCOOHC
01330         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCOOHC
01331         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
01332         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTCOOHC
01333                                                                   ELTCOOHC
01334      MOVE ZEROS       TO WS-PER-DIEM,                             ELTCOOHC
01335                          WS-PERCENTAGE.                           ELTCOOHC
01336      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
01337      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01338         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCOOHC
01339                                                               ZEROELTCOOHC
01340            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01341                                                            =  ZEROELTCOOHC
01342               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTCOOHC
01343            ELSE                                                   ELTCOOHC
01344               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOOHC
01345          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01346                                                  TO  WS-PERCENTAGEELTCOOHC
01347          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTCOOHC
01348         ELSE                                                      ELTCOOHC
01349            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOOHC
01350          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01351                                                 TO  WS-PERCENTAGE ELTCOOHC
01352          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTCOOHC
01353                                                                   ELTCOOHC
01354      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01355       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01356         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01357                                                            =  ZEROELTCOOHC
01358          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
01359                                                            =  ZEROELTCOOHC
01360            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01361                                                            =  ZEROELTCOOHC
01362               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW.              ELTCOOHC
01363                                                                   ELTCOOHC
01364      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01365       IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)            ELTCOOHC
01366                                                 NOT =  ZERO       ELTCOOHC
01367             MOVE                                                  ELTCOOHC
01368               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01369                                                  TO  WS-PER-DIEM  ELTCOOHC
01370             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTCOOHC
01371                                                                   ELTCOOHC
01372      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOOHC
01373         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOOHC
01374                                             ZERO AND  NOT =  '19' ELTCOOHC
01375         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
01376         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOOHC
01377         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOOHC
01378                                     CMF-CODE-VALUE                ELTCOOHC
01379                                     WS-TEST-FOR-PER-DIEM          ELTCOOHC
01380         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCOOHC
01381         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
01382         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTCOOHC
01383                                                                   ELTCOOHC
01384      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
01385         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCOOHC
01386         ADD  +1   TO  WS-CIA                                      ELTCOOHC
01387         PERFORM 8000-OUTPUT-TEXT                                  ELTCOOHC
01388      ELSE                                                         ELTCOOHC
01389       PERFORM 8000-OUTPUT-TEXT.                                   ELTCOOHC
01390 **                                                               |ELTCOOHC
01391 **---------------------------------------------------------------+ELTCOOHC
01392                                                                   ELTCOOHC
01393 **---------------------------------------------------------------+ELTCOOHC
01394 **    M  A  X   V  I  S  I  T  S                                 |ELTCOOHC
01395                                                                   ELTCOOHC
01396      MOVE WS-NO TO WS-DISPLAY-MAX-AMT-TEXT                        ELTCOOHC
01397                    WS-DISPLAY-MAX-VISITS-TEXT.                    ELTCOOHC
01398                                                                   ELTCOOHC
01399      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
01400      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01401       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCOOHC
01402        AND NOT = LOW-VALUES)                                      ELTCOOHC
01403          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTCOOHC
01404                                                                   ELTCOOHC
01405      SET  PLT-INDEX2 TO  2.                                       ELTCOOHC
01406      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01407       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCOOHC
01408        AND NOT = LOW-VALUES)                                      ELTCOOHC
01409          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTCOOHC
01410                                                                   ELTCOOHC
01411      IF WS-DISPLAY-MAX-VISITS-TEXT = WS-YES                       ELTCOOHC
01412          ADD +1             TO WS-CIA                             ELTCOOHC
01413          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTCOOHC
01414          ADD +1             TO WS-CIA.                            ELTCOOHC
01415                                                                   ELTCOOHC
01416      SET  PLT-INDEX2           TO  1.                             ELTCOOHC
01417      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01418       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01419        IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)          ELTCOOHC
01420                  NOT = ZEROS                                      ELTCOOHC
01421         MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01422                TO WS-DTL-MAX-DAYS                                 ELTCOOHC
01423         MOVE SPACES   TO TCAR-FROM-AREA                           ELTCOOHC
01424         STRING WS-BASIC-LIT ' '                                   ELTCOOHC
01425                WS-DTL-MAX-DAYS ' '                                ELTCOOHC
01426                WS-PER ' '                                         ELTCOOHC
01427                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTCOOHC
01428         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCOOHC
01429         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTCOOHC
01430         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTCOOHC
01431         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTCOOHC
01432         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCOOHC
01433         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTCOOHC
01434         MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO    ELTCOOHC
01435                                                 CMF-CODE-VALUE    ELTCOOHC
01436         MOVE 'BPD'                TO  CMF-RECORD-PREFIX           ELTCOOHC
01437         MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME      ELTCOOHC
01438         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
01439         PERFORM 8000-OUTPUT-TEXT.                                 ELTCOOHC
01440                                                                   ELTCOOHC
01441      SET  PLT-INDEX2           TO  2.                             ELTCOOHC
01442      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01443       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01444         IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
01445                NOT = ZEROS                                        ELTCOOHC
01446          MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTCOOHC
01447                TO WS-DTL-MAX-DAYS                                 ELTCOOHC
01448          MOVE SPACES   TO TCAR-FROM-AREA                          ELTCOOHC
01449          STRING WS-SUPP-LIT ' '                                   ELTCOOHC
01450                 WS-DTL-MAX-DAYS                                   ELTCOOHC
01451                 WS-PER ' '                                        ELTCOOHC
01452                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCOOHC
01453          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCOOHC
01454          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTCOOHC
01455          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTCOOHC
01456          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTCOOHC
01457          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCOOHC
01458          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTCOOHC
01459          MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOOHC
01460                                                 CMF-CODE-VALUE    ELTCOOHC
01461          MOVE 'BPD'                TO  CMF-RECORD-PREFIX          ELTCOOHC
01462          MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTCOOHC
01463          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTCOOHC
01464          PERFORM 8000-OUTPUT-TEXT.                                ELTCOOHC
01465                                                                   ELTCOOHC
01466 **                                                               |ELTCOOHC
01467 **---------------------------------------------------------------+ELTCOOHC
01468                                                                   ELTCOOHC
01469 **---------------------------------------------------------------+ELTCOOHC
01470 **    M  A  X   A  M  O  U  N  T   P  E  R   V  I  S  I  T       |ELTCOOHC
01471      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
01472      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01473       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01474         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTCOOHC
01475                NOT = ZEROS                                        ELTCOOHC
01476            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTCOOHC
01477                                                                   ELTCOOHC
01478      SET  PLT-INDEX2 TO  2.                                       ELTCOOHC
01479      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01480       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01481         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTCOOHC
01482                NOT = ZEROS                                        ELTCOOHC
01483            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTCOOHC
01484                                                                   ELTCOOHC
01485      IF WS-DISPLAY-MAX-AMT-TEXT = WS-YES                          ELTCOOHC
01486          ADD +1             TO WS-CIA                             ELTCOOHC
01487          MOVE WS-MAX-AMOUNT TO COF-DTL-LINE(WS-CIA)               ELTCOOHC
01488          ADD +1             TO WS-CIA.                            ELTCOOHC
01489                                                                   ELTCOOHC
01490      SET  PLT-INDEX2           TO  1.                             ELTCOOHC
01491      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01492       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01493        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
01494                  NOT = ZEROS                                      ELTCOOHC
01495         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
01496                TO WS-DTL-MAX-AMOUNT                               ELTCOOHC
01497         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTCOOHC
01498         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTCOOHC
01499         PERFORM 8000-OUTPUT-TEXT.                                 ELTCOOHC
01500                                                                   ELTCOOHC
01501      SET  PLT-INDEX2           TO  2.                             ELTCOOHC
01502      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01503       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTCOOHC
01504        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
01505                  NOT = ZEROS                                      ELTCOOHC
01506         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
01507                TO WS-DTL-MAX-AMOUNT                               ELTCOOHC
01508         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTCOOHC
01509         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTCOOHC
01510         PERFORM 8000-OUTPUT-TEXT.                                 ELTCOOHC
01511                                                                   ELTCOOHC
01512 **                                                               |ELTCOOHC
01513 **---------------------------------------------------------------+ELTCOOHC
01514                                                                   ELTCOOHC
01515 **---------------------------------------------------------------+ELTCOOHC
01516 **                                                               |ELTCOOHC
01517 **   E X T E N D E D  C A R E  P R I O R  A D M  R E Q U I R E   |ELTCOOHC
01518      IF GCG-CHC-PRIOR-ADMISSION NOT = ZEROS                       ELTCOOHC
01519          ADD +1                       TO WS-CIA                   ELTCOOHC
01520          MOVE WS-PRIOR-ADM-REQUIREMENT TO COF-DTL-LINE(WS-CIA)    ELTCOOHC
01521          ADD +1                       TO WS-CIA                   ELTCOOHC
01522          MOVE 'GROUP'                 TO  CMF-RECORD-PREFIX       ELTCOOHC
01523          MOVE 'CHC-PRIOR-ADMISSION'   TO  CMF-ELEMENT-SYSTEM-NAME ELTCOOHC
01524          MOVE GCG-CHC-PRIOR-ADMISSION                             ELTCOOHC
01525                                   TO  CMF-CODE-VALUE              ELTCOOHC
01526          MOVE ZERO    TO  WS-TEMP-NOT-USED-CNT                    ELTCOOHC
01527          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTCOOHC
01528          MOVE 'Y'  TO  WS-INDENT-FOUR-IND                         ELTCOOHC
01529          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTCOOHC
01530 **                                                               |ELTCOOHC
01531 **---------------------------------------------------------------+ELTCOOHC
01532                                                                   ELTCOOHC
01533                                                                   ELTCOOHC
01534 **---------------------------------------------------------------+ELTCOOHC
01535 **                                                               |ELTCOOHC
01536 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTCOOHC
01537                                                                   ELTCOOHC
01538      SET  PLT-INDEX2          TO  1.                              ELTCOOHC
01539      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01540        IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
01541                     NOT = '0'                                     ELTCOOHC
01542         IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTCOOHC
01543                     NOT = LOW-VALUES                              ELTCOOHC
01544          ADD +1                   TO WS-CIA                       ELTCOOHC
01545          MOVE 'BPD'               TO  CMF-RECORD-PREFIX           ELTCOOHC
01546          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTCOOHC
01547          MOVE PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTCOOHC
01548                                   TO  CMF-CODE-VALUE              ELTCOOHC
01549          MOVE WS-YES              TO  WS-ADD-A-BLANK-IND          ELTCOOHC
01550          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTCOOHC
01551                                                                   ELTCOOHC
01552      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01553        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01554                     NOT = ZEROS  AND                              ELTCOOHC
01555            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
01556                      NOT = ZEROS                                  ELTCOOHC
01557             MOVE                                                  ELTCOOHC
01558              PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTCOOHC
01559                    TO WS-DTL-DAYS-REDUCED-APL                     ELTCOOHC
01560             MOVE                                                  ELTCOOHC
01561              PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTCOOHC
01562                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTCOOHC
01563             MOVE SPACES          TO TCAR-FROM-AREA                ELTCOOHC
01564             STRING WS-DAYS-REDUCED,                               ELTCOOHC
01565                    WS-BASIC-LIT,                                  ELTCOOHC
01566                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTCOOHC
01567                    WS-FOR, ' '                                    ELTCOOHC
01568                    WS-DTL-DAYS-REDUCED-BASE,                      ELTCOOHC
01569                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTCOOHC
01570             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTCOOHC
01571             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTCOOHC
01572             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTCOOHC
01573             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTCOOHC
01574             PERFORM TCPR-000-TEXT-UNSTRING                        ELTCOOHC
01575             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTCOOHC
01576             MOVE WS-YES          TO WS-ADD-A-BLANK-IND.           ELTCOOHC
01577                                                                   ELTCOOHC
01578      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01579        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01580                     NOT = ZEROS  AND                              ELTCOOHC
01581            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
01582                      NOT = ZEROS                                  ELTCOOHC
01583                              ADD +1  TO  WS-CIA.                  ELTCOOHC
01584                                                                   ELTCOOHC
01585      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOOHC
01586        IF  PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTCOOHC
01587                     NOT = ZEROS  AND                              ELTCOOHC
01588            PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
01589                      NOT = ZEROS                                  ELTCOOHC
01590             MOVE                                                  ELTCOOHC
01591              PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTCOOHC
01592                    TO WS-DTL-DAYS-REDUCED-APL                     ELTCOOHC
01593             MOVE                                                  ELTCOOHC
01594              PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTCOOHC
01595                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTCOOHC
01596             MOVE SPACES          TO TCAR-FROM-AREA                ELTCOOHC
01597             STRING WS-DAYS-REDUCED,                               ELTCOOHC
01598                    WS-SECONDARY, ' '                              ELTCOOHC
01599                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTCOOHC
01600                    WS-FOR, ' '                                    ELTCOOHC
01601                    WS-DTL-DAYS-REDUCED-BASE,                      ELTCOOHC
01602                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTCOOHC
01603             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTCOOHC
01604             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTCOOHC
01605             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTCOOHC
01606             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTCOOHC
01607             PERFORM TCPR-000-TEXT-UNSTRING                        ELTCOOHC
01608             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTCOOHC
01609             MOVE WS-YES          TO WS-ADD-A-BLANK-IND.           ELTCOOHC
01610                                                                   ELTCOOHC
01611      IF WS-ADD-A-BLANK-LINE                                       ELTCOOHC
01612         MOVE WS-NO  TO  WS-ADD-A-BLANK-IND                        ELTCOOHC
01613         ADD +1      TO  WS-CIA                                    ELTCOOHC
01614         PERFORM  8000-OUTPUT-TEXT.                                ELTCOOHC
01615 **                                                               |ELTCOOHC
01616 **---------------------------------------------------------------+ELTCOOHC
01617                                                                   ELTCOOHC
01618      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
01619      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOOHC
01620           PERFORM 7000-SPILLOVER-COINS                            ELTCOOHC
01621           PERFORM 7200-SPILLOVER-DEDUCT                           ELTCOOHC
01622           PERFORM 8000-OUTPUT-TEXT.                               ELTCOOHC
01623                                                                   ELTCOOHC
01624      PERFORM 5900-TRANSF-OTHER-RESP.                              ELTCOOHC
01625      PERFORM 6000-SCAN-TAB.                                       ELTCOOHC
01626      PERFORM 5700-ANCILLARY-TEXT.                                 ELTCOOHC
01627      PERFORM 5600-PAY-CONSID-TEXT.                                ELTCOOHC
01628                                                                   ELTCOOHC
01629  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTCOOHC
01630                                                                   ELTCOOHC
01631      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCOOHC
01632         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
01633         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCOOHC
01634         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCOOHC
01635                                                    CMF-CODE-VALUE ELTCOOHC
01636         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTCOOHC
01637         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTCOOHC
01638         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
01639         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCOOHC
01640         ADD  1  TO  WS-SUB2                                       ELTCOOHC
01641         IF WS-CIA  >  20 OR  =  20                                ELTCOOHC
01642            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTCOOHC
01643            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCOOHC
01644                COMMAREA(DFHCOMMAREA)                              ELTCOOHC
01645            END-EXEC                                               ELTCOOHC
01646            MOVE +1  TO  WS-CIA.                                   ELTCOOHC
01647                                                                   ELTCOOHC
01648  2090-PROBLEM-WITH-INDICES.                                       ELTCOOHC
01649                                                                   ELTCOOHC
01650      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCOOHC
01651      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOOHC
01652                                                                   ELTCOOHC
01653      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCOOHC
01654      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOOHC
01655                                                                   ELTCOOHC
01656      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOOHC
01657      END-EXEC.                                                    ELTCOOHC
01658                                                                   ELTCOOHC
01659  2099-EXIT.            EXIT.                                      ELTCOOHC
01660                                                                   ELTCOOHC
01661 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTCOOHC
01662  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTCOOHC
01663      MOVE '2100'  TO  WS-PARA-ID2.                                ELTCOOHC
01664                                                                   ELTCOOHC
01665      INITIALIZE CMF-RETURN-CODE,                                  ELTCOOHC
01666                 TCAR-FROM-AREA.                                   ELTCOOHC
01667                                                                   ELTCOOHC
01668      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCOOHC
01669      END-EXEC.                                                    ELTCOOHC
01670                                                                   ELTCOOHC
01671      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOHC
01672      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
01673          ADDRESS OF CMF-DESCR.                                    ELTCOOHC
01674                                                                   ELTCOOHC
01675      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTCOOHC
01676         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTCOOHC
01677         STRING WS-TEMP-TEXT-AREA,                                 ELTCOOHC
01678            CMF-DESCR-LINE(1),        ' ',                         ELTCOOHC
01679            CMF-DESCR-LINE(2),        ' ',                         ELTCOOHC
01680            CMF-DESCR-LINE(3)                                      ELTCOOHC
01681            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTCOOHC
01682      ELSE                                                         ELTCOOHC
01683         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTCOOHC
01684         STRING CMF-DESCR-LINE(1),        ' ',                     ELTCOOHC
01685            CMF-DESCR-LINE(2),        ' ',                         ELTCOOHC
01686            CMF-DESCR-LINE(3)                                      ELTCOOHC
01687            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTCOOHC
01688                                                                   ELTCOOHC
01689      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
01690                                                                   ELTCOOHC
01691      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTCOOHC
01692      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTCOOHC
01693      IF WS-INDENT-ON                                              ELTCOOHC
01694         MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN,                    ELTCOOHC
01695                       TCAR-OUTPUT-FIELD-3-LEN,                    ELTCOOHC
01696                       TCAR-OUTPUT-FIELD-4-LEN                     ELTCOOHC
01697      ELSE                                                         ELTCOOHC
01698       MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                      ELTCOOHC
01699                     TCAR-OUTPUT-FIELD-3-LEN,                      ELTCOOHC
01700                     TCAR-OUTPUT-FIELD-4-LEN.                      ELTCOOHC
01701                                                                   ELTCOOHC
01702      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
01703                                                                   ELTCOOHC
01704      IF WS-MOVE-LINES-TO-CIA                                      ELTCOOHC
01705         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTCOOHC
01706            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTCOOHC
01707                                             WS-TEMP-NOT-USED-CNT  ELTCOOHC
01708            MOVE '2150'  TO  WS-PARA-ID2                           ELTCOOHC
01709            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTCOOHC
01710               VARYING  WS-SUB1  FROM  1  BY  1                    ELTCOOHC
01711               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTCOOHC
01712            MOVE '2100'  TO  WS-PARA-ID2                           ELTCOOHC
01713            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTCOOHC
01714            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTCOOHC
01715            ADD +1  TO  WS-CIA                                     ELTCOOHC
01716         ELSE                                                      ELTCOOHC
01717            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTCOOHC
01718            ADD +1  TO  WS-CIA.                                    ELTCOOHC
01719                                                                   ELTCOOHC
01720      IF WS-MOVE-LINES-TO-CIA                                      ELTCOOHC
01721         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTCOOHC
01722            MOVE '2160'  TO  WS-PARA-ID2                           ELTCOOHC
01723            PERFORM 2160-MOVE-LINES-TO-CIA                         ELTCOOHC
01724               VARYING  WS-SUB1  FROM  2  BY  1                    ELTCOOHC
01725               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTCOOHC
01726            MOVE '2100' TO WS-PARA-ID2                             ELTCOOHC
01727         ELSE                                                      ELTCOOHC
01728            NEXT SENTENCE                                          ELTCOOHC
01729      ELSE                                                         ELTCOOHC
01730         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTCOOHC
01731                                                                   ELTCOOHC
01732      MOVE 'N' TO WS-INDENT-IND.                                   ELTCOOHC
01733      GO TO 2199-EXIT.                                             ELTCOOHC
01734                                                                   ELTCOOHC
01735  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTCOOHC
01736      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTCOOHC
01737      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTCOOHC
01738                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTCOOHC
01739                                                                   ELTCOOHC
01740  2160-MOVE-LINES-TO-CIA.                                          ELTCOOHC
01741      IF WS-INDENT-ON                                              ELTCOOHC
01742         MOVE TCAR-OPF-DATA(WS-SUB1)  TO  WS-DTL-INDENTED          ELTCOOHC
01743         MOVE WS-INDENTED             TO  COF-DTL-LINE(WS-CIA)     ELTCOOHC
01744      ELSE                                                         ELTCOOHC
01745        MOVE TCAR-OPF-DATA(WS-SUB1)  TO COF-DTL-LINE(WS-CIA).      ELTCOOHC
01746      ADD +1  TO  WS-CIA.                                          ELTCOOHC
01747                                                                   ELTCOOHC
01748  2199-EXIT.           EXIT.                                       ELTCOOHC
01749                                                                   ELTCOOHC
01750 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTCOOHC
01751  2200-CODES-MANUAL-WITH-AMOUNT SECTION.                           ELTCOOHC
01752      MOVE '2200'  TO  WS-PARA-ID2.                                ELTCOOHC
01753                                                                   ELTCOOHC
01754      INITIALIZE CMF-RETURN-CODE,                                  ELTCOOHC
01755                 TCAR-FROM-AREA.                                   ELTCOOHC
01756                                                                   ELTCOOHC
01757      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCOOHC
01758      END-EXEC.                                                    ELTCOOHC
01759                                                                   ELTCOOHC
01760      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOHC
01761      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
01762          ADDRESS OF CMF-DESCR.                                    ELTCOOHC
01763                                                                   ELTCOOHC
01764      IF FLAT-RATE                                                 ELTCOOHC
01765        IF WS-PER-DIEM NOT = ZEROS                                 ELTCOOHC
01766         IF WS-TEMP-NOT-USED-CNT  =  ZERO                          ELTCOOHC
01767            MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                  ELTCOOHC
01768            STRING WS-TEMP-TEXT-AREA,  ' ',                        ELTCOOHC
01769               CMF-DESCR-LINE(1),        ' ',                      ELTCOOHC
01770               CMF-DESCR-LINE(2),        ' ',                      ELTCOOHC
01771               CMF-DESCR-LINE(3), ' ',        'OF' ' ' WS-PER-DIEM ELTCOOHC
01772               DELIMITED BY SIZE  INTO  TCAR-FROM-AREA             ELTCOOHC
01773               GO TO 2250-OUTPUT-TEXT                              ELTCOOHC
01774         ELSE                                                      ELTCOOHC
01775            MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN ELTCOOHC
01776            STRING CMF-DESCR-LINE(1),        ' ',                  ELTCOOHC
01777               CMF-DESCR-LINE(2),        ' ',                      ELTCOOHC
01778               CMF-DESCR-LINE(3), ' ',        'OF' ' ' WS-PER-DIEM ELTCOOHC
01779               DELIMITED BY SIZE  INTO  TCAR-FROM-AREA             ELTCOOHC
01780               GO TO 2250-OUTPUT-TEXT.                             ELTCOOHC
01781                                                                   ELTCOOHC
01782      IF FLAT-RATE-PLUS-PERCENT                                    ELTCOOHC
01783        IF WS-PERCENTAGE NOT = ZERO AND WS-PER-DIEM NOT = ZERO     ELTCOOHC
01784         IF WS-TEMP-NOT-USED-CNT  =  ZERO                          ELTCOOHC
01785            MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                  ELTCOOHC
01786            STRING WS-TEMP-TEXT-AREA,  ' ',                        ELTCOOHC
01787               CMF-DESCR-LINE(1),        ' ',                      ELTCOOHC
01788               CMF-DESCR-LINE(2),        ' ',                      ELTCOOHC
01789               CMF-DESCR-LINE(3), ' ', ';' ' '                     ELTCOOHC
01790                WS-FLAT-RATE-IS ' ' WS-PER-DIEM ' ' 'AND'          ELTCOOHC
01791                ' ' WS-PERCENT-REMAINDER                           ELTCOOHC
01792                WS-PRCNT-PERDM-ALLOW                               ELTCOOHC
01793               DELIMITED BY SIZE  INTO  TCAR-FROM-AREA             ELTCOOHC
01794               GO TO 2250-OUTPUT-TEXT                              ELTCOOHC
01795         ELSE                                                      ELTCOOHC
01796            MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN ELTCOOHC
01797            STRING CMF-DESCR-LINE(1),        ' ',                  ELTCOOHC
01798                   CMF-DESCR-LINE(2),        ' ',                  ELTCOOHC
01799                   CMF-DESCR-LINE(3), ' ', ';' ' '                 ELTCOOHC
01800                    WS-FLAT-RATE-IS ' ' WS-PER-DIEM ' ' 'AND'      ELTCOOHC
01801                    ' ' WS-PERCENT-REMAINDER                       ELTCOOHC
01802                    WS-PRCNT-PERDM-ALLOW                           ELTCOOHC
01803                    DELIMITED BY SIZE  INTO  TCAR-FROM-AREA        ELTCOOHC
01804            GO TO 2250-OUTPUT-TEXT.                                ELTCOOHC
01805                                                                   ELTCOOHC
01806      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTCOOHC
01807         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTCOOHC
01808         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTCOOHC
01809            CMF-DESCR-LINE(1),        ' ',                         ELTCOOHC
01810            CMF-DESCR-LINE(2),        ' ',                         ELTCOOHC
01811            CMF-DESCR-LINE(3), ' ',        WS-PRCNT-PERDM-ALLOW    ELTCOOHC
01812            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTCOOHC
01813      ELSE                                                         ELTCOOHC
01814         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTCOOHC
01815         STRING CMF-DESCR-LINE(1),        ' ',                     ELTCOOHC
01816            CMF-DESCR-LINE(2),        ' ',                         ELTCOOHC
01817            CMF-DESCR-LINE(3),        ' ',  WS-PRCNT-PERDM-ALLOW   ELTCOOHC
01818            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTCOOHC
01819                                                                   ELTCOOHC
01820  2250-OUTPUT-TEXT.                                                ELTCOOHC
01821      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
01822                                                                   ELTCOOHC
01823      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTCOOHC
01824      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTCOOHC
01825      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTCOOHC
01826                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTCOOHC
01827                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTCOOHC
01828      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
01829                                                                   ELTCOOHC
01830      IF WS-MOVE-LINES-TO-CIA                                      ELTCOOHC
01831         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTCOOHC
01832            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTCOOHC
01833                                             WS-TEMP-NOT-USED-CNT  ELTCOOHC
01834            MOVE '2250'  TO  WS-PARA-ID2                           ELTCOOHC
01835            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTCOOHC
01836               VARYING  WS-SUB1  FROM  1  BY  1                    ELTCOOHC
01837               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTCOOHC
01838            MOVE '2200'  TO  WS-PARA-ID2                           ELTCOOHC
01839            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTCOOHC
01840            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTCOOHC
01841            ADD +1  TO  WS-CIA                                     ELTCOOHC
01842         ELSE                                                      ELTCOOHC
01843            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTCOOHC
01844            ADD +1  TO  WS-CIA.                                    ELTCOOHC
01845                                                                   ELTCOOHC
01846      IF WS-MOVE-LINES-TO-CIA                                      ELTCOOHC
01847         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTCOOHC
01848            MOVE '2260'  TO  WS-PARA-ID2                           ELTCOOHC
01849            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTCOOHC
01850               VARYING  WS-SUB1  FROM  2  BY  1                    ELTCOOHC
01851               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTCOOHC
01852         ELSE                                                      ELTCOOHC
01853            NEXT SENTENCE                                          ELTCOOHC
01854      ELSE                                                         ELTCOOHC
01855         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTCOOHC
01856                                                                   ELTCOOHC
01857      GO TO 2299-EXIT.                                             ELTCOOHC
01858                                                                   ELTCOOHC
01859  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTCOOHC
01860      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTCOOHC
01861      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTCOOHC
01862                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTCOOHC
01863                                                                   ELTCOOHC
01864  2260-MOVE-LINES-TO-CIA.                                          ELTCOOHC
01865      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTCOOHC
01866      ADD +1  TO  WS-CIA.                                          ELTCOOHC
01867                                                                   ELTCOOHC
01868  2299-EXIT.           EXIT.                                       ELTCOOHC
01869                                                                   ELTCOOHC
01870 /            G E T   T A B U L A R   R E C O R D                  ELTCOOHC
01871 ***************************************************************** ELTCOOHC
01872 *            G E T   T A B U L A R   R E C O R D                  ELTCOOHC
01873 *                                                                 ELTCOOHC
01874 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTCOOHC
01875 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTCOOHC
01876 *  TO DISPLAY.                                                    ELTCOOHC
01877 *                                                                 ELTCOOHC
01878 ***************************************************************** ELTCOOHC
01879  2300-GET-TABULAR-RECORD SECTION.                                 ELTCOOHC
01880      MOVE '2300'  TO  WS-PARA-ID2.                                ELTCOOHC
01881                                                                   ELTCOOHC
01882      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCOOHC
01883      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
01884          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTCOOHC
01885      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTCOOHC
01886      SET  CIA-GCTABULR-DDN               TO TRUE.                 ELTCOOHC
01887      SET  IOP-RD                         TO TRUE.                 ELTCOOHC
01888      SET  IOP-FCQ-NONE                   TO TRUE.                 ELTCOOHC
01889      SET  IOP-KVQ-NONE                   TO TRUE.                 ELTCOOHC
01890      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTCOOHC
01891             COMMAREA(DFHCOMMAREA)                                 ELTCOOHC
01892      END-EXEC.                                                    ELTCOOHC
01893                                                                   ELTCOOHC
01894      IF IOP-RC-NOTFND                                             ELTCOOHC
01895         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTCOOHC
01896         EXEC CICS ABEND                                           ELTCOOHC
01897                   ABCODE(CIA-ABCODE)                              ELTCOOHC
01898         END-EXEC.                                                 ELTCOOHC
01899                                                                   ELTCOOHC
01900      IF NOT IOP-RC-OK                                             ELTCOOHC
01901         SET CIA-AB-CRITIO          TO TRUE                        ELTCOOHC
01902         EXEC CICS ABEND                                           ELTCOOHC
01903                   ABCODE(CIA-ABCODE)                              ELTCOOHC
01904         END-EXEC.                                                 ELTCOOHC
01905                                                                   ELTCOOHC
01906                                                                   ELTCOOHC
01907  2399-EXIT.           EXIT.                                       ELTCOOHC
01908 /                                                                 ELTCOOHC
01909  2400-CALL-CODES-MANUAL-DISP SECTION.                             ELTCOOHC
01910                                                                   ELTCOOHC
01911      INITIALIZE CMF-RETURN-CODE,                                  ELTCOOHC
01912                 TCAR-FROM-AREA.                                   ELTCOOHC
01913                                                                   ELTCOOHC
01914      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCOOHC
01915      END-EXEC.                                                    ELTCOOHC
01916                                                                   ELTCOOHC
01917      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOHC
01918      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
01919          ADDRESS OF CMF-DESCR.                                    ELTCOOHC
01920                                                                   ELTCOOHC
01921      STRING CMF-DESCR-LINE(1) ' '                                 ELTCOOHC
01922             CMF-DESCR-LINE(2)                                     ELTCOOHC
01923              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCOOHC
01924      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
01925      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTCOOHC
01926      IF WS-INDENT-FOUR-ON                                         ELTCOOHC
01927         MOVE +75  TO TCAR-OUTPUT-FIELD-1-LEN                      ELTCOOHC
01928         MOVE +75  TO TCAR-OUTPUT-FIELD-2-LEN                      ELTCOOHC
01929      ELSE                                                         ELTCOOHC
01930       MOVE +79  TO TCAR-OUTPUT-FIELD-1-LEN                        ELTCOOHC
01931       MOVE +79  TO TCAR-OUTPUT-FIELD-2-LEN.                       ELTCOOHC
01932      PERFORM TCPR-000-TEXT-UNSTRING                               ELTCOOHC
01933      IF WS-INDENT-FOUR-ON                                         ELTCOOHC
01934          MOVE TCAR-OPF-DATA(1) TO WS-DTL-INDENTED-FOUR            ELTCOOHC
01935          MOVE WS-INDENTED-FOUR TO COF-DTL-LINE(WS-CIA)            ELTCOOHC
01936      ELSE                                                         ELTCOOHC
01937        MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).             ELTCOOHC
01938                                                                   ELTCOOHC
01939      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCOOHC
01940       IF WS-INDENT-FOUR-ON                                        ELTCOOHC
01941           ADD +1                TO WS-CIA                         ELTCOOHC
01942           MOVE TCAR-OPF-DATA(2) TO  WS-DTL-INDENTED-FOUR          ELTCOOHC
01943           MOVE WS-INDENTED-FOUR TO  COF-DTL-LINE(WS-CIA)          ELTCOOHC
01944       ELSE                                                        ELTCOOHC
01945         ADD +1                TO WS-CIA                           ELTCOOHC
01946         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTCOOHC
01947                                                                   ELTCOOHC
01948      ADD +1    TO  WS-CIA.                                        ELTCOOHC
01949      MOVE 'N'  TO  WS-INDENT-FOUR-IND.                            ELTCOOHC
01950      IF NOT WS-ADD-A-BLANK-LINE                                   ELTCOOHC
01951           PERFORM 8000-OUTPUT-TEXT.                               ELTCOOHC
01952  2499-EXIT.   EXIT.                                               ELTCOOHC
01953 /                                                                 ELTCOOHC
01954  5000-PLACE-OF-TREATMENT-BASIC SECTION.                           ELTCOOHC
01955 **---------------------------------------------------------------+ELTCOOHC
01956 **                                                               |ELTCOOHC
01957 **        P L A C E   O F   T R E A T M E N T                    |ELTCOOHC
01958      SET  PLT-INDEX2  TO  1.                                      ELTCOOHC
01959      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTCOOHC
01960                                                              ZERO ELTCOOHC
01961         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
01962         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOOHC
01963         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTCOOHC
01964                                                   CMF-CODE-VALUE  ELTCOOHC
01965         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTCOOHC
01966         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTCOOHC
01967         MOVE 'Y'  TO  WS-INDENT-IND                               ELTCOOHC
01968         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTCOOHC
01969  5099-EXIT.  EXIT.                                                ELTCOOHC
01970 /                                                                 ELTCOOHC
01971  5100-PLACE-OF-TREATMENT-SUPP SECTION.                            ELTCOOHC
01972 **---------------------------------------------------------------+ELTCOOHC
01973 **                                                               |ELTCOOHC
01974 **        P L A C E   O F   T R E A T M E N T                    |ELTCOOHC
01975      SET  PLT-INDEX2  TO  2.                                      ELTCOOHC
01976      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTCOOHC
01977                                                              ZERO ELTCOOHC
01978         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
01979         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOOHC
01980         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTCOOHC
01981                                                   CMF-CODE-VALUE  ELTCOOHC
01982         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTCOOHC
01983         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTCOOHC
01984         MOVE 'Y'  TO  WS-INDENT-IND                               ELTCOOHC
01985         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
01986         ADD  +1      TO  WS-CIA.                                  ELTCOOHC
01987  5199-EXIT.  EXIT.                                                ELTCOOHC
01988 /                                                                 ELTCOOHC
01989  5500-CERTIFICATION-REQ SECTION.                                  ELTCOOHC
01990      MOVE 'BP'                TO  CMF-RECORD-PREFIX               ELTCOOHC
01991      MOVE 'BEN-PR-ID'         TO  CMF-ELEMENT-SYSTEM-NAME         ELTCOOHC
01992      MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                        ELTCOOHC
01993                                                CMF-CODE-VALUE     ELTCOOHC
01994                                                                   ELTCOOHC
01995      INITIALIZE CMF-RETURN-CODE,                                  ELTCOOHC
01996                 TCAR-FROM-AREA.                                   ELTCOOHC
01997                                                                   ELTCOOHC
01998      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCOOHC
01999      END-EXEC.                                                    ELTCOOHC
02000                                                                   ELTCOOHC
02001      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOHC
02002      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
02003          ADDRESS OF CMF-DESCR.                                    ELTCOOHC
02004                                                                   ELTCOOHC
02005      STRING CMF-DESCR-LINE(1) ' '                                 ELTCOOHC
02006             CMF-DESCR-LINE(2)                                     ELTCOOHC
02007                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTCOOHC
02008      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
02009      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTCOOHC
02010      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTCOOHC
02011      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTCOOHC
02012      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
02013      MOVE TCAR-OPF-DATA(1) TO WS-DTL-CERT-REQ-ID.                 ELTCOOHC
02014      MOVE 'BP'  TO  CMF-RECORD-PREFIX                             ELTCOOHC
02015      MOVE 'CERTFN-REQRM-IND'   TO  CMF-ELEMENT-SYSTEM-NAME        ELTCOOHC
02016      MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTCOOHC
02017                   TO CMF-CODE-VALUE.                              ELTCOOHC
02018                                                                   ELTCOOHC
02019      INITIALIZE CMF-RETURN-CODE,                                  ELTCOOHC
02020                 TCAR-FROM-AREA.                                   ELTCOOHC
02021                                                                   ELTCOOHC
02022      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCOOHC
02023      END-EXEC.                                                    ELTCOOHC
02024                                                                   ELTCOOHC
02025      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOHC
02026      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOHC
02027          ADDRESS OF CMF-DESCR.                                    ELTCOOHC
02028                                                                   ELTCOOHC
02029      STRING WS-DTL-CERT-REQ-ID ' '                                ELTCOOHC
02030             CMF-DESCR-LINE(1) ' '                                 ELTCOOHC
02031             CMF-DESCR-LINE(2)                                     ELTCOOHC
02032             CMF-DESCR-LINE(3)                                     ELTCOOHC
02033               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTCOOHC
02034      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
02035      MOVE +04             TO TCAR-OUTPUT-FIELD-COUNT.             ELTCOOHC
02036      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN,             ELTCOOHC
02037                              TCAR-OUTPUT-FIELD-2-LEN,             ELTCOOHC
02038                              TCAR-OUTPUT-FIELD-3-LEN,             ELTCOOHC
02039                              TCAR-OUTPUT-FIELD-4-LEN.             ELTCOOHC
02040      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
02041      IF PLT-INDEX2 = 1                                            ELTCOOHC
02042          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTCOOHC
02043          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTCOOHC
02044      ELSE                                                         ELTCOOHC
02045       MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL                ELTCOOHC
02046       MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA).              ELTCOOHC
02047      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCOOHC
02048            ADD +1                TO WS-CIA                        ELTCOOHC
02049            MOVE TCAR-OPF-DATA(2) TO WS-DTL-INDENTED               ELTCOOHC
02050            MOVE WS-INDENTED      TO COF-DTL-LINE(WS-CIA).         ELTCOOHC
02051      ADD +1  TO  WS-CIA.                                          ELTCOOHC
02052  5590-EXIT.    EXIT.                                              ELTCOOHC
02053 /                                                                 ELTCOOHC
02054  5600-PAY-CONSID-TEXT SECTION.                                    ELTCOOHC
02055      INITIALIZE TCAR-FROM-AREA.                                   ELTCOOHC
02056      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCOOHC
02057             WS-PAY-CONSDR-TEXT2                                   ELTCOOHC
02058                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCOOHC
02059      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
02060      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCOOHC
02061      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCOOHC
02062                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCOOHC
02063      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
02064      IF WS-CIA > 17                                               ELTCOOHC
02065            PERFORM 8000-OUTPUT-TEXT                               ELTCOOHC
02066            MOVE +1            TO WS-CIA.                          ELTCOOHC
02067      ADD +1                TO  WS-CIA.                            ELTCOOHC
02068      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCOOHC
02069      ADD +1                TO  WS-CIA.                            ELTCOOHC
02070      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCOOHC
02071      PERFORM 8000-OUTPUT-TEXT.                                    ELTCOOHC
02072  5600-EXIT.   EXIT.                                               ELTCOOHC
02073 /                                                                 ELTCOOHC
02074  5700-ANCILLARY-TEXT SECTION.                                     ELTCOOHC
02075      INITIALIZE TCAR-FROM-AREA.                                   ELTCOOHC
02076      STRING WS-ANCILLARY-TEXT1 ' '                                ELTCOOHC
02077             WS-ANCILLARY-TEXT2 ' '                                ELTCOOHC
02078             WS-ANCILLARY-TEXT3                                    ELTCOOHC
02079                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCOOHC
02080      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOOHC
02081      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCOOHC
02082      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCOOHC
02083                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCOOHC
02084      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOOHC
02085      IF WS-CIA > 17                                               ELTCOOHC
02086            PERFORM 8000-OUTPUT-TEXT                               ELTCOOHC
02087            MOVE +1            TO WS-CIA.                          ELTCOOHC
02088      ADD +1                TO  WS-CIA.                            ELTCOOHC
02089      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCOOHC
02090      ADD +1                TO  WS-CIA.                            ELTCOOHC
02091      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCOOHC
02092      PERFORM 8000-OUTPUT-TEXT.                                    ELTCOOHC
02093  5790-EXIT.   EXIT.                                               ELTCOOHC
02094 /                                                                 ELTCOOHC
02095  5900-TRANSF-OTHER-RESP SECTION.                                  ELTCOOHC
02096 **---------------------------------------------------------------+ELTCOOHC
02097 **                                                               |ELTCOOHC
02098 **              TRANSFER TO OTHER RESPONSIBILITY                 |ELTCOOHC
02099                                                                   ELTCOOHC
02100      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES     ELTCOOHC
02101         SET PLT-INDEX2  TO  2                                     ELTCOOHC
02102      ELSE                                                         ELTCOOHC
02103         SET PLT-INDEX2  TO  1.                                    ELTCOOHC
02104                                                                   ELTCOOHC
02105      IF PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02106                                                       NOT = ZERO  ELTCOOHC
02107         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
02108         MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTCOOHC
02109         MOVE PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)    ELTCOOHC
02110                                           TO   CMF-CODE-VALUE     ELTCOOHC
02111         MOVE SPACES             TO WS-TEMP-TEXT-AREA              ELTCOOHC
02112         MOVE +0 TO WS-TEMP-NOT-USED-CNT                           ELTCOOHC
02113         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
02114         ADD +1  TO  WS-CIA.                                       ELTCOOHC
02115  5900-EXIT.   EXIT.                                               ELTCOOHC
02116 /                                                                 ELTCOOHC
02117  6000-SCAN-TAB SECTION.                                           ELTCOOHC
02118      PERFORM 6200-BEN-TAB-AAR.                                    ELTCOOHC
02119      PERFORM 6300-BEN-TAB-PPF.                                    ELTCOOHC
02120      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTCOOHC
02121      PERFORM 8000-OUTPUT-TEXT.                                    ELTCOOHC
02122      PERFORM 6500-BEN-TAB-ADL.                                    ELTCOOHC
02123      PERFORM 6600-BEN-TAB-ABM.                                    ELTCOOHC
02124      PERFORM 6700-BEN-TAB-ACL.                                    ELTCOOHC
02125      PERFORM 6800-BEN-TAB-AOL.                                    ELTCOOHC
02126  6099-EXIT.  EXIT.                                                ELTCOOHC
02127 /                                                                 ELTCOOHC
02128  6200-BEN-TAB-AAR SECTION.                                        ELTCOOHC
02129      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTCOOHC
02130      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02131                                                                   ELTCOOHC
02132      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02133          NOT = LOW-VALUES                                         ELTCOOHC
02134       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02135          NOT = SPACE                                              ELTCOOHC
02136                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTCOOHC
02137                                                                   ELTCOOHC
02138      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02139                                                                   ELTCOOHC
02140      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02141          NOT = LOW-VALUES                                         ELTCOOHC
02142       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02143          NOT = SPACE                                              ELTCOOHC
02144                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTCOOHC
02145                                                                   ELTCOOHC
02146      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTCOOHC
02147             MOVE +2                  TO WS-CIA                    ELTCOOHC
02148             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTCOOHC
02149             MOVE +1                  TO WS-CIA                    ELTCOOHC
02150             PERFORM 8000-OUTPUT-TEXT.                             ELTCOOHC
02151  6200-EXIT.  EXIT.                                                ELTCOOHC
02152 /                                                                 ELTCOOHC
02153  6300-BEN-TAB-PPF SECTION.                                        ELTCOOHC
02154      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCOOHC
02155                       WS-HOLD2.                                   ELTCOOHC
02156      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02157      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02158          NOT = LOW-VALUES                                         ELTCOOHC
02159       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02160          NOT = SPACE                                              ELTCOOHC
02161             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02162                        TO  WS-HOLD1.                              ELTCOOHC
02163                                                                   ELTCOOHC
02164      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02165      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02166          NOT = LOW-VALUES                                         ELTCOOHC
02167       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02168          NOT = SPACE                                              ELTCOOHC
02169             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02170                        TO  WS-HOLD2.                              ELTCOOHC
02171                                                                   ELTCOOHC
02172      IF WS-HOLD1 = WS-HOLD2                                       ELTCOOHC
02173         IF WS-HOLD1 = ZEROS                                       ELTCOOHC
02174                 GO TO 6399-EXIT                                   ELTCOOHC
02175         ELSE                                                      ELTCOOHC
02176             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCOOHC
02177             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02178             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTCOOHC
02179                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02180             END-EXEC                                              ELTCOOHC
02181             GO TO 6399-EXIT.                                      ELTCOOHC
02182                                                                   ELTCOOHC
02183      IF WS-HOLD1 = ZEROS                                          ELTCOOHC
02184             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02185             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02186             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTCOOHC
02187                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02188             END-EXEC                                              ELTCOOHC
02189      ELSE                                                         ELTCOOHC
02190       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCOOHC
02191       PERFORM 2300-GET-TABULAR-RECORD                             ELTCOOHC
02192       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTCOOHC
02193                       COMMAREA(DFHCOMMAREA)                       ELTCOOHC
02194       END-EXEC                                                    ELTCOOHC
02195       IF WS-HOLD2 = ZEROS                                         ELTCOOHC
02196           GO TO 6399-EXIT                                         ELTCOOHC
02197       ELSE                                                        ELTCOOHC
02198          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCOOHC
02199          PERFORM 2300-GET-TABULAR-RECORD                          ELTCOOHC
02200          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTCOOHC
02201                          COMMAREA(DFHCOMMAREA)                    ELTCOOHC
02202          END-EXEC.                                                ELTCOOHC
02203  6399-EXIT.    EXIT.                                              ELTCOOHC
02204 /                                                                 ELTCOOHC
02205  6500-BEN-TAB-ADL SECTION.                                        ELTCOOHC
02206      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCOOHC
02207                       WS-HOLD2.                                   ELTCOOHC
02208      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02209      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02210          NOT = LOW-VALUES                                         ELTCOOHC
02211       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02212          NOT = SPACE                                              ELTCOOHC
02213             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02214                        TO  WS-HOLD1.                              ELTCOOHC
02215                                                                   ELTCOOHC
02216      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02217      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02218          NOT = LOW-VALUES                                         ELTCOOHC
02219       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02220          NOT = SPACE                                              ELTCOOHC
02221             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02222                        TO  WS-HOLD2.                              ELTCOOHC
02223                                                                   ELTCOOHC
02224      IF WS-HOLD1 = WS-HOLD2                                       ELTCOOHC
02225         IF WS-HOLD1 = ZEROS                                       ELTCOOHC
02226                 GO TO 6599-EXIT                                   ELTCOOHC
02227         ELSE                                                      ELTCOOHC
02228             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCOOHC
02229             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02230             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTCOOHC
02231                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02232             END-EXEC                                              ELTCOOHC
02233             GO TO 6599-EXIT.                                      ELTCOOHC
02234                                                                   ELTCOOHC
02235      IF WS-HOLD1 = ZEROS                                          ELTCOOHC
02236             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02237             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02238             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTCOOHC
02239                         COMMAREA(DFHCOMMAREA)                     ELTCOOHC
02240             END-EXEC                                              ELTCOOHC
02241      ELSE                                                         ELTCOOHC
02242       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCOOHC
02243       PERFORM 2300-GET-TABULAR-RECORD                             ELTCOOHC
02244       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTCOOHC
02245                       COMMAREA(DFHCOMMAREA)                       ELTCOOHC
02246       END-EXEC                                                    ELTCOOHC
02247       IF WS-HOLD2 = ZEROS                                         ELTCOOHC
02248           GO TO 6599-EXIT                                         ELTCOOHC
02249       ELSE                                                        ELTCOOHC
02250             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02251             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02252             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTCOOHC
02253                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02254             END-EXEC.                                             ELTCOOHC
02255  6599-EXIT.     EXIT.                                             ELTCOOHC
02256 /                                                                 ELTCOOHC
02257  6600-BEN-TAB-ABM SECTION.                                        ELTCOOHC
02258      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCOOHC
02259                       WS-HOLD2.                                   ELTCOOHC
02260      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02261      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02262          NOT = LOW-VALUES                                         ELTCOOHC
02263       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02264          NOT = SPACE                                              ELTCOOHC
02265             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02266                        TO  WS-HOLD1.                              ELTCOOHC
02267                                                                   ELTCOOHC
02268      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02269      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02270          NOT = LOW-VALUES                                         ELTCOOHC
02271       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02272          NOT = SPACE                                              ELTCOOHC
02273             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02274                        TO  WS-HOLD2.                              ELTCOOHC
02275                                                                   ELTCOOHC
02276      IF WS-HOLD1 = WS-HOLD2                                       ELTCOOHC
02277         IF WS-HOLD1 = ZEROS                                       ELTCOOHC
02278                 GO TO 6699-EXIT                                   ELTCOOHC
02279         ELSE                                                      ELTCOOHC
02280             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCOOHC
02281             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02282               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTCOOHC
02283                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02284               END-EXEC                                            ELTCOOHC
02285               GO TO 6699-EXIT.                                    ELTCOOHC
02286                                                                   ELTCOOHC
02287      IF WS-HOLD1 = ZEROS                                          ELTCOOHC
02288             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02289             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02290             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTCOOHC
02291                        COMMAREA(DFHCOMMAREA)                      ELTCOOHC
02292             END-EXEC                                              ELTCOOHC
02293      ELSE                                                         ELTCOOHC
02294       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCOOHC
02295       PERFORM 2300-GET-TABULAR-RECORD                             ELTCOOHC
02296       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTCOOHC
02297                       COMMAREA(DFHCOMMAREA)                       ELTCOOHC
02298       END-EXEC                                                    ELTCOOHC
02299       IF WS-HOLD2 = ZEROS                                         ELTCOOHC
02300           GO TO 6699-EXIT                                         ELTCOOHC
02301       ELSE                                                        ELTCOOHC
02302          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCOOHC
02303           PERFORM 2300-GET-TABULAR-RECORD                         ELTCOOHC
02304           EXEC CICS  LINK PROGRAM('ELGMAXIM')                     ELTCOOHC
02305                           COMMAREA(DFHCOMMAREA)                   ELTCOOHC
02306           END-EXEC.                                               ELTCOOHC
02307  6699-EXIT.     EXIT.                                             ELTCOOHC
02308 /                                                                 ELTCOOHC
02309  6700-BEN-TAB-ACL SECTION.                                        ELTCOOHC
02310      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCOOHC
02311                       WS-HOLD2.                                   ELTCOOHC
02312      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02313      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02314          NOT = LOW-VALUES                                         ELTCOOHC
02315       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02316          NOT = SPACE                                              ELTCOOHC
02317             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02318                        TO  WS-HOLD1.                              ELTCOOHC
02319                                                                   ELTCOOHC
02320      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02321      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02322          NOT = LOW-VALUES                                         ELTCOOHC
02323       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02324          NOT = SPACE                                              ELTCOOHC
02325             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02326                        TO  WS-HOLD2.                              ELTCOOHC
02327                                                                   ELTCOOHC
02328      IF WS-HOLD1 = WS-HOLD2                                       ELTCOOHC
02329         IF WS-HOLD1 = ZEROS                                       ELTCOOHC
02330                 GO TO 6799-EXIT                                   ELTCOOHC
02331         ELSE                                                      ELTCOOHC
02332             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCOOHC
02333             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02334             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTCOOHC
02335                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02336             END-EXEC                                              ELTCOOHC
02337             GO TO 6799-EXIT.                                      ELTCOOHC
02338                                                                   ELTCOOHC
02339      IF WS-HOLD1 = ZEROS                                          ELTCOOHC
02340             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02341             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02342              EXEC CICS  LINK PROGRAM('ELGCOINS')                  ELTCOOHC
02343                         COMMAREA(DFHCOMMAREA)                     ELTCOOHC
02344              END-EXEC                                             ELTCOOHC
02345      ELSE                                                         ELTCOOHC
02346       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCOOHC
02347       PERFORM 2300-GET-TABULAR-RECORD                             ELTCOOHC
02348       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTCOOHC
02349                       COMMAREA(DFHCOMMAREA)                       ELTCOOHC
02350       END-EXEC                                                    ELTCOOHC
02351       IF WS-HOLD2 = ZEROS                                         ELTCOOHC
02352           GO TO 6799-EXIT                                         ELTCOOHC
02353       ELSE                                                        ELTCOOHC
02354          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCOOHC
02355          PERFORM 2300-GET-TABULAR-RECORD                          ELTCOOHC
02356          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTCOOHC
02357                          COMMAREA(DFHCOMMAREA)                    ELTCOOHC
02358          END-EXEC.                                                ELTCOOHC
02359  6799-EXIT.     EXIT.                                             ELTCOOHC
02360 /                                                                 ELTCOOHC
02361  6800-BEN-TAB-AOL SECTION.                                        ELTCOOHC
02362      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCOOHC
02363                       WS-HOLD2.                                   ELTCOOHC
02364      SET PLT-INDEX2 TO 1.                                         ELTCOOHC
02365      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02366          NOT = LOW-VALUES                                         ELTCOOHC
02367       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02368          NOT = SPACE                                              ELTCOOHC
02369             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02370                        TO  WS-HOLD1.                              ELTCOOHC
02371                                                                   ELTCOOHC
02372      SET PLT-INDEX2 TO 2.                                         ELTCOOHC
02373      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTCOOHC
02374          NOT = LOW-VALUES                                         ELTCOOHC
02375       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02376          NOT = SPACE                                              ELTCOOHC
02377             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02378                        TO  WS-HOLD2.                              ELTCOOHC
02379                                                                   ELTCOOHC
02380      IF WS-HOLD1 = WS-HOLD2                                       ELTCOOHC
02381         IF WS-HOLD1 = ZEROS                                       ELTCOOHC
02382                 GO TO 6899-EXIT                                   ELTCOOHC
02383         ELSE                                                      ELTCOOHC
02384             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCOOHC
02385             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02386             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTCOOHC
02387                             COMMAREA(DFHCOMMAREA)                 ELTCOOHC
02388             END-EXEC                                              ELTCOOHC
02389             GO TO 6899-EXIT.                                      ELTCOOHC
02390                                                                   ELTCOOHC
02391      IF WS-HOLD1 = ZEROS                                          ELTCOOHC
02392             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCOOHC
02393             PERFORM 2300-GET-TABULAR-RECORD                       ELTCOOHC
02394             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTCOOHC
02395                         COMMAREA(DFHCOMMAREA)                     ELTCOOHC
02396             END-EXEC                                              ELTCOOHC
02397      ELSE                                                         ELTCOOHC
02398       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCOOHC
02399       PERFORM 2300-GET-TABULAR-RECORD                             ELTCOOHC
02400       EXEC CICS  LINK PROGRAM('ELGOUTPX')                         ELTCOOHC
02401                       COMMAREA(DFHCOMMAREA)                       ELTCOOHC
02402       END-EXEC                                                    ELTCOOHC
02403       IF WS-HOLD2 = ZEROS                                         ELTCOOHC
02404          GO TO 6899-EXIT                                          ELTCOOHC
02405       ELSE                                                        ELTCOOHC
02406          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCOOHC
02407          PERFORM 2300-GET-TABULAR-RECORD                          ELTCOOHC
02408          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTCOOHC
02409                          COMMAREA(DFHCOMMAREA)                    ELTCOOHC
02410          END-EXEC.                                                ELTCOOHC
02411  6899-EXIT.     EXIT.                                             ELTCOOHC
02412 /                                                                 ELTCOOHC
02413  7000-SPILLOVER-COINS SECTION.                                    ELTCOOHC
02414 **---------------------------------------------------------------+ELTCOOHC
02415 **                                                               |ELTCOOHC
02416 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTCOOHC
02417      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCOOHC
02418                                                       NOT =  '0'  ELTCOOHC
02419         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
02420         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTCOOHC
02421                                           CMF-ELEMENT-SYSTEM-NAME ELTCOOHC
02422         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTCOOHC
02423                                           TO   CMF-CODE-VALUE     ELTCOOHC
02424         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTCOOHC
02425         MOVE 56 TO WS-TEMP-NOT-USED-CNT                           ELTCOOHC
02426         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
02427         ADD +1  TO  WS-CIA.                                       ELTCOOHC
02428  7099-EXIT.    EXIT.                                              ELTCOOHC
02429 /                                                                 ELTCOOHC
02430  7200-SPILLOVER-DEDUCT SECTION.                                   ELTCOOHC
02431 **---------------------------------------------------------------+ELTCOOHC
02432 **                                                               |ELTCOOHC
02433 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTCOOHC
02434      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCOOHC
02435                                                       NOT =  '0'  ELTCOOHC
02436         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOOHC
02437         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTCOOHC
02438                                           CMF-ELEMENT-SYSTEM-NAME ELTCOOHC
02439         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTCOOHC
02440                                                     CMF-CODE-VALUEELTCOOHC
02441         MOVE WS-SPILLOVER-DEDUCT TO WS-TEMP-TEXT-AREA             ELTCOOHC
02442         MOVE 57 TO WS-TEMP-NOT-USED-CNT                           ELTCOOHC
02443         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOOHC
02444         ADD +1  TO  WS-CIA.                                       ELTCOOHC
02445 **                                                               |ELTCOOHC
02446 **---------------------------------------------------------------+ELTCOOHC
02447  7299-EXIT.    EXIT.                                              ELTCOOHC
02448                                                                   ELTCOOHC
02449 /                                                                 ELTCOOHC
02450  8000-OUTPUT-TEXT SECTION.                                        ELTCOOHC
02451       MOVE +0     TO COF-NBR-HDR-LINES.                           ELTCOOHC
02452       MOVE WS-CIA TO COF-NBR-DTL-LINES.                           ELTCOOHC
02453       MOVE ' '    TO  COF-FUNCTION.                               ELTCOOHC
02454       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTCOOHC
02455              COMMAREA(DFHCOMMAREA)                                ELTCOOHC
02456       END-EXEC.                                                   ELTCOOHC
02457       MOVE 1  TO  WS-CIA.                                         ELTCOOHC
02458  8099-EXIT.   EXIT.                                               ELTCOOHC
02459                                                                   ELTCOOHC
02460      COPY ELSTCOMP.                                               ELTCOOHC
02461                                                                   ELTCOOHC
02462 /              A B E N D                                          ELTCOOHC
02463 ******************************************************************ELTCOOHC
02464 *                        A B E N D                                ELTCOOHC
02465 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTCOOHC
02466 *                                                                 ELTCOOHC
02467 ******************************************************************ELTCOOHC
02468  9999-ABEND SECTION.                                              ELTCOOHC
02469                                                                   ELTCOOHC
02470      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTCOOHC
02471                                                                   ELTCOOHC
02472  9999-EXIT.     EXIT.                                             ELTCOOHC
