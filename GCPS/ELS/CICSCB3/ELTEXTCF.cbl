00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTEXTCF.                                            ELTEXTCF
00003  AUTHOR. JOHN CURIN - KEANE.                                         LV002
00004  DATE-WRITTEN.   5/20/86.                                         ELTEXTCF
00005  DATE-COMPILED.                                                   ELTEXTCF
00006      SKIP3                                                        ELTEXTCF
00007 ******************************************************************ELTEXTCF
00008 *@>ELTEXTCF                                                       ELTEXTCF
00009 *@¬                                                               ELTEXTCF
00010 *                        PROGRAM ABSTRACT                         ELTEXTCF
00011 *                                                                 ELTEXTCF
00012 *@¬ PROGRAM NAME:   E.L.S. EXTENDED CARE FACILITIES TOPIC         ELTEXTCF
00013 *@¬                                                               ELTEXTCF
00014 *@¬ PROGRAM I.D.:   ELTEXTCF                                      ELTEXTCF
00015 *@¬                                                               ELTEXTCF
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTEXTCF
00017 *@¬            EXTENDED CARE FACILITIES                           ELTEXTCF
00018 *@¬            BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTEXTCF
00019 *@¬                                                               ELTEXTCF
00020 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF EXTENDED CARE     ELTEXTCF
00021 *@¬            SERVICES AFFORDED A MEMBER BY HIS GROUP.           ELTEXTCF
00022 *@¬            THIS INFORMATION IS GOTTEN BY INTEROGATING THE     ELTEXTCF
00023 *@¬            BENEFIT PROVISIONS FOR THE GROUP WITHIN THE        ELTEXTCF
00024 *@¬            CONTRACT FOR A PARTICULAR RANGE OF DATES.          ELTEXTCF
00025 *@¬                                                               ELTEXTCF
00026 *@¬ RECORDS                                                       ELTEXTCF
00027 *@¬ ACCESSED:  CONTRACT, GROUP SPECIFIC, VARIOUS BENEFIT          ELTEXTCF
00028 *@¬            PROVISION, AND A LARGE NUMBER OF DATA ELEMENT      ELTEXTCF
00029 *@¬            AND CODE VALUE RECORDS.                            ELTEXTCF
00030 *@¬                                                               ELTEXTCF
00031 *@¬                                                               ELTEXTCF
00032            TITLE ' PROGRAM HISTORY'.                              ELTEXTCF
00033 *@¬                                                               ELTEXTCF
00034 *@¬ 07/24/86 JTC  CHANGED THE PICTURE OF WS-DTL-MAX-AMOUNT        ELTEXTCF
00035 *@¬               FROM $$$9 TO ZZ9.99-.                           ELTEXTCF
00036 *@¬               ALSO CHANGED THE DISPLAY OF THE MAXIMUM AMOUNT. ELTEXTCF
00037 *@¬               THERE WAS AN ERROR.  A SUPPLEMENTAL MAX AMOUNT, ELTEXTCF
00038 *@¬               WOULD HAVE BEEN DISPLAYED AS A BASIC MAX AMOUNT.ELTEXTCF
00039 *@¬                                                               ELTEXTCF
00040 *@¬               ALSO REMOVED THE UNNEEDED REFERENCES TO FORMAT  ELTEXTCF
00041 *@¬               E FIELDS                                        ELTEXTCF
00042 *@¬                                                               ELTEXTCF
00043 *@¬ 08/13/86 JTC  REVISED THE PER DIEM PROCESSING OF THE          ELTEXTCF
00044 *@¬               PROVISION PRICING METHOD FIELD.  ALSO, REMOVED  ELTEXTCF
00045 *@¬               THE SETTING UP OF THE FIRST HEADER LINE         ELTEXTCF
00046 *@¬               WS-HDR-1                                        ELTEXTCF
00047 *@¬                                                               ELTEXTCF
00048 *@¬ 10/09/86 JTC  VS COBOL II CONVERSION                          ELTEXTCF
00049 *@¬                                                               ELTEXTCF
00050 *@¬  11/15/86 LET  CHANGE CODE TO ACCOMADATE THE MOVING  OF       ELTEXTCF
00051 *@¬                 CERTIFICATION REQUIREMENT FROM THE FROMAT     ELTEXTCF
00052 *@¬                 TYPE SECTION TO THE COMMON SECTION.           ELTEXTCF
00053 *@¬                                                               ELTEXTCF
00054 *@¬ 10/20/87  AKK  CHANGED 'THIS GROUP OF BENEFITS ARE HANDLED    ELTEXTCF
00055 *@¬                AS FOLLOWS' TO 'COVERED SERVICES ARE'.         ELTEXTCF
00056 *@¬                                                               ELTEXTCF
00057 *@¬ 04/21/88 REB   SETTING A SWITCH SO THAT ELGCOVER WILL DO      ELTEXTCF
00058 *@¬                SPECIAL PROCESSING TO DETERMINE IF ECF         ELTEXTCF
00059 *@¬                PROVIDER CODES ARE CODED FOR THE #PVE TABULARS ELTEXTCF
00060 *@¬                FOR THE INST BENEFITS ('DRB  A', 'PVTR A')     ELTEXTCF
00061 *@¬                                                               ELTEXTCF
00062 *@¬ 03/13/89 GEM   MADE CHANGES TO ACCOMDATE STORAGE STORAGE      ELTEXTCF
00063 *@¬                MANAGEMENT ENHANCEMENTS.                       ELTEXTCF
00064 *@¬                                                               ELTEXTCF
00065 *@¬ 10/10/89 RKH   ADDED TRANSFER TO OTHER RESPONSIBILITY IND     ELTEXTCF
00066 *@¬                                                               ELTEXTCF
00067 *@¬ 11/08/90 GEM   ADDED ANCILLARY MESSAGE TO SCREEN DISPLAY      ELTEXTCF
00068 *@¬                                                               ELTEXTCF
00069 *@¬ 11/15/90 GEM   CHANGED PLP-TRANSF-OTHER-RESP-IND COMPARE TO   ELTEXTCF
00070 *@¬                THE LITERAL ZERO, INSTEAD OF THE DIGIT '0' DO  ELTEXTCF
00071 *@¬                TO THE EXPANSION OF THE RDW GCBENPVC.          ELTEXTCF
00072 *@¬                                                               ELTEXTCF
00073 *@¬ 06/06/91 JPB   CHANGED BEN-MAX-VISITS-IND TO BEN-MAX-VISIT-   ELTEXTCF
00074 *@¬                IND FOR BPD FORMAT.                            ELTEXTCF
00075 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTEXTCF
00076 *@¬                                                               ELTEXTCF
00077 ***************************************************************** ELTEXTCF
00078        TITLE 'WORKING STORAGE SECTION'.                           ELTEXTCF
00079  ENVIRONMENT DIVISION.                                            ELTEXTCF
00080      SKIP3                                                        ELTEXTCF
00081  DATA DIVISION.                                                   ELTEXTCF
00082  WORKING-STORAGE SECTION.                                         ELTEXTCF
00083  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTEXTCF
00084      '***ELTEXTCF WS BEGINS***'.                                  ELTEXTCF
00085  01  WS-PARA-COMMENTS.                                            ELTEXTCF
00086    05  WS-PARA-ID1               PIC X(4) VALUE 'XXXX'.           ELTEXTCF
00087    05  WS-PARA-ID2               PIC X(4) VALUE 'XXXX'.           ELTEXTCF
00088                                                                   ELTEXTCF
00089  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTEXTCF
00090                                                                   ELTEXTCF
00091 *     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTEXTCF
00092  01  WS-WORK-FIELDS.                                              ELTEXTCF
00093      05  WS-CHAR-0                     PIC X.                     ELTEXTCF
00094      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTEXTCF
00095      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTEXTCF
00096      05  WS-DTL-CERT-REQ-ID            PIC X(79) VALUE SPACES.    ELTEXTCF
00097      05  WS-DTL-CERT-REQ-IND           PIC X(79) VALUE SPACES.    ELTEXTCF
00098      05  WS-HOLD1                      PIC X(10).                 ELTEXTCF
00099      05  WS-HOLD2                      PIC X(10).                 ELTEXTCF
00100      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTEXTCF
00101      05  WS-DISPLAY-CERT-REQ           PIC X.                     ELTEXTCF
00102      05  WS-DISPLAY-MAX-AMT-TEXT       PIC X.                     ELTEXTCF
00103      05  WS-DISPLAY-MAX-VISITS-TEXT    PIC X.                     ELTEXTCF
00104      05  WS-YES                        PIC X     VALUE 'Y'.       ELTEXTCF
00105      05  WS-NO                         PIC X     VALUE 'N'.       ELTEXTCF
00106      05  WS-4096                       PIC S9(8) VALUE +4096.     ELTEXTCF
00107      05  WS-PER-DIEM                   PIC $$$$$9.99.             ELTEXTCF
00108      05  WS-ALLOW                      PIC $$$9.99.               ELTEXTCF
00109      05  WS-DTL-MAX-AMOUNT             PIC ZZ9.99-.               ELTEXTCF
00110      05  WS-DTL-MAX-DAYS               PIC ZZ9.                   ELTEXTCF
00111      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTEXTCF
00112      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTEXTCF
00113      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTEXTCF
00114      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTEXTCF
00115      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTEXTCF
00116      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTEXTCF
00117      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTEXTCF
00118      05  WS-FIRSTTIME-IND              PIC X.                     ELTEXTCF
00119        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTEXTCF
00120      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTEXTCF
00121        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTEXTCF
00122      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTEXTCF
00123        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTEXTCF
00124      05  WS-INDENT-IND                 PIC X.                     ELTEXTCF
00125        88  WS-INDENT-ON                    VALUE 'Y'.             ELTEXTCF
00126      05  WS-INDENT-FOUR-IND            PIC X.                     ELTEXTCF
00127        88  WS-INDENT-FOUR-ON               VALUE 'Y'.             ELTEXTCF
00128      05  WS-TEST-FOR-PER-DIEM          PIC XX.                    ELTEXTCF
00129        88  FLAT-RATE                       VALUE '04'.            ELTEXTCF
00130        88  FLAT-RATE-PLUS-PERCENT          VALUES ARE '14', '21', ELTEXTCF
00131                                                       '22'.       ELTEXTCF
00132                                                                   ELTEXTCF
00133      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTEXTCF
00134      05  WS-PRCNT-PERDM-ALLOW          PIC X(9).                  ELTEXTCF
00135      05  WS-PERCENT-FLD.                                          ELTEXTCF
00136        10  WS-PERCENTAGE               PIC ZZ9.                   ELTEXTCF
00137        10  WS-PERCENT-SIGN             PIC X.                     ELTEXTCF
00138                                                                   ELTEXTCF
00139        TITLE 'BEN PROV IDS BY TYPE'.                              ELTEXTCF
00140  01  WS-BEN-PROV-ID.                                              ELTEXTCF
00141      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +05.ELTEXTCF
00142      05  WS-INST-IP-CNT                PIC S9(4) COMP   VALUE +02.ELTEXTCF
00143      05  WS-INST-IP-TAB.                                          ELTEXTCF
00144        10  FILLER                      PIC X(6)  VALUE 'DRB  A'.  ELTEXTCF
00145        10  FILLER                      PIC X(6)  VALUE 'PVTR A'.  ELTEXTCF
00146      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTEXTCF
00147                                        PIC X(6)  OCCURS 2 TIMES.  ELTEXTCF
00148                                                                   ELTEXTCF
00149      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +05.ELTEXTCF
00150      05  WS-PROF-IP-TAB.                                          ELTEXTCF
00151        10  FILLER                      PIC X(6)  VALUE 'ECFV D'.  ELTEXTCF
00152        10  FILLER                      PIC X(6)  VALUE 'ECFA D'.  ELTEXTCF
00153        10  FILLER                      PIC X(6)  VALUE 'ECFD D'.  ELTEXTCF
00154        10  FILLER                      PIC X(6)  VALUE 'ECFM D'.  ELTEXTCF
00155        10  FILLER                      PIC X(6)  VALUE 'ECFT D'.  ELTEXTCF
00156      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTEXTCF
00157                                        PIC X(6)  OCCURS 5 TIMES.  ELTEXTCF
00158                                                                   ELTEXTCF
00159        TITLE ' D I S P L A Y   L I N E S '.                       ELTEXTCF
00160  01  WS-ELS-DISPLAY-LINES.                                        ELTEXTCF
00161    05  WS-HDR-2-PROF-IP.                                          ELTEXTCF
00162      10  FILLER                    PIC X(16) VALUE SPACES.        ELTEXTCF
00163      10  FILLER                    PIC X(44)                      ELTEXTCF
00164          VALUE 'EXTENDED CARE FACILITY SERVICES PROFESSIONAL'.    ELTEXTCF
00165      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTEXTCF
00166                                                                   ELTEXTCF
00167    05  WS-HDR-2-INST-IP.                                          ELTEXTCF
00168      10  FILLER                    PIC X(16) VALUE SPACES.        ELTEXTCF
00169      10  FILLER                    PIC X(45)                      ELTEXTCF
00170          VALUE 'EXTENDED CARE FACILITY SERVICES INSTITUTIONAL'.   ELTEXTCF
00171      10  FILLER                    PIC X(18) VALUE LOW-VALUES.    ELTEXTCF
00172                                                                   ELTEXTCF
00173    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTEXTCF
00174          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTEXTCF
00175                                                                   ELTEXTCF
00176    05  WS-FOLLOWING-BEN.                                          ELTEXTCF
00177      10  FILLER                    PIC X(79) VALUE                ELTEXTCF
00178          'COVERED SERVICES ARE:'.                                 ELTEXTCF
00179                                                                   ELTEXTCF
00180    05  WS-PAY-CONSDR-TEXT1.                                       ELTEXTCF
00181      10  FILLER                    PIC X(45)                      ELTEXTCF
00182        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTEXTCF
00183                                                                   ELTEXTCF
00184    05  WS-ANCILLARY-TEXT1.                                        ELTEXTCF
00185      10  FILLER                    PIC X(32)                      ELTEXTCF
00186        VALUE  'SEE THE BENEFIT LEVEL TOPICS FOR'.                 ELTEXTCF
00187                                                                   ELTEXTCF
00188    05  WS-ANCILLARY-TEXT2.                                        ELTEXTCF
00189      10  FILLER                    PIC X(31)                      ELTEXTCF
00190        VALUE  'ANCILLARY SERVICES ELIGIBILITY.'.                  ELTEXTCF
00191                                                                   ELTEXTCF
00192    05  WS-PAY-CONSDR-TEXT2.                                       ELTEXTCF
00193      10  FILLER                    PIC X(44)                      ELTEXTCF
00194        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTEXTCF
00195                                                                   ELTEXTCF
00196    05  WS-PAYMNT-BASED.                                           ELTEXTCF
00197      10  FILLER                    PIC X(20)                      ELTEXTCF
00198          VALUE 'PAYMENT IS BASED ON:'.                            ELTEXTCF
00199      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTEXTCF
00200                                                                   ELTEXTCF
00201    05  WS-BASIC.                                                  ELTEXTCF
00202      10  WS-BASIC-LIT              PIC X(16)                      ELTEXTCF
00203          VALUE '         BASIC: '.                                ELTEXTCF
00204      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTEXTCF
00205                                                                   ELTEXTCF
00206    05  WS-SUPPLEMENTAL.                                           ELTEXTCF
00207      10  WS-SUPP-LIT               PIC X(16)                      ELTEXTCF
00208          VALUE '  SUPPLEMENTAL: '.                                ELTEXTCF
00209      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTEXTCF
00210                                                                   ELTEXTCF
00211    05  WS-MAX-LEAVES.                                             ELTEXTCF
00212      10  FILLER                    PIC X(44) VALUE                ELTEXTCF
00213          'THE MAXIMUM NUMBER OF LEAVES PER ADMISSION: '.          ELTEXTCF
00214      10  WS-DTL-MAX-LEAVES         PIC ZZ9.                       ELTEXTCF
00215                                                                   ELTEXTCF
00216    05  WS-INDENTED.                                               ELTEXTCF
00217      10  FILLER                    PIC X(16) VALUE SPACES.        ELTEXTCF
00218      10  WS-DTL-INDENTED           PIC X(63) VALUE SPACES.        ELTEXTCF
00219                                                                   ELTEXTCF
00220    05  WS-INDENTED-FOUR.                                          ELTEXTCF
00221      10  FILLER                    PIC X(04) VALUE SPACES.        ELTEXTCF
00222      10  WS-DTL-INDENTED-FOUR      PIC X(75) VALUE SPACES.        ELTEXTCF
00223                                                                   ELTEXTCF
00224    05  WS-PAYABLE-AS.                                             ELTEXTCF
00225      10  FILLER                    PIC X(45) VALUE                ELTEXTCF
00226          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTEXTCF
00227      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTEXTCF
00228                                                                   ELTEXTCF
00229    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTEXTCF
00230        'SPILLOVER COINSURANCE: '.                                 ELTEXTCF
00231                                                                   ELTEXTCF
00232    05  WS-SPILLOVER-DEDUCT         PIC X(22)  VALUE               ELTEXTCF
00233        'SPILLOVER DEDUCTIBLE: '.                                  ELTEXTCF
00234                                                                   ELTEXTCF
00235    05  WS-SPILLOVER-FL-RT-PER-D    PIC X(30)  VALUE               ELTEXTCF
00236        'SPILLOVER FLAT RATE PER DIEM: '.                          ELTEXTCF
00237                                                                   ELTEXTCF
00238    05  WS-MAX-VISITS               PIC X(34)  VALUE               ELTEXTCF
00239        'THE MAXIMUM NUMBER OF VISITS ARE: '.                      ELTEXTCF
00240                                                                   ELTEXTCF
00241    05  WS-MAX-AMOUNT               PIC X(42)  VALUE               ELTEXTCF
00242        'THE MAXIMUM AMOUNT ELIGIBLE PER VISIT IS: '.              ELTEXTCF
00243                                                                   ELTEXTCF
00244    05  WS-SECONDARY                PIC X(10) VALUE 'SECONDARY:'.  ELTEXTCF
00245                                                                   ELTEXTCF
00246    05  WS-DAYS-REDUCED             PIC X(22) VALUE                ELTEXTCF
00247          ' DAYS REDUCTION RATIO '.                                ELTEXTCF
00248                                                                   ELTEXTCF
00249    05  WS-PER                      PIC X(03) VALUE 'PER'.         ELTEXTCF
00250                                                                   ELTEXTCF
00251    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTEXTCF
00252                                                                   ELTEXTCF
00253    05  WS-PRIOR-ADM-REQUIREMENT    PIC X(56)  VALUE               ELTEXTCF
00254        'EXTENDED CARE FACILITY PRIOR ADMISSION REQUIREMENT IS: '. ELTEXTCF
00255                                                                   ELTEXTCF
00256    05  WS-CERT-REQUIREMENT         PIC X(26)  VALUE               ELTEXTCF
00257        'CERTIFICATION REQUIREMENT:'.                              ELTEXTCF
00258                                                                   ELTEXTCF
00259    05  WS-CONTRACT-RELATED.                                       ELTEXTCF
00260      10  FILLER                    PIC X(49)                      ELTEXTCF
00261        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTEXTCF
00262      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTEXTCF
00263                                                                   ELTEXTCF
00264    05  WS-PVE-TEXT.                                               ELTEXTCF
00265      10  FILLER                    PIC X(44)                      ELTEXTCF
00266        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTEXTCF
00267      10  FILLER                    PIC X(35) VALUE LOW-VALUES.    ELTEXTCF
00268                                                                   ELTEXTCF
00269    05  WS-FLAT-RATE-IS             PIC X(17)  VALUE               ELTEXTCF
00270        'THE FLAT RATE IS '.                                       ELTEXTCF
00271    05  WS-PERCENT-REMAINDER        PIC X(23)  VALUE               ELTEXTCF
00272        'THE % ON REMAINDER IS '.                                  ELTEXTCF
00273    05  WS-NO-TABULAR1.                                            ELTEXTCF
00274      10  FILLER                    PIC X(51)  VALUE               ELTEXTCF
00275         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTEXTCF
00276      10  FILLER                    PIC X(22)  VALUE               ELTEXTCF
00277         'GOING FROM BENEFIT ***'.                                 ELTEXTCF
00278                                                                   ELTEXTCF
00279    05  WS-NO-TABULAR2.                                            ELTEXTCF
00280      10  FILLER                    PIC X(15)  VALUE               ELTEXTCF
00281         '*** PROVISION: '.                                        ELTEXTCF
00282      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTEXTCF
00283      10  FILLER                    PIC X VALUE SPACE.             ELTEXTCF
00284      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTEXTCF
00285      10  FILLER                    PIC X(13)  VALUE               ELTEXTCF
00286         ' TO TABULAR: '.                                          ELTEXTCF
00287      10  WS-NO-TAB-ID              PIC X(6).                      ELTEXTCF
00288      10  FILLER                    PIC X VALUE SPACE.             ELTEXTCF
00289      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTEXTCF
00290      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTEXTCF
00291                                                                   ELTEXTCF
00292    05  WS-PGM-ERROR.                                              ELTEXTCF
00293      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTEXTCF
00294      10  FILLER                    PIC X(35)  VALUE               ELTEXTCF
00295         '***  P R O G R A M   E R R O R  ***'.                    ELTEXTCF
00296      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTEXTCF
00297                                                                   ELTEXTCF
00298    05  WS-BAD-INST-PROF-SEL.                                      ELTEXTCF
00299      10  FILLER                    PIC XX VALUE SPACE.            ELTEXTCF
00300      10  FILLER                    PIC X(47) VALUE                ELTEXTCF
00301         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTEXTCF
00302      10  FILLER                    PIC X(48) VALUE                ELTEXTCF
00303         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTEXTCF
00304      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTEXTCF
00305                                                                   ELTEXTCF
00306    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTEXTCF
00307       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTEXTCF
00308                                                                   ELTEXTCF
00309    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTEXTCF
00310    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTEXTCF
00311      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTEXTCF
00312                                                                   ELTEXTCF
00313  01  WS-END                            PIC X(16)  VALUE           ELTEXTCF
00314      '*** W/S ENDS ***'.                                          ELTEXTCF
00315                                                                   ELTEXTCF
00316        TITLE ' L I N K A G E   S E C T I O N '.                   ELTEXTCF
00317  LINKAGE SECTION.                                                 ELTEXTCF
00318  01  DFHCOMMAREA.                                                 ELTEXTCF
00319      COPY ELSCOMMC.                                               ELTEXTCF
00320                                                                   ELTEXTCF
00321 /  *** CIA  AREA ***                                              ELTEXTCF
00322      COPY ELSCIA2C.                                               ELTEXTCF
00323 /  *** IO PARM AREA ***                                           ELTEXTCF
00324      COPY ELSIOPMC.                                               ELTEXTCF
00325 /  *** KEY AREA ***                                               ELTEXTCF
00326      COPY ELSKEYSC.                                               ELTEXTCF
00327 /  *** OUTPUT TEXT AREA ***                                       ELTEXTCF
00328      COPY ELSOUTPC.                                               ELTEXTCF
00329 /  *** TOPIC SELECTION AREA ***                                   ELTEXTCF
00330      COPY ELSSSCBC.                                               ELTEXTCF
00331 /  *** CODE MANUAL INTERFACE ***                                  ELTEXTCF
00332      COPY ELSCMIFC.                                               ELTEXTCF
00333 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTEXTCF
00334      COPY ELSCMDSC.                                               ELTEXTCF
00335 /  *** BENEFIT PROVISION TABLE ***                                ELTEXTCF
00336      COPY ELSPRVNC.                                               ELTEXTCF
00337 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTEXTCF
00338      COPY ELSTCWAC.                                               ELTEXTCF
00339 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTEXTCF
00340      COPY ELSPLGSW.                                               ELTEXTCF
00341                                                                   ELTEXTCF
00342 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTEXTCF
00343      COPY ELSPLGTB.                                               ELTEXTCF
00344 /        G R O U P   S P E C I F I C   R E C O R D                ELTEXTCF
00345  01  GROUP-SPECIFIC-RECORD.                                       ELTEXTCF
00346      COPY GCGROUPC.                                               ELTEXTCF
00347 /                  M A I N L I N E                                ELTEXTCF
00348  PROCEDURE DIVISION.                                              ELTEXTCF
00349                                                                   ELTEXTCF
00350 ******************************************************************ELTEXTCF
00351 *                                                                 ELTEXTCF
00352 *   PERFORM THE MAINLINE OPERATIONS.                              ELTEXTCF
00353 *                                                                 ELTEXTCF
00354 ******************************************************************ELTEXTCF
00355  0000-MAINLINE.                                                   ELTEXTCF
00356                                                                   ELTEXTCF
00357      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTEXTCF
00358         EXEC CICS ABEND                                           ELTEXTCF
00359                   ABCODE('EL01')                                  ELTEXTCF
00360         END-EXEC.                                                 ELTEXTCF
00361                                                                   ELTEXTCF
00362      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTEXTCF
00363          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTEXTCF
00364                                                                   ELTEXTCF
00365      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTEXTCF
00366      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00367          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTEXTCF
00368                                                                   ELTEXTCF
00369      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTEXTCF
00370      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00371          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTEXTCF
00372                                                                   ELTEXTCF
00373      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTEXTCF
00374      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00375          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTEXTCF
00376                                                                   ELTEXTCF
00377      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTEXTCF
00378      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00379          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTEXTCF
00380                                                                   ELTEXTCF
00381      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTEXTCF
00382      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00383          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTEXTCF
00384                                                                   ELTEXTCF
00385      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTEXTCF
00386      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00387          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTEXTCF
00388                                                                   ELTEXTCF
00389      MOVE '0'  TO  WS-CHAR-0.                                     ELTEXTCF
00390      MOVE 'N'  TO WS-INDENT-IND,                                  ELTEXTCF
00391                   WS-INDENT-FOUR-IND.                             ELTEXTCF
00392                                                                   ELTEXTCF
00393      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTEXTCF
00394                                                                   ELTEXTCF
00395      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTEXTCF
00396              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTEXTCF
00397                                                                   ELTEXTCF
00398      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTEXTCF
00399                                                                   ELTEXTCF
00400      SET CIA-STG-GETMAIN  TO TRUE.                                ELTEXTCF
00401                                                                   ELTEXTCF
00402      EXEC CICS LINK                                               ELTEXTCF
00403                PROGRAM('ELUSTGMG')                                ELTEXTCF
00404                COMMAREA(DFHCOMMAREA)                              ELTEXTCF
00405      END-EXEC.                                                    ELTEXTCF
00406                                                                   ELTEXTCF
00407      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTEXTCF
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00409          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTEXTCF
00410                                                                   ELTEXTCF
00411      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTEXTCF
00412      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00413          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTEXTCF
00414                                                                   ELTEXTCF
00415      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH)           ELTEXTCF
00416         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTEXTCF
00417                                                                   ELTEXTCF
00418      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH)            ELTEXTCF
00419         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTEXTCF
00420                                                                   ELTEXTCF
00421      IF NOT SSB-PROV-CLASS-INST AND    NOT SSB-PROV-CLASS-PROF    ELTEXTCF
00422                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTEXTCF
00423         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTEXTCF
00424         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTEXTCF
00425         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTEXTCF
00426         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTEXTCF
00427         MOVE SPACE  TO  COF-FUNCTION                              ELTEXTCF
00428         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEXTCF
00429               COMMAREA(DFHCOMMAREA)                               ELTEXTCF
00430         END-EXEC.                                                 ELTEXTCF
00431                                                                   ELTEXTCF
00432      MOVE 'E'  TO  COF-FUNCTION.                                  ELTEXTCF
00433      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEXTCF
00434                     COF-NBR-DTL-LINES.                            ELTEXTCF
00435      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00436      END-EXEC.                                                    ELTEXTCF
00437                                                                   ELTEXTCF
00438                                                                   ELTEXTCF
00439  0099-RETURN.                                                     ELTEXTCF
00440      EXEC CICS RETURN   END-EXEC.                                 ELTEXTCF
00441                                                                   ELTEXTCF
00442      GOBACK.                                                      ELTEXTCF
00443      TITLE 'INSTITUTIONAL INPATIENT'.                             ELTEXTCF
00444  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTEXTCF
00445 ***************************************************************** ELTEXTCF
00446 *        I N S T I T U T I O N A L   I P   R T N E                ELTEXTCF
00447 *                                                                 ELTEXTCF
00448 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEXTCF
00449 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEXTCF
00450 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEXTCF
00451 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEXTCF
00452 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEXTCF
00453 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEXTCF
00454 *  MODULE.                                                        ELTEXTCF
00455 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEXTCF
00456 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEXTCF
00457 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEXTCF
00458 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEXTCF
00459 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEXTCF
00460 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEXTCF
00461 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEXTCF
00462 *                                                                 ELTEXTCF
00463 ***************************************************************** ELTEXTCF
00464      MOVE '1000'  TO  WS-PARA-ID1.                                ELTEXTCF
00465                                                                   ELTEXTCF
00466      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEXTCF
00467      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEXTCF
00468      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEXTCF
00469                     COF-NBR-DTL-LINES.                            ELTEXTCF
00470      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00471      END-EXEC.                                                    ELTEXTCF
00472      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEXTCF
00473      MOVE WS-HDR-2-INST-IP  TO  COF-HDR-LINE(2).                  ELTEXTCF
00474                                                                   ELTEXTCF
00475      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTEXTCF
00476      PERFORM 1010-MOVE-IN-INST-IP                                 ELTEXTCF
00477         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEXTCF
00478         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTEXTCF
00479                                                                   ELTEXTCF
00480      GO TO 1020-CALL-COVERAGE.                                    ELTEXTCF
00481  1010-MOVE-IN-INST-IP.                                            ELTEXTCF
00482      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEXTCF
00483      MOVE WS-INST-IP-LIST(WS-SUB)  TO                             ELTEXTCF
00484                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEXTCF
00485      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEXTCF
00486                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEXTCF
00487                                                                   ELTEXTCF
00488  1020-CALL-COVERAGE.                                              ELTEXTCF
00489      MOVE '1020'  TO  WS-PARA-ID1.                                ELTEXTCF
00490      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEXTCF
00491      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00492      END-EXEC.                                                    ELTEXTCF
00493                                                                   ELTEXTCF
00494      MOVE 'EXTENDED CARE FACILITY SERVICES '                      ELTEXTCF
00495                TO SSB-TOPIC-PHRASE.                               ELTEXTCF
00496                                                                   ELTEXTCF
00497      SET  FIND-ECF-PROV-ON-PVE         TO TRUE.                   ELTEXTCF
00498                                                                   ELTEXTCF
00499      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEXTCF
00500      END-EXEC.                                                    ELTEXTCF
00501                                                                   ELTEXTCF
00502      MOVE COF-NBR-DTL-LINES     TO  WS-CIA.                       ELTEXTCF
00503      ADD  +1  TO  WS-CIA.                                         ELTEXTCF
00504      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTEXTCF
00505      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00506      END-EXEC.                                                    ELTEXTCF
00507                                                                   ELTEXTCF
00508      IF PVN-COVG-NONE                                             ELTEXTCF
00509         GO TO 1099-EXIT.                                          ELTEXTCF
00510                                                                   ELTEXTCF
00511      MOVE +1  TO  WS-CIA.                                         ELTEXTCF
00512                                                                   ELTEXTCF
00513      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTEXTCF
00514      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEXTCF
00515            PSP-PROVN-PRICING-METHD,                               ELTEXTCF
00516            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEXTCF
00517            PSP-TRANSF-OTHER-RESP-IND,                             ELTEXTCF
00518            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEXTCF
00519            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEXTCF
00520            PSP-SPILL-OVER-DED-APL-IND,                            ELTEXTCF
00521            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTEXTCF
00522            PSP-CERTFN-REQRM-IND,                                  ELTEXTCF
00523            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEXTCF
00524            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEXTCF
00525            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEXTCF
00526            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEXTCF
00527            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEXTCF
00528            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEXTCF
00529            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTEXTCF
00530            PSA-DAYS-RDCN-RAT-IND,                                 ELTEXTCF
00531            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTEXTCF
00532            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTEXTCF
00533            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTEXTCF
00534            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTEXTCF
00535            PSA-ECF-F-RAT-PER-DIEM-AMT.                            ELTEXTCF
00536                                                                   ELTEXTCF
00537      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEXTCF
00538      END-EXEC.                                                    ELTEXTCF
00539                                                                   ELTEXTCF
00540      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEXTCF
00541      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
00542          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTEXTCF
00543                                                                   ELTEXTCF
00544      PERFORM 1030-FIND-FIRST-NONZERO                              ELTEXTCF
00545         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEXTCF
00546         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTEXTCF
00547                                                                   ELTEXTCF
00548      GO TO 1099-EXIT.                                             ELTEXTCF
00549  1030-FIND-FIRST-NONZERO.                                         ELTEXTCF
00550      SET PVN-BEN-PROVN-IDX, PLT-INDEX1 TO WS-SUB.                 ELTEXTCF
00551      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEXTCF
00552         NEXT SENTENCE                                             ELTEXTCF
00553      ELSE                                                         ELTEXTCF
00554         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTEXTCF
00555                                                                   ELTEXTCF
00556  1040-BUILD-SCREEN-LINES.                                         ELTEXTCF
00557      MOVE '1040'  TO  WS-PARA-ID1.                                ELTEXTCF
00558                                                                   ELTEXTCF
00559      SET PLT-INDEX1  TO                                           ELTEXTCF
00560                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTEXTCF
00561      IF WS-NOT-FIRST-TIME                                         ELTEXTCF
00562         MOVE 'P'  TO  COF-FUNCTION                                ELTEXTCF
00563         MOVE +0   TO  COF-NBR-DTL-LINES                           ELTEXTCF
00564         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEXTCF
00565             COMMAREA(DFHCOMMAREA)                                 ELTEXTCF
00566         END-EXEC                                                  ELTEXTCF
00567      ELSE                                                         ELTEXTCF
00568         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEXTCF
00569                                                                   ELTEXTCF
00570      MOVE 1  TO  WS-CIA.                                          ELTEXTCF
00571      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEXTCF
00572         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEXTCF
00573            SET PLT-INDEX2  TO  2                                  ELTEXTCF
00574         ELSE                                                      ELTEXTCF
00575            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTEXTCF
00576            GO TO 1099-EXIT                                        ELTEXTCF
00577      ELSE                                                         ELTEXTCF
00578         SET PLT-INDEX2  TO  1.                                    ELTEXTCF
00579                                                                   ELTEXTCF
00580 **---------------------------------------------------------------+ELTEXTCF
00581 **                                                               |ELTEXTCF
00582 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEXTCF
00583      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEXTCF
00584      ADD  +1  TO  WS-CIA.                                         ELTEXTCF
00585      MOVE ZERO  TO  WS-SUB2.                                      ELTEXTCF
00586      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEXTCF
00587      MOVE '1050'  TO  WS-PARA-ID1.                                ELTEXTCF
00588                                                                   ELTEXTCF
00589      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTEXTCF
00590         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTEXTCF
00591         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.                ELTEXTCF
00592      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEXTCF
00593                                                                   ELTEXTCF
00594      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEXTCF
00595      MOVE +1  TO  WS-CIA                                          ELTEXTCF
00596      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00597      END-EXEC.                                                    ELTEXTCF
00598 **                                                               |ELTEXTCF
00599 **---------------------------------------------------------------+ELTEXTCF
00600                                                                   ELTEXTCF
00601      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
00602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00603        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
00604               NOT = ZERO                                          ELTEXTCF
00605         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTEXTCF
00606         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
00607         ADD +1  TO  WS-CIA.                                       ELTEXTCF
00608                                                                   ELTEXTCF
00609      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
00610      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00611       AND  NOT WS-ADD-A-BLANK-LINE                                ELTEXTCF
00612        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
00613               NOT = ZERO                                          ELTEXTCF
00614         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTEXTCF
00615         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
00616         ADD +1  TO  WS-CIA.                                       ELTEXTCF
00617                                                                   ELTEXTCF
00618      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00619             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTEXTCF
00620                                                                   ELTEXTCF
00621      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00622             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTEXTCF
00623                                                                   ELTEXTCF
00624      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
00625            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTEXTCF
00626            ADD  +1   TO  WS-CIA                                   ELTEXTCF
00627            PERFORM 8000-OUTPUT-TEXT.                              ELTEXTCF
00628                                                                   ELTEXTCF
00629 **---------------------------------------------------------------+ELTEXTCF
00630 **                                                               |ELTEXTCF
00631 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEXTCF
00632 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEXTCF
00633 **     A D D I T I O N A L   P R I C I N G   P E R C E N T   O R |ELTEXTCF
00634 **     F L A T  R A T E  P E R  D I E M                      O R |ELTEXTCF
00635 **     A D D I T I O N A L   A L L O W A N C E  A M O U N T      |ELTEXTCF
00636      MOVE ZEROS       TO WS-PER-DIEM,                             ELTEXTCF
00637                          WS-PERCENTAGE.                           ELTEXTCF
00638      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
00639      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
00640         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
00641                                                              '19' ELTEXTCF
00642         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEXTCF
00643         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
00644         ADD +1  TO  WS-CIA.                                       ELTEXTCF
00645                                                                   ELTEXTCF
00646      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
00647      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEXTCF
00648         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
00649                                                        '19' AND   ELTEXTCF
00650         NOT WS-ADD-A-BLANK-LINE                                   ELTEXTCF
00651         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEXTCF
00652         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
00653         ADD +1  TO  WS-CIA.                                       ELTEXTCF
00654                                                                   ELTEXTCF
00655      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
00656      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
00657         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEXTCF
00658                            AND                                    ELTEXTCF
00659         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00660         SET  PLT-INDEX2  TO  2                                    ELTEXTCF
00661         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEXTCF
00662                                                             ZERO  ELTEXTCF
00663            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEXTCF
00664            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEXTCF
00665            ADD +1  TO  WS-CIA.                                    ELTEXTCF
00666                                                                   ELTEXTCF
00667      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
00668      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
00669         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEXTCF
00670                            AND                                    ELTEXTCF
00671         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTEXTCF
00672         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEXTCF
00673         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEXTCF
00674         ADD +1  TO  WS-CIA.                                       ELTEXTCF
00675                                                                   ELTEXTCF
00676      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEXTCF
00677         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00678         SET  PLT-INDEX2  TO  2                                    ELTEXTCF
00679         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEXTCF
00680                                                             ZERO  ELTEXTCF
00681            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEXTCF
00682            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEXTCF
00683            ADD +1  TO  WS-CIA.                                    ELTEXTCF
00684                                                                   ELTEXTCF
00685      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
00686      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00687         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
00688                                                            =  ZEROELTEXTCF
00689            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00690                                                            =  ZEROELTEXTCF
00691               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTEXTCF
00692            ELSE                                                   ELTEXTCF
00693               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEXTCF
00694          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00695                                                  TO  WS-PERCENTAGEELTEXTCF
00696          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTEXTCF
00697         ELSE                                                      ELTEXTCF
00698          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTEXTCF
00699          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00700                                                 TO  WS-PERCENTAGE ELTEXTCF
00701          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTEXTCF
00702                                                                   ELTEXTCF
00703      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00704       IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00705                                                            =  ZEROELTEXTCF
00706        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTEXTCF
00707                                                            =  ZEROELTEXTCF
00708         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00709                                                            =  ZEROELTEXTCF
00710            IF PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00711                                                            =  ZEROELTEXTCF
00712               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTEXTCF
00713            ELSE                                                   ELTEXTCF
00714              NEXT SENTENCE                                        ELTEXTCF
00715         ELSE                                                      ELTEXTCF
00716          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00717                                                 TO  WS-ALLOW      ELTEXTCF
00718          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTEXTCF
00719                                                                   ELTEXTCF
00720      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00721       IF PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
00722                                                     NOT =  ZERO   ELTEXTCF
00723         MOVE                                                      ELTEXTCF
00724           PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)      ELTEXTCF
00725                                               TO  WS-PER-DIEM.    ELTEXTCF
00726                                                                   ELTEXTCF
00727      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
00728         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
00729                                             ZERO AND  NOT =  '19' ELTEXTCF
00730         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
00731         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEXTCF
00732         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEXTCF
00733                                               CMF-CODE-VALUE      ELTEXTCF
00734                                               WS-TEST-FOR-PER-DIEMELTEXTCF
00735         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEXTCF
00736         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
00737         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTEXTCF
00738                                                                   ELTEXTCF
00739      MOVE ZEROS       TO WS-PER-DIEM,                             ELTEXTCF
00740                          WS-PERCENTAGE.                           ELTEXTCF
00741      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
00742      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00743         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEXTCF
00744                                                               ZEROELTEXTCF
00745            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00746                                                            =  ZEROELTEXTCF
00747               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTEXTCF
00748            ELSE                                                   ELTEXTCF
00749               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEXTCF
00750          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00751                                                  TO  WS-PERCENTAGEELTEXTCF
00752          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTEXTCF
00753         ELSE                                                      ELTEXTCF
00754            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEXTCF
00755          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00756                                                 TO  WS-PERCENTAGE ELTEXTCF
00757          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTEXTCF
00758                                                                   ELTEXTCF
00759      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00760       IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00761                                                            =  ZEROELTEXTCF
00762        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTEXTCF
00763                                                            =  ZEROELTEXTCF
00764         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00765                                                            =  ZEROELTEXTCF
00766            IF PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00767                                                            =  ZEROELTEXTCF
00768               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTEXTCF
00769            ELSE                                                   ELTEXTCF
00770             NEXT SENTENCE                                         ELTEXTCF
00771         ELSE                                                      ELTEXTCF
00772          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00773                                                 TO  WS-ALLOW      ELTEXTCF
00774          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTEXTCF
00775                                                                   ELTEXTCF
00776      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00777       IF PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
00778                                                  NOT =  ZERO      ELTEXTCF
00779         MOVE                                                      ELTEXTCF
00780            PLA-ECF-F-RAT-PER-DIEM-AMT(PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00781                                               TO  WS-PER-DIEM.    ELTEXTCF
00782                                                                   ELTEXTCF
00783      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEXTCF
00784         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
00785                                             ZERO AND  NOT =  '19' ELTEXTCF
00786         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
00787         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEXTCF
00788         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEXTCF
00789                                               CMF-CODE-VALUE      ELTEXTCF
00790                                               WS-TEST-FOR-PER-DIEMELTEXTCF
00791         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEXTCF
00792         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
00793         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTEXTCF
00794                                                                   ELTEXTCF
00795      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
00796         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
00797         ADD  +1   TO  WS-CIA                                      ELTEXTCF
00798         PERFORM 8000-OUTPUT-TEXT                                  ELTEXTCF
00799      ELSE                                                         ELTEXTCF
00800       PERFORM 8000-OUTPUT-TEXT.                                   ELTEXTCF
00801 **                                                               |ELTEXTCF
00802 **---------------------------------------------------------------+ELTEXTCF
00803                                                                   ELTEXTCF
00804      MOVE WS-NO TO WS-DISPLAY-CERT-REQ.                           ELTEXTCF
00805                                                                   ELTEXTCF
00806      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00807        SET  PLT-INDEX2          TO  1                             ELTEXTCF
00808        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
00809                          NOT = ZEROES                             ELTEXTCF
00810                           MOVE WS-YES TO WS-DISPLAY-CERT-REQ.     ELTEXTCF
00811                                                                   ELTEXTCF
00812      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00813        SET  PLT-INDEX2          TO  2                             ELTEXTCF
00814        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
00815                     NOT = ZEROES                                  ELTEXTCF
00816                           MOVE WS-YES TO WS-DISPLAY-CERT-REQ.     ELTEXTCF
00817                                                                   ELTEXTCF
00818      IF WS-DISPLAY-CERT-REQ = WS-YES                              ELTEXTCF
00819         ADD +1                   TO WS-CIA                        ELTEXTCF
00820         MOVE WS-CERT-REQUIREMENT TO COF-DTL-LINE(WS-CIA)          ELTEXTCF
00821         ADD +1                   TO WS-CIA.                       ELTEXTCF
00822                                                                   ELTEXTCF
00823      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00824        SET  PLT-INDEX2          TO  1                             ELTEXTCF
00825        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
00826                          NOT = ZEROES                             ELTEXTCF
00827                        PERFORM 5500-CERTIFICATION-REQ.            ELTEXTCF
00828                                                                   ELTEXTCF
00829      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00830        SET  PLT-INDEX2          TO  2                             ELTEXTCF
00831        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
00832                     NOT = ZEROES                                  ELTEXTCF
00833                        PERFORM 5500-CERTIFICATION-REQ.            ELTEXTCF
00834                                                                   ELTEXTCF
00835      IF WS-DISPLAY-CERT-REQ = WS-YES                              ELTEXTCF
00836         PERFORM 8000-OUTPUT-TEXT.                                 ELTEXTCF
00837 **---------------------------------------------------------------+ELTEXTCF
00838 **                                                               |ELTEXTCF
00839 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTEXTCF
00840                                                                   ELTEXTCF
00841      SET  PLT-INDEX2          TO  1.                              ELTEXTCF
00842      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00843        IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
00844                     NOT = '0'                                     ELTEXTCF
00845         IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
00846                     NOT = LOW-VALUES                              ELTEXTCF
00847          ADD +1                   TO WS-CIA                       ELTEXTCF
00848          MOVE 'BPA'               TO  CMF-RECORD-PREFIX           ELTEXTCF
00849          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTEXTCF
00850          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
00851                                   TO  CMF-CODE-VALUE              ELTEXTCF
00852          MOVE WS-YES              TO WS-ADD-A-BLANK-IND           ELTEXTCF
00853          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTEXTCF
00854                                                                   ELTEXTCF
00855      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00856        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
00857                     NOT = ZEROS  AND                              ELTEXTCF
00858            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00859                      NOT = ZEROS                                  ELTEXTCF
00860             MOVE                                                  ELTEXTCF
00861              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTEXTCF
00862                    TO WS-DTL-DAYS-REDUCED-APL                     ELTEXTCF
00863             MOVE                                                  ELTEXTCF
00864              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTEXTCF
00865                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTEXTCF
00866             MOVE SPACES          TO TCAR-FROM-AREA                ELTEXTCF
00867             STRING WS-DAYS-REDUCED,                               ELTEXTCF
00868                    WS-BASIC-LIT,                                  ELTEXTCF
00869                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTEXTCF
00870                    WS-FOR, ' '                                    ELTEXTCF
00871                    WS-DTL-DAYS-REDUCED-BASE,                      ELTEXTCF
00872                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTEXTCF
00873             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTEXTCF
00874             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTEXTCF
00875             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTEXTCF
00876             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTEXTCF
00877             PERFORM TCPR-000-TEXT-UNSTRING                        ELTEXTCF
00878             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTEXTCF
00879             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTEXTCF
00880                                                                   ELTEXTCF
00881      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00882        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
00883                     NOT = ZEROS  AND                              ELTEXTCF
00884            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00885                     NOT  = ZEROS                                  ELTEXTCF
00886                              ADD +1  TO  WS-CIA.                  ELTEXTCF
00887                                                                   ELTEXTCF
00888      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
00889        IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
00890                     NOT = ZEROS  AND                              ELTEXTCF
00891            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTEXTCF
00892                      NOT = ZEROS                                  ELTEXTCF
00893             MOVE                                                  ELTEXTCF
00894              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
00895                    TO WS-DTL-DAYS-REDUCED-APL                     ELTEXTCF
00896             MOVE                                                  ELTEXTCF
00897              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00898                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTEXTCF
00899             MOVE SPACES          TO TCAR-FROM-AREA                ELTEXTCF
00900             STRING WS-DAYS-REDUCED,                               ELTEXTCF
00901                    WS-SECONDARY, ' '                              ELTEXTCF
00902                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTEXTCF
00903                    WS-FOR, ' '                                    ELTEXTCF
00904                    WS-DTL-DAYS-REDUCED-BASE,                      ELTEXTCF
00905                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTEXTCF
00906             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTEXTCF
00907             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTEXTCF
00908             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTEXTCF
00909             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTEXTCF
00910             PERFORM TCPR-000-TEXT-UNSTRING                        ELTEXTCF
00911             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTEXTCF
00912             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTEXTCF
00913                                                                   ELTEXTCF
00914      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
00915          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTEXTCF
00916          ADD +1  TO  WS-CIA                                       ELTEXTCF
00917          PERFORM 8000-OUTPUT-TEXT.                                ELTEXTCF
00918 **                                                               |ELTEXTCF
00919 **---------------------------------------------------------------+ELTEXTCF
00920                                                                   ELTEXTCF
00921 **---------------------------------------------------------------+ELTEXTCF
00922 **                                                               |ELTEXTCF
00923 **   E X T E N D E D  C A R E  P R I O R  A D M  R E Q U I R E   |ELTEXTCF
00924      IF GCG-ECF-SNF-PRIOR-ADM-CD NOT = ZEROS                      ELTEXTCF
00925          ADD +1                       TO WS-CIA                   ELTEXTCF
00926          MOVE WS-PRIOR-ADM-REQUIREMENT TO COF-DTL-LINE(WS-CIA)    ELTEXTCF
00927          ADD +1                       TO WS-CIA                   ELTEXTCF
00928          MOVE 'GROUP'                 TO  CMF-RECORD-PREFIX       ELTEXTCF
00929          MOVE 'ECF-SNF-PRIOR-ADM-CD'  TO  CMF-ELEMENT-SYSTEM-NAME ELTEXTCF
00930          MOVE GCG-ECF-SNF-PRIOR-ADM-CD                            ELTEXTCF
00931                                   TO  CMF-CODE-VALUE              ELTEXTCF
00932          MOVE ZERO    TO  WS-TEMP-NOT-USED-CNT                    ELTEXTCF
00933          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTEXTCF
00934          MOVE 'Y'  TO  WS-INDENT-FOUR-IND                         ELTEXTCF
00935          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTEXTCF
00936 **                                                               |ELTEXTCF
00937 **---------------------------------------------------------------+ELTEXTCF
00938                                                                   ELTEXTCF
00939      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
00940      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
00941        PERFORM 7000-SPILLOVER-COINS                               ELTEXTCF
00942        PERFORM 7200-SPILLOVER-DEDUCT                              ELTEXTCF
00943        IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
00944          NOT = '0' AND NOT = LOW-VALUES                           ELTEXTCF
00945           ADD +1  TO  WS-CIA                                      ELTEXTCF
00946           MOVE 'BP' TO CMF-RECORD-PREFIX                          ELTEXTCF
00947           MOVE 'SPILL-OVR-RM-F-RT-APL-IND' TO                     ELTEXTCF
00948                        CMF-ELEMENT-SYSTEM-NAME                    ELTEXTCF
00949           MOVE                                                    ELTEXTCF
00950            PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
00951             TO CMF-CODE-VALUE                                     ELTEXTCF
00952           MOVE WS-SPILLOVER-FL-RT-PER-D TO WS-TEMP-TEXT-AREA      ELTEXTCF
00953           MOVE 50 TO WS-TEMP-NOT-USED-CNT                         ELTEXTCF
00954           PERFORM 2100-CALL-CODES-MANUAL-LONG                     ELTEXTCF
00955           PERFORM 8000-OUTPUT-TEXT                                ELTEXTCF
00956        ELSE                                                       ELTEXTCF
00957          PERFORM 8000-OUTPUT-TEXT.                                ELTEXTCF
00958                                                                   ELTEXTCF
00959      PERFORM 6000-SCAN-TAB.                                       ELTEXTCF
00960      PERFORM 6050-ANCILLARY-TEXT.                                 ELTEXTCF
00961      PERFORM 6100-PAY-CONSID-TEXT.                                ELTEXTCF
00962      PERFORM 7300-TRANS-OTHR-RESPON-IND.                          ELTEXTCF
00963                                                                   ELTEXTCF
00964  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTEXTCF
00965                                                                   ELTEXTCF
00966      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEXTCF
00967         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
00968         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEXTCF
00969         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEXTCF
00970                                                   CMF-CODE-VALUE  ELTEXTCF
00971         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEXTCF
00972         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
00973         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
00974         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEXTCF
00975         ADD  1  TO  WS-SUB2                                       ELTEXTCF
00976         IF WS-CIA  >  20 OR  =  20                                ELTEXTCF
00977            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEXTCF
00978            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEXTCF
00979                COMMAREA(DFHCOMMAREA)                              ELTEXTCF
00980            END-EXEC                                               ELTEXTCF
00981            MOVE +1  TO  WS-CIA.                                   ELTEXTCF
00982                                                                   ELTEXTCF
00983  1090-PROBLEM-WITH-INDICES.                                       ELTEXTCF
00984                                                                   ELTEXTCF
00985      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEXTCF
00986      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEXTCF
00987                                                                   ELTEXTCF
00988      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTEXTCF
00989      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEXTCF
00990                                                                   ELTEXTCF
00991      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
00992      END-EXEC.                                                    ELTEXTCF
00993                                                                   ELTEXTCF
00994  1099-EXIT.            EXIT.                                      ELTEXTCF
00995      TITLE 'PROFESSIONAL  INPATIENT'.                             ELTEXTCF
00996  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTEXTCF
00997 ***************************************************************** ELTEXTCF
00998 *        P R O F E S S I O N A L   I P   R T N E                  ELTEXTCF
00999 *                                                                 ELTEXTCF
01000 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEXTCF
01001 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEXTCF
01002 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEXTCF
01003 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEXTCF
01004 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEXTCF
01005 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEXTCF
01006 *  MODULE.                                                        ELTEXTCF
01007 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEXTCF
01008 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEXTCF
01009 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEXTCF
01010 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEXTCF
01011 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEXTCF
01012 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEXTCF
01013 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEXTCF
01014 *                                                                 ELTEXTCF
01015 ***************************************************************** ELTEXTCF
01016      MOVE '2000'  TO  WS-PARA-ID1.                                ELTEXTCF
01017                                                                   ELTEXTCF
01018      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEXTCF
01019      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEXTCF
01020      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEXTCF
01021                     COF-NBR-DTL-LINES.                            ELTEXTCF
01022      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
01023      END-EXEC.                                                    ELTEXTCF
01024      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEXTCF
01025      MOVE WS-HDR-2-PROF-IP  TO  COF-HDR-LINE(2).                  ELTEXTCF
01026                                                                   ELTEXTCF
01027      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTEXTCF
01028      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTEXTCF
01029         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEXTCF
01030         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTEXTCF
01031                                                                   ELTEXTCF
01032      GO TO 2020-CALL-COVERAGE.                                    ELTEXTCF
01033  2010-MOVE-IN-PROF-IP.                                            ELTEXTCF
01034      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEXTCF
01035      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTEXTCF
01036                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEXTCF
01037      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEXTCF
01038                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEXTCF
01039                                                                   ELTEXTCF
01040  2020-CALL-COVERAGE.                                              ELTEXTCF
01041      MOVE '2020'  TO  WS-PARA-ID1.                                ELTEXTCF
01042      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEXTCF
01043      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
01044      END-EXEC.                                                    ELTEXTCF
01045                                                                   ELTEXTCF
01046      MOVE 'EXTENDED CARE FACILITY SERVICES '                      ELTEXTCF
01047                TO SSB-TOPIC-PHRASE.                               ELTEXTCF
01048                                                                   ELTEXTCF
01049      INITIALIZE PVN-PROCESSING-ECF-IND.                           ELTEXTCF
01050                                                                   ELTEXTCF
01051      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEXTCF
01052      END-EXEC.                                                    ELTEXTCF
01053                                                                   ELTEXTCF
01054      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTEXTCF
01055      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
01056      END-EXEC.                                                    ELTEXTCF
01057                                                                   ELTEXTCF
01058      IF PVN-COVG-NONE                                             ELTEXTCF
01059         GO TO 2099-EXIT.                                          ELTEXTCF
01060                                                                   ELTEXTCF
01061      MOVE +1  TO  WS-CIA.                                         ELTEXTCF
01062                                                                   ELTEXTCF
01063      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTEXTCF
01064      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEXTCF
01065            PSP-PROVN-PRICING-METHD,                               ELTEXTCF
01066            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEXTCF
01067            PSP-TRANSF-OTHER-RESP-IND,                             ELTEXTCF
01068            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEXTCF
01069            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEXTCF
01070            PSP-SPILL-OVER-DED-APL-IND,                            ELTEXTCF
01071            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEXTCF
01072            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEXTCF
01073            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEXTCF
01074            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEXTCF
01075            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEXTCF
01076            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEXTCF
01077            PSD-BEN-SCOPE-ID,                                      ELTEXTCF
01078            PSD-DAYS-RDCN-RAT-IND,                                 ELTEXTCF
01079            PSD-DAYS-RDCN-RAT-BASIC-APL,                           ELTEXTCF
01080            PSD-DAYS-RDCN-RAT-BASIC-BASE,                          ELTEXTCF
01081            PSD-DAYS-RDCN-RAT-SEC-APL,                             ELTEXTCF
01082            PSD-DAYS-RDCN-RAT-SEC-BASE,                            ELTEXTCF
01083            PSD-FLAT-RATE-PDM-AMT,                                 ELTEXTCF
01084            PSD-MAX-AMT-PER-VISIT,                                 ELTEXTCF
01085            PSD-BEN-MAX-VISIT-IND,                                 ELTEXTCF
01086            PSD-BEN-MAX-VISIT-DAYS.                                ELTEXTCF
01087                                                                   ELTEXTCF
01088      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEXTCF
01089      END-EXEC.                                                    ELTEXTCF
01090                                                                   ELTEXTCF
01091      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEXTCF
01092      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
01093          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTEXTCF
01094                                                                   ELTEXTCF
01095      PERFORM 2030-FIND-FIRST-NONZERO                              ELTEXTCF
01096         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEXTCF
01097         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTEXTCF
01098                                                                   ELTEXTCF
01099      GO TO 2099-EXIT.                                             ELTEXTCF
01100  2030-FIND-FIRST-NONZERO.                                         ELTEXTCF
01101      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEXTCF
01102      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEXTCF
01103         NEXT SENTENCE                                             ELTEXTCF
01104      ELSE                                                         ELTEXTCF
01105         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTEXTCF
01106                                                                   ELTEXTCF
01107  2040-BUILD-SCREEN-LINES.                                         ELTEXTCF
01108      MOVE '2040'  TO  WS-PARA-ID1.                                ELTEXTCF
01109                                                                   ELTEXTCF
01110      SET PLT-INDEX1   TO                                          ELTEXTCF
01111                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTEXTCF
01112      IF WS-NOT-FIRST-TIME                                         ELTEXTCF
01113         MOVE 'P'  TO  COF-FUNCTION                                ELTEXTCF
01114         MOVE +0   TO  COF-NBR-DTL-LINES                           ELTEXTCF
01115         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEXTCF
01116             COMMAREA(DFHCOMMAREA)                                 ELTEXTCF
01117         END-EXEC                                                  ELTEXTCF
01118      ELSE                                                         ELTEXTCF
01119         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEXTCF
01120                                                                   ELTEXTCF
01121      MOVE +1  TO  WS-CIA.                                         ELTEXTCF
01122      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEXTCF
01123         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEXTCF
01124            SET PLT-INDEX2  TO  2                                  ELTEXTCF
01125         ELSE                                                      ELTEXTCF
01126            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTEXTCF
01127            GO TO 2099-EXIT                                        ELTEXTCF
01128      ELSE                                                         ELTEXTCF
01129         SET PLT-INDEX2  TO  1.                                    ELTEXTCF
01130                                                                   ELTEXTCF
01131 **---------------------------------------------------------------+ELTEXTCF
01132 **                                                               |ELTEXTCF
01133 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEXTCF
01134      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEXTCF
01135      ADD  +1  TO  WS-CIA.                                         ELTEXTCF
01136      MOVE ZERO  TO  WS-SUB2.                                      ELTEXTCF
01137      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEXTCF
01138      MOVE '2050'  TO  WS-PARA-ID1.                                ELTEXTCF
01139                                                                   ELTEXTCF
01140      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTEXTCF
01141         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTEXTCF
01142         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTEXTCF
01143      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEXTCF
01144                                                                   ELTEXTCF
01145      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEXTCF
01146      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
01147      END-EXEC.                                                    ELTEXTCF
01148      MOVE +1  TO  WS-CIA.                                         ELTEXTCF
01149 **                                                               |ELTEXTCF
01150 **---------------------------------------------------------------+ELTEXTCF
01151                                                                   ELTEXTCF
01152      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01153      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01154        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01155               NOT = ZERO                                          ELTEXTCF
01156         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTEXTCF
01157         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01158         ADD +1  TO  WS-CIA.                                       ELTEXTCF
01159                                                                   ELTEXTCF
01160      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
01161      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01162       AND  NOT WS-ADD-A-BLANK-LINE                                ELTEXTCF
01163        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01164               NOT = ZERO                                          ELTEXTCF
01165         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTEXTCF
01166         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01167         ADD +1  TO  WS-CIA.                                       ELTEXTCF
01168                                                                   ELTEXTCF
01169      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01170             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTEXTCF
01171                                                                   ELTEXTCF
01172      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01173             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTEXTCF
01174                                                                   ELTEXTCF
01175      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
01176            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTEXTCF
01177            ADD  +1   TO  WS-CIA                                   ELTEXTCF
01178            PERFORM 8000-OUTPUT-TEXT.                              ELTEXTCF
01179                                                                   ELTEXTCF
01180 **---------------------------------------------------------------+ELTEXTCF
01181 **                                                               |ELTEXTCF
01182 **            B E N E F I T   S C O P E   I D                    |ELTEXTCF
01183      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01184      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01185          AND NOT WS-ADD-A-BLANK-LINE                              ELTEXTCF
01186           AND  PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01187                                         '0000' AND  NOT =  '00  ' ELTEXTCF
01188               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTEXTCF
01189               ADD  +1  TO  WS-CIA                                 ELTEXTCF
01190               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTEXTCF
01191                                                                   ELTEXTCF
01192      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
01193      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01194          AND NOT WS-ADD-A-BLANK-LINE                              ELTEXTCF
01195            AND PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01196                                         '0000' AND  NOT =  '00  ' ELTEXTCF
01197               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTEXTCF
01198               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTEXTCF
01199               ADD  +1  TO  WS-CIA.                                ELTEXTCF
01200                                                                   ELTEXTCF
01201      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01202      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01203             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01204                                         '0000' AND  NOT =  '00  ' ELTEXTCF
01205               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTEXTCF
01206               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTEXTCF
01207               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTEXTCF
01208                                                    CMF-CODE-VALUE ELTEXTCF
01209               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTEXTCF
01210               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTEXTCF
01211               MOVE 'Y'  TO  WS-INDENT-IND                         ELTEXTCF
01212               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTEXTCF
01213                                                                   ELTEXTCF
01214                                                                   ELTEXTCF
01215      SET PLT-INDEX2  TO  2.                                       ELTEXTCF
01216      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01217             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01218                                         '0000' AND  NOT =  '00  ' ELTEXTCF
01219               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTEXTCF
01220               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTEXTCF
01221               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTEXTCF
01222                                                    CMF-CODE-VALUE ELTEXTCF
01223               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTEXTCF
01224               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTEXTCF
01225               MOVE 'Y'  TO  WS-INDENT-IND                         ELTEXTCF
01226               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTEXTCF
01227                                                                   ELTEXTCF
01228      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
01229         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01230         ADD  +1   TO  WS-CIA                                      ELTEXTCF
01231         PERFORM 8000-OUTPUT-TEXT.                                 ELTEXTCF
01232 **                                                               |ELTEXTCF
01233 **---------------------------------------------------------------+ELTEXTCF
01234                                                                   ELTEXTCF
01235 **---------------------------------------------------------------+ELTEXTCF
01236 **                                                               |ELTEXTCF
01237 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEXTCF
01238 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEXTCF
01239 **     A D D I T I O N A L   P R I C I N G   P E R C E N T       |ELTEXTCF
01240      MOVE ZEROS       TO WS-PER-DIEM,                             ELTEXTCF
01241                          WS-PERCENTAGE.                           ELTEXTCF
01242      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01243      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
01244         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01245                                                              '19' ELTEXTCF
01246         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEXTCF
01247         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01248         ADD +1  TO  WS-CIA.                                       ELTEXTCF
01249                                                                   ELTEXTCF
01250      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
01251      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEXTCF
01252         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01253                                                        '19' AND   ELTEXTCF
01254         NOT WS-ADD-A-BLANK-LINE                                   ELTEXTCF
01255         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEXTCF
01256         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01257         ADD +1  TO  WS-CIA.                                       ELTEXTCF
01258                                                                   ELTEXTCF
01259      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01260      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
01261         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEXTCF
01262                            AND                                    ELTEXTCF
01263         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01264         SET  PLT-INDEX2  TO  2                                    ELTEXTCF
01265         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEXTCF
01266                                                             ZERO  ELTEXTCF
01267            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEXTCF
01268            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEXTCF
01269            ADD +1  TO  WS-CIA.                                    ELTEXTCF
01270                                                                   ELTEXTCF
01271      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01272      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
01273         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEXTCF
01274                            AND                                    ELTEXTCF
01275         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTEXTCF
01276         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEXTCF
01277         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEXTCF
01278         ADD +1  TO  WS-CIA.                                       ELTEXTCF
01279                                                                   ELTEXTCF
01280      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEXTCF
01281         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01282         SET  PLT-INDEX2  TO  2                                    ELTEXTCF
01283         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEXTCF
01284                                                             ZERO  ELTEXTCF
01285            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEXTCF
01286            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEXTCF
01287            ADD +1  TO  WS-CIA.                                    ELTEXTCF
01288                                                                   ELTEXTCF
01289      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01290      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01291         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01292                                                            =  ZEROELTEXTCF
01293            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01294                                                            =  ZEROELTEXTCF
01295               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTEXTCF
01296            ELSE                                                   ELTEXTCF
01297               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEXTCF
01298          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01299                                                  TO  WS-PERCENTAGEELTEXTCF
01300          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTEXTCF
01301         ELSE                                                      ELTEXTCF
01302          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTEXTCF
01303          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01304                                                 TO  WS-PERCENTAGE ELTEXTCF
01305          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTEXTCF
01306                                                                   ELTEXTCF
01307      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01308         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01309                                                            =  ZEROELTEXTCF
01310          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
01311                                                            =  ZEROELTEXTCF
01312            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01313                                                            =  ZEROELTEXTCF
01314               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW.              ELTEXTCF
01315                                                                   ELTEXTCF
01316      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01317         IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01318                                                       NOT  =  ZEROELTEXTCF
01319             MOVE                                                  ELTEXTCF
01320               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01321                                                  TO  WS-PER-DIEM. ELTEXTCF
01322                                                                   ELTEXTCF
01323      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEXTCF
01324         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01325                                             ZERO AND  NOT =  '19' ELTEXTCF
01326         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
01327         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEXTCF
01328         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEXTCF
01329                                               CMF-CODE-VALUE      ELTEXTCF
01330                                               WS-TEST-FOR-PER-DIEMELTEXTCF
01331         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEXTCF
01332         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
01333         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTEXTCF
01334                                                                   ELTEXTCF
01335      MOVE ZEROS       TO WS-PER-DIEM,                             ELTEXTCF
01336                          WS-PERCENTAGE.                           ELTEXTCF
01337      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
01338      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01339         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEXTCF
01340                                                               ZEROELTEXTCF
01341            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01342                                                            =  ZEROELTEXTCF
01343               MOVE SPACES  TO  WS-PERCENT-SIGN                    ELTEXTCF
01344            ELSE                                                   ELTEXTCF
01345               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEXTCF
01346          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01347                                                  TO  WS-PERCENTAGEELTEXTCF
01348          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTEXTCF
01349         ELSE                                                      ELTEXTCF
01350            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEXTCF
01351          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01352                                                 TO  WS-PERCENTAGE ELTEXTCF
01353          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTEXTCF
01354                                                                   ELTEXTCF
01355      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01356         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01357                                                            =  ZEROELTEXTCF
01358          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
01359                                                            =  ZEROELTEXTCF
01360            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01361                                                            =  ZEROELTEXTCF
01362               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW.              ELTEXTCF
01363                                                                   ELTEXTCF
01364                                                                   ELTEXTCF
01365      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01366         IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01367                                                       NOT  =  ZEROELTEXTCF
01368             MOVE                                                  ELTEXTCF
01369               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01370                                                  TO  WS-PER-DIEM. ELTEXTCF
01371                                                                   ELTEXTCF
01372      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEXTCF
01373         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEXTCF
01374                                             ZERO AND  NOT =  '19' ELTEXTCF
01375         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
01376         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEXTCF
01377         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEXTCF
01378                                               CMF-CODE-VALUE      ELTEXTCF
01379                                               WS-TEST-FOR-PER-DIEMELTEXTCF
01380         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEXTCF
01381         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
01382         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTEXTCF
01383                                                                   ELTEXTCF
01384      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
01385         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEXTCF
01386         ADD  +1   TO  WS-CIA                                      ELTEXTCF
01387         PERFORM 8000-OUTPUT-TEXT                                  ELTEXTCF
01388      ELSE                                                         ELTEXTCF
01389       PERFORM 8000-OUTPUT-TEXT.                                   ELTEXTCF
01390 **                                                               |ELTEXTCF
01391 **---------------------------------------------------------------+ELTEXTCF
01392                                                                   ELTEXTCF
01393 **---------------------------------------------------------------+ELTEXTCF
01394 **    M  A  X   V  I  S  I  T  S                                 |ELTEXTCF
01395                                                                   ELTEXTCF
01396      MOVE WS-NO TO WS-DISPLAY-MAX-AMT-TEXT                        ELTEXTCF
01397                    WS-DISPLAY-MAX-VISITS-TEXT.                    ELTEXTCF
01398                                                                   ELTEXTCF
01399      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
01400      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01401       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTEXTCF
01402        AND NOT = LOW-VALUES)                                      ELTEXTCF
01403          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTEXTCF
01404                                                                   ELTEXTCF
01405      SET  PLT-INDEX2 TO  2.                                       ELTEXTCF
01406      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01407       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTEXTCF
01408        AND NOT = LOW-VALUES)                                      ELTEXTCF
01409          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTEXTCF
01410                                                                   ELTEXTCF
01411      IF WS-DISPLAY-MAX-VISITS-TEXT = WS-YES                       ELTEXTCF
01412          ADD +1             TO WS-CIA                             ELTEXTCF
01413          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTEXTCF
01414          ADD +1             TO WS-CIA.                            ELTEXTCF
01415                                                                   ELTEXTCF
01416      SET  PLT-INDEX2           TO  1.                             ELTEXTCF
01417      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01418        IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01419                  NOT = ZEROS                                      ELTEXTCF
01420         MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01421                TO WS-DTL-MAX-DAYS                                 ELTEXTCF
01422         MOVE SPACES   TO TCAR-FROM-AREA                           ELTEXTCF
01423         STRING WS-BASIC-LIT ' '                                   ELTEXTCF
01424                WS-DTL-MAX-DAYS ' '                                ELTEXTCF
01425                WS-PER ' '                                         ELTEXTCF
01426                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTEXTCF
01427         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTEXTCF
01428         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTEXTCF
01429         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTEXTCF
01430         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTEXTCF
01431         PERFORM TCPR-000-TEXT-UNSTRING                            ELTEXTCF
01432         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTEXTCF
01433         MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO    ELTEXTCF
01434                                                 CMF-CODE-VALUE    ELTEXTCF
01435         MOVE 'BPD'                TO  CMF-RECORD-PREFIX           ELTEXTCF
01436         MOVE 'BEN-MAX-VISIT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME     ELTEXTCF
01437         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
01438         PERFORM 8000-OUTPUT-TEXT.                                 ELTEXTCF
01439                                                                   ELTEXTCF
01440      SET  PLT-INDEX2           TO  2.                             ELTEXTCF
01441      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01442         IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
01443                NOT = ZEROS                                        ELTEXTCF
01444          MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTEXTCF
01445                TO WS-DTL-MAX-DAYS                                 ELTEXTCF
01446          MOVE SPACES   TO TCAR-FROM-AREA                          ELTEXTCF
01447          STRING WS-SUPP-LIT ' '                                   ELTEXTCF
01448                 WS-DTL-MAX-DAYS                                   ELTEXTCF
01449                 WS-PER ' '                                        ELTEXTCF
01450                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTEXTCF
01451          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTEXTCF
01452          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTEXTCF
01453          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTEXTCF
01454          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTEXTCF
01455          PERFORM TCPR-000-TEXT-UNSTRING                           ELTEXTCF
01456          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTEXTCF
01457          MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO   ELTEXTCF
01458                                                 CMF-CODE-VALUE    ELTEXTCF
01459          MOVE 'BPD'                TO  CMF-RECORD-PREFIX          ELTEXTCF
01460          MOVE 'BEN-MAX-VISIT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTEXTCF
01461          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTEXTCF
01462          PERFORM 8000-OUTPUT-TEXT.                                ELTEXTCF
01463                                                                   ELTEXTCF
01464 **                                                               |ELTEXTCF
01465 **---------------------------------------------------------------+ELTEXTCF
01466                                                                   ELTEXTCF
01467 **---------------------------------------------------------------+ELTEXTCF
01468 **    M  A  X   A  M  O  U  N  T   P  E  R   V  I  S  I  T       |ELTEXTCF
01469      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
01470      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01471         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01472                NOT = ZEROS                                        ELTEXTCF
01473            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTEXTCF
01474                                                                   ELTEXTCF
01475      SET  PLT-INDEX2 TO  2.                                       ELTEXTCF
01476      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01477         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01478                NOT = ZEROS                                        ELTEXTCF
01479            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTEXTCF
01480                                                                   ELTEXTCF
01481      IF WS-DISPLAY-MAX-AMT-TEXT = WS-YES                          ELTEXTCF
01482          ADD +1             TO WS-CIA                             ELTEXTCF
01483          MOVE WS-MAX-AMOUNT TO COF-DTL-LINE(WS-CIA)               ELTEXTCF
01484          ADD +1             TO WS-CIA.                            ELTEXTCF
01485                                                                   ELTEXTCF
01486      SET  PLT-INDEX2           TO  1.                             ELTEXTCF
01487      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01488        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
01489                  NOT = ZEROS                                      ELTEXTCF
01490         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
01491                TO WS-DTL-MAX-AMOUNT                               ELTEXTCF
01492         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTEXTCF
01493         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTEXTCF
01494         PERFORM 8000-OUTPUT-TEXT.                                 ELTEXTCF
01495                                                                   ELTEXTCF
01496      SET  PLT-INDEX2           TO  2.                             ELTEXTCF
01497      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01498        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
01499                  NOT = ZEROS                                      ELTEXTCF
01500         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
01501                TO WS-DTL-MAX-AMOUNT                               ELTEXTCF
01502         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTEXTCF
01503         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTEXTCF
01504         PERFORM 8000-OUTPUT-TEXT.                                 ELTEXTCF
01505                                                                   ELTEXTCF
01506 **                                                               |ELTEXTCF
01507 **---------------------------------------------------------------+ELTEXTCF
01508                                                                   ELTEXTCF
01509 **---------------------------------------------------------------+ELTEXTCF
01510 **                                                               |ELTEXTCF
01511 **   E X T E N D E D  C A R E  P R I O R  A D M  R E Q U I R E   |ELTEXTCF
01512      IF GCG-ECF-SNF-PRIOR-ADM-CD NOT = ZEROS                      ELTEXTCF
01513          ADD +1                       TO WS-CIA                   ELTEXTCF
01514          MOVE WS-PRIOR-ADM-REQUIREMENT TO COF-DTL-LINE(WS-CIA)    ELTEXTCF
01515          ADD +1                       TO WS-CIA                   ELTEXTCF
01516          MOVE 'GROUP'                 TO  CMF-RECORD-PREFIX       ELTEXTCF
01517          MOVE 'ECF-SNF-PRIOR-ADM-CD'  TO  CMF-ELEMENT-SYSTEM-NAME ELTEXTCF
01518          MOVE GCG-ECF-SNF-PRIOR-ADM-CD                            ELTEXTCF
01519                                   TO  CMF-CODE-VALUE              ELTEXTCF
01520          MOVE ZERO    TO  WS-TEMP-NOT-USED-CNT                    ELTEXTCF
01521          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTEXTCF
01522          MOVE 'Y'  TO  WS-INDENT-FOUR-IND                         ELTEXTCF
01523          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTEXTCF
01524 **                                                               |ELTEXTCF
01525 **---------------------------------------------------------------+ELTEXTCF
01526                                                                   ELTEXTCF
01527                                                                   ELTEXTCF
01528 **---------------------------------------------------------------+ELTEXTCF
01529 **                                                               |ELTEXTCF
01530 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTEXTCF
01531                                                                   ELTEXTCF
01532      SET  PLT-INDEX2          TO  1.                              ELTEXTCF
01533      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01534        IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
01535                     NOT = '0'                                     ELTEXTCF
01536         IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTEXTCF
01537                     NOT = LOW-VALUES                              ELTEXTCF
01538          ADD +1                   TO WS-CIA                       ELTEXTCF
01539          MOVE 'BPD'               TO  CMF-RECORD-PREFIX           ELTEXTCF
01540          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTEXTCF
01541          MOVE PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTEXTCF
01542                                   TO  CMF-CODE-VALUE              ELTEXTCF
01543          MOVE WS-YES              TO WS-ADD-A-BLANK-IND           ELTEXTCF
01544          PERFORM 2400-CALL-CODES-MANUAL-DISP.                     ELTEXTCF
01545                                                                   ELTEXTCF
01546      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01547        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01548                     NOT = ZEROS  AND                              ELTEXTCF
01549            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
01550                      NOT = ZEROS                                  ELTEXTCF
01551             MOVE                                                  ELTEXTCF
01552              PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTEXTCF
01553                    TO WS-DTL-DAYS-REDUCED-APL                     ELTEXTCF
01554             MOVE                                                  ELTEXTCF
01555              PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTEXTCF
01556                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTEXTCF
01557             MOVE SPACES          TO TCAR-FROM-AREA                ELTEXTCF
01558             STRING WS-DAYS-REDUCED,                               ELTEXTCF
01559                    WS-BASIC-LIT,                                  ELTEXTCF
01560                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTEXTCF
01561                    WS-FOR, ' '                                    ELTEXTCF
01562                    WS-DTL-DAYS-REDUCED-BASE,                      ELTEXTCF
01563                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTEXTCF
01564             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTEXTCF
01565             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTEXTCF
01566             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTEXTCF
01567             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTEXTCF
01568             PERFORM TCPR-000-TEXT-UNSTRING                        ELTEXTCF
01569             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTEXTCF
01570             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTEXTCF
01571                                                                   ELTEXTCF
01572      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01573        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01574                    NOT  = ZEROS  AND                              ELTEXTCF
01575            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
01576                     NOT  = ZEROS                                  ELTEXTCF
01577                              ADD +1  TO  WS-CIA.                  ELTEXTCF
01578                                                                   ELTEXTCF
01579      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEXTCF
01580        IF  PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTEXTCF
01581                     NOT = ZEROS  AND                              ELTEXTCF
01582            PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTEXTCF
01583                      NOT = ZEROS                                  ELTEXTCF
01584             MOVE                                                  ELTEXTCF
01585              PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTEXTCF
01586                    TO WS-DTL-DAYS-REDUCED-APL                     ELTEXTCF
01587             MOVE                                                  ELTEXTCF
01588              PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTEXTCF
01589                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTEXTCF
01590             MOVE SPACES          TO TCAR-FROM-AREA                ELTEXTCF
01591             STRING WS-DAYS-REDUCED,                               ELTEXTCF
01592                    WS-SECONDARY, ' '                              ELTEXTCF
01593                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTEXTCF
01594                    WS-FOR, ' '                                    ELTEXTCF
01595                    WS-DTL-DAYS-REDUCED-BASE,                      ELTEXTCF
01596                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTEXTCF
01597             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTEXTCF
01598             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTEXTCF
01599             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTEXTCF
01600             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTEXTCF
01601             PERFORM TCPR-000-TEXT-UNSTRING                        ELTEXTCF
01602             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTEXTCF
01603             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTEXTCF
01604                                                                   ELTEXTCF
01605      IF WS-ADD-A-BLANK-LINE                                       ELTEXTCF
01606          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTEXTCF
01607          ADD +1  TO  WS-CIA                                       ELTEXTCF
01608          PERFORM 8000-OUTPUT-TEXT.                                ELTEXTCF
01609 **                                                               |ELTEXTCF
01610 **---------------------------------------------------------------+ELTEXTCF
01611                                                                   ELTEXTCF
01612      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
01613      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEXTCF
01614           PERFORM 7000-SPILLOVER-COINS                            ELTEXTCF
01615           PERFORM 7200-SPILLOVER-DEDUCT.                          ELTEXTCF
01616           PERFORM 8000-OUTPUT-TEXT.                               ELTEXTCF
01617                                                                   ELTEXTCF
01618      PERFORM 6000-SCAN-TAB.                                       ELTEXTCF
01619      PERFORM 6050-ANCILLARY-TEXT.                                 ELTEXTCF
01620      PERFORM 6100-PAY-CONSID-TEXT.                                ELTEXTCF
01621      PERFORM 7300-TRANS-OTHR-RESPON-IND.                          ELTEXTCF
01622                                                                   ELTEXTCF
01623  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTEXTCF
01624                                                                   ELTEXTCF
01625      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEXTCF
01626         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
01627         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEXTCF
01628         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEXTCF
01629                                                    CMF-CODE-VALUE ELTEXTCF
01630         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEXTCF
01631         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEXTCF
01632         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
01633         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEXTCF
01634         ADD  1  TO  WS-SUB2                                       ELTEXTCF
01635         IF WS-CIA  >  20 OR  =  20                                ELTEXTCF
01636            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEXTCF
01637            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEXTCF
01638                COMMAREA(DFHCOMMAREA)                              ELTEXTCF
01639            END-EXEC                                               ELTEXTCF
01640            MOVE +1  TO  WS-CIA.                                   ELTEXTCF
01641                                                                   ELTEXTCF
01642  2090-PROBLEM-WITH-INDICES.                                       ELTEXTCF
01643                                                                   ELTEXTCF
01644      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEXTCF
01645      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEXTCF
01646                                                                   ELTEXTCF
01647      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTEXTCF
01648      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEXTCF
01649                                                                   ELTEXTCF
01650      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEXTCF
01651      END-EXEC.                                                    ELTEXTCF
01652                                                                   ELTEXTCF
01653  2099-EXIT.            EXIT.                                      ELTEXTCF
01654                                                                   ELTEXTCF
01655      TITLE 'CODES MANUAL FOR LONG DESCRIPT'.                      ELTEXTCF
01656  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTEXTCF
01657      MOVE '2100'  TO  WS-PARA-ID2.                                ELTEXTCF
01658                                                                   ELTEXTCF
01659      INITIALIZE CMF-RETURN-CODE,                                  ELTEXTCF
01660                 TCAR-FROM-AREA.                                   ELTEXTCF
01661                                                                   ELTEXTCF
01662      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEXTCF
01663      END-EXEC.                                                    ELTEXTCF
01664                                                                   ELTEXTCF
01665      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEXTCF
01666      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
01667          ADDRESS OF CMF-DESCR.                                    ELTEXTCF
01668                                                                   ELTEXTCF
01669      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTEXTCF
01670         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTEXTCF
01671         STRING WS-TEMP-TEXT-AREA,                                 ELTEXTCF
01672            CMF-DESCR-LINE(1),        ' ',                         ELTEXTCF
01673            CMF-DESCR-LINE(2),        ' ',                         ELTEXTCF
01674            CMF-DESCR-LINE(3)                                      ELTEXTCF
01675            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTEXTCF
01676      ELSE                                                         ELTEXTCF
01677         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTEXTCF
01678         STRING CMF-DESCR-LINE(1),        ' ',                     ELTEXTCF
01679            CMF-DESCR-LINE(2),        ' ',                         ELTEXTCF
01680            CMF-DESCR-LINE(3)                                      ELTEXTCF
01681            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTEXTCF
01682                                                                   ELTEXTCF
01683      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
01684                                                                   ELTEXTCF
01685      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTEXTCF
01686      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTEXTCF
01687      IF WS-INDENT-ON                                              ELTEXTCF
01688         MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN,                    ELTEXTCF
01689                       TCAR-OUTPUT-FIELD-3-LEN,                    ELTEXTCF
01690                       TCAR-OUTPUT-FIELD-4-LEN                     ELTEXTCF
01691      ELSE                                                         ELTEXTCF
01692       MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                      ELTEXTCF
01693                     TCAR-OUTPUT-FIELD-3-LEN,                      ELTEXTCF
01694                     TCAR-OUTPUT-FIELD-4-LEN.                      ELTEXTCF
01695                                                                   ELTEXTCF
01696      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
01697                                                                   ELTEXTCF
01698      IF WS-MOVE-LINES-TO-CIA                                      ELTEXTCF
01699         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTEXTCF
01700            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTEXTCF
01701                                             WS-TEMP-NOT-USED-CNT  ELTEXTCF
01702            MOVE '2150'  TO  WS-PARA-ID2                           ELTEXTCF
01703            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTEXTCF
01704               VARYING  WS-SUB1  FROM  1  BY  1                    ELTEXTCF
01705               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTEXTCF
01706            MOVE '2100'  TO  WS-PARA-ID2                           ELTEXTCF
01707            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTEXTCF
01708            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTEXTCF
01709            ADD +1  TO  WS-CIA                                     ELTEXTCF
01710         ELSE                                                      ELTEXTCF
01711            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTEXTCF
01712            ADD +1  TO  WS-CIA.                                    ELTEXTCF
01713                                                                   ELTEXTCF
01714      IF WS-MOVE-LINES-TO-CIA                                      ELTEXTCF
01715         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTEXTCF
01716            MOVE '2160'  TO  WS-PARA-ID2                           ELTEXTCF
01717            PERFORM 2160-MOVE-LINES-TO-CIA                         ELTEXTCF
01718               VARYING  WS-SUB1  FROM  2  BY  1                    ELTEXTCF
01719               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTEXTCF
01720            MOVE '2100' TO WS-PARA-ID2                             ELTEXTCF
01721         ELSE                                                      ELTEXTCF
01722            NEXT SENTENCE                                          ELTEXTCF
01723      ELSE                                                         ELTEXTCF
01724         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTEXTCF
01725                                                                   ELTEXTCF
01726      MOVE 'N' TO WS-INDENT-IND.                                   ELTEXTCF
01727      GO TO 2199-EXIT.                                             ELTEXTCF
01728                                                                   ELTEXTCF
01729  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTEXTCF
01730      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTEXTCF
01731      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEXTCF
01732                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEXTCF
01733                                                                   ELTEXTCF
01734  2160-MOVE-LINES-TO-CIA.                                          ELTEXTCF
01735      IF WS-INDENT-ON                                              ELTEXTCF
01736         MOVE TCAR-OPF-DATA(WS-SUB1)  TO  WS-DTL-INDENTED          ELTEXTCF
01737         MOVE WS-INDENTED             TO  COF-DTL-LINE(WS-CIA)     ELTEXTCF
01738      ELSE                                                         ELTEXTCF
01739        MOVE TCAR-OPF-DATA(WS-SUB1)  TO COF-DTL-LINE(WS-CIA).      ELTEXTCF
01740      ADD +1  TO  WS-CIA.                                          ELTEXTCF
01741                                                                   ELTEXTCF
01742  2199-EXIT.           EXIT.                                       ELTEXTCF
01743                                                                   ELTEXTCF
01744      TITLE 'CODES MANUAL FOR LONG DESCRIPT'.                      ELTEXTCF
01745  2200-CODES-MANUAL-WITH-AMOUNT SECTION.                           ELTEXTCF
01746      MOVE '2200'  TO  WS-PARA-ID2.                                ELTEXTCF
01747                                                                   ELTEXTCF
01748      INITIALIZE CMF-RETURN-CODE,                                  ELTEXTCF
01749                 TCAR-FROM-AREA.                                   ELTEXTCF
01750                                                                   ELTEXTCF
01751      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEXTCF
01752      END-EXEC.                                                    ELTEXTCF
01753                                                                   ELTEXTCF
01754      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEXTCF
01755      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
01756          ADDRESS OF CMF-DESCR.                                    ELTEXTCF
01757                                                                   ELTEXTCF
01758      IF FLAT-RATE                                                 ELTEXTCF
01759       IF WS-PER-DIEM NOT = ZEROS                                  ELTEXTCF
01760        IF WS-TEMP-NOT-USED-CNT  =  ZERO                           ELTEXTCF
01761           MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                   ELTEXTCF
01762           STRING WS-TEMP-TEXT-AREA,  ' ',                         ELTEXTCF
01763              CMF-DESCR-LINE(1),        ' ',                       ELTEXTCF
01764              CMF-DESCR-LINE(2),        ' ',                       ELTEXTCF
01765              CMF-DESCR-LINE(3), ' ',        'OF' ' ' WS-PER-DIEM  ELTEXTCF
01766              DELIMITED BY SIZE  INTO  TCAR-FROM-AREA              ELTEXTCF
01767              GO TO 2250-OUTPUT-TEXT                               ELTEXTCF
01768        ELSE                                                       ELTEXTCF
01769         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTEXTCF
01770         STRING CMF-DESCR-LINE(1),        ' ',                     ELTEXTCF
01771            CMF-DESCR-LINE(2),        ' ',                         ELTEXTCF
01772            CMF-DESCR-LINE(3),        ' ',  'OF' ' ' WS-PER-DIEM   ELTEXTCF
01773            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTEXTCF
01774            GO TO 2250-OUTPUT-TEXT.                                ELTEXTCF
01775                                                                   ELTEXTCF
01776      IF FLAT-RATE-PLUS-PERCENT                                    ELTEXTCF
01777       IF WS-PERCENTAGE NOT = ZEROS AND WS-PER-DIEM NOT = ZEROS    ELTEXTCF
01778         IF WS-TEMP-NOT-USED-CNT  =  ZERO                          ELTEXTCF
01779            MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                  ELTEXTCF
01780            STRING WS-TEMP-TEXT-AREA,  ' ',                        ELTEXTCF
01781               CMF-DESCR-LINE(1),        ' ',                      ELTEXTCF
01782               CMF-DESCR-LINE(2),        ' ',                      ELTEXTCF
01783               CMF-DESCR-LINE(3), ' ',        ' ' ';' ' '          ELTEXTCF
01784                WS-FLAT-RATE-IS ' ' WS-PER-DIEM ' ' 'AND'          ELTEXTCF
01785                ' ' WS-PERCENT-REMAINDER                           ELTEXTCF
01786                WS-PRCNT-PERDM-ALLOW                               ELTEXTCF
01787               DELIMITED BY SIZE  INTO  TCAR-FROM-AREA             ELTEXTCF
01788               GO TO 2250-OUTPUT-TEXT                              ELTEXTCF
01789         ELSE                                                      ELTEXTCF
01790           MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN  ELTEXTCF
01791           STRING CMF-DESCR-LINE(1),        ' ',                   ELTEXTCF
01792              CMF-DESCR-LINE(2),        ' ',                       ELTEXTCF
01793              CMF-DESCR-LINE(3), ' ',        ' ' ';' ' '           ELTEXTCF
01794                WS-FLAT-RATE-IS ' ' WS-PER-DIEM ' ' 'AND'          ELTEXTCF
01795                ' ' WS-PERCENT-REMAINDER                           ELTEXTCF
01796                WS-PRCNT-PERDM-ALLOW                               ELTEXTCF
01797              DELIMITED BY SIZE  INTO  TCAR-FROM-AREA              ELTEXTCF
01798              GO TO 2250-OUTPUT-TEXT.                              ELTEXTCF
01799                                                                   ELTEXTCF
01800      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTEXTCF
01801         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTEXTCF
01802         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTEXTCF
01803            CMF-DESCR-LINE(1),        ' ',                         ELTEXTCF
01804            CMF-DESCR-LINE(2),        ' ',                         ELTEXTCF
01805            CMF-DESCR-LINE(3), ' ',        WS-PRCNT-PERDM-ALLOW    ELTEXTCF
01806            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTEXTCF
01807      ELSE                                                         ELTEXTCF
01808         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTEXTCF
01809         STRING CMF-DESCR-LINE(1),        ' ',                     ELTEXTCF
01810            CMF-DESCR-LINE(2),        ' ',                         ELTEXTCF
01811            CMF-DESCR-LINE(3),        ' ',  WS-PRCNT-PERDM-ALLOW   ELTEXTCF
01812            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTEXTCF
01813                                                                   ELTEXTCF
01814  2250-OUTPUT-TEXT.                                                ELTEXTCF
01815      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
01816                                                                   ELTEXTCF
01817      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTEXTCF
01818      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTEXTCF
01819      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTEXTCF
01820                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTEXTCF
01821                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTEXTCF
01822      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
01823                                                                   ELTEXTCF
01824      IF WS-MOVE-LINES-TO-CIA                                      ELTEXTCF
01825         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTEXTCF
01826            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTEXTCF
01827                                             WS-TEMP-NOT-USED-CNT  ELTEXTCF
01828            MOVE '2250'  TO  WS-PARA-ID2                           ELTEXTCF
01829            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTEXTCF
01830               VARYING  WS-SUB1  FROM  1  BY  1                    ELTEXTCF
01831               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTEXTCF
01832            MOVE '2200'  TO  WS-PARA-ID2                           ELTEXTCF
01833            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTEXTCF
01834            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTEXTCF
01835            ADD +1  TO  WS-CIA                                     ELTEXTCF
01836         ELSE                                                      ELTEXTCF
01837            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTEXTCF
01838            ADD +1  TO  WS-CIA.                                    ELTEXTCF
01839                                                                   ELTEXTCF
01840      IF WS-MOVE-LINES-TO-CIA                                      ELTEXTCF
01841         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTEXTCF
01842            MOVE '2260'  TO  WS-PARA-ID2                           ELTEXTCF
01843            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTEXTCF
01844               VARYING  WS-SUB1  FROM  2  BY  1                    ELTEXTCF
01845               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTEXTCF
01846         ELSE                                                      ELTEXTCF
01847            NEXT SENTENCE                                          ELTEXTCF
01848      ELSE                                                         ELTEXTCF
01849         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTEXTCF
01850                                                                   ELTEXTCF
01851      GO TO 2299-EXIT.                                             ELTEXTCF
01852                                                                   ELTEXTCF
01853  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTEXTCF
01854      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTEXTCF
01855      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEXTCF
01856                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEXTCF
01857                                                                   ELTEXTCF
01858  2260-MOVE-LINES-TO-CIA.                                          ELTEXTCF
01859      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTEXTCF
01860      ADD +1  TO  WS-CIA.                                          ELTEXTCF
01861                                                                   ELTEXTCF
01862  2299-EXIT.           EXIT.                                       ELTEXTCF
01863                                                                   ELTEXTCF
01864      TITLE 'READ TABULAR RECORD'.                                 ELTEXTCF
01865 ***************************************************************** ELTEXTCF
01866 *            G E T   T A B U L A R   R E C O R D                  ELTEXTCF
01867 *                                                                 ELTEXTCF
01868 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTEXTCF
01869 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTEXTCF
01870 *  TO DISPLAY.                                                    ELTEXTCF
01871 *                                                                 ELTEXTCF
01872 ***************************************************************** ELTEXTCF
01873  2300-GET-TABULAR-RECORD SECTION.                                 ELTEXTCF
01874      MOVE '2300'  TO  WS-PARA-ID2.                                ELTEXTCF
01875                                                                   ELTEXTCF
01876      SET CIA-GCTABULR-DDN TO TRUE.                                ELTEXTCF
01877      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
01878          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTEXTCF
01879                                                                   ELTEXTCF
01880      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTEXTCF
01881                                                                   ELTEXTCF
01882      SET CIA-GCTABULR-DDN           TO TRUE.                      ELTEXTCF
01883      SET IOP-RD                     TO TRUE.                      ELTEXTCF
01884      SET IOP-FCQ-NONE               TO TRUE.                      ELTEXTCF
01885      SET IOP-KVQ-NONE               TO TRUE.                      ELTEXTCF
01886                                                                   ELTEXTCF
01887      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTEXTCF
01888             COMMAREA(DFHCOMMAREA)                                 ELTEXTCF
01889      END-EXEC.                                                    ELTEXTCF
01890                                                                   ELTEXTCF
01891      IF IOP-RC-NOTFND                                             ELTEXTCF
01892         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTEXTCF
01893         EXEC CICS ABEND                                           ELTEXTCF
01894                   ABCODE(CIA-ABCODE)                              ELTEXTCF
01895         END-EXEC.                                                 ELTEXTCF
01896                                                                   ELTEXTCF
01897      IF NOT IOP-RC-OK                                             ELTEXTCF
01898         SET CIA-AB-CRITIO          TO TRUE                        ELTEXTCF
01899         EXEC CICS ABEND                                           ELTEXTCF
01900                   ABCODE(CIA-ABCODE)                              ELTEXTCF
01901         END-EXEC.                                                 ELTEXTCF
01902                                                                   ELTEXTCF
01903  2399-EXIT.           EXIT.                                       ELTEXTCF
01904                                                                   ELTEXTCF
01905      TITLE 'CODES MANUAL DISPLAY SECTION'.                        ELTEXTCF
01906  2400-CALL-CODES-MANUAL-DISP SECTION.                             ELTEXTCF
01907                                                                   ELTEXTCF
01908      INITIALIZE CMF-RETURN-CODE,                                  ELTEXTCF
01909                 TCAR-FROM-AREA.                                   ELTEXTCF
01910                                                                   ELTEXTCF
01911      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEXTCF
01912      END-EXEC.                                                    ELTEXTCF
01913                                                                   ELTEXTCF
01914      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEXTCF
01915      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
01916          ADDRESS OF CMF-DESCR.                                    ELTEXTCF
01917                                                                   ELTEXTCF
01918      STRING CMF-DESCR-LINE(1) ' '                                 ELTEXTCF
01919             CMF-DESCR-LINE(2)                                     ELTEXTCF
01920              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTEXTCF
01921      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
01922      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTEXTCF
01923      IF WS-INDENT-FOUR-ON                                         ELTEXTCF
01924         MOVE +75   TO TCAR-OUTPUT-FIELD-1-LEN                     ELTEXTCF
01925         MOVE +75   TO TCAR-OUTPUT-FIELD-2-LEN                     ELTEXTCF
01926      ELSE                                                         ELTEXTCF
01927        MOVE +79  TO TCAR-OUTPUT-FIELD-1-LEN                       ELTEXTCF
01928        MOVE +79  TO TCAR-OUTPUT-FIELD-2-LEN.                      ELTEXTCF
01929      PERFORM TCPR-000-TEXT-UNSTRING                               ELTEXTCF
01930      IF WS-INDENT-FOUR-ON                                         ELTEXTCF
01931          MOVE TCAR-OPF-DATA(1) TO WS-DTL-INDENTED-FOUR            ELTEXTCF
01932          MOVE WS-INDENTED-FOUR TO COF-DTL-LINE(WS-CIA)            ELTEXTCF
01933      ELSE                                                         ELTEXTCF
01934        MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).             ELTEXTCF
01935                                                                   ELTEXTCF
01936      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTEXTCF
01937       IF WS-INDENT-FOUR-ON                                        ELTEXTCF
01938           ADD +1                TO WS-CIA                         ELTEXTCF
01939           MOVE TCAR-OPF-DATA(2) TO  WS-DTL-INDENTED-FOUR          ELTEXTCF
01940           MOVE WS-INDENTED-FOUR TO  COF-DTL-LINE(WS-CIA)          ELTEXTCF
01941       ELSE                                                        ELTEXTCF
01942         ADD +1                TO WS-CIA                           ELTEXTCF
01943         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTEXTCF
01944                                                                   ELTEXTCF
01945      ADD +1    TO  WS-CIA.                                        ELTEXTCF
01946      MOVE 'N'  TO  WS-INDENT-FOUR-IND.                            ELTEXTCF
01947      IF NOT WS-ADD-A-BLANK-LINE                                   ELTEXTCF
01948           PERFORM 8000-OUTPUT-TEXT.                               ELTEXTCF
01949  2499-EXIT.   EXIT.                                               ELTEXTCF
01950                                                                   ELTEXTCF
01951      TITLE 'PLACE OF TREATMENT BASIC SECTION'.                    ELTEXTCF
01952  5000-PLACE-OF-TREATMENT-BASIC SECTION.                           ELTEXTCF
01953 **---------------------------------------------------------------+ELTEXTCF
01954 **                                                               |ELTEXTCF
01955 **        P L A C E   O F   T R E A T M E N T                    |ELTEXTCF
01956      SET  PLT-INDEX2  TO  1.                                      ELTEXTCF
01957      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTEXTCF
01958                                                              ZERO ELTEXTCF
01959         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
01960         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEXTCF
01961         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTEXTCF
01962                                                   CMF-CODE-VALUE  ELTEXTCF
01963         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTEXTCF
01964         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTEXTCF
01965         MOVE 'Y'  TO  WS-INDENT-IND                               ELTEXTCF
01966         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTEXTCF
01967  5099-EXIT.  EXIT.                                                ELTEXTCF
01968                                                                   ELTEXTCF
01969      TITLE 'PLACE OF TREATMENT SUPPL SECTION'.                    ELTEXTCF
01970  5100-PLACE-OF-TREATMENT-SUPP SECTION.                            ELTEXTCF
01971 **---------------------------------------------------------------+ELTEXTCF
01972 **                                                               |ELTEXTCF
01973 **        P L A C E   O F   T R E A T M E N T                    |ELTEXTCF
01974      SET  PLT-INDEX2  TO  2.                                      ELTEXTCF
01975      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTEXTCF
01976                                                              ZERO ELTEXTCF
01977         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
01978         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEXTCF
01979         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTEXTCF
01980                                                   CMF-CODE-VALUE  ELTEXTCF
01981         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTEXTCF
01982         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTEXTCF
01983         MOVE 'Y'  TO  WS-INDENT-IND                               ELTEXTCF
01984         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
01985         ADD  +1      TO  WS-CIA.                                  ELTEXTCF
01986  5199-EXIT.  EXIT.                                                ELTEXTCF
01987                                                                   ELTEXTCF
01988      TITLE 'CERTIFICATION REQUIRED SECTION'.                      ELTEXTCF
01989  5500-CERTIFICATION-REQ SECTION.                                  ELTEXTCF
01990      MOVE 'BP'                TO  CMF-RECORD-PREFIX               ELTEXTCF
01991      MOVE 'BEN-PR-ID'         TO  CMF-ELEMENT-SYSTEM-NAME         ELTEXTCF
01992      MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                        ELTEXTCF
01993                                                CMF-CODE-VALUE     ELTEXTCF
01994                                                                   ELTEXTCF
01995      INITIALIZE CMF-RETURN-CODE,                                  ELTEXTCF
01996                 TCAR-FROM-AREA.                                   ELTEXTCF
01997                                                                   ELTEXTCF
01998      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEXTCF
01999      END-EXEC.                                                    ELTEXTCF
02000                                                                   ELTEXTCF
02001      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEXTCF
02002      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
02003          ADDRESS OF CMF-DESCR.                                    ELTEXTCF
02004                                                                   ELTEXTCF
02005      STRING CMF-DESCR-LINE(1) ' '                                 ELTEXTCF
02006             CMF-DESCR-LINE(2)                                     ELTEXTCF
02007                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTEXTCF
02008      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
02009      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTEXTCF
02010      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTEXTCF
02011      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTEXTCF
02012      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
02013      MOVE TCAR-OPF-DATA(1) TO WS-DTL-CERT-REQ-ID.                 ELTEXTCF
02014      MOVE 'BP' TO  CMF-RECORD-PREFIX                              ELTEXTCF
02015      MOVE 'CERTFN-REQRM-IND'   TO  CMF-ELEMENT-SYSTEM-NAME        ELTEXTCF
02016      MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTEXTCF
02017                   TO CMF-CODE-VALUE.                              ELTEXTCF
02018                                                                   ELTEXTCF
02019      INITIALIZE CMF-RETURN-CODE,                                  ELTEXTCF
02020                 TCAR-FROM-AREA.                                   ELTEXTCF
02021                                                                   ELTEXTCF
02022      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEXTCF
02023      END-EXEC.                                                    ELTEXTCF
02024                                                                   ELTEXTCF
02025      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEXTCF
02026      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEXTCF
02027          ADDRESS OF CMF-DESCR.                                    ELTEXTCF
02028                                                                   ELTEXTCF
02029      STRING WS-DTL-CERT-REQ-ID ' '                                ELTEXTCF
02030             CMF-DESCR-LINE(1) ' '                                 ELTEXTCF
02031             CMF-DESCR-LINE(2)                                     ELTEXTCF
02032             CMF-DESCR-LINE(3)                                     ELTEXTCF
02033             CMF-DESCR-LINE(4)                                     ELTEXTCF
02034             CMF-DESCR-LINE(5)                                     ELTEXTCF
02035               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTEXTCF
02036      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
02037      MOVE +07             TO TCAR-OUTPUT-FIELD-COUNT.             ELTEXTCF
02038      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN,             ELTEXTCF
02039                              TCAR-OUTPUT-FIELD-2-LEN,             ELTEXTCF
02040                              TCAR-OUTPUT-FIELD-3-LEN,             ELTEXTCF
02041                              TCAR-OUTPUT-FIELD-4-LEN,             ELTEXTCF
02042                              TCAR-OUTPUT-FIELD-5-LEN,             ELTEXTCF
02043                              TCAR-OUTPUT-FIELD-6-LEN,             ELTEXTCF
02044                              TCAR-OUTPUT-FIELD-7-LEN.             ELTEXTCF
02045      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
02046      IF PLT-INDEX2 = 1                                            ELTEXTCF
02047          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTEXTCF
02048          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTEXTCF
02049      ELSE                                                         ELTEXTCF
02050       MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL                ELTEXTCF
02051       MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA).              ELTEXTCF
02052      ADD +1                TO WS-CIA.                             ELTEXTCF
02053      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTEXTCF
02054            MOVE '5560'  TO  WS-PARA-ID2                           ELTEXTCF
02055            PERFORM 5560-MOVE-LINES-TO-CIA                         ELTEXTCF
02056               VARYING  WS-SUB1  FROM  2  BY  1                    ELTEXTCF
02057               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTEXTCF
02058            MOVE '5500' TO WS-PARA-ID2.                            ELTEXTCF
02059      GO TO 5590-EXIT.                                             ELTEXTCF
02060                                                                   ELTEXTCF
02061  5560-MOVE-LINES-TO-CIA.                                          ELTEXTCF
02062      MOVE TCAR-OPF-DATA(WS-SUB1) TO WS-DTL-INDENTED.              ELTEXTCF
02063      MOVE WS-INDENTED            TO COF-DTL-LINE(WS-CIA).         ELTEXTCF
02064      ADD +1  TO WS-CIA.                                           ELTEXTCF
02065  5590-EXIT.    EXIT.                                              ELTEXTCF
02066                                                                   ELTEXTCF
02067      TITLE 'TABULAR RECORDS SECTION'.                             ELTEXTCF
02068  6000-SCAN-TAB SECTION.                                           ELTEXTCF
02069                                                                   ELTEXTCF
02070      PERFORM 6200-BEN-TAB-AAR.                                    ELTEXTCF
02071      PERFORM 6300-BEN-TAB-PPF.                                    ELTEXTCF
02072      ADD  +1          TO WS-CIA                                   ELTEXTCF
02073      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTEXTCF
02074      ADD  +1          TO WS-CIA                                   ELTEXTCF
02075      PERFORM 8000-OUTPUT-TEXT.                                    ELTEXTCF
02076      PERFORM 6500-BEN-TAB-ADL.                                    ELTEXTCF
02077      PERFORM 6600-BEN-TAB-ABM.                                    ELTEXTCF
02078      PERFORM 6700-BEN-TAB-ACL.                                    ELTEXTCF
02079      PERFORM 6800-BEN-TAB-AOL.                                    ELTEXTCF
02080                                                                   ELTEXTCF
02081  6099-EXIT.  EXIT.                                                ELTEXTCF
02082                                                                   ELTEXTCF
02083      TITLE 'ANCILLARY TEXT '.                                     ELTEXTCF
02084  6050-ANCILLARY-TEXT SECTION.                                     ELTEXTCF
02085      INITIALIZE TCAR-FROM-AREA.                                   ELTEXTCF
02086      STRING WS-ANCILLARY-TEXT1 ' '                                ELTEXTCF
02087             WS-ANCILLARY-TEXT2                                    ELTEXTCF
02088                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTEXTCF
02089      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
02090      MOVE +01              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTEXTCF
02091      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTEXTCF
02092      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
02093      IF WS-CIA > 17                                               ELTEXTCF
02094            PERFORM 8000-OUTPUT-TEXT                               ELTEXTCF
02095            MOVE +1         TO  WS-CIA.                            ELTEXTCF
02096      ADD +1                TO  WS-CIA.                            ELTEXTCF
02097      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTEXTCF
02098      PERFORM 8000-OUTPUT-TEXT.                                    ELTEXTCF
02099  6050-EXIT.   EXIT.                                               ELTEXTCF
02100                                                                   ELTEXTCF
02101      TITLE 'PAY CONSID TEXT '.                                    ELTEXTCF
02102  6100-PAY-CONSID-TEXT SECTION.                                    ELTEXTCF
02103      INITIALIZE TCAR-FROM-AREA.                                   ELTEXTCF
02104      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTEXTCF
02105             WS-PAY-CONSDR-TEXT2                                   ELTEXTCF
02106                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTEXTCF
02107      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEXTCF
02108      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTEXTCF
02109      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTEXTCF
02110                                TCAR-OUTPUT-FIELD-2-LEN.           ELTEXTCF
02111      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEXTCF
02112      IF WS-CIA > 17                                               ELTEXTCF
02113            PERFORM 8000-OUTPUT-TEXT                               ELTEXTCF
02114            MOVE +1            TO WS-CIA.                          ELTEXTCF
02115      ADD +1                TO  WS-CIA.                            ELTEXTCF
02116      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTEXTCF
02117      ADD +1                TO  WS-CIA.                            ELTEXTCF
02118      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTEXTCF
02119      PERFORM 8000-OUTPUT-TEXT.                                    ELTEXTCF
02120  6100-EXIT.   EXIT.                                               ELTEXTCF
02121                                                                   ELTEXTCF
02122      TITLE 'AAR TABULAR'.                                         ELTEXTCF
02123  6200-BEN-TAB-AAR SECTION.                                        ELTEXTCF
02124      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTEXTCF
02125      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02126                                                                   ELTEXTCF
02127      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02128          NOT = LOW-VALUES                                         ELTEXTCF
02129       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02130          NOT = SPACE                                              ELTEXTCF
02131                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTEXTCF
02132                                                                   ELTEXTCF
02133      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02134                                                                   ELTEXTCF
02135      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02136          NOT = LOW-VALUES                                         ELTEXTCF
02137       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02138          NOT = SPACE                                              ELTEXTCF
02139                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTEXTCF
02140                                                                   ELTEXTCF
02141      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTEXTCF
02142             MOVE +2                  TO WS-CIA                    ELTEXTCF
02143             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTEXTCF
02144             PERFORM 8000-OUTPUT-TEXT.                             ELTEXTCF
02145  6200-EXIT.  EXIT.                                                ELTEXTCF
02146                                                                   ELTEXTCF
02147      TITLE 'PPF TABULAR'.                                         ELTEXTCF
02148  6300-BEN-TAB-PPF SECTION.                                        ELTEXTCF
02149      MOVE ZEROS   TO  WS-HOLD1,                                   ELTEXTCF
02150                       WS-HOLD2.                                   ELTEXTCF
02151      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02152      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02153          NOT = LOW-VALUES                                         ELTEXTCF
02154       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02155          NOT = SPACE                                              ELTEXTCF
02156             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02157                        TO  WS-HOLD1.                              ELTEXTCF
02158                                                                   ELTEXTCF
02159      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02160      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02161          NOT = LOW-VALUES                                         ELTEXTCF
02162       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02163          NOT = SPACE                                              ELTEXTCF
02164             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02165                        TO  WS-HOLD2.                              ELTEXTCF
02166                                                                   ELTEXTCF
02167      IF WS-HOLD1 = WS-HOLD2                                       ELTEXTCF
02168         IF WS-HOLD1 = ZEROS                                       ELTEXTCF
02169                 GO TO 6399-EXIT                                   ELTEXTCF
02170         ELSE                                                      ELTEXTCF
02171             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTEXTCF
02172             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02173             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTEXTCF
02174                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02175             END-EXEC                                              ELTEXTCF
02176             GO TO 6399-EXIT.                                      ELTEXTCF
02177                                                                   ELTEXTCF
02178      IF WS-HOLD1 = ZEROS                                          ELTEXTCF
02179             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTEXTCF
02180             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02181             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTEXTCF
02182                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02183             END-EXEC                                              ELTEXTCF
02184      ELSE                                                         ELTEXTCF
02185       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTEXTCF
02186       PERFORM 2300-GET-TABULAR-RECORD                             ELTEXTCF
02187       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTEXTCF
02188                       COMMAREA(DFHCOMMAREA)                       ELTEXTCF
02189       END-EXEC                                                    ELTEXTCF
02190       IF WS-HOLD2 = ZEROS                                         ELTEXTCF
02191            GO TO 6399-EXIT                                        ELTEXTCF
02192       ELSE                                                        ELTEXTCF
02193          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTEXTCF
02194          PERFORM 2300-GET-TABULAR-RECORD                          ELTEXTCF
02195          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTEXTCF
02196                          COMMAREA(DFHCOMMAREA)                    ELTEXTCF
02197          END-EXEC.                                                ELTEXTCF
02198  6399-EXIT.    EXIT.                                              ELTEXTCF
02199                                                                   ELTEXTCF
02200      TITLE 'ADL TABULAR'.                                         ELTEXTCF
02201  6500-BEN-TAB-ADL SECTION.                                        ELTEXTCF
02202      MOVE ZEROS   TO  WS-HOLD1,                                   ELTEXTCF
02203                       WS-HOLD2.                                   ELTEXTCF
02204      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02205      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02206          NOT = LOW-VALUES                                         ELTEXTCF
02207       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02208          NOT = SPACE                                              ELTEXTCF
02209             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02210                        TO  WS-HOLD1.                              ELTEXTCF
02211                                                                   ELTEXTCF
02212      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02213      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02214          NOT = LOW-VALUES                                         ELTEXTCF
02215       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02216          NOT = SPACE                                              ELTEXTCF
02217             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02218                        TO  WS-HOLD2.                              ELTEXTCF
02219                                                                   ELTEXTCF
02220      IF WS-HOLD1 = WS-HOLD2                                       ELTEXTCF
02221         IF WS-HOLD1 = ZEROS                                       ELTEXTCF
02222                 GO TO 6599-EXIT                                   ELTEXTCF
02223         ELSE                                                      ELTEXTCF
02224             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTEXTCF
02225             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02226             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTEXTCF
02227                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02228             END-EXEC                                              ELTEXTCF
02229             GO TO 6599-EXIT.                                      ELTEXTCF
02230                                                                   ELTEXTCF
02231      IF WS-HOLD1 = ZEROS                                          ELTEXTCF
02232             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTEXTCF
02233             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02234             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTEXTCF
02235                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02236             END-EXEC                                              ELTEXTCF
02237      ELSE                                                         ELTEXTCF
02238       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTEXTCF
02239       PERFORM 2300-GET-TABULAR-RECORD                             ELTEXTCF
02240       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTEXTCF
02241                       COMMAREA(DFHCOMMAREA)                       ELTEXTCF
02242       END-EXEC                                                    ELTEXTCF
02243       IF WS-HOLD2 = ZEROS                                         ELTEXTCF
02244           GO TO 6599-EXIT                                         ELTEXTCF
02245       ELSE                                                        ELTEXTCF
02246          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTEXTCF
02247          PERFORM 2300-GET-TABULAR-RECORD                          ELTEXTCF
02248          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTEXTCF
02249                          COMMAREA(DFHCOMMAREA)                    ELTEXTCF
02250          END-EXEC.                                                ELTEXTCF
02251  6599-EXIT.     EXIT.                                             ELTEXTCF
02252                                                                   ELTEXTCF
02253      TITLE 'ABM TABULAR'.                                         ELTEXTCF
02254  6600-BEN-TAB-ABM SECTION.                                        ELTEXTCF
02255      MOVE ZEROS   TO  WS-HOLD1,                                   ELTEXTCF
02256                       WS-HOLD2.                                   ELTEXTCF
02257      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02258      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02259          NOT = LOW-VALUES                                         ELTEXTCF
02260       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02261          NOT = SPACE                                              ELTEXTCF
02262             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02263                        TO  WS-HOLD1.                              ELTEXTCF
02264                                                                   ELTEXTCF
02265      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02266      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02267          NOT = LOW-VALUES                                         ELTEXTCF
02268       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02269          NOT = SPACE                                              ELTEXTCF
02270             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02271                        TO  WS-HOLD2.                              ELTEXTCF
02272                                                                   ELTEXTCF
02273      IF WS-HOLD1 = WS-HOLD2                                       ELTEXTCF
02274         IF WS-HOLD1 = ZEROS                                       ELTEXTCF
02275                 GO TO 6699-EXIT                                   ELTEXTCF
02276         ELSE                                                      ELTEXTCF
02277             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTEXTCF
02278             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02279             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTEXTCF
02280                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02281             END-EXEC                                              ELTEXTCF
02282             GO TO 6699-EXIT.                                      ELTEXTCF
02283                                                                   ELTEXTCF
02284      IF WS-HOLD1 = ZEROS                                          ELTEXTCF
02285             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTEXTCF
02286             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02287             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTEXTCF
02288                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02289             END-EXEC                                              ELTEXTCF
02290      ELSE                                                         ELTEXTCF
02291       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTEXTCF
02292       PERFORM 2300-GET-TABULAR-RECORD                             ELTEXTCF
02293       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTEXTCF
02294                       COMMAREA(DFHCOMMAREA)                       ELTEXTCF
02295       END-EXEC                                                    ELTEXTCF
02296       IF WS-HOLD2 = ZEROS                                         ELTEXTCF
02297           GO TO 6699-EXIT                                         ELTEXTCF
02298       ELSE                                                        ELTEXTCF
02299          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTEXTCF
02300          PERFORM 2300-GET-TABULAR-RECORD                          ELTEXTCF
02301          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTEXTCF
02302                          COMMAREA(DFHCOMMAREA)                    ELTEXTCF
02303          END-EXEC.                                                ELTEXTCF
02304  6699-EXIT.     EXIT.                                             ELTEXTCF
02305                                                                   ELTEXTCF
02306      TITLE 'ACL TABULAR'.                                         ELTEXTCF
02307  6700-BEN-TAB-ACL SECTION.                                        ELTEXTCF
02308      MOVE ZEROS   TO  WS-HOLD1,                                   ELTEXTCF
02309                       WS-HOLD2.                                   ELTEXTCF
02310      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02311      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02312          NOT = LOW-VALUES                                         ELTEXTCF
02313       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02314          NOT = SPACE                                              ELTEXTCF
02315             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02316                        TO  WS-HOLD1.                              ELTEXTCF
02317                                                                   ELTEXTCF
02318      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02319      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02320          NOT = LOW-VALUES                                         ELTEXTCF
02321       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02322          NOT = SPACE                                              ELTEXTCF
02323             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02324                        TO  WS-HOLD2.                              ELTEXTCF
02325                                                                   ELTEXTCF
02326      IF WS-HOLD1 = WS-HOLD2                                       ELTEXTCF
02327         IF WS-HOLD1 = ZEROS                                       ELTEXTCF
02328                 GO TO 6799-EXIT                                   ELTEXTCF
02329         ELSE                                                      ELTEXTCF
02330             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTEXTCF
02331             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02332             EXEC CICS  LINK PROGRAM('ELFMACL')                    ELTEXTCF
02333                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02334             END-EXEC                                              ELTEXTCF
02335             GO TO 6799-EXIT.                                      ELTEXTCF
02336                                                                   ELTEXTCF
02337      IF WS-HOLD1 = ZEROS                                          ELTEXTCF
02338             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTEXTCF
02339             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02340             EXEC CICS  LINK PROGRAM('ELFMACL')                    ELTEXTCF
02341                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02342             END-EXEC                                              ELTEXTCF
02343      ELSE                                                         ELTEXTCF
02344       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTEXTCF
02345       PERFORM 2300-GET-TABULAR-RECORD                             ELTEXTCF
02346       EXEC CICS  LINK PROGRAM('ELFMACL')                          ELTEXTCF
02347                       COMMAREA(DFHCOMMAREA)                       ELTEXTCF
02348       END-EXEC                                                    ELTEXTCF
02349       IF WS-HOLD2 = ZEROS                                         ELTEXTCF
02350            GO TO 6799-EXIT                                        ELTEXTCF
02351       ELSE                                                        ELTEXTCF
02352          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTEXTCF
02353          PERFORM 2300-GET-TABULAR-RECORD                          ELTEXTCF
02354          EXEC CICS  LINK PROGRAM('ELFMACL')                       ELTEXTCF
02355                          COMMAREA(DFHCOMMAREA)                    ELTEXTCF
02356          END-EXEC.                                                ELTEXTCF
02357  6799-EXIT.     EXIT.                                             ELTEXTCF
02358                                                                   ELTEXTCF
02359      TITLE 'AOL TABULAR'.                                         ELTEXTCF
02360  6800-BEN-TAB-AOL SECTION.                                        ELTEXTCF
02361      MOVE ZEROS   TO  WS-HOLD1,                                   ELTEXTCF
02362                       WS-HOLD2.                                   ELTEXTCF
02363      SET PLT-INDEX2 TO 1.                                         ELTEXTCF
02364      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02365          NOT = LOW-VALUES                                         ELTEXTCF
02366       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02367          NOT = SPACE                                              ELTEXTCF
02368             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02369                        TO  WS-HOLD1.                              ELTEXTCF
02370                                                                   ELTEXTCF
02371      SET PLT-INDEX2 TO 2.                                         ELTEXTCF
02372      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTEXTCF
02373          NOT = LOW-VALUES                                         ELTEXTCF
02374       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02375          NOT = SPACE                                              ELTEXTCF
02376             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02377                        TO  WS-HOLD2.                              ELTEXTCF
02378                                                                   ELTEXTCF
02379      IF WS-HOLD1 = WS-HOLD2                                       ELTEXTCF
02380         IF WS-HOLD1 = ZEROS                                       ELTEXTCF
02381                 GO TO 6899-EXIT                                   ELTEXTCF
02382         ELSE                                                      ELTEXTCF
02383             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTEXTCF
02384             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02385             EXEC CICS  LINK PROGRAM('ELFMAOL')                    ELTEXTCF
02386                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02387             END-EXEC                                              ELTEXTCF
02388             GO TO 6899-EXIT.                                      ELTEXTCF
02389                                                                   ELTEXTCF
02390      IF WS-HOLD1 = ZEROS                                          ELTEXTCF
02391             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTEXTCF
02392             PERFORM 2300-GET-TABULAR-RECORD                       ELTEXTCF
02393             EXEC CICS  LINK PROGRAM('ELFMAOL')                    ELTEXTCF
02394                             COMMAREA(DFHCOMMAREA)                 ELTEXTCF
02395             END-EXEC                                              ELTEXTCF
02396      ELSE                                                         ELTEXTCF
02397       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTEXTCF
02398       PERFORM 2300-GET-TABULAR-RECORD                             ELTEXTCF
02399       EXEC CICS  LINK PROGRAM('ELFMAOL')                          ELTEXTCF
02400                       COMMAREA(DFHCOMMAREA)                       ELTEXTCF
02401       END-EXEC                                                    ELTEXTCF
02402       IF WS-HOLD2 = ZEROS                                         ELTEXTCF
02403            GO TO 6899-EXIT                                        ELTEXTCF
02404       ELSE                                                        ELTEXTCF
02405          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTEXTCF
02406          PERFORM 2300-GET-TABULAR-RECORD                          ELTEXTCF
02407          EXEC CICS  LINK PROGRAM('ELFMAOL')                       ELTEXTCF
02408                          COMMAREA(DFHCOMMAREA)                    ELTEXTCF
02409          END-EXEC.                                                ELTEXTCF
02410  6899-EXIT.     EXIT.                                             ELTEXTCF
02411                                                                   ELTEXTCF
02412      TITLE 'SPILLOVER COINSURANCE'.                               ELTEXTCF
02413  7000-SPILLOVER-COINS SECTION.                                    ELTEXTCF
02414 **---------------------------------------------------------------+ELTEXTCF
02415 **                                                               |ELTEXTCF
02416 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTEXTCF
02417      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTEXTCF
02418                                                       NOT =  '0'  ELTEXTCF
02419         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
02420         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTEXTCF
02421                                           CMF-ELEMENT-SYSTEM-NAME ELTEXTCF
02422         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTEXTCF
02423                                           TO   CMF-CODE-VALUE     ELTEXTCF
02424         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTEXTCF
02425         MOVE 56 TO WS-TEMP-NOT-USED-CNT                           ELTEXTCF
02426         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
02427         ADD +1  TO  WS-CIA.                                       ELTEXTCF
02428  7099-EXIT.    EXIT.                                              ELTEXTCF
02429 /                                                                 ELTEXTCF
02430  7200-SPILLOVER-DEDUCT SECTION.                                   ELTEXTCF
02431 **---------------------------------------------------------------+ELTEXTCF
02432 **                                                               |ELTEXTCF
02433 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTEXTCF
02434      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTEXTCF
02435                                                       NOT =  '0'  ELTEXTCF
02436         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEXTCF
02437         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTEXTCF
02438                                           CMF-ELEMENT-SYSTEM-NAME ELTEXTCF
02439         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTEXTCF
02440                                                     CMF-CODE-VALUEELTEXTCF
02441         MOVE WS-SPILLOVER-DEDUCT TO WS-TEMP-TEXT-AREA             ELTEXTCF
02442         MOVE 57 TO WS-TEMP-NOT-USED-CNT                           ELTEXTCF
02443         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTEXTCF
02444         ADD +1  TO  WS-CIA.                                       ELTEXTCF
02445 **                                                               |ELTEXTCF
02446 **---------------------------------------------------------------+ELTEXTCF
02447  7299-EXIT.    EXIT.                                              ELTEXTCF
02448                                                                   ELTEXTCF
02449      TITLE 'TRANSFER TO OTHER RESPONSIBILITY'.                    ELTEXTCF
02450  7300-TRANS-OTHR-RESPON-IND SECTION.                              ELTEXTCF
02451                                                                   ELTEXTCF
02452      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)          =  ZEROES    ELTEXTCF
02453         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZEROES    ELTEXTCF
02454            SET PLT-INDEX2  TO  2                                  ELTEXTCF
02455         ELSE                                                      ELTEXTCF
02456            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTEXTCF
02457            GO TO 7399-EXIT                                        ELTEXTCF
02458      ELSE                                                         ELTEXTCF
02459         SET PLT-INDEX2  TO  1.                                    ELTEXTCF
02460                                                                   ELTEXTCF
02461      IF  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZEROELTEXTCF
02462          GO TO 7399-EXIT.                                         ELTEXTCF
02463                                                                   ELTEXTCF
02464      MOVE 'BP'                     TO  CMF-RECORD-PREFIX.         ELTEXTCF
02465      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTEXTCF
02466      MOVE  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)  TO ELTEXTCF
02467                  CMF-CODE-VALUE.                                  ELTEXTCF
02468      MOVE SPACES                   TO  WS-TEMP-TEXT-AREA.         ELTEXTCF
02469      MOVE +0                       TO  WS-TEMP-NOT-USED-CNT.      ELTEXTCF
02470      PERFORM 2100-CALL-CODES-MANUAL-LONG.                         ELTEXTCF
02471                                                                   ELTEXTCF
02472  7399-EXIT.  EXIT.                                                ELTEXTCF
02473                                                                   ELTEXTCF
02474      TITLE 'OUTPUT TEXT'.                                         ELTEXTCF
02475  8000-OUTPUT-TEXT SECTION.                                        ELTEXTCF
02476       MOVE +0     TO COF-NBR-HDR-LINES.                           ELTEXTCF
02477       MOVE WS-CIA TO COF-NBR-DTL-LINES.                           ELTEXTCF
02478       MOVE ' '    TO  COF-FUNCTION.                               ELTEXTCF
02479       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTEXTCF
02480              COMMAREA(DFHCOMMAREA)                                ELTEXTCF
02481       END-EXEC.                                                   ELTEXTCF
02482       MOVE 1  TO  WS-CIA.                                         ELTEXTCF
02483  8099-EXIT.   EXIT.                                               ELTEXTCF
02484                                                                   ELTEXTCF
02485      COPY ELSTCOMP.                                               ELTEXTCF
02486                                                                   ELTEXTCF
02487      TITLE 'ABEND SECTION'.                                       ELTEXTCF
02488  9999-ABEND SECTION.                                              ELTEXTCF
02489 ******************************************************************ELTEXTCF
02490 *                        A B E N D                                ELTEXTCF
02491 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTEXTCF
02492 *                                                                 ELTEXTCF
02493 ******************************************************************ELTEXTCF
02494                                                                   ELTEXTCF
02495      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTEXTCF
02496                                                                   ELTEXTCF
02497  9999-EXIT.     EXIT.                                             ELTEXTCF
02498      TITLE 'EXTENDED CARE FACILITIES TOPIC'.                      ELTEXTCF
