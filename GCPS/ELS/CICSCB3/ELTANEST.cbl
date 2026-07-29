00001 *      LAST MAINTENANCE TIME:  8.22.03  DATE: 05/23/86            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTANEST
00003  PROGRAM-ID.    ELTANEST.                                            LV002
00004  AUTHOR.        LUCY TORRES.                                      ELTANEST
00005  DATE-WRITTEN.  05/15/86                                          ELTANEST
00006  DATE-COMPILED.                                                   ELTANEST
00007      SKIP3                                                        ELTANEST
00008 ****************************************************************  ELTANEST
00009 *      ELTANEST - ELS:  ANESTHIA TOPIC PROGRAM                 *  ELTANEST
00010 ****************************************************************  ELTANEST
00011      SKIP3                                                        ELTANEST
00012 ****************************************************************  ELTANEST
00013 *              U P D A T E   H I S T O R Y                     *  ELTANEST
00014 *                                                              *  ELTANEST
00015 *   DATE    PGM  DESCRIPTION                                   *  ELTANEST
00016 * --------  ---  --------------------------------------------- *  ELTANEST
00017 * 05/15/86  LET  ORIGINAL VERSION                              *  ELTANEST
00018 * 06/17/86  LET  DISCREPANCY FIX                               *  ELTANEST
00019 * 06/27/86  LET  DISCREPANCY FIX #174                          *  ELTANEST
00020 * 08/13/86  LET  USING A HEADER LINE FROM THE PROLOG           *  ELTANEST
00021 * 09/29/86  JTC  COBOL VS II CONVERSION                        *  ELTANEST
00022 * 10/19/87  NAC  REWORD PHRASE FOR COVERED BENEFITS.           *  ELTANEST
00023 * 04/06/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS               *  ELTANEST
00024 * 10/17/89  RKH  ADDED TRANSFER TO OTHER RESPONSIBILITY IND    *  ELTANEST
00025 * 10/14/90  GEM  ADDED BENEFIT PROVISIONS                      *  ELTANEST
00026 * 11/09/90  GEM  IN PARAGRAPH 5165-TRANSF-OTHER-RESP:          *  ELTANEST
00027 *                             CHANGE 'NOT = 0' TO 'NOT = ZERO' *  ELTANEST
00028 * *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST   ELTANEST
00029 ****************************************************************  ELTANEST
00030      SKIP3                                                        ELTANEST
00031  ENVIRONMENT DIVISION.                                            ELTANEST
00032      SKIP3                                                        ELTANEST
00033  DATA DIVISION.                                                   ELTANEST
00034  WORKING-STORAGE SECTION.                                         ELTANEST
00035                                                                   ELTANEST
00036  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTANEST
00037          '** ELTANEST WS BEGINS **'.                              ELTANEST
00038  01  WS-PARA-ID1                 PIC  X(04) VALUE 'XXXX'.         ELTANEST
00039  01  WS-PARA-ID2                 PIC  X(04) VALUE 'XXXX'.         ELTANEST
00040  01  WS-PARA-ID3                 PIC  X(04) VALUE 'XXXX'.         ELTANEST
00041  01  WS-ABEND-CODE               PIC  X(04) VALUE 'ANES'.         ELTANEST
00042 /                                                                 ELTANEST
00043 ****************************************************************  ELTANEST
00044 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTANEST
00045 ****************************************************************  ELTANEST
00046  01  WORK-FIELDS.                                                 ELTANEST
00047      05  WS-CHAR-0               PIC  X(01).                      ELTANEST
00048      05  WS-CIA-PNTR             PIC S9(08) COMP   VALUE ZEROS.   ELTANEST
00049      05  WS-GRPSP-RECORD-PNTR    PIC S9(08) COMP   VALUE ZEROS.   ELTANEST
00050      05  WS-SUB                  PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00051      05  WS-SUB1                 PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00052      05  WS-SUB2                 PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00053      05  WS-SUB3                 PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00054      05  WS-SUB4                 PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00055      05  WS-CIA                  PIC S9(03) COMP-3 VALUE +0.      ELTANEST
00056      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP-3.               ELTANEST
00057      05  WS-PERCENT-FLD.                                          ELTANEST
00058        10  WS-PERCENTAGE         PIC ZZ9.                         ELTANEST
00059        10  WS-PERCENT-SIGN       PIC X.                           ELTANEST
00060      05  WS-EXPLANATION-IND      PIC S9 COMP-3.                   ELTANEST
00061          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTANEST
00062          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTANEST
00063          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTANEST
00064          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTANEST
00065          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTANEST
00066          88  WS-NO-EXPLANATION             VALUE +0.              ELTANEST
00067      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP-3.                   ELTANEST
00068      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP-3.                   ELTANEST
00069                                                                   ELTANEST
00070  01  WS-EXPLAINS.                                                 ELTANEST
00071    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTANEST
00072    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTANEST
00073    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTANEST
00074    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTANEST
00075                                                                   ELTANEST
00076  01  SWITCHES.                                                    ELTANEST
00077      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTANEST
00078          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTANEST
00079      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTANEST
00080          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTANEST
00081      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTANEST
00082          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTANEST
00083      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTANEST
00084          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTANEST
00085                                                                   ELTANEST
00086 *--------------------------------------------------------------*  ELTANEST
00087 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTANEST
00088 *--------------------------------------------------------------*  ELTANEST
00089  01  WS-BEN-PROV-IDS.                                             ELTANEST
00090      05  WS-TABLE-MAX-CNT        PIC S9(04) VALUE +6 COMP.        ELTANEST
00091      05  WS-PROF-IP-CNT          PIC S9(04) VALUE +6 COMP.        ELTANEST
00092      05  WS-PROF-IP-TABS.                                         ELTANEST
00093          10  FILLER              PIC  X(06) VALUE 'ACAI C'.       ELTANEST
00094          10  FILLER              PIC  X(06) VALUE 'ANSI C'.       ELTANEST
00095          10  FILLER              PIC  X(06) VALUE 'ANSL C'.       ELTANEST
00096          10  FILLER              PIC  X(06) VALUE 'APFI B'.       ELTANEST
00097          10  FILLER              PIC  X(06) VALUE 'ETAI C'.       ELTANEST
00098          10  FILLER              PIC  X(06) VALUE 'PRIO C'.       ELTANEST
00099      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTANEST
00100                                                                   ELTANEST
00101                                  PIC  X(06) OCCURS 6 TIMES.       ELTANEST
00102      05  WS-PROF-OP-CNT          PIC S9(04) VALUE +6 COMP.        ELTANEST
00103      05  WS-PROF-OP-TABS.                                         ELTANEST
00104          10  FILLER              PIC  X(06) VALUE 'ACAO C'.       ELTANEST
00105          10  FILLER              PIC  X(06) VALUE 'ANSL C'.       ELTANEST
00106          10  FILLER              PIC  X(06) VALUE 'ANSO C'.       ELTANEST
00107          10  FILLER              PIC  X(06) VALUE 'APFO B'.       ELTANEST
00108          10  FILLER              PIC  X(06) VALUE 'ETAO C'.       ELTANEST
00109          10  FILLER              PIC  X(06) VALUE 'PRIO C'.       ELTANEST
00110      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTANEST
00111                                  PIC  X(06) OCCURS 6 TIMES.       ELTANEST
00112                                                                   ELTANEST
00113 /                                                                 ELTANEST
00114 ****************************************************************  ELTANEST
00115 *              HEADER AND LITERAL TEXT AREA                    *  ELTANEST
00116 ****************************************************************  ELTANEST
00117  01  HEADER-LINE-2.                                               ELTANEST
00118      05  FILLER                  PIC  X(12) VALUE 'SECTION NO: '. ELTANEST
00119      05  WS-SECT-NO              PIC  9(05) VALUE ZEROS.          ELTANEST
00120      05  FILLER                  PIC  X(23) VALUE                 ELTANEST
00121              '       EFFECTIVE DATE: '.                           ELTANEST
00122      05  WS-EFF-DATE             PIC 99/99/99.                    ELTANEST
00123      05  FILLER                  PIC  X(29) VALUE                 ELTANEST
00124              '        FAMILY RELATIONSHIP: '.                     ELTANEST
00125      05  WS-FAM-REL              PIC  9(01) VALUE ZERO.           ELTANEST
00126      05  FILLER                  PIC  X(01) VALUE LOW-VALUE.      ELTANEST
00127                                                                   ELTANEST
00128  01  HEADER-I-LINE-3.                                             ELTANEST
00129      05  FILLER                  PIC  X(21) VALUE SPACES.         ELTANEST
00130      05  FILLER                  PIC  X(33) VALUE                 ELTANEST
00131              'ANESTHESIA SERVICES INSTITUTIONAL'.                 ELTANEST
00132      05  FILLER                  PIC  X(25) VALUE LOW-VALUES.     ELTANEST
00133                                                                   ELTANEST
00134  01  HEADER-P-IP-LINE-3.                                          ELTANEST
00135      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTANEST
00136      05  FILLER                  PIC  X(42) VALUE                 ELTANEST
00137              'ANESTHESIA SERVICES INPATIENT PROFESSIONAL'.        ELTANEST
00138      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTANEST
00139                                                                   ELTANEST
00140  01  HEADER-P-OP-LINE-3.                                          ELTANEST
00141      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTANEST
00142      05  FILLER                  PIC  X(43) VALUE                 ELTANEST
00143              'ANESTHESIA SERVICES OUTPATIENT PROFESSIONAL'.       ELTANEST
00144      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTANEST
00145                                                                   ELTANEST
00146  01  INST-LITERALS.                                               ELTANEST
00147      05  FILLER                  PIC  X(45) VALUE                 ELTANEST
00148          'IF SURGERY IS ELIGIBLE ANESTHESIA IS PAYABLE.'.         ELTANEST
00149      05  FILLER                  PIC  X(34) VALUE LOW-VALUES.     ELTANEST
00150      05  FILLER                  PIC  X(53) VALUE                 ELTANEST
00151          'SEE ROOM AND BOARD FOR ADDITIONAL INPATIENT BENEFITS.'. ELTANEST
00152      05  FILLER                  PIC  X(26) VALUE LOW-VALUES.     ELTANEST
00153      05  FILLER                  PIC  X(47) VALUE                 ELTANEST
00154          'SEE OUTPATIENT SURGERY FOR OUTPATIENT BENEFITS.'.       ELTANEST
00155      05  FILLER                  PIC  X(32) VALUE LOW-VALUES.     ELTANEST
00156                                                                   ELTANEST
00157  01  I-L-L-RED  REDEFINES  INST-LITERALS.                         ELTANEST
00158      05  INST-LITERAL-LINE       PIC  X(79) OCCURS 3 TIMES.       ELTANEST
00159                                                                   ELTANEST
00160  01  WS-FOLLOWING-BEN.                                            ELTANEST
00161      05  FILLER                    PIC X(21) VALUE                ELTANEST
00162          'COVERED SERVICES ARE:'.                                 ELTANEST
00163                                                                   ELTANEST
00164  01  WS-PAY-CONSDR-TEXT1.                                         ELTANEST
00165      05  FILLER                    PIC X(45)                      ELTANEST
00166        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTANEST
00167  01  WS-PAY-CONSDR-TEXT2.                                         ELTANEST
00168      05  FILLER                    PIC X(44)                      ELTANEST
00169        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTANEST
00170                                                                   ELTANEST
00171  01  WS-SERVICES-RENDERED.                                        ELTANEST
00172      05  FILLER                  PIC  X(25) VALUE                 ELTANEST
00173              'SERVICES MAY BE RENDERED '.                         ELTANEST
00174      05  FILLER                  PIC  X(54) VALUE LOW-VALUES.     ELTANEST
00175                                                                   ELTANEST
00176  01  WS-PAYMNT-BASED.                                             ELTANEST
00177      10  FILLER                  PIC  X(20) VALUE                 ELTANEST
00178              'PAYMENT IS BASED ON '.                              ELTANEST
00179      10  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTANEST
00180                                                                   ELTANEST
00181  01  WS-BASIC.                                                    ELTANEST
00182      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTANEST
00183              '         BASIC: '.                                  ELTANEST
00184      05  WS-DTL-BASIC-LONG.                                       ELTANEST
00185          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTANEST
00186          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTANEST
00187                                                                   ELTANEST
00188  01  WS-SUPPLEMENTAL.                                             ELTANEST
00189      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTANEST
00190              '  SUPPLEMENTAL: '.                                  ELTANEST
00191      05  WS-DTL-SUPP-LONG.                                        ELTANEST
00192          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTANEST
00193          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTANEST
00194                                                                   ELTANEST
00195  01  WS-SAME-PROV.                                                ELTANEST
00196      05  FILLER                  PIC  X(32) VALUE                 ELTANEST
00197              'IF THE SAME PROVIDER IS BILLING:'.                  ELTANEST
00198      05  FILLER                  PIC  X(47) VALUE LOW-VALUES.     ELTANEST
00199                                                                   ELTANEST
00200  01  WS-ELEC-SHOCK.                                               ELTANEST
00201      05  FILLER                  PIC  X(36) VALUE                 ELTANEST
00202              '  ELECTRIC SHOCK THERAPY/ANESTHESIA '.              ELTANEST
00203      05  FILLER                  PIC  X(43) VALUE LOW-VALUES.     ELTANEST
00204                                                                   ELTANEST
00205  01  WS-NEWBORN.                                                  ELTANEST
00206      05  FILLER                  PIC  X(35) VALUE                 ELTANEST
00207              '  NEWBORN EXAM/DELIVERY/ANESTHESIA '.               ELTANEST
00208      05  FILLER                  PIC  X(44) VALUE LOW-VALUES.     ELTANEST
00209                                                                   ELTANEST
00210  01  WS-SURGERY.                                                  ELTANEST
00211      05  FILLER                  PIC  X(39) VALUE                 ELTANEST
00212              '  SURGERY/ANESTHESIA/ASSISTANT SURGERY '.           ELTANEST
00213      05  FILLER                  PIC  X(40) VALUE LOW-VALUES.     ELTANEST
00214                                                                   ELTANEST
00215  01  WS-ELIG-METH-TREAT.                                          ELTANEST
00216      05  FILLER                  PIC  X(37) VALUE                 ELTANEST
00217              'THE ELIGIBLE METHOD OF TREATMENT IS: '.             ELTANEST
00218      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTANEST
00219                                                                   ELTANEST
00220  01  WS-PVE.                                                      ELTANEST
00221      05  FILLER                  PIC  X(44) VALUE                 ELTANEST
00222              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTANEST
00223      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTANEST
00224                                                                   ELTANEST
00225  01  WS-INDICES-PROBLEM.                                          ELTANEST
00226      05  FILLER                  PIC  X(20) VALUE                 ELTANEST
00227              'PROBLEM WITH INDICES'.                              ELTANEST
00228      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTANEST
00229                                                                   ELTANEST
00230  01  WS-INVALID-REQ.                                              ELTANEST
00231      05  FILLER                  PIC  X(37) VALUE                 ELTANEST
00232              '*** I N V A L I D   R E Q U E S T ***'.             ELTANEST
00233      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTANEST
00234                                                                   ELTANEST
00235  01  WS-PAYABLE-AS.                                               ELTANEST
00236      05  FILLER                    PIC X(45) VALUE                ELTANEST
00237          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTANEST
00238      05  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTANEST
00239                                                                   ELTANEST
00240  01  WS-POSSIBLE-ERROR.                                           ELTANEST
00241      05  FILLER                   PIC  X(50) VALUE                ELTANEST
00242              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTANEST
00243      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTANEST
00244                                                                   ELTANEST
00245  01  WS-SPILLOVER.                                                ELTANEST
00246      05  FILLER                  PIC  X(10) VALUE                 ELTANEST
00247              'SPILLOVER '.                                        ELTANEST
00248                                                                   ELTANEST
00249  01  WS-CONTRACT-RELATED.                                         ELTANEST
00250      05  FILLER                   PIC  X(48) VALUE                ELTANEST
00251              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTANEST
00252      05  FILLER                   PIC  X(31) VALUE LOW-VALUES.    ELTANEST
00253                                                                   ELTANEST
00254  01  WS-OTHER-LITERALS.                                           ELTANEST
00255    05  WS-NO-TABULAR1.                                            ELTANEST
00256      10  FILLER                    PIC X(51)  VALUE               ELTANEST
00257         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTANEST
00258      10  FILLER                    PIC X(22)  VALUE               ELTANEST
00259         'GOING FROM BENEFIT ***'.                                 ELTANEST
00260                                                                   ELTANEST
00261    05  WS-NO-TABULAR2.                                            ELTANEST
00262      10  FILLER                    PIC X(15)  VALUE               ELTANEST
00263         '*** PROVISION: '.                                        ELTANEST
00264      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTANEST
00265      10  FILLER                    PIC X VALUE SPACE.             ELTANEST
00266      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTANEST
00267      10  FILLER                    PIC X(13)  VALUE               ELTANEST
00268         ' TO TABULAR: '.                                          ELTANEST
00269      10  WS-NO-TAB-ID              PIC X(6).                      ELTANEST
00270      10  FILLER                    PIC X VALUE SPACE.             ELTANEST
00271      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTANEST
00272      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTANEST
00273                                                                   ELTANEST
00274    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTANEST
00275    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTANEST
00276      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTANEST
00277                                                                   ELTANEST
00278 /                                                                 ELTANEST
00279  LINKAGE SECTION.                                                 ELTANEST
00280  01  DFHCOMMAREA.                                                 ELTANEST
00281      COPY ELSCOMMC.                                               ELTANEST
00282 /                                                                 ELTANEST
00283      COPY ELSCIA2C.                                               ELTANEST
00284 /                                                                 ELTANEST
00285 ***  IO PARM AREA ***                                             ELTANEST
00286      COPY ELSIOPMC.                                               ELTANEST
00287 /                                                                 ELTANEST
00288      COPY ELSKEYSC.                                               ELTANEST
00289 /                                                                 ELTANEST
00290      COPY ELSOUTPC.                                               ELTANEST
00291 /                                                                 ELTANEST
00292      COPY ELSSSCBC.                                               ELTANEST
00293 /                                                                 ELTANEST
00294      COPY ELSCMIFC.                                               ELTANEST
00295 /                                                                 ELTANEST
00296      COPY ELSCMDSC.                                               ELTANEST
00297 /                                                                 ELTANEST
00298      COPY ELSPRVNC.                                               ELTANEST
00299 /                                                                 ELTANEST
00300 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTANEST
00301      COPY ELSPLGSW.                                               ELTANEST
00302 *** BENEFIT PROVISION TABLE OF FLDS ***                           ELTANEST
00303      COPY ELSPLGTB.                                               ELTANEST
00304 /                                                                 ELTANEST
00305      COPY ELSTCWAC.                                               ELTANEST
00306 *--------------------------------------------------------------*  ELTANEST
00307 *              C O N T R A C T   R E C O R D                   *  ELTANEST
00308 *--------------------------------------------------------------*  ELTANEST
00309  01  CONTRACT-RECORD.                                             ELTANEST
00310      COPY GCCONTRC.                                               ELTANEST
00311 /                                                                 ELTANEST
00312  PROCEDURE DIVISION.                                              ELTANEST
00313  0000-MAINLINE.                                                   ELTANEST
00314                                                                   ELTANEST
00315      PERFORM 1000-INITIALIZATION                                  ELTANEST
00316         THRU 1000-EXIT.                                           ELTANEST
00317                                                                   ELTANEST
00318      PERFORM 2000-PROCESS-RTN                                     ELTANEST
00319         THRU 2000-EXIT.                                           ELTANEST
00320                                                                   ELTANEST
00321      EXEC CICS RETURN END-EXEC.                                   ELTANEST
00322                                                                   ELTANEST
00323      GOBACK.                                                      ELTANEST
00324 /                                                                 ELTANEST
00325  1000-INITIALIZATION.                                             ELTANEST
00326 ****************************************************************  ELTANEST
00327 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTANEST
00328 ****************************************************************  ELTANEST
00329                                                                   ELTANEST
00330      MOVE '1000'  TO  WS-PARA-ID1.                                ELTANEST
00331                                                                   ELTANEST
00332      IF EIBCALEN  NOT = LENGTH OF DFHCOMMAREA                     ELTANEST
00333            SET CIA-AB-DFHCOMMAREA TO TRUE                         ELTANEST
00334            EXEC CICS ABEND ABCODE (CIA-ABCODE)  END-EXEC.         ELTANEST
00335                                                                   ELTANEST
00336      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTANEST
00337          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTANEST
00338                                                                   ELTANEST
00339      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTANEST
00340      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00341          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTANEST
00342                                                                   ELTANEST
00343      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTANEST
00344      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00345          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTANEST
00346                                                                   ELTANEST
00347      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTANEST
00348      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00349          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTANEST
00350                                                                   ELTANEST
00351      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTANEST
00352      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00353          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTANEST
00354                                                                   ELTANEST
00355      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTANEST
00356      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00357          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTANEST
00358                                                                   ELTANEST
00359      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTANEST
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00361          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTANEST
00362                                                                   ELTANEST
00363      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTANEST
00364                                                                   ELTANEST
00365      MOVE '0'  TO WS-CHAR-0.                                      ELTANEST
00366                                                                   ELTANEST
00367      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTANEST
00368                                                                   ELTANEST
00369      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTANEST
00370                            (WS-TABLE-MAX-CNT        *             ELTANEST
00371                             LENGTH OF PVN-BEN-PROVN-TBL).         ELTANEST
00372                                                                   ELTANEST
00373      SET CIA-STG-GETMAIN TO TRUE.                                 ELTANEST
00374                                                                   ELTANEST
00375      EXEC CICS LINK                                               ELTANEST
00376                PROGRAM('ELUSTGMG')                                ELTANEST
00377                COMMAREA(DFHCOMMAREA)                              ELTANEST
00378      END-EXEC.                                                    ELTANEST
00379  1000-EXIT.  EXIT.                                                ELTANEST
00380 /                                                                 ELTANEST
00381  2000-PROCESS-RTN.                                                ELTANEST
00382 ****************************************************************  ELTANEST
00383 *               ANESTHSIA TOPIC PROCESSING                     *  ELTANEST
00384 ****************************************************************  ELTANEST
00385                                                                   ELTANEST
00386      IF SSB-PROV-CLASS-INST                                       ELTANEST
00387          PERFORM 3000-INSTITUTIONAL                               ELTANEST
00388             THRU 3000-EXIT                                        ELTANEST
00389      ELSE                                                         ELTANEST
00390          IF SSB-PROV-CLASS-PROF                                   ELTANEST
00391              PERFORM 4000-PROFESSIONAL                            ELTANEST
00392                 THRU 4000-EXIT                                    ELTANEST
00393          ELSE                                                     ELTANEST
00394              IF SSB-PROV-CLASS-BOTH                               ELTANEST
00395                  PERFORM 3000-INSTITUTIONAL                       ELTANEST
00396                     THRU 3000-EXIT                                ELTANEST
00397                  PERFORM 4000-PROFESSIONAL                        ELTANEST
00398                     THRU 4000-EXIT                                ELTANEST
00399              ELSE                                                 ELTANEST
00400                  MOVE ' '             TO  COF-FUNCTION            ELTANEST
00401                  MOVE +0              TO  COF-NBR-HDR-LINES       ELTANEST
00402                  MOVE +2              TO  COF-NBR-DTL-LINES       ELTANEST
00403                  MOVE WS-INVALID-REQ  TO  COF-HDR-LINE (2)        ELTANEST
00404                  EXEC CICS  LINK  PROGRAM('ELUOUTPT')             ELTANEST
00405                                   COMMAREA(DFHCOMMAREA)           ELTANEST
00406                                   END-EXEC.                       ELTANEST
00407                                                                   ELTANEST
00408      MOVE 'E'   TO  COF-FUNCTION.                                 ELTANEST
00409      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTANEST
00410                                                                   ELTANEST
00411      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTANEST
00412                     COMMAREA (DFHCOMMAREA)                        ELTANEST
00413                     END-EXEC.                                     ELTANEST
00414                                                                   ELTANEST
00415  2000-EXIT.  EXIT.                                                ELTANEST
00416 /                                                                 ELTANEST
00417 ****************************************************************  ELTANEST
00418 *            ANESTHSIA INSTITUTIONAL PROCESSING                *  ELTANEST
00419 ****************************************************************  ELTANEST
00420  3000-INSTITUTIONAL.                                              ELTANEST
00421                                                                   ELTANEST
00422      MOVE HEADER-I-LINE-3  TO  COF-HDR-LINE (2).                  ELTANEST
00423                                                                   ELTANEST
00424      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTANEST
00425         THRU 9100-EXIT.                                           ELTANEST
00426                                                                   ELTANEST
00427      MOVE INST-LITERAL-LINE (1)  TO  COF-DTL-LINE (1).            ELTANEST
00428      MOVE INST-LITERAL-LINE (2)  TO  COF-DTL-LINE (3).            ELTANEST
00429      MOVE INST-LITERAL-LINE (3)  TO  COF-DTL-LINE (5).            ELTANEST
00430      MOVE +5                     TO  WS-CIA.                      ELTANEST
00431                                                                   ELTANEST
00432      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
00433         THRU 9200-EXIT.                                           ELTANEST
00434                                                                   ELTANEST
00435      PERFORM 7000-PAY-CONSID-TEXT                                 ELTANEST
00436         THRU 7000-EXIT.                                           ELTANEST
00437                                                                   ELTANEST
00438  3000-EXIT.  EXIT.                                                ELTANEST
00439 /                                                                 ELTANEST
00440  4000-PROFESSIONAL.                                               ELTANEST
00441 ****************************************************************  ELTANEST
00442 *                PROFESSIONAL INFORMATION                      *  ELTANEST
00443 ****************************************************************  ELTANEST
00444                                                                   ELTANEST
00445      MOVE '4000'  TO  WS-PARA-ID1.                                ELTANEST
00446                                                                   ELTANEST
00447 *----------  FIRST DO INPATIENT AND THEN OUTPATIENT -----------*  ELTANEST
00448                                                                   ELTANEST
00449      PERFORM 5000-PROF-IP                                         ELTANEST
00450         THRU 5000-EXIT.                                           ELTANEST
00451                                                                   ELTANEST
00452      PERFORM 6000-PROF-OP                                         ELTANEST
00453         THRU 6000-EXIT.                                           ELTANEST
00454                                                                   ELTANEST
00455  4000-EXIT.  EXIT.                                                ELTANEST
00456 /                                                                 ELTANEST
00457 ****************************************************************  ELTANEST
00458 *             PROFESSIONAL INPATIENT INFORMATION               *  ELTANEST
00459 ****************************************************************  ELTANEST
00460  5000-PROF-IP.                                                    ELTANEST
00461                                                                   ELTANEST
00462      MOVE '5000'  TO  WS-PARA-ID1.                                ELTANEST
00463      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTANEST
00464                                                                   ELTANEST
00465      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTANEST
00466                                                                   ELTANEST
00467      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTANEST
00468         THRU 9100-EXIT.                                           ELTANEST
00469                                                                   ELTANEST
00470                                                                   ELTANEST
00471      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTANEST
00472      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00473          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTANEST
00474                                                                   ELTANEST
00475      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTANEST
00476                                                                   ELTANEST
00477      PERFORM 5010-MOVE-IN-PROF-IP-TABS                            ELTANEST
00478         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTANEST
00479                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTANEST
00480                                                                   ELTANEST
00481      PERFORM 5020-CALL-COVERAGE                                   ELTANEST
00482         THRU 5020-EXIT.                                           ELTANEST
00483                                                                   ELTANEST
00484      IF PVN-COVG-NONE                                             ELTANEST
00485          GO TO 5000-EXIT.                                         ELTANEST
00486                                                                   ELTANEST
00487      PERFORM 5030-FIND-FIRST-NONZERO                              ELTANEST
00488         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTANEST
00489                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTANEST
00490                                                                   ELTANEST
00491  5000-EXIT.  EXIT.                                                ELTANEST
00492 /                                                                 ELTANEST
00493  5010-MOVE-IN-PROF-IP-TABS.                                       ELTANEST
00494      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
00495      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTANEST
00496            TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).              ELTANEST
00497                                                                   ELTANEST
00498      MOVE ZERO  TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),          ELTANEST
00499                    PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).          ELTANEST
00500                                                                   ELTANEST
00501  5010-EXIT.  EXIT.                                                ELTANEST
00502      SKIP3                                                        ELTANEST
00503  5020-CALL-COVERAGE.                                              ELTANEST
00504      MOVE '5020'  TO  WS-PARA-ID1.                                ELTANEST
00505                                                                   ELTANEST
00506      MOVE 'ANESTHESIA SERVICES ARE'  TO  SSB-TOPIC-PHRASE.        ELTANEST
00507                                                                   ELTANEST
00508      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTANEST
00509                     COMMAREA (DFHCOMMAREA)                        ELTANEST
00510                     END-EXEC.                                     ELTANEST
00511                                                                   ELTANEST
00512      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTANEST
00513                     COMMAREA (DFHCOMMAREA)                        ELTANEST
00514                     END-EXEC.                                     ELTANEST
00515                                                                   ELTANEST
00516      IF PVN-COVG-NONE                                             ELTANEST
00517          GO TO 5020-EXIT.                                         ELTANEST
00518                                                                   ELTANEST
00519      MOVE +1  TO  WS-CIA.                                         ELTANEST
00520                                                                   ELTANEST
00521      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTANEST
00522                                                                   ELTANEST
00523      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTANEST
00524                    PSP-PROVN-PRICING-METHD,                       ELTANEST
00525                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTANEST
00526                    PSP-TRANSF-OTHER-RESP-IND,                     ELTANEST
00527                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTANEST
00528                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTANEST
00529                    PSP-SPILL-OVER-DED-APL-IND,                    ELTANEST
00530                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTANEST
00531                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTANEST
00532                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTANEST
00533                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTANEST
00534                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTANEST
00535                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTANEST
00536                    PSC-BEN-SCOPE-ID,                              ELTANEST
00537                    PSC-ELIG-METHD-OF-TREAT-IND.                   ELTANEST
00538                                                                   ELTANEST
00539      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTANEST
00540                     COMMAREA (DFHCOMMAREA)                        ELTANEST
00541                     END-EXEC.                                     ELTANEST
00542                                                                   ELTANEST
00543      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTANEST
00544      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
00545          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTANEST
00546                                                                   ELTANEST
00547  5020-EXIT.  EXIT.                                                ELTANEST
00548      SKIP3                                                        ELTANEST
00549  5030-FIND-FIRST-NONZERO.                                         ELTANEST
00550      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
00551                                                                   ELTANEST
00552      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTANEST
00553          NEXT SENTENCE                                            ELTANEST
00554      ELSE                                                         ELTANEST
00555          PERFORM 5100-BUILD-SCREEN-LINES                          ELTANEST
00556             THRU 5100-EXIT.                                       ELTANEST
00557                                                                   ELTANEST
00558  5030-EXIT.  EXIT.                                                ELTANEST
00559 /                                                                 ELTANEST
00560  5100-BUILD-SCREEN-LINES.                                         ELTANEST
00561      MOVE '5100'  TO  WS-PARA-ID1.                                ELTANEST
00562      SET PLT-INDEX1  TO                                           ELTANEST
00563              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTANEST
00564                                                                   ELTANEST
00565      IF WS-NOT-FIRST-TIME                                         ELTANEST
00566         MOVE 'P'    TO COF-FUNCTION                               ELTANEST
00567         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTANEST
00568         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTANEST
00569                          COMMAREA(DFHCOMMAREA)                    ELTANEST
00570         END-EXEC                                                  ELTANEST
00571      ELSE                                                         ELTANEST
00572        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTANEST
00573                                                                   ELTANEST
00574      MOVE  +1  TO  WS-CIA.                                        ELTANEST
00575                                                                   ELTANEST
00576      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTANEST
00577          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTANEST
00578              SET PLT-INDEX2  TO  2                                ELTANEST
00579          ELSE                                                     ELTANEST
00580              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTANEST
00581              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTANEST
00582                 THRU 9200-EXIT                                    ELTANEST
00583              GO TO 5100-EXIT                                      ELTANEST
00584      ELSE                                                         ELTANEST
00585          SET PLT-INDEX2  TO  1.                                   ELTANEST
00586                                                                   ELTANEST
00587      PERFORM 5105-LIST-BEN-PROV                                   ELTANEST
00588         THRU 5105-EXIT.                                           ELTANEST
00589                                                                   ELTANEST
00590      PERFORM 5110-PLACE-OF-TREATMENT                              ELTANEST
00591         THRU 5110-EXIT.                                           ELTANEST
00592                                                                   ELTANEST
00593      PERFORM 5120-BENEFIT-SCOPE                                   ELTANEST
00594         THRU 5120-EXIT.                                           ELTANEST
00595                                                                   ELTANEST
00596      PERFORM 5130-PRIC-METH                                       ELTANEST
00597         THRU 5130-EXIT.                                           ELTANEST
00598                                                                   ELTANEST
00599      PERFORM 5140-SAME-PROVIDER                                   ELTANEST
00600         THRU 5140-EXIT.                                           ELTANEST
00601                                                                   ELTANEST
00602      PERFORM 5150-ELIG-METH-TREATMENT                             ELTANEST
00603         THRU 5150-EXIT.                                           ELTANEST
00604                                                                   ELTANEST
00605      PERFORM 5160-SPILLOVR-COINS-N-DEDUC                          ELTANEST
00606         THRU 5160-EXIT.                                           ELTANEST
00607                                                                   ELTANEST
00608      PERFORM 5165-TRANSF-OTHER-RESP  THRU                         ELTANEST
00609              5165-EXIT.                                           ELTANEST
00610                                                                   ELTANEST
00611      PERFORM 5170-AAR-PPF-PVE-TABS                                ELTANEST
00612         THRU 5170-EXIT.                                           ELTANEST
00613                                                                   ELTANEST
00614      PERFORM 5180-ALL-LEVEL-TABS                                  ELTANEST
00615         THRU 5180-EXIT.                                           ELTANEST
00616                                                                   ELTANEST
00617      PERFORM 7000-PAY-CONSID-TEXT                                 ELTANEST
00618         THRU 7000-EXIT.                                           ELTANEST
00619  5100-EXIT.  EXIT.                                                ELTANEST
00620 /                                                                 ELTANEST
00621 ****************************************************************  ELTANEST
00622 *      L I S T   O F   B E N E F I T   P R O V I S O N S       *  ELTANEST
00623 ****************************************************************  ELTANEST
00624  5105-LIST-BEN-PROV.                                              ELTANEST
00625      MOVE  +2  TO  WS-CIA.                                        ELTANEST
00626      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
00627      MOVE ZERO  TO  WS-SUB2.                                      ELTANEST
00628      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTANEST
00629      MOVE '5106'  TO  WS-PARA-ID1.                                ELTANEST
00630      PERFORM 5106-ZERO-ALL-WITH-SAME-NO                           ELTANEST
00631         THRU 5106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTANEST
00632                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.ELTANEST
00633      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
00634                                                                   ELTANEST
00635      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTANEST
00636      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTANEST
00637                       COMMAREA(DFHCOMMAREA)                       ELTANEST
00638      END-EXEC.                                                    ELTANEST
00639                                                                   ELTANEST
00640  5105-EXIT.  EXIT.                                                ELTANEST
00641      SKIP3                                                        ELTANEST
00642  5106-ZERO-ALL-WITH-SAME-NO.                                      ELTANEST
00643                                                                   ELTANEST
00644      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTANEST
00645         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
00646         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTANEST
00647         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTANEST
00648                                                    CMF-CODE-VALUE ELTANEST
00649         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTANEST
00650         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
00651         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
00652            THRU 9500-EXIT                                         ELTANEST
00653         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTANEST
00654         ADD  1  TO  WS-SUB2                                       ELTANEST
00655         IF WS-CIA  >  20 OR  =  20                                ELTANEST
00656            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTANEST
00657            EXEC CICS  LINK  PROGRAM ('ELUOUTPT')                  ELTANEST
00658                             COMMAREA (DFHCOMMAREA)                ELTANEST
00659                             END-EXEC                              ELTANEST
00660            MOVE +1  TO  WS-CIA.                                   ELTANEST
00661                                                                   ELTANEST
00662  5106-EXIT.  EXIT.                                                ELTANEST
00663      SKIP3                                                        ELTANEST
00664  5110-PLACE-OF-TREATMENT.                                         ELTANEST
00665 ****************************************************************  ELTANEST
00666 *              P L A C E   O F   T R E A T M E N T             *  ELTANEST
00667 ****************************************************************  ELTANEST
00668      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00669      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00670                          AND                                      ELTANEST
00671         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
00672                                                  NOT =  ZERO      ELTANEST
00673          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTANEST
00674          MOVE +2                    TO  WS-CIA                    ELTANEST
00675          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTANEST
00676                                                                   ELTANEST
00677      SET  PLT-INDEX2  TO  2.                                      ELTANEST
00678      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00679                           AND                                     ELTANEST
00680         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
00681                                                 NOT  =  ZERO      ELTANEST
00682                           AND                                     ELTANEST
00683         NOT  WS-ADD-A-BLANK-LINE                                  ELTANEST
00684          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTANEST
00685          MOVE +2                    TO  WS-CIA                    ELTANEST
00686          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTANEST
00687                                                                   ELTANEST
00688      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00689      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00690                           AND                                     ELTANEST
00691         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
00692                                                  NOT  =  ZERO     ELTANEST
00693          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTANEST
00694          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTANEST
00695                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
00696          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTANEST
00697                               TO  CMF-CODE-VALUE                  ELTANEST
00698          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
00699          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
00700          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
00701             THRU 9500-EXIT.                                       ELTANEST
00702                                                                   ELTANEST
00703      SET PLT-INDEX2  TO  2.                                       ELTANEST
00704      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00705                           AND                                     ELTANEST
00706         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
00707                                                 NOT  =  ZERO      ELTANEST
00708          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTANEST
00709          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTANEST
00710                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
00711          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTANEST
00712                               TO  CMF-CODE-VALUE                  ELTANEST
00713          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
00714          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
00715          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
00716             THRU 9500-EXIT.                                       ELTANEST
00717                                                                   ELTANEST
00718      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
00719         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
00720          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
00721             THRU 9200-EXIT.                                       ELTANEST
00722                                                                   ELTANEST
00723      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
00724         SET PLT-INDEX2  TO  2                                     ELTANEST
00725      ELSE                                                         ELTANEST
00726         SET PLT-INDEX2  TO  1.                                    ELTANEST
00727                                                                   ELTANEST
00728  5110-EXIT.  EXIT.                                                ELTANEST
00729      SKIP3                                                        ELTANEST
00730  5120-BENEFIT-SCOPE.                                              ELTANEST
00731 ****************************************************************  ELTANEST
00732 *                 B E N E F I T   S C O P E                    *  ELTANEST
00733 ****************************************************************  ELTANEST
00734      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00735      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00736                          AND                                      ELTANEST
00737         PLC-BEN-SCOPE-ID (PLT-INDEX1, PLT-INDEX2)                 ELTANEST
00738                                  NOT =  '0000' AND  NOT =  '00  ' ELTANEST
00739          MOVE +2  TO  WS-CIA                                      ELTANEST
00740          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).          ELTANEST
00741                                                                   ELTANEST
00742      SET  PLT-INDEX2  TO  2.                                      ELTANEST
00743      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00744                           AND                                     ELTANEST
00745         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
00746                                NOT =    '0000' AND  NOT =  '00  ' ELTANEST
00747          MOVE +2  TO  WS-CIA.                                     ELTANEST
00748          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTANEST
00749                                                                   ELTANEST
00750      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00751      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00752                           AND                                     ELTANEST
00753         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
00754                                  NOT =  '0000' AND  NOT =  '00  ' ELTANEST
00755          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
00756          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
00757          MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)            ELTANEST
00758                               TO  CMF-CODE-VALUE                  ELTANEST
00759          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
00760          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
00761          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
00762             THRU 9500-EXIT                                        ELTANEST
00763          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
00764             THRU 9200-EXIT.                                       ELTANEST
00765                                                                   ELTANEST
00766      SET PLT-INDEX2  TO  2.                                       ELTANEST
00767      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00768                           AND                                     ELTANEST
00769         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
00770                                 NOT =   '0000' AND  NOT =  '00  ' ELTANEST
00771          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
00772          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
00773          MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)            ELTANEST
00774                               TO  CMF-CODE-VALUE                  ELTANEST
00775          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
00776          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
00777          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
00778             THRU 9500-EXIT                                        ELTANEST
00779          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
00780             THRU 9200-EXIT.                                       ELTANEST
00781                                                                   ELTANEST
00782      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
00783         SET PLT-INDEX2  TO  2                                     ELTANEST
00784      ELSE                                                         ELTANEST
00785         SET PLT-INDEX2  TO  1.                                    ELTANEST
00786                                                                   ELTANEST
00787  5120-EXIT.  EXIT.                                                ELTANEST
00788      SKIP3                                                        ELTANEST
00789  5130-PRIC-METH.                                                  ELTANEST
00790 ****************************************************************  ELTANEST
00791 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTANEST
00792 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTANEST
00793 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTANEST
00794 ****************************************************************  ELTANEST
00795      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00797         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
00798                                                              '19' ELTANEST
00799         MOVE +2             TO  WS-CIA                            ELTANEST
00800         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTANEST
00801         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
00802                                                                   ELTANEST
00803      SET  PLT-INDEX2  TO  2.                                      ELTANEST
00804      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00805         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
00806                                                        '19' AND   ELTANEST
00807         NOT WS-ADD-A-BLANK-LINE                                   ELTANEST
00808         MOVE +2             TO  WS-CIA                            ELTANEST
00809         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTANEST
00810         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
00811                                                                   ELTANEST
00812      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00813      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00814         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTANEST
00815                            AND                                    ELTANEST
00816         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00817         SET  PLT-INDEX2  TO  2                                    ELTANEST
00818         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTANEST
00819                                                             ZERO  ELTANEST
00820            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTANEST
00821            ADD +1  TO  WS-CIA                                     ELTANEST
00822            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTANEST
00823                                                                   ELTANEST
00824      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00825      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00826         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTANEST
00827                            AND                                    ELTANEST
00828         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTANEST
00829         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTANEST
00830         ADD +1  TO  WS-CIA                                        ELTANEST
00831         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTANEST
00832                                                                   ELTANEST
00833      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTANEST
00834         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00835         SET  PLT-INDEX2  TO  2                                    ELTANEST
00836         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTANEST
00837                                                             ZERO  ELTANEST
00838            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTANEST
00839            ADD +1  TO  WS-CIA                                     ELTANEST
00840            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTANEST
00841                                                                   ELTANEST
00842      SET  PLT-INDEX2  TO  1.                                      ELTANEST
00843      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00844         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTANEST
00845                                                            =  ZEROELTANEST
00846            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00847                                                            =  ZEROELTANEST
00848               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTANEST
00849            ELSE                                                   ELTANEST
00850               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTANEST
00851          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00852                                                  TO  WS-PERCENTAGEELTANEST
00853         ELSE                                                      ELTANEST
00854            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTANEST
00855          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00856                                                 TO  WS-PERCENTAGE.ELTANEST
00857      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00858         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
00859                                             ZERO AND  NOT =  '19' ELTANEST
00860         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
00861         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTANEST
00862         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTANEST
00863                                                    CMF-CODE-VALUE ELTANEST
00864         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
00865         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
00866         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTANEST
00867            THRU 9600-EXIT.                                        ELTANEST
00868                                                                   ELTANEST
00869      SET  PLT-INDEX2  TO  2.                                      ELTANEST
00870      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
00871         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTANEST
00872                                                               ZEROELTANEST
00873            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00874                                                            =  ZEROELTANEST
00875               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTANEST
00876            ELSE                                                   ELTANEST
00877               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTANEST
00878          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00879                                                  TO  WS-PERCENTAGEELTANEST
00880         ELSE                                                      ELTANEST
00881            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTANEST
00882          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
00883                                                 TO  WS-PERCENTAGE.ELTANEST
00884      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
00885         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
00886                                             ZERO AND  NOT =  '19' ELTANEST
00887         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
00888         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTANEST
00889         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTANEST
00890                                                    CMF-CODE-VALUE ELTANEST
00891         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTANEST
00892         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
00893         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTANEST
00894            THRU 9600-EXIT.                                        ELTANEST
00895                                                                   ELTANEST
00896      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
00897          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTANEST
00898          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
00899             THRU 9200-EXIT.                                       ELTANEST
00900                                                                   ELTANEST
00901      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
00902         SET PLT-INDEX2  TO  2                                     ELTANEST
00903      ELSE                                                         ELTANEST
00904         SET PLT-INDEX2  TO  1.                                    ELTANEST
00905                                                                   ELTANEST
00906  5130-EXIT.  EXIT.                                                ELTANEST
00907 /                                                                 ELTANEST
00908  5140-SAME-PROVIDER.                                              ELTANEST
00909 ****************************************************************  ELTANEST
00910 *        I F   T H E   S A M E   P R O V I D E R               *  ELTANEST
00911 ****************************************************************  ELTANEST
00912                                                                   ELTANEST
00913      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR.                       ELTANEST
00914      IF CIA-RC-PTR-NULL                                           ELTANEST
00915         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
00916         IF CIA-RC-PTR-NULL                                        ELTANEST
00917            GO TO 5140-EXIT.                                       ELTANEST
00918                                                                   ELTANEST
00919 *--------------- ELECTRIC SHOCK OR ANESTHSIA ------------------*  ELTANEST
00920                                                                   ELTANEST
00921      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR.                       ELTANEST
00922      IF NOT CIA-RC-PTR-NULL                                       ELTANEST
00923         IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  =  ZERO                ELTANEST
00924            PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                  ELTANEST
00925            IF NOT CIA-RC-PTR-NULL                                 ELTANEST
00926               IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  =  ZERO          ELTANEST
00927                  GO TO 5142-NEWBORN                               ELTANEST
00928               ELSE                                                ELTANEST
00929                  NEXT SENTENCE                                    ELTANEST
00930            ELSE                                                   ELTANEST
00931               GO TO 5142-NEWBORN.                                 ELTANEST
00932                                                                   ELTANEST
00933      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR.                       ELTANEST
00934      IF CIA-RC-PTR-NULL                                           ELTANEST
00935         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
00936         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
00937            IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  =  ZERO             ELTANEST
00938               GO TO 5142-NEWBORN.                                 ELTANEST
00939                                                                   ELTANEST
00940      MOVE +2             TO  WS-CIA.                              ELTANEST
00941      MOVE WS-SAME-PROV   TO  COF-DTL-LINE (WS-CIA).               ELTANEST
00942      MOVE 'Y'            TO  WS-SAME-PROV-LINE-SW.                ELTANEST
00943      ADD +1              TO  WS-CIA.                              ELTANEST
00944      MOVE WS-ELEC-SHOCK  TO  COF-DTL-LINE (WS-CIA).               ELTANEST
00945                                                                   ELTANEST
00946      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
00947         THRU 9200-EXIT.                                           ELTANEST
00948                                                                   ELTANEST
00949      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
00950         PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                     ELTANEST
00951         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
00952            IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  NOT  =  ZERO        ELTANEST
00953               MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                 ELTANEST
00954                            TO  CMF-CODE-VALUE                     ELTANEST
00955               MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                   ELTANEST
00956                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
00957               PERFORM 9700-BASIC-EXPLANATION                      ELTANEST
00958                  THRU 9700-EXIT                                   ELTANEST
00959               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
00960                  THRU 9400-EXIT.                                  ELTANEST
00961                                                                   ELTANEST
00962      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
00963         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
00964         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
00965            IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  NOT  =  ZERO        ELTANEST
00966               MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                 ELTANEST
00967                            TO  CMF-CODE-VALUE                     ELTANEST
00968               MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                   ELTANEST
00969                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
00970               PERFORM 9800-SUPP-EXPLANATION                       ELTANEST
00971                  THRU 9800-EXIT                                   ELTANEST
00972               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
00973                  THRU 9400-EXIT.                                  ELTANEST
00974                                                                   ELTANEST
00975  5142-NEWBORN.                                                    ELTANEST
00976 *---------- NEWBORN EXAM, DELIVERY, ANESTHSIA -----------------*  ELTANEST
00977                                                                   ELTANEST
00978      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                        ELTANEST
00979      IF NOT CIA-RC-PTR-NULL                                       ELTANEST
00980         IF GCT-SAME-PROV-IP-NWBN-DEL-ANS  =  ZERO                 ELTANEST
00981            PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                  ELTANEST
00982            IF NOT CIA-RC-PTR-NULL                                 ELTANEST
00983               IF GCT-SAME-PROV-IP-NWBN-DEL-ANS  =  ZERO           ELTANEST
00984                  GO TO 5144-SURGERY                               ELTANEST
00985               ELSE                                                ELTANEST
00986                  NEXT SENTENCE                                    ELTANEST
00987            ELSE                                                   ELTANEST
00988               GO TO 5144-SURGERY.                                 ELTANEST
00989                                                                   ELTANEST
00990      IF WS-SAME-PROV-PRT                                          ELTANEST
00991          MOVE  +0  TO  WS-CIA                                     ELTANEST
00992      ELSE                                                         ELTANEST
00993          MOVE  +2            TO  WS-CIA                           ELTANEST
00994          MOVE WS-SAME-PROV   TO  COF-DTL-LINE (WS-CIA)            ELTANEST
00995          MOVE 'Y'            TO  WS-SAME-PROV-LINE-SW.            ELTANEST
00996                                                                   ELTANEST
00997      ADD +1           TO  WS-CIA.                                 ELTANEST
00998      MOVE WS-NEWBORN  TO  COF-DTL-LINE (WS-CIA).                  ELTANEST
00999      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
01000         THRU 9200-EXIT.                                           ELTANEST
01001                                                                   ELTANEST
01002      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
01003         PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                     ELTANEST
01004         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01005            IF GCT-SAME-PROV-IP-NWBN-DEL-ANS  NOT  =  ZERO         ELTANEST
01006               MOVE GCT-SAME-PROV-IP-NWBN-DEL-ANS                  ELTANEST
01007                            TO  CMF-CODE-VALUE                     ELTANEST
01008               MOVE 'SAME-PROV-IP-NWBN-DEL-ANS'                    ELTANEST
01009                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01010               PERFORM 9700-BASIC-EXPLANATION                      ELTANEST
01011                  THRU 9700-EXIT                                   ELTANEST
01012               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01013                  THRU 9400-EXIT.                                  ELTANEST
01014                                                                   ELTANEST
01015      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
01016         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01017         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01018            IF GCT-SAME-PROV-IP-NWBN-DEL-ANS  NOT  =  ZERO         ELTANEST
01019               MOVE GCT-SAME-PROV-IP-NWBN-DEL-ANS                  ELTANEST
01020                            TO  CMF-CODE-VALUE                     ELTANEST
01021               MOVE 'SAME-PROV-IP-NWBN-DEL-ANS'                    ELTANEST
01022                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01023               PERFORM 9800-SUPP-EXPLANATION                       ELTANEST
01024                  THRU 9800-EXIT                                   ELTANEST
01025               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01026                  THRU 9400-EXIT.                                  ELTANEST
01027                                                                   ELTANEST
01028  5144-SURGERY.                                                    ELTANEST
01029 *---------- SURGERY, ANESTHESIA, ASSISTANT SURGERY ------------*  ELTANEST
01030                                                                   ELTANEST
01031      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                        ELTANEST
01032      IF NOT CIA-RC-PTR-NULL                                       ELTANEST
01033         IF GCT-SAME-PROV-SRG-ANS-SRG-AST  =  ZERO                 ELTANEST
01034            PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                  ELTANEST
01035            IF NOT CIA-RC-PTR-NULL                                 ELTANEST
01036                 IF GCT-SAME-PROV-SRG-ANS-SRG-AST  =  ZERO         ELTANEST
01037                    GO TO 5140-EXIT                                ELTANEST
01038                 ELSE                                              ELTANEST
01039                    NEXT SENTENCE                                  ELTANEST
01040            ELSE                                                   ELTANEST
01041               GO TO 5140-EXIT.                                    ELTANEST
01042                                                                   ELTANEST
01043      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                        ELTANEST
01044      IF CIA-RC-PTR-NULL                                           ELTANEST
01045         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01046         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01047            IF GCT-SAME-PROV-SRG-ANS-SRG-AST  =  ZERO              ELTANEST
01048               GO TO 5140-EXIT.                                    ELTANEST
01049                                                                   ELTANEST
01050      IF WS-SAME-PROV-PRT                                          ELTANEST
01051          MOVE  +0  TO  WS-CIA                                     ELTANEST
01052      ELSE                                                         ELTANEST
01053          MOVE  +2            TO  WS-CIA                           ELTANEST
01054          MOVE WS-SAME-PROV   TO  COF-DTL-LINE (WS-CIA)            ELTANEST
01055          MOVE 'Y'            TO  WS-SAME-PROV-LINE-SW.            ELTANEST
01056                                                                   ELTANEST
01057      ADD +1           TO  WS-CIA.                                 ELTANEST
01058      MOVE WS-SURGERY  TO  COF-DTL-LINE (WS-CIA).                  ELTANEST
01059      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
01060         THRU 9200-EXIT.                                           ELTANEST
01061                                                                   ELTANEST
01062      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
01063         PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                     ELTANEST
01064         IF CIA-RC-PTR-NULL                                        ELTANEST
01065            IF GCT-SAME-PROV-SRG-ANS-SRG-AST  NOT  =  ZERO         ELTANEST
01066               MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                  ELTANEST
01067                            TO  CMF-CODE-VALUE                     ELTANEST
01068               MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                    ELTANEST
01069                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01070               PERFORM 9700-BASIC-EXPLANATION                      ELTANEST
01071                  THRU 9700-EXIT                                   ELTANEST
01072               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01073                  THRU 9400-EXIT.                                  ELTANEST
01074                                                                   ELTANEST
01075      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTANEST
01076         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01077         IF CIA-RC-PTR-NULL                                        ELTANEST
01078            IF GCT-SAME-PROV-SRG-ANS-SRG-AST  NOT  =  ZERO         ELTANEST
01079               MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                  ELTANEST
01080                            TO  CMF-CODE-VALUE                     ELTANEST
01081               MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                    ELTANEST
01082                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01083               PERFORM 9800-SUPP-EXPLANATION                       ELTANEST
01084                  THRU 9800-EXIT                                   ELTANEST
01085               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01086                  THRU 9400-EXIT.                                  ELTANEST
01087                                                                   ELTANEST
01088  5140-EXIT.  EXIT.                                                ELTANEST
01089      SKIP3                                                        ELTANEST
01090  5150-ELIG-METH-TREATMENT.                                        ELTANEST
01091 ****************************************************************  ELTANEST
01092 *   E L I G I B L E   M E T H O D   O F   T R E A T M E N T    *  ELTANEST
01093 ****************************************************************  ELTANEST
01094      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01095      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01096                          AND                                      ELTANEST
01097         PLC-ELIG-METHD-OF-TREAT-IND (PLT-INDEX1, PLT-INDEX2)      ELTANEST
01098                                  NOT =  ZERO                      ELTANEST
01099          MOVE +2  TO  WS-CIA                                      ELTANEST
01100          MOVE WS-ELIG-METH-TREAT  TO  COF-DTL-LINE (WS-CIA)       ELTANEST
01101          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                        ELTANEST
01102                                                                   ELTANEST
01103      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01104      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01105                           AND                                     ELTANEST
01106         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
01107                                NOT =   ZERO                       ELTANEST
01108                           AND                                     ELTANEST
01109         NOT  WS-ADD-A-BLANK-LINE                                  ELTANEST
01110          MOVE +2  TO  WS-CIA                                      ELTANEST
01111          MOVE WS-ELIG-METH-TREAT  TO  COF-DTL-LINE(WS-CIA)        ELTANEST
01112          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                        ELTANEST
01113                                                                   ELTANEST
01114      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01115      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01116                           AND                                     ELTANEST
01117         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
01118                                  NOT =  ZERO                      ELTANEST
01119          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
01120          MOVE 'ELIG-METHD-OF-TREAT-IND'                           ELTANEST
01121                      TO  CMF-ELEMENT-SYSTEM-NAME                  ELTANEST
01122          MOVE PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
01123                               TO  CMF-CODE-VALUE                  ELTANEST
01124          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
01125          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01126          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01127             THRU 9500-EXIT.                                       ELTANEST
01128                                                                   ELTANEST
01129      SET PLT-INDEX2  TO  2.                                       ELTANEST
01130      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01131                           AND                                     ELTANEST
01132         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
01133                                 NOT =   ZERO                      ELTANEST
01134          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
01135          MOVE 'ELIG-METHD-OF-TREAT-IND' TO CMF-ELEMENT-SYSTEM-NAMEELTANEST
01136          MOVE PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
01137                               TO  CMF-CODE-VALUE                  ELTANEST
01138          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
01139          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01140          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01141             THRU 9500-EXIT.                                       ELTANEST
01142                                                                   ELTANEST
01143      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
01144         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
01145         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
01146            THRU 9200-EXIT.                                        ELTANEST
01147 **---------------------------------------------------------------+ELTANEST
01148                                                                   ELTANEST
01149      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
01150         SET PLT-INDEX2  TO  2                                     ELTANEST
01151      ELSE                                                         ELTANEST
01152         SET PLT-INDEX2  TO  1.                                    ELTANEST
01153                                                                   ELTANEST
01154  5150-EXIT.  EXIT.                                                ELTANEST
01155  5160-SPILLOVR-COINS-N-DEDUC.                                     ELTANEST
01156 ****************************************************************  ELTANEST
01157 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTANEST
01158 ****************************************************************  ELTANEST
01159      MOVE +1  TO  WS-CIA.                                         ELTANEST
01160                                                                   ELTANEST
01161      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01162      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01163                            AND                                    ELTANEST
01164         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTANEST
01165                                                  NOT =  '0'       ELTANEST
01166         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01167         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTANEST
01168                                           CMF-ELEMENT-SYSTEM-NAME ELTANEST
01169         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
01170                                                TO  CMF-CODE-VALUE ELTANEST
01171         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTANEST
01172         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTANEST
01173         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
01174            THRU 9500-EXIT                                         ELTANEST
01175         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
01176            THRU 9200-EXIT.                                        ELTANEST
01177 ****************************************************************  ELTANEST
01178 *          S P I L L O V E R   D E D U C T I B L E             *  ELTANEST
01179 ****************************************************************  ELTANEST
01180      MOVE +1  TO  WS-CIA.                                         ELTANEST
01181                                                                   ELTANEST
01182      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01183      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01184                            AND                                    ELTANEST
01185         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTANEST
01186                                                  NOT =  '0'       ELTANEST
01187         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01188         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTANEST
01189                                           CMF-ELEMENT-SYSTEM-NAME ELTANEST
01190         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTANEST
01191                                                 TO  CMF-CODE-VALUEELTANEST
01192         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTANEST
01193         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTANEST
01194         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
01195            THRU 9500-EXIT                                         ELTANEST
01196         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
01197            THRU 9200-EXIT.                                        ELTANEST
01198                                                                   ELTANEST
01199      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
01200         SET PLT-INDEX2  TO  2                                     ELTANEST
01201      ELSE                                                         ELTANEST
01202         SET PLT-INDEX2  TO  1.                                    ELTANEST
01203                                                                   ELTANEST
01204  5160-EXIT.  EXIT.                                                ELTANEST
01205 /                                                                 ELTANEST
01206  5165-TRANSF-OTHER-RESP.                                          ELTANEST
01207                                                                   ELTANEST
01208      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTANEST
01209         SET PLT-INDEX2  TO  2                                     ELTANEST
01210      ELSE                                                         ELTANEST
01211         SET PLT-INDEX2  TO  1.                                    ELTANEST
01212                                                                   ELTANEST
01213      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01214                            AND                                    ELTANEST
01215         PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTANEST
01216                                                  NOT = ZERO       ELTANEST
01217         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01218         MOVE 'TRANSF-OTHER-RESP-IND'   TO                         ELTANEST
01219                                           CMF-ELEMENT-SYSTEM-NAME ELTANEST
01220         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTANEST
01221                                                 TO  CMF-CODE-VALUEELTANEST
01222         MOVE SPACES            TO  WS-TEMP-TEXT-AREA              ELTANEST
01223         MOVE +0                TO  WS-TEMP-NOT-USED-CNT           ELTANEST
01224         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
01225            THRU 9500-EXIT                                         ELTANEST
01226         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
01227            THRU 9200-EXIT.                                        ELTANEST
01228                                                                   ELTANEST
01229  5165-EXIT.  EXIT.                                                ELTANEST
01230 /                                                                 ELTANEST
01231  5170-AAR-PPF-PVE-TABS.                                           ELTANEST
01232 ****************************************************************  ELTANEST
01233 *                  A A R   T A B U L A R                       *  ELTANEST
01234 ****************************************************************  ELTANEST
01235      SET PLT-INDEX2  TO  1.                                       ELTANEST
01236      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01237         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01238                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01239         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
01240         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTANEST
01241         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTANEST
01242      ELSE                                                         ELTANEST
01243         SET PLT-INDEX2  TO  2                                     ELTANEST
01244         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01245            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01246                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01247            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTANEST
01248            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTANEST
01249            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTANEST
01250                                                                   ELTANEST
01251      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
01252         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
01253         MOVE 1  TO  WS-CIA                                        ELTANEST
01254         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTANEST
01255             COMMAREA(DFHCOMMAREA)                                 ELTANEST
01256         END-EXEC.                                                 ELTANEST
01257 *--------------------------------------------------------------*  ELTANEST
01258 *                  P P F   T A B U L A R                       *  ELTANEST
01259 *--------------------------------------------------------------*  ELTANEST
01260      SET PLT-INDEX2  TO  1.                                       ELTANEST
01261      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01262         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01263                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01264         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
01265                                                KWA-GCTABULR-KEY   ELTANEST
01266         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
01267            THRU 9900-EXIT                                         ELTANEST
01268         IF IOP-RC-OK                                              ELTANEST
01269            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTANEST
01270                 COMMAREA(DFHCOMMAREA)                             ELTANEST
01271            END-EXEC                                               ELTANEST
01272         ELSE                                                      ELTANEST
01273            NEXT SENTENCE                                          ELTANEST
01274      ELSE                                                         ELTANEST
01275         SET PLT-INDEX2  TO  2                                     ELTANEST
01276         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01277            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01278                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01279          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
01280                                                KWA-GCTABULR-KEY   ELTANEST
01281            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
01282               THRU 9900-EXIT                                      ELTANEST
01283            IF IOP-RC-OK                                           ELTANEST
01284               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTANEST
01285                    COMMAREA(DFHCOMMAREA)                          ELTANEST
01286               END-EXEC.                                           ELTANEST
01287 *--------------------------------------------------------------*  ELTANEST
01288 *                  P V E   T A B U L A R                       *  ELTANEST
01289 *--------------------------------------------------------------*  ELTANEST
01290                                                                   ELTANEST
01291      MOVE  +2     TO  WS-CIA.                                     ELTANEST
01292      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTANEST
01293      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
01294         THRU 9200-EXIT.                                           ELTANEST
01295                                                                   ELTANEST
01296  5170-EXIT.  EXIT.                                                ELTANEST
01297  5180-ALL-LEVEL-TABS.                                             ELTANEST
01298 *--------------------------------------------------------------*  ELTANEST
01299 *                  A B M   T A B U L A R                       *  ELTANEST
01300 *--------------------------------------------------------------*  ELTANEST
01301      SET PLT-INDEX2  TO  1.                                       ELTANEST
01302      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01303                              AND                                  ELTANEST
01304         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01305                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01306         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
01307                                                 KWA-GCTABULR-KEY  ELTANEST
01308         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
01309            THRU 9900-EXIT                                         ELTANEST
01310         IF IOP-RC-OK                                              ELTANEST
01311            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTANEST
01312                 COMMAREA(DFHCOMMAREA)                             ELTANEST
01313            END-EXEC                                               ELTANEST
01314         ELSE                                                      ELTANEST
01315            NEXT SENTENCE                                          ELTANEST
01316      ELSE                                                         ELTANEST
01317         SET PLT-INDEX2  TO  2                                     ELTANEST
01318         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01319            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01320                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01321          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
01322                                                 KWA-GCTABULR-KEY  ELTANEST
01323            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
01324               THRU 9900-EXIT                                      ELTANEST
01325            IF IOP-RC-OK                                           ELTANEST
01326               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTANEST
01327                    COMMAREA(DFHCOMMAREA)                          ELTANEST
01328               END-EXEC.                                           ELTANEST
01329 *--------------------------------------------------------------*  ELTANEST
01330 *                  A C L   T A B U L A R                       *  ELTANEST
01331 *--------------------------------------------------------------*  ELTANEST
01332      SET PLT-INDEX2  TO  1.                                       ELTANEST
01333      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01334         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01335                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01336         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
01337                                                 KWA-GCTABULR-KEY  ELTANEST
01338         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
01339            THRU 9900-EXIT                                         ELTANEST
01340         IF IOP-RC-OK                                              ELTANEST
01341            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTANEST
01342                 COMMAREA(DFHCOMMAREA)                             ELTANEST
01343            END-EXEC                                               ELTANEST
01344         ELSE                                                      ELTANEST
01345            NEXT SENTENCE                                          ELTANEST
01346      ELSE                                                         ELTANEST
01347         SET PLT-INDEX2  TO  2                                     ELTANEST
01348         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01349            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01350                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01351          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
01352                                                 KWA-GCTABULR-KEY  ELTANEST
01353            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
01354               THRU 9900-EXIT                                      ELTANEST
01355            IF IOP-RC-OK                                           ELTANEST
01356               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTANEST
01357                    COMMAREA(DFHCOMMAREA)                          ELTANEST
01358               END-EXEC.                                           ELTANEST
01359 *--------------------------------------------------------------*  ELTANEST
01360 *                  A D L   T A B U L A R                       *  ELTANEST
01361 *--------------------------------------------------------------*  ELTANEST
01362      SET PLT-INDEX2  TO  1.                                       ELTANEST
01363      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01364         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01365                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01366         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
01367                                                 KWA-GCTABULR-KEY  ELTANEST
01368         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
01369            THRU 9900-EXIT                                         ELTANEST
01370         IF IOP-RC-OK                                              ELTANEST
01371            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTANEST
01372                 COMMAREA(DFHCOMMAREA)                             ELTANEST
01373            END-EXEC                                               ELTANEST
01374         ELSE                                                      ELTANEST
01375            NEXT SENTENCE                                          ELTANEST
01376      ELSE                                                         ELTANEST
01377         SET PLT-INDEX2  TO  2                                     ELTANEST
01378         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01379            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01380                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01381          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
01382                                                 KWA-GCTABULR-KEY  ELTANEST
01383            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
01384               THRU 9900-EXIT                                      ELTANEST
01385            IF IOP-RC-OK                                           ELTANEST
01386               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTANEST
01387                    COMMAREA(DFHCOMMAREA)                          ELTANEST
01388               END-EXEC.                                           ELTANEST
01389 *--------------------------------------------------------------*  ELTANEST
01390 *                  A O L   T A B U L A R                       *  ELTANEST
01391 *--------------------------------------------------------------*  ELTANEST
01392      SET PLT-INDEX2  TO  1.                                       ELTANEST
01393      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01394         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
01395                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01396         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
01397                                                 KWA-GCTABULR-KEY  ELTANEST
01398         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
01399            THRU 9900-EXIT                                         ELTANEST
01400         IF IOP-RC-OK                                              ELTANEST
01401            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTANEST
01402                 COMMAREA(DFHCOMMAREA)                             ELTANEST
01403            END-EXEC                                               ELTANEST
01404         ELSE                                                      ELTANEST
01405            NEXT SENTENCE                                          ELTANEST
01406      ELSE                                                         ELTANEST
01407         SET PLT-INDEX2  TO  2                                     ELTANEST
01408         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
01409            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
01410                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
01411          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
01412                                                 KWA-GCTABULR-KEY  ELTANEST
01413            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
01414               THRU 9900-EXIT                                      ELTANEST
01415            IF IOP-RC-OK                                           ELTANEST
01416               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTANEST
01417                    COMMAREA(DFHCOMMAREA)                          ELTANEST
01418               END-EXEC.                                           ELTANEST
01419  5180-EXIT.  EXIT.                                                ELTANEST
01420 /                                                                 ELTANEST
01421 ****************************************************************  ELTANEST
01422 *             PROFESSIONAL OUTPATIENT INFORMATION              *  ELTANEST
01423 ****************************************************************  ELTANEST
01424  6000-PROF-OP.                                                    ELTANEST
01425                                                                   ELTANEST
01426      MOVE '6000'  TO  WS-PARA-ID1.                                ELTANEST
01427      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTANEST
01428                                                                   ELTANEST
01429      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTANEST
01430                                                                   ELTANEST
01431      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTANEST
01432         THRU 9100-EXIT.                                           ELTANEST
01433                                                                   ELTANEST
01434      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTANEST
01435      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
01436          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTANEST
01437                                                                   ELTANEST
01438      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTANEST
01439                                                                   ELTANEST
01440      PERFORM 6010-MOVE-IN-PROF-OP-TABS                            ELTANEST
01441         THRU 6010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTANEST
01442                        UNTIL   WS-SUB  >     WS-PROF-OP-CNT.      ELTANEST
01443                                                                   ELTANEST
01444      PERFORM 6020-CALL-COVERAGE                                   ELTANEST
01445         THRU 6020-EXIT.                                           ELTANEST
01446                                                                   ELTANEST
01447      IF PVN-COVG-NONE                                             ELTANEST
01448          GO TO 6000-EXIT.                                         ELTANEST
01449                                                                   ELTANEST
01450      PERFORM 6030-FIND-FIRST-NONZERO                              ELTANEST
01451         THRU 6030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTANEST
01452                        UNTIL   WS-SUB  > WS-PROF-OP-CNT.          ELTANEST
01453                                                                   ELTANEST
01454  6000-EXIT.  EXIT.                                                ELTANEST
01455 /                                                                 ELTANEST
01456  6010-MOVE-IN-PROF-OP-TABS.                                       ELTANEST
01457      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
01458      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTANEST
01459                TO  PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).           ELTANEST
01460                                                                   ELTANEST
01461      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)          ELTANEST
01462                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTANEST
01463                                                                   ELTANEST
01464  6010-EXIT.  EXIT.                                                ELTANEST
01465      SKIP3                                                        ELTANEST
01466  6020-CALL-COVERAGE.                                              ELTANEST
01467      MOVE '6020'  TO  WS-PARA-ID1.                                ELTANEST
01468                                                                   ELTANEST
01469      MOVE 'ANESTHESIA SERVICES ARE'  TO  SSB-TOPIC-PHRASE.        ELTANEST
01470                                                                   ELTANEST
01471      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTANEST
01472                     COMMAREA (DFHCOMMAREA)                        ELTANEST
01473                     END-EXEC.                                     ELTANEST
01474                                                                   ELTANEST
01475      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTANEST
01476                     COMMAREA (DFHCOMMAREA)                        ELTANEST
01477                     END-EXEC.                                     ELTANEST
01478                                                                   ELTANEST
01479      IF PVN-COVG-NONE                                             ELTANEST
01480          GO TO 6020-EXIT.                                         ELTANEST
01481                                                                   ELTANEST
01482      MOVE +1  TO  WS-CIA.                                         ELTANEST
01483                                                                   ELTANEST
01484      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTANEST
01485                                                                   ELTANEST
01486      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTANEST
01487                    PSP-PROVN-PRICING-METHD,                       ELTANEST
01488                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTANEST
01489                    PSP-TRANSF-OTHER-RESP-IND,                     ELTANEST
01490                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTANEST
01491                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTANEST
01492                    PSP-SPILL-OVER-DED-APL-IND,                    ELTANEST
01493                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTANEST
01494                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTANEST
01495                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTANEST
01496                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTANEST
01497                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTANEST
01498                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTANEST
01499                    PSC-BEN-SCOPE-ID,                              ELTANEST
01500                    PSC-ELIG-METHD-OF-TREAT-IND.                   ELTANEST
01501                                                                   ELTANEST
01502      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTANEST
01503                     COMMAREA (DFHCOMMAREA)                        ELTANEST
01504                     END-EXEC.                                     ELTANEST
01505                                                                   ELTANEST
01506      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTANEST
01507      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
01508          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTANEST
01509                                                                   ELTANEST
01510  6020-EXIT.  EXIT.                                                ELTANEST
01511      SKIP3                                                        ELTANEST
01512  6030-FIND-FIRST-NONZERO.                                         ELTANEST
01513      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
01514                                                                   ELTANEST
01515      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTANEST
01516          NEXT SENTENCE                                            ELTANEST
01517      ELSE                                                         ELTANEST
01518          PERFORM 6100-BUILD-SCREEN-LINES                          ELTANEST
01519             THRU 6100-EXIT.                                       ELTANEST
01520                                                                   ELTANEST
01521  6030-EXIT.  EXIT.                                                ELTANEST
01522 /                                                                 ELTANEST
01523  6100-BUILD-SCREEN-LINES.                                         ELTANEST
01524      MOVE '6100'  TO  WS-PARA-ID1.                                ELTANEST
01525      SET PLT-INDEX1  TO                                           ELTANEST
01526              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTANEST
01527                                                                   ELTANEST
01528      IF WS-NOT-FIRST-TIME                                         ELTANEST
01529         MOVE 'P'    TO COF-FUNCTION                               ELTANEST
01530         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTANEST
01531         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTANEST
01532                          COMMAREA(DFHCOMMAREA)                    ELTANEST
01533         END-EXEC                                                  ELTANEST
01534      ELSE                                                         ELTANEST
01535        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTANEST
01536                                                                   ELTANEST
01537      MOVE  +1  TO  WS-CIA.                                        ELTANEST
01538                                                                   ELTANEST
01539      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTANEST
01540          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTANEST
01541              SET PLT-INDEX2  TO  2                                ELTANEST
01542          ELSE                                                     ELTANEST
01543              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTANEST
01544              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTANEST
01545                 THRU 9200-EXIT                                    ELTANEST
01546              GO TO 6100-EXIT                                      ELTANEST
01547      ELSE                                                         ELTANEST
01548          SET PLT-INDEX2  TO  1.                                   ELTANEST
01549                                                                   ELTANEST
01550      PERFORM 6105-LIST-BEN-PROV                                   ELTANEST
01551         THRU 6105-EXIT.                                           ELTANEST
01552                                                                   ELTANEST
01553      PERFORM 6110-PLACE-OF-TREATMENT                              ELTANEST
01554         THRU 6110-EXIT.                                           ELTANEST
01555                                                                   ELTANEST
01556      PERFORM 6120-BENEFIT-SCOPE                                   ELTANEST
01557         THRU 6120-EXIT.                                           ELTANEST
01558                                                                   ELTANEST
01559      PERFORM 6130-PRIC-METH                                       ELTANEST
01560         THRU 6130-EXIT.                                           ELTANEST
01561                                                                   ELTANEST
01562      PERFORM 6140-SAME-PROVIDER                                   ELTANEST
01563         THRU 6140-EXIT.                                           ELTANEST
01564                                                                   ELTANEST
01565      PERFORM 6150-ELIG-METH-TREATMENT                             ELTANEST
01566         THRU 6150-EXIT.                                           ELTANEST
01567                                                                   ELTANEST
01568      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTANEST
01569         THRU 6160-EXIT.                                           ELTANEST
01570                                                                   ELTANEST
01571      PERFORM 5165-TRANSF-OTHER-RESP  THRU                         ELTANEST
01572              5165-EXIT.                                           ELTANEST
01573                                                                   ELTANEST
01574      PERFORM 6170-AAR-PPF-PVE-TABS                                ELTANEST
01575         THRU 6170-EXIT.                                           ELTANEST
01576                                                                   ELTANEST
01577      PERFORM 6180-ALL-LEVEL-TABS                                  ELTANEST
01578         THRU 6180-EXIT.                                           ELTANEST
01579                                                                   ELTANEST
01580      PERFORM 7000-PAY-CONSID-TEXT                                 ELTANEST
01581         THRU 7000-EXIT.                                           ELTANEST
01582  6100-EXIT.  EXIT.                                                ELTANEST
01583 /                                                                 ELTANEST
01584 ****************************************************************  ELTANEST
01585 *      L I S T   O F   B E N E F I T   P R O V I S O N S       *  ELTANEST
01586 ****************************************************************  ELTANEST
01587  6105-LIST-BEN-PROV.                                              ELTANEST
01588      MOVE  +2  TO  WS-CIA.                                        ELTANEST
01589      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
01590      MOVE ZERO  TO  WS-SUB2.                                      ELTANEST
01591      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTANEST
01592      MOVE '6106'  TO  WS-PARA-ID1.                                ELTANEST
01593      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTANEST
01594         THRU 6106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTANEST
01595                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.ELTANEST
01596      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTANEST
01597                                                                   ELTANEST
01598      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTANEST
01599      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTANEST
01600                       COMMAREA(DFHCOMMAREA)                       ELTANEST
01601      END-EXEC.                                                    ELTANEST
01602                                                                   ELTANEST
01603  6105-EXIT.  EXIT.                                                ELTANEST
01604      SKIP3                                                        ELTANEST
01605  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTANEST
01606                                                                   ELTANEST
01607      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTANEST
01608         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01609         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTANEST
01610         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTANEST
01611                                                    CMF-CODE-VALUE ELTANEST
01612         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTANEST
01613         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
01614         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
01615            THRU 9500-EXIT                                         ELTANEST
01616         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTANEST
01617         ADD  1  TO  WS-SUB2                                       ELTANEST
01618         IF WS-CIA  >  20 OR  =  20                                ELTANEST
01619            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTANEST
01620            EXEC CICS  LINK  PROGRAM ('ELUOUTPT')                  ELTANEST
01621                             COMMAREA (DFHCOMMAREA)                ELTANEST
01622                             END-EXEC                              ELTANEST
01623            MOVE +1  TO  WS-CIA.                                   ELTANEST
01624                                                                   ELTANEST
01625  6106-EXIT.  EXIT.                                                ELTANEST
01626      SKIP3                                                        ELTANEST
01627  6110-PLACE-OF-TREATMENT.                                         ELTANEST
01628 ****************************************************************  ELTANEST
01629 *              P L A C E   O F   T R E A T M E N T             *  ELTANEST
01630 ****************************************************************  ELTANEST
01631      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01632      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01633                          AND                                      ELTANEST
01634         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
01635                                                  NOT =  ZERO      ELTANEST
01636          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTANEST
01637          MOVE +2                    TO  WS-CIA                    ELTANEST
01638          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTANEST
01639                                                                   ELTANEST
01640      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01641      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01642                           AND                                     ELTANEST
01643         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
01644                                                 NOT  =  ZERO      ELTANEST
01645                           AND                                     ELTANEST
01646         NOT  WS-ADD-A-BLANK-LINE                                  ELTANEST
01647          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTANEST
01648          MOVE +2                    TO  WS-CIA                    ELTANEST
01649          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTANEST
01650                                                                   ELTANEST
01651      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01652      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01653                           AND                                     ELTANEST
01654         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
01655                                                  NOT  =  ZERO     ELTANEST
01656          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTANEST
01657          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTANEST
01658                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
01659          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTANEST
01660                               TO  CMF-CODE-VALUE                  ELTANEST
01661          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
01662          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01663          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01664             THRU 9500-EXIT.                                       ELTANEST
01665                                                                   ELTANEST
01666      SET PLT-INDEX2  TO  2.                                       ELTANEST
01667      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01668                           AND                                     ELTANEST
01669         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTANEST
01670                                                 NOT  =  ZERO      ELTANEST
01671          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTANEST
01672          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTANEST
01673                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
01674          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTANEST
01675                               TO  CMF-CODE-VALUE                  ELTANEST
01676          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
01677          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01678          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01679             THRU 9500-EXIT.                                       ELTANEST
01680                                                                   ELTANEST
01681      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
01682         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
01683         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
01684            THRU 9200-EXIT.                                        ELTANEST
01685                                                                   ELTANEST
01686      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROS               ELTANEST
01687         SET PLT-INDEX2  TO  2                                     ELTANEST
01688      ELSE                                                         ELTANEST
01689         SET PLT-INDEX2  TO  1.                                    ELTANEST
01690                                                                   ELTANEST
01691  6110-EXIT.  EXIT.                                                ELTANEST
01692      SKIP3                                                        ELTANEST
01693  6120-BENEFIT-SCOPE.                                              ELTANEST
01694 ****************************************************************  ELTANEST
01695 *                 B E N E F I T   S C O P E                    *  ELTANEST
01696 ****************************************************************  ELTANEST
01697      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01698      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01699                          AND                                      ELTANEST
01700         PLC-BEN-SCOPE-ID (PLT-INDEX1, PLT-INDEX2)                 ELTANEST
01701                                  NOT =  '0000' AND  NOT =  '00  ' ELTANEST
01702          MOVE +2  TO  WS-CIA                                      ELTANEST
01703          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).          ELTANEST
01704                                                                   ELTANEST
01705      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01706      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01707                           AND                                     ELTANEST
01708         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
01709                                NOT =    '0000' AND  NOT =  '00  ' ELTANEST
01710          MOVE +2  TO  WS-CIA.                                     ELTANEST
01711          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTANEST
01712                                                                   ELTANEST
01713      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01714      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01715                           AND                                     ELTANEST
01716         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
01717                                  NOT =  '0000' AND  NOT =  '00  ' ELTANEST
01718          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
01719          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
01720          MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)            ELTANEST
01721                               TO  CMF-CODE-VALUE                  ELTANEST
01722          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
01723          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01724          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01725             THRU 9500-EXIT                                        ELTANEST
01726          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
01727             THRU 9200-EXIT.                                       ELTANEST
01728                                                                   ELTANEST
01729      SET PLT-INDEX2  TO  2.                                       ELTANEST
01730      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01731                           AND                                     ELTANEST
01732         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)                  ELTANEST
01733                                 NOT =   '0000' AND  NOT =  '00  ' ELTANEST
01734          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
01735          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTANEST
01736          MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)            ELTANEST
01737                               TO  CMF-CODE-VALUE                  ELTANEST
01738          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
01739          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
01740          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
01741             THRU 9500-EXIT                                        ELTANEST
01742          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
01743             THRU 9200-EXIT.                                       ELTANEST
01744                                                                   ELTANEST
01745      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTANEST
01746         SET PLT-INDEX2  TO  2                                     ELTANEST
01747      ELSE                                                         ELTANEST
01748         SET PLT-INDEX2  TO  1.                                    ELTANEST
01749                                                                   ELTANEST
01750  6120-EXIT.  EXIT.                                                ELTANEST
01751      SKIP3                                                        ELTANEST
01752  6130-PRIC-METH.                                                  ELTANEST
01753 ****************************************************************  ELTANEST
01754 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTANEST
01755 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTANEST
01756 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTANEST
01757 ****************************************************************  ELTANEST
01758      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01759      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01760         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
01761                                                              '19' ELTANEST
01762         MOVE +2             TO  WS-CIA                            ELTANEST
01763         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTANEST
01764         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
01765                                                                   ELTANEST
01766      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01767      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZERO AND       ELTANEST
01768         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
01769                                                        '19' AND   ELTANEST
01770         NOT WS-ADD-A-BLANK-LINE                                   ELTANEST
01771         MOVE +2             TO  WS-CIA                            ELTANEST
01772         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTANEST
01773         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTANEST
01774                                                                   ELTANEST
01775      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01776      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01777         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTANEST
01778                            AND                                    ELTANEST
01779         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)NOT = ZERO             ELTANEST
01780         SET  PLT-INDEX2  TO  2                                    ELTANEST
01781         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTANEST
01782                                                             ZERO  ELTANEST
01783            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTANEST
01784            ADD +1  TO  WS-CIA                                     ELTANEST
01785            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTANEST
01786                                                                   ELTANEST
01787      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01788      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01789         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTANEST
01790                            AND                                    ELTANEST
01791         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTANEST
01792         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTANEST
01793         ADD +1  TO  WS-CIA                                        ELTANEST
01794         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTANEST
01795                                                                   ELTANEST
01796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTANEST
01797         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01798         SET  PLT-INDEX2  TO  2                                    ELTANEST
01799         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTANEST
01800                                                             ZERO  ELTANEST
01801            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTANEST
01802            ADD +1  TO  WS-CIA                                     ELTANEST
01803            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTANEST
01804                                                                   ELTANEST
01805      SET  PLT-INDEX2  TO  1.                                      ELTANEST
01806      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01807         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTANEST
01808                                                            =  ZEROELTANEST
01809            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01810                                                            =  ZEROELTANEST
01811               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTANEST
01812            ELSE                                                   ELTANEST
01813               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTANEST
01814          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01815                                                  TO  WS-PERCENTAGEELTANEST
01816         ELSE                                                      ELTANEST
01817            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTANEST
01818          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01819                                                 TO  WS-PERCENTAGE.ELTANEST
01820      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01821         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
01822                                             ZERO AND  NOT =  '19' ELTANEST
01823         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01824         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTANEST
01825         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTANEST
01826                                                    CMF-CODE-VALUE ELTANEST
01827         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
01828         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
01829         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTANEST
01830            THRU 9600-EXIT.                                        ELTANEST
01831                                                                   ELTANEST
01832      SET  PLT-INDEX2  TO  2.                                      ELTANEST
01833      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
01834         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTANEST
01835                                                               ZEROELTANEST
01836            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01837                                                            =  ZEROELTANEST
01838               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTANEST
01839            ELSE                                                   ELTANEST
01840               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTANEST
01841          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01842                                                  TO  WS-PERCENTAGEELTANEST
01843         ELSE                                                      ELTANEST
01844            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTANEST
01845          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTANEST
01846                                                 TO  WS-PERCENTAGE.ELTANEST
01847      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
01848         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTANEST
01849                                             ZERO AND  NOT =  '19' ELTANEST
01850         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
01851         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTANEST
01852         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTANEST
01853                                                    CMF-CODE-VALUE ELTANEST
01854         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTANEST
01855         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTANEST
01856         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTANEST
01857            THRU 9600-EXIT.                                        ELTANEST
01858                                                                   ELTANEST
01859      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
01860          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTANEST
01861          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTANEST
01862             THRU 9200-EXIT.                                       ELTANEST
01863                                                                   ELTANEST
01864      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTANEST
01865         SET PLT-INDEX2  TO  2                                     ELTANEST
01866      ELSE                                                         ELTANEST
01867         SET PLT-INDEX2  TO  1.                                    ELTANEST
01868                                                                   ELTANEST
01869  6130-EXIT.  EXIT.                                                ELTANEST
01870 /                                                                 ELTANEST
01871  6140-SAME-PROVIDER.                                              ELTANEST
01872 ****************************************************************  ELTANEST
01873 *        I F   T H E   S A M E   P R O V I D E R               *  ELTANEST
01874 ****************************************************************  ELTANEST
01875                                                                   ELTANEST
01876      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR.                       ELTANEST
01877      IF CIA-RC-PTR-NULL                                           ELTANEST
01878         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01879         IF CIA-RC-PTR-NULL                                        ELTANEST
01880            GO TO 6140-EXIT.                                       ELTANEST
01881                                                                   ELTANEST
01882 *--------------- ELECTRIC SHOCK OR ANESTHSIA ------------------*  ELTANEST
01883                                                                   ELTANEST
01884      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR.                       ELTANEST
01885      IF NOT CIA-RC-PTR-NULL                                       ELTANEST
01886         IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  =  ZERO                ELTANEST
01887            PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                  ELTANEST
01888            IF NOT CIA-RC-PTR-NULL                                 ELTANEST
01889               IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  =  ZERO          ELTANEST
01890                  GO TO 6144-SURGERY                               ELTANEST
01891               ELSE                                                ELTANEST
01892                  NEXT SENTENCE                                    ELTANEST
01893             ELSE                                                  ELTANEST
01894                GO TO 6144-SURGERY.                                ELTANEST
01895                                                                   ELTANEST
01896                                                                   ELTANEST
01897      MOVE +2             TO  WS-CIA.                              ELTANEST
01898      MOVE WS-SAME-PROV   TO  COF-DTL-LINE (WS-CIA).               ELTANEST
01899      MOVE 'Y'            TO  WS-SAME-PROV-LINE-SW.                ELTANEST
01900      ADD +1              TO  WS-CIA.                              ELTANEST
01901      MOVE WS-ELEC-SHOCK  TO  COF-DTL-LINE (WS-CIA).               ELTANEST
01902                                                                   ELTANEST
01903      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
01904         THRU 9200-EXIT.                                           ELTANEST
01905                                                                   ELTANEST
01906      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01907         PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                     ELTANEST
01908         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01909            IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  NOT  =  ZERO        ELTANEST
01910               MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                 ELTANEST
01911                            TO  CMF-CODE-VALUE                     ELTANEST
01912               MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                   ELTANEST
01913                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01914               PERFORM 9700-BASIC-EXPLANATION                      ELTANEST
01915                 THRU 9700-EXIT                                    ELTANEST
01916               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01917                 THRU 9400-EXIT.                                   ELTANEST
01918                                                                   ELTANEST
01919      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01920         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01921         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01922            IF GCT-SAME-PROV-ELCSHK-THRPY-ANS  NOT  =  ZERO        ELTANEST
01923               MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                 ELTANEST
01924                            TO  CMF-CODE-VALUE                     ELTANEST
01925               MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                   ELTANEST
01926                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01927               PERFORM 9800-SUPP-EXPLANATION                       ELTANEST
01928                 THRU 9800-EXIT                                    ELTANEST
01929               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01930                 THRU 9400-EXIT.                                   ELTANEST
01931                                                                   ELTANEST
01932                                                                   ELTANEST
01933  6144-SURGERY.                                                    ELTANEST
01934 *---------- SURGERY, ANESTHESIA, ASSISTANT SURGERY ------------*  ELTANEST
01935                                                                   ELTANEST
01936      PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                        ELTANEST
01937      IF NOT CIA-RC-PTR-NULL                                       ELTANEST
01938         IF GCT-SAME-PROV-SRG-ANS-SRG-AST  =  ZERO                 ELTANEST
01939            PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                  ELTANEST
01940            IF NOT CIA-RC-PTR-NULL                                 ELTANEST
01941               IF GCT-SAME-PROV-SRG-ANS-SRG-AST  =  ZERO           ELTANEST
01942                  GO TO 6140-EXIT                                  ELTANEST
01943               ELSE                                                ELTANEST
01944                  NEXT SENTENCE                                    ELTANEST
01945            ELSE                                                   ELTANEST
01946               GO TO 6140-EXIT.                                    ELTANEST
01947                                                                   ELTANEST
01948      IF WS-SAME-PROV-PRT                                          ELTANEST
01949          MOVE  +0  TO  WS-CIA                                     ELTANEST
01950      ELSE                                                         ELTANEST
01951          MOVE  +2            TO  WS-CIA                           ELTANEST
01952          MOVE WS-SAME-PROV   TO  COF-DTL-LINE (WS-CIA)            ELTANEST
01953          MOVE 'Y'            TO  WS-SAME-PROV-LINE-SW.            ELTANEST
01954                                                                   ELTANEST
01955      ADD +1           TO  WS-CIA.                                 ELTANEST
01956      MOVE WS-SURGERY  TO  COF-DTL-LINE (WS-CIA).                  ELTANEST
01957      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
01958         THRU 9200-EXIT.                                           ELTANEST
01959                                                                   ELTANEST
01960      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01961         PERFORM 6140-SET-CON-REC-ELSCONPB-PTR                     ELTANEST
01962         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01963            IF GCT-SAME-PROV-SRG-ANS-SRG-AST  NOT  =  ZERO         ELTANEST
01964               MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                  ELTANEST
01965                            TO  CMF-CODE-VALUE                     ELTANEST
01966               MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                    ELTANEST
01967                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01968               PERFORM 9700-BASIC-EXPLANATION                      ELTANEST
01969                  THRU 9700-EXIT                                   ELTANEST
01970               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01971                  THRU 9400-EXIT.                                  ELTANEST
01972                                                                   ELTANEST
01973      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
01974         PERFORM 6140-SET-CON-REC-ELSCONPS-PTR                     ELTANEST
01975         IF NOT CIA-RC-PTR-NULL                                    ELTANEST
01976            IF GCT-SAME-PROV-SRG-ANS-SRG-AST  NOT  =  ZERO         ELTANEST
01977               MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                  ELTANEST
01978                            TO  CMF-CODE-VALUE                     ELTANEST
01979               MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                    ELTANEST
01980                            TO  CMF-ELEMENT-SYSTEM-NAME            ELTANEST
01981               PERFORM 9800-SUPP-EXPLANATION                       ELTANEST
01982                  THRU 9800-EXIT                                   ELTANEST
01983               PERFORM 9400-EXPLAINATION-OUTPUT                    ELTANEST
01984                  THRU 9400-EXIT.                                  ELTANEST
01985                                                                   ELTANEST
01986  6140-EXIT.  EXIT.                                                ELTANEST
01987      SKIP3                                                        ELTANEST
01988                                                                   ELTANEST
01989  6140-SET-CON-REC-ELSCONPB-PTR.                                   ELTANEST
01990      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTANEST
01991      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
01992          ADDRESS OF CONTRACT-RECORD.                              ELTANEST
01993                                                                   ELTANEST
01994  6140-SET-CON-REC-ELSCONPS-PTR.                                   ELTANEST
01995      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTANEST
01996      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
01997          ADDRESS OF CONTRACT-RECORD.                              ELTANEST
01998                                                                   ELTANEST
01999  6150-ELIG-METH-TREATMENT.                                        ELTANEST
02000 ****************************************************************  ELTANEST
02001 *   E L I G I B L E   M E T H O D   O F   T R E A T M E N T    *  ELTANEST
02002 ****************************************************************  ELTANEST
02003      SET  PLT-INDEX2  TO  1.                                      ELTANEST
02004      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
02005                          AND                                      ELTANEST
02006         PLC-ELIG-METHD-OF-TREAT-IND (PLT-INDEX1, PLT-INDEX2)      ELTANEST
02007                                  NOT =  ZERO                      ELTANEST
02008          MOVE +2  TO  WS-CIA                                      ELTANEST
02009          MOVE WS-ELIG-METH-TREAT  TO  COF-DTL-LINE (WS-CIA)       ELTANEST
02010          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                        ELTANEST
02011                                                                   ELTANEST
02012      SET  PLT-INDEX2  TO  2.                                      ELTANEST
02013      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
02014                           AND                                     ELTANEST
02015         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
02016                                NOT =   ZERO                       ELTANEST
02017                           AND                                     ELTANEST
02018         NOT  WS-ADD-A-BLANK-LINE                                  ELTANEST
02019          MOVE +2  TO  WS-CIA                                      ELTANEST
02020          MOVE WS-ELIG-METH-TREAT  TO  COF-DTL-LINE(WS-CIA)        ELTANEST
02021          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                        ELTANEST
02022                                                                   ELTANEST
02023      SET  PLT-INDEX2  TO  1.                                      ELTANEST
02024      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
02025                           AND                                     ELTANEST
02026         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
02027                                  NOT =  ZERO                      ELTANEST
02028          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
02029          MOVE 'ELIG-METHD-OF-TREAT-IND'                           ELTANEST
02030                      TO  CMF-ELEMENT-SYSTEM-NAME                  ELTANEST
02031          MOVE PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
02032                               TO  CMF-CODE-VALUE                  ELTANEST
02033          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTANEST
02034          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
02035          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
02036             THRU 9500-EXIT.                                       ELTANEST
02037                                                                   ELTANEST
02038      SET PLT-INDEX2  TO  2.                                       ELTANEST
02039      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTANEST
02040                           AND                                     ELTANEST
02041         PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTANEST
02042                                 NOT =   ZERO                      ELTANEST
02043          MOVE 'BPC'  TO  CMF-RECORD-PREFIX                        ELTANEST
02044          MOVE 'ELIG-METHD-OF-TREAT-IND' TO CMF-ELEMENT-SYSTEM-NAMEELTANEST
02045          MOVE PLC-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
02046                               TO  CMF-CODE-VALUE                  ELTANEST
02047          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTANEST
02048          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTANEST
02049          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTANEST
02050             THRU 9500-EXIT.                                       ELTANEST
02051                                                                   ELTANEST
02052      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
02053         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
02054         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
02055            THRU 9200-EXIT.                                        ELTANEST
02056 **---------------------------------------------------------------+ELTANEST
02057                                                                   ELTANEST
02058      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROS               ELTANEST
02059         SET PLT-INDEX2  TO  2                                     ELTANEST
02060      ELSE                                                         ELTANEST
02061         SET PLT-INDEX2  TO  1.                                    ELTANEST
02062                                                                   ELTANEST
02063  6150-EXIT.  EXIT.                                                ELTANEST
02064  6160-SPILLOVR-COINS-N-DEDUC.                                     ELTANEST
02065 ****************************************************************  ELTANEST
02066 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTANEST
02067 ****************************************************************  ELTANEST
02068      MOVE +1  TO  WS-CIA.                                         ELTANEST
02069                                                                   ELTANEST
02070      SET  PLT-INDEX2  TO  2.                                      ELTANEST
02071      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
02072                            AND                                    ELTANEST
02073         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTANEST
02074                                                  NOT =  '0'       ELTANEST
02075         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
02076         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTANEST
02077                                           CMF-ELEMENT-SYSTEM-NAME ELTANEST
02078         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTANEST
02079                                                TO  CMF-CODE-VALUE ELTANEST
02080         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTANEST
02081         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTANEST
02082         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
02083            THRU 9500-EXIT                                         ELTANEST
02084         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
02085            THRU 9200-EXIT.                                        ELTANEST
02086 ****************************************************************  ELTANEST
02087 *          S P I L L O V E R   D E D U C T I B L E             *  ELTANEST
02088 ****************************************************************  ELTANEST
02089      MOVE +1  TO  WS-CIA.                                         ELTANEST
02090                                                                   ELTANEST
02091      SET  PLT-INDEX2  TO  2.                                      ELTANEST
02092      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
02093                            AND                                    ELTANEST
02094         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTANEST
02095                                                  NOT =  '0'       ELTANEST
02096         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTANEST
02097         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTANEST
02098                                           CMF-ELEMENT-SYSTEM-NAME ELTANEST
02099         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTANEST
02100                                                 TO  CMF-CODE-VALUEELTANEST
02101         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTANEST
02102         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTANEST
02103         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTANEST
02104            THRU 9500-EXIT                                         ELTANEST
02105         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTANEST
02106            THRU 9200-EXIT.                                        ELTANEST
02107                                                                   ELTANEST
02108      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTANEST
02109         SET PLT-INDEX2  TO  2                                     ELTANEST
02110      ELSE                                                         ELTANEST
02111         SET PLT-INDEX2  TO  1.                                    ELTANEST
02112                                                                   ELTANEST
02113  6160-EXIT.  EXIT.                                                ELTANEST
02114      SKIP3                                                        ELTANEST
02115  6170-AAR-PPF-PVE-TABS.                                           ELTANEST
02116 ****************************************************************  ELTANEST
02117 *                  A A R   T A B U L A R                       *  ELTANEST
02118 ****************************************************************  ELTANEST
02119      SET PLT-INDEX2  TO  1.                                       ELTANEST
02120      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
02121         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02122                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02123         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
02124         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTANEST
02125         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTANEST
02126      ELSE                                                         ELTANEST
02127         SET PLT-INDEX2  TO  2                                     ELTANEST
02128         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02129            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02130                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02131            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTANEST
02132            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTANEST
02133            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTANEST
02134                                                                   ELTANEST
02135      IF WS-ADD-A-BLANK-LINE                                       ELTANEST
02136         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTANEST
02137         MOVE 1  TO  WS-CIA                                        ELTANEST
02138         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTANEST
02139             COMMAREA(DFHCOMMAREA)                                 ELTANEST
02140         END-EXEC.                                                 ELTANEST
02141 *--------------------------------------------------------------*  ELTANEST
02142 *                  P P F   T A B U L A R                       *  ELTANEST
02143 *--------------------------------------------------------------*  ELTANEST
02144      SET PLT-INDEX2  TO  1.                                       ELTANEST
02145      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
02146         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02147                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02148         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
02149                                                 KWA-GCTABULR-KEY  ELTANEST
02150         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
02151            THRU 9900-EXIT                                         ELTANEST
02152         IF IOP-RC-OK                                              ELTANEST
02153            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTANEST
02154                 COMMAREA(DFHCOMMAREA)                             ELTANEST
02155            END-EXEC                                               ELTANEST
02156         ELSE                                                      ELTANEST
02157            NEXT SENTENCE                                          ELTANEST
02158      ELSE                                                         ELTANEST
02159         SET PLT-INDEX2  TO  2                                     ELTANEST
02160         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02161            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02162                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02163          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
02164                                                 KWA-GCTABULR-KEY  ELTANEST
02165            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
02166               THRU 9900-EXIT                                      ELTANEST
02167            IF IOP-RC-OK                                           ELTANEST
02168               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTANEST
02169                    COMMAREA(DFHCOMMAREA)                          ELTANEST
02170               END-EXEC.                                           ELTANEST
02171 *--------------------------------------------------------------*  ELTANEST
02172 *                  P V E   T A B U L A R                       *  ELTANEST
02173 *--------------------------------------------------------------*  ELTANEST
02174                                                                   ELTANEST
02175      MOVE  +2     TO  WS-CIA.                                     ELTANEST
02176      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTANEST
02177      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
02178         THRU 9200-EXIT.                                           ELTANEST
02179                                                                   ELTANEST
02180  6170-EXIT.  EXIT.                                                ELTANEST
02181  6180-ALL-LEVEL-TABS.                                             ELTANEST
02182 *--------------------------------------------------------------*  ELTANEST
02183 *                  A B M   T A B U L A R                       *  ELTANEST
02184 *--------------------------------------------------------------*  ELTANEST
02185      SET PLT-INDEX2  TO  1.                                       ELTANEST
02186      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTANEST
02187                              AND                                  ELTANEST
02188         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02189                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02190         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
02191                                                 KWA-GCTABULR-KEY  ELTANEST
02192         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
02193            THRU 9900-EXIT                                         ELTANEST
02194         IF IOP-RC-OK                                              ELTANEST
02195            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTANEST
02196                 COMMAREA(DFHCOMMAREA)                             ELTANEST
02197            END-EXEC                                               ELTANEST
02198         ELSE                                                      ELTANEST
02199            NEXT SENTENCE                                          ELTANEST
02200      ELSE                                                         ELTANEST
02201         SET PLT-INDEX2  TO  2                                     ELTANEST
02202         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02203            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02204                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02205          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
02206                                                 KWA-GCTABULR-KEY  ELTANEST
02207            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
02208               THRU 9900-EXIT                                      ELTANEST
02209            IF IOP-RC-OK                                           ELTANEST
02210               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTANEST
02211                    COMMAREA(DFHCOMMAREA)                          ELTANEST
02212               END-EXEC.                                           ELTANEST
02213 *--------------------------------------------------------------*  ELTANEST
02214 *                  A C L   T A B U L A R                       *  ELTANEST
02215 *--------------------------------------------------------------*  ELTANEST
02216      SET PLT-INDEX2  TO  1.                                       ELTANEST
02217      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
02218         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02219                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02220         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
02221                                              KWA-GCTABULR-KEY     ELTANEST
02222         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
02223            THRU 9900-EXIT                                         ELTANEST
02224         IF IOP-RC-OK                                              ELTANEST
02225            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTANEST
02226                 COMMAREA(DFHCOMMAREA)                             ELTANEST
02227            END-EXEC                                               ELTANEST
02228         ELSE                                                      ELTANEST
02229            NEXT SENTENCE                                          ELTANEST
02230      ELSE                                                         ELTANEST
02231         SET PLT-INDEX2  TO  2                                     ELTANEST
02232         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02233            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02234                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02235          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
02236                                               KWA-GCTABULR-KEY    ELTANEST
02237            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
02238               THRU 9900-EXIT                                      ELTANEST
02239            IF IOP-RC-OK                                           ELTANEST
02240               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTANEST
02241                    COMMAREA(DFHCOMMAREA)                          ELTANEST
02242               END-EXEC.                                           ELTANEST
02243 *--------------------------------------------------------------*  ELTANEST
02244 *                  A D L   T A B U L A R                       *  ELTANEST
02245 *--------------------------------------------------------------*  ELTANEST
02246      SET PLT-INDEX2  TO  1.                                       ELTANEST
02247      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
02248         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02249                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02250         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
02251                                                KWA-GCTABULR-KEY   ELTANEST
02252         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
02253            THRU 9900-EXIT                                         ELTANEST
02254         IF IOP-RC-OK                                              ELTANEST
02255            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTANEST
02256                 COMMAREA(DFHCOMMAREA)                             ELTANEST
02257            END-EXEC                                               ELTANEST
02258         ELSE                                                      ELTANEST
02259            NEXT SENTENCE                                          ELTANEST
02260      ELSE                                                         ELTANEST
02261         SET PLT-INDEX2  TO  2                                     ELTANEST
02262         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02263            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02264                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02265          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
02266                                                KWA-GCTABULR-KEY   ELTANEST
02267            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
02268               THRU 9900-EXIT                                      ELTANEST
02269            IF IOP-RC-OK                                           ELTANEST
02270               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTANEST
02271                    COMMAREA(DFHCOMMAREA)                          ELTANEST
02272               END-EXEC.                                           ELTANEST
02273 *--------------------------------------------------------------*  ELTANEST
02274 *                  A O L   T A B U L A R                       *  ELTANEST
02275 *--------------------------------------------------------------*  ELTANEST
02276      SET PLT-INDEX2  TO  1.                                       ELTANEST
02277      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTANEST
02278         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTANEST
02279                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02280         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTANEST
02281                                                KWA-GCTABULR-KEY   ELTANEST
02282         PERFORM 9900-GET-TABULAR-RECORD                           ELTANEST
02283            THRU 9900-EXIT                                         ELTANEST
02284         IF IOP-RC-OK                                              ELTANEST
02285            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTANEST
02286                 COMMAREA(DFHCOMMAREA)                             ELTANEST
02287            END-EXEC                                               ELTANEST
02288         ELSE                                                      ELTANEST
02289            NEXT SENTENCE                                          ELTANEST
02290      ELSE                                                         ELTANEST
02291         SET PLT-INDEX2  TO  2                                     ELTANEST
02292         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTANEST
02293            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTANEST
02294                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTANEST
02295          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTANEST
02296                                                KWA-GCTABULR-KEY   ELTANEST
02297            PERFORM 9900-GET-TABULAR-RECORD                        ELTANEST
02298               THRU 9900-EXIT                                      ELTANEST
02299            IF IOP-RC-OK                                           ELTANEST
02300               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTANEST
02301                    COMMAREA(DFHCOMMAREA)                          ELTANEST
02302               END-EXEC.                                           ELTANEST
02303  6180-EXIT.  EXIT.                                                ELTANEST
02304 /                                                                 ELTANEST
02305  7000-PAY-CONSID-TEXT.                                            ELTANEST
02306      INITIALIZE TCAR-FROM-AREA.                                   ELTANEST
02307      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTANEST
02308             WS-PAY-CONSDR-TEXT2                                   ELTANEST
02309                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTANEST
02310      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTANEST
02311      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTANEST
02312      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTANEST
02313                                TCAR-OUTPUT-FIELD-2-LEN.           ELTANEST
02314      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTANEST
02315      IF WS-CIA > 17                                               ELTANEST
02316            PERFORM 9200-TEXT-OUTPUT-REQUEST                       ELTANEST
02317                         THRU 9200-EXIT                            ELTANEST
02318            MOVE +1            TO WS-CIA.                          ELTANEST
02319      ADD +1                TO  WS-CIA.                            ELTANEST
02320      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTANEST
02321      ADD +1                TO  WS-CIA.                            ELTANEST
02322      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTANEST
02323      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTANEST
02324         THRU 9200-EXIT.                                           ELTANEST
02325  7000-EXIT.   EXIT.                                               ELTANEST
02326 /                                                                 ELTANEST
02327 ****************************************************************  ELTANEST
02328 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTANEST
02329 ****************************************************************  ELTANEST
02330  9100-HEADER-OUTPUT-REQUEST.                                      ELTANEST
02331                                                                   ELTANEST
02332      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTANEST
02333      MOVE 'P'            TO  COF-FUNCTION.                        ELTANEST
02334                                                                   ELTANEST
02335      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTANEST
02336                     COMMAREA (DFHCOMMAREA)                        ELTANEST
02337                     END-EXEC.                                     ELTANEST
02338                                                                   ELTANEST
02339  9100-EXIT.  EXIT.                                                ELTANEST
02340      SKIP3                                                        ELTANEST
02341 ****************************************************************  ELTANEST
02342 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTANEST
02343 ****************************************************************  ELTANEST
02344  9200-TEXT-OUTPUT-REQUEST.                                        ELTANEST
02345                                                                   ELTANEST
02346      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTANEST
02347      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELTANEST
02348      MOVE ' '     TO  COF-FUNCTION.                               ELTANEST
02349                                                                   ELTANEST
02350      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTANEST
02351                     COMMAREA (DFHCOMMAREA)                        ELTANEST
02352                     END-EXEC.                                     ELTANEST
02353                                                                   ELTANEST
02354  9200-EXIT.  EXIT.                                                ELTANEST
02355  9400-EXPLAINATION-OUTPUT.                                        ELTANEST
02356      MOVE '9400'  TO  WS-PARA-ID2.                                ELTANEST
02357      MOVE +0      TO  WS-CIA.                                     ELTANEST
02358                                                                   ELTANEST
02359      IF WS-BASIC-EXPLANATION                                      ELTANEST
02360         ADD +1  TO  WS-CIA                                        ELTANEST
02361         MOVE WS-BASIC-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)          ELTANEST
02362         IF WS-BASIC-EXPLAIN-CNT  >  1                             ELTANEST
02363            ADD +1  TO  WS-CIA                                     ELTANEST
02364            MOVE WS-BASIC-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA).      ELTANEST
02365                                                                   ELTANEST
02366      IF WS-SUPP-EXPLANATION                                       ELTANEST
02367         ADD +1  TO  WS-CIA                                        ELTANEST
02368         MOVE WS-SUPP-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)           ELTANEST
02369         IF WS-SUPP-EXPLAIN-CNT  >  1                              ELTANEST
02370            ADD +1  TO  WS-CIA                                     ELTANEST
02371            MOVE WS-SUPP-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA).       ELTANEST
02372                                                                   ELTANEST
02373      MOVE ZERO    TO  WS-EXPLANATION-IND.                         ELTANEST
02374      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTANEST
02375                                                                   ELTANEST
02376      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTANEST
02377                       COMMAREA(DFHCOMMAREA)                       ELTANEST
02378                       END-EXEC.                                   ELTANEST
02379                                                                   ELTANEST
02380  9400-EXIT.  EXIT.                                                ELTANEST
02381 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTANEST
02382  9500-CALL-CODES-MANUAL-LONG.                                     ELTANEST
02383      MOVE '9500'  TO  WS-PARA-ID2.                                ELTANEST
02384                                                                   ELTANEST
02385      INITIALIZE CMF-RETURN-CODE,                                  ELTANEST
02386                 TCAR-FROM-AREA,                                   ELTANEST
02387                 TCAR-TO-AREA.                                     ELTANEST
02388                                                                   ELTANEST
02389      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTANEST
02390      END-EXEC.                                                    ELTANEST
02391                                                                   ELTANEST
02392      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTANEST
02393      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
02394          ADDRESS OF CMF-DESCR.                                    ELTANEST
02395                                                                   ELTANEST
02396      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTANEST
02397         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTANEST
02398         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTANEST
02399            CMF-DESCR-LINE(1),        ' ',                         ELTANEST
02400            CMF-DESCR-LINE(2),        ' ',                         ELTANEST
02401            CMF-DESCR-LINE(3)                                      ELTANEST
02402            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTANEST
02403      ELSE                                                         ELTANEST
02404         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTANEST
02405         STRING CMF-DESCR-LINE(1),        ' ',                     ELTANEST
02406            CMF-DESCR-LINE(2),        ' ',                         ELTANEST
02407            CMF-DESCR-LINE(3)                                      ELTANEST
02408            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTANEST
02409                                                                   ELTANEST
02410      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTANEST
02411                                                                   ELTANEST
02412      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTANEST
02413      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTANEST
02414      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTANEST
02415      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTANEST
02416                                                                   ELTANEST
02417      IF WS-MOVE-LINES-TO-CIA                                      ELTANEST
02418         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTANEST
02419            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTANEST
02420                                             WS-TEMP-NOT-USED-CNT  ELTANEST
02421            MOVE '9550'  TO  WS-PARA-ID2                           ELTANEST
02422            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTANEST
02423               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTANEST
02424                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTANEST
02425            MOVE '9500'  TO  WS-PARA-ID2                           ELTANEST
02426            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTANEST
02427            ADD +1  TO  WS-CIA                                     ELTANEST
02428            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTANEST
02429         ELSE                                                      ELTANEST
02430            ADD +1  TO  WS-CIA                                     ELTANEST
02431            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTANEST
02432                                                                   ELTANEST
02433      IF WS-MOVE-LINES-TO-CIA                                      ELTANEST
02434         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTANEST
02435            ADD +1  TO  WS-CIA                                     ELTANEST
02436            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTANEST
02437         ELSE                                                      ELTANEST
02438            NEXT SENTENCE                                          ELTANEST
02439      ELSE                                                         ELTANEST
02440         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTANEST
02441                                                                   ELTANEST
02442  9500-EXIT.  EXIT.                                                ELTANEST
02443                                                                   ELTANEST
02444  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTANEST
02445      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTANEST
02446      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTANEST
02447                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTANEST
02448                                                                   ELTANEST
02449  9550-EXIT.  EXIT.                                                ELTANEST
02450                                                                   ELTANEST
02451 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTANEST
02452  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTANEST
02453      MOVE '9600'  TO  WS-PARA-ID2.                                ELTANEST
02454                                                                   ELTANEST
02455      INITIALIZE CMF-RETURN-CODE,                                  ELTANEST
02456                 TCAR-FROM-AREA,                                   ELTANEST
02457                 TCAR-TO-AREA.                                     ELTANEST
02458                                                                   ELTANEST
02459      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTANEST
02460      END-EXEC.                                                    ELTANEST
02461                                                                   ELTANEST
02462      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTANEST
02463      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
02464          ADDRESS OF CMF-DESCR.                                    ELTANEST
02465                                                                   ELTANEST
02466      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTANEST
02467         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTANEST
02468         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTANEST
02469            CMF-DESCR-LINE(1),        ' ',                         ELTANEST
02470            CMF-DESCR-LINE(2),        ' ',                         ELTANEST
02471            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTANEST
02472            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTANEST
02473      ELSE                                                         ELTANEST
02474         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTANEST
02475         STRING CMF-DESCR-LINE(1),        ' ',                     ELTANEST
02476            CMF-DESCR-LINE(2),        ' ',                         ELTANEST
02477            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTANEST
02478            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTANEST
02479                                                                   ELTANEST
02480      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTANEST
02481                                                                   ELTANEST
02482      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTANEST
02483      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTANEST
02484      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTANEST
02485                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTANEST
02486                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTANEST
02487      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTANEST
02488                                                                   ELTANEST
02489      IF WS-MOVE-LINES-TO-CIA                                      ELTANEST
02490         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTANEST
02491            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTANEST
02492                                             WS-TEMP-NOT-USED-CNT  ELTANEST
02493            MOVE '9650'  TO  WS-PARA-ID2                           ELTANEST
02494            PERFORM 9650-CONCATENATE-TO-TEMP-TEXT                  ELTANEST
02495               THRU 9650-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTANEST
02496                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTANEST
02497            MOVE '9600'  TO  WS-PARA-ID2                           ELTANEST
02498            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTANEST
02499            ADD +1  TO  WS-CIA                                     ELTANEST
02500            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTANEST
02501         ELSE                                                      ELTANEST
02502            ADD +1  TO  WS-CIA                                     ELTANEST
02503            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTANEST
02504                                                                   ELTANEST
02505      IF WS-MOVE-LINES-TO-CIA                                      ELTANEST
02506         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTANEST
02507            MOVE '9650'  TO  WS-PARA-ID2                           ELTANEST
02508            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTANEST
02509               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTANEST
02510                              UNTIL   WS-SUB1 >                    ELTANEST
02511                              TCAR-OUTPUT-FIELDS-USED              ELTANEST
02512         ELSE                                                      ELTANEST
02513            NEXT SENTENCE                                          ELTANEST
02514      ELSE                                                         ELTANEST
02515         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTANEST
02516                                                                   ELTANEST
02517  9600-EXIT.  EXIT.                                                ELTANEST
02518                                                                   ELTANEST
02519  9650-CONCATENATE-TO-TEMP-TEXT.                                   ELTANEST
02520      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTANEST
02521      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTANEST
02522                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTANEST
02523                                                                   ELTANEST
02524  9650-EXIT.  EXIT.                                                ELTANEST
02525      SKIP3                                                        ELTANEST
02526  9660-MOVE-LINES-TO-CIA.                                          ELTANEST
02527      ADD +1  TO  WS-CIA.                                          ELTANEST
02528      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTANEST
02529                                                                   ELTANEST
02530  9660-EXIT.  EXIT.                                                ELTANEST
02531 /                                                                 ELTANEST
02532 ***************************************************************** ELTANEST
02533 *            B A S I C   E X P L A N A T I O N                    ELTANEST
02534 ***************************************************************** ELTANEST
02535  9700-BASIC-EXPLANATION.                                          ELTANEST
02536      MOVE '9700'  TO  WS-PARA-ID3.                                ELTANEST
02537                                                                   ELTANEST
02538      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTANEST
02539      MOVE 'N'  TO  WS-MOVE-LINES-IND.                             ELTANEST
02540      MOVE SPACES  TO  WS-DTL-BASIC-LONG  WS-BASIC-EXPLAIN1,       ELTANEST
02541                       WS-BASIC-EXPLAIN2.                          ELTANEST
02542      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTANEST
02543      ADD +1  TO  WS-EXPLANATION-IND.                              ELTANEST
02544      MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA.                    ELTANEST
02545      MOVE +63  TO  WS-TEMP-NOT-USED-CNT.                          ELTANEST
02546      PERFORM 9500-CALL-CODES-MANUAL-LONG                          ELTANEST
02547         THRU 9500-EXIT.                                           ELTANEST
02548      COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -  WS-TEMP-NOT-USED-CNT.ELTANEST
02549      PERFORM 9750-CONCATENATE-TO-TEMP-TEXT                        ELTANEST
02550         THRU 9750-EXIT VARYING WS-SUB1 FROM  1  BY  1             ELTANEST
02551                        UNTIL  WS-TEMP-NOT-USED-CNT  >  +78.       ELTANEST
02552      MOVE WS-TEMP-TEXT-AREA  TO  WS-BASIC-EXPLAIN1.               ELTANEST
02553      IF TCAR-OUTPUT-FIELDS-USED  >  1                             ELTANEST
02554         MOVE 2  TO  WS-BASIC-EXPLAIN-CNT                          ELTANEST
02555         MOVE TCAR-OPF-DATA(2)  TO  WS-BASIC-EXPLAIN2              ELTANEST
02556      ELSE                                                         ELTANEST
02557         MOVE 1  TO  WS-BASIC-EXPLAIN-CNT.                         ELTANEST
02558                                                                   ELTANEST
02559  9700-EXIT.  EXIT.                                                ELTANEST
02560  9750-CONCATENATE-TO-TEMP-TEXT.                                   ELTANEST
02561      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTANEST
02562      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTANEST
02563                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTANEST
02564                                                                   ELTANEST
02565  9750-EXIT.  EXIT.                                                ELTANEST
02566 /                                                                 ELTANEST
02567 ***************************************************************** ELTANEST
02568 *            S U P P   E X P L A N A T I O N                      ELTANEST
02569 ***************************************************************** ELTANEST
02570  9800-SUPP-EXPLANATION.                                           ELTANEST
02571      MOVE '9800'  TO  WS-PARA-ID3.                                ELTANEST
02572                                                                   ELTANEST
02573      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTANEST
02574      MOVE SPACES  TO  WS-DTL-SUPP-LONG,  WS-SUPP-EXPLAIN1,        ELTANEST
02575                       WS-SUPP-EXPLAIN2.                           ELTANEST
02576      MOVE 'N'  TO  WS-MOVE-LINES-IND.                             ELTANEST
02577      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTANEST
02578      ADD +2  TO  WS-EXPLANATION-IND.                              ELTANEST
02579      MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA.                     ELTANEST
02580      MOVE +63  TO  WS-TEMP-NOT-USED-CNT.                          ELTANEST
02581      PERFORM 9500-CALL-CODES-MANUAL-LONG                          ELTANEST
02582         THRU 9500-EXIT.                                           ELTANEST
02583                                                                   ELTANEST
02584      COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -  WS-TEMP-NOT-USED-CNT.ELTANEST
02585      PERFORM 9850-CONCATENATE-TO-TEMP-TEXT                        ELTANEST
02586         THRU 9850-EXIT VARYING WS-SUB1  FROM  1  BY  1            ELTANEST
02587                        UNTIL  WS-TEMP-NOT-USED-CNT  >  +78.       ELTANEST
02588                                                                   ELTANEST
02589      MOVE WS-TEMP-TEXT-AREA  TO  WS-SUPP-EXPLAIN1.                ELTANEST
02590      IF TCAR-OUTPUT-FIELDS-USED  >  1                             ELTANEST
02591         MOVE 2  TO  WS-SUPP-EXPLAIN-CNT                           ELTANEST
02592         MOVE TCAR-OPF-DATA(2)  TO  WS-SUPP-EXPLAIN2               ELTANEST
02593      ELSE                                                         ELTANEST
02594         MOVE 1  TO  WS-SUPP-EXPLAIN-CNT.                          ELTANEST
02595                                                                   ELTANEST
02596  9800-EXIT. EXIT.                                                 ELTANEST
02597      SKIP3                                                        ELTANEST
02598  9850-CONCATENATE-TO-TEMP-TEXT.                                   ELTANEST
02599      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTANEST
02600      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTANEST
02601                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTANEST
02602                                                                   ELTANEST
02603  9850-EXIT.  EXIT.                                                ELTANEST
02604 /                                                                 ELTANEST
02605 ***************************************************************** ELTANEST
02606 *            G E T   T A B U L A R   R E C O R D                  ELTANEST
02607 *                                                                 ELTANEST
02608 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTANEST
02609 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTANEST
02610 *  TO DISPLAY.                                                    ELTANEST
02611 *                                                                 ELTANEST
02612 ***************************************************************** ELTANEST
02613  9900-GET-TABULAR-RECORD.                                         ELTANEST
02614      MOVE '9900'  TO  WS-PARA-ID2.                                ELTANEST
02615                                                                   ELTANEST
02616      SET CIA-GCTABULR-DDN TO TRUE.                                ELTANEST
02617      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTANEST
02618          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTANEST
02619      MOVE KWA-GCTABULR-KEY                TO IOP-FILE-KEY.        ELTANEST
02620      SET CIA-GCTABULR-DDN                 TO TRUE.                ELTANEST
02621      SET IOP-RD                           TO TRUE.                ELTANEST
02622      SET IOP-FCQ-NONE                     TO TRUE.                ELTANEST
02623      SET IOP-KVQ-NONE                     TO TRUE.                ELTANEST
02624      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTANEST
02625             COMMAREA(DFHCOMMAREA)                                 ELTANEST
02626      END-EXEC.                                                    ELTANEST
02627                                                                   ELTANEST
02628      IF IOP-RC-NOTFND                                             ELTANEST
02629         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTANEST
02630         EXEC CICS ABEND                                           ELTANEST
02631                   ABCODE(CIA-ABCODE)                              ELTANEST
02632         END-EXEC.                                                 ELTANEST
02633                                                                   ELTANEST
02634      IF NOT IOP-RC-OK                                             ELTANEST
02635         SET CIA-AB-CRITIO          TO TRUE                        ELTANEST
02636         EXEC CICS ABEND                                           ELTANEST
02637                   ABCODE(CIA-ABCODE)                              ELTANEST
02638         END-EXEC.                                                 ELTANEST
02639                                                                   ELTANEST
02640  9900-EXIT.  EXIT.                                                ELTANEST
02641                                                                   ELTANEST
02642      COPY ELSTCOMP.                                               ELTANEST
