00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.    ELTDRUGS.                                         ELTDRUGS
00003  AUTHOR.        LUCY TORRES.                                         LV002
00004  DATE-WRITTEN.  07/15/86                                          ELTDRUGS
00005  DATE-COMPILED.                                                   ELTDRUGS
00006      SKIP3                                                        ELTDRUGS
00007 ****************************************************************  ELTDRUGS
00008 *      ELTDRUGS - ELS:  DRUGS/MEDICATION TOPIC PROGRAM         *  ELTDRUGS
00009 ****************************************************************  ELTDRUGS
00010      TITLE 'PROGRAM HISTORY'.                                     ELTDRUGS
00011 ****************************************************************  ELTDRUGS
00012 *              U P D A T E   H I S T O R Y                     *  ELTDRUGS
00013 *                                                              *  ELTDRUGS
00014 *   DATE    PGM  DESCRIPTION                                   *  ELTDRUGS
00015 * --------  ---  --------------------------------------------- *  ELTDRUGS
00016 * 07/15/86  LET  ORIGINAL VERSION                              *  ELTDRUGS
00017 *                                                              *  ELTDRUGS
00018 * 08/13/86  LET  USING A HEADER LINE FROM PROLOG               *  ELTDRUGS
00019 *                                                              *  ELTDRUGS
00020 * 10/07/86  JTC  VS COBOL II CONVERSION                        *  ELTDRUGS
00021 *                                                              *  ELTDRUGS
00022 * 08/14/87  AKK  ADDED CODE TO HAVE INJECTIONS ADDED TO        *  ELTDRUGS
00023 *                PROFESSIONAL BENEFITS.                        *  ELTDRUGS
00024 *                                                              *  ELTDRUGS
00025 * 10/17/87  AKK  CHANGED 'THIS GROUP OF COVERED BENEFITS ARE   *  ELTDRUGS
00026 *                HANDLED AS FOLLOWS' TO 'COVERED SERVICES ARE' *  ELTDRUGS
00027 *                                                              *  ELTDRUGS
00028 * 03/20/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS               *  ELTDRUGS
00029 *                                                              *  ELTDRUGS
00030 * 09/22/89  RKH  ADDED CODE FOR TRANSFER TO OTHER              *  ELTDRUGS
00031 *                RESPONSIBILITY IND.                           *  ELTDRUGS
00032 *                                                              *  ELTDRUGS
00033 * 09/17/90  GEM  ADDED/DELETED BENEFIT PROVISION IDS.          *  ELTDRUGS
00034 *                                                              *  ELTDRUGS
00035 * 09/20/90  GEM  USAGE OF WS-INST-IP-CNT TO CONTROL THE EXECU- *  ELTDRUGS
00036 *                TION OF '6106-ZERO-ALL-WITH-SAME-NO' GENERATED*  ELTDRUGS
00037 *                INCOMPLETE BP LIST. ADDED WS-LIST-BP-CNT, A   *  ELTDRUGS
00038 *                GENERIC FIELD, TO REFLECT THE ACCURATE BP CNT.*  ELTDRUGS
00039 *                                                              *  ELTDRUGS
00040 * 11/15/90  GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND COMPARE TO  *  ELTDRUGS
00041 *                THE LITERAL ZERO, INSTEAD OF THE DIGIT '0'    *  ELTDRUGS
00042 *                DO TO THE EXPANSION OF THE RDW GCBENPVC.      *  ELTDRUGS
00043 *                                                              *  ELTDRUGS
00044 *                                                              *  ELTDRUGS
00045 * 02/20/92  AKK  ADDED MANY BENEFIT PROVISIONS TO ALL FOUR     *  ELTDRUGS
00046 *                CATEGORIES (INST PROF IP AND OP).             *  ELTDRUGS
00047 *                                                              *  ELTDRUGS
00048 * 05/23/95  AKK  ADD SUPPORT FOR BLUE SCRIPT. ALSO ADDED       *  ELTDRUGS
00049 *                CERT REQ IND AS PART OF THE ADDITION OF       *  ELTDRUGS
00050 *                OF THE CERT REQ IND TO ALL BP PROGS.          *  ELTDRUGS
00051 *                                                              *  ELTDRUGS
00052 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTDRUGS
00053 *                                                              *  ELTDRUGS
00054 ****************************************************************  ELTDRUGS
00055      TITLE 'WORKING STORAGE SECTION'.                             ELTDRUGS
00056  ENVIRONMENT DIVISION.                                            ELTDRUGS
00057                                                                   ELTDRUGS
00058  DATA DIVISION.                                                   ELTDRUGS
00059  WORKING-STORAGE SECTION.                                         ELTDRUGS
00060  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTDRUGS
00061          '** ELTDRUGS WS BEGINS **'.                              ELTDRUGS
00062  01  WS-PARA-ID1                 PIC  X(04) VALUE 'XXXX'.         ELTDRUGS
00063  01  WS-PARA-ID2                 PIC  X(04) VALUE 'XXXX'.         ELTDRUGS
00064  01  WS-PARA-ID3                 PIC  X(04) VALUE 'XXXX'.         ELTDRUGS
00065  01  WS-ABEND-CODE               PIC  X(04) VALUE 'DRUG'.         ELTDRUGS
00066                                                                   ELTDRUGS
00067 /***************************************************************  ELTDRUGS
00068 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTDRUGS
00069 ****************************************************************  ELTDRUGS
00070  01  WORK-FIELDS.                                                 ELTDRUGS
00071      05  WS-CHAR-0               PIC  X(01).                      ELTDRUGS
00072      05  WS-CIA-PNTR             PIC S9(08) COMP   VALUE ZEROS.   ELTDRUGS
00073      05  WS-BS-SUB               PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00074      05  WS-SUB                  PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00075      05  WS-SUB1                 PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00076      05  WS-SUB2                 PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00077      05  WS-SUB3                 PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00078      05  WS-SUB4                 PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00079      05  WS-CIA                  PIC S9(03) COMP-3 VALUE +0.      ELTDRUGS
00080      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP-3.               ELTDRUGS
00081      05  WS-REC-LEN              PIC S9(04) COMP   VALUE +0.      ELTDRUGS
00082      05  WS-PERCENT-FLD.                                          ELTDRUGS
00083        10  WS-PERCENTAGE         PIC ZZ9.                         ELTDRUGS
00084        10  WS-PERCENT-SIGN       PIC X.                           ELTDRUGS
00085      05  WS-EXPLANATION-IND      PIC S9 COMP-3.                   ELTDRUGS
00086          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTDRUGS
00087          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTDRUGS
00088          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTDRUGS
00089          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTDRUGS
00090          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTDRUGS
00091          88  WS-NO-EXPLANATION             VALUE +0.              ELTDRUGS
00092      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP-3.                   ELTDRUGS
00093      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP-3.                   ELTDRUGS
00094                                                                   ELTDRUGS
00095  01  WS-WORK-AREA.                                                ELTDRUGS
00096      05  WS-MAX-AMT.                                              ELTDRUGS
00097          10  WS-BASIC-SUPP       PIC  X(17) VALUE SPACES.         ELTDRUGS
00098          10  WS-EDIT-MAX-AMT     PIC  -$$9.99.                    ELTDRUGS
00099                                                                   ELTDRUGS
00100  01  WS-EXPLAINS.                                                 ELTDRUGS
00101    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTDRUGS
00102    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTDRUGS
00103    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTDRUGS
00104    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTDRUGS
00105                                                                   ELTDRUGS
00106  01  SWITCHES.                                                    ELTDRUGS
00107      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTDRUGS
00108          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTDRUGS
00109      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTDRUGS
00110          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTDRUGS
00111      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTDRUGS
00112          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTDRUGS
00113      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTDRUGS
00114          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTDRUGS
00115                                                                   ELTDRUGS
00116 /--------------------------------------------------------------*  ELTDRUGS
00117 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTDRUGS
00118 *--------------------------------------------------------------*  ELTDRUGS
00119  01  WS-BEN-PROV-IDS.                                             ELTDRUGS
00120      05  WS-TABLE-MAX-CNT        PIC S9(4)  VALUE +19 COMP.       ELTDRUGS
00121      05  WS-LIST-BP-CNT          PIC S9(04) VALUE +0 COMP.        ELTDRUGS
00122      05  WS-INST-IP-CNT          PIC S9(04) VALUE +10 COMP.       ELTDRUGS
00123      05  WS-INST-IP-TABS.                                         ELTDRUGS
00124          10  FILLER              PIC  X(06) VALUE 'BDRI B'.       ELTDRUGS
00125          10  FILLER              PIC  X(06) VALUE 'BRXI B'.       ELTDRUGS
00126          10  FILLER              PIC  X(06) VALUE 'DRGI B'.       ELTDRUGS
00127          10  FILLER              PIC  X(06) VALUE 'INJ  B'.       ELTDRUGS
00128          10  FILLER              PIC  X(06) VALUE 'MRXI B'.       ELTDRUGS
00129          10  FILLER              PIC  X(06) VALUE 'MSHI B'.       ELTDRUGS
00130          10  FILLER              PIC  X(06) VALUE 'MSSI B'.       ELTDRUGS
00131          10  FILLER              PIC  X(06) VALUE 'NOPG B'.       ELTDRUGS
00132          10  FILLER              PIC  X(06) VALUE 'RMSI B'.       ELTDRUGS
00133          10  FILLER              PIC  X(06) VALUE 'RXHI B'.       ELTDRUGS
00134      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTDRUGS
00135                                  PIC  X(06) OCCURS 10 TIMES.      ELTDRUGS
00136                                                                   ELTDRUGS
00137      05  WS-INST-OP-CNT          PIC S9(04) VALUE +19 COMP.       ELTDRUGS
00138      05  WS-INST-OP-TABS.                                         ELTDRUGS
00139          10  FILLER              PIC  X(06) VALUE 'BDRO B'.       ELTDRUGS
00140          10  FILLER              PIC  X(06) VALUE 'BRXO B'.       ELTDRUGS
00141          10  FILLER              PIC  X(06) VALUE 'DRGO B'.       ELTDRUGS
00142          10  FILLER              PIC  X(06) VALUE 'EABD B'.       ELTDRUGS
00143          10  FILLER              PIC  X(06) VALUE 'EACI B'.       ELTDRUGS
00144          10  FILLER              PIC  X(06) VALUE 'EAMS B'.       ELTDRUGS
00145          10  FILLER              PIC  X(06) VALUE 'EARX B'.       ELTDRUGS
00146          10  FILLER              PIC  X(06) VALUE 'EMBD B'.       ELTDRUGS
00147          10  FILLER              PIC  X(06) VALUE 'EMCI B'.       ELTDRUGS
00148          10  FILLER              PIC  X(06) VALUE 'EMMS B'.       ELTDRUGS
00149          10  FILLER              PIC  X(06) VALUE 'EMRX B'.       ELTDRUGS
00150          10  FILLER              PIC  X(06) VALUE 'HHRX B'.       ELTDRUGS
00151          10  FILLER              PIC X(06)  VALUE 'INJ  B'.       ELTDRUGS
00152          10  FILLER              PIC  X(06) VALUE 'MRXO B'.       ELTDRUGS
00153          10  FILLER              PIC  X(06) VALUE 'MSHO B'.       ELTDRUGS
00154          10  FILLER              PIC  X(06) VALUE 'MSSO B'.       ELTDRUGS
00155          10  FILLER              PIC  X(06) VALUE 'NOPG B'.       ELTDRUGS
00156          10  FILLER              PIC  X(06) VALUE 'RMSO B'.       ELTDRUGS
00157          10  FILLER              PIC  X(06) VALUE 'RXHO B'.       ELTDRUGS
00158      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTDRUGS
00159                                  PIC  X(06) OCCURS 19 TIMES.      ELTDRUGS
00160                                                                   ELTDRUGS
00161      05  WS-PROF-IP-CNT          PIC S9(04) VALUE +8 COMP.        ELTDRUGS
00162      05  WS-PROF-IP-TABS.                                         ELTDRUGS
00163          10  FILLER              PIC  X(06) VALUE 'BDRI E'.       ELTDRUGS
00164          10  FILLER              PIC  X(06) VALUE 'BRXI E'.       ELTDRUGS
00165          10  FILLER              PIC  X(06) VALUE 'DRGI E'.       ELTDRUGS
00166          10  FILLER              PIC  X(06) VALUE 'INJ  E'.       ELTDRUGS
00167          10  FILLER              PIC  X(06) VALUE 'MRXI E'.       ELTDRUGS
00168          10  FILLER              PIC  X(06) VALUE 'MSSI E'.       ELTDRUGS
00169          10  FILLER              PIC  X(06) VALUE 'NOPG E'.       ELTDRUGS
00170          10  FILLER              PIC  X(06) VALUE 'RMSI E'.       ELTDRUGS
00171      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTDRUGS
00172                                  PIC  X(06) OCCURS 8 TIMES.       ELTDRUGS
00173                                                                   ELTDRUGS
00174      05  WS-PROF-OP-CNT          PIC S9(04) VALUE +16 COMP.       ELTDRUGS
00175      05  WS-PROF-OP-TABS.                                         ELTDRUGS
00176          10  FILLER              PIC  X(06) VALUE 'BDRO E'.       ELTDRUGS
00177          10  FILLER              PIC  X(06) VALUE 'BRXO E'.       ELTDRUGS
00178          10  FILLER              PIC  X(06) VALUE 'DRGO E'.       ELTDRUGS
00179          10  FILLER              PIC  X(06) VALUE 'EABD E'.       ELTDRUGS
00180          10  FILLER              PIC  X(06) VALUE 'EACI E'.       ELTDRUGS
00181          10  FILLER              PIC  X(06) VALUE 'EAMS E'.       ELTDRUGS
00182          10  FILLER              PIC  X(06) VALUE 'EARX E'.       ELTDRUGS
00183          10  FILLER              PIC  X(06) VALUE 'EMBD E'.       ELTDRUGS
00184          10  FILLER              PIC  X(06) VALUE 'EMCI E'.       ELTDRUGS
00185          10  FILLER              PIC  X(06) VALUE 'EMMS E'.       ELTDRUGS
00186          10  FILLER              PIC  X(06) VALUE 'EMRX E'.       ELTDRUGS
00187          10  FILLER              PIC  X(06) VALUE 'INJ  E'.       ELTDRUGS
00188          10  FILLER              PIC  X(06) VALUE 'MRXO E'.       ELTDRUGS
00189          10  FILLER              PIC  X(06) VALUE 'MSSO E'.       ELTDRUGS
00190          10  FILLER              PIC  X(06) VALUE 'NOPG E'.       ELTDRUGS
00191          10  FILLER              PIC  X(06) VALUE 'RMSO E'.       ELTDRUGS
00192      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTDRUGS
00193                                  PIC  X(06) OCCURS 16 TIMES.      ELTDRUGS
00194                                                                   ELTDRUGS
00195 /***************************************************************  ELTDRUGS
00196 *              HEADER AND LITERAL TEXT AREA                    *  ELTDRUGS
00197 ****************************************************************  ELTDRUGS
00198  01  HEADER-LINE-2.                                               ELTDRUGS
00199      05  FILLER                  PIC  X(12) VALUE 'SECTION NO: '. ELTDRUGS
00200      05  WS-SECT-NO              PIC  9(05) VALUE ZEROS.          ELTDRUGS
00201      05  FILLER                  PIC  X(23) VALUE                 ELTDRUGS
00202              '       EFFECTIVE DATE: '.                           ELTDRUGS
00203      05  WS-EFF-DATE             PIC 99/99/99.                    ELTDRUGS
00204      05  FILLER                  PIC  X(29) VALUE                 ELTDRUGS
00205              '        FAMILY RELATIONSHIP: '.                     ELTDRUGS
00206      05  WS-FAM-REL              PIC  9(01) VALUE ZERO.           ELTDRUGS
00207      05  FILLER                  PIC  X(01) VALUE LOW-VALUE.      ELTDRUGS
00208                                                                   ELTDRUGS
00209  01  HEADER-I-IP-LINE-3.                                          ELTDRUGS
00210      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTDRUGS
00211      05  FILLER                  PIC  X(43) VALUE                 ELTDRUGS
00212              'DRUGS / MEDICATIONS INSTITUTIONAL INPATIENT'.       ELTDRUGS
00213      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTDRUGS
00214                                                                   ELTDRUGS
00215  01  HEADER-I-OP-LINE-3.                                          ELTDRUGS
00216      05  FILLER                  PIC  X(17) VALUE SPACES.         ELTDRUGS
00217      05  FILLER                  PIC  X(44) VALUE                 ELTDRUGS
00218              'DRUGS / MEDICATIONS INSTITUTIONAL OUTPATIENT'.      ELTDRUGS
00219      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTDRUGS
00220                                                                   ELTDRUGS
00221  01  HEADER-P-IP-LINE-3.                                          ELTDRUGS
00222      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTDRUGS
00223      05  FILLER                  PIC  X(42) VALUE                 ELTDRUGS
00224              'DRUGS / MEDICATIONS PROFESSIONAL INPATIENT'.        ELTDRUGS
00225      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTDRUGS
00226                                                                   ELTDRUGS
00227  01  HEADER-P-OP-LINE-3.                                          ELTDRUGS
00228      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTDRUGS
00229      05  FILLER                  PIC  X(43) VALUE                 ELTDRUGS
00230              'DRUGS / MEDICATIONS PROFESSIONAL OUTPATIENT'.       ELTDRUGS
00231      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTDRUGS
00232                                                                   ELTDRUGS
00233  01  WS-BLUE-SCRIPT1.                                             ELTDRUGS
00234      05  FILLER                  PIC  X(79) VALUE                 ELTDRUGS
00235          'PRESCRIPTION DRUGS WILL BE ELECTRONICALLY SUBMITTED BY AELTDRUGS
00236 -        ' PARTICIPATING PHARMACY'.                               ELTDRUGS
00237                                                                   ELTDRUGS
00238  01  WS-BLUE-SCRIPT2.                                             ELTDRUGS
00239      05  FILLER                  PIC  X(23) VALUE                 ELTDRUGS
00240          'FOR THIS GROUP/SECTION.'.                               ELTDRUGS
00241                                                                   ELTDRUGS
00242  01  WS-CERT-REQ.                                                 ELTDRUGS
00243      05  FILLER                  PIC  X(44) VALUE                 ELTDRUGS
00244          'CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.          ELTDRUGS
00245      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTDRUGS
00246                                                                   ELTDRUGS
00247  01  WS-SERVICES-RENDERED.                                        ELTDRUGS
00248      05  FILLER                  PIC  X(26) VALUE                 ELTDRUGS
00249              'SERVICES MAY BE RENDERED: '.                        ELTDRUGS
00250      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTDRUGS
00251                                                                   ELTDRUGS
00252  01  WS-FOLLOWING-BEN.                                            ELTDRUGS
00253      05  FILLER                  PIC  X(79) VALUE                 ELTDRUGS
00254            'COVERED SERVICES ARE:'.                               ELTDRUGS
00255                                                                   ELTDRUGS
00256  01  WS-PAY-CONSDR-TEXT1.                                         ELTDRUGS
00257      05  FILLER                  PIC X(49)                        ELTDRUGS
00258        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDRUGS
00259                                                                   ELTDRUGS
00260  01  WS-PAY-CONSDR-TEXT2.                                         ELTDRUGS
00261      05  FILLER                  PIC X(45)                        ELTDRUGS
00262        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDRUGS
00263                                                                   ELTDRUGS
00264  01  WS-PAYABLE-AS.                                               ELTDRUGS
00265      10  FILLER                  PIC  X(45) VALUE                 ELTDRUGS
00266              'THESE SERVICES ARE PRICED ACCORDING TO: '.          ELTDRUGS
00267      10  FILLER                  PIC  X(34) VALUE LOW-VALUES.     ELTDRUGS
00268                                                                   ELTDRUGS
00269  01  WS-CONTRACT-RELATED.                                         ELTDRUGS
00270      05  FILLER                  PIC  X(48) VALUE                 ELTDRUGS
00271              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTDRUGS
00272                                                                   ELTDRUGS
00273  01  WS-BASIC.                                                    ELTDRUGS
00274      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTDRUGS
00275              '         BASIC: '.                                  ELTDRUGS
00276      05  WS-DTL-BASIC-LONG.                                       ELTDRUGS
00277          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTDRUGS
00278          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTDRUGS
00279                                                                   ELTDRUGS
00280  01  WS-SUPPLEMENTAL.                                             ELTDRUGS
00281      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTDRUGS
00282              '  SUPPLEMENTAL: '.                                  ELTDRUGS
00283      05  WS-DTL-SUPP-LONG.                                        ELTDRUGS
00284          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTDRUGS
00285          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTDRUGS
00286                                                                   ELTDRUGS
00287  01  WS-PVE.                                                      ELTDRUGS
00288      05  FILLER                  PIC  X(44) VALUE                 ELTDRUGS
00289              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTDRUGS
00290      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTDRUGS
00291                                                                   ELTDRUGS
00292  01  WS-INDICES-PROBLEM.                                          ELTDRUGS
00293      05  FILLER                  PIC  X(20) VALUE                 ELTDRUGS
00294              'PROBLEM WITH INDICES'.                              ELTDRUGS
00295      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTDRUGS
00296                                                                   ELTDRUGS
00297  01  WS-POSSIBLE-ERROR.                                           ELTDRUGS
00298      05  FILLER                   PIC  X(50) VALUE                ELTDRUGS
00299              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTDRUGS
00300      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTDRUGS
00301                                                                   ELTDRUGS
00302  01  WS-INVALID-REQ.                                              ELTDRUGS
00303      05  FILLER                  PIC  X(37) VALUE                 ELTDRUGS
00304              '*** I N V A L I D   R E Q U E S T ***'.             ELTDRUGS
00305      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTDRUGS
00306                                                                   ELTDRUGS
00307  01  WS-SPILLOVER.                                                ELTDRUGS
00308      05  FILLER                  PIC  X(10) VALUE                 ELTDRUGS
00309              'SPILLOVER '.                                        ELTDRUGS
00310                                                                   ELTDRUGS
00311  01  WS-OTHER-LITERALS.                                           ELTDRUGS
00312    05  WS-NO-TABULAR1.                                            ELTDRUGS
00313      10  FILLER                    PIC X(51)  VALUE               ELTDRUGS
00314         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTDRUGS
00315      10  FILLER                    PIC X(22)  VALUE               ELTDRUGS
00316         'GOING FROM BENEFIT ***'.                                 ELTDRUGS
00317                                                                   ELTDRUGS
00318    05  WS-NO-TABULAR2.                                            ELTDRUGS
00319      10  FILLER                    PIC X(15)  VALUE               ELTDRUGS
00320         '*** PROVISION: '.                                        ELTDRUGS
00321      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTDRUGS
00322      10  FILLER                    PIC X VALUE SPACE.             ELTDRUGS
00323      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTDRUGS
00324      10  FILLER                    PIC X(13)  VALUE               ELTDRUGS
00325         ' TO TABULAR: '.                                          ELTDRUGS
00326      10  WS-NO-TAB-ID              PIC X(6).                      ELTDRUGS
00327      10  FILLER                    PIC X VALUE SPACE.             ELTDRUGS
00328      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTDRUGS
00329      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTDRUGS
00330                                                                   ELTDRUGS
00331    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTDRUGS
00332    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTDRUGS
00333      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTDRUGS
00334                                                                   ELTDRUGS
00335      TITLE 'LINKAGE SECTION'.                                     ELTDRUGS
00336  LINKAGE SECTION.                                                 ELTDRUGS
00337  01  DFHCOMMAREA.                                                 ELTDRUGS
00338      COPY ELSCOMMC.                                               ELTDRUGS
00339 /***************************************************************  ELTDRUGS
00340 *  *** CIA  AREA ***                                              ELTDRUGS
00341 ****************************************************************  ELTDRUGS
00342      COPY ELSCIA2C.                                               ELTDRUGS
00343 /***************************************************************  ELTDRUGS
00344 /  *** IO PARM AREA ***                                           ELTDRUGS
00345 ****************************************************************  ELTDRUGS
00346      COPY ELSIOPMC.                                               ELTDRUGS
00347 /***************************************************************  ELTDRUGS
00348 *  *** KEY AREA     ***                                           ELTDRUGS
00349 ****************************************************************  ELTDRUGS
00350      COPY ELSKEYSC.                                               ELTDRUGS
00351 /***************************************************************  ELTDRUGS
00352 *  *** OUTPUT TEXT AREA ***                                       ELTDRUGS
00353 ****************************************************************  ELTDRUGS
00354      COPY ELSOUTPC.                                               ELTDRUGS
00355 /***************************************************************  ELTDRUGS
00356 *  *** TOPIC SELECTION AREA ***                                   ELTDRUGS
00357 ****************************************************************  ELTDRUGS
00358      COPY ELSSSCBC.                                               ELTDRUGS
00359 /***************************************************************  ELTDRUGS
00360 *  *** CODE MANUAL INTERFACE ***                                  ELTDRUGS
00361 ****************************************************************  ELTDRUGS
00362      COPY ELSCMIFC.                                               ELTDRUGS
00363 /***************************************************************  ELTDRUGS
00364 *  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDRUGS
00365 ****************************************************************  ELTDRUGS
00366      COPY ELSCMDSC.                                               ELTDRUGS
00367 /***************************************************************  ELTDRUGS
00368 *  *** BENEFIT PROVISION TABLE ***                                ELTDRUGS
00369 ****************************************************************  ELTDRUGS
00370      COPY ELSPRVNC.                                               ELTDRUGS
00371 /***************************************************************  ELTDRUGS
00372 *  *** COMPRESSION TEXT WORK-AREA ***                             ELTDRUGS
00373 ****************************************************************  ELTDRUGS
00374      COPY ELSTCWAC.                                               ELTDRUGS
00375 /***************************************************************  ELTDRUGS
00376 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDRUGS
00377 ****************************************************************  ELTDRUGS
00378      COPY ELSPLGSW.                                               ELTDRUGS
00379                                                                   ELTDRUGS
00380 /***************************************************************  ELTDRUGS
00381 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDRUGS
00382 ****************************************************************  ELTDRUGS
00383      COPY ELSPLGTB.                                               ELTDRUGS
00384                                                                   ELTDRUGS
00385 /***************************************************************  ELTDRUGS
00386 *    G R O U P  S P E C I F I C                                   ELTDRUGS
00387 ****************************************************************  ELTDRUGS
00388  01 GROUP-SPECIFIC-RECORD.                                        ELTDRUGS
00389      COPY GCGROUPC.                                               ELTDRUGS
00390                                                                   ELTDRUGS
00391      TITLE 'PROCEDURE DIVISION ELTDRUGS'.                         ELTDRUGS
00392  PROCEDURE DIVISION.                                              ELTDRUGS
00393  0000-MAINLINE.                                                   ELTDRUGS
00394                                                                   ELTDRUGS
00395      PERFORM 1000-INITIALIZATION                                  ELTDRUGS
00396         THRU 1000-EXIT.                                           ELTDRUGS
00397                                                                   ELTDRUGS
00398      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTDRUGS
00399                        AND                                        ELTDRUGS
00400         (SSB-SERV-CLASS-IP  OR  SSB-SERV-CLASS-BOTH)              ELTDRUGS
00401          PERFORM 2000-INSTITUTIONAL-IP                            ELTDRUGS
00402             THRU 2000-EXIT.                                       ELTDRUGS
00403                                                                   ELTDRUGS
00404      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTDRUGS
00405                        AND                                        ELTDRUGS
00406         (SSB-SERV-CLASS-OP  OR  SSB-SERV-CLASS-BOTH)              ELTDRUGS
00407          PERFORM 3000-INSTITUTIONAL-OP                            ELTDRUGS
00408             THRU 3000-EXIT.                                       ELTDRUGS
00409                                                                   ELTDRUGS
00410      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTDRUGS
00411                        AND                                        ELTDRUGS
00412         (SSB-SERV-CLASS-IP  OR  SSB-SERV-CLASS-BOTH)              ELTDRUGS
00413          PERFORM 4000-PROFESSIONAL-IP                             ELTDRUGS
00414             THRU 4000-EXIT.                                       ELTDRUGS
00415                                                                   ELTDRUGS
00416      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTDRUGS
00417                        AND                                        ELTDRUGS
00418         (SSB-SERV-CLASS-OP  OR  SSB-SERV-CLASS-BOTH)              ELTDRUGS
00419          PERFORM 5000-PROFESSIONAL-OP                             ELTDRUGS
00420             THRU 5000-EXIT.                                       ELTDRUGS
00421                                                                   ELTDRUGS
00422      IF NOT SSB-PROV-CLASS-INST    AND                            ELTDRUGS
00423         NOT SSB-PROV-CLASS-PROF    AND                            ELTDRUGS
00424         NOT SSB-PROV-CLASS-BOTH                                   ELTDRUGS
00425          MOVE ' '             TO  COF-FUNCTION                    ELTDRUGS
00426          MOVE +0              TO  COF-NBR-HDR-LINES               ELTDRUGS
00427          MOVE +2              TO  COF-NBR-DTL-LINES               ELTDRUGS
00428          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTDRUGS
00429          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTDRUGS
00430                           COMMAREA(DFHCOMMAREA)                   ELTDRUGS
00431                           END-EXEC.                               ELTDRUGS
00432                                                                   ELTDRUGS
00433      IF SSB-SERV-CLASS-IP  AND                                    ELTDRUGS
00434         SSB-SERV-CLASS-OP  AND                                    ELTDRUGS
00435         SSB-SERV-CLASS-BOTH                                       ELTDRUGS
00436          MOVE ' '             TO  COF-FUNCTION                    ELTDRUGS
00437          MOVE +0              TO  COF-NBR-HDR-LINES               ELTDRUGS
00438          MOVE +2              TO  COF-NBR-DTL-LINES               ELTDRUGS
00439          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTDRUGS
00440          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTDRUGS
00441                           COMMAREA(DFHCOMMAREA)                   ELTDRUGS
00442                           END-EXEC.                               ELTDRUGS
00443                                                                   ELTDRUGS
00444      MOVE 'E'   TO  COF-FUNCTION.                                 ELTDRUGS
00445      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTDRUGS
00446                                                                   ELTDRUGS
00447      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
00448                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00449                     END-EXEC.                                     ELTDRUGS
00450                                                                   ELTDRUGS
00451      EXEC CICS RETURN END-EXEC.                                   ELTDRUGS
00452                                                                   ELTDRUGS
00453      GOBACK.                                                      ELTDRUGS
00454                                                                   ELTDRUGS
00455      TITLE 'INITIALIZATION ELTDRUGS'.                             ELTDRUGS
00456  1000-INITIALIZATION.                                             ELTDRUGS
00457 ****************************************************************  ELTDRUGS
00458 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTDRUGS
00459 ****************************************************************  ELTDRUGS
00460                                                                   ELTDRUGS
00461      MOVE '1000'  TO  WS-PARA-ID1.                                ELTDRUGS
00462                                                                   ELTDRUGS
00463      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDRUGS
00464         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTDRUGS
00465         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDRUGS
00466                                                                   ELTDRUGS
00467      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDRUGS
00468          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTDRUGS
00469                                                                   ELTDRUGS
00470      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDRUGS
00471      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00472          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDRUGS
00473                                                                   ELTDRUGS
00474      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDRUGS
00475      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00476          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDRUGS
00477                                                                   ELTDRUGS
00478      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDRUGS
00479      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00480          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDRUGS
00481                                                                   ELTDRUGS
00482      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDRUGS
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00484          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDRUGS
00485                                                                   ELTDRUGS
00486      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDRUGS
00487      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00488          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDRUGS
00489                                                                   ELTDRUGS
00490      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTDRUGS
00491      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00492          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTDRUGS
00493                                                                   ELTDRUGS
00494      MOVE '0'  TO  WS-CHAR-0.                                     ELTDRUGS
00495                                                                   ELTDRUGS
00496      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDRUGS
00497                                                                   ELTDRUGS
00498      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDRUGS
00499              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDRUGS
00500                                                                   ELTDRUGS
00501      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTDRUGS
00502                                                                   ELTDRUGS
00503      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDRUGS
00504                                                                   ELTDRUGS
00505      EXEC CICS LINK                                               ELTDRUGS
00506                PROGRAM('ELUSTGMG')                                ELTDRUGS
00507                COMMAREA(DFHCOMMAREA)                              ELTDRUGS
00508      END-EXEC.                                                    ELTDRUGS
00509                                                                   ELTDRUGS
00510      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDRUGS
00511      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00512          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDRUGS
00513                                                                   ELTDRUGS
00514  1000-EXIT.  EXIT.                                                ELTDRUGS
00515                                                                   ELTDRUGS
00516      TITLE 'INISTUTIONAL INPATIENT   ELTDRUGS'.                   ELTDRUGS
00517 ****************************************************************  ELTDRUGS
00518 *   DRUGS/MEDICATIONS INSTITUTIONAL INPATIENT PROCESSING       *  ELTDRUGS
00519 ****************************************************************  ELTDRUGS
00520  2000-INSTITUTIONAL-IP.                                           ELTDRUGS
00521                                                                   ELTDRUGS
00522      MOVE '2000'  TO  WS-PARA-ID1.                                ELTDRUGS
00523      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTDRUGS
00524                                                                   ELTDRUGS
00525      MOVE HEADER-I-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTDRUGS
00526                                                                   ELTDRUGS
00527      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTDRUGS
00528         THRU 9100-EXIT.                                           ELTDRUGS
00529                                                                   ELTDRUGS
00530      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDRUGS
00531                                                                   ELTDRUGS
00532      PERFORM 2010-MOVE-IN-INST-IP-TABS                            ELTDRUGS
00533         THRU 2010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00534                        UNTIL   WS-SUB  >     WS-INST-IP-CNT.      ELTDRUGS
00535                                                                   ELTDRUGS
00536      PERFORM 2020-CALL-COVERAGE                                   ELTDRUGS
00537         THRU 2020-EXIT.                                           ELTDRUGS
00538                                                                   ELTDRUGS
00539      IF PVN-COVG-NONE                                             ELTDRUGS
00540          GO TO 2000-EXIT.                                         ELTDRUGS
00541                                                                   ELTDRUGS
00542      PERFORM 2030-FIND-FIRST-NONZERO                              ELTDRUGS
00543         THRU 2030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00544                        UNTIL   WS-SUB  > WS-INST-IP-CNT.          ELTDRUGS
00545                                                                   ELTDRUGS
00546  2000-EXIT.  EXIT.                                                ELTDRUGS
00547 /                                                                 ELTDRUGS
00548  2010-MOVE-IN-INST-IP-TABS.                                       ELTDRUGS
00549      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00550      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTDRUGS
00551                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDRUGS
00552                                                                   ELTDRUGS
00553      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDRUGS
00554                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDRUGS
00555                                                                   ELTDRUGS
00556  2010-EXIT.  EXIT.                                                ELTDRUGS
00557      SKIP3                                                        ELTDRUGS
00558  2020-CALL-COVERAGE.                                              ELTDRUGS
00559      MOVE '2020'  TO  WS-PARA-ID1.                                ELTDRUGS
00560                                                                   ELTDRUGS
00561      MOVE 'DRUGS/MEDICATIONS '  TO  SSB-TOPIC-PHRASE.             ELTDRUGS
00562                                                                   ELTDRUGS
00563      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDRUGS
00564                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00565                     END-EXEC.                                     ELTDRUGS
00566                                                                   ELTDRUGS
00567      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
00568                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00569                     END-EXEC.                                     ELTDRUGS
00570                                                                   ELTDRUGS
00571      IF PVN-COVG-NONE                                             ELTDRUGS
00572          GO TO 2020-EXIT.                                         ELTDRUGS
00573                                                                   ELTDRUGS
00574      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
00575                                                                   ELTDRUGS
00576      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDRUGS
00577      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00578          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDRUGS
00579                                                                   ELTDRUGS
00580      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTDRUGS
00581                                                                   ELTDRUGS
00582      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDRUGS
00583                    PSP-PROVN-PRICING-METHD,                       ELTDRUGS
00584                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDRUGS
00585                    PSP-CERTFN-REQRM-IND,                          ELTDRUGS
00586                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDRUGS
00587                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDRUGS
00588                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDRUGS
00589                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDRUGS
00590                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDRUGS
00591                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDRUGS
00592                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDRUGS
00593                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDRUGS
00594                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDRUGS
00595                    PSP-BEN-TAB-PROVN-ID-PPF.                      ELTDRUGS
00596                                                                   ELTDRUGS
00597      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTDRUGS
00598                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00599                     END-EXEC.                                     ELTDRUGS
00600                                                                   ELTDRUGS
00601      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDRUGS
00602      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00603          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDRUGS
00604                                                                   ELTDRUGS
00605  2020-EXIT.  EXIT.                                                ELTDRUGS
00606      SKIP3                                                        ELTDRUGS
00607  2030-FIND-FIRST-NONZERO.                                         ELTDRUGS
00608      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00609                                                                   ELTDRUGS
00610      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTDRUGS
00611          NEXT SENTENCE                                            ELTDRUGS
00612      ELSE                                                         ELTDRUGS
00613          PERFORM 2100-BUILD-SCREEN-LINES                          ELTDRUGS
00614             THRU 2100-EXIT.                                       ELTDRUGS
00615                                                                   ELTDRUGS
00616  2030-EXIT.  EXIT.                                                ELTDRUGS
00617 /                                                                 ELTDRUGS
00618  2100-BUILD-SCREEN-LINES.                                         ELTDRUGS
00619      MOVE '2100'  TO  WS-PARA-ID1.                                ELTDRUGS
00620      SET PLT-INDEX1  TO                                           ELTDRUGS
00621              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDRUGS
00622                                                                   ELTDRUGS
00623      IF WS-NOT-FIRST-TIME                                         ELTDRUGS
00624         MOVE 'P'    TO COF-FUNCTION                               ELTDRUGS
00625         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDRUGS
00626         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDRUGS
00627                          COMMAREA(DFHCOMMAREA)                    ELTDRUGS
00628         END-EXEC                                                  ELTDRUGS
00629      ELSE                                                         ELTDRUGS
00630        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDRUGS
00631                                                                   ELTDRUGS
00632      MOVE  +1  TO  WS-CIA.                                        ELTDRUGS
00633                                                                   ELTDRUGS
00634      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDRUGS
00635          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDRUGS
00636              SET PLT-INDEX2  TO  2                                ELTDRUGS
00637          ELSE                                                     ELTDRUGS
00638              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDRUGS
00639              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDRUGS
00640                 THRU 9200-EXIT                                    ELTDRUGS
00641              GO TO 2100-EXIT                                      ELTDRUGS
00642      ELSE                                                         ELTDRUGS
00643          SET PLT-INDEX2  TO  1.                                   ELTDRUGS
00644                                                                   ELTDRUGS
00645      MOVE WS-INST-IP-CNT TO WS-LIST-BP-CNT.                       ELTDRUGS
00646      PERFORM 6105-LIST-BEN-PROV                                   ELTDRUGS
00647         THRU 6105-EXIT.                                           ELTDRUGS
00648                                                                   ELTDRUGS
00649      PERFORM 6110-PLACE-OF-TREATMENT                              ELTDRUGS
00650         THRU 6110-EXIT.                                           ELTDRUGS
00651                                                                   ELTDRUGS
00652      PERFORM 6115-CERT-REQ-IND                                    ELTDRUGS
00653         THRU 6115-EXIT.                                           ELTDRUGS
00654                                                                   ELTDRUGS
00655      PERFORM 6120-PRIC-METH                                       ELTDRUGS
00656         THRU 6120-EXIT.                                           ELTDRUGS
00657                                                                   ELTDRUGS
00658      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTDRUGS
00659         THRU 6160-EXIT.                                           ELTDRUGS
00660                                                                   ELTDRUGS
00661      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTDRUGS
00662         THRU 6165-EXIT.                                           ELTDRUGS
00663                                                                   ELTDRUGS
00664      PERFORM 6170-AAR-PPF-PVE-TABS                                ELTDRUGS
00665         THRU 6170-EXIT.                                           ELTDRUGS
00666                                                                   ELTDRUGS
00667      PERFORM 6180-ALL-LEVEL-TABS                                  ELTDRUGS
00668         THRU 6180-EXIT.                                           ELTDRUGS
00669                                                                   ELTDRUGS
00670      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTDRUGS
00671  2100-EXIT.  EXIT.                                                ELTDRUGS
00672                                                                   ELTDRUGS
00673      TITLE 'INSTITUTIONAL OUTPATIENT  - ELTDRUGS'.                ELTDRUGS
00674  3000-INSTITUTIONAL-OP.                                           ELTDRUGS
00675 ****************************************************************  ELTDRUGS
00676 *   DRUGS/MEDICATIONS INSTITUTIONAL OUTPATIENT PROCESSING      *  ELTDRUGS
00677 ****************************************************************  ELTDRUGS
00678                                                                   ELTDRUGS
00679      MOVE '3000'  TO  WS-PARA-ID1.                                ELTDRUGS
00680      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTDRUGS
00681                                                                   ELTDRUGS
00682      MOVE HEADER-I-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTDRUGS
00683                                                                   ELTDRUGS
00684      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTDRUGS
00685         THRU 9100-EXIT.                                           ELTDRUGS
00686                                                                   ELTDRUGS
00687      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDRUGS
00688                                                                   ELTDRUGS
00689      PERFORM 3010-MOVE-IN-INST-OP-TABS                            ELTDRUGS
00690         THRU 3010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00691                        UNTIL   WS-SUB  >     WS-INST-OP-CNT.      ELTDRUGS
00692                                                                   ELTDRUGS
00693      PERFORM 3020-CALL-COVERAGE                                   ELTDRUGS
00694         THRU 3020-EXIT.                                           ELTDRUGS
00695                                                                   ELTDRUGS
00696      IF PVN-COVG-NONE                                             ELTDRUGS
00697          GO TO 3000-EXIT.                                         ELTDRUGS
00698                                                                   ELTDRUGS
00699      PERFORM 3030-FIND-FIRST-NONZERO                              ELTDRUGS
00700         THRU 3030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00701                        UNTIL   WS-SUB  > WS-INST-OP-CNT.          ELTDRUGS
00702                                                                   ELTDRUGS
00703  3000-EXIT.  EXIT.                                                ELTDRUGS
00704 /                                                                 ELTDRUGS
00705  3010-MOVE-IN-INST-OP-TABS.                                       ELTDRUGS
00706      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00707      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTDRUGS
00708                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDRUGS
00709                                                                   ELTDRUGS
00710      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDRUGS
00711                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDRUGS
00712                                                                   ELTDRUGS
00713  3010-EXIT.  EXIT.                                                ELTDRUGS
00714      SKIP3                                                        ELTDRUGS
00715  3020-CALL-COVERAGE.                                              ELTDRUGS
00716      MOVE '3020'  TO  WS-PARA-ID1.                                ELTDRUGS
00717                                                                   ELTDRUGS
00718      MOVE 'DRUGS/MEDICATIONS '  TO  SSB-TOPIC-PHRASE.             ELTDRUGS
00719                                                                   ELTDRUGS
00720      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDRUGS
00721                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00722                     END-EXEC.                                     ELTDRUGS
00723                                                                   ELTDRUGS
00724      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
00725                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00726                     END-EXEC.                                     ELTDRUGS
00727                                                                   ELTDRUGS
00728      IF PVN-COVG-NONE                                             ELTDRUGS
00729          GO TO 3020-EXIT.                                         ELTDRUGS
00730                                                                   ELTDRUGS
00731      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
00732                                                                   ELTDRUGS
00733      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDRUGS
00734      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00735          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDRUGS
00736                                                                   ELTDRUGS
00737      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDRUGS
00738                                                                   ELTDRUGS
00739      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDRUGS
00740                    PSP-PROVN-PRICING-METHD,                       ELTDRUGS
00741                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDRUGS
00742                    PSP-CERTFN-REQRM-IND,                          ELTDRUGS
00743                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDRUGS
00744                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDRUGS
00745                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDRUGS
00746                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDRUGS
00747                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDRUGS
00748                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDRUGS
00749                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDRUGS
00750                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDRUGS
00751                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDRUGS
00752                    PSP-BEN-TAB-PROVN-ID-PPF.                      ELTDRUGS
00753                                                                   ELTDRUGS
00754      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTDRUGS
00755                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00756                     END-EXEC.                                     ELTDRUGS
00757                                                                   ELTDRUGS
00758      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDRUGS
00759      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00760          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDRUGS
00761                                                                   ELTDRUGS
00762  3020-EXIT.  EXIT.                                                ELTDRUGS
00763      SKIP3                                                        ELTDRUGS
00764  3030-FIND-FIRST-NONZERO.                                         ELTDRUGS
00765      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00766                                                                   ELTDRUGS
00767      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTDRUGS
00768          NEXT SENTENCE                                            ELTDRUGS
00769      ELSE                                                         ELTDRUGS
00770          PERFORM 3100-BUILD-SCREEN-LINES                          ELTDRUGS
00771             THRU 3100-EXIT.                                       ELTDRUGS
00772                                                                   ELTDRUGS
00773  3030-EXIT.  EXIT.                                                ELTDRUGS
00774 /                                                                 ELTDRUGS
00775  3100-BUILD-SCREEN-LINES.                                         ELTDRUGS
00776      MOVE '3100'  TO  WS-PARA-ID1.                                ELTDRUGS
00777      SET PLT-INDEX1  TO                                           ELTDRUGS
00778              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDRUGS
00779                                                                   ELTDRUGS
00780      IF WS-NOT-FIRST-TIME                                         ELTDRUGS
00781         MOVE 'P'    TO COF-FUNCTION                               ELTDRUGS
00782         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDRUGS
00783         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDRUGS
00784                          COMMAREA(DFHCOMMAREA)                    ELTDRUGS
00785         END-EXEC                                                  ELTDRUGS
00786      ELSE                                                         ELTDRUGS
00787        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDRUGS
00788                                                                   ELTDRUGS
00789      MOVE  +1  TO  WS-CIA.                                        ELTDRUGS
00790                                                                   ELTDRUGS
00791      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDRUGS
00792          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDRUGS
00793              SET PLT-INDEX2  TO  2                                ELTDRUGS
00794          ELSE                                                     ELTDRUGS
00795              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDRUGS
00796              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDRUGS
00797                 THRU 9200-EXIT                                    ELTDRUGS
00798              GO TO 3100-EXIT                                      ELTDRUGS
00799      ELSE                                                         ELTDRUGS
00800          SET PLT-INDEX2  TO  1.                                   ELTDRUGS
00801                                                                   ELTDRUGS
00802      MOVE WS-INST-OP-CNT TO WS-LIST-BP-CNT.                       ELTDRUGS
00803      PERFORM 6105-LIST-BEN-PROV                                   ELTDRUGS
00804         THRU 6105-EXIT.                                           ELTDRUGS
00805                                                                   ELTDRUGS
00806      PERFORM 6110-PLACE-OF-TREATMENT                              ELTDRUGS
00807         THRU 6110-EXIT.                                           ELTDRUGS
00808                                                                   ELTDRUGS
00809      PERFORM 6115-CERT-REQ-IND                                    ELTDRUGS
00810         THRU 6115-EXIT.                                           ELTDRUGS
00811                                                                   ELTDRUGS
00812      PERFORM 6120-PRIC-METH                                       ELTDRUGS
00813         THRU 6120-EXIT.                                           ELTDRUGS
00814                                                                   ELTDRUGS
00815      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTDRUGS
00816         THRU 6160-EXIT.                                           ELTDRUGS
00817                                                                   ELTDRUGS
00818      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTDRUGS
00819         THRU 6165-EXIT.                                           ELTDRUGS
00820                                                                   ELTDRUGS
00821      PERFORM 6170-AAR-PPF-PVE-TABS                                ELTDRUGS
00822         THRU 6170-EXIT.                                           ELTDRUGS
00823                                                                   ELTDRUGS
00824      PERFORM 6180-ALL-LEVEL-TABS                                  ELTDRUGS
00825         THRU 6180-EXIT.                                           ELTDRUGS
00826                                                                   ELTDRUGS
00827      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTDRUGS
00828  3100-EXIT.  EXIT.                                                ELTDRUGS
00829                                                                   ELTDRUGS
00830                                                                   ELTDRUGS
00831      TITLE 'PROFESSIONAL INPATIENT  - ELTDRUGS'.                  ELTDRUGS
00832  4000-PROFESSIONAL-IP.                                            ELTDRUGS
00833 ****************************************************************  ELTDRUGS
00834 *   DRUGS/MEDICATIONS PROFESSIONAL INPATIENT PROCESSING        *  ELTDRUGS
00835 ****************************************************************  ELTDRUGS
00836                                                                   ELTDRUGS
00837      MOVE '4000'  TO  WS-PARA-ID1.                                ELTDRUGS
00838      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTDRUGS
00839                                                                   ELTDRUGS
00840      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTDRUGS
00841                                                                   ELTDRUGS
00842      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTDRUGS
00843         THRU 9100-EXIT.                                           ELTDRUGS
00844                                                                   ELTDRUGS
00845      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDRUGS
00846                                                                   ELTDRUGS
00847      PERFORM 4010-MOVE-IN-PROF-IP-TABS                            ELTDRUGS
00848         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00849                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTDRUGS
00850                                                                   ELTDRUGS
00851      PERFORM 4020-CALL-COVERAGE                                   ELTDRUGS
00852         THRU 4020-EXIT.                                           ELTDRUGS
00853                                                                   ELTDRUGS
00854      IF PVN-COVG-NONE                                             ELTDRUGS
00855          GO TO 4000-EXIT.                                         ELTDRUGS
00856                                                                   ELTDRUGS
00857      PERFORM 4030-FIND-FIRST-NONZERO                              ELTDRUGS
00858         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
00859                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTDRUGS
00860                                                                   ELTDRUGS
00861  4000-EXIT.  EXIT.                                                ELTDRUGS
00862 /                                                                 ELTDRUGS
00863  4010-MOVE-IN-PROF-IP-TABS.                                       ELTDRUGS
00864      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00865      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTDRUGS
00866                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDRUGS
00867                                                                   ELTDRUGS
00868      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDRUGS
00869                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDRUGS
00870                                                                   ELTDRUGS
00871  4010-EXIT.  EXIT.                                                ELTDRUGS
00872      SKIP3                                                        ELTDRUGS
00873  4020-CALL-COVERAGE.                                              ELTDRUGS
00874      MOVE '4020'  TO  WS-PARA-ID1.                                ELTDRUGS
00875                                                                   ELTDRUGS
00876      MOVE 'DRUGS/MEDICATIONS '  TO  SSB-TOPIC-PHRASE.             ELTDRUGS
00877                                                                   ELTDRUGS
00878      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDRUGS
00879                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00880                     END-EXEC.                                     ELTDRUGS
00881                                                                   ELTDRUGS
00882      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
00883                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00884                     END-EXEC.                                     ELTDRUGS
00885                                                                   ELTDRUGS
00886      IF PVN-COVG-NONE                                             ELTDRUGS
00887          GO TO 4020-EXIT.                                         ELTDRUGS
00888                                                                   ELTDRUGS
00889      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
00890                                                                   ELTDRUGS
00891      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDRUGS
00892      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00893          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDRUGS
00894                                                                   ELTDRUGS
00895      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDRUGS
00896                                                                   ELTDRUGS
00897      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDRUGS
00898                    PSP-PROVN-PRICING-METHD,                       ELTDRUGS
00899                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDRUGS
00900                    PSP-CERTFN-REQRM-IND,                          ELTDRUGS
00901                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDRUGS
00902                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDRUGS
00903                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDRUGS
00904                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDRUGS
00905                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDRUGS
00906                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDRUGS
00907                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDRUGS
00908                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDRUGS
00909                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDRUGS
00910                    PSP-BEN-TAB-PROVN-ID-PPF.                      ELTDRUGS
00911                                                                   ELTDRUGS
00912      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTDRUGS
00913                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
00914                     END-EXEC.                                     ELTDRUGS
00915                                                                   ELTDRUGS
00916      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDRUGS
00917      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
00918          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDRUGS
00919                                                                   ELTDRUGS
00920  4020-EXIT.  EXIT.                                                ELTDRUGS
00921      SKIP3                                                        ELTDRUGS
00922  4030-FIND-FIRST-NONZERO.                                         ELTDRUGS
00923      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
00924                                                                   ELTDRUGS
00925      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTDRUGS
00926          NEXT SENTENCE                                            ELTDRUGS
00927      ELSE                                                         ELTDRUGS
00928          PERFORM 4100-BUILD-SCREEN-LINES                          ELTDRUGS
00929             THRU 4100-EXIT.                                       ELTDRUGS
00930                                                                   ELTDRUGS
00931  4030-EXIT.  EXIT.                                                ELTDRUGS
00932 /                                                                 ELTDRUGS
00933  4100-BUILD-SCREEN-LINES.                                         ELTDRUGS
00934      MOVE '4100'  TO  WS-PARA-ID1.                                ELTDRUGS
00935      SET PLT-INDEX1  TO                                           ELTDRUGS
00936              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDRUGS
00937                                                                   ELTDRUGS
00938      IF WS-NOT-FIRST-TIME                                         ELTDRUGS
00939         MOVE 'P'    TO COF-FUNCTION                               ELTDRUGS
00940         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDRUGS
00941         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDRUGS
00942                          COMMAREA(DFHCOMMAREA)                    ELTDRUGS
00943         END-EXEC                                                  ELTDRUGS
00944      ELSE                                                         ELTDRUGS
00945        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDRUGS
00946                                                                   ELTDRUGS
00947      MOVE  +1  TO  WS-CIA.                                        ELTDRUGS
00948                                                                   ELTDRUGS
00949      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDRUGS
00950          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDRUGS
00951              SET PLT-INDEX2  TO  2                                ELTDRUGS
00952          ELSE                                                     ELTDRUGS
00953              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDRUGS
00954              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDRUGS
00955                 THRU 9200-EXIT                                    ELTDRUGS
00956              GO TO 4100-EXIT                                      ELTDRUGS
00957      ELSE                                                         ELTDRUGS
00958          SET PLT-INDEX2  TO  1.                                   ELTDRUGS
00959                                                                   ELTDRUGS
00960      MOVE WS-PROF-IP-CNT TO WS-LIST-BP-CNT.                       ELTDRUGS
00961      PERFORM 6105-LIST-BEN-PROV                                   ELTDRUGS
00962         THRU 6105-EXIT.                                           ELTDRUGS
00963                                                                   ELTDRUGS
00964      PERFORM 6110-PLACE-OF-TREATMENT                              ELTDRUGS
00965         THRU 6110-EXIT.                                           ELTDRUGS
00966                                                                   ELTDRUGS
00967      PERFORM 6115-CERT-REQ-IND                                    ELTDRUGS
00968         THRU 6115-EXIT.                                           ELTDRUGS
00969                                                                   ELTDRUGS
00970      PERFORM 6120-PRIC-METH                                       ELTDRUGS
00971         THRU 6120-EXIT.                                           ELTDRUGS
00972                                                                   ELTDRUGS
00973      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTDRUGS
00974         THRU 6160-EXIT.                                           ELTDRUGS
00975                                                                   ELTDRUGS
00976      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTDRUGS
00977         THRU 6165-EXIT.                                           ELTDRUGS
00978                                                                   ELTDRUGS
00979      PERFORM 6170-AAR-PPF-PVE-TABS                                ELTDRUGS
00980         THRU 6170-EXIT.                                           ELTDRUGS
00981                                                                   ELTDRUGS
00982      PERFORM 6180-ALL-LEVEL-TABS                                  ELTDRUGS
00983         THRU 6180-EXIT.                                           ELTDRUGS
00984                                                                   ELTDRUGS
00985      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTDRUGS
00986  4100-EXIT.  EXIT.                                                ELTDRUGS
00987                                                                   ELTDRUGS
00988      TITLE 'PROFESSIONAL OUTPATIENT  - ELTDRUGS'.                 ELTDRUGS
00989  5000-PROFESSIONAL-OP.                                            ELTDRUGS
00990 ****************************************************************  ELTDRUGS
00991 *   DRUGS/MEDICATIONS PROFESSIONAL OUTPATIENT PROCESSING       *  ELTDRUGS
00992 ****************************************************************  ELTDRUGS
00993                                                                   ELTDRUGS
00994      MOVE '5000'  TO  WS-PARA-ID1.                                ELTDRUGS
00995      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTDRUGS
00996                                                                   ELTDRUGS
00997      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTDRUGS
00998                                                                   ELTDRUGS
00999      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTDRUGS
01000         THRU 9100-EXIT.                                           ELTDRUGS
01001                                                                   ELTDRUGS
01002      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDRUGS
01003                                                                   ELTDRUGS
01004      PERFORM 5010-MOVE-IN-PROF-OP-TABS                            ELTDRUGS
01005         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
01006                        UNTIL   WS-SUB  >     WS-PROF-OP-CNT.      ELTDRUGS
01007                                                                   ELTDRUGS
01008      PERFORM 5020-CALL-COVERAGE                                   ELTDRUGS
01009         THRU 5020-EXIT.                                           ELTDRUGS
01010                                                                   ELTDRUGS
01011      IF PVN-COVG-NONE                                             ELTDRUGS
01012          GO TO 5000-EXIT                                          ELTDRUGS
01013      ELSE                                                         ELTDRUGS
01014         PERFORM 8000-CHECK-FOR-BLUE-SCRIPT                        ELTDRUGS
01015      END-IF.                                                      ELTDRUGS
01016                                                                   ELTDRUGS
01017      PERFORM 5030-FIND-FIRST-NONZERO                              ELTDRUGS
01018         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTDRUGS
01019                        UNTIL   WS-SUB  > WS-PROF-OP-CNT.          ELTDRUGS
01020                                                                   ELTDRUGS
01021  5000-EXIT.  EXIT.                                                ELTDRUGS
01022 /                                                                 ELTDRUGS
01023  5010-MOVE-IN-PROF-OP-TABS.                                       ELTDRUGS
01024      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
01025      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTDRUGS
01026                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDRUGS
01027                                                                   ELTDRUGS
01028      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDRUGS
01029                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDRUGS
01030                                                                   ELTDRUGS
01031  5010-EXIT.  EXIT.                                                ELTDRUGS
01032      SKIP3                                                        ELTDRUGS
01033  5020-CALL-COVERAGE.                                              ELTDRUGS
01034      MOVE '5020'  TO  WS-PARA-ID1.                                ELTDRUGS
01035                                                                   ELTDRUGS
01036      MOVE 'DRUGS/MEDICATIONS '  TO  SSB-TOPIC-PHRASE.             ELTDRUGS
01037                                                                   ELTDRUGS
01038      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDRUGS
01039                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01040                     END-EXEC.                                     ELTDRUGS
01041                                                                   ELTDRUGS
01042      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
01043                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01044                     END-EXEC.                                     ELTDRUGS
01045                                                                   ELTDRUGS
01046      IF PVN-COVG-NONE                                             ELTDRUGS
01047          GO TO 5020-EXIT.                                         ELTDRUGS
01048                                                                   ELTDRUGS
01049      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
01050                                                                   ELTDRUGS
01051      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDRUGS
01052      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
01053          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDRUGS
01054                                                                   ELTDRUGS
01055      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDRUGS
01056                                                                   ELTDRUGS
01057      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDRUGS
01058                    PSP-PROVN-PRICING-METHD,                       ELTDRUGS
01059                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDRUGS
01060                    PSP-CERTFN-REQRM-IND,                          ELTDRUGS
01061                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDRUGS
01062                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDRUGS
01063                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDRUGS
01064                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDRUGS
01065                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDRUGS
01066                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDRUGS
01067                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDRUGS
01068                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDRUGS
01069                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDRUGS
01070                    PSP-BEN-TAB-PROVN-ID-PPF.                      ELTDRUGS
01071                                                                   ELTDRUGS
01072      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTDRUGS
01073                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01074                     END-EXEC.                                     ELTDRUGS
01075                                                                   ELTDRUGS
01076      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDRUGS
01077      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
01078          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDRUGS
01079                                                                   ELTDRUGS
01080  5020-EXIT.  EXIT.                                                ELTDRUGS
01081      SKIP3                                                        ELTDRUGS
01082  5030-FIND-FIRST-NONZERO.                                         ELTDRUGS
01083      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
01084                                                                   ELTDRUGS
01085      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  ZERO      ELTDRUGS
01086          NEXT SENTENCE                                            ELTDRUGS
01087      ELSE                                                         ELTDRUGS
01088          PERFORM 5100-BUILD-SCREEN-LINES                          ELTDRUGS
01089             THRU 5100-EXIT.                                       ELTDRUGS
01090                                                                   ELTDRUGS
01091  5030-EXIT.  EXIT.                                                ELTDRUGS
01092 /                                                                 ELTDRUGS
01093  5100-BUILD-SCREEN-LINES.                                         ELTDRUGS
01094      MOVE '5100'  TO  WS-PARA-ID1.                                ELTDRUGS
01095      SET PLT-INDEX1  TO                                           ELTDRUGS
01096              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDRUGS
01097                                                                   ELTDRUGS
01098      IF WS-NOT-FIRST-TIME                                         ELTDRUGS
01099         MOVE 'P'    TO COF-FUNCTION                               ELTDRUGS
01100         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDRUGS
01101         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDRUGS
01102                          COMMAREA(DFHCOMMAREA)                    ELTDRUGS
01103         END-EXEC                                                  ELTDRUGS
01104      ELSE                                                         ELTDRUGS
01105        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDRUGS
01106                                                                   ELTDRUGS
01107      MOVE  +1  TO  WS-CIA.                                        ELTDRUGS
01108                                                                   ELTDRUGS
01109      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDRUGS
01110          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDRUGS
01111              SET PLT-INDEX2  TO  2                                ELTDRUGS
01112          ELSE                                                     ELTDRUGS
01113              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDRUGS
01114              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDRUGS
01115                 THRU 9200-EXIT                                    ELTDRUGS
01116              GO TO 5100-EXIT                                      ELTDRUGS
01117      ELSE                                                         ELTDRUGS
01118          SET PLT-INDEX2  TO  1.                                   ELTDRUGS
01119                                                                   ELTDRUGS
01120      MOVE WS-PROF-OP-CNT TO WS-LIST-BP-CNT.                       ELTDRUGS
01121      PERFORM 6105-LIST-BEN-PROV                                   ELTDRUGS
01122         THRU 6105-EXIT.                                           ELTDRUGS
01123                                                                   ELTDRUGS
01124      PERFORM 6110-PLACE-OF-TREATMENT                              ELTDRUGS
01125         THRU 6110-EXIT.                                           ELTDRUGS
01126                                                                   ELTDRUGS
01127      PERFORM 6115-CERT-REQ-IND                                    ELTDRUGS
01128         THRU 6115-EXIT.                                           ELTDRUGS
01129                                                                   ELTDRUGS
01130      PERFORM 6120-PRIC-METH                                       ELTDRUGS
01131         THRU 6120-EXIT.                                           ELTDRUGS
01132                                                                   ELTDRUGS
01133      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTDRUGS
01134         THRU 6160-EXIT.                                           ELTDRUGS
01135                                                                   ELTDRUGS
01136      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTDRUGS
01137         THRU 6165-EXIT.                                           ELTDRUGS
01138                                                                   ELTDRUGS
01139      PERFORM 6170-AAR-PPF-PVE-TABS                                ELTDRUGS
01140         THRU 6170-EXIT.                                           ELTDRUGS
01141                                                                   ELTDRUGS
01142      PERFORM 6180-ALL-LEVEL-TABS                                  ELTDRUGS
01143         THRU 6180-EXIT.                                           ELTDRUGS
01144                                                                   ELTDRUGS
01145      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTDRUGS
01146  5100-EXIT.  EXIT.                                                ELTDRUGS
01147      TITLE 'LIST OF BENEFIT PROVNS - ELTDRUGS'.                   ELTDRUGS
01148  6105-LIST-BEN-PROV.                                              ELTDRUGS
01149 ****************************************************************  ELTDRUGS
01150 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTDRUGS
01151 ****************************************************************  ELTDRUGS
01152      MOVE '6105'            TO  WS-PARA-ID1.                      ELTDRUGS
01153                                                                   ELTDRUGS
01154      MOVE  +2               TO  WS-CIA.                           ELTDRUGS
01155      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTDRUGS
01156      MOVE ZERO              TO  WS-SUB2.                          ELTDRUGS
01157      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTDRUGS
01158                             TO  WS-SUB3.                          ELTDRUGS
01159                                                                   ELTDRUGS
01160      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTDRUGS
01161         THRU 6106-EXIT VARYING                                    ELTDRUGS
01162                 PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTDRUGS
01163         UNTIL   PVN-BEN-PROVN-IDX > WS-LIST-BP-CNT.               ELTDRUGS
01164                                                                   ELTDRUGS
01165      MOVE '6105'            TO  WS-PARA-ID1.                      ELTDRUGS
01166      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDRUGS
01167      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTDRUGS
01168                                                                   ELTDRUGS
01169      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
01170                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01171                     END-EXEC.                                     ELTDRUGS
01172                                                                   ELTDRUGS
01173  6105-EXIT.  EXIT.                                                ELTDRUGS
01174  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTDRUGS
01175                                                                   ELTDRUGS
01176      MOVE '6106'            TO  WS-PARA-ID1.                      ELTDRUGS
01177      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTDRUGS
01178          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTDRUGS
01179          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTDRUGS
01180          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTDRUGS
01181                            TO  CMF-CODE-VALUE                     ELTDRUGS
01182          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTDRUGS
01183          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTDRUGS
01184          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTDRUGS
01185             THRU 9500-EXIT                                        ELTDRUGS
01186          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTDRUGS
01187          ADD  +1    TO  WS-SUB2                                   ELTDRUGS
01188          IF WS-CIA  >  20  OR  =  20                              ELTDRUGS
01189              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTDRUGS
01190              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTDRUGS
01191                             COMMAREA (DFHCOMMAREA)                ELTDRUGS
01192                             END-EXEC                              ELTDRUGS
01193              MOVE  +1  TO  WS-CIA.                                ELTDRUGS
01194                                                                   ELTDRUGS
01195  6106-EXIT.  EXIT.                                                ELTDRUGS
01196                                                                   ELTDRUGS
01197      TITLE 'PLACE OF TREATMENT IND - ELTDRUGS'.                   ELTDRUGS
01198  6110-PLACE-OF-TREATMENT.                                         ELTDRUGS
01199 ****************************************************************  ELTDRUGS
01200 *              P L A C E   O F   T R E A T M E N T             *  ELTDRUGS
01201 ****************************************************************  ELTDRUGS
01202      MOVE '6116'            TO  WS-PARA-ID1.                      ELTDRUGS
01203      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01204      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01205         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDRUGS
01206               NOT =  ZERO                                         ELTDRUGS
01207          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTDRUGS
01208          MOVE +2                    TO  WS-CIA                    ELTDRUGS
01209          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTDRUGS
01210                                                                   ELTDRUGS
01211      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01212      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01213         AND   PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01214               NOT  =  ZERO                                        ELTDRUGS
01215         AND   NOT  WS-ADD-A-BLANK-LINE                            ELTDRUGS
01216          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTDRUGS
01217          MOVE +2                    TO  WS-CIA                    ELTDRUGS
01218          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTDRUGS
01219                                                                   ELTDRUGS
01220      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01221      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01222         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDRUGS
01223               NOT  =  ZERO                                        ELTDRUGS
01224          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDRUGS
01225          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTDRUGS
01226                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDRUGS
01227          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01228                TO  CMF-CODE-VALUE                                 ELTDRUGS
01229          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTDRUGS
01230          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTDRUGS
01231          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTDRUGS
01232             THRU 9500-EXIT.                                       ELTDRUGS
01233                                                                   ELTDRUGS
01234      SET PLT-INDEX2  TO  2.                                       ELTDRUGS
01235      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01236         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDRUGS
01237              NOT  =  ZERO                                         ELTDRUGS
01238          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDRUGS
01239          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTDRUGS
01240                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDRUGS
01241          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01242               TO  CMF-CODE-VALUE                                  ELTDRUGS
01243          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTDRUGS
01244          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTDRUGS
01245          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTDRUGS
01246             THRU 9500-EXIT.                                       ELTDRUGS
01247                                                                   ELTDRUGS
01248      IF WS-ADD-A-BLANK-LINE                                       ELTDRUGS
01249         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDRUGS
01250          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTDRUGS
01251             THRU 9200-EXIT.                                       ELTDRUGS
01252                                                                   ELTDRUGS
01253      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDRUGS
01254         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01255      ELSE                                                         ELTDRUGS
01256         SET PLT-INDEX2  TO  1.                                    ELTDRUGS
01257                                                                   ELTDRUGS
01258  6110-EXIT.  EXIT.                                                ELTDRUGS
01259                                                                   ELTDRUGS
01260  6115-CERT-REQ-IND.                                               ELTDRUGS
01261 ****************************************************************  ELTDRUGS
01262 *       C E R T I F I C A T E  R  E Q U I R E M E N T          *  ELTDRUGS
01263 ****************************************************************  ELTDRUGS
01264      MOVE '6115'            TO  WS-PARA-ID1.                      ELTDRUGS
01265      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01266      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01267         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01268               NOT =  ZERO                                         ELTDRUGS
01269          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTDRUGS
01270          MOVE +2                    TO  WS-CIA                    ELTDRUGS
01271          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTDRUGS
01272                                                                   ELTDRUGS
01273      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01274      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01275         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01276               NOT  =  ZERO                                        ELTDRUGS
01277         AND   NOT  WS-ADD-A-BLANK-LINE                            ELTDRUGS
01278          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTDRUGS
01279          MOVE +2                    TO  WS-CIA                    ELTDRUGS
01280          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTDRUGS
01281                                                                   ELTDRUGS
01282      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01283      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01284         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01285               NOT  =  ZERO                                        ELTDRUGS
01286          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDRUGS
01287          MOVE 'CERTFN-REQRM-IND'                                  ELTDRUGS
01288                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDRUGS
01289          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01290                TO  CMF-CODE-VALUE                                 ELTDRUGS
01291          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTDRUGS
01292          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTDRUGS
01293          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTDRUGS
01294             THRU 9500-EXIT.                                       ELTDRUGS
01295                                                                   ELTDRUGS
01296      SET PLT-INDEX2  TO  2.                                       ELTDRUGS
01297      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01298         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01299              NOT  =  ZERO                                         ELTDRUGS
01300          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDRUGS
01301          MOVE 'CERTFN-REQRM-IND'                                  ELTDRUGS
01302                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDRUGS
01303          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01304               TO  CMF-CODE-VALUE                                  ELTDRUGS
01305          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTDRUGS
01306          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTDRUGS
01307          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTDRUGS
01308             THRU 9500-EXIT.                                       ELTDRUGS
01309                                                                   ELTDRUGS
01310      IF WS-ADD-A-BLANK-LINE                                       ELTDRUGS
01311         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDRUGS
01312          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTDRUGS
01313             THRU 9200-EXIT.                                       ELTDRUGS
01314                                                                   ELTDRUGS
01315      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDRUGS
01316         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01317      ELSE                                                         ELTDRUGS
01318         SET PLT-INDEX2  TO  1.                                    ELTDRUGS
01319                                                                   ELTDRUGS
01320  6115-EXIT.  EXIT.                                                ELTDRUGS
01321                                                                   ELTDRUGS
01322      TITLE 'PROVN PRICING METHOD - ELTDRUGS'.                     ELTDRUGS
01323  6120-PRIC-METH.                                                  ELTDRUGS
01324 ****************************************************************  ELTDRUGS
01325 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTDRUGS
01326 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTDRUGS
01327 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTDRUGS
01328 ****************************************************************  ELTDRUGS
01329      MOVE '6120'            TO  WS-PARA-ID1.                      ELTDRUGS
01330      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01331      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTDRUGS
01332         AND  PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDRUGS
01333              NOT = '19'                                           ELTDRUGS
01334         MOVE +2             TO  WS-CIA                            ELTDRUGS
01335         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTDRUGS
01336         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTDRUGS
01337                                                                   ELTDRUGS
01338      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01339      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO     ELTDRUGS
01340           AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01341               NOT = '19'                                          ELTDRUGS
01342           AND NOT WS-ADD-A-BLANK-LINE                             ELTDRUGS
01343         MOVE +2             TO  WS-CIA                            ELTDRUGS
01344         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTDRUGS
01345         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTDRUGS
01346                                                                   ELTDRUGS
01347      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01348      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01349         AND   PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01350               =  ZERO                                             ELTDRUGS
01351         AND   PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO    ELTDRUGS
01352         SET  PLT-INDEX2  TO  2                                    ELTDRUGS
01353         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01354            =  ZERO                                                ELTDRUGS
01355            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDRUGS
01356            ADD +1  TO  WS-CIA                                     ELTDRUGS
01357            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTDRUGS
01358                                                                   ELTDRUGS
01359      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01360      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01361         AND  PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDRUGS
01362              =  ZERO                                              ELTDRUGS
01363         AND   PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  =  ZERO        ELTDRUGS
01364         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTDRUGS
01365         ADD +1  TO  WS-CIA                                        ELTDRUGS
01366         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTDRUGS
01367                                                                   ELTDRUGS
01368      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)           =  ZERO     ELTDRUGS
01369         AND  PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO     ELTDRUGS
01370         SET  PLT-INDEX2  TO  2                                    ELTDRUGS
01371         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTDRUGS
01372            EQUAL ZERO                                             ELTDRUGS
01373            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDRUGS
01374            ADD +1  TO  WS-CIA                                     ELTDRUGS
01375            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTDRUGS
01376                                                                   ELTDRUGS
01377      SET  PLT-INDEX2  TO  1.                                      ELTDRUGS
01378      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01379         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01380            EQUAL ZERO                                             ELTDRUGS
01381            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01382               EQUAL ZERO                                          ELTDRUGS
01383               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTDRUGS
01384            ELSE                                                   ELTDRUGS
01385               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDRUGS
01386          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01387               TO  WS-PERCENTAGE                                   ELTDRUGS
01388         ELSE                                                      ELTDRUGS
01389            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTDRUGS
01390            MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1,          ELTDRUGS
01391                 PLT-INDEX2)   TO  WS-PERCENTAGE.                  ELTDRUGS
01392                                                                   ELTDRUGS
01393      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01394         AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01395             NOT = ZERO AND  NOT =  '19'                           ELTDRUGS
01396         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDRUGS
01397         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDRUGS
01398         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDRUGS
01399              TO  CMF-CODE-VALUE                                   ELTDRUGS
01400         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTDRUGS
01401         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDRUGS
01402         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTDRUGS
01403            THRU 9600-EXIT.                                        ELTDRUGS
01404                                                                   ELTDRUGS
01405      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01406      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01407         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01408            EQUAL ZERO                                             ELTDRUGS
01409            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01410               EQUAL ZERO                                          ELTDRUGS
01411               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTDRUGS
01412            ELSE                                                   ELTDRUGS
01413               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDRUGS
01414          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01415               TO  WS-PERCENTAGE                                   ELTDRUGS
01416         ELSE                                                      ELTDRUGS
01417          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTDRUGS
01418          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01419                TO  WS-PERCENTAGE.                                 ELTDRUGS
01420                                                                   ELTDRUGS
01421      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDRUGS
01422         AND   PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01423               NOT = ZERO AND  NOT =  '19'                         ELTDRUGS
01424         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDRUGS
01425         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDRUGS
01426         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDRUGS
01427              TO CMF-CODE-VALUE                                    ELTDRUGS
01428         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTDRUGS
01429         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDRUGS
01430         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTDRUGS
01431            THRU 9600-EXIT.                                        ELTDRUGS
01432                                                                   ELTDRUGS
01433      IF WS-ADD-A-BLANK-LINE                                       ELTDRUGS
01434          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTDRUGS
01435          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTDRUGS
01436             THRU 9200-EXIT.                                       ELTDRUGS
01437                                                                   ELTDRUGS
01438      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDRUGS
01439         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01440      ELSE                                                         ELTDRUGS
01441         SET PLT-INDEX2  TO  1.                                    ELTDRUGS
01442                                                                   ELTDRUGS
01443  6120-EXIT.  EXIT.                                                ELTDRUGS
01444                                                                   ELTDRUGS
01445      TITLE 'SPILLOVER COINSURANCE - ELTDRUGS'.                    ELTDRUGS
01446  6160-SPILLOVR-COINS-N-DEDUC.                                     ELTDRUGS
01447 ****************************************************************  ELTDRUGS
01448 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTDRUGS
01449 ****************************************************************  ELTDRUGS
01450      MOVE '6160'            TO  WS-PARA-ID1.                      ELTDRUGS
01451      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
01452                                                                   ELTDRUGS
01453      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01454      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)  NOT =  ZERO         ELTDRUGS
01455         AND  PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01456              NOT =  '0'                                           ELTDRUGS
01457         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDRUGS
01458         MOVE 'SPILL-OVER-COINS-APL-IND'                           ELTDRUGS
01459               TO CMF-ELEMENT-SYSTEM-NAME                          ELTDRUGS
01460         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTDRUGS
01461               TO  CMF-CODE-VALUE                                  ELTDRUGS
01462         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTDRUGS
01463         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTDRUGS
01464         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTDRUGS
01465            THRU 9500-EXIT                                         ELTDRUGS
01466         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTDRUGS
01467            THRU 9200-EXIT.                                        ELTDRUGS
01468 ****************************************************************  ELTDRUGS
01469 *          S P I L L O V E R   D E D U C T I B L E             *  ELTDRUGS
01470 ****************************************************************  ELTDRUGS
01471      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
01472                                                                   ELTDRUGS
01473      SET  PLT-INDEX2  TO  2.                                      ELTDRUGS
01474      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTDRUGS
01475         AND  PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)  ELTDRUGS
01476              NOT =  '0'                                           ELTDRUGS
01477         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDRUGS
01478         MOVE 'SPILL-OVER-DED-APL-IND'                             ELTDRUGS
01479               TO   CMF-ELEMENT-SYSTEM-NAME                        ELTDRUGS
01480         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01481               TO  CMF-CODE-VALUE                                  ELTDRUGS
01482         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTDRUGS
01483         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTDRUGS
01484         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTDRUGS
01485            THRU 9500-EXIT                                         ELTDRUGS
01486         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTDRUGS
01487            THRU 9200-EXIT.                                        ELTDRUGS
01488                                                                   ELTDRUGS
01489      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDRUGS
01490         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01491      ELSE                                                         ELTDRUGS
01492         SET PLT-INDEX2  TO  1.                                    ELTDRUGS
01493                                                                   ELTDRUGS
01494  6160-EXIT.  EXIT.                                                ELTDRUGS
01495                                                                   ELTDRUGS
01496                                                                   ELTDRUGS
01497      TITLE 'TRANSFER TO OTHR RESPON - ELTDRUGS'.                  ELTDRUGS
01498  6165-TRANS-OTHR-RESPON-IND.                                      ELTDRUGS
01499 ******************************************************************ELTDRUGS
01500 *   T R A N S F E R   T O   O T H E R  R E S P O N S I B I L I T YELTDRUGS
01501 *                     I N D I C A T O R                      9/89 ELTDRUGS
01502 ******************************************************************ELTDRUGS
01503      MOVE '6165'            TO  WS-PARA-ID1.                      ELTDRUGS
01504                                                                   ELTDRUGS
01505      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)          =  ZEROS    ELTDRUGS
01506         IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   NOT  = ZEROS    ELTDRUGS
01507              SET PLT-INDEX2  TO  2                                ELTDRUGS
01508          ELSE                                                     ELTDRUGS
01509              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDRUGS
01510              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDRUGS
01511                 THRU 9200-EXIT                                    ELTDRUGS
01512              GO TO 6165-EXIT                                      ELTDRUGS
01513      ELSE                                                         ELTDRUGS
01514          SET PLT-INDEX2  TO  1.                                   ELTDRUGS
01515                                                                   ELTDRUGS
01516      IF  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZEROELTDRUGS
01517          GO TO 6165-EXIT.                                         ELTDRUGS
01518                                                                   ELTDRUGS
01519      MOVE +1  TO  WS-CIA.                                         ELTDRUGS
01520      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDRUGS
01521                                                                   ELTDRUGS
01522      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTDRUGS
01523                                                                   ELTDRUGS
01524      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDRUGS
01525           TO  CMF-CODE-VALUE.                                     ELTDRUGS
01526                                                                   ELTDRUGS
01527      MOVE SPACES            TO  WS-TEMP-TEXT-AREA.                ELTDRUGS
01528      MOVE +0                TO  WS-TEMP-NOT-USED-CNT.             ELTDRUGS
01529                                                                   ELTDRUGS
01530      PERFORM 9500-CALL-CODES-MANUAL-LONG  THRU 9500-EXIT.         ELTDRUGS
01531                                                                   ELTDRUGS
01532      PERFORM 9200-TEXT-OUTPUT-REQUEST     THRU 9200-EXIT.         ELTDRUGS
01533                                                                   ELTDRUGS
01534  6165-EXIT.       EXIT.                                           ELTDRUGS
01535                                                                   ELTDRUGS
01536      TITLE 'AAR  PPF AND PVE TABULARS - ELTDRUGS'.                ELTDRUGS
01537  6170-AAR-PPF-PVE-TABS.                                           ELTDRUGS
01538 ****************************************************************  ELTDRUGS
01539 *                  A A R   T A B U L A R                       *  ELTDRUGS
01540 ****************************************************************  ELTDRUGS
01541      MOVE '6170'            TO  WS-PARA-ID1.                      ELTDRUGS
01542      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01543      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDRUGS
01544         AND  PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01545             NOT =  ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01546         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDRUGS
01547         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTDRUGS
01548         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTDRUGS
01549      ELSE                                                         ELTDRUGS
01550         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01551         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDRUGS
01552            AND PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)   ELTDRUGS
01553            NOT =   ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01554            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTDRUGS
01555            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTDRUGS
01556            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTDRUGS
01557                                                                   ELTDRUGS
01558      IF WS-ADD-A-BLANK-LINE                                       ELTDRUGS
01559         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDRUGS
01560         MOVE 1  TO  WS-CIA                                        ELTDRUGS
01561         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDRUGS
01562             COMMAREA(DFHCOMMAREA)                                 ELTDRUGS
01563         END-EXEC.                                                 ELTDRUGS
01564 *--------------------------------------------------------------*  ELTDRUGS
01565 *                  P P F   T A B U L A R                       *  ELTDRUGS
01566 *--------------------------------------------------------------*  ELTDRUGS
01567      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01568      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDRUGS
01569         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)          ELTDRUGS
01570             NOT =  ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01571         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01572              TO  KWA-GCTABULR-KEY                                 ELTDRUGS
01573         PERFORM 9900-GET-TABULAR-RECORD                           ELTDRUGS
01574            THRU 9900-EXIT                                         ELTDRUGS
01575         IF IOP-RC-OK                                              ELTDRUGS
01576            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTDRUGS
01577                 COMMAREA(DFHCOMMAREA)                             ELTDRUGS
01578            END-EXEC                                               ELTDRUGS
01579         ELSE                                                      ELTDRUGS
01580            NEXT SENTENCE                                          ELTDRUGS
01581      ELSE                                                         ELTDRUGS
01582         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01583         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTDRUGS
01584            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01585              NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01586          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)    ELTDRUGS
01587               TO  KWA-GCTABULR-KEY                                ELTDRUGS
01588            PERFORM 9900-GET-TABULAR-RECORD                        ELTDRUGS
01589               THRU 9900-EXIT                                      ELTDRUGS
01590            IF IOP-RC-OK                                           ELTDRUGS
01591               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTDRUGS
01592                    COMMAREA(DFHCOMMAREA)                          ELTDRUGS
01593               END-EXEC.                                           ELTDRUGS
01594 *--------------------------------------------------------------*  ELTDRUGS
01595 *                  P V E   T A B U L A R                       *  ELTDRUGS
01596 *--------------------------------------------------------------*  ELTDRUGS
01597                                                                   ELTDRUGS
01598      MOVE  +2     TO  WS-CIA.                                     ELTDRUGS
01599      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTDRUGS
01600      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTDRUGS
01601         THRU 9200-EXIT.                                           ELTDRUGS
01602                                                                   ELTDRUGS
01603  6170-EXIT.  EXIT.                                                ELTDRUGS
01604                                                                   ELTDRUGS
01605      TITLE 'ACCUM TABULARS - ELTDRUGS'.                           ELTDRUGS
01606  6180-ALL-LEVEL-TABS.                                             ELTDRUGS
01607 *--------------------------------------------------------------*  ELTDRUGS
01608 *                  A B M   T A B U L A R                       *  ELTDRUGS
01609 *--------------------------------------------------------------*  ELTDRUGS
01610      MOVE '6180'            TO  WS-PARA-ID1.                      ELTDRUGS
01611      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01612      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTDRUGS
01613                              AND                                  ELTDRUGS
01614         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)          ELTDRUGS
01615             NOT =  ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01616         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01617              TO  KWA-GCTABULR-KEY                                 ELTDRUGS
01618         PERFORM 9900-GET-TABULAR-RECORD                           ELTDRUGS
01619            THRU 9900-EXIT                                         ELTDRUGS
01620         IF IOP-RC-OK                                              ELTDRUGS
01621            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTDRUGS
01622                 COMMAREA(DFHCOMMAREA)                             ELTDRUGS
01623            END-EXEC                                               ELTDRUGS
01624         ELSE                                                      ELTDRUGS
01625            NEXT SENTENCE                                          ELTDRUGS
01626      ELSE                                                         ELTDRUGS
01627         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01628         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZERO        ELTDRUGS
01629            AND                                                    ELTDRUGS
01630            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01631              NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01632              MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01633              TO  KWA-GCTABULR-KEY                                 ELTDRUGS
01634            PERFORM 9900-GET-TABULAR-RECORD                        ELTDRUGS
01635               THRU 9900-EXIT                                      ELTDRUGS
01636            IF IOP-RC-OK                                           ELTDRUGS
01637               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTDRUGS
01638                    COMMAREA(DFHCOMMAREA)                          ELTDRUGS
01639               END-EXEC.                                           ELTDRUGS
01640 /--------------------------------------------------------------*  ELTDRUGS
01641 *                  A C L   T A B U L A R                       *  ELTDRUGS
01642 *--------------------------------------------------------------*  ELTDRUGS
01643      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01644      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01645         AND                                                       ELTDRUGS
01646         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)          ELTDRUGS
01647             NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES  ELTDRUGS
01648         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)     ELTDRUGS
01649             TO  KWA-GCTABULR-KEY                                  ELTDRUGS
01650         PERFORM 9900-GET-TABULAR-RECORD                           ELTDRUGS
01651            THRU 9900-EXIT                                         ELTDRUGS
01652         IF IOP-RC-OK                                              ELTDRUGS
01653            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTDRUGS
01654                 COMMAREA(DFHCOMMAREA)                             ELTDRUGS
01655            END-EXEC                                               ELTDRUGS
01656         ELSE                                                      ELTDRUGS
01657            NEXT SENTENCE                                          ELTDRUGS
01658      ELSE                                                         ELTDRUGS
01659         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01660         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO       ELTDRUGS
01661            AND                                                    ELTDRUGS
01662            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01663              NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01664              MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01665                   TO   KWA-GCTABULR-KEY                           ELTDRUGS
01666            PERFORM 9900-GET-TABULAR-RECORD                        ELTDRUGS
01667               THRU 9900-EXIT                                      ELTDRUGS
01668            IF IOP-RC-OK                                           ELTDRUGS
01669               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTDRUGS
01670                    COMMAREA(DFHCOMMAREA)                          ELTDRUGS
01671               END-EXEC.                                           ELTDRUGS
01672 /--------------------------------------------------------------*  ELTDRUGS
01673 *                  A D L   T A B U L A R                       *  ELTDRUGS
01674 *--------------------------------------------------------------*  ELTDRUGS
01675      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01676      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01677         AND                                                       ELTDRUGS
01678         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)          ELTDRUGS
01679            NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES   ELTDRUGS
01680            MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  ELTDRUGS
01681                 TO   KWA-GCTABULR-KEY                             ELTDRUGS
01682         PERFORM 9900-GET-TABULAR-RECORD                           ELTDRUGS
01683            THRU 9900-EXIT                                         ELTDRUGS
01684         IF IOP-RC-OK                                              ELTDRUGS
01685            EXEC  CICS  LINK  PROGRAM('ELFMADL')                   ELTDRUGS
01686                 COMMAREA(DFHCOMMAREA)                             ELTDRUGS
01687            END-EXEC                                               ELTDRUGS
01688         ELSE                                                      ELTDRUGS
01689            NEXT SENTENCE                                          ELTDRUGS
01690      ELSE                                                         ELTDRUGS
01691         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01692         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO       ELTDRUGS
01693            AND                                                    ELTDRUGS
01694            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01695              NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01696              MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01697                   TO   KWA-GCTABULR-KEY                           ELTDRUGS
01698            PERFORM 9900-GET-TABULAR-RECORD                        ELTDRUGS
01699               THRU 9900-EXIT                                      ELTDRUGS
01700            IF IOP-RC-OK                                           ELTDRUGS
01701               EXEC  CICS  LINK  PROGRAM('ELFMADL')                ELTDRUGS
01702                    COMMAREA(DFHCOMMAREA)                          ELTDRUGS
01703               END-EXEC.                                           ELTDRUGS
01704 /--------------------------------------------------------------*  ELTDRUGS
01705 *                  A O L   T A B U L A R                       *  ELTDRUGS
01706 *--------------------------------------------------------------*  ELTDRUGS
01707      SET PLT-INDEX2  TO  1.                                       ELTDRUGS
01708      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDRUGS
01709         AND                                                       ELTDRUGS
01710         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)          ELTDRUGS
01711            NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES   ELTDRUGS
01712            MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  ELTDRUGS
01713                 TO   KWA-GCTABULR-KEY                             ELTDRUGS
01714         PERFORM 9900-GET-TABULAR-RECORD                           ELTDRUGS
01715            THRU 9900-EXIT                                         ELTDRUGS
01716         IF IOP-RC-OK                                              ELTDRUGS
01717            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTDRUGS
01718                 COMMAREA(DFHCOMMAREA)                             ELTDRUGS
01719            END-EXEC                                               ELTDRUGS
01720         ELSE                                                      ELTDRUGS
01721            NEXT SENTENCE                                          ELTDRUGS
01722      ELSE                                                         ELTDRUGS
01723         SET PLT-INDEX2  TO  2                                     ELTDRUGS
01724         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO       ELTDRUGS
01725            AND                                                    ELTDRUGS
01726            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)       ELTDRUGS
01727              NOT = ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTDRUGS
01728              MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)ELTDRUGS
01729              TO KWA-GCTABULR-KEY                                  ELTDRUGS
01730            PERFORM 9900-GET-TABULAR-RECORD                        ELTDRUGS
01731               THRU 9900-EXIT                                      ELTDRUGS
01732            IF IOP-RC-OK                                           ELTDRUGS
01733               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTDRUGS
01734                    COMMAREA(DFHCOMMAREA)                          ELTDRUGS
01735               END-EXEC.                                           ELTDRUGS
01736  6180-EXIT.  EXIT.                                                ELTDRUGS
01737                                                                   ELTDRUGS
01738       TITLE 'DISPLAY COINS / OPX MESSAGE'.                        ELTDRUGS
01739  6200-PAY-CONSID-TEXT.                                            ELTDRUGS
01740      INITIALIZE TCAR-FROM-AREA.                                   ELTDRUGS
01741      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDRUGS
01742             WS-PAY-CONSDR-TEXT2                                   ELTDRUGS
01743                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDRUGS
01744      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDRUGS
01745      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDRUGS
01746      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDRUGS
01747                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDRUGS
01748      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDRUGS
01749      IF WS-CIA > 17                                               ELTDRUGS
01750            PERFORM 9200-TEXT-OUTPUT-REQUEST THRU 9200-EXIT        ELTDRUGS
01751            MOVE +1            TO WS-CIA.                          ELTDRUGS
01752      ADD +1                TO  WS-CIA.                            ELTDRUGS
01753      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDRUGS
01754      ADD +1                TO  WS-CIA.                            ELTDRUGS
01755      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDRUGS
01756      PERFORM 9200-TEXT-OUTPUT-REQUEST THRU 9200-EXIT.             ELTDRUGS
01757  6200-EXIT.   EXIT.                                               ELTDRUGS
01758                                                                   ELTDRUGS
01759 ****************************************************************  ELTDRUGS
01760 *          CHECK FOR BLUE SCRIPT                               *  ELTDRUGS
01761 ****************************************************************  ELTDRUGS
01762  8000-CHECK-FOR-BLUE-SCRIPT.                                      ELTDRUGS
01763      SET PVN-BEN-PROVN-IDX TO 1.                                  ELTDRUGS
01764      PERFORM VARYING WS-BS-SUB FROM 1 BY 1                        ELTDRUGS
01765         UNTIL WS-BS-SUB > WS-PROF-OP-CNT OR                       ELTDRUGS
01766           PVN-BEN-ID(PVN-BEN-PROVN-IDX) > 'DRGO'                  ELTDRUGS
01767           SET PVN-BEN-PROVN-IDX TO WS-BS-SUB                      ELTDRUGS
01768              IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'DRGO'            ELTDRUGS
01769                 IF GCG-ELEC-PRES-DRUG-PGM-IND > ZERO              ELTDRUGS
01770                    MOVE ' '             TO  COF-FUNCTION          ELTDRUGS
01771                    MOVE +0              TO  COF-NBR-HDR-LINES     ELTDRUGS
01772                    MOVE +0              TO  COF-NBR-DTL-LINES     ELTDRUGS
01773                    ADD +1               TO  COF-NBR-DTL-LINES     ELTDRUGS
01774                    MOVE SPACES TO  COF-DTL-LINE                   ELTDRUGS
01775                                (COF-NBR-DTL-LINES)                ELTDRUGS
01776                    ADD +1               TO  COF-NBR-DTL-LINES     ELTDRUGS
01777                    MOVE WS-BLUE-SCRIPT1 TO  COF-DTL-LINE          ELTDRUGS
01778                                (COF-NBR-DTL-LINES)                ELTDRUGS
01779                    ADD +1               TO  COF-NBR-DTL-LINES     ELTDRUGS
01780                    MOVE WS-BLUE-SCRIPT2 TO  COF-DTL-LINE          ELTDRUGS
01781                                (COF-NBR-DTL-LINES)                ELTDRUGS
01782                    EXEC CICS  LINK  PROGRAM('ELUOUTPT')           ELTDRUGS
01783                                     COMMAREA(DFHCOMMAREA)         ELTDRUGS
01784                                     END-EXEC                      ELTDRUGS
01785                 END-IF                                            ELTDRUGS
01786              END-IF                                               ELTDRUGS
01787      END-PERFORM.                                                 ELTDRUGS
01788                                                                   ELTDRUGS
01789                                                                   ELTDRUGS
01790 ****************************************************************  ELTDRUGS
01791 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTDRUGS
01792 ****************************************************************  ELTDRUGS
01793  9100-HEADER-OUTPUT-REQUEST.                                      ELTDRUGS
01794                                                                   ELTDRUGS
01795      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTDRUGS
01796      MOVE 'P'            TO  COF-FUNCTION.                        ELTDRUGS
01797                                                                   ELTDRUGS
01798      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
01799                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01800                     END-EXEC.                                     ELTDRUGS
01801                                                                   ELTDRUGS
01802  9100-EXIT.  EXIT.                                                ELTDRUGS
01803                                                                   ELTDRUGS
01804 ****************************************************************  ELTDRUGS
01805 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTDRUGS
01806 ****************************************************************  ELTDRUGS
01807  9200-TEXT-OUTPUT-REQUEST.                                        ELTDRUGS
01808                                                                   ELTDRUGS
01809      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTDRUGS
01810      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELTDRUGS
01811      MOVE ' '     TO  COF-FUNCTION.                               ELTDRUGS
01812                                                                   ELTDRUGS
01813      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTDRUGS
01814                     COMMAREA (DFHCOMMAREA)                        ELTDRUGS
01815                     END-EXEC.                                     ELTDRUGS
01816                                                                   ELTDRUGS
01817  9200-EXIT.  EXIT.                                                ELTDRUGS
01818                                                                   ELTDRUGS
01819      TITLE ' CODES MANUAL INTERFACE  -- ELTDRUGS'.                ELTDRUGS
01820  9500-CALL-CODES-MANUAL-LONG.                                     ELTDRUGS
01821 ****************************************************************  ELTDRUGS
01822 *                                                              *  ELTDRUGS
01823 * C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  *  ELTDRUGS
01824 *                                                              *  ELTDRUGS
01825 ****************************************************************  ELTDRUGS
01826      MOVE '9500'  TO  WS-PARA-ID2.                                ELTDRUGS
01827                                                                   ELTDRUGS
01828      INITIALIZE CMF-RETURN-CODE,                                  ELTDRUGS
01829                 TCAR-FROM-AREA.                                   ELTDRUGS
01830                                                                   ELTDRUGS
01831      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDRUGS
01832      END-EXEC.                                                    ELTDRUGS
01833                                                                   ELTDRUGS
01834      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDRUGS
01835      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
01836          ADDRESS OF CMF-DESCR.                                    ELTDRUGS
01837                                                                   ELTDRUGS
01838      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTDRUGS
01839         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTDRUGS
01840         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTDRUGS
01841            CMF-DESCR-LINE(1),        ' ',                         ELTDRUGS
01842            CMF-DESCR-LINE(2),        ' ',                         ELTDRUGS
01843            CMF-DESCR-LINE(3)                                      ELTDRUGS
01844            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTDRUGS
01845      ELSE                                                         ELTDRUGS
01846         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTDRUGS
01847         STRING CMF-DESCR-LINE(1),        ' ',                     ELTDRUGS
01848            CMF-DESCR-LINE(2),        ' ',                         ELTDRUGS
01849            CMF-DESCR-LINE(3)                                      ELTDRUGS
01850            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTDRUGS
01851                                                                   ELTDRUGS
01852      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDRUGS
01853                                                                   ELTDRUGS
01854      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTDRUGS
01855      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTDRUGS
01856      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTDRUGS
01857      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDRUGS
01858                                                                   ELTDRUGS
01859      IF WS-MOVE-LINES-TO-CIA                                      ELTDRUGS
01860         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTDRUGS
01861            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTDRUGS
01862                                             WS-TEMP-NOT-USED-CNT  ELTDRUGS
01863            MOVE '9550'  TO  WS-PARA-ID2                           ELTDRUGS
01864            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTDRUGS
01865               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTDRUGS
01866                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTDRUGS
01867            MOVE '9500'  TO  WS-PARA-ID2                           ELTDRUGS
01868            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTDRUGS
01869            ADD +1  TO  WS-CIA                                     ELTDRUGS
01870            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTDRUGS
01871         ELSE                                                      ELTDRUGS
01872            ADD +1  TO  WS-CIA                                     ELTDRUGS
01873            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTDRUGS
01874                                                                   ELTDRUGS
01875      IF WS-MOVE-LINES-TO-CIA                                      ELTDRUGS
01876         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTDRUGS
01877            ADD +1  TO  WS-CIA                                     ELTDRUGS
01878            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTDRUGS
01879         ELSE                                                      ELTDRUGS
01880            NEXT SENTENCE                                          ELTDRUGS
01881      ELSE                                                         ELTDRUGS
01882         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTDRUGS
01883                                                                   ELTDRUGS
01884  9500-EXIT.  EXIT.                                                ELTDRUGS
01885                                                                   ELTDRUGS
01886  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTDRUGS
01887      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTDRUGS
01888      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDRUGS
01889                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDRUGS
01890                                                                   ELTDRUGS
01891  9550-EXIT.  EXIT.                                                ELTDRUGS
01892                                                                   ELTDRUGS
01893  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTDRUGS
01894      MOVE '9600'  TO  WS-PARA-ID2.                                ELTDRUGS
01895                                                                   ELTDRUGS
01896      INITIALIZE CMF-RETURN-CODE,                                  ELTDRUGS
01897                 TCAR-FROM-AREA.                                   ELTDRUGS
01898                                                                   ELTDRUGS
01899      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDRUGS
01900      END-EXEC.                                                    ELTDRUGS
01901                                                                   ELTDRUGS
01902      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDRUGS
01903      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
01904          ADDRESS OF CMF-DESCR.                                    ELTDRUGS
01905                                                                   ELTDRUGS
01906      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTDRUGS
01907         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTDRUGS
01908         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTDRUGS
01909            CMF-DESCR-LINE(1),        ' ',                         ELTDRUGS
01910            CMF-DESCR-LINE(2),        ' ',                         ELTDRUGS
01911            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTDRUGS
01912            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTDRUGS
01913      ELSE                                                         ELTDRUGS
01914         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTDRUGS
01915         STRING CMF-DESCR-LINE(1),        ' ',                     ELTDRUGS
01916            CMF-DESCR-LINE(2),        ' ',                         ELTDRUGS
01917            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTDRUGS
01918            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTDRUGS
01919                                                                   ELTDRUGS
01920      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDRUGS
01921                                                                   ELTDRUGS
01922      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTDRUGS
01923      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTDRUGS
01924      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTDRUGS
01925                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTDRUGS
01926                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTDRUGS
01927      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDRUGS
01928                                                                   ELTDRUGS
01929      IF WS-MOVE-LINES-TO-CIA                                      ELTDRUGS
01930         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTDRUGS
01931            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTDRUGS
01932                                             WS-TEMP-NOT-USED-CNT  ELTDRUGS
01933            MOVE '9650'  TO  WS-PARA-ID2                           ELTDRUGS
01934            PERFORM 9650-CONCATENATE-TO-TEMP-TEXT                  ELTDRUGS
01935               THRU 9650-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTDRUGS
01936                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTDRUGS
01937            MOVE '9600'  TO  WS-PARA-ID2                           ELTDRUGS
01938            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTDRUGS
01939            ADD +1  TO  WS-CIA                                     ELTDRUGS
01940            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTDRUGS
01941         ELSE                                                      ELTDRUGS
01942            ADD +1  TO  WS-CIA                                     ELTDRUGS
01943            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTDRUGS
01944                                                                   ELTDRUGS
01945      IF WS-MOVE-LINES-TO-CIA                                      ELTDRUGS
01946         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTDRUGS
01947            MOVE '9650'  TO  WS-PARA-ID2                           ELTDRUGS
01948            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTDRUGS
01949               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTDRUGS
01950                              UNTIL   WS-SUB1 >                    ELTDRUGS
01951                              TCAR-OUTPUT-FIELDS-USED              ELTDRUGS
01952         ELSE                                                      ELTDRUGS
01953            NEXT SENTENCE                                          ELTDRUGS
01954      ELSE                                                         ELTDRUGS
01955         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTDRUGS
01956                                                                   ELTDRUGS
01957  9600-EXIT.  EXIT.                                                ELTDRUGS
01958                                                                   ELTDRUGS
01959  9650-CONCATENATE-TO-TEMP-TEXT.                                   ELTDRUGS
01960      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTDRUGS
01961      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDRUGS
01962                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDRUGS
01963                                                                   ELTDRUGS
01964  9650-EXIT.  EXIT.                                                ELTDRUGS
01965      SKIP3                                                        ELTDRUGS
01966  9660-MOVE-LINES-TO-CIA.                                          ELTDRUGS
01967      ADD +1  TO  WS-CIA.                                          ELTDRUGS
01968      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTDRUGS
01969                                                                   ELTDRUGS
01970  9660-EXIT.  EXIT.                                                ELTDRUGS
01971                                                                   ELTDRUGS
01972      TITLE ' TABULAR RECORD INTERFACE  -- ELTDRUGS'.              ELTDRUGS
01973  9900-GET-TABULAR-RECORD.                                         ELTDRUGS
01974 ***************************************************************** ELTDRUGS
01975 *            G E T   T A B U L A R   R E C O R D                  ELTDRUGS
01976 *                                                                 ELTDRUGS
01977 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTDRUGS
01978 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTDRUGS
01979 *  TO DISPLAY.                                                    ELTDRUGS
01980 *                                                                 ELTDRUGS
01981 ***************************************************************** ELTDRUGS
01982      MOVE '9900'  TO  WS-PARA-ID2.                                ELTDRUGS
01983                                                                   ELTDRUGS
01984      SET CIA-GCTABULR-DDN TO TRUE.                                ELTDRUGS
01985      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDRUGS
01986          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTDRUGS
01987                                                                   ELTDRUGS
01988      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTDRUGS
01989      SET  CIA-GCTABULR-DDN          TO TRUE.                      ELTDRUGS
01990                                                                   ELTDRUGS
01991      SET IOP-RD                     TO TRUE.                      ELTDRUGS
01992      SET IOP-FCQ-NONE               TO TRUE.                      ELTDRUGS
01993      SET IOP-KVQ-NONE               TO TRUE.                      ELTDRUGS
01994                                                                   ELTDRUGS
01995      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTDRUGS
01996             COMMAREA(DFHCOMMAREA)                                 ELTDRUGS
01997      END-EXEC.                                                    ELTDRUGS
01998                                                                   ELTDRUGS
01999      IF IOP-RC-NOTFND                                             ELTDRUGS
02000         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTDRUGS
02001         EXEC CICS ABEND                                           ELTDRUGS
02002                   ABCODE(CIA-ABCODE)                              ELTDRUGS
02003         END-EXEC.                                                 ELTDRUGS
02004                                                                   ELTDRUGS
02005      IF NOT IOP-RC-OK                                             ELTDRUGS
02006         SET CIA-AB-CRITIO          TO TRUE                        ELTDRUGS
02007         EXEC CICS ABEND                                           ELTDRUGS
02008                   ABCODE(CIA-ABCODE)                              ELTDRUGS
02009         END-EXEC.                                                 ELTDRUGS
02010                                                                   ELTDRUGS
02011  9900-EXIT.  EXIT.                                                ELTDRUGS
02012                                                                   ELTDRUGS
02013      TITLE ' TEXT COMPRESSION AND EXPANSION'.                     ELTDRUGS
02014      COPY ELSTCOMP.                                               ELTDRUGS
02015                                                                   ELTDRUGS
02016      TITLE ' ELTDRUGS '.                                          ELTDRUGS
