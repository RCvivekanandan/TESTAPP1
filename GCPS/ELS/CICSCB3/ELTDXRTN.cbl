00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELTDXRTN.                                         ELTDXRTN
00003  AUTHOR.        ANNE KING.                                           LV001
00004  DATE-WRITTEN.  10/05/95.                                         ELTDXRTN
00005  DATE-COMPILED.                                                   ELTDXRTN
00006                                                                   ELTDXRTN
00007 ****************************************************************  ELTDXRTN
00008 *     ELTDXRTN  - ELS: DIAGNOSTIC ROUTINE CARE                 *  ELTDXRTN
00009 ****************************************************************  ELTDXRTN
00010 ****************************************************************  ELTDXRTN
00011 *              U P D A T E   H I S T O R Y                     *  ELTDXRTN
00012 *                                                              *  ELTDXRTN
00013 *   DATE    PGM  DESCRIPTION                                   *  ELTDXRTN
00014 * --------  ---  --------------------------------------------- *  ELTDXRTN
00015 * 10/05/95  AKK  ORIGINAL VERSION USED ELTWELL AS BASIS MADE   *  ELTDXRTN
00016 *                MANY CHANGES.                                 *  ELTDXRTN
00017 *                                                              *  ELTDXRTN
00018 * 11/01/95  AKK  CHANGING THE OUTPUT TO MOVE EVERYTHING TO THE *  ELTDXRTN
00019 *                LEFT AND ANY LOB NOT 1,2,3 WILL NOT SAY       *  ELTDXRTN
00020 *                BASIC AND SUPPLEMENTAL.                       *  ELTDXRTN
00021 ****************************************************************  ELTDXRTN
00022  ENVIRONMENT DIVISION.                                            ELTDXRTN
00023  DATA DIVISION.                                                   ELTDXRTN
00024                                                                   ELTDXRTN
00025  WORKING-STORAGE SECTION.                                         ELTDXRTN
00026  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTDXRTN
00027          '** ELTDXRTN WS BEGINS **'.                              ELTDXRTN
00028                                                                   ELTDXRTN
00029  01  WS-SWITCHES.                                                 ELTDXRTN
00030      05  WS-CODES-MANUAL-SW      PIC X(01) VALUE SPACE.           ELTDXRTN
00031          88 WS-CODES-MANUAL                VALUE 'C'.             ELTDXRTN
00032          88 WS-NO-CODES-MANUAL             VALUE 'N'.             ELTDXRTN
00033      05  WS-FIRST-TIME-SW        PIC X(01) VALUE SPACE.           ELTDXRTN
00034          88 WS-FIRST-TIME                  VALUE 'F'.             ELTDXRTN
00035          88 WS-NOT-FIRST-TIME              VALUE 'N'.             ELTDXRTN
00036      05  WS-PROCESSING-SWITCH    PIC X(01) VALUE SPACE.           ELTDXRTN
00037          88 PROCESSING-INSTITUTIONAL       VALUE 'I'.             ELTDXRTN
00038          88 PROCESSING-PROFESSIONAL        VALUE 'P'.             ELTDXRTN
00039      05  WS-BASIC-SUPP-SWITCH    PIC X(01) VALUE SPACE.           ELTDXRTN
00040          88 PROCESSING-BASIC-INFO          VALUE 'I'.             ELTDXRTN
00041          88 PROCESSING-SUPPLEMENTAL        VALUE 'P'.             ELTDXRTN
00042      05  WS-LOB-SWITCH           PIC X(01) VALUE SPACE.           ELTDXRTN
00043          88 WS-BASIC-LOB                   VALUE 'B'.             ELTDXRTN
00044          88 WS-NOT-BASIC-LOB               VALUE SPACE.           ELTDXRTN
00045                                                                   ELTDXRTN
00046  01  WS-MISC.                                                     ELTDXRTN
00047      05  WS-SINGLE-QUOTE         PIC X  VALUE ''''.               ELTDXRTN
00048      05  WS-BASIC-LINE           PIC X.                           ELTDXRTN
00049          88 WS-BSC-LINE                    VALUE '1' '2' '3'.     ELTDXRTN
00050      05  WS-BASIC                PIC X(8)                         ELTDXRTN
00051                                    VALUE  'BASIC: '.              ELTDXRTN
00052      05  WS-SUPPLEMENTAL         PIC X(14)                        ELTDXRTN
00053                                    VALUE  'SUPPLEMENTAL: '.       ELTDXRTN
00054      05  WS-CIA                  PIC S9(03) COMP-3 VALUE +0.      ELTDXRTN
00055      05  WS-SUB                  PIC S9(03) COMP-3 VALUE +0.      ELTDXRTN
00056      05  WS-SUB2                 PIC S9(03) COMP-3 VALUE +0.      ELTDXRTN
00057      05  WS-SUB3                 PIC S9(03) COMP-3 VALUE +0.      ELTDXRTN
00058                                                                   ELTDXRTN
00059      05  WS-AGE1                 PIC 9(03).                       ELTDXRTN
00060      05  WS-AGE2 REDEFINES WS-AGE1.                               ELTDXRTN
00061          10 WS-AGE2X             PIC 9.                           ELTDXRTN
00062          10 WS-AGE2A             PIC 99.                          ELTDXRTN
00063      05  WS-AGE3 REDEFINES WS-AGE2.                               ELTDXRTN
00064          10 WS-AGE3X             PIC 99.                          ELTDXRTN
00065          10 WS-AGE3A             PIC 9.                           ELTDXRTN
00066                                                                   ELTDXRTN
00067  01  WS-TEST-LINE.                                                ELTDXRTN
00068      05  WS-TEST-CHAR         PIC X.                              ELTDXRTN
00069      05  WS-TEST-DATA         PIC X(78).                          ELTDXRTN
00070                                                                   ELTDXRTN
00071  01  WS-BP-OUTPUT-TABLE.                                          ELTDXRTN
00072      03 WS-PROV-OUTPUT-LINE OCCURS 8 TIMES                        ELTDXRTN
00073            INDEXED BY WS-OUTPUT-IDX.                              ELTDXRTN
00074         05  FILLER            PIC X(21).                          ELTDXRTN
00075         05  WS-PROV-DETAIL    PIC X(58).                          ELTDXRTN
00076                                                                   ELTDXRTN
00077 /--------------------------------------------------------------*  ELTDXRTN
00078 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTDXRTN
00079 *--------------------------------------------------------------*  ELTDXRTN
00080  01  WS-BEN-PROV-IDS.                                             ELTDXRTN
00081      05  WS-TABLE-MAX-CNT        PIC S9(04)  VALUE +5 COMP.       ELTDXRTN
00082      05  WS-LIST-BP-CNT          PIC S9(04) VALUE +0 COMP.        ELTDXRTN
00083      05  WS-INST-CNT             PIC S9(04) VALUE +5 COMP.        ELTDXRTN
00084      05  WS-INST-TABS.                                            ELTDXRTN
00085          10  FILLER              PIC  X(06) VALUE 'RDMP B'.       ELTDXRTN
00086          10  FILLER              PIC  X(06) VALUE 'RLAB B'.       ELTDXRTN
00087          10  FILLER              PIC  X(06) VALUE 'RMAM B'.       ELTDXRTN
00088          10  FILLER              PIC  X(06) VALUE 'RPAP B'.       ELTDXRTN
00089          10  FILLER              PIC  X(06) VALUE 'RXRY B'.       ELTDXRTN
00090      05  WS-INST-BP  REDEFINES  WS-INST-TABS                      ELTDXRTN
00091                                  PIC  X(06) OCCURS 5 TIMES.       ELTDXRTN
00092                                                                   ELTDXRTN
00093                                                                   ELTDXRTN
00094      05  WS-PROF-CNT          PIC S9(04) VALUE +5 COMP.           ELTDXRTN
00095      05  WS-PROF-TABS.                                            ELTDXRTN
00096          10  FILLER              PIC  X(06) VALUE 'RDMP E'.       ELTDXRTN
00097          10  FILLER              PIC  X(06) VALUE 'RLAB E'.       ELTDXRTN
00098          10  FILLER              PIC  X(06) VALUE 'RMAM E'.       ELTDXRTN
00099          10  FILLER              PIC  X(06) VALUE 'RPAP E'.       ELTDXRTN
00100          10  FILLER              PIC  X(06) VALUE 'RXRY E'.       ELTDXRTN
00101      05  WS-PROF-BP  REDEFINES  WS-PROF-TABS                      ELTDXRTN
00102                                  PIC  X(06) OCCURS 5 TIMES.       ELTDXRTN
00103                                                                   ELTDXRTN
00104 /***************************************************************  ELTDXRTN
00105 *              HEADER AND LITERAL TEXT AREA                    *  ELTDXRTN
00106 ****************************************************************  ELTDXRTN
00107  01  WS-HEADER-LINE-I.                                            ELTDXRTN
00108      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTDXRTN
00109      05  FILLER                  PIC  X(42) VALUE                 ELTDXRTN
00110              'DIAGNOSTIC SERVICES, ROUTINE INSTITUTIONAL'.        ELTDXRTN
00111      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTDXRTN
00112                                                                   ELTDXRTN
00113  01  WS-HEADER-LINE-P.                                            ELTDXRTN
00114      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTDXRTN
00115      05  FILLER                  PIC  X(41) VALUE                 ELTDXRTN
00116              'DIAGNOSTIC SERVICES, ROUTINE PROFESSIONAL'.         ELTDXRTN
00117      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTDXRTN
00118                                                                   ELTDXRTN
00119  01  WS-SERVICES-RENDERED.                                        ELTDXRTN
00120      05  FILLER                  PIC  X(26) VALUE                 ELTDXRTN
00121              'SERVICES MAY BE RENDERED: '.                        ELTDXRTN
00122      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTDXRTN
00123                                                                   ELTDXRTN
00124  01  WS-FOLLOWING-BEN.                                            ELTDXRTN
00125      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00126            'COVERED SERVICES ARE:'.                               ELTDXRTN
00127                                                                   ELTDXRTN
00128  01  WS-COV-QUAL.                                                 ELTDXRTN
00129      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00130            'THE PATIENT IS ELIGIBLE FOR THIS BENEFIT: '.          ELTDXRTN
00131                                                                   ELTDXRTN
00132  01  WS-COV-QUAL1.                                                ELTDXRTN
00133      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00134            'UNTIL THE END OF THE YEAR THE AGE OF '.               ELTDXRTN
00135                                                                   ELTDXRTN
00136  01  WS-COV-QUAL1A.                                               ELTDXRTN
00137      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00138            'IS REACHED.'.                                         ELTDXRTN
00139                                                                   ELTDXRTN
00140  01  WS-COV-QUAL2.                                                ELTDXRTN
00141      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00142            'AFTER REACHING THE AGE OF '.                          ELTDXRTN
00143                                                                   ELTDXRTN
00144  01  WS-AGE-TERM.                                                 ELTDXRTN
00145      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00146         'ELIGIBILITY FOR THIS BENEFIT TERMINATES WHEN THE PATIENT ELTDXRTN
00147 -       'REACHES: '.                                              ELTDXRTN
00148                                                                   ELTDXRTN
00149  01  WS-AGE-TERMA.                                                ELTDXRTN
00150      05  FILLER                  PIC  X(79) VALUE                 ELTDXRTN
00151            'THE AGE OF '.                                         ELTDXRTN
00152                                                                   ELTDXRTN
00153  01  WS-PAY-CONSDR-TEXT1.                                         ELTDXRTN
00154      05  FILLER                  PIC X(49)                        ELTDXRTN
00155        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDXRTN
00156                                                                   ELTDXRTN
00157  01  WS-PAY-CONSDR-TEXT2.                                         ELTDXRTN
00158      05  FILLER                  PIC X(45)                        ELTDXRTN
00159        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDXRTN
00160                                                                   ELTDXRTN
00161  01  WS-PAYABLE-AS.                                               ELTDXRTN
00162      10  FILLER                  PIC  X(45) VALUE                 ELTDXRTN
00163              'THESE SERVICES ARE PRICED ACCORDING TO: '.          ELTDXRTN
00164      10  FILLER                  PIC  X(34) VALUE LOW-VALUES.     ELTDXRTN
00165                                                                   ELTDXRTN
00166  01  WS-CONTRACT-RELATED.                                         ELTDXRTN
00167      05  FILLER                  PIC  X(48) VALUE                 ELTDXRTN
00168              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTDXRTN
00169                                                                   ELTDXRTN
00170  01  WS-BASICX.                                                   ELTDXRTN
00171      05  WS-BASIC-LIT            PIC  X(14) VALUE                 ELTDXRTN
00172              '       BASIC: '.                                    ELTDXRTN
00173      05  WS-DTL-BASIC-LONG.                                       ELTDXRTN
00174          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTDXRTN
00175          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTDXRTN
00176  01  WS-BASIC-A.                                                  ELTDXRTN
00177      05  FILLER                  PIC X(14) VALUE SPACES.          ELTDXRTN
00178      05  WS-DTL-BASIC-A          PIC X(65) VALUE SPACES.          ELTDXRTN
00179                                                                   ELTDXRTN
00180  01  WS-SUPPLEMENTAL-A.                                           ELTDXRTN
00181      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTDXRTN
00182              '  SUPPLEMENTAL: '.                                  ELTDXRTN
00183      05  WS-DTL-SUPP-LONG.                                        ELTDXRTN
00184          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTDXRTN
00185          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTDXRTN
00186                                                                   ELTDXRTN
00187  01  WS-PVE.                                                      ELTDXRTN
00188      05  FILLER                  PIC  X(44) VALUE                 ELTDXRTN
00189              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTDXRTN
00190      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTDXRTN
00191                                                                   ELTDXRTN
00192  01  WS-INDICES-PROBLEM.                                          ELTDXRTN
00193      05  FILLER                  PIC  X(20) VALUE                 ELTDXRTN
00194              'PROBLEM WITH INDICES'.                              ELTDXRTN
00195      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTDXRTN
00196                                                                   ELTDXRTN
00197      05  WS-PROF-OUTPT-CHRGES        PIC  X(62) VALUE             ELTDXRTN
00198            'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE ELTDXRTN
00199 -          'REPORT: '.                                            ELTDXRTN
00200                                                                   ELTDXRTN
00201  01  WS-POSSIBLE-ERROR.                                           ELTDXRTN
00202      05  FILLER                   PIC  X(50) VALUE                ELTDXRTN
00203              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTDXRTN
00204      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTDXRTN
00205                                                                   ELTDXRTN
00206  01  WS-INVALID-REQ.                                              ELTDXRTN
00207      05  FILLER                  PIC  X(37) VALUE                 ELTDXRTN
00208              '*** I N V A L I D   R E Q U E S T ***'.             ELTDXRTN
00209      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTDXRTN
00210                                                                   ELTDXRTN
00211  01  WS-SPILLOVER.                                                ELTDXRTN
00212      05  WS-SPILLOVER-DEDBL          PIC X(22)                    ELTDXRTN
00213                             VALUE 'SPILLOVER DEDUCTIBLE: '.       ELTDXRTN
00214      05  WS-SPILLOVER-COINS          PIC X(23)                    ELTDXRTN
00215                             VALUE 'SPILLOVER COINSURANCE: '.      ELTDXRTN
00216  01  WS-OTHER-LITERALS.                                           ELTDXRTN
00217       05  WS-NO-TABULAR1.                                         ELTDXRTN
00218          10  FILLER                    PIC X(51)  VALUE           ELTDXRTN
00219         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTDXRTN
00220          10  FILLER                    PIC X(22)  VALUE           ELTDXRTN
00221         'GOING FROM BENEFIT ***'.                                 ELTDXRTN
00222                                                                   ELTDXRTN
00223    05  WS-NO-TABULAR2.                                            ELTDXRTN
00224      10  FILLER                    PIC X(15)  VALUE               ELTDXRTN
00225          '*** PROVISION: '.                                       ELTDXRTN
00226      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTDXRTN
00227      10  FILLER                    PIC X VALUE SPACE.             ELTDXRTN
00228      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTDXRTN
00229      10  FILLER                    PIC X(13)  VALUE               ELTDXRTN
00230         ' TO TABULAR: '.                                          ELTDXRTN
00231      10  WS-NO-TAB-ID              PIC X(6).                      ELTDXRTN
00232      10  FILLER                    PIC X VALUE SPACE.             ELTDXRTN
00233      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTDXRTN
00234      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTDXRTN
00235                                                                   ELTDXRTN
00236    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTDXRTN
00237    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTDXRTN
00238      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTDXRTN
00239                                                                   ELTDXRTN
00240      TITLE 'LINKAGE SECTION'.                                     ELTDXRTN
00241  LINKAGE SECTION.                                                 ELTDXRTN
00242  01  DFHCOMMAREA.                                                 ELTDXRTN
00243      COPY ELSCOMMC.                                               ELTDXRTN
00244                                                                   ELTDXRTN
00245      COPY ELSCIA2C.                                               ELTDXRTN
00246                                                                   ELTDXRTN
00247      COPY ELSIOPMC.                                               ELTDXRTN
00248                                                                   ELTDXRTN
00249      COPY ELSKEYSC.                                               ELTDXRTN
00250                                                                   ELTDXRTN
00251      COPY ELSOUTPC.                                               ELTDXRTN
00252                                                                   ELTDXRTN
00253      COPY ELSSSCBC.                                               ELTDXRTN
00254                                                                   ELTDXRTN
00255      COPY ELSCMIFC.                                               ELTDXRTN
00256                                                                   ELTDXRTN
00257      COPY ELSCMDSC.                                               ELTDXRTN
00258                                                                   ELTDXRTN
00259      COPY ELSPRVNC.                                               ELTDXRTN
00260                                                                   ELTDXRTN
00261      COPY ELSTCWAC.                                               ELTDXRTN
00262                                                                   ELTDXRTN
00263      COPY ELSPLGSW.                                               ELTDXRTN
00264                                                                   ELTDXRTN
00265      COPY ELSPLGTB.                                               ELTDXRTN
00266                                                                   ELTDXRTN
00267  01  CONTRACT-RECORD.                                             ELTDXRTN
00268       COPY GCCONTRC.                                              ELTDXRTN
00269 /                                                                 ELTDXRTN
00270  PROCEDURE DIVISION.                                              ELTDXRTN
00271  0000-MAINLINE.                                                   ELTDXRTN
00272      PERFORM 1000-INITIALIZATION.                                 ELTDXRTN
00273      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTDXRTN
00274          PERFORM 2000-INSTITUTIONAL.                              ELTDXRTN
00275      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTDXRTN
00276          PERFORM 4000-PROFESSIONAL.                               ELTDXRTN
00277                                                                   ELTDXRTN
00278      MOVE 'E'   TO  COF-FUNCTION.                                 ELTDXRTN
00279      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTDXRTN
00280                                                                   ELTDXRTN
00281      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
00282                            DFHCOMMAREA.                           ELTDXRTN
00283      GOBACK.                                                      ELTDXRTN
00284                                                                   ELTDXRTN
00285  1000-INITIALIZATION.                                             ELTDXRTN
00286                                                                   ELTDXRTN
00287      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDXRTN
00288         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTDXRTN
00289         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDXRTN
00290                                                                   ELTDXRTN
00291      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDXRTN
00292          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTDXRTN
00293                                                                   ELTDXRTN
00294      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDXRTN
00295      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00296          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDXRTN
00297                                                                   ELTDXRTN
00298      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDXRTN
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00300          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDXRTN
00301                                                                   ELTDXRTN
00302      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDXRTN
00303      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00304          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDXRTN
00305                                                                   ELTDXRTN
00306      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDXRTN
00307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00308          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDXRTN
00309                                                                   ELTDXRTN
00310      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDXRTN
00311      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00312          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDXRTN
00313                                                                   ELTDXRTN
00314      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDXRTN
00315      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDXRTN
00316              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDXRTN
00317                                                                   ELTDXRTN
00318      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTDXRTN
00319      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDXRTN
00320      EXEC CICS LINK                                               ELTDXRTN
00321                PROGRAM('ELUSTGMG')                                ELTDXRTN
00322                COMMAREA(DFHCOMMAREA)                              ELTDXRTN
00323      END-EXEC.                                                    ELTDXRTN
00324      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXRTN
00325      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
00326          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDXRTN
00327      PERFORM 9999-CHECK-CONTRACT.                                 ELTDXRTN
00328                                                                   ELTDXRTN
00329  2000-INSTITUTIONAL.                                              ELTDXRTN
00330      MOVE WS-HEADER-LINE-I  TO  COF-HDR-LINE (2).                 ELTDXRTN
00331      PERFORM 9100-HEADER-OUTPUT-REQUEST.                          ELTDXRTN
00332      MOVE WS-INST-CNT  TO  PVN-NBR-BEN-PROVN.                     ELTDXRTN
00333      PERFORM 2010-MOVE-IN-INST-TABS                               ELTDXRTN
00334           VARYING WS-SUB FROM +1 BY +1                            ELTDXRTN
00335               UNTIL WS-SUB  >  WS-INST-CNT.                       ELTDXRTN
00336      PERFORM 8020-CALL-COVERAGE.                                  ELTDXRTN
00337      IF PVN-COVG-NONE                                             ELTDXRTN
00338         CONTINUE                                                  ELTDXRTN
00339      ELSE                                                         ELTDXRTN
00340         INITIALIZE WS-BP-OUTPUT-TABLE                             ELTDXRTN
00341         SET WS-OUTPUT-IDX TO 1                                    ELTDXRTN
00342         PERFORM 2030-FIND-FIRST-NONZERO                           ELTDXRTN
00343              VARYING WS-SUB FROM +1 BY +1                         ELTDXRTN
00344                 UNTIL   WS-SUB  > WS-INST-CNT                     ELTDXRTN
00345      END-IF.                                                      ELTDXRTN
00346 /                                                                 ELTDXRTN
00347  2010-MOVE-IN-INST-TABS.                                          ELTDXRTN
00348      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXRTN
00349      MOVE WS-INST-BP (WS-SUB)                                     ELTDXRTN
00350                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDXRTN
00351                                                                   ELTDXRTN
00352      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDXRTN
00353                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDXRTN
00354                                                                   ELTDXRTN
00355                                                                   ELTDXRTN
00356  2030-FIND-FIRST-NONZERO.                                         ELTDXRTN
00357      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXRTN
00358      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTDXRTN
00359          CONTINUE                                                 ELTDXRTN
00360      ELSE                                                         ELTDXRTN
00361          PERFORM 2100-BUILD-SCREEN-LINES                          ELTDXRTN
00362      END-IF.                                                      ELTDXRTN
00363 /                                                                 ELTDXRTN
00364  2100-BUILD-SCREEN-LINES.                                         ELTDXRTN
00365      SET PLT-INDEX1  TO                                           ELTDXRTN
00366              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDXRTN
00367      MOVE  +1  TO  WS-CIA.                                        ELTDXRTN
00368      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDXRTN
00369          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDXRTN
00370              SET PLT-INDEX2  TO  2                                ELTDXRTN
00371              MOVE WS-INST-CNT TO WS-LIST-BP-CNT                   ELTDXRTN
00372              PERFORM 5000-CREATE-DETAIL-DISPLAY                   ELTDXRTN
00373          ELSE                                                     ELTDXRTN
00374              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDXRTN
00375              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDXRTN
00376      ELSE                                                         ELTDXRTN
00377          SET PLT-INDEX2  TO  1                                    ELTDXRTN
00378          MOVE WS-INST-CNT TO WS-LIST-BP-CNT                       ELTDXRTN
00379          PERFORM 5000-CREATE-DETAIL-DISPLAY                       ELTDXRTN
00380      END-IF.                                                      ELTDXRTN
00381 /                                                                 ELTDXRTN
00382 ****************************************************************  ELTDXRTN
00383 *   DRUGS/MEDICATIONS PROFESSIONAL INPATIENT PROCESSING        *  ELTDXRTN
00384 ****************************************************************  ELTDXRTN
00385  4000-PROFESSIONAL.                                               ELTDXRTN
00386      MOVE WS-HEADER-LINE-P  TO  COF-HDR-LINE (2).                 ELTDXRTN
00387      PERFORM 9100-HEADER-OUTPUT-REQUEST.                          ELTDXRTN
00388      MOVE WS-PROF-CNT  TO  PVN-NBR-BEN-PROVN.                     ELTDXRTN
00389      PERFORM 4010-MOVE-IN-PROF-TABS                               ELTDXRTN
00390           VARYING WS-SUB FROM +1 BY +1                            ELTDXRTN
00391               UNTIL WS-SUB  >  WS-PROF-CNT.                       ELTDXRTN
00392      PERFORM 9020-CALL-COVERAGE.                                  ELTDXRTN
00393      IF PVN-COVG-NONE                                             ELTDXRTN
00394         CONTINUE                                                  ELTDXRTN
00395      ELSE                                                         ELTDXRTN
00396         INITIALIZE WS-BP-OUTPUT-TABLE                             ELTDXRTN
00397         SET WS-OUTPUT-IDX TO 1                                    ELTDXRTN
00398         PERFORM 4030-FIND-FIRST-NONZERO                           ELTDXRTN
00399              VARYING WS-SUB FROM +1 BY +1                         ELTDXRTN
00400                 UNTIL   WS-SUB  > WS-PROF-CNT                     ELTDXRTN
00401      END-IF.                                                      ELTDXRTN
00402 /                                                                 ELTDXRTN
00403  4010-MOVE-IN-PROF-TABS.                                          ELTDXRTN
00404      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXRTN
00405      MOVE WS-PROF-BP (WS-SUB)                                     ELTDXRTN
00406                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTDXRTN
00407      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTDXRTN
00408                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTDXRTN
00409                                                                   ELTDXRTN
00410  4030-FIND-FIRST-NONZERO.                                         ELTDXRTN
00411      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXRTN
00412      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTDXRTN
00413          NEXT SENTENCE                                            ELTDXRTN
00414      ELSE                                                         ELTDXRTN
00415          PERFORM 4100-BUILD-SCREEN-LINES                          ELTDXRTN
00416      END-IF.                                                      ELTDXRTN
00417                                                                   ELTDXRTN
00418 /                                                                 ELTDXRTN
00419  4100-BUILD-SCREEN-LINES.                                         ELTDXRTN
00420      SET PLT-INDEX1  TO                                           ELTDXRTN
00421              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTDXRTN
00422      MOVE  +1  TO  WS-CIA.                                        ELTDXRTN
00423      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTDXRTN
00424          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTDXRTN
00425              SET PLT-INDEX2  TO  2                                ELTDXRTN
00426              MOVE WS-PROF-CNT TO WS-LIST-BP-CNT                   ELTDXRTN
00427              PERFORM 5000-CREATE-DETAIL-DISPLAY                   ELTDXRTN
00428          ELSE                                                     ELTDXRTN
00429              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTDXRTN
00430              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTDXRTN
00431      ELSE                                                         ELTDXRTN
00432          SET PLT-INDEX2  TO  1                                    ELTDXRTN
00433          MOVE WS-PROF-CNT TO WS-LIST-BP-CNT                       ELTDXRTN
00434          PERFORM 5000-CREATE-DETAIL-DISPLAY                       ELTDXRTN
00435      END-IF.                                                      ELTDXRTN
00436                                                                   ELTDXRTN
00437  5000-CREATE-DETAIL-DISPLAY.                                      ELTDXRTN
00438      PERFORM 6105-LIST-BEN-PROV.                                  ELTDXRTN
00439      PERFORM 6150-COV-QUALIFIER.                                  ELTDXRTN
00440      PERFORM 6175-AGE-TERM.                                       ELTDXRTN
00441      PERFORM 6110-PLACE-OF-TREATMENT.                             ELTDXRTN
00442      PERFORM 6120-PRIC-METH.                                      ELTDXRTN
00443      PERFORM 6300-TRNSFR-OTHER-RESP.                              ELTDXRTN
00444      PERFORM 6400-SPILL-OVER-COINS.                               ELTDXRTN
00445      PERFORM 6500-SPILL-OVER-DED.                                 ELTDXRTN
00446      IF PROCESSING-INSTITUTIONAL                                  ELTDXRTN
00447         PERFORM 6600-PROF-CHRG-HSP-CLM.                           ELTDXRTN
00448      PERFORM 6900-BEN-TAB-PVE.                                    ELTDXRTN
00449      PERFORM 7200-PAY-CONSID-TEXT.                                ELTDXRTN
00450                                                                   ELTDXRTN
00451  6105-LIST-BEN-PROV.                                              ELTDXRTN
00452      MOVE  +2               TO  WS-CIA.                           ELTDXRTN
00453      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTDXRTN
00454      ADD  +1               TO  WS-CIA.                            ELTDXRTN
00455      MOVE ZERO              TO  WS-SUB2.                          ELTDXRTN
00456      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTDXRTN
00457                             TO  WS-SUB3.                          ELTDXRTN
00458      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTDXRTN
00459         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTDXRTN
00460            UNTIL   PVN-BEN-PROVN-IDX > WS-LIST-BP-CNT.            ELTDXRTN
00461      PERFORM 6108-DISPLAY-BP-LIST.                                ELTDXRTN
00462                                                                   ELTDXRTN
00463      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXRTN
00464      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTDXRTN
00465      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
00466                            DFHCOMMAREA.                           ELTDXRTN
00467      MOVE 1 TO WS-CIA.                                            ELTDXRTN
00468      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTDXRTN
00469      INITIALIZE WS-BP-OUTPUT-TABLE.                               ELTDXRTN
00470      SET WS-OUTPUT-IDX TO 1.                                      ELTDXRTN
00471                                                                   ELTDXRTN
00472  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTDXRTN
00473      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTDXRTN
00474          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTDXRTN
00475          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTDXRTN
00476          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTDXRTN
00477                            TO  CMF-CODE-VALUE                     ELTDXRTN
00478          PERFORM 9500-SETUP-DSPLY-BP-LIST                         ELTDXRTN
00479          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTDXRTN
00480          ADD  +1    TO  WS-SUB2.                                  ELTDXRTN
00481                                                                   ELTDXRTN
00482                                                                   ELTDXRTN
00483 ****************************************************************  ELTDXRTN
00484 *    DISPLAY BENEFIT PROVISION LIST                            *  ELTDXRTN
00485 ****************************************************************  ELTDXRTN
00486  6108-DISPLAY-BP-LIST.                                            ELTDXRTN
00487      PERFORM VARYING WS-OUTPUT-IDX FROM 1 BY 1 UNTIL              ELTDXRTN
00488         WS-OUTPUT-IDX > WS-LIST-BP-CNT OR                         ELTDXRTN
00489         (WS-OUTPUT-IDX > WS-SUB2)                                 ELTDXRTN
00490             OR WS-PROV-OUTPUT-LINE(WS-OUTPUT-IDX) = SPACES        ELTDXRTN
00491           MOVE WS-PROV-OUTPUT-LINE (WS-OUTPUT-IDX) TO             ELTDXRTN
00492              COF-DTL-LINE (WS-CIA)                                ELTDXRTN
00493           ADD 1 TO WS-CIA                                         ELTDXRTN
00494           IF WS-CIA > 20  OR WS-CIA = 20                          ELTDXRTN
00495              MOVE WS-CIA    TO  COF-NBR-DTL-LINES                 ELTDXRTN
00496              CALL 'ELUOUTPT' USING DFHEIBLK                       ELTDXRTN
00497                                    DFHCOMMAREA                    ELTDXRTN
00498              MOVE 1 TO WS-CIA                                     ELTDXRTN
00499              INITIALIZE WS-BP-OUTPUT-TABLE                        ELTDXRTN
00500              SET WS-OUTPUT-IDX TO 1                               ELTDXRTN
00501           END-IF                                                  ELTDXRTN
00502      END-PERFORM.                                                 ELTDXRTN
00503                                                                   ELTDXRTN
00504 ****************************************************************  ELTDXRTN
00505 *              P L A C E   O F   T R E A T M E N T             *  ELTDXRTN
00506 ****************************************************************  ELTDXRTN
00507  6110-PLACE-OF-TREATMENT.                                         ELTDXRTN
00508      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00509      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
00510      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00511      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTDXRTN
00512         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00513               NOT =  ZERO                                         ELTDXRTN
00514 *        MOVE +2                    TO  WS-CIA                    ELTDXRTN
00515          MOVE +1                    TO  WS-CIA                    ELTDXRTN
00516          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)     ELTDXRTN
00517 *        ADD +1 TO WS-CIA.                                        ELTDXRTN
00518      ELSE                                                         ELTDXRTN
00519         SET  PLT-INDEX2  TO  2                                    ELTDXRTN
00520         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDXRTN
00521          AND   PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)  ELTDXRTN
00522                  NOT  =  ZERO                                     ELTDXRTN
00523 *           MOVE +2                    TO  WS-CIA                 ELTDXRTN
00524             MOVE +1                    TO  WS-CIA                 ELTDXRTN
00525             MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)  ELTDXRTN
00526             ADD +1 TO WS-CIA                                      ELTDXRTN
00527      END-IF.                                                      ELTDXRTN
00528                                                                   ELTDXRTN
00529      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00530      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXRTN
00531         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00532               NOT  =  ZERO                                        ELTDXRTN
00533          IF WS-BASIC-LOB                                          ELTDXRTN
00534             SET PROCESSING-BASIC-INFO TO TRUE                     ELTDXRTN
00535          END-IF                                                   ELTDXRTN
00536 *        IF WS-BASIC-LOB                                          ELTDXRTN
00537 *           MOVE WS-BASIC  TO TCAR-FROM-LINE(TCAR-FROM-SUB)       ELTDXRTN
00538 *           ADD +1 TO TCAR-FROM-SUB                               ELTDXRTN
00539 *        END-IF                                                   ELTDXRTN
00540          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDXRTN
00541          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTDXRTN
00542                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDXRTN
00543          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTDXRTN
00544                TO  CMF-CODE-VALUE                                 ELTDXRTN
00545          PERFORM 9650-CALL-CODES-MANUAL                           ELTDXRTN
00546          PERFORM 9700-DETERMINE-OUTPUT-METHOD                     ELTDXRTN
00547      END-IF.                                                      ELTDXRTN
00548                                                                   ELTDXRTN
00549      SET PLT-INDEX2  TO  2.                                       ELTDXRTN
00550      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXRTN
00551         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00552              NOT  =  ZERO                                         ELTDXRTN
00553         IF WS-BASIC-LOB                                           ELTDXRTN
00554            SET PROCESSING-SUPPLEMENTAL TO TRUE                    ELTDXRTN
00555         END-IF                                                    ELTDXRTN
00556 *           MOVE WS-SUPPLEMENTAL                                  ELTDXRTN
00557 *                      TO TCAR-FROM-LINE(TCAR-FROM-SUB)           ELTDXRTN
00558 *           ADD +1 TO TCAR-FROM-SUB                               ELTDXRTN
00559 *        END-IF                                                   ELTDXRTN
00560          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTDXRTN
00561          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTDXRTN
00562                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTDXRTN
00563          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTDXRTN
00564               TO  CMF-CODE-VALUE                                  ELTDXRTN
00565          PERFORM 9650-CALL-CODES-MANUAL                           ELTDXRTN
00566          PERFORM 9700-DETERMINE-OUTPUT-METHOD                     ELTDXRTN
00567 *        INITIALIZE WS-LOB-SWITCH                                 ELTDXRTN
00568          INITIALIZE WS-BASIC-SUPP-SWITCH                          ELTDXRTN
00569      END-IF.                                                      ELTDXRTN
00570                                                                   ELTDXRTN
00571      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXRTN
00572         SET PLT-INDEX2  TO  2                                     ELTDXRTN
00573      ELSE                                                         ELTDXRTN
00574         SET PLT-INDEX2  TO  1.                                    ELTDXRTN
00575 *    INITIALIZE WS-LOB-SWITCH.                                    ELTDXRTN
00576      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXRTN
00577                                                                   ELTDXRTN
00578 ****************************************************************  ELTDXRTN
00579 *  P R O V I S I O N   P R I C I N G   M E T H O D             *  ELTDXRTN
00580 *                                                              *  ELTDXRTN
00581 ****************************************************************  ELTDXRTN
00582  6120-PRIC-METH.                                                  ELTDXRTN
00583      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00584      MOVE +1 TO TCAR-FROM-SUB.                                    ELTDXRTN
00585      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00586      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTDXRTN
00587         AND  PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00588              NOT = '19'                                           ELTDXRTN
00589         MOVE +1             TO  WS-CIA                            ELTDXRTN
00590         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDXRTN
00591      ELSE                                                         ELTDXRTN
00592         SET  PLT-INDEX2  TO  2                                    ELTDXRTN
00593         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO  ELTDXRTN
00594              AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  ELTDXRTN
00595                  NOT = '19'                                       ELTDXRTN
00596            MOVE +1             TO  WS-CIA                         ELTDXRTN
00597            MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)           ELTDXRTN
00598            ADD +1 TO WS-CIA                                       ELTDXRTN
00599      END-IF.                                                      ELTDXRTN
00600                                                                   ELTDXRTN
00601      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXRTN
00603         AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)       ELTDXRTN
00604             NOT = ZERO AND  NOT =  '19'                           ELTDXRTN
00605        IF WS-BASIC-LOB                                            ELTDXRTN
00606             SET PROCESSING-BASIC-INFO TO TRUE                     ELTDXRTN
00607        END-IF                                                     ELTDXRTN
00608         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXRTN
00609         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXRTN
00610         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00611              TO  CMF-CODE-VALUE                                   ELTDXRTN
00612         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00613         PERFORM 9700-DETERMINE-OUTPUT-METHOD.                     ELTDXRTN
00614                                                                   ELTDXRTN
00615      SET  PLT-INDEX2  TO  2.                                      ELTDXRTN
00616      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXRTN
00617         AND   PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTDXRTN
00618               NOT = ZERO AND  NOT =  '19'                         ELTDXRTN
00619        IF WS-BASIC-LOB                                            ELTDXRTN
00620             SET PROCESSING-SUPPLEMENTAL TO TRUE                   ELTDXRTN
00621        END-IF                                                     ELTDXRTN
00622         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXRTN
00623         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXRTN
00624         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00625              TO CMF-CODE-VALUE                                    ELTDXRTN
00626         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00627         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTDXRTN
00628      END-IF.                                                      ELTDXRTN
00629         INITIALIZE WS-BASIC-SUPP-SWITCH.                          ELTDXRTN
00630                                                                   ELTDXRTN
00631      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXRTN
00632         SET PLT-INDEX2  TO  2                                     ELTDXRTN
00633      ELSE                                                         ELTDXRTN
00634         SET PLT-INDEX2  TO  1.                                    ELTDXRTN
00635                                                                   ELTDXRTN
00636 ****************************************************************  ELTDXRTN
00637 *  COVERAGE QUALIFIER AND AGE LEVEL 1                          *  ELTDXRTN
00638 *                                                              *  ELTDXRTN
00639 ****************************************************************  ELTDXRTN
00640  6150-COV-QUALIFIER.                                              ELTDXRTN
00641      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00642      MOVE +1 TO TCAR-FROM-SUB.                                    ELTDXRTN
00643      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00644      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTDXRTN
00645        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTDXRTN
00646 *       MOVE +2             TO  WS-CIA                            ELTDXRTN
00647         MOVE +1             TO  WS-CIA                            ELTDXRTN
00648         MOVE WS-COV-QUAL  TO  COF-DTL-LINE(WS-CIA)                ELTDXRTN
00649 *       ADD +1 TO WS-CIA.                                         ELTDXRTN
00650      ELSE                                                         ELTDXRTN
00651         SET  PLT-INDEX2  TO  2                                    ELTDXRTN
00652         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO  ELTDXRTN
00653           AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES     ELTDXRTN
00654 *          MOVE +2             TO  WS-CIA                         ELTDXRTN
00655            MOVE +1             TO  WS-CIA                         ELTDXRTN
00656            MOVE WS-COV-QUAL  TO  COF-DTL-LINE(WS-CIA)             ELTDXRTN
00657            ADD +1 TO WS-CIA                                       ELTDXRTN
00658      END-IF.                                                      ELTDXRTN
00659                                                                   ELTDXRTN
00660      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00661      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXRTN
00662        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTDXRTN
00663        IF WS-BASIC-LOB                                            ELTDXRTN
00664             SET PROCESSING-BASIC-INFO TO TRUE                     ELTDXRTN
00665        END-IF                                                     ELTDXRTN
00666        PERFORM 6155-SETUP-COV-QUAL-PHRASE                         ELTDXRTN
00667        PERFORM 6157-SETUP-AGE-LEVEL                               ELTDXRTN
00668        IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'          ELTDXRTN
00669           MOVE WS-COV-QUAL1A TO TCAR-FROM-LINE(TCAR-FROM-SUB)     ELTDXRTN
00670           ADD +1 TO TCAR-FROM-SUB                                 ELTDXRTN
00671        END-IF                                                     ELTDXRTN
00672        SET WS-NO-CODES-MANUAL TO TRUE                             ELTDXRTN
00673        PERFORM 9600-DISPLAY-CODE-VALUES                           ELTDXRTN
00674      END-IF.                                                      ELTDXRTN
00675                                                                   ELTDXRTN
00676      SET  PLT-INDEX2  TO  2.                                      ELTDXRTN
00677      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXRTN
00678        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTDXRTN
00679        IF WS-BASIC-LOB                                            ELTDXRTN
00680         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXRTN
00681        END-IF                                                     ELTDXRTN
00682        PERFORM 6155-SETUP-COV-QUAL-PHRASE                         ELTDXRTN
00683        PERFORM 6157-SETUP-AGE-LEVEL                               ELTDXRTN
00684        IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'          ELTDXRTN
00685           MOVE WS-COV-QUAL1A TO TCAR-FROM-LINE(TCAR-FROM-SUB)     ELTDXRTN
00686           ADD +1 TO TCAR-FROM-SUB                                 ELTDXRTN
00687        END-IF                                                     ELTDXRTN
00688        SET WS-NO-CODES-MANUAL TO TRUE                             ELTDXRTN
00689        PERFORM 9600-DISPLAY-CODE-VALUES                           ELTDXRTN
00690      END-IF.                                                      ELTDXRTN
00691 *    INITIALIZE WS-LOB-SWITCH.                                    ELTDXRTN
00692                                                                   ELTDXRTN
00693      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXRTN
00694         SET PLT-INDEX2  TO  2                                     ELTDXRTN
00695      ELSE                                                         ELTDXRTN
00696         SET PLT-INDEX2  TO  1.                                    ELTDXRTN
00697      INITIALIZE WS-CODES-MANUAL-SW.                               ELTDXRTN
00698      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXRTN
00699                                                                   ELTDXRTN
00700  6155-SETUP-COV-QUAL-PHRASE.                                      ELTDXRTN
00701      IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'            ELTDXRTN
00702         MOVE WS-COV-QUAL1 TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTDXRTN
00703      ELSE                                                         ELTDXRTN
00704         IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '002'         ELTDXRTN
00705            MOVE WS-COV-QUAL2 TO TCAR-FROM-LINE(TCAR-FROM-SUB)     ELTDXRTN
00706      END-IF.                                                      ELTDXRTN
00707      ADD 1 TO TCAR-FROM-SUB.                                      ELTDXRTN
00708                                                                   ELTDXRTN
00709  6157-SETUP-AGE-LEVEL.                                            ELTDXRTN
00710      MOVE PLP-AGE-LVL-1(PLT-INDEX1, PLT-INDEX2) TO WS-AGE1.       ELTDXRTN
00711      EVALUATE TRUE                                                ELTDXRTN
00712         WHEN WS-AGE3X = ZEROES                                    ELTDXRTN
00713            MOVE WS-AGE3A                                          ELTDXRTN
00714               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00715         WHEN WS-AGE2X = ZERO                                      ELTDXRTN
00716            MOVE WS-AGE2A                                          ELTDXRTN
00717               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00718         WHEN OTHER                                                ELTDXRTN
00719            MOVE WS-AGE1                                           ELTDXRTN
00720               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00721      END-EVALUATE.                                                ELTDXRTN
00722      ADD +1 TO TCAR-FROM-SUB.                                     ELTDXRTN
00723                                                                   ELTDXRTN
00724 ****************************************************************  ELTDXRTN
00725 *  AGE TERMINATION INDICATOR                                   *  ELTDXRTN
00726 *                                                              *  ELTDXRTN
00727 ****************************************************************  ELTDXRTN
00728  6175-AGE-TERM.                                                   ELTDXRTN
00729      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00730      MOVE +1 TO TCAR-FROM-SUB.                                    ELTDXRTN
00731      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00732      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTDXRTN
00733        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXRTN
00734                   > ZEROES                                        ELTDXRTN
00735         MOVE +2             TO  WS-CIA                            ELTDXRTN
00736         MOVE WS-AGE-TERM  TO  COF-DTL-LINE(WS-CIA).               ELTDXRTN
00737 *       ADD +1 TO WS-CIA.                                         ELTDXRTN
00738                                                                   ELTDXRTN
00739      SET  PLT-INDEX2  TO  2.                                      ELTDXRTN
00740      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO     ELTDXRTN
00741        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXRTN
00742                  > ZEROES                                         ELTDXRTN
00743         MOVE +2             TO  WS-CIA                            ELTDXRTN
00744         MOVE WS-AGE-TERM  TO  COF-DTL-LINE(WS-CIA).               ELTDXRTN
00745 *       ADD +1 TO WS-CIA.                                         ELTDXRTN
00746                                                                   ELTDXRTN
00747      SET  PLT-INDEX2  TO  1.                                      ELTDXRTN
00748      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXRTN
00749        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXRTN
00750                        > ZEROES                                   ELTDXRTN
00751 *       IF WS-BASIC-LOB                                           ELTDXRTN
00752 *          MOVE WS-BASIC  TO  TCAR-FROM-LINE(TCAR-FROM-SUB)       ELTDXRTN
00753 *          ADD +1 TO TCAR-FROM-SUB                                ELTDXRTN
00754 *       END-IF                                                    ELTDXRTN
00755         MOVE WS-AGE-TERMA TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTDXRTN
00756         ADD +1 TO TCAR-FROM-SUB                                   ELTDXRTN
00757         PERFORM 6179-SETUP-TERMINATION-AGE                        ELTDXRTN
00758         PERFORM 6177-SETUP-AGE-TERM-PHRASE                        ELTDXRTN
00759 *       PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00760         PERFORM 9700-DETERMINE-OUTPUT-METHOD                      ELTDXRTN
00761      END-IF.                                                      ELTDXRTN
00762                                                                   ELTDXRTN
00763      SET  PLT-INDEX2  TO  2.                                      ELTDXRTN
00764      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXRTN
00765        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTDXRTN
00766                        > ZEROES                                   ELTDXRTN
00767         IF WS-BASIC-LOB                                           ELTDXRTN
00768            MOVE WS-SUPPLEMENTAL                                   ELTDXRTN
00769                        TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTDXRTN
00770            ADD +1 TO TCAR-FROM-SUB                                ELTDXRTN
00771         END-IF                                                    ELTDXRTN
00772         MOVE WS-AGE-TERMA TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTDXRTN
00773         ADD +1 TO TCAR-FROM-SUB                                   ELTDXRTN
00774         PERFORM 6179-SETUP-TERMINATION-AGE                        ELTDXRTN
00775         PERFORM 6177-SETUP-AGE-TERM-PHRASE                        ELTDXRTN
00776         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00777         PERFORM 9700-DETERMINE-OUTPUT-METHOD                      ELTDXRTN
00778      END-IF.                                                      ELTDXRTN
00779                                                                   ELTDXRTN
00780      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXRTN
00781         SET PLT-INDEX2  TO  2                                     ELTDXRTN
00782      ELSE                                                         ELTDXRTN
00783         SET PLT-INDEX2  TO  1.                                    ELTDXRTN
00784                                                                   ELTDXRTN
00785  6177-SETUP-AGE-TERM-PHRASE.                                      ELTDXRTN
00786      EVALUATE TRUE                                                ELTDXRTN
00787         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '0'  ELTDXRTN
00788            CONTINUE                                               ELTDXRTN
00789         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '1'  ELTDXRTN
00790            MOVE 'YEARS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTDXRTN
00791         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '2'  ELTDXRTN
00792            MOVE 'WEEKS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTDXRTN
00793         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '3'  ELTDXRTN
00794            MOVE 'DAYS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)           ELTDXRTN
00795      END-EVALUATE.                                                ELTDXRTN
00796      ADD 1 TO TCAR-FROM-SUB.                                      ELTDXRTN
00797                                                                   ELTDXRTN
00798  6179-SETUP-TERMINATION-AGE.                                      ELTDXRTN
00799      MOVE PLP-AGE-COV-TERMN(PLT-INDEX1, PLT-INDEX2) TO WS-AGE1.   ELTDXRTN
00800      EVALUATE TRUE                                                ELTDXRTN
00801         WHEN WS-AGE3X = ZEROES                                    ELTDXRTN
00802            MOVE WS-AGE3A                                          ELTDXRTN
00803               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00804         WHEN WS-AGE2X = ZERO                                      ELTDXRTN
00805            MOVE WS-AGE2A                                          ELTDXRTN
00806               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00807         WHEN OTHER                                                ELTDXRTN
00808            MOVE WS-AGE1                                           ELTDXRTN
00809               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00810      END-EVALUATE.                                                ELTDXRTN
00811      ADD +1 TO TCAR-FROM-SUB.                                     ELTDXRTN
00812                                                                   ELTDXRTN
00813  6300-TRNSFR-OTHER-RESP.                                          ELTDXRTN
00814      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTDXRTN
00815            = ZEROS OR LOW-VALUES                                  ELTDXRTN
00816           CONTINUE                                                ELTDXRTN
00817      ELSE                                                         ELTDXRTN
00818         PERFORM 6310-CONTINUE-TRNSFR-RESP                         ELTDXRTN
00819      END-IF.                                                      ELTDXRTN
00820                                                                   ELTDXRTN
00821  6310-CONTINUE-TRNSFR-RESP.                                       ELTDXRTN
00822      ADD +1     TO  WS-CIA.                                       ELTDXRTN
00823      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXRTN
00824      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXRTN
00825      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00826                       TO CMF-CODE-VALUE                           ELTDXRTN
00827      PERFORM 9650-CALL-CODES-MANUAL                               ELTDXRTN
00828      PERFORM 9700-DETERMINE-OUTPUT-METHOD.                        ELTDXRTN
00829                                                                   ELTDXRTN
00830  6400-SPILL-OVER-COINS.                                           ELTDXRTN
00831      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00832      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
00833      MOVE 1 TO WS-CIA.                                            ELTDXRTN
00834      SET PLT-INDEX2 TO 1.                                         ELTDXRTN
00835      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTDXRTN
00836        AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTDXRTN
00837          NOT  = '0'                                               ELTDXRTN
00838         MOVE WS-SPILLOVER-COINS TO COF-DTL-LINE(WS-CIA)           ELTDXRTN
00839         PERFORM 6410-CONTINUE-SPILLOVER-COINS                     ELTDXRTN
00840      ELSE                                                         ELTDXRTN
00841         SET PLT-INDEX2 TO 2                                       ELTDXRTN
00842         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO         ELTDXRTN
00843           AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)ELTDXRTN
00844             NOT  = '0'                                            ELTDXRTN
00845            MOVE WS-SPILLOVER-COINS TO COF-DTL-LINE(WS-CIA)        ELTDXRTN
00846            PERFORM 6411-CONTINUE-SPILLOVER-COINS                  ELTDXRTN
00847         END-IF                                                    ELTDXRTN
00848      END-IF.                                                      ELTDXRTN
00849                                                                   ELTDXRTN
00850  6410-CONTINUE-SPILLOVER-COINS.                                   ELTDXRTN
00851 *    ADD +1     TO  WS-CIA.                                       ELTDXRTN
00852      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXRTN
00853      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDXRTN
00854      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00855                       TO CMF-CODE-VALUE.                          ELTDXRTN
00856      PERFORM 9900-LINK-CODES-MANUAL.                              ELTDXRTN
00857      IF WS-BASIC-LOB                                              ELTDXRTN
00858          SET PROCESSING-BASIC-INFO TO TRUE                        ELTDXRTN
00859      END-IF                                                       ELTDXRTN
00860      IF CMF-RC-OK                                                 ELTDXRTN
00861         MOVE 1 TO TCAR-FROM-SUB                                   ELTDXRTN
00862         PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                 ELTDXRTN
00863             UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES             ELTDXRTN
00864                MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO              ELTDXRTN
00865                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00866                ADD 1 TO TCAR-FROM-SUB                             ELTDXRTN
00867         END-PERFORM                                               ELTDXRTN
00868         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00869         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTDXRTN
00870      END-IF.                                                      ELTDXRTN
00871                                                                   ELTDXRTN
00872                                                                   ELTDXRTN
00873  6411-CONTINUE-SPILLOVER-COINS.                                   ELTDXRTN
00874 *    ADD +1     TO  WS-CIA.                                       ELTDXRTN
00875      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDXRTN
00876      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDXRTN
00877      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00878                       TO CMF-CODE-VALUE.                          ELTDXRTN
00879      PERFORM 9900-LINK-CODES-MANUAL.                              ELTDXRTN
00880      IF WS-BASIC-LOB                                              ELTDXRTN
00881          SET PROCESSING-SUPPLEMENTAL TO TRUE                      ELTDXRTN
00882      END-IF                                                       ELTDXRTN
00883      IF CMF-RC-OK                                                 ELTDXRTN
00884         MOVE 1 TO TCAR-FROM-SUB                                   ELTDXRTN
00885         PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                 ELTDXRTN
00886             UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES             ELTDXRTN
00887                MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO              ELTDXRTN
00888                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00889                ADD 1 TO TCAR-FROM-SUB                             ELTDXRTN
00890         END-PERFORM                                               ELTDXRTN
00891         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00892         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTDXRTN
00893      END-IF.                                                      ELTDXRTN
00894                                                                   ELTDXRTN
00895  6500-SPILL-OVER-DED.                                             ELTDXRTN
00896      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
00897      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
00898      MOVE 1 TO WS-CIA.                                            ELTDXRTN
00899      SET PLT-INDEX2 TO 1.                                         ELTDXRTN
00900      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTDXRTN
00901        AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTDXRTN
00902          NOT  = '0'                                               ELTDXRTN
00903         MOVE WS-SPILLOVER-DEDBL TO COF-DTL-LINE(WS-CIA)           ELTDXRTN
00904         ADD 1 TO WS-CIA                                           ELTDXRTN
00905         IF WS-BASIC-LOB                                           ELTDXRTN
00906             SET PROCESSING-BASIC-INFO TO TRUE                     ELTDXRTN
00907         END-IF                                                    ELTDXRTN
00908         PERFORM 6510-CONTINUE-SPILLOVER-DEDUCT                    ELTDXRTN
00909      ELSE                                                         ELTDXRTN
00910         SET PLT-INDEX2 TO 2                                       ELTDXRTN
00911         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO         ELTDXRTN
00912           AND PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)ELTDXRTN
00913             NOT  = '0'                                            ELTDXRTN
00914           IF WS-BASIC-LOB                                         ELTDXRTN
00915             SET PROCESSING-SUPPLEMENTAL TO TRUE                   ELTDXRTN
00916           END-IF                                                  ELTDXRTN
00917            MOVE WS-SPILLOVER-COINS TO COF-DTL-LINE(WS-CIA)        ELTDXRTN
00918            ADD 1 TO WS-CIA                                        ELTDXRTN
00919            PERFORM 6510-CONTINUE-SPILLOVER-DEDUCT                 ELTDXRTN
00920         END-IF                                                    ELTDXRTN
00921      END-IF.                                                      ELTDXRTN
00922                                                                   ELTDXRTN
00923  6510-CONTINUE-SPILLOVER-DEDUCT.                                  ELTDXRTN
00924      ADD +1     TO  WS-CIA.                                       ELTDXRTN
00925      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDXRTN
00926      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTDXRTN
00927      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00928                       TO CMF-CODE-VALUE                           ELTDXRTN
00929      PERFORM 9900-LINK-CODES-MANUAL.                              ELTDXRTN
00930      IF CMF-RC-OK                                                 ELTDXRTN
00931         MOVE 1 TO TCAR-FROM-SUB                                   ELTDXRTN
00932         PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                 ELTDXRTN
00933             UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES             ELTDXRTN
00934                MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO              ELTDXRTN
00935                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXRTN
00936                ADD 1 TO TCAR-FROM-SUB                             ELTDXRTN
00937         END-PERFORM                                               ELTDXRTN
00938         PERFORM 9650-CALL-CODES-MANUAL                            ELTDXRTN
00939         PERFORM 9700-DETERMINE-OUTPUT-METHOD                      ELTDXRTN
00940      END-IF.                                                      ELTDXRTN
00941                                                                   ELTDXRTN
00942  6600-PROF-CHRG-HSP-CLM.                                          ELTDXRTN
00943      SET PLT-INDEX2  TO  1.                                       ELTDXRTN
00944      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTDXRTN
00945          AND PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)       ELTDXRTN
00946                       NOT  =  '0'                                 ELTDXRTN
00947               MOVE +1         TO  WS-CIA                          ELTDXRTN
00948 *             ADD  +1         TO  WS-CIA                          ELTDXRTN
00949 *             MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)           ELTDXRTN
00950               ADD  +1         TO  WS-CIA                          ELTDXRTN
00951               MOVE WS-PROF-OUTPT-CHRGES                           ELTDXRTN
00952                               TO  COF-DTL-LINE (WS-CIA)           ELTDXRTN
00953               ADD  +1         TO  WS-CIA                          ELTDXRTN
00954 *             MOVE SPACES TO COF-DTL-LINE (WS-CIA)                ELTDXRTN
00955      ELSE                                                         ELTDXRTN
00956         SET PLT-INDEX2  TO  2                                     ELTDXRTN
00957         IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO  ELTDXRTN
00958           AND PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)      ELTDXRTN
00959                          NOT  =  '0'  AND  NOT  =  LOW-VALUES     ELTDXRTN
00960               MOVE +1         TO  WS-CIA                          ELTDXRTN
00961 *             MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)           ELTDXRTN
00962 *             ADD  +1         TO  WS-CIA                          ELTDXRTN
00963               MOVE WS-PROF-OUTPT-CHRGES                           ELTDXRTN
00964                                  TO  COF-DTL-LINE (WS-CIA)        ELTDXRTN
00965               ADD  +1         TO  WS-CIA                          ELTDXRTN
00966 *             MOVE SPACES TO COF-DTL-LINE (WS-CIA).               ELTDXRTN
00967      SET PLT-INDEX2  TO  1.                                       ELTDXRTN
00968      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  ZERO       ELTDXRTN
00969          IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)        ELTDXRTN
00970                  NOT  =  '0'  AND  NOT  =  LOW-VALUES             ELTDXRTN
00971             IF WS-BASIC-LOB                                       ELTDXRTN
00972                SET PROCESSING-BASIC-INFO TO TRUE                  ELTDXRTN
00973             END-IF                                                ELTDXRTN
00974             MOVE 'BPB'         TO  CMF-RECORD-PREFIX              ELTDXRTN
00975             MOVE 'PROF-CHRG-HSP-CLM'                              ELTDXRTN
00976                                TO  CMF-ELEMENT-SYSTEM-NAME        ELTDXRTN
00977             MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00978                                TO  CMF-CODE-VALUE                 ELTDXRTN
00979            PERFORM 9650-CALL-CODES-MANUAL                         ELTDXRTN
00980            PERFORM 9700-DETERMINE-OUTPUT-METHOD.                  ELTDXRTN
00981      SET PLT-INDEX2  TO  2.                                       ELTDXRTN
00982      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  ZERO        ELTDXRTN
00983          IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)        ELTDXRTN
00984                  NOT  =  '0'  AND  NOT  =  LOW-VALUES             ELTDXRTN
00985             IF WS-BASIC-LOB                                       ELTDXRTN
00986                SET PROCESSING-SUPPLEMENTAL TO TRUE                ELTDXRTN
00987             END-IF                                                ELTDXRTN
00988             MOVE 'BPB'         TO  CMF-RECORD-PREFIX              ELTDXRTN
00989             MOVE 'PROF-CHRG-HSP-CLM'                              ELTDXRTN
00990                                TO  CMF-ELEMENT-SYSTEM-NAME        ELTDXRTN
00991             MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)    ELTDXRTN
00992                                TO  CMF-CODE-VALUE                 ELTDXRTN
00993            PERFORM 9650-CALL-CODES-MANUAL                         ELTDXRTN
00994            PERFORM 9700-DETERMINE-OUTPUT-METHOD.                  ELTDXRTN
00995      SKIP3                                                        ELTDXRTN
00996 /                                                                 ELTDXRTN
00997  6900-BEN-TAB-PVE.                                                ELTDXRTN
00998      MOVE +1 TO WS-CIA.                                           ELTDXRTN
00999      MOVE SPACES TO COF-DTL-LINE(WS-CIA).                         ELTDXRTN
01000      ADD +1 TO WS-CIA.                                            ELTDXRTN
01001      MOVE WS-PVE TO COF-DTL-LINE(WS-CIA).                         ELTDXRTN
01002      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXRTN
01003      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01004                            DFHCOMMAREA.                           ELTDXRTN
01005                                                                   ELTDXRTN
01006  7200-PAY-CONSID-TEXT.                                            ELTDXRTN
01007      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTDXRTN
01008      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
01009      MOVE  WS-PAY-CONSDR-TEXT1 TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELTDXRTN
01010      ADD 1 TO TCAR-FROM-SUB.                                      ELTDXRTN
01011      MOVE  WS-PAY-CONSDR-TEXT2 TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELTDXRTN
01012      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTDXRTN
01013      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXRTN
01014      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
01015      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDXRTN
01016      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDXRTN
01017                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDXRTN
01018      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXRTN
01019      ADD +1                TO  WS-CIA.                            ELTDXRTN
01020      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDXRTN
01021      ADD +1                TO  WS-CIA.                            ELTDXRTN
01022      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDXRTN
01023      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXRTN
01024      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01025                            DFHCOMMAREA.                           ELTDXRTN
01026      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXRTN
01027                WS-CIA                                             ELTDXRTN
01028                TCAR-FROM-SUB.                                     ELTDXRTN
01029 /                                                                 ELTDXRTN
01030 ****************************************************************  ELTDXRTN
01031 *          CALL COVERAGE MODULE                               *   ELTDXRTN
01032 ****************************************************************  ELTDXRTN
01033  8020-CALL-COVERAGE.                                              ELTDXRTN
01034      MOVE 'DIAGNOSTIC, ROUTINE SERVICE' TO  SSB-TOPIC-PHRASE.     ELTDXRTN
01035      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDXRTN
01036                     COMMAREA (DFHCOMMAREA)                        ELTDXRTN
01037                     END-EXEC.                                     ELTDXRTN
01038      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01039                            DFHCOMMAREA.                           ELTDXRTN
01040      IF PVN-COVG-NONE                                             ELTDXRTN
01041          CONTINUE                                                 ELTDXRTN
01042      ELSE                                                         ELTDXRTN
01043         MOVE +1  TO  WS-CIA                                       ELTDXRTN
01044         SET CIA-ELSPLGSW-DDN TO TRUE                              ELTDXRTN
01045         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDXRTN
01046             ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES                 ELTDXRTN
01047         INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES                    ELTDXRTN
01048         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTDXRTN
01049                       PSP-PROVN-PRICING-METHD,                    ELTDXRTN
01050                       PSP-TRANSF-OTHER-RESP-IND,                  ELTDXRTN
01051                       PSP-SPILL-OVER-COINS-APL-IND,               ELTDXRTN
01052                       PSP-SPILL-OVER-DED-APL-IND,                 ELTDXRTN
01053                       PSP-COV-QUALIF,                             ELTDXRTN
01054                       PSP-AGE-LVL-1,                              ELTDXRTN
01055                       PSP-AGE-COV-TERMN-IND,                      ELTDXRTN
01056                       PSP-AGE-COV-TERMN,                          ELTDXRTN
01057                       PSB-PROF-CHRG-HSP-CLM                       ELTDXRTN
01058         EXEC CICS LINK PROGRAM ('ELUPLGRP')                       ELTDXRTN
01059                        COMMAREA (DFHCOMMAREA)                     ELTDXRTN
01060                        END-EXEC                                   ELTDXRTN
01061         SET CIA-ELSPLGTB-DDN TO TRUE                              ELTDXRTN
01062         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDXRTN
01063             ADDRESS OF PLT-PAYMENT-LEVEL-TABLE                    ELTDXRTN
01064      END-IF.                                                      ELTDXRTN
01065      SET PROCESSING-INSTITUTIONAL TO TRUE.                        ELTDXRTN
01066                                                                   ELTDXRTN
01067 ****************************************************************  ELTDXRTN
01068 *          CALL COVERAGE MODULE                               *   ELTDXRTN
01069 ****************************************************************  ELTDXRTN
01070  9020-CALL-COVERAGE.                                              ELTDXRTN
01071      MOVE 'DIAGNOSTIC, ROUTINE SERVICES ' TO  SSB-TOPIC-PHRASE.   ELTDXRTN
01072      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTDXRTN
01073                     COMMAREA (DFHCOMMAREA)                        ELTDXRTN
01074                     END-EXEC.                                     ELTDXRTN
01075      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01076                            DFHCOMMAREA.                           ELTDXRTN
01077      IF PVN-COVG-NONE                                             ELTDXRTN
01078          CONTINUE                                                 ELTDXRTN
01079      ELSE                                                         ELTDXRTN
01080         MOVE +1  TO  WS-CIA                                       ELTDXRTN
01081         SET CIA-ELSPLGSW-DDN TO TRUE                              ELTDXRTN
01082         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDXRTN
01083             ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES                 ELTDXRTN
01084         INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES                    ELTDXRTN
01085         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTDXRTN
01086                       PSP-PROVN-PRICING-METHD,                    ELTDXRTN
01087                       PSP-TRANSF-OTHER-RESP-IND,                  ELTDXRTN
01088                       PSP-SPILL-OVER-COINS-APL-IND,               ELTDXRTN
01089                       PSP-SPILL-OVER-DED-APL-IND,                 ELTDXRTN
01090                       PSP-COV-QUALIF,                             ELTDXRTN
01091                       PSP-AGE-LVL-1,                              ELTDXRTN
01092                       PSP-AGE-COV-TERMN-IND,                      ELTDXRTN
01093                       PSP-AGE-COV-TERMN,                          ELTDXRTN
01094         EXEC CICS LINK PROGRAM ('ELUPLGRP')                       ELTDXRTN
01095                        COMMAREA (DFHCOMMAREA)                     ELTDXRTN
01096                        END-EXEC                                   ELTDXRTN
01097         SET CIA-ELSPLGTB-DDN TO TRUE                              ELTDXRTN
01098         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDXRTN
01099             ADDRESS OF PLT-PAYMENT-LEVEL-TABLE                    ELTDXRTN
01100      END-IF.                                                      ELTDXRTN
01101      SET PROCESSING-PROFESSIONAL TO TRUE.                         ELTDXRTN
01102                                                                   ELTDXRTN
01103 ****************************************************************  ELTDXRTN
01104 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTDXRTN
01105 ****************************************************************  ELTDXRTN
01106  9100-HEADER-OUTPUT-REQUEST.                                      ELTDXRTN
01107      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTDXRTN
01108      MOVE 'P'            TO  COF-FUNCTION.                        ELTDXRTN
01109                                                                   ELTDXRTN
01110      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01111                            DFHCOMMAREA.                           ELTDXRTN
01112                                                                   ELTDXRTN
01113 ****************************************************************  ELTDXRTN
01114 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTDXRTN
01115 ****************************************************************  ELTDXRTN
01116  9200-TEXT-OUTPUT-REQUEST.                                        ELTDXRTN
01117                                                                   ELTDXRTN
01118      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTDXRTN
01119      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELTDXRTN
01120      MOVE ' '     TO  COF-FUNCTION.                               ELTDXRTN
01121                                                                   ELTDXRTN
01122      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01123                            DFHCOMMAREA.                           ELTDXRTN
01124                                                                   ELTDXRTN
01125                                                                   ELTDXRTN
01126                                                                   ELTDXRTN
01127 ****************************************************************  ELTDXRTN
01128 *                                                                 ELTDXRTN
01129 * SET UP DISPLAY OF BENEFIT PROVISION LIST                        ELTDXRTN
01130 *                                                                 ELTDXRTN
01131 ****************************************************************  ELTDXRTN
01132  9500-SETUP-DSPLY-BP-LIST.                                        ELTDXRTN
01133      INITIALIZE CMF-RETURN-CODE,                                  ELTDXRTN
01134                 TCAR-FROM-AREA.                                   ELTDXRTN
01135      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXRTN
01136                       COMMAREA(DFHCOMMAREA)                       ELTDXRTN
01137                 END-EXEC.                                         ELTDXRTN
01138      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXRTN
01139      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01140          ADDRESS OF CMF-DESCR.                                    ELTDXRTN
01141      SET CMF-DESCR-IDX TO 1.                                      ELTDXRTN
01142      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXRTN
01143         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXRTN
01144          MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                   ELTDXRTN
01145             WS-PROV-DETAIL(WS-OUTPUT-IDX)                         ELTDXRTN
01146          SET WS-OUTPUT-IDX UP BY 1                                ELTDXRTN
01147      END-PERFORM.                                                 ELTDXRTN
01148                                                                   ELTDXRTN
01149                                                                   ELTDXRTN
01150  9550-COMPRESS-UNSTRING-RTN.                                      ELTDXRTN
01151 *    ADD 1 TO WS-CIA.                                             ELTDXRTN
01152      PERFORM 9600-DISPLAY-CODE-VALUES.                            ELTDXRTN
01153                                                                   ELTDXRTN
01154  9600-DISPLAY-CODE-VALUES.                                        ELTDXRTN
01155      IF WS-NO-CODES-MANUAL                                        ELTDXRTN
01156         CONTINUE                                                  ELTDXRTN
01157      ELSE                                                         ELTDXRTN
01158         PERFORM VARYING CMF-DESCR-IDX FROM                        ELTDXRTN
01159            1 BY 1 UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES       ELTDXRTN
01160               MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO               ELTDXRTN
01161                 TCAR-FROM-LINE(TCAR-FROM-SUB)                     ELTDXRTN
01162               ADD 1 TO TCAR-FROM-SUB                              ELTDXRTN
01163         END-PERFORM                                               ELTDXRTN
01164      END-IF.                                                      ELTDXRTN
01165      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTDXRTN
01166      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXRTN
01167      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXRTN
01168      PERFORM 9610-UNSTRING-TEXT.                                  ELTDXRTN
01169      PERFORM UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED        ELTDXRTN
01170 *          ADD 1 TO WS-CIA                                        ELTDXRTN
01171            IF TCAR-FROM-SUB = 1                                   ELTDXRTN
01172               EVALUATE TRUE                                       ELTDXRTN
01173                  WHEN PROCESSING-BASIC-INFO                       ELTDXRTN
01174                     ADD 1 TO WS-CIA                               ELTDXRTN
01175                     MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)         ELTDXRTN
01176                  WHEN PROCESSING-SUPPLEMENTAL                     ELTDXRTN
01177                     MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)  ELTDXRTN
01178                  WHEN OTHER                                       ELTDXRTN
01179                     CONTINUE                                      ELTDXRTN
01180               END-EVALUATE                                        ELTDXRTN
01181               ADD 1 TO WS-CIA                                     ELTDXRTN
01182               MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)       ELTDXRTN
01183            ELSE                                                   ELTDXRTN
01184               ADD 1 TO WS-CIA                                     ELTDXRTN
01185               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                ELTDXRTN
01186                   COF-DTL-LINE(WS-CIA)                            ELTDXRTN
01187            END-IF                                                 ELTDXRTN
01188            ADD 1 TO TCAR-FROM-SUB                                 ELTDXRTN
01189      END-PERFORM.                                                 ELTDXRTN
01190      IF NOT PROCESSING-BASIC-INFO                                 ELTDXRTN
01191         ADD 1 TO WS-CIA                                           ELTDXRTN
01192         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXRTN
01193      END-IF.                                                      ELTDXRTN
01194      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXRTN
01195      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01196                            DFHCOMMAREA.                           ELTDXRTN
01197      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXRTN
01198                WS-CIA                                             ELTDXRTN
01199                TCAR-FROM-SUB.                                     ELTDXRTN
01200                                                                   ELTDXRTN
01201  9610-UNSTRING-TEXT.                                              ELTDXRTN
01202      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDXRTN
01203      MOVE +65 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDXRTN
01204      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDXRTN
01205      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTDXRTN
01206      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTDXRTN
01207      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTDXRTN
01208      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTDXRTN
01209      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTDXRTN
01210      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTDXRTN
01211      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTDXRTN
01212      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTDXRTN
01213      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTDXRTN
01214      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTDXRTN
01215      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTDXRTN
01216      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTDXRTN
01217      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTDXRTN
01218      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTDXRTN
01219      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTDXRTN
01220      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTDXRTN
01221      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTDXRTN
01222      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTDXRTN
01223      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXRTN
01224                                                                   ELTDXRTN
01225  9650-CALL-CODES-MANUAL.                                          ELTDXRTN
01226      EXEC CICS  LINK PROGRAM('ELUCMIF')                           ELTDXRTN
01227                      COMMAREA (DFHCOMMAREA)                       ELTDXRTN
01228      END-EXEC.                                                    ELTDXRTN
01229      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXRTN
01230      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01231          ADDRESS OF CMF-DESCR.                                    ELTDXRTN
01232                                                                   ELTDXRTN
01233  9700-DETERMINE-OUTPUT-METHOD.                                    ELTDXRTN
01234      MOVE CMF-DESCR-LINE(1) TO WS-TEST-LINE.                      ELTDXRTN
01235      IF WS-TEST-CHAR = WS-SINGLE-QUOTE                            ELTDXRTN
01236        MOVE SPACE TO WS-TEST-CHAR                                 ELTDXRTN
01237        MOVE WS-TEST-DATA TO CMF-DESCR-LINE(1)                     ELTDXRTN
01238        PERFORM 9725-DIRECT-OUTPUT                                 ELTDXRTN
01239      ELSE                                                         ELTDXRTN
01240         PERFORM 9550-COMPRESS-UNSTRING-RTN                        ELTDXRTN
01241      END-IF.                                                      ELTDXRTN
01242                                                                   ELTDXRTN
01243  9725-DIRECT-OUTPUT.                                              ELTDXRTN
01244 *    ADD 1 TO WS-CIA.                                             ELTDXRTN
01245      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXRTN
01246      EVALUATE TRUE                                                ELTDXRTN
01247        WHEN PROCESSING-BASIC-INFO                                 ELTDXRTN
01248            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTDXRTN
01249        WHEN PROCESSING-SUPPLEMENTAL                               ELTDXRTN
01250            MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)           ELTDXRTN
01251        WHEN OTHER                                                 ELTDXRTN
01252             CONTINUE                                              ELTDXRTN
01253      END-EVALUATE.                                                ELTDXRTN
01254      PERFORM VARYING CMF-DESCR-IDX FROM 1                         ELTDXRTN
01255         BY 1 UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES            ELTDXRTN
01256        ADD 1 TO WS-CIA                                            ELTDXRTN
01257        MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                      ELTDXRTN
01258           COF-DTL-LINE(WS-CIA)                                    ELTDXRTN
01259      END-PERFORM.                                                 ELTDXRTN
01260      IF NOT PROCESSING-BASIC-INFO                                 ELTDXRTN
01261         ADD 1 TO WS-CIA                                           ELTDXRTN
01262         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXRTN
01263      END-IF.                                                      ELTDXRTN
01264      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXRTN
01265      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXRTN
01266                            DFHCOMMAREA.                           ELTDXRTN
01267      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXRTN
01268                WS-CIA                                             ELTDXRTN
01269                TCAR-FROM-SUB.                                     ELTDXRTN
01270                                                                   ELTDXRTN
01271  9900-LINK-CODES-MANUAL.                                          ELTDXRTN
01272      EXEC CICS  LINK PROGRAM('ELUCMIF')                           ELTDXRTN
01273                      COMMAREA (DFHCOMMAREA)                       ELTDXRTN
01274      END-EXEC.                                                    ELTDXRTN
01275      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXRTN
01276      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01277          ADDRESS OF CMF-DESCR.                                    ELTDXRTN
01278                                                                   ELTDXRTN
01279      TITLE ' TEXT COMPRESSION AND EXPANSION'.                     ELTDXRTN
01280      COPY ELSTCOMP.                                               ELTDXRTN
01281                                                                   ELTDXRTN
01282  9999-CHECK-CONTRACT.                                             ELTDXRTN
01283      INITIALIZE WS-LOB-SWITCH.                                    ELTDXRTN
01284      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDXRTN
01285      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01286                      ADDRESS OF CONTRACT-RECORD.                  ELTDXRTN
01287      IF CIA-RC-PTR-NULL                                           ELTDXRTN
01288         CONTINUE                                                  ELTDXRTN
01289      ELSE                                                         ELTDXRTN
01290         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXRTN
01291         IF WS-BSC-LINE                                            ELTDXRTN
01292            SET WS-BASIC-LOB TO TRUE                               ELTDXRTN
01293         END-IF                                                    ELTDXRTN
01294      END-IF.                                                      ELTDXRTN
01295      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDXRTN
01296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01297                      ADDRESS OF CONTRACT-RECORD.                  ELTDXRTN
01298      IF CIA-RC-PTR-NULL                                           ELTDXRTN
01299         CONTINUE                                                  ELTDXRTN
01300      ELSE                                                         ELTDXRTN
01301         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXRTN
01302         IF WS-BSC-LINE                                            ELTDXRTN
01303            SET WS-BASIC-LOB TO TRUE                               ELTDXRTN
01304         END-IF                                                    ELTDXRTN
01305      END-IF.                                                      ELTDXRTN
01306      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTDXRTN
01307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01308                      ADDRESS OF CONTRACT-RECORD.                  ELTDXRTN
01309      IF CIA-RC-PTR-NULL                                           ELTDXRTN
01310         CONTINUE                                                  ELTDXRTN
01311      ELSE                                                         ELTDXRTN
01312         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXRTN
01313         IF WS-BSC-LINE                                            ELTDXRTN
01314            SET WS-BASIC-LOB TO TRUE                               ELTDXRTN
01315         END-IF                                                    ELTDXRTN
01316      END-IF.                                                      ELTDXRTN
01317      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTDXRTN
01318      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXRTN
01319                      ADDRESS OF CONTRACT-RECORD.                  ELTDXRTN
01320      IF CIA-RC-PTR-NULL                                           ELTDXRTN
01321         CONTINUE                                                  ELTDXRTN
01322      ELSE                                                         ELTDXRTN
01323         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXRTN
01324         IF WS-BSC-LINE                                            ELTDXRTN
01325            SET WS-BASIC-LOB TO TRUE                               ELTDXRTN
01326         END-IF                                                    ELTDXRTN
01327      END-IF.                                                      ELTDXRTN
