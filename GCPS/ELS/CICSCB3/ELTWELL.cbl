00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELTWELL.                                          ELTWELL 
00003  AUTHOR.        ANNE KING.                                           LV001
00004  DATE-WRITTEN.  09/12/94                                          ELTWELL 
00005  DATE-COMPILED.                                                   ELTWELL 
00006                                                                   ELTWELL 
00007 ****************************************************************  ELTWELL 
00008 *      ELTWELL - ELS:  WELLNESS/PREVENTIVE CARE                *  ELTWELL 
00009 ****************************************************************  ELTWELL 
00010 ****************************************************************  ELTWELL 
00011 *              U P D A T E   H I S T O R Y                     *  ELTWELL 
00012 *                                                              *  ELTWELL 
00013 *   DATE    PGM  DESCRIPTION                                   *  ELTWELL 
00014 * --------  ---  --------------------------------------------- *  ELTWELL 
00015 * 09/12/94  AKK  ORIGINAL VERSION USED ELTDRUGS AS BASIS MADE  *  ELTWELL 
00016 *                MANY CHANGES.                                 *  ELTWELL 
00017 *                                                              *  ELTWELL 
00018 * 04/06/95  AKK  CORRECTIONS MADE DUE TO STORAGE VIOLATION.    *  ELTWELL 
00019 *                                                              *  ELTWELL 
00020 ****************************************************************  ELTWELL 
00021  ENVIRONMENT DIVISION.                                            ELTWELL 
00022  DATA DIVISION.                                                   ELTWELL 
00023                                                                   ELTWELL 
00024  WORKING-STORAGE SECTION.                                         ELTWELL 
00025  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTWELL 
00026          '** ELTWELL WS BEGINS **'.                               ELTWELL 
00027                                                                   ELTWELL 
00028  01  WS-SWITCHES.                                                 ELTWELL 
00029      05  WS-FIRST-TIME-SW        PIC X(01) VALUE SPACE.           ELTWELL 
00030          88 WS-FIRST-TIME                  VALUE 'F'.             ELTWELL 
00031          88 WS-NOT-FIRST-TIME              VALUE 'N'.             ELTWELL 
00032                                                                   ELTWELL 
00033  01  WS-MISC.                                                     ELTWELL 
00034      05  WS-BASIC                PIC X(14)                        ELTWELL 
00035                                    VALUE  '       BASIC: '.       ELTWELL 
00036      05  WS-SUPPLEMENTAL         PIC X(14)                        ELTWELL 
00037                                    VALUE  'SUPPLEMENTAL: '.       ELTWELL 
00038      05  WS-CIA                  PIC S9(03) COMP-3 VALUE +0.      ELTWELL 
00039      05  WS-SUB                  PIC S9(03) COMP-3 VALUE +0.      ELTWELL 
00040      05  WS-SUB2                 PIC S9(03) COMP-3 VALUE +0.      ELTWELL 
00041      05  WS-SUB3                 PIC S9(03) COMP-3 VALUE +0.      ELTWELL 
00042                                                                   ELTWELL 
00043      05  WS-AGE1                 PIC 9(03).                       ELTWELL 
00044      05  WS-AGE2 REDEFINES WS-AGE1.                               ELTWELL 
00045          10 WS-AGE2X             PIC 9.                           ELTWELL 
00046          10 WS-AGE2A             PIC 99.                          ELTWELL 
00047      05  WS-AGE3 REDEFINES WS-AGE2.                               ELTWELL 
00048          10 WS-AGE3X             PIC 99.                          ELTWELL 
00049          10 WS-AGE3A             PIC 9.                           ELTWELL 
00050                                                                   ELTWELL 
00051  01  WS-BP-OUTPUT-TABLE.                                          ELTWELL 
00052      03 WS-PROV-OUTPUT-LINE OCCURS 8 TIMES                        ELTWELL 
00053            INDEXED BY WS-OUTPUT-IDX.                              ELTWELL 
00054         05  FILLER            PIC X(21).                          ELTWELL 
00055         05  WS-PROV-DETAIL    PIC X(58).                          ELTWELL 
00056                                                                   ELTWELL 
00057 /--------------------------------------------------------------*  ELTWELL 
00058 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTWELL 
00059 *--------------------------------------------------------------*  ELTWELL 
00060  01  WS-BEN-PROV-IDS.                                             ELTWELL 
00061      05  WS-TABLE-MAX-CNT        PIC S9(04)  VALUE +8 COMP.       ELTWELL 
00062      05  WS-LIST-BP-CNT          PIC S9(04) VALUE +0 COMP.        ELTWELL 
00063      05  WS-INST-OP-CNT          PIC S9(04) VALUE +7 COMP.        ELTWELL 
00064      05  WS-INST-OP-TABS.                                         ELTWELL 
00065          10  FILLER              PIC  X(06) VALUE 'IMM  B'.       ELTWELL 
00066          10  FILLER              PIC  X(06) VALUE 'PXO  B'.       ELTWELL 
00067          10  FILLER              PIC  X(06) VALUE 'RDMP B'.       ELTWELL 
00068          10  FILLER              PIC  X(06) VALUE 'RLAB B'.       ELTWELL 
00069          10  FILLER              PIC  X(06) VALUE 'RMAM B'.       ELTWELL 
00070          10  FILLER              PIC  X(06) VALUE 'RPAP B'.       ELTWELL 
00071          10  FILLER              PIC  X(06) VALUE 'RXRY B'.       ELTWELL 
00072      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTWELL 
00073                                  PIC  X(06) OCCURS 7 TIMES.       ELTWELL 
00074                                                                   ELTWELL 
00075                                                                   ELTWELL 
00076      05  WS-PROF-OP-CNT          PIC S9(04) VALUE +8 COMP.        ELTWELL 
00077      05  WS-PROF-OP-TABS.                                         ELTWELL 
00078          10  FILLER              PIC  X(06) VALUE 'IMM  E'.       ELTWELL 
00079          10  FILLER              PIC  X(06) VALUE 'PXO  E'.       ELTWELL 
00080          10  FILLER              PIC  X(06) VALUE 'RDMP E'.       ELTWELL 
00081          10  FILLER              PIC  X(06) VALUE 'RLAB E'.       ELTWELL 
00082          10  FILLER              PIC  X(06) VALUE 'RMAM E'.       ELTWELL 
00083          10  FILLER              PIC  X(06) VALUE 'RPAP E'.       ELTWELL 
00084          10  FILLER              PIC  X(06) VALUE 'RXRY E'.       ELTWELL 
00085          10  FILLER              PIC  X(06) VALUE 'WBCO E'.       ELTWELL 
00086      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTWELL 
00087                                  PIC  X(06) OCCURS 8 TIMES.       ELTWELL 
00088                                                                   ELTWELL 
00089 /***************************************************************  ELTWELL 
00090 *              HEADER AND LITERAL TEXT AREA                    *  ELTWELL 
00091 ****************************************************************  ELTWELL 
00092  01  WS-HEADER-LINE-I.                                            ELTWELL 
00093      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTWELL 
00094      05  FILLER                  PIC  X(40) VALUE                 ELTWELL 
00095              'WELLNESS/PREVENTATIVE CARE INSTITUTIONAL'.          ELTWELL 
00096      05  FILLER                  PIC  X(21) VALUE SPACES.         ELTWELL 
00097                                                                   ELTWELL 
00098  01  WS-HEADER-LINE-P.                                            ELTWELL 
00099      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTWELL 
00100      05  FILLER                  PIC  X(39) VALUE                 ELTWELL 
00101              'WELLNESS/PREVENTATIVE CARE PROFESSIONAL'.           ELTWELL 
00102      05  FILLER                  PIC  X(22) VALUE SPACES.         ELTWELL 
00103                                                                   ELTWELL 
00104  01  WS-SERVICES-RENDERED.                                        ELTWELL 
00105      05  FILLER                  PIC  X(26) VALUE                 ELTWELL 
00106              'SERVICES MAY BE RENDERED: '.                        ELTWELL 
00107      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTWELL 
00108                                                                   ELTWELL 
00109  01  WS-FOLLOWING-BEN.                                            ELTWELL 
00110      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00111            'COVERED SERVICES ARE:'.                               ELTWELL 
00112                                                                   ELTWELL 
00113  01  WS-COV-QUAL.                                                 ELTWELL 
00114      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00115            'THE PATIENT IS ELIGIBLE FOR THIS BENEFIT: '.          ELTWELL 
00116                                                                   ELTWELL 
00117  01  WS-COV-QUAL1.                                                ELTWELL 
00118      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00119            'UNTIL THE END OF THE YEAR THE AGE OF '.               ELTWELL 
00120                                                                   ELTWELL 
00121  01  WS-COV-QUAL1A.                                               ELTWELL 
00122      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00123            'IS REACHED.'.                                         ELTWELL 
00124                                                                   ELTWELL 
00125  01  WS-COV-QUAL2.                                                ELTWELL 
00126      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00127            'AFTER REACHING THE AGE OF '.                          ELTWELL 
00128                                                                   ELTWELL 
00129  01  WS-AGE-TERM.                                                 ELTWELL 
00130      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00131         'ELIGIBILITY FOR THIS BENEFIT TERMINATES WHEN THE PATIENT ELTWELL 
00132 -       'REACHES: '.                                              ELTWELL 
00133                                                                   ELTWELL 
00134  01  WS-AGE-TERMA.                                                ELTWELL 
00135      05  FILLER                  PIC  X(79) VALUE                 ELTWELL 
00136            'THE AGE OF '.                                         ELTWELL 
00137                                                                   ELTWELL 
00138  01  WS-PAY-CONSDR-TEXT1.                                         ELTWELL 
00139      05  FILLER                  PIC X(49)                        ELTWELL 
00140        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTWELL 
00141                                                                   ELTWELL 
00142  01  WS-PAY-CONSDR-TEXT2.                                         ELTWELL 
00143      05  FILLER                  PIC X(45)                        ELTWELL 
00144        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTWELL 
00145                                                                   ELTWELL 
00146  01  WS-PAYABLE-AS.                                               ELTWELL 
00147      10  FILLER                  PIC  X(45) VALUE                 ELTWELL 
00148              'THESE SERVICES ARE PRICED ACCORDING TO: '.          ELTWELL 
00149      10  FILLER                  PIC  X(34) VALUE LOW-VALUES.     ELTWELL 
00150                                                                   ELTWELL 
00151  01  WS-CONTRACT-RELATED.                                         ELTWELL 
00152      05  FILLER                  PIC  X(48) VALUE                 ELTWELL 
00153              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTWELL 
00154                                                                   ELTWELL 
00155  01  WS-BASICX.                                                   ELTWELL 
00156      05  WS-BASIC-LIT            PIC  X(14) VALUE                 ELTWELL 
00157              '       BASIC: '.                                    ELTWELL 
00158      05  WS-DTL-BASIC-LONG.                                       ELTWELL 
00159          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTWELL 
00160          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTWELL 
00161  01  WS-BASIC-A.                                                  ELTWELL 
00162      05  FILLER                  PIC X(14) VALUE SPACES.          ELTWELL 
00163      05  WS-DTL-BASIC-A          PIC X(65) VALUE SPACES.          ELTWELL 
00164                                                                   ELTWELL 
00165  01  WS-SUPPLEMENTAL-A.                                           ELTWELL 
00166      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTWELL 
00167              '  SUPPLEMENTAL: '.                                  ELTWELL 
00168      05  WS-DTL-SUPP-LONG.                                        ELTWELL 
00169          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTWELL 
00170          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTWELL 
00171                                                                   ELTWELL 
00172  01  WS-PVE.                                                      ELTWELL 
00173      05  FILLER                  PIC  X(44) VALUE                 ELTWELL 
00174              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTWELL 
00175      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTWELL 
00176                                                                   ELTWELL 
00177  01  WS-INDICES-PROBLEM.                                          ELTWELL 
00178      05  FILLER                  PIC  X(20) VALUE                 ELTWELL 
00179              'PROBLEM WITH INDICES'.                              ELTWELL 
00180      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTWELL 
00181                                                                   ELTWELL 
00182  01  WS-POSSIBLE-ERROR.                                           ELTWELL 
00183      05  FILLER                   PIC  X(50) VALUE                ELTWELL 
00184              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTWELL 
00185      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTWELL 
00186                                                                   ELTWELL 
00187  01  WS-INVALID-REQ.                                              ELTWELL 
00188      05  FILLER                  PIC  X(37) VALUE                 ELTWELL 
00189              '*** I N V A L I D   R E Q U E S T ***'.             ELTWELL 
00190      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTWELL 
00191                                                                   ELTWELL 
00192  01  WS-SPILLOVER.                                                ELTWELL 
00193      05  FILLER                  PIC  X(10) VALUE                 ELTWELL 
00194              'SPILLOVER '.                                        ELTWELL 
00195                                                                   ELTWELL 
00196  01  WS-OTHER-LITERALS.                                           ELTWELL 
00197    05  WS-NO-TABULAR1.                                            ELTWELL 
00198      10  FILLER                    PIC X(51)  VALUE               ELTWELL 
00199         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTWELL 
00200      10  FILLER                    PIC X(22)  VALUE               ELTWELL 
00201         'GOING FROM BENEFIT ***'.                                 ELTWELL 
00202                                                                   ELTWELL 
00203    05  WS-NO-TABULAR2.                                            ELTWELL 
00204      10  FILLER                    PIC X(15)  VALUE               ELTWELL 
00205         '*** PROVISION: '.                                        ELTWELL 
00206      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTWELL 
00207      10  FILLER                    PIC X VALUE SPACE.             ELTWELL 
00208      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTWELL 
00209      10  FILLER                    PIC X(13)  VALUE               ELTWELL 
00210         ' TO TABULAR: '.                                          ELTWELL 
00211      10  WS-NO-TAB-ID              PIC X(6).                      ELTWELL 
00212      10  FILLER                    PIC X VALUE SPACE.             ELTWELL 
00213      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTWELL 
00214      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTWELL 
00215                                                                   ELTWELL 
00216    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTWELL 
00217    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTWELL 
00218      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTWELL 
00219                                                                   ELTWELL 
00220      TITLE 'LINKAGE SECTION'.                                     ELTWELL 
00221  LINKAGE SECTION.                                                 ELTWELL 
00222  01  DFHCOMMAREA.                                                 ELTWELL 
00223      COPY ELSCOMMC.                                               ELTWELL 
00224                                                                   ELTWELL 
00225      COPY ELSCIA2C.                                               ELTWELL 
00226                                                                   ELTWELL 
00227      COPY ELSIOPMC.                                               ELTWELL 
00228                                                                   ELTWELL 
00229      COPY ELSKEYSC.                                               ELTWELL 
00230                                                                   ELTWELL 
00231      COPY ELSOUTPC.                                               ELTWELL 
00232                                                                   ELTWELL 
00233      COPY ELSSSCBC.                                               ELTWELL 
00234                                                                   ELTWELL 
00235      COPY ELSCMIFC.                                               ELTWELL 
00236                                                                   ELTWELL 
00237      COPY ELSCMDSC.                                               ELTWELL 
00238                                                                   ELTWELL 
00239      COPY ELSPRVNC.                                               ELTWELL 
00240                                                                   ELTWELL 
00241      COPY ELSTCWAC.                                               ELTWELL 
00242                                                                   ELTWELL 
00243      COPY ELSPLGSW.                                               ELTWELL 
00244                                                                   ELTWELL 
00245      COPY ELSPLGTB.                                               ELTWELL 
00246                                                                   ELTWELL 
00247 /                                                                 ELTWELL 
00248  PROCEDURE DIVISION.                                              ELTWELL 
00249  0000-MAINLINE.                                                   ELTWELL 
00250      PERFORM 1000-INITIALIZATION.                                 ELTWELL 
00251      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTWELL 
00252          PERFORM 2000-INSTITUTIONAL-OP.                           ELTWELL 
00253      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTWELL 
00254          PERFORM 4000-PROFESSIONAL-OP.                            ELTWELL 
00255                                                                   ELTWELL 
00256      MOVE 'E'   TO  COF-FUNCTION.                                 ELTWELL 
00257      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTWELL 
00258                                                                   ELTWELL 
00259      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00260                            DFHCOMMAREA.                           ELTWELL 
00261      GOBACK.                                                      ELTWELL 
00262                                                                   ELTWELL 
00263  1000-INITIALIZATION.                                             ELTWELL 
00264                                                                   ELTWELL 
00265      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTWELL 
00266         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTWELL 
00267         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTWELL 
00268                                                                   ELTWELL 
00269      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTWELL 
00270          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTWELL 
00271                                                                   ELTWELL 
00272      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTWELL 
00273      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00274          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTWELL 
00275                                                                   ELTWELL 
00276      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTWELL 
00277      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00278          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTWELL 
00279                                                                   ELTWELL 
00280      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTWELL 
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00282          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTWELL 
00283                                                                   ELTWELL 
00284      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTWELL 
00285      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00286          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTWELL 
00287                                                                   ELTWELL 
00288      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTWELL 
00289      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00290          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTWELL 
00291                                                                   ELTWELL 
00292      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTWELL 
00293      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTWELL 
00294              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTWELL 
00295                                                                   ELTWELL 
00296      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTWELL 
00297      SET CIA-STG-GETMAIN  TO TRUE.                                ELTWELL 
00298      EXEC CICS LINK                                               ELTWELL 
00299                PROGRAM('ELUSTGMG')                                ELTWELL 
00300                COMMAREA(DFHCOMMAREA)                              ELTWELL 
00301      END-EXEC.                                                    ELTWELL 
00302      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTWELL 
00303      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00304          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTWELL 
00305                                                                   ELTWELL 
00306  2000-INSTITUTIONAL-OP.                                           ELTWELL 
00307      MOVE WS-HEADER-LINE-I  TO  COF-HDR-LINE (2).                 ELTWELL 
00308      PERFORM 9100-HEADER-OUTPUT-REQUEST.                          ELTWELL 
00309      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTWELL 
00310      PERFORM 2010-MOVE-IN-INST-OP-TABS                            ELTWELL 
00311           VARYING WS-SUB FROM +1 BY +1                            ELTWELL 
00312               UNTIL WS-SUB  >  WS-INST-OP-CNT.                    ELTWELL 
00313      PERFORM 8020-CALL-COVERAGE.                                  ELTWELL 
00314      IF PVN-COVG-NONE                                             ELTWELL 
00315         CONTINUE                                                  ELTWELL 
00316      ELSE                                                         ELTWELL 
00317         INITIALIZE WS-BP-OUTPUT-TABLE                             ELTWELL 
00318         SET WS-OUTPUT-IDX TO 1                                    ELTWELL 
00319         PERFORM 2030-FIND-FIRST-NONZERO                           ELTWELL 
00320              VARYING WS-SUB FROM +1 BY +1                         ELTWELL 
00321                 UNTIL   WS-SUB  > WS-INST-OP-CNT                  ELTWELL 
00322      END-IF.                                                      ELTWELL 
00323 /                                                                 ELTWELL 
00324  2010-MOVE-IN-INST-OP-TABS.                                       ELTWELL 
00325      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTWELL 
00326      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTWELL 
00327                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTWELL 
00328                                                                   ELTWELL 
00329      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTWELL 
00330                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTWELL 
00331                                                                   ELTWELL 
00332                                                                   ELTWELL 
00333  2030-FIND-FIRST-NONZERO.                                         ELTWELL 
00334      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTWELL 
00335      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTWELL 
00336          CONTINUE                                                 ELTWELL 
00337      ELSE                                                         ELTWELL 
00338          PERFORM 2100-BUILD-SCREEN-LINES                          ELTWELL 
00339      END-IF.                                                      ELTWELL 
00340 /                                                                 ELTWELL 
00341  2100-BUILD-SCREEN-LINES.                                         ELTWELL 
00342      SET PLT-INDEX1  TO                                           ELTWELL 
00343              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTWELL 
00344      MOVE  +1  TO  WS-CIA.                                        ELTWELL 
00345      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTWELL 
00346          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTWELL 
00347              SET PLT-INDEX2  TO  2                                ELTWELL 
00348              MOVE WS-INST-OP-CNT TO WS-LIST-BP-CNT                ELTWELL 
00349              PERFORM 5000-CREATE-DETAIL-DISPLAY                   ELTWELL 
00350          ELSE                                                     ELTWELL 
00351              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTWELL 
00352              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTWELL 
00353      ELSE                                                         ELTWELL 
00354          SET PLT-INDEX2  TO  1                                    ELTWELL 
00355          MOVE WS-INST-OP-CNT TO WS-LIST-BP-CNT                    ELTWELL 
00356          PERFORM 5000-CREATE-DETAIL-DISPLAY                       ELTWELL 
00357      END-IF.                                                      ELTWELL 
00358 /                                                                 ELTWELL 
00359 ****************************************************************  ELTWELL 
00360 *   DRUGS/MEDICATIONS PROFESSIONAL INPATIENT PROCESSING        *  ELTWELL 
00361 ****************************************************************  ELTWELL 
00362  4000-PROFESSIONAL-OP.                                            ELTWELL 
00363      MOVE WS-HEADER-LINE-P  TO  COF-HDR-LINE (2).                 ELTWELL 
00364      PERFORM 9100-HEADER-OUTPUT-REQUEST.                          ELTWELL 
00365      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTWELL 
00366      PERFORM 4010-MOVE-IN-PROF-OP-TABS                            ELTWELL 
00367           VARYING WS-SUB FROM +1 BY +1                            ELTWELL 
00368               UNTIL WS-SUB  >  WS-PROF-OP-CNT.                    ELTWELL 
00369      PERFORM 8020-CALL-COVERAGE.                                  ELTWELL 
00370      IF PVN-COVG-NONE                                             ELTWELL 
00371         CONTINUE                                                  ELTWELL 
00372      ELSE                                                         ELTWELL 
00373         INITIALIZE WS-BP-OUTPUT-TABLE                             ELTWELL 
00374         SET WS-OUTPUT-IDX TO 1                                    ELTWELL 
00375         PERFORM 4030-FIND-FIRST-NONZERO                           ELTWELL 
00376              VARYING WS-SUB FROM +1 BY +1                         ELTWELL 
00377                 UNTIL   WS-SUB  > WS-PROF-OP-CNT                  ELTWELL 
00378      END-IF.                                                      ELTWELL 
00379 /                                                                 ELTWELL 
00380  4010-MOVE-IN-PROF-OP-TABS.                                       ELTWELL 
00381      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTWELL 
00382      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTWELL 
00383                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTWELL 
00384      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTWELL 
00385                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTWELL 
00386                                                                   ELTWELL 
00387  4030-FIND-FIRST-NONZERO.                                         ELTWELL 
00388      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTWELL 
00389                                                                   ELTWELL 
00390      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTWELL 
00391          NEXT SENTENCE                                            ELTWELL 
00392      ELSE                                                         ELTWELL 
00393          PERFORM 4100-BUILD-SCREEN-LINES                          ELTWELL 
00394      END-IF.                                                      ELTWELL 
00395                                                                   ELTWELL 
00396 /                                                                 ELTWELL 
00397  4100-BUILD-SCREEN-LINES.                                         ELTWELL 
00398      SET PLT-INDEX1  TO                                           ELTWELL 
00399              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTWELL 
00400      MOVE  +1  TO  WS-CIA.                                        ELTWELL 
00401      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTWELL 
00402          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTWELL 
00403              SET PLT-INDEX2  TO  2                                ELTWELL 
00404              MOVE WS-PROF-OP-CNT TO WS-LIST-BP-CNT                ELTWELL 
00405              PERFORM 5000-CREATE-DETAIL-DISPLAY                   ELTWELL 
00406          ELSE                                                     ELTWELL 
00407              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTWELL 
00408              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTWELL 
00409      ELSE                                                         ELTWELL 
00410          SET PLT-INDEX2  TO  1                                    ELTWELL 
00411          MOVE WS-PROF-OP-CNT TO WS-LIST-BP-CNT                    ELTWELL 
00412          PERFORM 5000-CREATE-DETAIL-DISPLAY                       ELTWELL 
00413      END-IF.                                                      ELTWELL 
00414                                                                   ELTWELL 
00415  5000-CREATE-DETAIL-DISPLAY.                                      ELTWELL 
00416      PERFORM 6105-LIST-BEN-PROV.                                  ELTWELL 
00417      PERFORM 6150-COV-QUALIFIER.                                  ELTWELL 
00418      PERFORM 6175-AGE-TERM.                                       ELTWELL 
00419      PERFORM 6110-PLACE-OF-TREATMENT.                             ELTWELL 
00420      PERFORM 6120-PRIC-METH.                                      ELTWELL 
00421      PERFORM 6200-PAY-CONSID-TEXT.                                ELTWELL 
00422                                                                   ELTWELL 
00423  6105-LIST-BEN-PROV.                                              ELTWELL 
00424      MOVE  +2               TO  WS-CIA.                           ELTWELL 
00425      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTWELL 
00426      ADD  +1               TO  WS-CIA.                            ELTWELL 
00427      MOVE ZERO              TO  WS-SUB2.                          ELTWELL 
00428      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTWELL 
00429                             TO  WS-SUB3.                          ELTWELL 
00430      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTWELL 
00431         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTWELL 
00432            UNTIL   PVN-BEN-PROVN-IDX > WS-LIST-BP-CNT.            ELTWELL 
00433      PERFORM 6108-DISPLAY-BP-LIST.                                ELTWELL 
00434                                                                   ELTWELL 
00435      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTWELL 
00436      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTWELL 
00437      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00438                            DFHCOMMAREA.                           ELTWELL 
00439      MOVE 1 TO WS-CIA.                                            ELTWELL 
00440      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTWELL 
00441      INITIALIZE WS-BP-OUTPUT-TABLE.                               ELTWELL 
00442      SET WS-OUTPUT-IDX TO 1.                                      ELTWELL 
00443                                                                   ELTWELL 
00444  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTWELL 
00445      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTWELL 
00446          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTWELL 
00447          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTWELL 
00448          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTWELL 
00449                            TO  CMF-CODE-VALUE                     ELTWELL 
00450          PERFORM 9500-SETUP-DSPLY-BP-LIST                         ELTWELL 
00451          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTWELL 
00452          ADD  +1    TO  WS-SUB2.                                  ELTWELL 
00453                                                                   ELTWELL 
00454                                                                   ELTWELL 
00455 ****************************************************************  ELTWELL 
00456 *    DISPLAY BENEFIT PROVISION LIST                            *  ELTWELL 
00457 ****************************************************************  ELTWELL 
00458  6108-DISPLAY-BP-LIST.                                            ELTWELL 
00459      PERFORM VARYING WS-OUTPUT-IDX FROM 1 BY 1 UNTIL              ELTWELL 
00460         WS-OUTPUT-IDX > WS-LIST-BP-CNT OR                         ELTWELL 
00461         (WS-OUTPUT-IDX > WS-SUB2)                                 ELTWELL 
00462             OR WS-PROV-OUTPUT-LINE(WS-OUTPUT-IDX) = SPACES        ELTWELL 
00463           MOVE WS-PROV-OUTPUT-LINE (WS-OUTPUT-IDX) TO             ELTWELL 
00464              COF-DTL-LINE (WS-CIA)                                ELTWELL 
00465           ADD 1 TO WS-CIA                                         ELTWELL 
00466           IF WS-CIA > 20  OR WS-CIA = 20                          ELTWELL 
00467              MOVE WS-CIA    TO  COF-NBR-DTL-LINES                 ELTWELL 
00468              CALL 'ELUOUTPT' USING DFHEIBLK                       ELTWELL 
00469                                    DFHCOMMAREA                    ELTWELL 
00470              MOVE 1 TO WS-CIA                                     ELTWELL 
00471              INITIALIZE WS-BP-OUTPUT-TABLE                        ELTWELL 
00472              SET WS-OUTPUT-IDX TO 1                               ELTWELL 
00473           END-IF                                                  ELTWELL 
00474      END-PERFORM.                                                 ELTWELL 
00475                                                                   ELTWELL 
00476 ****************************************************************  ELTWELL 
00477 *              P L A C E   O F   T R E A T M E N T             *  ELTWELL 
00478 ****************************************************************  ELTWELL 
00479  6110-PLACE-OF-TREATMENT.                                         ELTWELL 
00480      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTWELL 
00481      MOVE 1 TO TCAR-FROM-SUB.                                     ELTWELL 
00482      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00483      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTWELL 
00484         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTWELL 
00485               NOT =  ZERO                                         ELTWELL 
00486          MOVE +2                    TO  WS-CIA                    ELTWELL 
00487          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)     ELTWELL 
00488          ADD +1 TO WS-CIA.                                        ELTWELL 
00489                                                                   ELTWELL 
00490      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00491      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTWELL 
00492         AND   PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTWELL 
00493               NOT  =  ZERO                                        ELTWELL 
00494          MOVE +2                    TO  WS-CIA                    ELTWELL 
00495          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)     ELTWELL 
00496          ADD +1 TO WS-CIA.                                        ELTWELL 
00497                                                                   ELTWELL 
00498      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00499      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTWELL 
00500         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTWELL 
00501               NOT  =  ZERO                                        ELTWELL 
00502          MOVE WS-BASIC  TO TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00503          ADD +1 TO TCAR-FROM-SUB                                  ELTWELL 
00504          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTWELL 
00505          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTWELL 
00506                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTWELL 
00507          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTWELL 
00508                TO  CMF-CODE-VALUE                                 ELTWELL 
00509          PERFORM 9650-CALL-CODES-MANUAL                           ELTWELL 
00510          PERFORM 9600-DISPLAY-CODE-VALUES.                        ELTWELL 
00511                                                                   ELTWELL 
00512      SET PLT-INDEX2  TO  2.                                       ELTWELL 
00513      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTWELL 
00514         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTWELL 
00515              NOT  =  ZERO                                         ELTWELL 
00516          MOVE WS-SUPPLEMENTAL                                     ELTWELL 
00517                     TO TCAR-FROM-LINE(TCAR-FROM-SUB)              ELTWELL 
00518          ADD +1 TO TCAR-FROM-SUB                                  ELTWELL 
00519          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTWELL 
00520          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTWELL 
00521                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTWELL 
00522          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTWELL 
00523               TO  CMF-CODE-VALUE                                  ELTWELL 
00524          PERFORM 9650-CALL-CODES-MANUAL                           ELTWELL 
00525          PERFORM 9600-DISPLAY-CODE-VALUES.                        ELTWELL 
00526                                                                   ELTWELL 
00527      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTWELL 
00528         SET PLT-INDEX2  TO  2                                     ELTWELL 
00529      ELSE                                                         ELTWELL 
00530         SET PLT-INDEX2  TO  1.                                    ELTWELL 
00531                                                                   ELTWELL 
00532 ****************************************************************  ELTWELL 
00533 *  P R O V I S I O N   P R I C I N G   M E T H O D             *  ELTWELL 
00534 *                                                              *  ELTWELL 
00535 ****************************************************************  ELTWELL 
00536  6120-PRIC-METH.                                                  ELTWELL 
00537      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTWELL 
00538      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWELL 
00539      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00540      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTWELL 
00541         AND  PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTWELL 
00542              NOT = '19'                                           ELTWELL 
00543         MOVE +2             TO  WS-CIA                            ELTWELL 
00544         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTWELL 
00545         ADD +1 TO WS-CIA.                                         ELTWELL 
00546                                                                   ELTWELL 
00547      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00548      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO     ELTWELL 
00549           AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTWELL 
00550               NOT = '19'                                          ELTWELL 
00551         MOVE +2             TO  WS-CIA                            ELTWELL 
00552         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTWELL 
00553         ADD +1 TO WS-CIA.                                         ELTWELL 
00554                                                                   ELTWELL 
00555                                                                   ELTWELL 
00556      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00557      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTWELL 
00558         AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)       ELTWELL 
00559             NOT = ZERO AND  NOT =  '19'                           ELTWELL 
00560         MOVE WS-BASIC  TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00561         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00562         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTWELL 
00563         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTWELL 
00564         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTWELL 
00565              TO  CMF-CODE-VALUE                                   ELTWELL 
00566         PERFORM 9650-CALL-CODES-MANUAL                            ELTWELL 
00567         PERFORM 9600-DISPLAY-CODE-VALUES.                         ELTWELL 
00568                                                                   ELTWELL 
00569      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00570      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTWELL 
00571         AND   PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)     ELTWELL 
00572               NOT = ZERO AND  NOT =  '19'                         ELTWELL 
00573         MOVE WS-SUPPLEMENTAL                                      ELTWELL 
00574                        TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00575         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00576         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTWELL 
00577         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTWELL 
00578         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTWELL 
00579              TO CMF-CODE-VALUE                                    ELTWELL 
00580         PERFORM 9650-CALL-CODES-MANUAL                            ELTWELL 
00581         PERFORM 9600-DISPLAY-CODE-VALUES.                         ELTWELL 
00582                                                                   ELTWELL 
00583      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTWELL 
00584         SET PLT-INDEX2  TO  2                                     ELTWELL 
00585      ELSE                                                         ELTWELL 
00586         SET PLT-INDEX2  TO  1.                                    ELTWELL 
00587                                                                   ELTWELL 
00588 ****************************************************************  ELTWELL 
00589 *  COVERAGE QUALIFIER AND AGE LEVEL 1                          *  ELTWELL 
00590 *                                                              *  ELTWELL 
00591 ****************************************************************  ELTWELL 
00592  6150-COV-QUALIFIER.                                              ELTWELL 
00593      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTWELL 
00594      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWELL 
00595      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00596      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTWELL 
00597        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTWELL 
00598         MOVE +2             TO  WS-CIA                            ELTWELL 
00599         MOVE WS-COV-QUAL  TO  COF-DTL-LINE(WS-CIA)                ELTWELL 
00600         ADD +1 TO WS-CIA.                                         ELTWELL 
00601                                                                   ELTWELL 
00602      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00603      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO     ELTWELL 
00604        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTWELL 
00605         MOVE +2             TO  WS-CIA                            ELTWELL 
00606         MOVE WS-COV-QUAL  TO  COF-DTL-LINE(WS-CIA)                ELTWELL 
00607         ADD +1 TO WS-CIA.                                         ELTWELL 
00608                                                                   ELTWELL 
00609      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00610      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTWELL 
00611        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTWELL 
00612         MOVE WS-BASIC  TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00613         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00614         PERFORM 6155-SETUP-COV-QUAL-PHRASE                        ELTWELL 
00615         PERFORM 6157-SETUP-AGE-LEVEL                              ELTWELL 
00616         IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'         ELTWELL 
00617            MOVE WS-COV-QUAL1A TO TCAR-FROM-LINE(TCAR-FROM-SUB)    ELTWELL 
00618            ADD +1 TO TCAR-FROM-SUB                                ELTWELL 
00619         END-IF                                                    ELTWELL 
00620         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTWELL 
00621      END-IF.                                                      ELTWELL 
00622                                                                   ELTWELL 
00623      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00624      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTWELL 
00625        AND PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) > ZEROES        ELTWELL 
00626         MOVE WS-SUPPLEMENTAL                                      ELTWELL 
00627                        TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00628         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00629         PERFORM 6155-SETUP-COV-QUAL-PHRASE                        ELTWELL 
00630         PERFORM 6157-SETUP-AGE-LEVEL                              ELTWELL 
00631         IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'         ELTWELL 
00632            MOVE WS-COV-QUAL1A TO TCAR-FROM-LINE(TCAR-FROM-SUB)    ELTWELL 
00633            ADD +1 TO TCAR-FROM-SUB                                ELTWELL 
00634         END-IF                                                    ELTWELL 
00635         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTWELL 
00636      END-IF.                                                      ELTWELL 
00637                                                                   ELTWELL 
00638      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTWELL 
00639         SET PLT-INDEX2  TO  2                                     ELTWELL 
00640      ELSE                                                         ELTWELL 
00641         SET PLT-INDEX2  TO  1.                                    ELTWELL 
00642                                                                   ELTWELL 
00643  6155-SETUP-COV-QUAL-PHRASE.                                      ELTWELL 
00644      IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '001'            ELTWELL 
00645         MOVE WS-COV-QUAL1 TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTWELL 
00646      ELSE                                                         ELTWELL 
00647         IF PLP-COV-QUALIF(PLT-INDEX1, PLT-INDEX2) = '002'         ELTWELL 
00648            MOVE WS-COV-QUAL2 TO TCAR-FROM-LINE(TCAR-FROM-SUB)     ELTWELL 
00649      END-IF.                                                      ELTWELL 
00650      ADD 1 TO TCAR-FROM-SUB.                                      ELTWELL 
00651                                                                   ELTWELL 
00652  6157-SETUP-AGE-LEVEL.                                            ELTWELL 
00653      MOVE PLP-AGE-LVL-1(PLT-INDEX1, PLT-INDEX2) TO WS-AGE1.       ELTWELL 
00654      EVALUATE TRUE                                                ELTWELL 
00655         WHEN WS-AGE3X = ZEROES                                    ELTWELL 
00656            MOVE WS-AGE3A                                          ELTWELL 
00657               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00658         WHEN WS-AGE2X = ZERO                                      ELTWELL 
00659            MOVE WS-AGE2A                                          ELTWELL 
00660               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00661         WHEN OTHER                                                ELTWELL 
00662            MOVE WS-AGE1                                           ELTWELL 
00663               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00664      END-EVALUATE.                                                ELTWELL 
00665      ADD +1 TO TCAR-FROM-SUB.                                     ELTWELL 
00666                                                                   ELTWELL 
00667 ****************************************************************  ELTWELL 
00668 *  AGE TERMINATION INDICATOR                                   *  ELTWELL 
00669 *                                                              *  ELTWELL 
00670 ****************************************************************  ELTWELL 
00671  6175-AGE-TERM.                                                   ELTWELL 
00672      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTWELL 
00673      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWELL 
00674      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00675      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZERO           ELTWELL 
00676        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTWELL 
00677                   > ZEROES                                        ELTWELL 
00678         MOVE +2             TO  WS-CIA                            ELTWELL 
00679         MOVE WS-AGE-TERM  TO  COF-DTL-LINE(WS-CIA)                ELTWELL 
00680         ADD +1 TO WS-CIA.                                         ELTWELL 
00681                                                                   ELTWELL 
00682      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00683      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)       NOT =  ZERO     ELTWELL 
00684        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTWELL 
00685                  > ZEROES                                         ELTWELL 
00686         MOVE +2             TO  WS-CIA                            ELTWELL 
00687         MOVE WS-AGE-TERM  TO  COF-DTL-LINE(WS-CIA)                ELTWELL 
00688         ADD +1 TO WS-CIA.                                         ELTWELL 
00689                                                                   ELTWELL 
00690      SET  PLT-INDEX2  TO  1.                                      ELTWELL 
00691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTWELL 
00692        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTWELL 
00693                        > ZEROES                                   ELTWELL 
00694         MOVE WS-BASIC  TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00695         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00696         MOVE WS-AGE-TERMA TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTWELL 
00697         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00698         PERFORM 6179-SETUP-TERMINATION-AGE                        ELTWELL 
00699         PERFORM 6177-SETUP-AGE-TERM-PHRASE                        ELTWELL 
00700         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTWELL 
00701      END-IF.                                                      ELTWELL 
00702                                                                   ELTWELL 
00703      SET  PLT-INDEX2  TO  2.                                      ELTWELL 
00704      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTWELL 
00705        AND PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2)          ELTWELL 
00706                        > ZEROES                                   ELTWELL 
00707         MOVE WS-SUPPLEMENTAL                                      ELTWELL 
00708                        TO  TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00709         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00710         MOVE WS-AGE-TERMA TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELTWELL 
00711         ADD +1 TO TCAR-FROM-SUB                                   ELTWELL 
00712         PERFORM 6179-SETUP-TERMINATION-AGE                        ELTWELL 
00713         PERFORM 6177-SETUP-AGE-TERM-PHRASE                        ELTWELL 
00714         PERFORM 9600-DISPLAY-CODE-VALUES                          ELTWELL 
00715      END-IF.                                                      ELTWELL 
00716                                                                   ELTWELL 
00717      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTWELL 
00718         SET PLT-INDEX2  TO  2                                     ELTWELL 
00719      ELSE                                                         ELTWELL 
00720         SET PLT-INDEX2  TO  1.                                    ELTWELL 
00721                                                                   ELTWELL 
00722  6177-SETUP-AGE-TERM-PHRASE.                                      ELTWELL 
00723      EVALUATE TRUE                                                ELTWELL 
00724         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '0'  ELTWELL 
00725            CONTINUE                                               ELTWELL 
00726         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '1'  ELTWELL 
00727            MOVE 'YEARS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00728         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '2'  ELTWELL 
00729            MOVE 'WEEKS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)          ELTWELL 
00730         WHEN PLP-AGE-COV-TERMN-IND(PLT-INDEX1, PLT-INDEX2) = '3'  ELTWELL 
00731            MOVE 'DAYS' TO TCAR-FROM-LINE(TCAR-FROM-SUB)           ELTWELL 
00732      END-EVALUATE.                                                ELTWELL 
00733      ADD 1 TO TCAR-FROM-SUB.                                      ELTWELL 
00734                                                                   ELTWELL 
00735  6179-SETUP-TERMINATION-AGE.                                      ELTWELL 
00736      MOVE PLP-AGE-COV-TERMN(PLT-INDEX1, PLT-INDEX2) TO WS-AGE1.   ELTWELL 
00737      EVALUATE TRUE                                                ELTWELL 
00738         WHEN WS-AGE3X = ZEROES                                    ELTWELL 
00739            MOVE WS-AGE3A                                          ELTWELL 
00740               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00741         WHEN WS-AGE2X = ZERO                                      ELTWELL 
00742            MOVE WS-AGE2A                                          ELTWELL 
00743               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00744         WHEN OTHER                                                ELTWELL 
00745            MOVE WS-AGE1                                           ELTWELL 
00746               TO  TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTWELL 
00747      END-EVALUATE.                                                ELTWELL 
00748      ADD +1 TO TCAR-FROM-SUB.                                     ELTWELL 
00749                                                                   ELTWELL 
00750  6200-PAY-CONSID-TEXT.                                            ELTWELL 
00751      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELTWELL 
00752      MOVE 1 TO TCAR-FROM-SUB.                                     ELTWELL 
00753      MOVE  WS-PAY-CONSDR-TEXT1 TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELTWELL 
00754      ADD 1 TO TCAR-FROM-SUB.                                      ELTWELL 
00755      MOVE  WS-PAY-CONSDR-TEXT2 TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELTWELL 
00756      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTWELL 
00757      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTWELL 
00758      MOVE 1 TO TCAR-FROM-SUB.                                     ELTWELL 
00759      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTWELL 
00760      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTWELL 
00761                                TCAR-OUTPUT-FIELD-2-LEN.           ELTWELL 
00762      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTWELL 
00763      ADD +1                TO  WS-CIA.                            ELTWELL 
00764      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTWELL 
00765      ADD +1                TO  WS-CIA.                            ELTWELL 
00766      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTWELL 
00767      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTWELL 
00768      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00769                            DFHCOMMAREA.                           ELTWELL 
00770      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTWELL 
00771                WS-CIA                                             ELTWELL 
00772                TCAR-FROM-SUB.                                     ELTWELL 
00773                                                                   ELTWELL 
00774 ****************************************************************  ELTWELL 
00775 *          CALL COVERAGE MODULE                               *   ELTWELL 
00776 ****************************************************************  ELTWELL 
00777  8020-CALL-COVERAGE.                                              ELTWELL 
00778      MOVE 'WELLNESS/PREVENTIVE' TO  SSB-TOPIC-PHRASE.             ELTWELL 
00779      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTWELL 
00780                     COMMAREA (DFHCOMMAREA)                        ELTWELL 
00781                     END-EXEC.                                     ELTWELL 
00782      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00783                            DFHCOMMAREA.                           ELTWELL 
00784      IF PVN-COVG-NONE                                             ELTWELL 
00785          CONTINUE                                                 ELTWELL 
00786      ELSE                                                         ELTWELL 
00787         MOVE +1  TO  WS-CIA                                       ELTWELL 
00788         SET CIA-ELSPLGSW-DDN TO TRUE                              ELTWELL 
00789         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTWELL 
00790             ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES                 ELTWELL 
00791         INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES                    ELTWELL 
00792         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTWELL 
00793                       PSP-PROVN-PRICING-METHD,                    ELTWELL 
00794                       PSP-COV-QUALIF,                             ELTWELL 
00795                       PSP-AGE-LVL-1,                              ELTWELL 
00796                       PSP-AGE-COV-TERMN-IND,                      ELTWELL 
00797                       PSP-AGE-COV-TERMN                           ELTWELL 
00798         EXEC CICS LINK PROGRAM ('ELUPLGRP')                       ELTWELL 
00799                        COMMAREA (DFHCOMMAREA)                     ELTWELL 
00800                        END-EXEC                                   ELTWELL 
00801         SET CIA-ELSPLGTB-DDN TO TRUE                              ELTWELL 
00802         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTWELL 
00803             ADDRESS OF PLT-PAYMENT-LEVEL-TABLE                    ELTWELL 
00804      END-IF.                                                      ELTWELL 
00805                                                                   ELTWELL 
00806 ****************************************************************  ELTWELL 
00807 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTWELL 
00808 ****************************************************************  ELTWELL 
00809  9100-HEADER-OUTPUT-REQUEST.                                      ELTWELL 
00810      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTWELL 
00811      MOVE 'P'            TO  COF-FUNCTION.                        ELTWELL 
00812                                                                   ELTWELL 
00813      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00814                            DFHCOMMAREA.                           ELTWELL 
00815                                                                   ELTWELL 
00816 ****************************************************************  ELTWELL 
00817 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTWELL 
00818 ****************************************************************  ELTWELL 
00819  9200-TEXT-OUTPUT-REQUEST.                                        ELTWELL 
00820                                                                   ELTWELL 
00821      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTWELL 
00822      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELTWELL 
00823      MOVE ' '     TO  COF-FUNCTION.                               ELTWELL 
00824                                                                   ELTWELL 
00825      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00826                            DFHCOMMAREA.                           ELTWELL 
00827                                                                   ELTWELL 
00828                                                                   ELTWELL 
00829                                                                   ELTWELL 
00830 ****************************************************************  ELTWELL 
00831 *                                                                 ELTWELL 
00832 * SET UP DISPLAY OF BENEFIT PROVISION LIST                        ELTWELL 
00833 *                                                                 ELTWELL 
00834 ****************************************************************  ELTWELL 
00835  9500-SETUP-DSPLY-BP-LIST.                                        ELTWELL 
00836      INITIALIZE CMF-RETURN-CODE,                                  ELTWELL 
00837                 TCAR-FROM-AREA.                                   ELTWELL 
00838      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTWELL 
00839                       COMMAREA(DFHCOMMAREA)                       ELTWELL 
00840                 END-EXEC.                                         ELTWELL 
00841      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWELL 
00842      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00843          ADDRESS OF CMF-DESCR.                                    ELTWELL 
00844      SET CMF-DESCR-IDX TO 1.                                      ELTWELL 
00845      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTWELL 
00846         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTWELL 
00847          MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                   ELTWELL 
00848             WS-PROV-DETAIL(WS-OUTPUT-IDX)                         ELTWELL 
00849          SET WS-OUTPUT-IDX UP BY 1                                ELTWELL 
00850      END-PERFORM.                                                 ELTWELL 
00851                                                                   ELTWELL 
00852                                                                   ELTWELL 
00853  9600-DISPLAY-CODE-VALUES.                                        ELTWELL 
00854      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTWELL 
00855      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTWELL 
00856      MOVE 1 TO TCAR-FROM-SUB.                                     ELTWELL 
00857      PERFORM 9610-UNSTRING-TEXT.                                  ELTWELL 
00858      PERFORM UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED        ELTWELL 
00859            IF TCAR-FROM-SUB = 1                                   ELTWELL 
00860               MOVE TCAR-OPF-DATA(1) TO                            ELTWELL 
00861                   WS-DTL-BASIC-A                                  ELTWELL 
00862               MOVE WS-BASIC-A TO                                  ELTWELL 
00863                   COF-DTL-LINE(WS-CIA)                            ELTWELL 
00864            ELSE                                                   ELTWELL 
00865               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                ELTWELL 
00866                   COF-DTL-LINE(WS-CIA)                            ELTWELL 
00867            END-IF                                                 ELTWELL 
00868            ADD 1 TO TCAR-FROM-SUB                                 ELTWELL 
00869                     WS-CIA                                        ELTWELL 
00870      END-PERFORM.                                                 ELTWELL 
00871      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTWELL 
00872      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTWELL 
00873                            DFHCOMMAREA.                           ELTWELL 
00874      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTWELL 
00875                WS-CIA                                             ELTWELL 
00876                TCAR-FROM-SUB.                                     ELTWELL 
00877                                                                   ELTWELL 
00878  9610-UNSTRING-TEXT.                                              ELTWELL 
00879      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTWELL 
00880      MOVE +65 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTWELL 
00881      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTWELL 
00882      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTWELL 
00883      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTWELL 
00884      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTWELL 
00885      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTWELL 
00886      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTWELL 
00887      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTWELL 
00888      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTWELL 
00889      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTWELL 
00890      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTWELL 
00891      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTWELL 
00892      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTWELL 
00893      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTWELL 
00894      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTWELL 
00895      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTWELL 
00896      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTWELL 
00897      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTWELL 
00898      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTWELL 
00899      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTWELL 
00900      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTWELL 
00901                                                                   ELTWELL 
00902  9650-CALL-CODES-MANUAL.                                          ELTWELL 
00903      EXEC CICS  LINK PROGRAM('ELUCMIF')                           ELTWELL 
00904                      COMMAREA (DFHCOMMAREA)                       ELTWELL 
00905      END-EXEC.                                                    ELTWELL 
00906      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWELL 
00907      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWELL 
00908          ADDRESS OF CMF-DESCR.                                    ELTWELL 
00909      PERFORM VARYING CMF-DESCR-IDX FROM                           ELTWELL 
00910         1 BY 1 UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES          ELTWELL 
00911            MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                  ELTWELL 
00912              TCAR-FROM-LINE(TCAR-FROM-SUB)                        ELTWELL 
00913            ADD 1 TO TCAR-FROM-SUB                                 ELTWELL 
00914      END-PERFORM.                                                 ELTWELL 
00915                                                                   ELTWELL 
00916                                                                   ELTWELL 
00917      TITLE ' TEXT COMPRESSION AND EXPANSION'.                     ELTWELL 
00918      COPY ELSTCOMP.                                               ELTWELL 
00919                                                                   ELTWELL 
