00001 *      LAST MAINTENANCE TIME: 10.19.09  DATE: 07/21/86            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTORTHO
00003  PROGRAM-ID.    ELTORTHO.                                            LV002
00004  AUTHOR.        LUCY TORRES.                                      ELTORTHO
00005  DATE-WRITTEN.  07/18/86                                          ELTORTHO
00006  DATE-COMPILED.                                                   ELTORTHO
00007                                                                   ELTORTHO
00008 ****************************************************************  ELTORTHO
00009 *      ELTORTHO - ELS:  ORTHOTIC TOPIC PROGRAM                 *  ELTORTHO
00010 ****************************************************************  ELTORTHO
00011                                                                   ELTORTHO
00012 ****************************************************************  ELTORTHO
00013 *              U P D A T E   H I S T O R Y                     *  ELTORTHO
00014 *                                                              *  ELTORTHO
00015 *   DATE    PGM  DESCRIPTION                                   *  ELTORTHO
00016 * --------  ---  --------------------------------------------- *  ELTORTHO
00017 * 07/18/86  LET  ORIGINAL VERSION                              *  ELTORTHO
00018 * 08/13/86  LET  USING A HEADER LINE FROM THE PROLOG           *  ELTORTHO
00019 * 10/07/86  NAC  VS COBOL II CONVERSION.                       *  ELTORTHO
00020 * 11/26/86  LET  CHANGED CODE TO ACCOMMODATE THE MOVING OF THE *  ELTORTHO
00021 *                CERTIFICATION REQUIREMENT INDICATOR FROM THE  *  ELTORTHO
00022 *                FORMAT TYPE SECTION TO THE COMMON SECTION.    *  ELTORTHO
00023 * 03/21/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS               *  ELTORTHO
00024 * 10/20/89  RKH  ADDED TRANSFER TO OTHER RESPONSIBILITY IND    *  ELTORTHO
00025 *                                                                 ELTORTHO
00026 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY INELTORTHO
00027 *                      FROM A SINGLE POSITION TO ZEROS            ELTORTHO
00028 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTORTHO
00029 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTORTHO
00030 *                                                                 ELTORTHO
00031 ****************************************************************  ELTORTHO
00032                                                                   ELTORTHO
00033  ENVIRONMENT DIVISION.                                            ELTORTHO
00034                                                                   ELTORTHO
00035  DATA DIVISION.                                                   ELTORTHO
00036  WORKING-STORAGE SECTION.                                         ELTORTHO
00037  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTORTHO
00038          '** ELTORTHO WS BEGINS **'.                              ELTORTHO
00039 /                                                                 ELTORTHO
00040 ****************************************************************  ELTORTHO
00041 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTORTHO
00042 ****************************************************************  ELTORTHO
00043  01  WORK-FIELDS.                                                 ELTORTHO
00044      05  WS-HEX-00               PIC  X(01).                      ELTORTHO
00045      05  WS-CHAR-0               PIC  X(01).                      ELTORTHO
00046      05  WS-SUB                  PIC S9(03) COMP VALUE +0.        ELTORTHO
00047      05  WS-SUB1                 PIC S9(03) COMP VALUE +0.        ELTORTHO
00048      05  WS-SUB2                 PIC S9(03) COMP VALUE +0.        ELTORTHO
00049      05  WS-SUB3                 PIC S9(03) COMP VALUE +0.        ELTORTHO
00050      05  WS-SUB4                 PIC S9(03) COMP VALUE +0.        ELTORTHO
00051      05  WS-CIA                  PIC S9(03) COMP VALUE +0.        ELTORTHO
00052      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP.                 ELTORTHO
00053      05  WS-PERCENT-FLD.                                          ELTORTHO
00054        10  WS-PERCENTAGE         PIC ZZ9.                         ELTORTHO
00055        10  WS-PERCENT-SIGN       PIC X.                           ELTORTHO
00056      05  WS-EXPLANATION-IND      PIC S9 COMP.                     ELTORTHO
00057          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTORTHO
00058          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTORTHO
00059          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTORTHO
00060          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTORTHO
00061          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTORTHO
00062          88  WS-NO-EXPLANATION             VALUE +0.              ELTORTHO
00063      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP.                     ELTORTHO
00064      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP.                     ELTORTHO
00065                                                                   ELTORTHO
00066  01  WS-EXPLAINS.                                                 ELTORTHO
00067    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTORTHO
00068    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTORTHO
00069    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTORTHO
00070    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTORTHO
00071                                                                   ELTORTHO
00072  01  SWITCHES.                                                    ELTORTHO
00073      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTORTHO
00074          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTORTHO
00075      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTORTHO
00076          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTORTHO
00077      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTORTHO
00078          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTORTHO
00079      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTORTHO
00080          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTORTHO
00081                                                                   ELTORTHO
00082 *--------------------------------------------------------------*  ELTORTHO
00083 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTORTHO
00084 *--------------------------------------------------------------*  ELTORTHO
00085  01  TABLE-MAX                   PIC S9(03) VALUE +1 COMP.        ELTORTHO
00086 * 1  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTORTHO
00087                                                                   ELTORTHO
00088  01  WS-BEN-PROV-IDS.                                             ELTORTHO
00089      05  WS-INST-IP-CNT          PIC S9(03) VALUE +1 COMP.        ELTORTHO
00090      05  WS-INST-IP-TABS.                                         ELTORTHO
00091          10  FILLER              PIC  X(06) VALUE 'OSI  B'.       ELTORTHO
00092      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTORTHO
00093                                  PIC  X(06) OCCURS 1 TIMES.       ELTORTHO
00094                                                                   ELTORTHO
00095      05  WS-INST-OP-CNT          PIC S9(03) VALUE +1 COMP.        ELTORTHO
00096      05  WS-INST-OP-TABS.                                         ELTORTHO
00097          10  FILLER              PIC  X(06) VALUE 'OSO  B'.       ELTORTHO
00098      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTORTHO
00099                                  PIC  X(06) OCCURS 1 TIMES.       ELTORTHO
00100                                                                   ELTORTHO
00101      05  WS-PROF-IP-CNT          PIC S9(03) VALUE +1 COMP.        ELTORTHO
00102      05  WS-PROF-IP-TABS.                                         ELTORTHO
00103          10  FILLER              PIC  X(06) VALUE 'OSI  E'.       ELTORTHO
00104      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTORTHO
00105                                  PIC  X(06) OCCURS 1 TIMES.       ELTORTHO
00106                                                                   ELTORTHO
00107      05  WS-PROF-OP-CNT          PIC S9(03) VALUE +1 COMP.        ELTORTHO
00108      05  WS-PROF-OP-TABS.                                         ELTORTHO
00109          10  FILLER              PIC  X(06) VALUE 'OSO  E'.       ELTORTHO
00110      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTORTHO
00111                                  PIC  X(06) OCCURS 1 TIMES.       ELTORTHO
00112                                                                   ELTORTHO
00113 ****************************************************************  ELTORTHO
00114 *              HEADER AND LITERAL TEXT AREA                    *  ELTORTHO
00115 ****************************************************************  ELTORTHO
00116  01  HEADER-I-IP-LINE-3.                                          ELTORTHO
00117      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTORTHO
00118      05  FILLER                  PIC  X(42) VALUE                 ELTORTHO
00119              'ORTHOTIC APPLIANCE INSTITUTIONAL INPATIENT'.        ELTORTHO
00120      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTORTHO
00121                                                                   ELTORTHO
00122  01  HEADER-I-OP-LINE-3.                                          ELTORTHO
00123      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTORTHO
00124      05  FILLER                  PIC  X(43) VALUE                 ELTORTHO
00125              'ORTHOTIC APPLIANCE INSTITUTIONAL OUTPATIENT'.       ELTORTHO
00126      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTORTHO
00127                                                                   ELTORTHO
00128  01  HEADER-P-IP-LINE-3.                                          ELTORTHO
00129      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTORTHO
00130      05  FILLER                  PIC  X(41) VALUE                 ELTORTHO
00131              'ORTHOTIC APPLIANCE PROFESSIONAL INPATIENT'.         ELTORTHO
00132      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTORTHO
00133                                                                   ELTORTHO
00134  01  HEADER-P-OP-LINE-3.                                          ELTORTHO
00135      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTORTHO
00136      05  FILLER                  PIC  X(42) VALUE                 ELTORTHO
00137              'ORTHOTIC APPLIANCE PROFESSIONAL OUTPATIENT'.        ELTORTHO
00138      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTORTHO
00139                                                                   ELTORTHO
00140  01  WS-SERVICES-RENDERED.                                        ELTORTHO
00141      05  FILLER                  PIC  X(26) VALUE                 ELTORTHO
00142              'SERVICES MAY BE RENDERED: '.                        ELTORTHO
00143      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTORTHO
00144                                                                   ELTORTHO
00145  01  WS-FOLLOWING-BEN.                                            ELTORTHO
00146      05  FILLER                  PIC  X(22) VALUE                 ELTORTHO
00147            'COVERED SERVICES ARE: '.                              ELTORTHO
00148                                                                   ELTORTHO
00149  01  WS-PAYABLE-AS.                                               ELTORTHO
00150      10  FILLER                  PIC  X(40) VALUE                 ELTORTHO
00151          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTORTHO
00152                                                                   ELTORTHO
00153  01  WS-CONTRACT-RELATED.                                         ELTORTHO
00154      05  FILLER                  PIC  X(48) VALUE                 ELTORTHO
00155              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTORTHO
00156                                                                   ELTORTHO
00157  01  WS-CERT-REQ.                                                 ELTORTHO
00158      05  FILLER                   PIC  X(47) VALUE                ELTORTHO
00159              'THE CERIFICATION REQUIRED FOR THIS SERVICE IS: '.   ELTORTHO
00160      05  FILLER                   PIC  X(32) VALUE LOW-VALUES.    ELTORTHO
00161                                                                   ELTORTHO
00162  01  WS-RECERT-REQ.                                               ELTORTHO
00163      05  FILLER                   PIC  X(56) VALUE                ELTORTHO
00164              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTORTHO
00165 -            'IS: '.                                              ELTORTHO
00166      05  FILLER                   PIC  X(23) VALUE LOW-VALUES.    ELTORTHO
00167                                                                   ELTORTHO
00168  01  WS-RESTRICT.                                                 ELTORTHO
00169      05  FILLER                  PIC  X(63) VALUE                 ELTORTHO
00170              'THE RESTRICTIONS FOR REPAIR/REPLACEMENT OF THIS APPLELTORTHO
00171 -            'IANCE IS: '.                                        ELTORTHO
00172      05  FILLER                  PIC  X(16) VALUE LOW-VALUES.     ELTORTHO
00173                                                                   ELTORTHO
00174  01  WS-BASIC.                                                    ELTORTHO
00175      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTORTHO
00176              '         BASIC: '.                                  ELTORTHO
00177      05  WS-DTL-BASIC-LONG.                                       ELTORTHO
00178          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTORTHO
00179          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTORTHO
00180                                                                   ELTORTHO
00181  01  WS-SUPPLEMENTAL.                                             ELTORTHO
00182      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTORTHO
00183              '  SUPPLEMENTAL: '.                                  ELTORTHO
00184      05  WS-DTL-SUPP-LONG.                                        ELTORTHO
00185          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTORTHO
00186          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTORTHO
00187                                                                   ELTORTHO
00188  01  WS-PVE.                                                      ELTORTHO
00189      05  FILLER                  PIC  X(44) VALUE                 ELTORTHO
00190              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTORTHO
00191      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTORTHO
00192                                                                   ELTORTHO
00193  01  WS-ACCUM-MSG1.                                               ELTORTHO
00194      05  FILLER                  PIC  X(79) VALUE                 ELTORTHO
00195      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTORTHO
00196 -    'CONSIDERATIONS.'.                                           ELTORTHO
00197                                                                   ELTORTHO
00198  01  WS-INDICES-PROBLEM.                                          ELTORTHO
00199      05  FILLER                  PIC  X(20) VALUE                 ELTORTHO
00200              'PROBLEM WITH INDICES'.                              ELTORTHO
00201      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTORTHO
00202                                                                   ELTORTHO
00203  01  WS-POSSIBLE-ERROR.                                           ELTORTHO
00204      05  FILLER                   PIC  X(50) VALUE                ELTORTHO
00205              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTORTHO
00206      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTORTHO
00207                                                                   ELTORTHO
00208  01  WS-INVALID-REQ.                                              ELTORTHO
00209      05  FILLER                  PIC  X(37) VALUE                 ELTORTHO
00210              '*** I N V A L I D   R E Q U E S T ***'.             ELTORTHO
00211      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTORTHO
00212                                                                   ELTORTHO
00213  01  WS-SPILLOVER.                                                ELTORTHO
00214      05  FILLER                  PIC  X(10) VALUE                 ELTORTHO
00215              'SPILLOVER '.                                        ELTORTHO
00216                                                                   ELTORTHO
00217  01  WS-OTHER-LITERALS.                                           ELTORTHO
00218    05  WS-NO-TABULAR1.                                            ELTORTHO
00219      10  FILLER                    PIC X(51)  VALUE               ELTORTHO
00220         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTORTHO
00221      10  FILLER                    PIC X(22)  VALUE               ELTORTHO
00222         'GOING FROM BENEFIT ***'.                                 ELTORTHO
00223                                                                   ELTORTHO
00224    05  WS-NO-TABULAR2.                                            ELTORTHO
00225      10  FILLER                    PIC X(15)  VALUE               ELTORTHO
00226         '*** PROVISION: '.                                        ELTORTHO
00227      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTORTHO
00228      10  FILLER                    PIC X VALUE SPACE.             ELTORTHO
00229      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTORTHO
00230      10  FILLER                    PIC X(13)  VALUE               ELTORTHO
00231         ' TO TABULAR: '.                                          ELTORTHO
00232      10  WS-NO-TAB-ID              PIC X(6).                      ELTORTHO
00233      10  FILLER                    PIC X VALUE SPACE.             ELTORTHO
00234      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTORTHO
00235      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTORTHO
00236                                                                   ELTORTHO
00237    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTORTHO
00238    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTORTHO
00239      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTORTHO
00240                                                                   ELTORTHO
00241 /                                                                 ELTORTHO
00242  LINKAGE SECTION.                                                 ELTORTHO
00243  01  DFHCOMMAREA.                                                 ELTORTHO
00244      COPY ELSCOMMC.                                               ELTORTHO
00245 /                                                                 ELTORTHO
00246      COPY ELSCIA2C.                                               ELTORTHO
00247 /                                                                 ELTORTHO
00248      COPY ELSIOPMC.                                               ELTORTHO
00249 /                                                                 ELTORTHO
00250      COPY ELSKEYSC.                                               ELTORTHO
00251 /                                                                 ELTORTHO
00252      COPY ELSOUTPC.                                               ELTORTHO
00253 /                                                                 ELTORTHO
00254      COPY ELSSSCBC.                                               ELTORTHO
00255 /                                                                 ELTORTHO
00256      COPY ELSCMIFC.                                               ELTORTHO
00257 /                                                                 ELTORTHO
00258      COPY ELSCMDSC.                                               ELTORTHO
00259 /                                                                 ELTORTHO
00260      COPY ELSPRVNC.                                               ELTORTHO
00261 /                                                                 ELTORTHO
00262 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTORTHO
00263      COPY ELSPLGSW.                                               ELTORTHO
00264 *** BENEFIT PROVISION TABLE OF FLDS                               ELTORTHO
00265      COPY ELSPLGTB.                                               ELTORTHO
00266 /                                                                 ELTORTHO
00267      COPY ELSTCWAC.                                               ELTORTHO
00268 /                                                                 ELTORTHO
00269 /                                                                 ELTORTHO
00270  PROCEDURE DIVISION.                                              ELTORTHO
00271  0000-MAINLINE.                                                   ELTORTHO
00272                                                                   ELTORTHO
00273      PERFORM 1000-INITIALIZATION                                  ELTORTHO
00274         THRU 1000-EXIT.                                           ELTORTHO
00275                                                                   ELTORTHO
00276      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTORTHO
00277                       AND                                         ELTORTHO
00278         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTORTHO
00279          PERFORM 2000-INSTITUTIONAL-IP THRU 2000-EXIT.            ELTORTHO
00280                                                                   ELTORTHO
00281      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTORTHO
00282                       AND                                         ELTORTHO
00283         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTORTHO
00284          PERFORM 3000-INSTITUTIONAL-OP THRU 3000-EXIT.            ELTORTHO
00285                                                                   ELTORTHO
00286      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTORTHO
00287                       AND                                         ELTORTHO
00288         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTORTHO
00289          PERFORM 4000-PROFESSIONAL-IP THRU 4000-EXIT.             ELTORTHO
00290                                                                   ELTORTHO
00291      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTORTHO
00292                       AND                                         ELTORTHO
00293         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTORTHO
00294          PERFORM 5000-PROFESSIONAL-OP THRU 5000-EXIT.             ELTORTHO
00295                                                                   ELTORTHO
00296      IF (SSB-PROV-CLASS-INST OR                                   ELTORTHO
00297          SSB-PROV-CLASS-BOTH OR                                   ELTORTHO
00298          SSB-PROV-CLASS-PROF)                                     ELTORTHO
00299                         AND                                       ELTORTHO
00300         (SSB-SERV-CLASS-IP   OR                                   ELTORTHO
00301          SSB-SERV-CLASS-OP   OR                                   ELTORTHO
00302          SSB-SERV-CLASS-BOTH)                                     ELTORTHO
00303            CONTINUE                                               ELTORTHO
00304      ELSE                                                         ELTORTHO
00305          SET CIA-AB-UNDEF TO TRUE                                 ELTORTHO
00306          EXEC CICS ABEND                                          ELTORTHO
00307                    ABCODE(CIA-ABCODE)                             ELTORTHO
00308          END-EXEC                                                 ELTORTHO
00309      END-IF.                                                      ELTORTHO
00310                                                                   ELTORTHO
00311                                                                   ELTORTHO
00312      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTORTHO
00313                                                                   ELTORTHO
00314      SET CIA-STG-FREEMAIN TO TRUE.                                ELTORTHO
00315      EXEC CICS LINK                                               ELTORTHO
00316                PROGRAM('ELUSTGMG')                                ELTORTHO
00317                COMMAREA(DFHCOMMAREA)                              ELTORTHO
00318      END-EXEC.                                                    ELTORTHO
00319                                                                   ELTORTHO
00320      MOVE 'E'   TO  COF-FUNCTION.                                 ELTORTHO
00321      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTORTHO
00322                                                                   ELTORTHO
00323      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
00324                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
00325                     END-EXEC.                                     ELTORTHO
00326                                                                   ELTORTHO
00327      EXEC CICS RETURN END-EXEC.                                   ELTORTHO
00328                                                                   ELTORTHO
00329      GOBACK.                                                      ELTORTHO
00330 /                                                                 ELTORTHO
00331  1000-INITIALIZATION.                                             ELTORTHO
00332 ****************************************************************  ELTORTHO
00333 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTORTHO
00334 ****************************************************************  ELTORTHO
00335                                                                   ELTORTHO
00336      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTORTHO
00337          EXEC CICS ABEND                                          ELTORTHO
00338                    ABCODE ('EL01')                                ELTORTHO
00339          END-EXEC                                                 ELTORTHO
00340      END-IF.                                                      ELTORTHO
00341                                                                   ELTORTHO
00342 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTORTHO
00343                                                                   ELTORTHO
00344      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTORTHO
00345          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTORTHO
00346                                                                   ELTORTHO
00347      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTORTHO
00348      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00349          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTORTHO
00350                                                                   ELTORTHO
00351      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTORTHO
00352      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00353          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTORTHO
00354                                                                   ELTORTHO
00355      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTORTHO
00356      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00357          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTORTHO
00358                                                                   ELTORTHO
00359      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTORTHO
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00361          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTORTHO
00362                                                                   ELTORTHO
00363      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTORTHO
00364      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00365          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTORTHO
00366                                                                   ELTORTHO
00367      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTORTHO
00368      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00369          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTORTHO
00370                                                                   ELTORTHO
00371      SET  CIA-ELSPRVN-DDN TO TRUE.                                ELTORTHO
00372                                                                   ELTORTHO
00373      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTORTHO
00374              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTORTHO
00375                                                                   ELTORTHO
00376      SET CIA-STG-GETMAIN TO TRUE.                                 ELTORTHO
00377      EXEC CICS LINK                                               ELTORTHO
00378                PROGRAM('ELUSTGMG')                                ELTORTHO
00379                COMMAREA(DFHCOMMAREA)                              ELTORTHO
00380      END-EXEC.                                                    ELTORTHO
00381                                                                   ELTORTHO
00382      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTORTHO
00383      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00384          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTORTHO
00385                                                                   ELTORTHO
00386                                                                   ELTORTHO
00387      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTORTHO
00388                                                                   ELTORTHO
00389  1000-EXIT.  EXIT.                                                ELTORTHO
00390 /                                                                 ELTORTHO
00391 ****************************************************************  ELTORTHO
00392 *       ORTHOTICS INSTITUTIONAL INPATIENT PROCESSING           *  ELTORTHO
00393 ****************************************************************  ELTORTHO
00394  2000-INSTITUTIONAL-IP.                                           ELTORTHO
00395                                                                   ELTORTHO
00396      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTORTHO
00397                                                                   ELTORTHO
00398      MOVE HEADER-I-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTORTHO
00399                                                                   ELTORTHO
00400      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTORTHO
00401         THRU 9100-EXIT.                                           ELTORTHO
00402                                                                   ELTORTHO
00403      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
00404      PERFORM WITH TEST BEFORE                                     ELTORTHO
00405              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTORTHO
00406              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTORTHO
00407         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTORTHO
00408         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTORTHO
00409         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTORTHO
00410         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTORTHO
00411      END-PERFORM.                                                 ELTORTHO
00412      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
00413                                                                   ELTORTHO
00414                                                                   ELTORTHO
00415      PERFORM WITH TEST BEFORE                                     ELTORTHO
00416         VARYING WS-SUB FROM +1 BY +1                              ELTORTHO
00417         UNTIL   WS-SUB  >     WS-INST-IP-CNT                      ELTORTHO
00418           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTORTHO
00419           MOVE WS-INST-IP-BP (WS-SUB)                             ELTORTHO
00420                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTORTHO
00421            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTORTHO
00422                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTORTHO
00423      END-PERFORM.                                                 ELTORTHO
00424                                                                   ELTORTHO
00425      MOVE 'ORTHOTIC APPLIANCES      '   TO  SSB-TOPIC-PHRASE.     ELTORTHO
00426                                                                   ELTORTHO
00427      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTORTHO
00428                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
00429                     END-EXEC.                                     ELTORTHO
00430                                                                   ELTORTHO
00431      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
00432                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
00433                     END-EXEC.                                     ELTORTHO
00434                                                                   ELTORTHO
00435      IF PVN-COVG-NONE                                             ELTORTHO
00436          GO TO 2000-EXIT.                                         ELTORTHO
00437                                                                   ELTORTHO
00438      MOVE +1  TO  WS-CIA.                                         ELTORTHO
00439                                                                   ELTORTHO
00440      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTORTHO
00441                    PSP-PROVN-PRICING-METHD,                       ELTORTHO
00442                    PSP-TRANSF-OTHER-RESP-IND,                     ELTORTHO
00443                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTORTHO
00444                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTORTHO
00445                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTORTHO
00446                    PSP-SPILL-OVER-DED-APL-IND,                    ELTORTHO
00447                    PSP-CERTFN-REQRM-IND,                          ELTORTHO
00448                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTORTHO
00449                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTORTHO
00450                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTORTHO
00451                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTORTHO
00452                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTORTHO
00453                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTORTHO
00454                    PSB-CERTN-REPETN-REQRD-IND,                    ELTORTHO
00455                    PSB-REPR-REPLAC-RESTRN-IND.                    ELTORTHO
00456                                                                   ELTORTHO
00457      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTORTHO
00458                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
00459                     END-EXEC.                                     ELTORTHO
00460                                                                   ELTORTHO
00461      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTORTHO
00462      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
00463          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTORTHO
00464                                                                   ELTORTHO
00465      PERFORM WITH TEST BEFORE                                     ELTORTHO
00466         VARYING WS-SUB  FROM  +1  BY  +1                          ELTORTHO
00467         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTORTHO
00468              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTORTHO
00469              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTORTHO
00470                   PERFORM 2100-BUILD-SCREEN-LINES THRU 2100-EXIT  ELTORTHO
00471              END-IF                                               ELTORTHO
00472      END-PERFORM.                                                 ELTORTHO
00473                                                                   ELTORTHO
00474  2000-EXIT.  EXIT.                                                ELTORTHO
00475                                                                   ELTORTHO
00476 /                                                                 ELTORTHO
00477  2100-BUILD-SCREEN-LINES.                                         ELTORTHO
00478      SET PLT-INDEX1  TO                                           ELTORTHO
00479              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTORTHO
00480                                                                   ELTORTHO
00481      IF WS-NOT-FIRST-TIME                                         ELTORTHO
00482         SET COF-NEW-PAGE TO TRUE                                  ELTORTHO
00483         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTORTHO
00484         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTORTHO
00485                   COMMAREA (DFHCOMMAREA)                          ELTORTHO
00486         END-EXEC                                                  ELTORTHO
00487      ELSE                                                         ELTORTHO
00488         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTORTHO
00489                                                                   ELTORTHO
00490      MOVE  +1  TO  WS-CIA.                                        ELTORTHO
00491                                                                   ELTORTHO
00492      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTORTHO
00493          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTORTHO
00494              SET PLT-INDEX2  TO  2                                ELTORTHO
00495          ELSE                                                     ELTORTHO
00496              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTORTHO
00497              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTORTHO
00498                 THRU 9200-EXIT                                    ELTORTHO
00499              GO TO 2100-EXIT                                      ELTORTHO
00500      ELSE                                                         ELTORTHO
00501          SET PLT-INDEX2  TO  1.                                   ELTORTHO
00502                                                                   ELTORTHO
00503      PERFORM 2105-LIST-BEN-PROV                                   ELTORTHO
00504         THRU 2105-EXIT.                                           ELTORTHO
00505                                                                   ELTORTHO
00506      PERFORM 2110-PLACE-OF-TREATMENT                              ELTORTHO
00507         THRU 2110-EXIT.                                           ELTORTHO
00508                                                                   ELTORTHO
00509      PERFORM 2120-PRIC-METH                                       ELTORTHO
00510         THRU 2120-EXIT.                                           ELTORTHO
00511                                                                   ELTORTHO
00512      PERFORM 2140-CERTIFICATION                                   ELTORTHO
00513         THRU 2140-EXIT.                                           ELTORTHO
00514                                                                   ELTORTHO
00515      PERFORM 2150-RECERTIFICATION                                 ELTORTHO
00516         THRU 2150-EXIT.                                           ELTORTHO
00517                                                                   ELTORTHO
00518      PERFORM 2155-REPR-REPL                                       ELTORTHO
00519         THRU 2155-EXIT.                                           ELTORTHO
00520                                                                   ELTORTHO
00521      PERFORM 2160-SPILLOVR-COINS-N-DEDUC                          ELTORTHO
00522         THRU 2160-EXIT.                                           ELTORTHO
00523                                                                   ELTORTHO
00524      PERFORM 2165-TRANS-OTHR-RESPON-IND                           ELTORTHO
00525         THRU 2165-EXIT.                                           ELTORTHO
00526                                                                   ELTORTHO
00527      PERFORM 7000-ALL-LEVEL-TABS                                  ELTORTHO
00528         THRU 7000-EXIT.                                           ELTORTHO
00529                                                                   ELTORTHO
00530  2100-EXIT.  EXIT.                                                ELTORTHO
00531 /                                                                 ELTORTHO
00532  2105-LIST-BEN-PROV.                                              ELTORTHO
00533 ****************************************************************  ELTORTHO
00534 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTORTHO
00535 ****************************************************************  ELTORTHO
00536      MOVE  +2               TO  WS-CIA.                           ELTORTHO
00537      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTORTHO
00538      MOVE ZERO              TO  WS-SUB2.                          ELTORTHO
00539      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTORTHO
00540                             TO  WS-SUB3.                          ELTORTHO
00541                                                                   ELTORTHO
00542      PERFORM 2106-ZERO-ALL-WITH-SAME-NO                           ELTORTHO
00543         THRU 2106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTORTHO
00544                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.ELTORTHO
00545                                                                   ELTORTHO
00546      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTORTHO
00547      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTORTHO
00548                                                                   ELTORTHO
00549      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
00550                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
00551                     END-EXEC.                                     ELTORTHO
00552                                                                   ELTORTHO
00553  2105-EXIT.  EXIT.                                                ELTORTHO
00554      SKIP3                                                        ELTORTHO
00555  2106-ZERO-ALL-WITH-SAME-NO.                                      ELTORTHO
00556                                                                   ELTORTHO
00557      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTORTHO
00558          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTORTHO
00559          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTORTHO
00560          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTORTHO
00561                            TO  CMF-CODE-VALUE                     ELTORTHO
00562          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00563          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTORTHO
00564          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00565             THRU 9500-EXIT                                        ELTORTHO
00566          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTORTHO
00567          ADD  +1    TO  WS-SUB2                                   ELTORTHO
00568          IF WS-CIA  >  20  OR  =  20                              ELTORTHO
00569              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTORTHO
00570              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTORTHO
00571                             COMMAREA (DFHCOMMAREA)                ELTORTHO
00572                             END-EXEC                              ELTORTHO
00573              MOVE  +1  TO  WS-CIA.                                ELTORTHO
00574                                                                   ELTORTHO
00575  2106-EXIT.  EXIT.                                                ELTORTHO
00576 /                                                                 ELTORTHO
00577  2110-PLACE-OF-TREATMENT.                                         ELTORTHO
00578 ****************************************************************  ELTORTHO
00579 *              P L A C E   O F   T R E A T M E N T             *  ELTORTHO
00580 ****************************************************************  ELTORTHO
00581      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00582      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00583                          AND                                      ELTORTHO
00584         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
00585                                                  NOT =  ZERO      ELTORTHO
00586          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00587          MOVE +2                    TO  WS-CIA                    ELTORTHO
00588          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00589                                                                   ELTORTHO
00590      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00591      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00592                           AND                                     ELTORTHO
00593         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
00594                                                 NOT  =  ZERO      ELTORTHO
00595                           AND                                     ELTORTHO
00596         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
00597          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00598          MOVE +2                    TO  WS-CIA                    ELTORTHO
00599          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00600                                                                   ELTORTHO
00601      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00603                           AND                                     ELTORTHO
00604         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
00605                                                  NOT  =  ZERO     ELTORTHO
00606          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
00607          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
00608                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00609          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
00610                               TO  CMF-CODE-VALUE                  ELTORTHO
00611          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
00612          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00613          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00614             THRU 9500-EXIT.                                       ELTORTHO
00615                                                                   ELTORTHO
00616      SET PLT-INDEX2  TO  2.                                       ELTORTHO
00617      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00618                           AND                                     ELTORTHO
00619         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
00620                                                 NOT  =  ZERO      ELTORTHO
00621          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
00622          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
00623                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00624          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
00625                               TO  CMF-CODE-VALUE                  ELTORTHO
00626          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00627          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00628          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00629             THRU 9500-EXIT.                                       ELTORTHO
00630                                                                   ELTORTHO
00631      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
00632         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
00633          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
00634             THRU 9200-EXIT.                                       ELTORTHO
00635                                                                   ELTORTHO
00636      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
00637         SET PLT-INDEX2  TO  2                                     ELTORTHO
00638      ELSE                                                         ELTORTHO
00639         SET PLT-INDEX2  TO  1.                                    ELTORTHO
00640                                                                   ELTORTHO
00641  2110-EXIT.  EXIT.                                                ELTORTHO
00642 /                                                                 ELTORTHO
00643  2120-PRIC-METH.                                                  ELTORTHO
00644 ****************************************************************  ELTORTHO
00645 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTORTHO
00646 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTORTHO
00647 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTORTHO
00648 ****************************************************************  ELTORTHO
00649      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00650      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
00651         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
00652                                                              '19' ELTORTHO
00653         MOVE +2             TO  WS-CIA                            ELTORTHO
00654         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
00655         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
00656                                                                   ELTORTHO
00657      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00658      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
00659         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
00660                                                        '19' AND   ELTORTHO
00661         NOT WS-ADD-A-BLANK-LINE                                   ELTORTHO
00662         MOVE +2             TO  WS-CIA                            ELTORTHO
00663         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
00664         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
00665                                                                   ELTORTHO
00666      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00667      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
00668         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
00669                            AND                                    ELTORTHO
00670         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00671         SET  PLT-INDEX2  TO  2                                    ELTORTHO
00672         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
00673                                                             ZERO  ELTORTHO
00674            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
00675            ADD +1  TO  WS-CIA                                     ELTORTHO
00676            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
00677                                                                   ELTORTHO
00678      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00679      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
00680         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
00681                            AND                                    ELTORTHO
00682         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTORTHO
00683         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTORTHO
00684         ADD +1  TO  WS-CIA                                        ELTORTHO
00685         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTORTHO
00686                                                                   ELTORTHO
00687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTORTHO
00688         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00689         SET  PLT-INDEX2  TO  2                                    ELTORTHO
00690         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
00691                                                             ZERO  ELTORTHO
00692            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
00693            ADD +1  TO  WS-CIA                                     ELTORTHO
00694            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
00695                                                                   ELTORTHO
00696      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00697      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00698         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
00699                                                            =  ZEROELTORTHO
00700            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00701                                                            =  ZEROELTORTHO
00702               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
00703            ELSE                                                   ELTORTHO
00704               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
00705          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00706                                                  TO  WS-PERCENTAGEELTORTHO
00707         ELSE                                                      ELTORTHO
00708            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
00709          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00710                                                 TO  WS-PERCENTAGE.ELTORTHO
00711      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
00712         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
00713                                             ZERO AND  NOT =  '19' ELTORTHO
00714         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
00715         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
00716         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
00717                                                    CMF-CODE-VALUE ELTORTHO
00718         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00719         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
00720         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
00721            THRU 9600-EXIT.                                        ELTORTHO
00722                                                                   ELTORTHO
00723      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00724      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00725         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTORTHO
00726                                                               ZEROELTORTHO
00727            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00728                                                            =  ZEROELTORTHO
00729               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
00730            ELSE                                                   ELTORTHO
00731               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
00732          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00733                                                  TO  WS-PERCENTAGEELTORTHO
00734         ELSE                                                      ELTORTHO
00735            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
00736          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
00737                                                 TO  WS-PERCENTAGE.ELTORTHO
00738      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
00739         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
00740                                             ZERO AND  NOT =  '19' ELTORTHO
00741         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
00742         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
00743         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
00744                                                    CMF-CODE-VALUE ELTORTHO
00745         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTORTHO
00746         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
00747         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
00748            THRU 9600-EXIT.                                        ELTORTHO
00749                                                                   ELTORTHO
00750      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
00751          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTORTHO
00752          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
00753             THRU 9200-EXIT.                                       ELTORTHO
00754                                                                   ELTORTHO
00755      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
00756         SET PLT-INDEX2  TO  2                                     ELTORTHO
00757      ELSE                                                         ELTORTHO
00758         SET PLT-INDEX2  TO  1.                                    ELTORTHO
00759                                                                   ELTORTHO
00760  2120-EXIT.  EXIT.                                                ELTORTHO
00761 /                                                                 ELTORTHO
00762  2140-CERTIFICATION.                                              ELTORTHO
00763 ****************************************************************  ELTORTHO
00764 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTORTHO
00765 ****************************************************************  ELTORTHO
00766      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00767      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00768                          AND                                      ELTORTHO
00769         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
00770                                                  NOT =  '00'      ELTORTHO
00771          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00772          MOVE +2                    TO  WS-CIA                    ELTORTHO
00773          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00774                                                                   ELTORTHO
00775      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00776      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00777                           AND                                     ELTORTHO
00778         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
00779                                                 NOT  =  '00'      ELTORTHO
00780                           AND                                     ELTORTHO
00781         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
00782          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00783          MOVE +2                    TO  WS-CIA                    ELTORTHO
00784          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00785                                                                   ELTORTHO
00786      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00787      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00788                           AND                                     ELTORTHO
00789         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
00790                                                  NOT  =  '00'     ELTORTHO
00791          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTORTHO
00792          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
00793                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00794          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00795                               TO  CMF-CODE-VALUE                  ELTORTHO
00796          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
00797          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00798          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00799             THRU 9500-EXIT.                                       ELTORTHO
00800                                                                   ELTORTHO
00801      SET PLT-INDEX2  TO  2.                                       ELTORTHO
00802      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00803                           AND                                     ELTORTHO
00804         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
00805                                                 NOT  =  '00'      ELTORTHO
00806          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTORTHO
00807          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
00808                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00809          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00810                               TO  CMF-CODE-VALUE                  ELTORTHO
00811          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00812          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00813          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00814             THRU 9500-EXIT.                                       ELTORTHO
00815                                                                   ELTORTHO
00816      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
00817         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
00818          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
00819             THRU 9200-EXIT.                                       ELTORTHO
00820                                                                   ELTORTHO
00821      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
00822         SET PLT-INDEX2  TO  2                                     ELTORTHO
00823      ELSE                                                         ELTORTHO
00824         SET PLT-INDEX2  TO  1.                                    ELTORTHO
00825                                                                   ELTORTHO
00826  2140-EXIT.  EXIT.                                                ELTORTHO
00827 /                                                                 ELTORTHO
00828  2150-RECERTIFICATION.                                            ELTORTHO
00829 ****************************************************************  ELTORTHO
00830 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTORTHO
00831 ****************************************************************  ELTORTHO
00832      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00833      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00834                          AND                                      ELTORTHO
00835         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00836                                                  NOT =  ZERO      ELTORTHO
00837          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00838          MOVE +2                    TO  WS-CIA                    ELTORTHO
00839          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00840                                                                   ELTORTHO
00841      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00842      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00843                           AND                                     ELTORTHO
00844         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00845                                                 NOT  =  ZERO      ELTORTHO
00846                           AND                                     ELTORTHO
00847         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
00848          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00849          MOVE +2                    TO  WS-CIA                    ELTORTHO
00850          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00851                                                                   ELTORTHO
00852      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00853      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00854                           AND                                     ELTORTHO
00855         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00856                                                  NOT  =  ZERO     ELTORTHO
00857          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTORTHO
00858          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
00859                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00860          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
00861                               TO  CMF-CODE-VALUE                  ELTORTHO
00862          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
00863          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00864          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00865             THRU 9500-EXIT.                                       ELTORTHO
00866                                                                   ELTORTHO
00867      SET PLT-INDEX2  TO  2.                                       ELTORTHO
00868      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00869                           AND                                     ELTORTHO
00870         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00871                                                 NOT  =  ZERO      ELTORTHO
00872          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTORTHO
00873          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
00874                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00875          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
00876                               TO  CMF-CODE-VALUE                  ELTORTHO
00877          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00878          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00879          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00880             THRU 9500-EXIT.                                       ELTORTHO
00881                                                                   ELTORTHO
00882      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
00883         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
00884          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
00885             THRU 9200-EXIT.                                       ELTORTHO
00886                                                                   ELTORTHO
00887      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
00888         SET PLT-INDEX2  TO  2                                     ELTORTHO
00889      ELSE                                                         ELTORTHO
00890         SET PLT-INDEX2  TO  1.                                    ELTORTHO
00891                                                                   ELTORTHO
00892  2150-EXIT.  EXIT.                                                ELTORTHO
00893 /                                                                 ELTORTHO
00894  2155-REPR-REPL.                                                  ELTORTHO
00895 ****************************************************************  ELTORTHO
00896 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTORTHO
00897 ****************************************************************  ELTORTHO
00898      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00899      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00900                          AND                                      ELTORTHO
00901         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00902                                                  NOT =  ZERO      ELTORTHO
00903          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00904          MOVE +2                    TO  WS-CIA                    ELTORTHO
00905          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00906                                                                   ELTORTHO
00907      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00908      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00909                           AND                                     ELTORTHO
00910         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00911                                                 NOT  =  ZERO      ELTORTHO
00912                           AND                                     ELTORTHO
00913         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
00914          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
00915          MOVE +2                    TO  WS-CIA                    ELTORTHO
00916          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
00917                                                                   ELTORTHO
00918      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
00919      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
00920                           AND                                     ELTORTHO
00921         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00922                                                  NOT  =  ZERO     ELTORTHO
00923          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTORTHO
00924          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
00925                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00926          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
00927                               TO  CMF-CODE-VALUE                  ELTORTHO
00928          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
00929          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00930          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00931             THRU 9500-EXIT.                                       ELTORTHO
00932                                                                   ELTORTHO
00933      SET PLT-INDEX2  TO  2.                                       ELTORTHO
00934      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
00935                           AND                                     ELTORTHO
00936         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00937                                                 NOT  =  ZERO      ELTORTHO
00938          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTORTHO
00939          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
00940                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
00941          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
00942                               TO  CMF-CODE-VALUE                  ELTORTHO
00943          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
00944          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
00945          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
00946             THRU 9500-EXIT.                                       ELTORTHO
00947                                                                   ELTORTHO
00948      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
00949         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
00950          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
00951             THRU 9200-EXIT.                                       ELTORTHO
00952                                                                   ELTORTHO
00953      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
00954         SET PLT-INDEX2  TO  2                                     ELTORTHO
00955      ELSE                                                         ELTORTHO
00956         SET PLT-INDEX2  TO  1.                                    ELTORTHO
00957                                                                   ELTORTHO
00958  2155-EXIT.  EXIT.                                                ELTORTHO
00959 /                                                                 ELTORTHO
00960  2160-SPILLOVR-COINS-N-DEDUC.                                     ELTORTHO
00961 ****************************************************************  ELTORTHO
00962 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTORTHO
00963 ****************************************************************  ELTORTHO
00964      MOVE +1  TO  WS-CIA.                                         ELTORTHO
00965                                                                   ELTORTHO
00966      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00967      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
00968                            AND                                    ELTORTHO
00969         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTORTHO
00970                                                  NOT =  '0'       ELTORTHO
00971         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
00972         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTORTHO
00973                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
00974         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTORTHO
00975                                                TO  CMF-CODE-VALUE ELTORTHO
00976         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTORTHO
00977         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTORTHO
00978         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
00979            THRU 9500-EXIT                                         ELTORTHO
00980         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
00981            THRU 9200-EXIT.                                        ELTORTHO
00982 ****************************************************************  ELTORTHO
00983 *          S P I L L O V E R   D E D U C T I B L E             *  ELTORTHO
00984 ****************************************************************  ELTORTHO
00985      MOVE +1  TO  WS-CIA.                                         ELTORTHO
00986                                                                   ELTORTHO
00987      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
00988      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
00989                            AND                                    ELTORTHO
00990         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
00991                                                  NOT =  '0'       ELTORTHO
00992         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
00993         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTORTHO
00994                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
00995         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
00996                                                 TO  CMF-CODE-VALUEELTORTHO
00997         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTORTHO
00998         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTORTHO
00999         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
01000            THRU 9500-EXIT                                         ELTORTHO
01001         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
01002            THRU 9200-EXIT.                                        ELTORTHO
01003                                                                   ELTORTHO
01004      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
01005         SET PLT-INDEX2  TO  2                                     ELTORTHO
01006      ELSE                                                         ELTORTHO
01007         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01008                                                                   ELTORTHO
01009  2160-EXIT.  EXIT.                                                ELTORTHO
01010 /                                                                 ELTORTHO
01011  2165-TRANS-OTHR-RESPON-IND.                                      ELTORTHO
01012 ******************************************************************ELTORTHO
01013 *   T R A N S F E R   T O   O T H E R  R E S P O N S I B I L I T YELTORTHO
01014 *                     I N D I C A T O R                      9/89 ELTORTHO
01015 ******************************************************************ELTORTHO
01016                                                                   ELTORTHO
01017      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)          =  ZEROS    ELTORTHO
01018         IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   NOT  = ZEROS    ELTORTHO
01019              SET PLT-INDEX2  TO  2                                ELTORTHO
01020          ELSE                                                     ELTORTHO
01021              GO TO 2165-EXIT                                      ELTORTHO
01022      ELSE                                                         ELTORTHO
01023          SET PLT-INDEX2  TO  1.                                   ELTORTHO
01024                                                                   ELTORTHO
01025      IF  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =     ELTORTHO
01026          ZEROS                                                    ELTORTHO
01027          GO TO 2165-EXIT.                                         ELTORTHO
01028                                                                   ELTORTHO
01029      MOVE +1  TO  WS-CIA.                                         ELTORTHO
01030      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTORTHO
01031                                                                   ELTORTHO
01032      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTORTHO
01033                                                                   ELTORTHO
01034      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTORTHO
01035           TO  CMF-CODE-VALUE.                                     ELTORTHO
01036                                                                   ELTORTHO
01037      MOVE SPACES            TO  WS-TEMP-TEXT-AREA.                ELTORTHO
01038      MOVE +0                TO  WS-TEMP-NOT-USED-CNT.             ELTORTHO
01039                                                                   ELTORTHO
01040      PERFORM 9500-CALL-CODES-MANUAL-LONG  THRU 9500-EXIT.         ELTORTHO
01041                                                                   ELTORTHO
01042      PERFORM 9200-TEXT-OUTPUT-REQUEST     THRU 9200-EXIT.         ELTORTHO
01043                                                                   ELTORTHO
01044                                                                   ELTORTHO
01045  2165-EXIT.     EXIT.                                             ELTORTHO
01046 /                                                                 ELTORTHO
01047 ****************************************************************  ELTORTHO
01048 *       ORTHOTIC INSTITUTIONAL OUTPATIENT PROCESSING           *  ELTORTHO
01049 ****************************************************************  ELTORTHO
01050  3000-INSTITUTIONAL-OP.                                           ELTORTHO
01051                                                                   ELTORTHO
01052      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTORTHO
01053                                                                   ELTORTHO
01054      MOVE HEADER-I-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTORTHO
01055                                                                   ELTORTHO
01056      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTORTHO
01057         THRU 9100-EXIT.                                           ELTORTHO
01058                                                                   ELTORTHO
01059      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
01060      PERFORM WITH TEST BEFORE                                     ELTORTHO
01061              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTORTHO
01062              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTORTHO
01063         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTORTHO
01064         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTORTHO
01065         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTORTHO
01066         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTORTHO
01067      END-PERFORM.                                                 ELTORTHO
01068      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
01069                                                                   ELTORTHO
01070                                                                   ELTORTHO
01071      PERFORM WITH TEST BEFORE                                     ELTORTHO
01072         VARYING WS-SUB FROM +1 BY +1                              ELTORTHO
01073         UNTIL   WS-SUB  >     WS-INST-OP-CNT                      ELTORTHO
01074           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTORTHO
01075           MOVE WS-INST-OP-BP (WS-SUB)                             ELTORTHO
01076                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTORTHO
01077            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTORTHO
01078                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTORTHO
01079      END-PERFORM.                                                 ELTORTHO
01080                                                                   ELTORTHO
01081      MOVE 'ORTHOTIC APPLIANCES      '   TO  SSB-TOPIC-PHRASE.     ELTORTHO
01082                                                                   ELTORTHO
01083      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTORTHO
01084                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01085                     END-EXEC.                                     ELTORTHO
01086                                                                   ELTORTHO
01087      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
01088                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01089                     END-EXEC.                                     ELTORTHO
01090                                                                   ELTORTHO
01091      IF PVN-COVG-NONE                                             ELTORTHO
01092          GO TO 3000-EXIT.                                         ELTORTHO
01093                                                                   ELTORTHO
01094      MOVE +1  TO  WS-CIA.                                         ELTORTHO
01095                                                                   ELTORTHO
01096      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTORTHO
01097                    PSP-PROVN-PRICING-METHD,                       ELTORTHO
01098                    PSP-TRANSF-OTHER-RESP-IND,                     ELTORTHO
01099                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTORTHO
01100                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTORTHO
01101                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTORTHO
01102                    PSP-SPILL-OVER-DED-APL-IND,                    ELTORTHO
01103                    PSP-CERTFN-REQRM-IND,                          ELTORTHO
01104                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTORTHO
01105                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTORTHO
01106                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTORTHO
01107                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTORTHO
01108                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTORTHO
01109                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTORTHO
01110                    PSB-CERTN-REPETN-REQRD-IND,                    ELTORTHO
01111                    PSB-REPR-REPLAC-RESTRN-IND.                    ELTORTHO
01112                                                                   ELTORTHO
01113      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTORTHO
01114                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01115                     END-EXEC.                                     ELTORTHO
01116                                                                   ELTORTHO
01117      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTORTHO
01118      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
01119          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTORTHO
01120                                                                   ELTORTHO
01121      PERFORM WITH TEST BEFORE                                     ELTORTHO
01122         VARYING WS-SUB  FROM  +1  BY  +1                          ELTORTHO
01123         UNTIL WS-SUB  >  WS-INST-OP-CNT                           ELTORTHO
01124              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTORTHO
01125              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTORTHO
01126                   PERFORM 3100-BUILD-SCREEN-LINES THRU 3100-EXIT  ELTORTHO
01127              END-IF                                               ELTORTHO
01128      END-PERFORM.                                                 ELTORTHO
01129                                                                   ELTORTHO
01130  3000-EXIT.  EXIT.                                                ELTORTHO
01131 /                                                                 ELTORTHO
01132  3100-BUILD-SCREEN-LINES.                                         ELTORTHO
01133      SET PLT-INDEX1  TO                                           ELTORTHO
01134              PVN-COVG-SAME-AS         (PVN-BEN-PROVN-IDX).        ELTORTHO
01135                                                                   ELTORTHO
01136      IF WS-NOT-FIRST-TIME                                         ELTORTHO
01137         SET COF-NEW-PAGE TO TRUE                                  ELTORTHO
01138         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTORTHO
01139         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTORTHO
01140                   COMMAREA (DFHCOMMAREA)                          ELTORTHO
01141         END-EXEC                                                  ELTORTHO
01142      ELSE                                                         ELTORTHO
01143         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTORTHO
01144                                                                   ELTORTHO
01145      MOVE  +1  TO  WS-CIA.                                        ELTORTHO
01146                                                                   ELTORTHO
01147      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTORTHO
01148          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTORTHO
01149              SET PLT-INDEX2  TO  2                                ELTORTHO
01150          ELSE                                                     ELTORTHO
01151              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTORTHO
01152              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTORTHO
01153                 THRU 9200-EXIT                                    ELTORTHO
01154              GO TO 3100-EXIT                                      ELTORTHO
01155      ELSE                                                         ELTORTHO
01156          SET PLT-INDEX2  TO  1.                                   ELTORTHO
01157                                                                   ELTORTHO
01158      PERFORM 3105-LIST-BEN-PROV                                   ELTORTHO
01159         THRU 3105-EXIT.                                           ELTORTHO
01160                                                                   ELTORTHO
01161      PERFORM 3110-PLACE-OF-TREATMENT                              ELTORTHO
01162         THRU 3110-EXIT.                                           ELTORTHO
01163                                                                   ELTORTHO
01164      PERFORM 3120-PRIC-METH                                       ELTORTHO
01165         THRU 3120-EXIT.                                           ELTORTHO
01166                                                                   ELTORTHO
01167      PERFORM 3140-CERTIFICATION                                   ELTORTHO
01168         THRU 3140-EXIT.                                           ELTORTHO
01169                                                                   ELTORTHO
01170      PERFORM 3150-RECERTIFICATION                                 ELTORTHO
01171         THRU 3150-EXIT.                                           ELTORTHO
01172                                                                   ELTORTHO
01173      PERFORM 3155-REPR-REPL                                       ELTORTHO
01174         THRU 3155-EXIT.                                           ELTORTHO
01175                                                                   ELTORTHO
01176      PERFORM 3160-SPILLOVR-COINS-N-DEDUC                          ELTORTHO
01177         THRU 3160-EXIT.                                           ELTORTHO
01178                                                                   ELTORTHO
01179      PERFORM 2165-TRANS-OTHR-RESPON-IND                           ELTORTHO
01180         THRU 2165-EXIT.                                           ELTORTHO
01181                                                                   ELTORTHO
01182      PERFORM 7000-ALL-LEVEL-TABS                                  ELTORTHO
01183         THRU 7000-EXIT.                                           ELTORTHO
01184                                                                   ELTORTHO
01185  3100-EXIT.  EXIT.                                                ELTORTHO
01186 /                                                                 ELTORTHO
01187  3105-LIST-BEN-PROV.                                              ELTORTHO
01188 ****************************************************************  ELTORTHO
01189 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTORTHO
01190 ****************************************************************  ELTORTHO
01191      MOVE  +2               TO  WS-CIA.                           ELTORTHO
01192      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTORTHO
01193      MOVE ZERO              TO  WS-SUB2.                          ELTORTHO
01194      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTORTHO
01195                             TO  WS-SUB3.                          ELTORTHO
01196                                                                   ELTORTHO
01197      PERFORM 3106-ZERO-ALL-WITH-SAME-NO                           ELTORTHO
01198         THRU 3106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTORTHO
01199                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.ELTORTHO
01200                                                                   ELTORTHO
01201      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTORTHO
01202      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTORTHO
01203                                                                   ELTORTHO
01204      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
01205                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01206                     END-EXEC.                                     ELTORTHO
01207                                                                   ELTORTHO
01208  3105-EXIT.  EXIT.                                                ELTORTHO
01209      SKIP3                                                        ELTORTHO
01210  3106-ZERO-ALL-WITH-SAME-NO.                                      ELTORTHO
01211                                                                   ELTORTHO
01212      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTORTHO
01213          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTORTHO
01214          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTORTHO
01215          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTORTHO
01216                            TO  CMF-CODE-VALUE                     ELTORTHO
01217          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01218          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTORTHO
01219          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01220             THRU 9500-EXIT                                        ELTORTHO
01221          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTORTHO
01222          ADD  +1    TO  WS-SUB2                                   ELTORTHO
01223          IF WS-CIA  >  20  OR  =  20                              ELTORTHO
01224              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTORTHO
01225              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTORTHO
01226                             COMMAREA (DFHCOMMAREA)                ELTORTHO
01227                             END-EXEC                              ELTORTHO
01228              MOVE  +1  TO  WS-CIA.                                ELTORTHO
01229                                                                   ELTORTHO
01230  3106-EXIT.  EXIT.                                                ELTORTHO
01231 /                                                                 ELTORTHO
01232  3110-PLACE-OF-TREATMENT.                                         ELTORTHO
01233 ****************************************************************  ELTORTHO
01234 *              P L A C E   O F   T R E A T M E N T             *  ELTORTHO
01235 ****************************************************************  ELTORTHO
01236      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01237      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01238                          AND                                      ELTORTHO
01239         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01240                                                  NOT =  ZERO      ELTORTHO
01241          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01242          MOVE +2                    TO  WS-CIA                    ELTORTHO
01243          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01244                                                                   ELTORTHO
01245      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01246      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01247                           AND                                     ELTORTHO
01248         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01249                                                 NOT  =  ZERO      ELTORTHO
01250                           AND                                     ELTORTHO
01251         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
01252          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01253          MOVE +2                    TO  WS-CIA                    ELTORTHO
01254          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01255                                                                   ELTORTHO
01256      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01257      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01258                           AND                                     ELTORTHO
01259         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01260                                                  NOT  =  ZERO     ELTORTHO
01261          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
01262          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
01263                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01264          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01265                               TO  CMF-CODE-VALUE                  ELTORTHO
01266          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
01267          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01268          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01269             THRU 9500-EXIT.                                       ELTORTHO
01270                                                                   ELTORTHO
01271      SET PLT-INDEX2  TO  2.                                       ELTORTHO
01272      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01273                           AND                                     ELTORTHO
01274         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01275                                                 NOT  =  ZERO      ELTORTHO
01276          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
01277          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
01278                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01279          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01280                               TO  CMF-CODE-VALUE                  ELTORTHO
01281          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01282          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01283          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01284             THRU 9500-EXIT.                                       ELTORTHO
01285                                                                   ELTORTHO
01286      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01287         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
01288          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01289             THRU 9200-EXIT.                                       ELTORTHO
01290                                                                   ELTORTHO
01291      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01292         SET PLT-INDEX2  TO  2                                     ELTORTHO
01293      ELSE                                                         ELTORTHO
01294         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01295                                                                   ELTORTHO
01296  3110-EXIT.  EXIT.                                                ELTORTHO
01297 /                                                                 ELTORTHO
01298  3120-PRIC-METH.                                                  ELTORTHO
01299 ****************************************************************  ELTORTHO
01300 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTORTHO
01301 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTORTHO
01302 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTORTHO
01303 ****************************************************************  ELTORTHO
01304      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01305      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01306         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01307                                                              '19' ELTORTHO
01308         MOVE +2             TO  WS-CIA                            ELTORTHO
01309         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
01310         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
01311                                                                   ELTORTHO
01312      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01313      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01314         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01315                                                        '19' AND   ELTORTHO
01316         NOT WS-ADD-A-BLANK-LINE                                   ELTORTHO
01317         MOVE +2             TO  WS-CIA                            ELTORTHO
01318         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
01319         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
01320                                                                   ELTORTHO
01321      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01322      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01323         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
01324                            AND                                    ELTORTHO
01325         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01326         SET  PLT-INDEX2  TO  2                                    ELTORTHO
01327         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
01328                                                             ZERO  ELTORTHO
01329            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
01330            ADD +1  TO  WS-CIA                                     ELTORTHO
01331            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
01332                                                                   ELTORTHO
01333      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01334      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01335         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
01336                            AND                                    ELTORTHO
01337         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTORTHO
01338         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTORTHO
01339         ADD +1  TO  WS-CIA                                        ELTORTHO
01340         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTORTHO
01341                                                                   ELTORTHO
01342      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTORTHO
01343         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01344         SET  PLT-INDEX2  TO  2                                    ELTORTHO
01345         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
01346                                                             ZERO  ELTORTHO
01347            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
01348            ADD +1  TO  WS-CIA                                     ELTORTHO
01349            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
01350                                                                   ELTORTHO
01351      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01352      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01353         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01354                                                            =  ZEROELTORTHO
01355            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01356                                                            =  ZEROELTORTHO
01357               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
01358            ELSE                                                   ELTORTHO
01359               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
01360          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01361                                                  TO  WS-PERCENTAGEELTORTHO
01362         ELSE                                                      ELTORTHO
01363            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
01364          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01365                                                 TO  WS-PERCENTAGE.ELTORTHO
01366      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01367         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01368                                             ZERO AND  NOT =  '19' ELTORTHO
01369         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
01370         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
01371         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
01372                                                    CMF-CODE-VALUE ELTORTHO
01373         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01374         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
01375         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
01376            THRU 9600-EXIT.                                        ELTORTHO
01377                                                                   ELTORTHO
01378      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01379      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01380         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTORTHO
01381                                                               ZEROELTORTHO
01382            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01383                                                            =  ZEROELTORTHO
01384               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
01385            ELSE                                                   ELTORTHO
01386               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
01387          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01388                                                  TO  WS-PERCENTAGEELTORTHO
01389         ELSE                                                      ELTORTHO
01390            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
01391          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01392                                                 TO  WS-PERCENTAGE.ELTORTHO
01393      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTORTHO
01394         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01395                                             ZERO AND  NOT =  '19' ELTORTHO
01396         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
01397         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
01398         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
01399                                                    CMF-CODE-VALUE ELTORTHO
01400         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTORTHO
01401         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
01402         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
01403            THRU 9600-EXIT.                                        ELTORTHO
01404                                                                   ELTORTHO
01405      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01406          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTORTHO
01407          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01408             THRU 9200-EXIT.                                       ELTORTHO
01409                                                                   ELTORTHO
01410      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01411         SET PLT-INDEX2  TO  2                                     ELTORTHO
01412      ELSE                                                         ELTORTHO
01413         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01414                                                                   ELTORTHO
01415  3120-EXIT.  EXIT.                                                ELTORTHO
01416 /                                                                 ELTORTHO
01417  3140-CERTIFICATION.                                              ELTORTHO
01418 ****************************************************************  ELTORTHO
01419 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTORTHO
01420 ****************************************************************  ELTORTHO
01421      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01422      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01423                          AND                                      ELTORTHO
01424         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
01425                                                  NOT =  '00'      ELTORTHO
01426          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01427          MOVE +2                    TO  WS-CIA                    ELTORTHO
01428          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01429                                                                   ELTORTHO
01430      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01431      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01432                           AND                                     ELTORTHO
01433         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
01434                                                 NOT  =  '00'      ELTORTHO
01435                           AND                                     ELTORTHO
01436         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
01437          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01438          MOVE +2                    TO  WS-CIA                    ELTORTHO
01439          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01440                                                                   ELTORTHO
01441      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01442      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01443                           AND                                     ELTORTHO
01444         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
01445                                                  NOT  =  '00'     ELTORTHO
01446          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTORTHO
01447          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
01448                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01449          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01450                               TO  CMF-CODE-VALUE                  ELTORTHO
01451          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
01452          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01453          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01454             THRU 9500-EXIT.                                       ELTORTHO
01455                                                                   ELTORTHO
01456      SET PLT-INDEX2  TO  2.                                       ELTORTHO
01457      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01458                           AND                                     ELTORTHO
01459         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
01460                                                 NOT  =  '00'      ELTORTHO
01461          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTORTHO
01462          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
01463                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01464          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01465                               TO  CMF-CODE-VALUE                  ELTORTHO
01466          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01467          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01468          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01469             THRU 9500-EXIT.                                       ELTORTHO
01470                                                                   ELTORTHO
01471      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01472         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
01473          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01474             THRU 9200-EXIT.                                       ELTORTHO
01475                                                                   ELTORTHO
01476      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01477         SET PLT-INDEX2  TO  2                                     ELTORTHO
01478      ELSE                                                         ELTORTHO
01479         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01480                                                                   ELTORTHO
01481  3140-EXIT.  EXIT.                                                ELTORTHO
01482 /                                                                 ELTORTHO
01483  3150-RECERTIFICATION.                                            ELTORTHO
01484 ****************************************************************  ELTORTHO
01485 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTORTHO
01486 ****************************************************************  ELTORTHO
01487      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01488      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01489                          AND                                      ELTORTHO
01490         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01491                                                  NOT =  ZERO      ELTORTHO
01492          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01493          MOVE +2                    TO  WS-CIA                    ELTORTHO
01494          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01495                                                                   ELTORTHO
01496      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01497      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01498                           AND                                     ELTORTHO
01499         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01500                                                 NOT  =  ZERO      ELTORTHO
01501                           AND                                     ELTORTHO
01502         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
01503          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01504          MOVE +2                    TO  WS-CIA                    ELTORTHO
01505          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01506                                                                   ELTORTHO
01507      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01508      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01509                           AND                                     ELTORTHO
01510         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01511                                                  NOT  =  ZERO     ELTORTHO
01512          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTORTHO
01513          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
01514                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01515          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
01516                               TO  CMF-CODE-VALUE                  ELTORTHO
01517          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
01518          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01519          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01520             THRU 9500-EXIT.                                       ELTORTHO
01521                                                                   ELTORTHO
01522      SET PLT-INDEX2  TO  2.                                       ELTORTHO
01523      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01524                           AND                                     ELTORTHO
01525         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01526                                                 NOT  =  ZERO      ELTORTHO
01527          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTORTHO
01528          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
01529                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01530          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
01531                               TO  CMF-CODE-VALUE                  ELTORTHO
01532          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01533          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01534          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01535             THRU 9500-EXIT.                                       ELTORTHO
01536                                                                   ELTORTHO
01537      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01538         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
01539          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01540             THRU 9200-EXIT.                                       ELTORTHO
01541                                                                   ELTORTHO
01542      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01543         SET PLT-INDEX2  TO  2                                     ELTORTHO
01544      ELSE                                                         ELTORTHO
01545         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01546                                                                   ELTORTHO
01547  3150-EXIT.  EXIT.                                                ELTORTHO
01548 /                                                                 ELTORTHO
01549  3155-REPR-REPL.                                                  ELTORTHO
01550 ****************************************************************  ELTORTHO
01551 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTORTHO
01552 ****************************************************************  ELTORTHO
01553      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01554      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01555                          AND                                      ELTORTHO
01556         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01557                                                  NOT =  ZERO      ELTORTHO
01558          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01559          MOVE +2                    TO  WS-CIA                    ELTORTHO
01560          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01561                                                                   ELTORTHO
01562      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01563      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01564                           AND                                     ELTORTHO
01565         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01566                                                 NOT  =  ZERO      ELTORTHO
01567                           AND                                     ELTORTHO
01568         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
01569          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01570          MOVE +2                    TO  WS-CIA                    ELTORTHO
01571          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01572                                                                   ELTORTHO
01573      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01574      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01575                           AND                                     ELTORTHO
01576         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01577                                                  NOT  =  ZERO     ELTORTHO
01578          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTORTHO
01579          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
01580                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01581          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
01582                               TO  CMF-CODE-VALUE                  ELTORTHO
01583          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
01584          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01585          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01586             THRU 9500-EXIT.                                       ELTORTHO
01587                                                                   ELTORTHO
01588      SET PLT-INDEX2  TO  2.                                       ELTORTHO
01589      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTORTHO
01590                           AND                                     ELTORTHO
01591         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01592                                                 NOT  =  ZERO      ELTORTHO
01593          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTORTHO
01594          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
01595                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01596          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
01597                               TO  CMF-CODE-VALUE                  ELTORTHO
01598          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01599          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01600          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01601             THRU 9500-EXIT.                                       ELTORTHO
01602                                                                   ELTORTHO
01603      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01604         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
01605          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01606             THRU 9200-EXIT.                                       ELTORTHO
01607                                                                   ELTORTHO
01608      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01609         SET PLT-INDEX2  TO  2                                     ELTORTHO
01610      ELSE                                                         ELTORTHO
01611         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01612                                                                   ELTORTHO
01613  3155-EXIT.  EXIT.                                                ELTORTHO
01614 /                                                                 ELTORTHO
01615  3160-SPILLOVR-COINS-N-DEDUC.                                     ELTORTHO
01616 ****************************************************************  ELTORTHO
01617 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTORTHO
01618 ****************************************************************  ELTORTHO
01619      MOVE +1  TO  WS-CIA.                                         ELTORTHO
01620                                                                   ELTORTHO
01621      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01622      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTORTHO
01623                            AND                                    ELTORTHO
01624         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTORTHO
01625                                                  NOT =  '0'       ELTORTHO
01626         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
01627         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTORTHO
01628                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
01629         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTORTHO
01630                                                TO  CMF-CODE-VALUE ELTORTHO
01631         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTORTHO
01632         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTORTHO
01633         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
01634            THRU 9500-EXIT                                         ELTORTHO
01635         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
01636            THRU 9200-EXIT.                                        ELTORTHO
01637 ****************************************************************  ELTORTHO
01638 *          S P I L L O V E R   D E D U C T I B L E             *  ELTORTHO
01639 ****************************************************************  ELTORTHO
01640      MOVE +1  TO  WS-CIA.                                         ELTORTHO
01641                                                                   ELTORTHO
01642      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01643      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTORTHO
01644                            AND                                    ELTORTHO
01645         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
01646                                                  NOT =  '0'       ELTORTHO
01647         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
01648         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTORTHO
01649                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
01650         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01651                                                 TO  CMF-CODE-VALUEELTORTHO
01652         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTORTHO
01653         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTORTHO
01654         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
01655            THRU 9500-EXIT                                         ELTORTHO
01656         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
01657            THRU 9200-EXIT.                                        ELTORTHO
01658                                                                   ELTORTHO
01659      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTORTHO
01660         SET PLT-INDEX2  TO  2                                     ELTORTHO
01661      ELSE                                                         ELTORTHO
01662         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01663                                                                   ELTORTHO
01664  3160-EXIT.  EXIT.                                                ELTORTHO
01665 /                                                                 ELTORTHO
01666  4000-PROFESSIONAL-IP.                                            ELTORTHO
01667 ****************************************************************  ELTORTHO
01668 *       ORTHOTIC PROFESSIONAL INPATIENT PROCESSING             *  ELTORTHO
01669 ****************************************************************  ELTORTHO
01670                                                                   ELTORTHO
01671      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTORTHO
01672                                                                   ELTORTHO
01673      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTORTHO
01674                                                                   ELTORTHO
01675      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTORTHO
01676         THRU 9100-EXIT.                                           ELTORTHO
01677                                                                   ELTORTHO
01678      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
01679      PERFORM WITH TEST BEFORE                                     ELTORTHO
01680              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTORTHO
01681              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTORTHO
01682         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTORTHO
01683         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTORTHO
01684         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTORTHO
01685         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTORTHO
01686      END-PERFORM.                                                 ELTORTHO
01687      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
01688                                                                   ELTORTHO
01689                                                                   ELTORTHO
01690      PERFORM WITH TEST BEFORE                                     ELTORTHO
01691         VARYING WS-SUB FROM +1 BY +1                              ELTORTHO
01692         UNTIL   WS-SUB  >     WS-PROF-IP-CNT                      ELTORTHO
01693           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTORTHO
01694           MOVE WS-PROF-IP-BP (WS-SUB)                             ELTORTHO
01695                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTORTHO
01696            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTORTHO
01697                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTORTHO
01698      END-PERFORM.                                                 ELTORTHO
01699                                                                   ELTORTHO
01700      MOVE 'ORTHOTIC APPLIANCES      '   TO  SSB-TOPIC-PHRASE.     ELTORTHO
01701                                                                   ELTORTHO
01702      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTORTHO
01703                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01704                     END-EXEC.                                     ELTORTHO
01705                                                                   ELTORTHO
01706      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
01707                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01708                     END-EXEC.                                     ELTORTHO
01709                                                                   ELTORTHO
01710      IF PVN-COVG-NONE                                             ELTORTHO
01711          GO TO 4000-EXIT.                                         ELTORTHO
01712                                                                   ELTORTHO
01713      MOVE +1  TO  WS-CIA.                                         ELTORTHO
01714                                                                   ELTORTHO
01715      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTORTHO
01716                    PSP-PROVN-PRICING-METHD,                       ELTORTHO
01717                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTORTHO
01718                    PSP-TRANSF-OTHER-RESP-IND,                     ELTORTHO
01719                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTORTHO
01720                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTORTHO
01721                    PSP-SPILL-OVER-DED-APL-IND,                    ELTORTHO
01722                    PSP-CERTFN-REQRM-IND,                          ELTORTHO
01723                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTORTHO
01724                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTORTHO
01725                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTORTHO
01726                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTORTHO
01727                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTORTHO
01728                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTORTHO
01729                    PSE-CERTN-REPETN-REQRD-IND,                    ELTORTHO
01730                    PSE-REPR-REPLAC-RESTRN-IND.                    ELTORTHO
01731                                                                   ELTORTHO
01732      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTORTHO
01733                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01734                     END-EXEC.                                     ELTORTHO
01735                                                                   ELTORTHO
01736      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTORTHO
01737      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
01738          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTORTHO
01739                                                                   ELTORTHO
01740      PERFORM WITH TEST BEFORE                                     ELTORTHO
01741         VARYING WS-SUB  FROM  +1  BY  +1                          ELTORTHO
01742         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTORTHO
01743              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTORTHO
01744              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTORTHO
01745                   PERFORM 4100-BUILD-SCREEN-LINES THRU 4100-EXIT  ELTORTHO
01746              END-IF                                               ELTORTHO
01747      END-PERFORM.                                                 ELTORTHO
01748                                                                   ELTORTHO
01749  4000-EXIT.  EXIT.                                                ELTORTHO
01750                                                                   ELTORTHO
01751 /                                                                 ELTORTHO
01752  4100-BUILD-SCREEN-LINES.                                         ELTORTHO
01753      SET PLT-INDEX1  TO                                           ELTORTHO
01754              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTORTHO
01755                                                                   ELTORTHO
01756      IF WS-NOT-FIRST-TIME                                         ELTORTHO
01757         SET COF-NEW-PAGE TO TRUE                                  ELTORTHO
01758         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTORTHO
01759         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTORTHO
01760                   COMMAREA (DFHCOMMAREA)                          ELTORTHO
01761         END-EXEC                                                  ELTORTHO
01762      ELSE                                                         ELTORTHO
01763         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTORTHO
01764                                                                   ELTORTHO
01765      MOVE  +1  TO  WS-CIA.                                        ELTORTHO
01766                                                                   ELTORTHO
01767      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTORTHO
01768          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTORTHO
01769              SET PLT-INDEX2  TO  2                                ELTORTHO
01770          ELSE                                                     ELTORTHO
01771              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTORTHO
01772              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTORTHO
01773                 THRU 9200-EXIT                                    ELTORTHO
01774              GO TO 4100-EXIT                                      ELTORTHO
01775      ELSE                                                         ELTORTHO
01776          SET PLT-INDEX2  TO  1.                                   ELTORTHO
01777                                                                   ELTORTHO
01778      PERFORM 4105-LIST-BEN-PROV                                   ELTORTHO
01779         THRU 4105-EXIT.                                           ELTORTHO
01780                                                                   ELTORTHO
01781      PERFORM 4110-PLACE-OF-TREATMENT                              ELTORTHO
01782         THRU 4110-EXIT.                                           ELTORTHO
01783                                                                   ELTORTHO
01784      PERFORM 4120-PRIC-METH                                       ELTORTHO
01785         THRU 4120-EXIT.                                           ELTORTHO
01786                                                                   ELTORTHO
01787      PERFORM 4140-CERTIFICATION                                   ELTORTHO
01788         THRU 4140-EXIT.                                           ELTORTHO
01789                                                                   ELTORTHO
01790      PERFORM 4150-RECERTIFICATION                                 ELTORTHO
01791         THRU 4150-EXIT.                                           ELTORTHO
01792                                                                   ELTORTHO
01793      PERFORM 4155-REPR-REPL                                       ELTORTHO
01794         THRU 4155-EXIT.                                           ELTORTHO
01795                                                                   ELTORTHO
01796      PERFORM 4160-SPILLOVR-COINS-N-DEDUC                          ELTORTHO
01797         THRU 4160-EXIT.                                           ELTORTHO
01798                                                                   ELTORTHO
01799      PERFORM 2165-TRANS-OTHR-RESPON-IND                           ELTORTHO
01800         THRU 2165-EXIT.                                           ELTORTHO
01801                                                                   ELTORTHO
01802      PERFORM 7000-ALL-LEVEL-TABS                                  ELTORTHO
01803         THRU 7000-EXIT.                                           ELTORTHO
01804                                                                   ELTORTHO
01805  4100-EXIT.  EXIT.                                                ELTORTHO
01806 /                                                                 ELTORTHO
01807  4105-LIST-BEN-PROV.                                              ELTORTHO
01808 ****************************************************************  ELTORTHO
01809 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTORTHO
01810 ****************************************************************  ELTORTHO
01811      MOVE  +2               TO  WS-CIA.                           ELTORTHO
01812      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTORTHO
01813      MOVE ZERO              TO  WS-SUB2.                          ELTORTHO
01814      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTORTHO
01815                             TO  WS-SUB3.                          ELTORTHO
01816                                                                   ELTORTHO
01817      PERFORM 4106-ZERO-ALL-WITH-SAME-NO                           ELTORTHO
01818         THRU 4106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTORTHO
01819                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.ELTORTHO
01820                                                                   ELTORTHO
01821      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTORTHO
01822      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTORTHO
01823                                                                   ELTORTHO
01824      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
01825                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
01826                     END-EXEC.                                     ELTORTHO
01827                                                                   ELTORTHO
01828  4105-EXIT.  EXIT.                                                ELTORTHO
01829      SKIP3                                                        ELTORTHO
01830  4106-ZERO-ALL-WITH-SAME-NO.                                      ELTORTHO
01831                                                                   ELTORTHO
01832      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTORTHO
01833          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTORTHO
01834          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTORTHO
01835          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTORTHO
01836                            TO  CMF-CODE-VALUE                     ELTORTHO
01837          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01838          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTORTHO
01839          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01840             THRU 9500-EXIT                                        ELTORTHO
01841          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTORTHO
01842          ADD  +1    TO  WS-SUB2                                   ELTORTHO
01843          IF WS-CIA  >  20  OR  =  20                              ELTORTHO
01844              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTORTHO
01845              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTORTHO
01846                             COMMAREA (DFHCOMMAREA)                ELTORTHO
01847                             END-EXEC                              ELTORTHO
01848              MOVE  +1  TO  WS-CIA.                                ELTORTHO
01849                                                                   ELTORTHO
01850  4106-EXIT.  EXIT.                                                ELTORTHO
01851 /                                                                 ELTORTHO
01852  4110-PLACE-OF-TREATMENT.                                         ELTORTHO
01853 ****************************************************************  ELTORTHO
01854 *              P L A C E   O F   T R E A T M E N T             *  ELTORTHO
01855 ****************************************************************  ELTORTHO
01856      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01857      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
01858                          AND                                      ELTORTHO
01859         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01860                                                  NOT =  ZERO      ELTORTHO
01861          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01862          MOVE +2                    TO  WS-CIA                    ELTORTHO
01863          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01864                                                                   ELTORTHO
01865      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01866      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
01867                           AND                                     ELTORTHO
01868         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01869                                                 NOT  =  ZERO      ELTORTHO
01870                           AND                                     ELTORTHO
01871         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
01872          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
01873          MOVE +2                    TO  WS-CIA                    ELTORTHO
01874          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
01875                                                                   ELTORTHO
01876      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01877      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
01878                           AND                                     ELTORTHO
01879         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01880                                                  NOT  =  ZERO     ELTORTHO
01881          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
01882          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
01883                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01884          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01885                               TO  CMF-CODE-VALUE                  ELTORTHO
01886          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
01887          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01888          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01889             THRU 9500-EXIT.                                       ELTORTHO
01890                                                                   ELTORTHO
01891      SET PLT-INDEX2  TO  2.                                       ELTORTHO
01892      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
01893                           AND                                     ELTORTHO
01894         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
01895                                                 NOT  =  ZERO      ELTORTHO
01896          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
01897          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
01898                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
01899          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01900                               TO  CMF-CODE-VALUE                  ELTORTHO
01901          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01902          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
01903          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
01904             THRU 9500-EXIT.                                       ELTORTHO
01905                                                                   ELTORTHO
01906      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
01907         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
01908          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
01909             THRU 9200-EXIT.                                       ELTORTHO
01910                                                                   ELTORTHO
01911      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
01912         SET PLT-INDEX2  TO  2                                     ELTORTHO
01913      ELSE                                                         ELTORTHO
01914         SET PLT-INDEX2  TO  1.                                    ELTORTHO
01915                                                                   ELTORTHO
01916  4110-EXIT.  EXIT.                                                ELTORTHO
01917 /                                                                 ELTORTHO
01918  4120-PRIC-METH.                                                  ELTORTHO
01919 ****************************************************************  ELTORTHO
01920 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTORTHO
01921 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTORTHO
01922 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTORTHO
01923 ****************************************************************  ELTORTHO
01924      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01925      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
01926         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01927                                                              '19' ELTORTHO
01928         MOVE +2             TO  WS-CIA                            ELTORTHO
01929         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
01930         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
01931                                                                   ELTORTHO
01932      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01933      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
01934         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01935                                                        '19' AND   ELTORTHO
01936         NOT WS-ADD-A-BLANK-LINE                                   ELTORTHO
01937         MOVE +2             TO  WS-CIA                            ELTORTHO
01938         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
01939         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
01940                                                                   ELTORTHO
01941      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01942      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
01943         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
01944                            AND                                    ELTORTHO
01945         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
01946         SET  PLT-INDEX2  TO  2                                    ELTORTHO
01947         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
01948                                                             ZERO  ELTORTHO
01949            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
01950            ADD +1  TO  WS-CIA                                     ELTORTHO
01951            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
01952                                                                   ELTORTHO
01953      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01954      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
01955         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
01956                            AND                                    ELTORTHO
01957         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTORTHO
01958         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTORTHO
01959         ADD +1  TO  WS-CIA                                        ELTORTHO
01960         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTORTHO
01961                                                                   ELTORTHO
01962      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTORTHO
01963         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
01964         SET  PLT-INDEX2  TO  2                                    ELTORTHO
01965         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
01966                                                             ZERO  ELTORTHO
01967            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
01968            ADD +1  TO  WS-CIA                                     ELTORTHO
01969            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
01970                                                                   ELTORTHO
01971      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
01972      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
01973         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
01974                                                            =  ZEROELTORTHO
01975            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01976                                                            =  ZEROELTORTHO
01977               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
01978            ELSE                                                   ELTORTHO
01979               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
01980          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01981                                                  TO  WS-PERCENTAGEELTORTHO
01982         ELSE                                                      ELTORTHO
01983            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
01984          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
01985                                                 TO  WS-PERCENTAGE.ELTORTHO
01986      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
01987         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
01988                                             ZERO AND  NOT =  '19' ELTORTHO
01989         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
01990         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
01991         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
01992                                                    CMF-CODE-VALUE ELTORTHO
01993         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
01994         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
01995         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
01996            THRU 9600-EXIT.                                        ELTORTHO
01997                                                                   ELTORTHO
01998      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
01999      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02000         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTORTHO
02001                                                               ZEROELTORTHO
02002            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02003                                                            =  ZEROELTORTHO
02004               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
02005            ELSE                                                   ELTORTHO
02006               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
02007          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02008                                                  TO  WS-PERCENTAGEELTORTHO
02009         ELSE                                                      ELTORTHO
02010            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
02011          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02012                                                 TO  WS-PERCENTAGE.ELTORTHO
02013      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
02014         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
02015                                             ZERO AND  NOT =  '19' ELTORTHO
02016         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02017         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
02018         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
02019                                                    CMF-CODE-VALUE ELTORTHO
02020         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTORTHO
02021         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
02022         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
02023            THRU 9600-EXIT.                                        ELTORTHO
02024                                                                   ELTORTHO
02025      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02026          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTORTHO
02027          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02028             THRU 9200-EXIT.                                       ELTORTHO
02029                                                                   ELTORTHO
02030      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02031         SET PLT-INDEX2  TO  2                                     ELTORTHO
02032      ELSE                                                         ELTORTHO
02033         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02034                                                                   ELTORTHO
02035  4120-EXIT.  EXIT.                                                ELTORTHO
02036 /                                                                 ELTORTHO
02037  4140-CERTIFICATION.                                              ELTORTHO
02038 ****************************************************************  ELTORTHO
02039 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTORTHO
02040 ****************************************************************  ELTORTHO
02041      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02042      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02043                          AND                                      ELTORTHO
02044         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02045                                                  NOT =  '00'      ELTORTHO
02046          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02047          MOVE +2                    TO  WS-CIA                    ELTORTHO
02048          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02049                                                                   ELTORTHO
02050      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02051      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02052                           AND                                     ELTORTHO
02053         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02054                                                 NOT  =  '00'      ELTORTHO
02055                           AND                                     ELTORTHO
02056         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02057          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02058          MOVE +2                    TO  WS-CIA                    ELTORTHO
02059          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02060                                                                   ELTORTHO
02061      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02062      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02063                           AND                                     ELTORTHO
02064         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02065                                                  NOT  =  '00'     ELTORTHO
02066          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTORTHO
02067          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
02068                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02069          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02070                               TO  CMF-CODE-VALUE                  ELTORTHO
02071          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02072          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02073          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02074             THRU 9500-EXIT.                                       ELTORTHO
02075                                                                   ELTORTHO
02076      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02077      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02078                           AND                                     ELTORTHO
02079         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02080                                                 NOT  =  '00'      ELTORTHO
02081          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTORTHO
02082          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
02083                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02084          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02085                               TO  CMF-CODE-VALUE                  ELTORTHO
02086          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02087          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02088          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02089             THRU 9500-EXIT.                                       ELTORTHO
02090                                                                   ELTORTHO
02091      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02092         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02093          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02094             THRU 9200-EXIT.                                       ELTORTHO
02095                                                                   ELTORTHO
02096      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02097         SET PLT-INDEX2  TO  2                                     ELTORTHO
02098      ELSE                                                         ELTORTHO
02099         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02100                                                                   ELTORTHO
02101  4140-EXIT.  EXIT.                                                ELTORTHO
02102 /                                                                 ELTORTHO
02103  4150-RECERTIFICATION.                                            ELTORTHO
02104 ****************************************************************  ELTORTHO
02105 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTORTHO
02106 ****************************************************************  ELTORTHO
02107      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02108      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02109                          AND                                      ELTORTHO
02110         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02111                                                  NOT =  ZERO      ELTORTHO
02112          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02113          MOVE +2                    TO  WS-CIA                    ELTORTHO
02114          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02115                                                                   ELTORTHO
02116      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02117      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02118                           AND                                     ELTORTHO
02119         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02120                                                 NOT  =  ZERO      ELTORTHO
02121                           AND                                     ELTORTHO
02122         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02123          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02124          MOVE +2                    TO  WS-CIA                    ELTORTHO
02125          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02126                                                                   ELTORTHO
02127      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02128      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02129                           AND                                     ELTORTHO
02130         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02131                                                  NOT  =  ZERO     ELTORTHO
02132          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTORTHO
02133          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
02134                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02135          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02136                               TO  CMF-CODE-VALUE                  ELTORTHO
02137          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02138          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02139          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02140             THRU 9500-EXIT.                                       ELTORTHO
02141                                                                   ELTORTHO
02142      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02143      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02144                           AND                                     ELTORTHO
02145         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02146                                                 NOT  =  ZERO      ELTORTHO
02147          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTORTHO
02148          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
02149                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02150          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02151                               TO  CMF-CODE-VALUE                  ELTORTHO
02152          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02153          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02154          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02155             THRU 9500-EXIT.                                       ELTORTHO
02156                                                                   ELTORTHO
02157      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02158         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02159          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02160             THRU 9200-EXIT.                                       ELTORTHO
02161                                                                   ELTORTHO
02162      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02163         SET PLT-INDEX2  TO  2                                     ELTORTHO
02164      ELSE                                                         ELTORTHO
02165         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02166                                                                   ELTORTHO
02167  4150-EXIT.  EXIT.                                                ELTORTHO
02168 /                                                                 ELTORTHO
02169  4155-REPR-REPL.                                                  ELTORTHO
02170 ****************************************************************  ELTORTHO
02171 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTORTHO
02172 ****************************************************************  ELTORTHO
02173      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02174      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02175                          AND                                      ELTORTHO
02176         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02177                                                  NOT =  ZERO      ELTORTHO
02178          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02179          MOVE +2                    TO  WS-CIA                    ELTORTHO
02180          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02181                                                                   ELTORTHO
02182      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02183      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02184                           AND                                     ELTORTHO
02185         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02186                                                 NOT  =  ZERO      ELTORTHO
02187                           AND                                     ELTORTHO
02188         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02189          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02190          MOVE +2                    TO  WS-CIA                    ELTORTHO
02191          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02192                                                                   ELTORTHO
02193      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02194      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02195                           AND                                     ELTORTHO
02196         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02197                                                  NOT  =  ZERO     ELTORTHO
02198          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTORTHO
02199          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
02200                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02201          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02202                               TO  CMF-CODE-VALUE                  ELTORTHO
02203          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02204          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02205          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02206             THRU 9500-EXIT.                                       ELTORTHO
02207                                                                   ELTORTHO
02208      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02209      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02210                           AND                                     ELTORTHO
02211         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02212                                                 NOT  =  ZERO      ELTORTHO
02213          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTORTHO
02214          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
02215                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02216          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02217                               TO  CMF-CODE-VALUE                  ELTORTHO
02218          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02219          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02220          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02221             THRU 9500-EXIT.                                       ELTORTHO
02222                                                                   ELTORTHO
02223      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02224         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02225          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02226             THRU 9200-EXIT.                                       ELTORTHO
02227                                                                   ELTORTHO
02228      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02229         SET PLT-INDEX2  TO  2                                     ELTORTHO
02230      ELSE                                                         ELTORTHO
02231         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02232                                                                   ELTORTHO
02233  4155-EXIT.  EXIT.                                                ELTORTHO
02234 /                                                                 ELTORTHO
02235  4160-SPILLOVR-COINS-N-DEDUC.                                     ELTORTHO
02236 ****************************************************************  ELTORTHO
02237 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTORTHO
02238 ****************************************************************  ELTORTHO
02239      MOVE +1  TO  WS-CIA.                                         ELTORTHO
02240                                                                   ELTORTHO
02241      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02242      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
02243                            AND                                    ELTORTHO
02244         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTORTHO
02245                                                  NOT =  '0'       ELTORTHO
02246         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02247         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTORTHO
02248                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
02249         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTORTHO
02250                                                TO  CMF-CODE-VALUE ELTORTHO
02251         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTORTHO
02252         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTORTHO
02253         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
02254            THRU 9500-EXIT                                         ELTORTHO
02255         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
02256            THRU 9200-EXIT.                                        ELTORTHO
02257 ****************************************************************  ELTORTHO
02258 *          S P I L L O V E R   D E D U C T I B L E             *  ELTORTHO
02259 ****************************************************************  ELTORTHO
02260      MOVE +1  TO  WS-CIA.                                         ELTORTHO
02261                                                                   ELTORTHO
02262      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02263      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
02264                            AND                                    ELTORTHO
02265         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02266                                                  NOT =  '0'       ELTORTHO
02267         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02268         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTORTHO
02269                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
02270         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
02271                                                 TO  CMF-CODE-VALUEELTORTHO
02272         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTORTHO
02273         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTORTHO
02274         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
02275            THRU 9500-EXIT                                         ELTORTHO
02276         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
02277            THRU 9200-EXIT.                                        ELTORTHO
02278                                                                   ELTORTHO
02279      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02280         SET PLT-INDEX2  TO  2                                     ELTORTHO
02281      ELSE                                                         ELTORTHO
02282         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02283                                                                   ELTORTHO
02284  4160-EXIT.  EXIT.                                                ELTORTHO
02285 /                                                                 ELTORTHO
02286  5000-PROFESSIONAL-OP.                                            ELTORTHO
02287 ****************************************************************  ELTORTHO
02288 *      ORTHOTIC PROFESSIONAL OUTPATIENT PROCESSING             *  ELTORTHO
02289 ****************************************************************  ELTORTHO
02290                                                                   ELTORTHO
02291      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTORTHO
02292                                                                   ELTORTHO
02293      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTORTHO
02294                                                                   ELTORTHO
02295      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTORTHO
02296         THRU 9100-EXIT.                                           ELTORTHO
02297                                                                   ELTORTHO
02298      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
02299      PERFORM WITH TEST BEFORE                                     ELTORTHO
02300              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTORTHO
02301              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTORTHO
02302         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTORTHO
02303         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTORTHO
02304         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTORTHO
02305         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTORTHO
02306      END-PERFORM.                                                 ELTORTHO
02307      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTORTHO
02308                                                                   ELTORTHO
02309                                                                   ELTORTHO
02310      PERFORM WITH TEST BEFORE                                     ELTORTHO
02311         VARYING WS-SUB FROM +1 BY +1                              ELTORTHO
02312         UNTIL   WS-SUB  >     WS-PROF-OP-CNT                      ELTORTHO
02313           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTORTHO
02314           MOVE WS-PROF-OP-BP (WS-SUB)                             ELTORTHO
02315                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTORTHO
02316            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTORTHO
02317                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTORTHO
02318      END-PERFORM.                                                 ELTORTHO
02319                                                                   ELTORTHO
02320      MOVE 'ORTHOTIC APPLIANCES      '   TO  SSB-TOPIC-PHRASE.     ELTORTHO
02321                                                                   ELTORTHO
02322      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTORTHO
02323                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
02324                     END-EXEC.                                     ELTORTHO
02325                                                                   ELTORTHO
02326      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
02327                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
02328                     END-EXEC.                                     ELTORTHO
02329                                                                   ELTORTHO
02330      IF PVN-COVG-NONE                                             ELTORTHO
02331          GO TO 5000-EXIT.                                         ELTORTHO
02332                                                                   ELTORTHO
02333      MOVE +1  TO  WS-CIA.                                         ELTORTHO
02334                                                                   ELTORTHO
02335      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTORTHO
02336                    PSP-PROVN-PRICING-METHD,                       ELTORTHO
02337                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTORTHO
02338                    PSP-TRANSF-OTHER-RESP-IND,                     ELTORTHO
02339                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTORTHO
02340                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTORTHO
02341                    PSP-SPILL-OVER-DED-APL-IND,                    ELTORTHO
02342                    PSP-CERTFN-REQRM-IND,                          ELTORTHO
02343                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTORTHO
02344                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTORTHO
02345                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTORTHO
02346                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTORTHO
02347                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTORTHO
02348                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTORTHO
02349                    PSE-CERTN-REPETN-REQRD-IND,                    ELTORTHO
02350                    PSE-REPR-REPLAC-RESTRN-IND.                    ELTORTHO
02351                                                                   ELTORTHO
02352      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTORTHO
02353                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
02354                     END-EXEC.                                     ELTORTHO
02355                                                                   ELTORTHO
02356      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTORTHO
02357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
02358          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTORTHO
02359                                                                   ELTORTHO
02360      PERFORM WITH TEST BEFORE                                     ELTORTHO
02361         VARYING WS-SUB  FROM  +1  BY  +1                          ELTORTHO
02362         UNTIL WS-SUB  >  WS-PROF-OP-CNT                           ELTORTHO
02363              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTORTHO
02364              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTORTHO
02365                   PERFORM 5100-BUILD-SCREEN-LINES THRU 5100-EXIT  ELTORTHO
02366              END-IF                                               ELTORTHO
02367      END-PERFORM.                                                 ELTORTHO
02368                                                                   ELTORTHO
02369  5000-EXIT.  EXIT.                                                ELTORTHO
02370                                                                   ELTORTHO
02371 /                                                                 ELTORTHO
02372  5100-BUILD-SCREEN-LINES.                                         ELTORTHO
02373      SET PLT-INDEX1  TO                                           ELTORTHO
02374              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTORTHO
02375                                                                   ELTORTHO
02376      IF WS-NOT-FIRST-TIME                                         ELTORTHO
02377         SET COF-NEW-PAGE TO TRUE                                  ELTORTHO
02378         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTORTHO
02379         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTORTHO
02380                   COMMAREA (DFHCOMMAREA)                          ELTORTHO
02381         END-EXEC                                                  ELTORTHO
02382      ELSE                                                         ELTORTHO
02383         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTORTHO
02384                                                                   ELTORTHO
02385      MOVE  +1  TO  WS-CIA.                                        ELTORTHO
02386                                                                   ELTORTHO
02387      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTORTHO
02388          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTORTHO
02389              SET PLT-INDEX2  TO  2                                ELTORTHO
02390          ELSE                                                     ELTORTHO
02391              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTORTHO
02392              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTORTHO
02393                 THRU 9200-EXIT                                    ELTORTHO
02394              GO TO 5100-EXIT                                      ELTORTHO
02395      ELSE                                                         ELTORTHO
02396          SET PLT-INDEX2  TO  1.                                   ELTORTHO
02397                                                                   ELTORTHO
02398      PERFORM 5105-LIST-BEN-PROV                                   ELTORTHO
02399         THRU 5105-EXIT.                                           ELTORTHO
02400                                                                   ELTORTHO
02401      PERFORM 5110-PLACE-OF-TREATMENT                              ELTORTHO
02402         THRU 5110-EXIT.                                           ELTORTHO
02403                                                                   ELTORTHO
02404      PERFORM 5120-PRIC-METH                                       ELTORTHO
02405         THRU 5120-EXIT.                                           ELTORTHO
02406                                                                   ELTORTHO
02407      PERFORM 5140-CERTIFICATION                                   ELTORTHO
02408         THRU 5140-EXIT.                                           ELTORTHO
02409                                                                   ELTORTHO
02410      PERFORM 5150-RECERTIFICATION                                 ELTORTHO
02411         THRU 5150-EXIT.                                           ELTORTHO
02412                                                                   ELTORTHO
02413      PERFORM 5155-REPR-REPL                                       ELTORTHO
02414         THRU 5155-EXIT.                                           ELTORTHO
02415                                                                   ELTORTHO
02416      PERFORM 5160-SPILLOVR-COINS-N-DEDUC                          ELTORTHO
02417         THRU 5160-EXIT.                                           ELTORTHO
02418                                                                   ELTORTHO
02419      PERFORM 2165-TRANS-OTHR-RESPON-IND                           ELTORTHO
02420         THRU 2165-EXIT.                                           ELTORTHO
02421                                                                   ELTORTHO
02422      PERFORM 7000-ALL-LEVEL-TABS                                  ELTORTHO
02423         THRU 7000-EXIT.                                           ELTORTHO
02424                                                                   ELTORTHO
02425  5100-EXIT.  EXIT.                                                ELTORTHO
02426 /                                                                 ELTORTHO
02427  5105-LIST-BEN-PROV.                                              ELTORTHO
02428 ****************************************************************  ELTORTHO
02429 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTORTHO
02430 ****************************************************************  ELTORTHO
02431      MOVE  +2               TO  WS-CIA.                           ELTORTHO
02432      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTORTHO
02433      MOVE ZERO              TO  WS-SUB2.                          ELTORTHO
02434      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTORTHO
02435                             TO  WS-SUB3.                          ELTORTHO
02436                                                                   ELTORTHO
02437      PERFORM 5106-ZERO-ALL-WITH-SAME-NO                           ELTORTHO
02438         THRU 5106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTORTHO
02439                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.ELTORTHO
02440                                                                   ELTORTHO
02441      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTORTHO
02442      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTORTHO
02443                                                                   ELTORTHO
02444      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
02445                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
02446                     END-EXEC.                                     ELTORTHO
02447                                                                   ELTORTHO
02448  5105-EXIT.  EXIT.                                                ELTORTHO
02449      SKIP3                                                        ELTORTHO
02450  5106-ZERO-ALL-WITH-SAME-NO.                                      ELTORTHO
02451                                                                   ELTORTHO
02452      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTORTHO
02453          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTORTHO
02454          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTORTHO
02455          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTORTHO
02456                            TO  CMF-CODE-VALUE                     ELTORTHO
02457          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02458          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTORTHO
02459          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02460             THRU 9500-EXIT                                        ELTORTHO
02461          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTORTHO
02462          ADD  +1    TO  WS-SUB2                                   ELTORTHO
02463          IF WS-CIA  >  20  OR  =  20                              ELTORTHO
02464              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTORTHO
02465              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTORTHO
02466                             COMMAREA (DFHCOMMAREA)                ELTORTHO
02467                             END-EXEC                              ELTORTHO
02468              MOVE  +1  TO  WS-CIA.                                ELTORTHO
02469                                                                   ELTORTHO
02470  5106-EXIT.  EXIT.                                                ELTORTHO
02471 /                                                                 ELTORTHO
02472  5110-PLACE-OF-TREATMENT.                                         ELTORTHO
02473 ****************************************************************  ELTORTHO
02474 *              P L A C E   O F   T R E A T M E N T             *  ELTORTHO
02475 ****************************************************************  ELTORTHO
02476      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02477      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02478                          AND                                      ELTORTHO
02479         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
02480                                                  NOT =  ZERO      ELTORTHO
02481          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02482          MOVE +2                    TO  WS-CIA                    ELTORTHO
02483          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02484                                                                   ELTORTHO
02485      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02486      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02487                           AND                                     ELTORTHO
02488         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
02489                                                 NOT  =  ZERO      ELTORTHO
02490                           AND                                     ELTORTHO
02491         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02492          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02493          MOVE +2                    TO  WS-CIA                    ELTORTHO
02494          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02495                                                                   ELTORTHO
02496      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02497      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02498                           AND                                     ELTORTHO
02499         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
02500                                                  NOT  =  ZERO     ELTORTHO
02501          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
02502          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
02503                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02504          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
02505                               TO  CMF-CODE-VALUE                  ELTORTHO
02506          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02507          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02508          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02509             THRU 9500-EXIT.                                       ELTORTHO
02510                                                                   ELTORTHO
02511      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02512      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02513                           AND                                     ELTORTHO
02514         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTORTHO
02515                                                 NOT  =  ZERO      ELTORTHO
02516          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTORTHO
02517          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTORTHO
02518                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02519          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTORTHO
02520                               TO  CMF-CODE-VALUE                  ELTORTHO
02521          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02522          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02523          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02524             THRU 9500-EXIT.                                       ELTORTHO
02525                                                                   ELTORTHO
02526      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02527         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02528          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02529             THRU 9200-EXIT.                                       ELTORTHO
02530                                                                   ELTORTHO
02531      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02532         SET PLT-INDEX2  TO  2                                     ELTORTHO
02533      ELSE                                                         ELTORTHO
02534         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02535                                                                   ELTORTHO
02536  5110-EXIT.  EXIT.                                                ELTORTHO
02537 /                                                                 ELTORTHO
02538  5120-PRIC-METH.                                                  ELTORTHO
02539 ****************************************************************  ELTORTHO
02540 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTORTHO
02541 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTORTHO
02542 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTORTHO
02543 ****************************************************************  ELTORTHO
02544      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02545      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02546         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
02547                                                              '19' ELTORTHO
02548         MOVE +2             TO  WS-CIA                            ELTORTHO
02549         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
02550         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
02551                                                                   ELTORTHO
02552      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02553      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
02554         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
02555                                                        '19' AND   ELTORTHO
02556         NOT WS-ADD-A-BLANK-LINE                                   ELTORTHO
02557         MOVE +2             TO  WS-CIA                            ELTORTHO
02558         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTORTHO
02559         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTORTHO
02560                                                                   ELTORTHO
02561      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02562      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02563         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
02564                            AND                                    ELTORTHO
02565         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02566         SET  PLT-INDEX2  TO  2                                    ELTORTHO
02567         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
02568                                                             ZERO  ELTORTHO
02569            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
02570            ADD +1  TO  WS-CIA                                     ELTORTHO
02571            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
02572                                                                   ELTORTHO
02573      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02574      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02575         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTORTHO
02576                            AND                                    ELTORTHO
02577         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTORTHO
02578         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTORTHO
02579         ADD +1  TO  WS-CIA                                        ELTORTHO
02580         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTORTHO
02581                                                                   ELTORTHO
02582      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTORTHO
02583         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02584         SET  PLT-INDEX2  TO  2                                    ELTORTHO
02585         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTORTHO
02586                                                             ZERO  ELTORTHO
02587            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTORTHO
02588            ADD +1  TO  WS-CIA                                     ELTORTHO
02589            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTORTHO
02590                                                                   ELTORTHO
02591      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02592      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02593         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
02594                                                            =  ZEROELTORTHO
02595            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02596                                                            =  ZEROELTORTHO
02597               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
02598            ELSE                                                   ELTORTHO
02599               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
02600          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02601                                                  TO  WS-PERCENTAGEELTORTHO
02602         ELSE                                                      ELTORTHO
02603            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
02604          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02605                                                 TO  WS-PERCENTAGE.ELTORTHO
02606      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02607         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
02608                                             ZERO AND  NOT =  '19' ELTORTHO
02609         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02610         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
02611         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
02612                                                    CMF-CODE-VALUE ELTORTHO
02613         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02614         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
02615         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
02616            THRU 9600-EXIT.                                        ELTORTHO
02617                                                                   ELTORTHO
02618      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02619      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02620         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTORTHO
02621                                                               ZEROELTORTHO
02622            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02623                                                            =  ZEROELTORTHO
02624               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTORTHO
02625            ELSE                                                   ELTORTHO
02626               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTORTHO
02627          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02628                                                  TO  WS-PERCENTAGEELTORTHO
02629         ELSE                                                      ELTORTHO
02630            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTORTHO
02631          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTORTHO
02632                                                 TO  WS-PERCENTAGE.ELTORTHO
02633      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTORTHO
02634         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTORTHO
02635                                             ZERO AND  NOT =  '19' ELTORTHO
02636         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02637         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTORTHO
02638         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTORTHO
02639                                                    CMF-CODE-VALUE ELTORTHO
02640         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTORTHO
02641         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTORTHO
02642         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTORTHO
02643            THRU 9600-EXIT.                                        ELTORTHO
02644                                                                   ELTORTHO
02645      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02646          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTORTHO
02647          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02648             THRU 9200-EXIT.                                       ELTORTHO
02649                                                                   ELTORTHO
02650      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02651         SET PLT-INDEX2  TO  2                                     ELTORTHO
02652      ELSE                                                         ELTORTHO
02653         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02654                                                                   ELTORTHO
02655  5120-EXIT.  EXIT.                                                ELTORTHO
02656 /                                                                 ELTORTHO
02657  5140-CERTIFICATION.                                              ELTORTHO
02658 ****************************************************************  ELTORTHO
02659 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTORTHO
02660 ****************************************************************  ELTORTHO
02661      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02662      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02663                          AND                                      ELTORTHO
02664         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02665                                                  NOT =  '00'      ELTORTHO
02666          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02667          MOVE +2                    TO  WS-CIA                    ELTORTHO
02668          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02669                                                                   ELTORTHO
02670      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02671      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02672                           AND                                     ELTORTHO
02673         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02674                                                 NOT  =  '00'      ELTORTHO
02675                           AND                                     ELTORTHO
02676         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02677          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02678          MOVE +2                    TO  WS-CIA                    ELTORTHO
02679          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02680                                                                   ELTORTHO
02681      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02682      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02683                           AND                                     ELTORTHO
02684         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02685                                                  NOT  =  '00'     ELTORTHO
02686          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTORTHO
02687          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
02688                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02689          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02690                               TO  CMF-CODE-VALUE                  ELTORTHO
02691          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02692          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02693          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02694             THRU 9500-EXIT.                                       ELTORTHO
02695                                                                   ELTORTHO
02696      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02697      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02698                           AND                                     ELTORTHO
02699         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTORTHO
02700                                                 NOT  =  '00'      ELTORTHO
02701          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTORTHO
02702          MOVE 'CERTFN-REQRM-IND'                                  ELTORTHO
02703                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02704          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02705                               TO  CMF-CODE-VALUE                  ELTORTHO
02706          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02707          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02708          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02709             THRU 9500-EXIT.                                       ELTORTHO
02710                                                                   ELTORTHO
02711      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02712         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02713          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02714             THRU 9200-EXIT.                                       ELTORTHO
02715                                                                   ELTORTHO
02716      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02717         SET PLT-INDEX2  TO  2                                     ELTORTHO
02718      ELSE                                                         ELTORTHO
02719         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02720                                                                   ELTORTHO
02721  5140-EXIT.  EXIT.                                                ELTORTHO
02722 /                                                                 ELTORTHO
02723  5150-RECERTIFICATION.                                            ELTORTHO
02724 ****************************************************************  ELTORTHO
02725 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTORTHO
02726 ****************************************************************  ELTORTHO
02727      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02728      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02729                          AND                                      ELTORTHO
02730         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02731                                                  NOT =  ZERO      ELTORTHO
02732          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02733          MOVE +2                    TO  WS-CIA                    ELTORTHO
02734          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02735                                                                   ELTORTHO
02736      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02737      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02738                           AND                                     ELTORTHO
02739         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02740                                                 NOT  =  ZERO      ELTORTHO
02741                           AND                                     ELTORTHO
02742         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02743          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02744          MOVE +2                    TO  WS-CIA                    ELTORTHO
02745          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02746                                                                   ELTORTHO
02747      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02748      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02749                           AND                                     ELTORTHO
02750         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02751                                                  NOT  =  ZERO     ELTORTHO
02752          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTORTHO
02753          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
02754                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02755          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02756                               TO  CMF-CODE-VALUE                  ELTORTHO
02757          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02758          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02759          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02760             THRU 9500-EXIT.                                       ELTORTHO
02761                                                                   ELTORTHO
02762      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02763      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02764                           AND                                     ELTORTHO
02765         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02766                                                 NOT  =  ZERO      ELTORTHO
02767          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTORTHO
02768          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTORTHO
02769                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02770          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02771                               TO  CMF-CODE-VALUE                  ELTORTHO
02772          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02773          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02774          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02775             THRU 9500-EXIT.                                       ELTORTHO
02776                                                                   ELTORTHO
02777      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02778         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02779          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02780             THRU 9200-EXIT.                                       ELTORTHO
02781                                                                   ELTORTHO
02782      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02783         SET PLT-INDEX2  TO  2                                     ELTORTHO
02784      ELSE                                                         ELTORTHO
02785         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02786                                                                   ELTORTHO
02787  5150-EXIT.  EXIT.                                                ELTORTHO
02788 /                                                                 ELTORTHO
02789  5155-REPR-REPL.                                                  ELTORTHO
02790 ****************************************************************  ELTORTHO
02791 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTORTHO
02792 ****************************************************************  ELTORTHO
02793      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02794      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02795                          AND                                      ELTORTHO
02796         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02797                                                  NOT =  ZERO      ELTORTHO
02798          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02799          MOVE +2                    TO  WS-CIA                    ELTORTHO
02800          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02801                                                                   ELTORTHO
02802      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02803      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02804                           AND                                     ELTORTHO
02805         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02806                                                 NOT  =  ZERO      ELTORTHO
02807                           AND                                     ELTORTHO
02808         NOT  WS-ADD-A-BLANK-LINE                                  ELTORTHO
02809          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTORTHO
02810          MOVE +2                    TO  WS-CIA                    ELTORTHO
02811          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTORTHO
02812                                                                   ELTORTHO
02813      SET  PLT-INDEX2  TO  1.                                      ELTORTHO
02814      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTORTHO
02815                           AND                                     ELTORTHO
02816         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02817                                                  NOT  =  ZERO     ELTORTHO
02818          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTORTHO
02819          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
02820                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02821          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02822                               TO  CMF-CODE-VALUE                  ELTORTHO
02823          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTORTHO
02824          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02825          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02826             THRU 9500-EXIT.                                       ELTORTHO
02827                                                                   ELTORTHO
02828      SET PLT-INDEX2  TO  2.                                       ELTORTHO
02829      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTORTHO
02830                           AND                                     ELTORTHO
02831         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02832                                                 NOT  =  ZERO      ELTORTHO
02833          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTORTHO
02834          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTORTHO
02835                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTORTHO
02836          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTORTHO
02837                               TO  CMF-CODE-VALUE                  ELTORTHO
02838          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTORTHO
02839          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTORTHO
02840          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTORTHO
02841             THRU 9500-EXIT.                                       ELTORTHO
02842                                                                   ELTORTHO
02843      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02844         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02845          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTORTHO
02846             THRU 9200-EXIT.                                       ELTORTHO
02847                                                                   ELTORTHO
02848      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02849         SET PLT-INDEX2  TO  2                                     ELTORTHO
02850      ELSE                                                         ELTORTHO
02851         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02852                                                                   ELTORTHO
02853  5155-EXIT.  EXIT.                                                ELTORTHO
02854 /                                                                 ELTORTHO
02855  5160-SPILLOVR-COINS-N-DEDUC.                                     ELTORTHO
02856 ****************************************************************  ELTORTHO
02857 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTORTHO
02858 ****************************************************************  ELTORTHO
02859      MOVE +1  TO  WS-CIA.                                         ELTORTHO
02860                                                                   ELTORTHO
02861      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02862      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
02863                            AND                                    ELTORTHO
02864         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTORTHO
02865                                                  NOT =  '0'       ELTORTHO
02866         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02867         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTORTHO
02868                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
02869         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTORTHO
02870                                                TO  CMF-CODE-VALUE ELTORTHO
02871         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTORTHO
02872         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTORTHO
02873         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
02874            THRU 9500-EXIT                                         ELTORTHO
02875         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
02876            THRU 9200-EXIT.                                        ELTORTHO
02877 ****************************************************************  ELTORTHO
02878 *          S P I L L O V E R   D E D U C T I B L E             *  ELTORTHO
02879 ****************************************************************  ELTORTHO
02880      MOVE +1  TO  WS-CIA.                                         ELTORTHO
02881                                                                   ELTORTHO
02882      SET  PLT-INDEX2  TO  2.                                      ELTORTHO
02883      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTORTHO
02884                            AND                                    ELTORTHO
02885         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTORTHO
02886                                                  NOT =  '0'       ELTORTHO
02887         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTORTHO
02888         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTORTHO
02889                                           CMF-ELEMENT-SYSTEM-NAME ELTORTHO
02890         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTORTHO
02891                                                 TO  CMF-CODE-VALUEELTORTHO
02892         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTORTHO
02893         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTORTHO
02894         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTORTHO
02895            THRU 9500-EXIT                                         ELTORTHO
02896         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTORTHO
02897            THRU 9200-EXIT.                                        ELTORTHO
02898                                                                   ELTORTHO
02899      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTORTHO
02900         SET PLT-INDEX2  TO  2                                     ELTORTHO
02901      ELSE                                                         ELTORTHO
02902         SET PLT-INDEX2  TO  1.                                    ELTORTHO
02903                                                                   ELTORTHO
02904  5160-EXIT.  EXIT.                                                ELTORTHO
02905 /                                                                 ELTORTHO
02906  7000-ALL-LEVEL-TABS.                                             ELTORTHO
02907 ****************************************************************  ELTORTHO
02908 *                  A A R   T A B U L A R                       *  ELTORTHO
02909 ****************************************************************  ELTORTHO
02910      SET PLT-INDEX2  TO  1.                                       ELTORTHO
02911      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02912         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
02913                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02914         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02915         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTORTHO
02916         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTORTHO
02917      ELSE                                                         ELTORTHO
02918         SET PLT-INDEX2  TO  2                                     ELTORTHO
02919         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
02920            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
02921                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02922            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTORTHO
02923            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTORTHO
02924            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTORTHO
02925                                                                   ELTORTHO
02926      IF WS-ADD-A-BLANK-LINE                                       ELTORTHO
02927         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTORTHO
02928         MOVE 1  TO  WS-CIA                                        ELTORTHO
02929         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTORTHO
02930             COMMAREA(DFHCOMMAREA)                                 ELTORTHO
02931         END-EXEC.                                                 ELTORTHO
02932 *--------------------------------------------------------------*  ELTORTHO
02933 *                  P P F   T A B U L A R                       *  ELTORTHO
02934 *--------------------------------------------------------------*  ELTORTHO
02935      SET PLT-INDEX2  TO  1.                                       ELTORTHO
02936      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02937         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
02938                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02939         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTORTHO
02940                                             KWA-GCTABULR-KEY      ELTORTHO
02941         PERFORM 9900-GET-TABULAR-RECORD                           ELTORTHO
02942            THRU 9900-EXIT                                         ELTORTHO
02943         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTORTHO
02944               COMMAREA(DFHCOMMAREA)                               ELTORTHO
02945         END-EXEC                                                  ELTORTHO
02946      ELSE                                                         ELTORTHO
02947         SET PLT-INDEX2  TO  2                                     ELTORTHO
02948         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
02949            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
02950                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02951          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTORTHO
02952                                             KWA-GCTABULR-KEY      ELTORTHO
02953            PERFORM 9900-GET-TABULAR-RECORD                        ELTORTHO
02954               THRU 9900-EXIT                                      ELTORTHO
02955            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTORTHO
02956                  COMMAREA(DFHCOMMAREA)                            ELTORTHO
02957            END-EXEC.                                              ELTORTHO
02958 *--------------------------------------------------------------*  ELTORTHO
02959 *                  P V E   T A B U L A R                       *  ELTORTHO
02960 *--------------------------------------------------------------*  ELTORTHO
02961                                                                   ELTORTHO
02962      MOVE  +2     TO  WS-CIA.                                     ELTORTHO
02963      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTORTHO
02964      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTORTHO
02965         THRU 9200-EXIT.                                           ELTORTHO
02966                                                                   ELTORTHO
02967 *--------------------------------------------------------------*  ELTORTHO
02968 *                  A B M   T A B U L A R                       *  ELTORTHO
02969 *--------------------------------------------------------------*  ELTORTHO
02970      SET PLT-INDEX2  TO  1.                                       ELTORTHO
02971      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTORTHO
02972                              AND                                  ELTORTHO
02973         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
02974                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02975         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTORTHO
02976                                              KWA-GCTABULR-KEY     ELTORTHO
02977         PERFORM 9900-GET-TABULAR-RECORD                           ELTORTHO
02978            THRU 9900-EXIT                                         ELTORTHO
02979         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTORTHO
02980               COMMAREA(DFHCOMMAREA)                               ELTORTHO
02981         END-EXEC                                                  ELTORTHO
02982      ELSE                                                         ELTORTHO
02983         SET PLT-INDEX2  TO  2                                     ELTORTHO
02984         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
02985            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
02986                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
02987          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTORTHO
02988                                             KWA-GCTABULR-KEY      ELTORTHO
02989            PERFORM 9900-GET-TABULAR-RECORD                        ELTORTHO
02990               THRU 9900-EXIT                                      ELTORTHO
02991            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTORTHO
02992                  COMMAREA(DFHCOMMAREA)                            ELTORTHO
02993            END-EXEC.                                              ELTORTHO
02994 *--------------------------------------------------------------*  ELTORTHO
02995 *                  A C L   T A B U L A R                       *  ELTORTHO
02996 *--------------------------------------------------------------*  ELTORTHO
02997      SET PLT-INDEX2  TO  1.                                       ELTORTHO
02998      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
02999         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
03000                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03001         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTORTHO
03002                                              KWA-GCTABULR-KEY     ELTORTHO
03003         PERFORM 9900-GET-TABULAR-RECORD                           ELTORTHO
03004            THRU 9900-EXIT                                         ELTORTHO
03005         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTORTHO
03006               COMMAREA(DFHCOMMAREA)                               ELTORTHO
03007         END-EXEC                                                  ELTORTHO
03008      ELSE                                                         ELTORTHO
03009         SET PLT-INDEX2  TO  2                                     ELTORTHO
03010         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
03011            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
03012                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03013          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTORTHO
03014                                              KWA-GCTABULR-KEY     ELTORTHO
03015            PERFORM 9900-GET-TABULAR-RECORD                        ELTORTHO
03016               THRU 9900-EXIT                                      ELTORTHO
03017            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTORTHO
03018                  COMMAREA(DFHCOMMAREA)                            ELTORTHO
03019            END-EXEC.                                              ELTORTHO
03020 *--------------------------------------------------------------*  ELTORTHO
03021 *                  A D L   T A B U L A R                       *  ELTORTHO
03022 *--------------------------------------------------------------*  ELTORTHO
03023      SET PLT-INDEX2  TO  1.                                       ELTORTHO
03024      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
03025         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
03026                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03027         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTORTHO
03028                                             KWA-GCTABULR-KEY      ELTORTHO
03029         PERFORM 9900-GET-TABULAR-RECORD                           ELTORTHO
03030            THRU 9900-EXIT                                         ELTORTHO
03031         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTORTHO
03032               COMMAREA(DFHCOMMAREA)                               ELTORTHO
03033         END-EXEC                                                  ELTORTHO
03034      ELSE                                                         ELTORTHO
03035         SET PLT-INDEX2  TO  2                                     ELTORTHO
03036         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
03037            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
03038                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03039          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTORTHO
03040                                               KWA-GCTABULR-KEY    ELTORTHO
03041            PERFORM 9900-GET-TABULAR-RECORD                        ELTORTHO
03042               THRU 9900-EXIT                                      ELTORTHO
03043            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTORTHO
03044                  COMMAREA(DFHCOMMAREA)                            ELTORTHO
03045            END-EXEC.                                              ELTORTHO
03046 *--------------------------------------------------------------*  ELTORTHO
03047 *                  A O L   T A B U L A R                       *  ELTORTHO
03048 *--------------------------------------------------------------*  ELTORTHO
03049      SET PLT-INDEX2  TO  1.                                       ELTORTHO
03050      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTORTHO
03051         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTORTHO
03052                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03053         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTORTHO
03054                                             KWA-GCTABULR-KEY      ELTORTHO
03055         PERFORM 9900-GET-TABULAR-RECORD                           ELTORTHO
03056            THRU 9900-EXIT                                         ELTORTHO
03057         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTORTHO
03058               COMMAREA(DFHCOMMAREA)                               ELTORTHO
03059         END-EXEC                                                  ELTORTHO
03060      ELSE                                                         ELTORTHO
03061         SET PLT-INDEX2  TO  2                                     ELTORTHO
03062         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTORTHO
03063            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTORTHO
03064                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTORTHO
03065          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTORTHO
03066                                              KWA-GCTABULR-KEY     ELTORTHO
03067            PERFORM 9900-GET-TABULAR-RECORD                        ELTORTHO
03068               THRU 9900-EXIT                                      ELTORTHO
03069            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTORTHO
03070                  COMMAREA(DFHCOMMAREA)                            ELTORTHO
03071            END-EXEC.                                              ELTORTHO
03072                                                                   ELTORTHO
03073 *--------------------------------------------------------------*  ELTORTHO
03074 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTORTHO
03075 *--------------------------------------------------------------*  ELTORTHO
03076                                                                   ELTORTHO
03077      ADD   +2     TO  WS-CIA.                                     ELTORTHO
03078      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTORTHO
03079      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTORTHO
03080         THRU 9200-EXIT.                                           ELTORTHO
03081                                                                   ELTORTHO
03082  7000-EXIT.  EXIT.                                                ELTORTHO
03083 /                                                                 ELTORTHO
03084 ****************************************************************  ELTORTHO
03085 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTORTHO
03086 ****************************************************************  ELTORTHO
03087  9100-HEADER-OUTPUT-REQUEST.                                      ELTORTHO
03088                                                                   ELTORTHO
03089      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTORTHO
03090      SET COF-NEW-PAGE TO TRUE.                                    ELTORTHO
03091                                                                   ELTORTHO
03092      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
03093                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
03094                     END-EXEC.                                     ELTORTHO
03095                                                                   ELTORTHO
03096  9100-EXIT.  EXIT.                                                ELTORTHO
03097      SKIP3                                                        ELTORTHO
03098 ****************************************************************  ELTORTHO
03099 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTORTHO
03100 ****************************************************************  ELTORTHO
03101  9200-TEXT-OUTPUT-REQUEST.                                        ELTORTHO
03102                                                                   ELTORTHO
03103      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTORTHO
03104      MOVE +0      TO  COF-NBR-HDR-LINES                           ELTORTHO
03105                        WS-CIA.                                    ELTORTHO
03106      SET COF-CONTINUE TO TRUE.                                    ELTORTHO
03107                                                                   ELTORTHO
03108      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTORTHO
03109                     COMMAREA (DFHCOMMAREA)                        ELTORTHO
03110                     END-EXEC.                                     ELTORTHO
03111                                                                   ELTORTHO
03112  9200-EXIT.  EXIT.                                                ELTORTHO
03113 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTORTHO
03114  9500-CALL-CODES-MANUAL-LONG.                                     ELTORTHO
03115                                                                   ELTORTHO
03116      INITIALIZE CMF-RETURN-CODE                                   ELTORTHO
03117                 TCAR-FROM-AREA.                                   ELTORTHO
03118                                                                   ELTORTHO
03119      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTORTHO
03120                       COMMAREA(DFHCOMMAREA)                       ELTORTHO
03121      END-EXEC.                                                    ELTORTHO
03122                                                                   ELTORTHO
03123      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTORTHO
03124      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
03125          ADDRESS OF CMF-DESCR.                                    ELTORTHO
03126                                                                   ELTORTHO
03127      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTORTHO
03128         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTORTHO
03129         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTORTHO
03130      ELSE                                                         ELTORTHO
03131         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTORTHO
03132                                                                   ELTORTHO
03133      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTORTHO
03134          VARYING WS-SUB1 FROM 1 BY 1                              ELTORTHO
03135          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTORTHO
03136                                                                   ELTORTHO
03137      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTORTHO
03138                                                                   ELTORTHO
03139      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTORTHO
03140      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTORTHO
03141      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTORTHO
03142                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTORTHO
03143                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTORTHO
03144      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTORTHO
03145                                                                   ELTORTHO
03146      IF WS-MOVE-LINES-TO-CIA                                      ELTORTHO
03147         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTORTHO
03148            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTORTHO
03149                                             WS-TEMP-NOT-USED-CNT  ELTORTHO
03150            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTORTHO
03151               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTORTHO
03152                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTORTHO
03153            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTORTHO
03154            ADD +1  TO  WS-CIA                                     ELTORTHO
03155            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTORTHO
03156         ELSE                                                      ELTORTHO
03157            ADD +1  TO  WS-CIA                                     ELTORTHO
03158            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTORTHO
03159                                                                   ELTORTHO
03160      IF WS-MOVE-LINES-TO-CIA                                      ELTORTHO
03161         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTORTHO
03162            PERFORM 9530-MOVE-LINES-OUT THRU 9530-EXIT             ELTORTHO
03163                VARYING WS-SUB1 FROM 2 BY 1                        ELTORTHO
03164                UNTIL WS-SUB1 GREATER THAN TCAR-OUTPUT-FIELDS-USED ELTORTHO
03165         ELSE                                                      ELTORTHO
03166            CONTINUE                                               ELTORTHO
03167      ELSE                                                         ELTORTHO
03168         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTORTHO
03169                                                                   ELTORTHO
03170  9500-EXIT.  EXIT.                                                ELTORTHO
03171                                                                   ELTORTHO
03172  9530-MOVE-LINES-OUT.                                             ELTORTHO
03173      ADD +1 TO WS-CIA.                                            ELTORTHO
03174      MOVE TCAR-OPF-DATA (WS-SUB1) TO COF-DTL-LINE (WS-CIA).       ELTORTHO
03175  9530-EXIT.  EXIT.                                                ELTORTHO
03176                                                                   ELTORTHO
03177  9540-MOVE-LINES-OUT.                                             ELTORTHO
03178      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTORTHO
03179             ' ' DELIMITED BY SIZE                                 ELTORTHO
03180             CMF-DESCR-LINE (WS-SUB1) DELIMITED BY SIZE            ELTORTHO
03181      INTO TCAR-FROM-AREA.                                         ELTORTHO
03182                                                                   ELTORTHO
03183  9540-EXIT.  EXIT.                                                ELTORTHO
03184                                                                   ELTORTHO
03185  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTORTHO
03186      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTORTHO
03187      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTORTHO
03188                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTORTHO
03189                                                                   ELTORTHO
03190  9550-EXIT.  EXIT.                                                ELTORTHO
03191                                                                   ELTORTHO
03192 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTORTHO
03193  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTORTHO
03194                                                                   ELTORTHO
03195      INITIALIZE CMF-RETURN-CODE                                   ELTORTHO
03196                 TCAR-FROM-AREA.                                   ELTORTHO
03197                                                                   ELTORTHO
03198      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTORTHO
03199                       COMMAREA(DFHCOMMAREA)                       ELTORTHO
03200      END-EXEC.                                                    ELTORTHO
03201                                                                   ELTORTHO
03202      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTORTHO
03203      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
03204          ADDRESS OF CMF-DESCR.                                    ELTORTHO
03205                                                                   ELTORTHO
03206      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTORTHO
03207         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTORTHO
03208         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTORTHO
03209      ELSE                                                         ELTORTHO
03210         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTORTHO
03211                                                                   ELTORTHO
03212      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTORTHO
03213             WS-PERCENT-FLD DELIMITED BY SIZE                      ELTORTHO
03214      INTO TCAR-FROM-AREA.                                         ELTORTHO
03215                                                                   ELTORTHO
03216      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTORTHO
03217          VARYING WS-SUB1 FROM 1 BY 1                              ELTORTHO
03218          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTORTHO
03219                                                                   ELTORTHO
03220                                                                   ELTORTHO
03221      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTORTHO
03222                                                                   ELTORTHO
03223      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTORTHO
03224      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTORTHO
03225      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTORTHO
03226                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTORTHO
03227                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTORTHO
03228      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTORTHO
03229                                                                   ELTORTHO
03230      IF WS-MOVE-LINES-TO-CIA                                      ELTORTHO
03231         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTORTHO
03232            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTORTHO
03233                                             WS-TEMP-NOT-USED-CNT  ELTORTHO
03234            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTORTHO
03235               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTORTHO
03236                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTORTHO
03237            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTORTHO
03238            ADD +1  TO  WS-CIA                                     ELTORTHO
03239            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTORTHO
03240         ELSE                                                      ELTORTHO
03241            ADD +1  TO  WS-CIA                                     ELTORTHO
03242            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTORTHO
03243                                                                   ELTORTHO
03244      IF WS-MOVE-LINES-TO-CIA                                      ELTORTHO
03245         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTORTHO
03246            PERFORM 9530-MOVE-LINES-OUT THRU 9530-EXIT             ELTORTHO
03247                VARYING WS-SUB1 FROM 2 BY 1                        ELTORTHO
03248                UNTIL WS-SUB1 GREATER THAN TCAR-OUTPUT-FIELDS-USED ELTORTHO
03249         ELSE                                                      ELTORTHO
03250            CONTINUE                                               ELTORTHO
03251      ELSE                                                         ELTORTHO
03252         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTORTHO
03253                                                                   ELTORTHO
03254  9600-EXIT.  EXIT.                                                ELTORTHO
03255                                                                   ELTORTHO
03256 /                                                                 ELTORTHO
03257 ***************************************************************** ELTORTHO
03258 *            G E T   T A B U L A R   R E C O R D                  ELTORTHO
03259 *                                                                 ELTORTHO
03260 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTORTHO
03261 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTORTHO
03262 *  TO DISPLAY.                                                    ELTORTHO
03263 *                                                                 ELTORTHO
03264 ***************************************************************** ELTORTHO
03265  9900-GET-TABULAR-RECORD.                                         ELTORTHO
03266                                                                   ELTORTHO
03267      SET CIA-GCTABULR-DDN TO TRUE.                                ELTORTHO
03268      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTORTHO
03269          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTORTHO
03270                                                                   ELTORTHO
03271      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTORTHO
03272      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTORTHO
03273      SET IOP-RD                          TO TRUE.                 ELTORTHO
03274      SET IOP-FCQ-NONE                    TO TRUE.                 ELTORTHO
03275      SET IOP-KVQ-NONE                    TO TRUE.                 ELTORTHO
03276      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTORTHO
03277                                                                   ELTORTHO
03278      EXEC CICS LINK                                               ELTORTHO
03279                PROGRAM ('ELUIOPGM')                               ELTORTHO
03280                COMMAREA (DFHCOMMAREA)                             ELTORTHO
03281      END-EXEC.                                                    ELTORTHO
03282                                                                   ELTORTHO
03283      IF IOP-RC-NOTFND                                             ELTORTHO
03284         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTORTHO
03285         EXEC CICS ABEND                                           ELTORTHO
03286                   ABCODE(CIA-ABCODE)                              ELTORTHO
03287         END-EXEC                                                  ELTORTHO
03288      ELSE                                                         ELTORTHO
03289          IF NOT IOP-RC-OK                                         ELTORTHO
03290             SET CIA-AB-CRITIO TO TRUE                             ELTORTHO
03291             EXEC CICS ABEND                                       ELTORTHO
03292                       ABCODE(CIA-ABCODE)                          ELTORTHO
03293             END-EXEC                                              ELTORTHO
03294          END-IF                                                   ELTORTHO
03295      END-IF.                                                      ELTORTHO
03296  9900-EXIT.  EXIT.                                                ELTORTHO
03297                                                                   ELTORTHO
03298      COPY ELSTCOMP.                                               ELTORTHO
