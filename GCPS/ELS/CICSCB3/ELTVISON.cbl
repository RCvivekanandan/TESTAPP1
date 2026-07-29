00001 *      LAST MAINTENANCE TIME: 11.06.00  DATE: 07/21/86            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTVISON
00003  PROGRAM-ID.    ELTVISON.                                            LV001
00004  AUTHOR.        LUCY TORRES.                                      ELTVISON
00005  DATE-WRITTEN.  07/21/86                                          ELTVISON
00006  DATE-COMPILED.                                                   ELTVISON
00007      SKIP3                                                        ELTVISON
00008 ****************************************************************  ELTVISON
00009 *      ELTVISON - ELS:  VISON CARE TOPIC PROGRAM               *  ELTVISON
00010 ****************************************************************  ELTVISON
00011      SKIP3                                                        ELTVISON
00012 ****************************************************************  ELTVISON
00013 *              U P D A T E   H I S T O R Y                     *  ELTVISON
00014 *                                                              *  ELTVISON
00015 *   DATE    PGM  DESCRIPTION                                   *  ELTVISON
00016 * --------  ---  --------------------------------------------- *  ELTVISON
00017 * 07/21/86  LET  ORIGINAL VERSION                              *  ELTVISON
00018 * 07/22/86  LET  MESSAGE FIX UP                                *  ELTVISON
00019 * 08/13/86  LET  USING A HEADER LINE FROM THE PROLOG           *  ELTVISON
00020 * 09/23/86  NAC  VS COBOL II CONVERSION                        *  ELTVISON
00021 * 11/25/86  LET  CHANGED CODE TO ACCOMMODATE THE MOVING OF THE *  ELTVISON
00022 *                CERTIFICATION REQUIREMENT INDICATOR FROM THE  *  ELTVISON
00023 *                FORMAT TYPE SECTION TO THE COMMON SECTION     *  ELTVISON
00024 * 04/07/87  NAC  SHOW CODE VALUE INSTEAD OF TRANSLATION FOR    *  ELTVISON
00025 *                EXCEPTION SCHEDULE INDICATOR.                 *  ELTVISON
00026 * 10/19/87  NAC  REWORD PHRASE FOR COVERED BENEFITS.              ELTVISON
00027 * 11/02/88  NAC  INCLUDE ADDITIONAL PROVISIONS; INCLUDE NEW    *  ELTVISON
00028 *                STORAGE ENHANCEMENTS.                         *  ELTVISON
00029 * 10/06/89  RKH  ADDED TRANSFER TO OTHER RESPON                *  ELTVISON
00030 * 10/15/90  GEM  ADDED BENEFIT PROVISION IDS.                  *  ELTVISON
00031 *                                                              *  ELTVISON
00032 * 11/16/90  GEM  CHANGE PLP-TRANSF-OTHER-RESP-IND COMPARE TO   *  ELTVISON
00033 *                THE LITERAL ZERO INSTEAD OF THE DIGIT '0'.    *  ELTVISON
00034 ****************************************************************  ELTVISON
00035      SKIP3                                                        ELTVISON
00036  ENVIRONMENT DIVISION.                                            ELTVISON
00037      SKIP3                                                        ELTVISON
00038  DATA DIVISION.                                                   ELTVISON
00039      TITLE ' WORKING STORAGE --- ELTVISON'.                       ELTVISON
00040  WORKING-STORAGE SECTION.                                         ELTVISON
00041  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTVISON
00042          '** ELTVISON WS BEGINS **'.                              ELTVISON
00043                                                                   ELTVISON
00044 ****************************************************************  ELTVISON
00045 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTVISON
00046 ****************************************************************  ELTVISON
00047  01  WORK-FIELDS.                                                 ELTVISON
00048      05  WS-PARA-ID              PIC  X(04).                      ELTVISON
00049      05  WS-HEX-00               PIC  X(01).                      ELTVISON
00050      05  WS-CHAR-0               PIC  X(01).                      ELTVISON
00051      05  WS-SUB                  PIC S9(03) COMP VALUE +0.        ELTVISON
00052      05  WS-SUB1                 PIC S9(03) COMP VALUE +0.        ELTVISON
00053      05  WS-SUB3                 PIC S9(03) COMP VALUE +0.        ELTVISON
00054      05  WS-CIA                  PIC S9(03) COMP VALUE +0.        ELTVISON
00055      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP.                 ELTVISON
00056      05  WS-REC-LEN              PIC S9(04) COMP   VALUE +0.      ELTVISON
00057      05  WS-PERCENT-FLD.                                          ELTVISON
00058        10  WS-PERCENTAGE         PIC ZZ9.                         ELTVISON
00059        10  WS-PERCENT-SIGN       PIC X.                           ELTVISON
00060      05  WS-EXPLANATION-IND      PIC S9 COMP.                     ELTVISON
00061          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTVISON
00062          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTVISON
00063          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTVISON
00064          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTVISON
00065          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTVISON
00066          88  WS-NO-EXPLANATION             VALUE +0.              ELTVISON
00067      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP.                     ELTVISON
00068      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP.                     ELTVISON
00069                                                                   ELTVISON
00070  01  WS-EXPLAINS.                                                 ELTVISON
00071    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTVISON
00072    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTVISON
00073    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTVISON
00074    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTVISON
00075                                                                   ELTVISON
00076  01  SWITCHES.                                                    ELTVISON
00077      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTVISON
00078          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTVISON
00079      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTVISON
00080          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTVISON
00081      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTVISON
00082          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTVISON
00083      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTVISON
00084          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTVISON
00085                                                                   ELTVISON
00086 /--------------------------------------------------------------*  ELTVISON
00087 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTVISON
00088 *--------------------------------------------------------------*  ELTVISON
00089  01  TABLE-MAX                   PIC S9(03) VALUE +30 COMP.       ELTVISON
00090 * 18 REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTVISON
00091                                                                   ELTVISON
00092  01  WS-BEN-PROV-IDS.                                             ELTVISON
00093      05  WS-INST-IP-CNT          PIC S9(03) VALUE +2 COMP.        ELTVISON
00094      05  WS-INST-IP-TABS.                                         ELTVISON
00095          10  FILLER              PIC  X(06) VALUE 'OVTI B'.       ELTVISON
00096          10  FILLER              PIC  X(06) VALUE 'VTI  B'.       ELTVISON
00097      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTVISON
00098                                  PIC  X(06) OCCURS 2 TIMES.       ELTVISON
00099                                                                   ELTVISON
00100      05  WS-INST-OP-CNT          PIC S9(03) VALUE +2 COMP.        ELTVISON
00101      05  WS-INST-OP-TABS.                                         ELTVISON
00102          10  FILLER              PIC  X(06) VALUE 'OVTO B'.       ELTVISON
00103          10  FILLER              PIC  X(06) VALUE 'VTO  B'.       ELTVISON
00104      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTVISON
00105                                  PIC  X(06) OCCURS 2 TIMES.       ELTVISON
00106                                                                   ELTVISON
00107      05  WS-PROF-IP-CNT          PIC S9(03) VALUE +18 COMP.       ELTVISON
00108      05  WS-PROF-IP-TABS.                                         ELTVISON
00109          10  FILLER              PIC  X(06) VALUE 'CLED E'.       ELTVISON
00110          10  FILLER              PIC  X(06) VALUE 'CLEM E'.       ELTVISON
00111          10  FILLER              PIC  X(06) VALUE 'CLEN E'.       ELTVISON
00112          10  FILLER              PIC  X(06) VALUE 'CLMD E'.       ELTVISON
00113          10  FILLER              PIC  X(06) VALUE 'FRAA E'.       ELTVISON
00114          10  FILLER              PIC  X(06) VALUE 'FRAD E'.       ELTVISON
00115          10  FILLER              PIC  X(06) VALUE 'FRAM E'.       ELTVISON
00116          10  FILLER              PIC  X(06) VALUE 'LBIF E'.       ELTVISON
00117          10  FILLER              PIC  X(06) VALUE 'LCNT E'.       ELTVISON
00118          10  FILLER              PIC  X(06) VALUE 'LENT E'.       ELTVISON
00119          10  FILLER              PIC  X(06) VALUE 'LSNG E'.       ELTVISON
00120          10  FILLER              PIC  X(06) VALUE 'LTRI E'.       ELTVISON
00121          10  FILLER              PIC  X(06) VALUE 'OVTI E'.       ELTVISON
00122          10  FILLER              PIC  X(06) VALUE 'PRSM E'.       ELTVISON
00123          10  FILLER              PIC  X(06) VALUE 'RLED E'.       ELTVISON
00124          10  FILLER              PIC  X(06) VALUE 'RLEN E'.       ELTVISON
00125          10  FILLER              PIC  X(06) VALUE 'VCPI E'.       ELTVISON
00126          10  FILLER              PIC  X(06) VALUE 'VTI  E'.       ELTVISON
00127      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTVISON
00128                                  PIC  X(06) OCCURS 18 TIMES.      ELTVISON
00129                                                                   ELTVISON
00130      05  WS-PROF-OP-CNT          PIC S9(03) VALUE +30 COMP.       ELTVISON
00131      05  WS-PROF-OP-TABS.                                         ELTVISON
00132          10  FILLER              PIC  X(06) VALUE 'CLED E'.       ELTVISON
00133          10  FILLER              PIC  X(06) VALUE 'CLEM E'.       ELTVISON
00134          10  FILLER              PIC  X(06) VALUE 'CLEN E'.       ELTVISON
00135          10  FILLER              PIC  X(06) VALUE 'CLMD E'.       ELTVISON
00136          10  FILLER              PIC  X(06) VALUE 'FRAA E'.       ELTVISON
00137          10  FILLER              PIC  X(06) VALUE 'FRAD E'.       ELTVISON
00138          10  FILLER              PIC  X(06) VALUE 'FRAM E'.       ELTVISON
00139          10  FILLER              PIC  X(06) VALUE 'LBIF E'.       ELTVISON
00140          10  FILLER              PIC  X(06) VALUE 'LCNT E'.       ELTVISON
00141          10  FILLER              PIC  X(06) VALUE 'LENT E'.       ELTVISON
00142          10  FILLER              PIC  X(06) VALUE 'LSNG E'.       ELTVISON
00143          10  FILLER              PIC  X(06) VALUE 'LTRI E'.       ELTVISON
00144          10  FILLER              PIC  X(06) VALUE 'OVTO E'.       ELTVISON
00145          10  FILLER              PIC  X(06) VALUE 'PRSM E'.       ELTVISON
00146          10  FILLER              PIC  X(06) VALUE 'RBAC E'.       ELTVISON
00147          10  FILLER              PIC  X(06) VALUE 'RBDS E'.       ELTVISON
00148          10  FILLER              PIC  X(06) VALUE 'RLED E'.       ELTVISON
00149          10  FILLER              PIC  X(06) VALUE 'RLEN E'.       ELTVISON
00150          10  FILLER              PIC  X(06) VALUE 'RTAC E'.       ELTVISON
00151          10  FILLER              PIC  X(06) VALUE 'RTDS E'.       ELTVISON
00152          10  FILLER              PIC  X(06) VALUE 'SBAC E'.       ELTVISON
00153          10  FILLER              PIC  X(06) VALUE 'SBDS E'.       ELTVISON
00154          10  FILLER              PIC  X(06) VALUE 'SLED E'.       ELTVISON
00155          10  FILLER              PIC  X(06) VALUE 'SLEN E'.       ELTVISON
00156          10  FILLER              PIC  X(06) VALUE 'STAC E'.       ELTVISON
00157          10  FILLER              PIC  X(06) VALUE 'STDS E'.       ELTVISON
00158          10  FILLER              PIC  X(06) VALUE 'TINT E'.       ELTVISON
00159          10  FILLER              PIC  X(06) VALUE 'VCPO E'.       ELTVISON
00160          10  FILLER              PIC  X(06) VALUE 'VMIS E'.       ELTVISON
00161          10  FILLER              PIC  X(06) VALUE 'VTO  E'.       ELTVISON
00162      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTVISON
00163                                  PIC  X(06) OCCURS  30 TIMES.     ELTVISON
00164                                                                   ELTVISON
00165 /***************************************************************  ELTVISON
00166 *              HEADER AND LITERAL TEXT AREA                    *  ELTVISON
00167 ****************************************************************  ELTVISON
00168  01  HEADER-I-IP-LINE-3.                                          ELTVISON
00169      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTVISON
00170      05  FILLER                  PIC  X(43) VALUE                 ELTVISON
00171              'VISON CARE SERVICES INSTITUTIONAL INPATIENT'.       ELTVISON
00172      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTVISON
00173                                                                   ELTVISON
00174  01  HEADER-I-OP-LINE-3.                                          ELTVISON
00175      05  FILLER                  PIC  X(17) VALUE SPACES.         ELTVISON
00176      05  FILLER                  PIC  X(44) VALUE                 ELTVISON
00177              'VISON CARE SERVICES INSTITUTIONAL OUTPATIENT'.      ELTVISON
00178      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTVISON
00179                                                                   ELTVISON
00180  01  HEADER-P-IP-LINE-3.                                          ELTVISON
00181      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTVISON
00182      05  FILLER                  PIC  X(42) VALUE                 ELTVISON
00183              'VISON CARE SERVICES PROFESSIONAL INPATIENT'.        ELTVISON
00184      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTVISON
00185                                                                   ELTVISON
00186  01  HEADER-P-OP-LINE-3.                                          ELTVISON
00187      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTVISON
00188      05  FILLER                  PIC  X(43) VALUE                 ELTVISON
00189              'VISON CARE SERVICES PROFESSIONAL OUTPATIENT'.       ELTVISON
00190      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTVISON
00191                                                                   ELTVISON
00192  01  WS-SERVICES-RENDERED.                                        ELTVISON
00193      05  FILLER                  PIC  X(26) VALUE                 ELTVISON
00194              'SERVICES MAY BE RENDERED: '.                        ELTVISON
00195      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTVISON
00196                                                                   ELTVISON
00197  01  WS-FOLLOWING-BEN.                                            ELTVISON
00198      05  FILLER                  PIC  X(21) VALUE                 ELTVISON
00199            'COVERED SERVICES ARE:'.                               ELTVISON
00200                                                                   ELTVISON
00201  01  WS-PAYABLE-AS.                                               ELTVISON
00202      10  FILLER                  PIC  X(40) VALUE                 ELTVISON
00203          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTVISON
00204                                                                   ELTVISON
00205  01  WS-CONTRACT-RELATED.                                         ELTVISON
00206      05  FILLER                  PIC  X(48) VALUE                 ELTVISON
00207              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTVISON
00208                                                                   ELTVISON
00209  01  WS-CERT-REQ.                                                 ELTVISON
00210      05  FILLER                   PIC  X(47) VALUE                ELTVISON
00211              'THE CERIFICATION REQUIRED FOR THIS SERVICE IS: '.   ELTVISON
00212      05  FILLER                   PIC  X(32) VALUE LOW-VALUES.    ELTVISON
00213                                                                   ELTVISON
00214  01  WS-EXCEPTION-SCHED.                                          ELTVISON
00215      05  FILLER                  PIC  X(49) VALUE                 ELTVISON
00216              'BENEFITS ARE PRICED BASED ON EXCEPTION SCHEDULE: '. ELTVISON
00217      05  FILLER                  PIC  X(30) VALUE LOW-VALUES.     ELTVISON
00218                                                                   ELTVISON
00219  01  WS-RECERT-REQ.                                               ELTVISON
00220      05  FILLER                   PIC  X(56) VALUE                ELTVISON
00221              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTVISON
00222 -            'IS: '.                                              ELTVISON
00223      05  FILLER                   PIC  X(23) VALUE LOW-VALUES.    ELTVISON
00224                                                                   ELTVISON
00225  01  WS-RELATED-MED-COND-1.                                       ELTVISON
00226      05  FILLER                   PIC  X(79) VALUE                ELTVISON
00227              'BENEFIT ELIGIBILITY REQUIRES THAT THIS SERVICE BE INELTVISON
00228 -            ' CONJUCTION WITH A RELATED '.                       ELTVISON
00229                                                                   ELTVISON
00230  01  WS-RELATED-MED-COND-2.                                       ELTVISON
00231      05  FILLER                   PIC  X(18) VALUE                ELTVISON
00232              'MEDICAL CONDITION.'.                                ELTVISON
00233      05  FILLER                   PIC  X(61) VALUE LOW-VALUES.    ELTVISON
00234                                                                   ELTVISON
00235  01  WS-BASIC.                                                    ELTVISON
00236      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTVISON
00237              '         BASIC: '.                                  ELTVISON
00238      05  WS-DTL-BASIC-LONG.                                       ELTVISON
00239          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTVISON
00240          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTVISON
00241                                                                   ELTVISON
00242  01  WS-SUPPLEMENTAL.                                             ELTVISON
00243      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTVISON
00244              '  SUPPLEMENTAL: '.                                  ELTVISON
00245      05  WS-DTL-SUPP-LONG.                                        ELTVISON
00246          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTVISON
00247          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTVISON
00248                                                                   ELTVISON
00249  01  WS-PVE.                                                      ELTVISON
00250      05  FILLER                  PIC  X(44) VALUE                 ELTVISON
00251              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTVISON
00252      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTVISON
00253                                                                   ELTVISON
00254  01  WS-ACCUM-MSG1.                                               ELTVISON
00255      05  FILLER                  PIC  X(79) VALUE                 ELTVISON
00256      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTVISON
00257 -    'CONSIDERATIONS.'.                                           ELTVISON
00258                                                                   ELTVISON
00259  01  WS-INDICES-PROBLEM.                                          ELTVISON
00260      05  FILLER                  PIC  X(20) VALUE                 ELTVISON
00261              'PROBLEM WITH INDICES'.                              ELTVISON
00262      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTVISON
00263                                                                   ELTVISON
00264  01  WS-POSSIBLE-ERROR.                                           ELTVISON
00265      05  FILLER                   PIC  X(50) VALUE                ELTVISON
00266              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTVISON
00267      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTVISON
00268                                                                   ELTVISON
00269  01  WS-INVALID-REQ.                                              ELTVISON
00270      05  FILLER                  PIC  X(37) VALUE                 ELTVISON
00271              '*** I N V A L I D   R E Q U E S T ***'.             ELTVISON
00272      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTVISON
00273                                                                   ELTVISON
00274  01  WS-SPILLOVER.                                                ELTVISON
00275      05  FILLER                  PIC  X(10) VALUE                 ELTVISON
00276              'SPILLOVER '.                                        ELTVISON
00277                                                                   ELTVISON
00278  01  WS-OTHER-LITERALS.                                           ELTVISON
00279    05  WS-NO-TABULAR1.                                            ELTVISON
00280      10  FILLER                    PIC X(51)  VALUE               ELTVISON
00281         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTVISON
00282      10  FILLER                    PIC X(22)  VALUE               ELTVISON
00283         'GOING FROM BENEFIT ***'.                                 ELTVISON
00284                                                                   ELTVISON
00285    05  WS-NO-TABULAR2.                                            ELTVISON
00286      10  FILLER                    PIC X(15)  VALUE               ELTVISON
00287         '*** PROVISION: '.                                        ELTVISON
00288      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTVISON
00289      10  FILLER                    PIC X VALUE SPACE.             ELTVISON
00290      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTVISON
00291      10  FILLER                    PIC X(13)  VALUE               ELTVISON
00292         ' TO TABULAR: '.                                          ELTVISON
00293      10  WS-NO-TAB-ID              PIC X(6).                      ELTVISON
00294      10  FILLER                    PIC X VALUE SPACE.             ELTVISON
00295      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTVISON
00296      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTVISON
00297                                                                   ELTVISON
00298    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTVISON
00299    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTVISON
00300      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTVISON
00301                                                                   ELTVISON
00302      TITLE ' LINKAGE SECTION -- ELTVISON'.                        ELTVISON
00303  LINKAGE SECTION.                                                 ELTVISON
00304  01  DFHCOMMAREA.                                                 ELTVISON
00305      COPY ELSCOMMC.                                               ELTVISON
00306 /                                                                 ELTVISON
00307      COPY ELSCIA2C.                                               ELTVISON
00308 /                                                                 ELTVISON
00309      COPY ELSIOPMC.                                               ELTVISON
00310 /                                                                 ELTVISON
00311      COPY ELSKEYSC.                                               ELTVISON
00312 /                                                                 ELTVISON
00313      COPY ELSOUTPC.                                               ELTVISON
00314 /                                                                 ELTVISON
00315      COPY ELSSSCBC.                                               ELTVISON
00316 /                                                                 ELTVISON
00317      COPY ELSCMIFC.                                               ELTVISON
00318 /                                                                 ELTVISON
00319      COPY ELSCMDSC.                                               ELTVISON
00320 /                                                                 ELTVISON
00321      COPY ELSPRVNC.                                               ELTVISON
00322 /                                                                 ELTVISON
00323      COPY ELSTCWAC.                                               ELTVISON
00324      TITLE 'PAYMENT LEVEL FLD REQUEST INDS '.                     ELTVISON
00325      COPY ELSPLGSW.                                               ELTVISON
00326      TITLE 'BENEFIT PROVISION TABLE OF FLDS'.                     ELTVISON
00327      COPY ELSPLGTB.                                               ELTVISON
00328                                                                   ELTVISON
00329      TITLE 'PROCEDURE DIVISION - ELTVISON'.                       ELTVISON
00330  PROCEDURE DIVISION.                                              ELTVISON
00331  0100-MAINLINE.                                                   ELTVISON
00332                                                                   ELTVISON
00333      PERFORM 1000-INITIALIZATION                                  ELTVISON
00334         THRU 1000-EXIT.                                           ELTVISON
00335                                                                   ELTVISON
00336      MOVE '0100' TO WS-PARA-ID.                                   ELTVISON
00337                                                                   ELTVISON
00338      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTVISON
00339                       AND                                         ELTVISON
00340         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTVISON
00341          PERFORM 2000-INSTITUTIONAL-IP THRU 2000-EXIT.            ELTVISON
00342                                                                   ELTVISON
00343      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTVISON
00344                       AND                                         ELTVISON
00345         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTVISON
00346          PERFORM 3000-INSTITUTIONAL-OP THRU 3000-EXIT.            ELTVISON
00347                                                                   ELTVISON
00348      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTVISON
00349                       AND                                         ELTVISON
00350         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTVISON
00351          PERFORM 4000-PROFESSIONAL-IP THRU 4000-EXIT.             ELTVISON
00352                                                                   ELTVISON
00353      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTVISON
00354                       AND                                         ELTVISON
00355         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTVISON
00356          PERFORM 5000-PROFESSIONAL-OP THRU 5000-EXIT.             ELTVISON
00357                                                                   ELTVISON
00358      IF (SSB-PROV-CLASS-INST OR                                   ELTVISON
00359          SSB-PROV-CLASS-BOTH OR                                   ELTVISON
00360          SSB-PROV-CLASS-PROF)                                     ELTVISON
00361                         AND                                       ELTVISON
00362         (SSB-SERV-CLASS-IP   OR                                   ELTVISON
00363          SSB-SERV-CLASS-OP   OR                                   ELTVISON
00364          SSB-SERV-CLASS-BOTH)                                     ELTVISON
00365            CONTINUE                                               ELTVISON
00366      ELSE                                                         ELTVISON
00367          MOVE ' '             TO  COF-FUNCTION                    ELTVISON
00368          MOVE +0              TO  COF-NBR-HDR-LINES               ELTVISON
00369          MOVE +2              TO  COF-NBR-DTL-LINES               ELTVISON
00370          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTVISON
00371          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTVISON
00372                           COMMAREA(DFHCOMMAREA)                   ELTVISON
00373          END-EXEC                                                 ELTVISON
00374      END-IF.                                                      ELTVISON
00375                                                                   ELTVISON
00376      MOVE 'E'   TO  COF-FUNCTION.                                 ELTVISON
00377      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTVISON
00378                                                                   ELTVISON
00379      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
00380                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00381      END-EXEC.                                                    ELTVISON
00382                                                                   ELTVISON
00383      EXEC CICS RETURN                                             ELTVISON
00384      END-EXEC.                                                    ELTVISON
00385                                                                   ELTVISON
00386      GOBACK.                                                      ELTVISON
00387                                                                   ELTVISON
00388  0098-SIGNAL-UNALL-AREA-ERROR.                                    ELTVISON
00389      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTVISON
00390      EXEC CICS ABEND                                              ELTVISON
00391                ABCODE (CIA-ABCODE)                                ELTVISON
00392      END-EXEC.                                                    ELTVISON
00393                                                                   ELTVISON
00394      TITLE 'INITIALIZATION - ELTVISON'.                           ELTVISON
00395  1000-INITIALIZATION.                                             ELTVISON
00396 ****************************************************************  ELTVISON
00397 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTVISON
00398 ****************************************************************  ELTVISON
00399                                                                   ELTVISON
00400      MOVE '1000' TO WS-PARA-ID.                                   ELTVISON
00401                                                                   ELTVISON
00402      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTVISON
00403          EXEC CICS ABEND                                          ELTVISON
00404                    ABCODE ('EL01')                                ELTVISON
00405          END-EXEC                                                 ELTVISON
00406      END-IF.                                                      ELTVISON
00407                                                                   ELTVISON
00408      IF ECA-CIA-PTR = NULL                                        ELTVISON
00409          EXEC CICS ABEND                                          ELTVISON
00410                    ABCODE ('EL02')                                ELTVISON
00411          END-EXEC                                                 ELTVISON
00412      END-IF.                                                      ELTVISON
00413                                                                   ELTVISON
00414      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTVISON
00415                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELTVISON
00416                                                                   ELTVISON
00417 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTVISON
00418                                                                   ELTVISON
00419      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTVISON
00420      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00421                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTVISON
00422      IF NOT CIA-RC-OK                                             ELTVISON
00423          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00424                                                                   ELTVISON
00425      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTVISON
00426      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00427                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELTVISON
00428      IF NOT CIA-RC-OK                                             ELTVISON
00429          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00430                                                                   ELTVISON
00431      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTVISON
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00433                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELTVISON
00434      IF NOT CIA-RC-OK                                             ELTVISON
00435          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00436                                                                   ELTVISON
00437      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTVISON
00438      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00439                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELTVISON
00440      IF NOT CIA-RC-OK                                             ELTVISON
00441          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00442                                                                   ELTVISON
00443      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTVISON
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00445                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELTVISON
00446      IF NOT CIA-RC-OK                                             ELTVISON
00447          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00448                                                                   ELTVISON
00449      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTVISON
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00451                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELTVISON
00452      IF NOT CIA-RC-OK                                             ELTVISON
00453          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTVISON
00454                                                                   ELTVISON
00455      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTVISON
00456      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTVISON
00457              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTVISON
00458                                                                   ELTVISON
00459      SET CIA-STG-GETMAIN TO TRUE.                                 ELTVISON
00460      EXEC CICS LINK                                               ELTVISON
00461                PROGRAM('ELUSTGMG')                                ELTVISON
00462                COMMAREA(DFHCOMMAREA)                              ELTVISON
00463      END-EXEC.                                                    ELTVISON
00464                                                                   ELTVISON
00465      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTVISON
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00467                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELTVISON
00468                                                                   ELTVISON
00469      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTVISON
00470                                                                   ELTVISON
00471  1000-EXIT.  EXIT.                                                ELTVISON
00472                                                                   ELTVISON
00473      TITLE 'INSTITUTIONAL INPATIENT'.                             ELTVISON
00474 ****************************************************************  ELTVISON
00475 *       VISION CARE INSTITUTIONAL INPATIENT PROCESSING         *  ELTVISON
00476 ****************************************************************  ELTVISON
00477  2000-INSTITUTIONAL-IP.                                           ELTVISON
00478                                                                   ELTVISON
00479      MOVE '2000' TO WS-PARA-ID.                                   ELTVISON
00480                                                                   ELTVISON
00481      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTVISON
00482                                                                   ELTVISON
00483      MOVE HEADER-I-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTVISON
00484                                                                   ELTVISON
00485      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTVISON
00486         THRU 9100-EXIT.                                           ELTVISON
00487                                                                   ELTVISON
00488      MOVE '2000' TO WS-PARA-ID.                                   ELTVISON
00489                                                                   ELTVISON
00490      INITIALIZE PVN-FIXED-PART.                                   ELTVISON
00491      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00492                                                                   ELTVISON
00493      PERFORM WITH TEST BEFORE                                     ELTVISON
00494              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTVISON
00495              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTVISON
00496         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTVISON
00497         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTVISON
00498         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTVISON
00499         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTVISON
00500      END-PERFORM.                                                 ELTVISON
00501      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00502                                                                   ELTVISON
00503      PERFORM 2010-MOVE-IN-INST-IP-TABS                            ELTVISON
00504         THRU 2010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00505                        UNTIL   WS-SUB  >     WS-INST-IP-CNT.      ELTVISON
00506                                                                   ELTVISON
00507      PERFORM 2020-CALL-COVERAGE                                   ELTVISON
00508         THRU 2020-EXIT.                                           ELTVISON
00509                                                                   ELTVISON
00510      IF PVN-COVG-NONE                                             ELTVISON
00511          GO TO 2000-EXIT.                                         ELTVISON
00512                                                                   ELTVISON
00513      PERFORM 2030-FIND-FIRST-NONZERO                              ELTVISON
00514         THRU 2030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00515                        UNTIL   WS-SUB  > WS-INST-IP-CNT.          ELTVISON
00516                                                                   ELTVISON
00517  2000-EXIT.  EXIT.                                                ELTVISON
00518 /                                                                 ELTVISON
00519  2010-MOVE-IN-INST-IP-TABS.                                       ELTVISON
00520      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00521      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTVISON
00522                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTVISON
00523                                                                   ELTVISON
00524      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTVISON
00525                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTVISON
00526                                                                   ELTVISON
00527  2010-EXIT.  EXIT.                                                ELTVISON
00528      SKIP3                                                        ELTVISON
00529  2020-CALL-COVERAGE.                                              ELTVISON
00530                                                                   ELTVISON
00531      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTVISON
00532      MOVE 'VISION CARE SERVICES ' TO  SSB-TOPIC-PHRASE.           ELTVISON
00533                                                                   ELTVISON
00534      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTVISON
00535                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00536                     END-EXEC.                                     ELTVISON
00537                                                                   ELTVISON
00538      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTVISON
00539      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
00540                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00541                     END-EXEC.                                     ELTVISON
00542                                                                   ELTVISON
00543      IF PVN-COVG-NONE                                             ELTVISON
00544          GO TO 2020-EXIT.                                         ELTVISON
00545                                                                   ELTVISON
00546      MOVE +1  TO  WS-CIA.                                         ELTVISON
00547      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTVISON
00548      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTVISON
00549                    PSP-PROVN-PRICING-METHD,                       ELTVISON
00550                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTVISON
00551                    PSP-TRANSF-OTHER-RESP-IND,                     ELTVISON
00552                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTVISON
00553                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTVISON
00554                    PSP-SPILL-OVER-DED-APL-IND,                    ELTVISON
00555                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTVISON
00556                    PSP-CERTFN-REQRM-IND,                          ELTVISON
00557                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTVISON
00558                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTVISON
00559                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTVISON
00560                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTVISON
00561                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTVISON
00562                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTVISON
00563                    PSB-CERTN-REPETN-REQRD-IND.                    ELTVISON
00564                                                                   ELTVISON
00565      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTVISON
00566                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00567      END-EXEC.                                                    ELTVISON
00568                                                                   ELTVISON
00569      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTVISON
00570      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00571              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTVISON
00572                                                                   ELTVISON
00573  2020-EXIT.  EXIT.                                                ELTVISON
00574 /                                                                 ELTVISON
00575  2030-FIND-FIRST-NONZERO.                                         ELTVISON
00576      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00577                                                                   ELTVISON
00578      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTVISON
00579          CONTINUE                                                 ELTVISON
00580      ELSE                                                         ELTVISON
00581          PERFORM 2100-BUILD-SCREEN-LINES                          ELTVISON
00582             THRU 2100-EXIT.                                       ELTVISON
00583                                                                   ELTVISON
00584  2030-EXIT.  EXIT.                                                ELTVISON
00585 /                                                                 ELTVISON
00586  2100-BUILD-SCREEN-LINES.                                         ELTVISON
00587      SET PLT-INDEX1  TO                                           ELTVISON
00588              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTVISON
00589                                                                   ELTVISON
00590      IF WS-NOT-FIRST-TIME                                         ELTVISON
00591         SET COF-NEW-PAGE TO TRUE                                  ELTVISON
00592         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTVISON
00593         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTVISON
00594                   COMMAREA (DFHCOMMAREA)                          ELTVISON
00595         END-EXEC                                                  ELTVISON
00596      ELSE                                                         ELTVISON
00597         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTVISON
00598                                                                   ELTVISON
00599      MOVE  +1  TO  WS-CIA.                                        ELTVISON
00600                                                                   ELTVISON
00601      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTVISON
00602          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTVISON
00603              SET PLT-INDEX2  TO  2                                ELTVISON
00604          ELSE                                                     ELTVISON
00605              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTVISON
00606              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTVISON
00607                 THRU 9200-EXIT                                    ELTVISON
00608              GO TO 2100-EXIT                                      ELTVISON
00609      ELSE                                                         ELTVISON
00610          SET PLT-INDEX2  TO  1.                                   ELTVISON
00611                                                                   ELTVISON
00612      PERFORM 7105-LIST-BEN-PROV                                   ELTVISON
00613         THRU 7105-EXIT.                                           ELTVISON
00614                                                                   ELTVISON
00615      PERFORM 7110-PLACE-OF-TREATMENT                              ELTVISON
00616         THRU 7110-EXIT.                                           ELTVISON
00617                                                                   ELTVISON
00618      PERFORM 7120-PRIC-METH                                       ELTVISON
00619         THRU 7120-EXIT.                                           ELTVISON
00620                                                                   ELTVISON
00621      PERFORM 7140-CERTIFICATION                                   ELTVISON
00622         THRU 7140-EXIT.                                           ELTVISON
00623                                                                   ELTVISON
00624      PERFORM 7150-RECERTIFICATION                                 ELTVISON
00625         THRU 7150-EXIT.                                           ELTVISON
00626                                                                   ELTVISON
00627      PERFORM 7155-SERVC-NEC                                       ELTVISON
00628         THRU 7155-EXIT.                                           ELTVISON
00629                                                                   ELTVISON
00630      PERFORM 7160-SPILLOVR-COINS-N-DEDUC                          ELTVISON
00631         THRU 7160-EXIT.                                           ELTVISON
00632                                                                   ELTVISON
00633      PERFORM 7165-TRANS-OTHR-RESPON-IND                           ELTVISON
00634         THRU 7165-EXIT.                                           ELTVISON
00635                                                                   ELTVISON
00636      PERFORM 7190-ALL-LEVEL-TABS                                  ELTVISON
00637         THRU 7190-EXIT.                                           ELTVISON
00638                                                                   ELTVISON
00639  2100-EXIT.  EXIT.                                                ELTVISON
00640                                                                   ELTVISON
00641      TITLE 'INSTUTIONAL OUTPATIENT'.                              ELTVISON
00642  3000-INSTITUTIONAL-OP.                                           ELTVISON
00643 ****************************************************************  ELTVISON
00644 *       VISON CARE INSTITUTIONAL OUTPATIENT PROVESSING         *  ELTVISON
00645 ****************************************************************  ELTVISON
00646                                                                   ELTVISON
00647      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTVISON
00648                                                                   ELTVISON
00649      MOVE HEADER-I-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTVISON
00650                                                                   ELTVISON
00651      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTVISON
00652         THRU 9100-EXIT.                                           ELTVISON
00653                                                                   ELTVISON
00654      INITIALIZE PVN-FIXED-PART.                                   ELTVISON
00655      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00656      PERFORM WITH TEST BEFORE                                     ELTVISON
00657              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTVISON
00658              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTVISON
00659         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTVISON
00660         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTVISON
00661         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTVISON
00662         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTVISON
00663      END-PERFORM.                                                 ELTVISON
00664      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00665                                                                   ELTVISON
00666      PERFORM 3010-MOVE-IN-INST-OP-TABS                            ELTVISON
00667         THRU 3010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00668                        UNTIL   WS-SUB  >     WS-INST-OP-CNT.      ELTVISON
00669                                                                   ELTVISON
00670      PERFORM 3020-CALL-COVERAGE                                   ELTVISON
00671         THRU 3020-EXIT.                                           ELTVISON
00672                                                                   ELTVISON
00673      IF PVN-COVG-NONE                                             ELTVISON
00674          GO TO 3000-EXIT.                                         ELTVISON
00675                                                                   ELTVISON
00676      PERFORM 3030-FIND-FIRST-NONZERO                              ELTVISON
00677         THRU 3030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00678                        UNTIL   WS-SUB  > WS-INST-OP-CNT.          ELTVISON
00679                                                                   ELTVISON
00680  3000-EXIT.  EXIT.                                                ELTVISON
00681 /                                                                 ELTVISON
00682  3010-MOVE-IN-INST-OP-TABS.                                       ELTVISON
00683      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00684      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTVISON
00685                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTVISON
00686                                                                   ELTVISON
00687      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTVISON
00688                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTVISON
00689                                                                   ELTVISON
00690  3010-EXIT.  EXIT.                                                ELTVISON
00691 /                                                                 ELTVISON
00692  3020-CALL-COVERAGE.                                              ELTVISON
00693                                                                   ELTVISON
00694      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTVISON
00695      MOVE 'VISION CARE SERVICES ' TO  SSB-TOPIC-PHRASE.           ELTVISON
00696                                                                   ELTVISON
00697      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTVISON
00698                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00699                     END-EXEC.                                     ELTVISON
00700                                                                   ELTVISON
00701      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTVISON
00702      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
00703                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00704                     END-EXEC.                                     ELTVISON
00705                                                                   ELTVISON
00706      IF PVN-COVG-NONE                                             ELTVISON
00707          GO TO 3020-EXIT.                                         ELTVISON
00708                                                                   ELTVISON
00709      MOVE +1  TO  WS-CIA.                                         ELTVISON
00710      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTVISON
00711                                                                   ELTVISON
00712      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTVISON
00713                    PSP-PROVN-PRICING-METHD,                       ELTVISON
00714                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTVISON
00715                    PSP-TRANSF-OTHER-RESP-IND,                     ELTVISON
00716                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTVISON
00717                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTVISON
00718                    PSP-SPILL-OVER-DED-APL-IND,                    ELTVISON
00719                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTVISON
00720                    PSP-CERTFN-REQRM-IND,                          ELTVISON
00721                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTVISON
00722                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTVISON
00723                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTVISON
00724                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTVISON
00725                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTVISON
00726                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTVISON
00727                    PSB-CERTN-REPETN-REQRD-IND.                    ELTVISON
00728                                                                   ELTVISON
00729      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTVISON
00730                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00731                     END-EXEC.                                     ELTVISON
00732                                                                   ELTVISON
00733      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTVISON
00734      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00735              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTVISON
00736                                                                   ELTVISON
00737                                                                   ELTVISON
00738  3020-EXIT.  EXIT.                                                ELTVISON
00739 /                                                                 ELTVISON
00740  3030-FIND-FIRST-NONZERO.                                         ELTVISON
00741      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00742                                                                   ELTVISON
00743      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTVISON
00744          CONTINUE                                                 ELTVISON
00745      ELSE                                                         ELTVISON
00746          PERFORM 3100-BUILD-SCREEN-LINES                          ELTVISON
00747             THRU 3100-EXIT.                                       ELTVISON
00748                                                                   ELTVISON
00749  3030-EXIT.  EXIT.                                                ELTVISON
00750 /                                                                 ELTVISON
00751  3100-BUILD-SCREEN-LINES.                                         ELTVISON
00752      SET PLT-INDEX1  TO                                           ELTVISON
00753              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTVISON
00754                                                                   ELTVISON
00755      IF WS-NOT-FIRST-TIME                                         ELTVISON
00756         SET COF-NEW-PAGE TO TRUE                                  ELTVISON
00757         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTVISON
00758         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTVISON
00759                   COMMAREA (DFHCOMMAREA)                          ELTVISON
00760         END-EXEC                                                  ELTVISON
00761      ELSE                                                         ELTVISON
00762         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTVISON
00763                                                                   ELTVISON
00764      MOVE  +1  TO  WS-CIA.                                        ELTVISON
00765                                                                   ELTVISON
00766      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTVISON
00767          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTVISON
00768              SET PLT-INDEX2  TO  2                                ELTVISON
00769          ELSE                                                     ELTVISON
00770              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTVISON
00771              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTVISON
00772                 THRU 9200-EXIT                                    ELTVISON
00773              GO TO 3100-EXIT                                      ELTVISON
00774      ELSE                                                         ELTVISON
00775          SET PLT-INDEX2  TO  1.                                   ELTVISON
00776                                                                   ELTVISON
00777      PERFORM 7105-LIST-BEN-PROV                                   ELTVISON
00778         THRU 7105-EXIT.                                           ELTVISON
00779                                                                   ELTVISON
00780      PERFORM 7110-PLACE-OF-TREATMENT                              ELTVISON
00781         THRU 7110-EXIT.                                           ELTVISON
00782                                                                   ELTVISON
00783      PERFORM 7120-PRIC-METH                                       ELTVISON
00784         THRU 7120-EXIT.                                           ELTVISON
00785                                                                   ELTVISON
00786      PERFORM 7140-CERTIFICATION                                   ELTVISON
00787         THRU 7140-EXIT.                                           ELTVISON
00788                                                                   ELTVISON
00789      PERFORM 7150-RECERTIFICATION                                 ELTVISON
00790         THRU 7150-EXIT.                                           ELTVISON
00791                                                                   ELTVISON
00792      PERFORM 7155-SERVC-NEC                                       ELTVISON
00793         THRU 7155-EXIT.                                           ELTVISON
00794                                                                   ELTVISON
00795      PERFORM 7160-SPILLOVR-COINS-N-DEDUC                          ELTVISON
00796         THRU 7160-EXIT.                                           ELTVISON
00797                                                                   ELTVISON
00798      PERFORM 7165-TRANS-OTHR-RESPON-IND                           ELTVISON
00799         THRU 7165-EXIT.                                           ELTVISON
00800                                                                   ELTVISON
00801      PERFORM 7190-ALL-LEVEL-TABS                                  ELTVISON
00802         THRU 7190-EXIT.                                           ELTVISON
00803                                                                   ELTVISON
00804  3100-EXIT.  EXIT.                                                ELTVISON
00805                                                                   ELTVISON
00806      TITLE 'PROFESSIONAL INPATIENT'.                              ELTVISON
00807  4000-PROFESSIONAL-IP.                                            ELTVISON
00808 ****************************************************************  ELTVISON
00809 *       VISON CARE PROFESSIONAL INPATIENT PROCESSING           *  ELTVISON
00810 ****************************************************************  ELTVISON
00811                                                                   ELTVISON
00812      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTVISON
00813                                                                   ELTVISON
00814      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTVISON
00815                                                                   ELTVISON
00816      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTVISON
00817         THRU 9100-EXIT.                                           ELTVISON
00818                                                                   ELTVISON
00819      INITIALIZE PVN-FIXED-PART.                                   ELTVISON
00820      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00821      PERFORM WITH TEST BEFORE                                     ELTVISON
00822              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTVISON
00823              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTVISON
00824         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTVISON
00825         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTVISON
00826         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTVISON
00827         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTVISON
00828      END-PERFORM.                                                 ELTVISON
00829      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00830                                                                   ELTVISON
00831      PERFORM 4010-MOVE-IN-PROF-IP-TABS                            ELTVISON
00832         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00833                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTVISON
00834                                                                   ELTVISON
00835      PERFORM 4020-CALL-COVERAGE                                   ELTVISON
00836         THRU 4020-EXIT.                                           ELTVISON
00837                                                                   ELTVISON
00838      IF PVN-COVG-NONE                                             ELTVISON
00839          GO TO 4000-EXIT.                                         ELTVISON
00840                                                                   ELTVISON
00841      PERFORM 4030-FIND-FIRST-NONZERO                              ELTVISON
00842         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
00843                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTVISON
00844                                                                   ELTVISON
00845  4000-EXIT.  EXIT.                                                ELTVISON
00846 /                                                                 ELTVISON
00847  4010-MOVE-IN-PROF-IP-TABS.                                       ELTVISON
00848      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00849      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTVISON
00850                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTVISON
00851                                                                   ELTVISON
00852      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTVISON
00853                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTVISON
00854                                                                   ELTVISON
00855  4010-EXIT.  EXIT.                                                ELTVISON
00856      SKIP3                                                        ELTVISON
00857  4020-CALL-COVERAGE.                                              ELTVISON
00858                                                                   ELTVISON
00859      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTVISON
00860      MOVE 'VISION CARE SERVICES ' TO  SSB-TOPIC-PHRASE.           ELTVISON
00861                                                                   ELTVISON
00862      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTVISON
00863                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00864                     END-EXEC.                                     ELTVISON
00865                                                                   ELTVISON
00866      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTVISON
00867      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
00868                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00869                     END-EXEC.                                     ELTVISON
00870                                                                   ELTVISON
00871      IF PVN-COVG-NONE                                             ELTVISON
00872          GO TO 4020-EXIT.                                         ELTVISON
00873                                                                   ELTVISON
00874      MOVE +1  TO  WS-CIA.                                         ELTVISON
00875                                                                   ELTVISON
00876      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTVISON
00877      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTVISON
00878                    PSP-PROVN-PRICING-METHD,                       ELTVISON
00879                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTVISON
00880                    PSP-TRANSF-OTHER-RESP-IND,                     ELTVISON
00881                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTVISON
00882                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTVISON
00883                    PSP-SPILL-OVER-DED-APL-IND,                    ELTVISON
00884                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTVISON
00885                    PSP-CERTFN-REQRM-IND,                          ELTVISON
00886                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTVISON
00887                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTVISON
00888                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTVISON
00889                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTVISON
00890                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTVISON
00891                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTVISON
00892                    PSE-EXCP-SCHED-ID,                             ELTVISON
00893                    PSE-CERTN-REPETN-REQRD-IND.                    ELTVISON
00894                                                                   ELTVISON
00895      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTVISON
00896                     COMMAREA (DFHCOMMAREA)                        ELTVISON
00897                     END-EXEC.                                     ELTVISON
00898                                                                   ELTVISON
00899      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTVISON
00900      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
00901              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTVISON
00902                                                                   ELTVISON
00903  4020-EXIT.  EXIT.                                                ELTVISON
00904 /                                                                 ELTVISON
00905  4030-FIND-FIRST-NONZERO.                                         ELTVISON
00906      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
00907                                                                   ELTVISON
00908      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTVISON
00909          CONTINUE                                                 ELTVISON
00910      ELSE                                                         ELTVISON
00911          PERFORM 4100-BUILD-SCREEN-LINES                          ELTVISON
00912             THRU 4100-EXIT.                                       ELTVISON
00913                                                                   ELTVISON
00914  4030-EXIT.  EXIT.                                                ELTVISON
00915 /                                                                 ELTVISON
00916  4100-BUILD-SCREEN-LINES.                                         ELTVISON
00917      SET PLT-INDEX1  TO                                           ELTVISON
00918              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTVISON
00919                                                                   ELTVISON
00920      IF WS-NOT-FIRST-TIME                                         ELTVISON
00921         SET COF-NEW-PAGE TO TRUE                                  ELTVISON
00922         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTVISON
00923         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTVISON
00924                   COMMAREA (DFHCOMMAREA)                          ELTVISON
00925         END-EXEC                                                  ELTVISON
00926      ELSE                                                         ELTVISON
00927         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTVISON
00928                                                                   ELTVISON
00929      MOVE  +1  TO  WS-CIA.                                        ELTVISON
00930                                                                   ELTVISON
00931      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTVISON
00932          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTVISON
00933              SET PLT-INDEX2  TO  2                                ELTVISON
00934          ELSE                                                     ELTVISON
00935              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTVISON
00936              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTVISON
00937                 THRU 9200-EXIT                                    ELTVISON
00938              GO TO 4100-EXIT                                      ELTVISON
00939      ELSE                                                         ELTVISON
00940          SET PLT-INDEX2  TO  1.                                   ELTVISON
00941                                                                   ELTVISON
00942      PERFORM 7105-LIST-BEN-PROV                                   ELTVISON
00943         THRU 7105-EXIT.                                           ELTVISON
00944                                                                   ELTVISON
00945      PERFORM 7110-PLACE-OF-TREATMENT                              ELTVISON
00946         THRU 7110-EXIT.                                           ELTVISON
00947                                                                   ELTVISON
00948      PERFORM 7120-PRIC-METH                                       ELTVISON
00949         THRU 7120-EXIT.                                           ELTVISON
00950                                                                   ELTVISON
00951      PERFORM 7125-EXCEPTION-SCHED                                 ELTVISON
00952         THRU 7125-EXIT.                                           ELTVISON
00953                                                                   ELTVISON
00954      PERFORM 7140-CERTIFICATION                                   ELTVISON
00955         THRU 7140-EXIT.                                           ELTVISON
00956                                                                   ELTVISON
00957      PERFORM 7151-RECERTIFICATION                                 ELTVISON
00958         THRU 7151-EXIT.                                           ELTVISON
00959                                                                   ELTVISON
00960      PERFORM 7155-SERVC-NEC                                       ELTVISON
00961         THRU 7155-EXIT.                                           ELTVISON
00962                                                                   ELTVISON
00963      PERFORM 7160-SPILLOVR-COINS-N-DEDUC                          ELTVISON
00964         THRU 7160-EXIT.                                           ELTVISON
00965                                                                   ELTVISON
00966      PERFORM 7165-TRANS-OTHR-RESPON-IND                           ELTVISON
00967         THRU 7165-EXIT.                                           ELTVISON
00968                                                                   ELTVISON
00969      PERFORM 7190-ALL-LEVEL-TABS                                  ELTVISON
00970         THRU 7190-EXIT.                                           ELTVISON
00971                                                                   ELTVISON
00972  4100-EXIT.  EXIT.                                                ELTVISON
00973                                                                   ELTVISON
00974      TITLE 'PROFESSIONAL OUTPATIENT'.                             ELTVISON
00975  5000-PROFESSIONAL-OP.                                            ELTVISON
00976 ****************************************************************  ELTVISON
00977 *      VISON CARE PROFESSIONAL OUTPATIENT PROCESSING           *  ELTVISON
00978 ****************************************************************  ELTVISON
00979                                                                   ELTVISON
00980                                                                   ELTVISON
00981      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTVISON
00982                                                                   ELTVISON
00983      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTVISON
00984                                                                   ELTVISON
00985      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTVISON
00986         THRU 9100-EXIT.                                           ELTVISON
00987                                                                   ELTVISON
00988      INITIALIZE PVN-FIXED-PART.                                   ELTVISON
00989      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00990      PERFORM WITH TEST BEFORE                                     ELTVISON
00991              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTVISON
00992              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTVISON
00993         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTVISON
00994         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTVISON
00995         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTVISON
00996         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTVISON
00997      END-PERFORM.                                                 ELTVISON
00998      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTVISON
00999                                                                   ELTVISON
01000      PERFORM 5010-MOVE-IN-PROF-OP-TABS                            ELTVISON
01001         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
01002                        UNTIL   WS-SUB  >     WS-PROF-OP-CNT.      ELTVISON
01003                                                                   ELTVISON
01004      PERFORM 5020-CALL-COVERAGE                                   ELTVISON
01005         THRU 5020-EXIT.                                           ELTVISON
01006                                                                   ELTVISON
01007      IF PVN-COVG-NONE                                             ELTVISON
01008          GO TO 5000-EXIT.                                         ELTVISON
01009                                                                   ELTVISON
01010      PERFORM 5030-FIND-FIRST-NONZERO                              ELTVISON
01011         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTVISON
01012                        UNTIL   WS-SUB  > WS-PROF-OP-CNT.          ELTVISON
01013                                                                   ELTVISON
01014  5000-EXIT.  EXIT.                                                ELTVISON
01015 /                                                                 ELTVISON
01016  5010-MOVE-IN-PROF-OP-TABS.                                       ELTVISON
01017      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
01018      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTVISON
01019                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTVISON
01020                                                                   ELTVISON
01021      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTVISON
01022                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTVISON
01023                                                                   ELTVISON
01024  5010-EXIT.  EXIT.                                                ELTVISON
01025      SKIP3                                                        ELTVISON
01026  5020-CALL-COVERAGE.                                              ELTVISON
01027                                                                   ELTVISON
01028      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTVISON
01029      MOVE 'VISION CARE SERVICES ' TO  SSB-TOPIC-PHRASE.           ELTVISON
01030                                                                   ELTVISON
01031      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTVISON
01032                     COMMAREA (DFHCOMMAREA)                        ELTVISON
01033                     END-EXEC.                                     ELTVISON
01034                                                                   ELTVISON
01035      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTVISON
01036      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
01037                     COMMAREA (DFHCOMMAREA)                        ELTVISON
01038                     END-EXEC.                                     ELTVISON
01039                                                                   ELTVISON
01040      IF PVN-COVG-NONE                                             ELTVISON
01041          GO TO 5020-EXIT.                                         ELTVISON
01042                                                                   ELTVISON
01043      MOVE +1  TO  WS-CIA.                                         ELTVISON
01044                                                                   ELTVISON
01045      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTVISON
01046      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTVISON
01047                    PSP-PROVN-PRICING-METHD,                       ELTVISON
01048                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTVISON
01049                    PSP-TRANSF-OTHER-RESP-IND,                     ELTVISON
01050                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTVISON
01051                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTVISON
01052                    PSP-SPILL-OVER-DED-APL-IND,                    ELTVISON
01053                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTVISON
01054                    PSP-CERTFN-REQRM-IND,                          ELTVISON
01055                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTVISON
01056                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTVISON
01057                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTVISON
01058                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTVISON
01059                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTVISON
01060                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTVISON
01061                    PSE-EXCP-SCHED-ID                              ELTVISON
01062                    PSE-CERTN-REPETN-REQRD-IND.                    ELTVISON
01063                                                                   ELTVISON
01064      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTVISON
01065                     COMMAREA (DFHCOMMAREA)                        ELTVISON
01066                     END-EXEC.                                     ELTVISON
01067                                                                   ELTVISON
01068      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTVISON
01069      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
01070              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTVISON
01071                                                                   ELTVISON
01072  5020-EXIT.  EXIT.                                                ELTVISON
01073      SKIP3                                                        ELTVISON
01074  5030-FIND-FIRST-NONZERO.                                         ELTVISON
01075      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
01076                                                                   ELTVISON
01077      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTVISON
01078          CONTINUE                                                 ELTVISON
01079      ELSE                                                         ELTVISON
01080          PERFORM 5100-BUILD-SCREEN-LINES                          ELTVISON
01081             THRU 5100-EXIT.                                       ELTVISON
01082                                                                   ELTVISON
01083  5030-EXIT.  EXIT.                                                ELTVISON
01084 /                                                                 ELTVISON
01085  5100-BUILD-SCREEN-LINES.                                         ELTVISON
01086      SET PLT-INDEX1  TO                                           ELTVISON
01087              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTVISON
01088                                                                   ELTVISON
01089      IF WS-NOT-FIRST-TIME                                         ELTVISON
01090         SET COF-NEW-PAGE TO TRUE                                  ELTVISON
01091         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTVISON
01092         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTVISON
01093                   COMMAREA (DFHCOMMAREA)                          ELTVISON
01094         END-EXEC                                                  ELTVISON
01095      ELSE                                                         ELTVISON
01096         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTVISON
01097                                                                   ELTVISON
01098      MOVE  +1  TO  WS-CIA.                                        ELTVISON
01099                                                                   ELTVISON
01100      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTVISON
01101          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTVISON
01102              SET PLT-INDEX2  TO  2                                ELTVISON
01103          ELSE                                                     ELTVISON
01104              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTVISON
01105              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTVISON
01106                 THRU 9200-EXIT                                    ELTVISON
01107              GO TO 5100-EXIT                                      ELTVISON
01108      ELSE                                                         ELTVISON
01109          SET PLT-INDEX2  TO  1.                                   ELTVISON
01110                                                                   ELTVISON
01111      PERFORM 7105-LIST-BEN-PROV                                   ELTVISON
01112         THRU 7105-EXIT.                                           ELTVISON
01113                                                                   ELTVISON
01114      PERFORM 7110-PLACE-OF-TREATMENT                              ELTVISON
01115         THRU 7110-EXIT.                                           ELTVISON
01116                                                                   ELTVISON
01117      PERFORM 7120-PRIC-METH                                       ELTVISON
01118         THRU 7120-EXIT.                                           ELTVISON
01119                                                                   ELTVISON
01120      PERFORM 7125-EXCEPTION-SCHED                                 ELTVISON
01121         THRU 7125-EXIT.                                           ELTVISON
01122                                                                   ELTVISON
01123      PERFORM 7140-CERTIFICATION                                   ELTVISON
01124         THRU 7140-EXIT.                                           ELTVISON
01125                                                                   ELTVISON
01126      PERFORM 7151-RECERTIFICATION                                 ELTVISON
01127         THRU 7151-EXIT.                                           ELTVISON
01128                                                                   ELTVISON
01129      PERFORM 7155-SERVC-NEC                                       ELTVISON
01130         THRU 7155-EXIT.                                           ELTVISON
01131                                                                   ELTVISON
01132      PERFORM 7160-SPILLOVR-COINS-N-DEDUC                          ELTVISON
01133         THRU 7160-EXIT.                                           ELTVISON
01134                                                                   ELTVISON
01135      PERFORM 7165-TRANS-OTHR-RESPON-IND                           ELTVISON
01136         THRU 7165-EXIT.                                           ELTVISON
01137                                                                   ELTVISON
01138      PERFORM 7190-ALL-LEVEL-TABS                                  ELTVISON
01139         THRU 7190-EXIT.                                           ELTVISON
01140                                                                   ELTVISON
01141  5100-EXIT.  EXIT.                                                ELTVISON
01142 /                                                                 ELTVISON
01143 /                                                                 ELTVISON
01144  5125-EXCEPTION-SCHED.                                            ELTVISON
01145 ****************************************************************  ELTVISON
01146 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTVISON
01147 ****************************************************************  ELTVISON
01148      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01149      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01150                          AND                                      ELTVISON
01151         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01152                                                 NOT =  ZERO       ELTVISON
01153          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01154          MOVE +2                    TO  WS-CIA                    ELTVISON
01155          MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01156                                                                   ELTVISON
01157      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01158      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01159                           AND                                     ELTVISON
01160         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01161                                                 NOT  =  ZERO      ELTVISON
01162                           AND                                     ELTVISON
01163         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01164          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01165          MOVE +2                    TO  WS-CIA                    ELTVISON
01166          MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01167                                                                   ELTVISON
01168      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01169      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01170                           AND                                     ELTVISON
01171         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01172                                                  NOT  =  ZERO     ELTVISON
01173          ADD  +1                    TO  WS-CIA                    ELTVISON
01174          STRING WS-BASIC-LIT                                      ELTVISON
01175                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTVISON
01176                 DELIMITED BY SIZE                                 ELTVISON
01177          INTO COF-DTL-LINE (WS-CIA).                              ELTVISON
01178                                                                   ELTVISON
01179      SET PLT-INDEX2  TO  2.                                       ELTVISON
01180      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01181                           AND                                     ELTVISON
01182         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01183                                                 NOT  =  ZERO      ELTVISON
01184          ADD  +1                    TO  WS-CIA                    ELTVISON
01185          STRING WS-SUPP-LIT                                       ELTVISON
01186                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTVISON
01187                 DELIMITED BY SIZE                                 ELTVISON
01188          INTO COF-DTL-LINE (WS-CIA).                              ELTVISON
01189                                                                   ELTVISON
01190      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01191         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01192          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01193             THRU 9200-EXIT.                                       ELTVISON
01194                                                                   ELTVISON
01195      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01196         SET PLT-INDEX2  TO  2                                     ELTVISON
01197      ELSE                                                         ELTVISON
01198         SET PLT-INDEX2  TO  1.                                    ELTVISON
01199                                                                   ELTVISON
01200  5125-EXIT.  EXIT.                                                ELTVISON
01201 /                                                                 ELTVISON
01202                                                                   ELTVISON
01203      TITLE ' LIST OF BENEFIT PROVISIONS'.                         ELTVISON
01204  7105-LIST-BEN-PROV.                                              ELTVISON
01205 ****************************************************************  ELTVISON
01206 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTVISON
01207 ****************************************************************  ELTVISON
01208                                                                   ELTVISON
01209      MOVE '7105' TO WS-PARA-ID.                                   ELTVISON
01210                                                                   ELTVISON
01211      MOVE  +2               TO  WS-CIA.                           ELTVISON
01212      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTVISON
01213      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTVISON
01214                             TO  WS-SUB3.                          ELTVISON
01215                                                                   ELTVISON
01216      PERFORM 7106-ZERO-ALL-WITH-SAME-NO THRU                      ELTVISON
01217              7106-EXIT VARYING                                    ELTVISON
01218                   PVN-BEN-PROVN-IDX FROM WS-SUB BY +1             ELTVISON
01219                        UNTIL   PVN-BEN-PROVN-IDX >                ELTVISON
01220                                PVN-NBR-BEN-PROVN.                 ELTVISON
01221                                                                   ELTVISON
01222      MOVE '7105' TO WS-PARA-ID.                                   ELTVISON
01223                                                                   ELTVISON
01224      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTVISON
01225      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTVISON
01226                                                                   ELTVISON
01227      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
01228                     COMMAREA (DFHCOMMAREA)                        ELTVISON
01229                     END-EXEC.                                     ELTVISON
01230                                                                   ELTVISON
01231      MOVE +1 TO WS-CIA.                                           ELTVISON
01232                                                                   ELTVISON
01233  7105-EXIT.  EXIT.                                                ELTVISON
01234      SKIP3                                                        ELTVISON
01235  7106-ZERO-ALL-WITH-SAME-NO.                                      ELTVISON
01236                                                                   ELTVISON
01237      MOVE '7106' TO WS-PARA-ID.                                   ELTVISON
01238                                                                   ELTVISON
01239      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTVISON
01240          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTVISON
01241          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTVISON
01242          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTVISON
01243                            TO  CMF-CODE-VALUE                     ELTVISON
01244          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTVISON
01245          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTVISON
01246          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01247             THRU 9500-EXIT                                        ELTVISON
01248          MOVE '7106' TO WS-PARA-ID                                ELTVISON
01249          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTVISON
01250          IF WS-CIA  >  20  OR  =  20                              ELTVISON
01251              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTVISON
01252              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTVISON
01253                             COMMAREA (DFHCOMMAREA)                ELTVISON
01254                             END-EXEC                              ELTVISON
01255              MOVE  +1  TO  WS-CIA.                                ELTVISON
01256                                                                   ELTVISON
01257  7106-EXIT.  EXIT.                                                ELTVISON
01258                                                                   ELTVISON
01259      TITLE ' PLACE OF TREATEMENT'.                                ELTVISON
01260  7110-PLACE-OF-TREATMENT.                                         ELTVISON
01261 ****************************************************************  ELTVISON
01262 *              P L A C E   O F   T R E A T M E N T             *  ELTVISON
01263 ****************************************************************  ELTVISON
01264                                                                   ELTVISON
01265      MOVE '7110' TO WS-PARA-ID.                                   ELTVISON
01266                                                                   ELTVISON
01267      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01268      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01269                          AND                                      ELTVISON
01270         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTVISON
01271                                                 NOT =  ZERO       ELTVISON
01272          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01273          MOVE +2                    TO  WS-CIA                    ELTVISON
01274          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01275                                                                   ELTVISON
01276      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01277      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01278                           AND                                     ELTVISON
01279         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTVISON
01280                                                 NOT  =  ZERO      ELTVISON
01281                           AND                                     ELTVISON
01282         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01283          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01284          MOVE +2                    TO  WS-CIA                    ELTVISON
01285          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01286                                                                   ELTVISON
01287      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01288      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01289                           AND                                     ELTVISON
01290         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTVISON
01291                                                  NOT  =  ZERO     ELTVISON
01292          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTVISON
01293          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTVISON
01294                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01295          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTVISON
01296                               TO  CMF-CODE-VALUE                  ELTVISON
01297          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTVISON
01298          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01299          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01300             THRU 9500-EXIT                                        ELTVISON
01301          MOVE '7110' TO WS-PARA-ID.                               ELTVISON
01302                                                                   ELTVISON
01303      SET PLT-INDEX2  TO  2.                                       ELTVISON
01304      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01305                           AND                                     ELTVISON
01306         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTVISON
01307                                                 NOT  =  ZERO      ELTVISON
01308          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTVISON
01309          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTVISON
01310                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01311          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTVISON
01312                               TO  CMF-CODE-VALUE                  ELTVISON
01313          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTVISON
01314          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01315          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01316             THRU 9500-EXIT                                        ELTVISON
01317          MOVE '7110' TO WS-PARA-ID.                               ELTVISON
01318                                                                   ELTVISON
01319      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01320         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01321          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01322             THRU 9200-EXIT                                        ELTVISON
01323          MOVE '7110' TO WS-PARA-ID.                               ELTVISON
01324                                                                   ELTVISON
01325      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01326         SET PLT-INDEX2  TO  2                                     ELTVISON
01327      ELSE                                                         ELTVISON
01328         SET PLT-INDEX2  TO  1.                                    ELTVISON
01329  7110-EXIT.  EXIT.                                                ELTVISON
01330      TITLE 'PROVISION PRICING METHOD'.                            ELTVISON
01331  7120-PRIC-METH.                                                  ELTVISON
01332 ****************************************************************  ELTVISON
01333 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTVISON
01334 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTVISON
01335 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTVISON
01336 ****************************************************************  ELTVISON
01337      MOVE '7120' TO WS-PARA-ID.                                   ELTVISON
01338      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01339      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO  AND       ELTVISON
01340         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTVISON
01341                                                              '19' ELTVISON
01342         MOVE +2             TO  WS-CIA                            ELTVISON
01343         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTVISON
01344         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTVISON
01345                                                                   ELTVISON
01346      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01347      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTVISON
01348         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTVISON
01349                                                        '19' AND   ELTVISON
01350         NOT WS-ADD-A-BLANK-LINE                                   ELTVISON
01351         MOVE +2             TO  WS-CIA                            ELTVISON
01352         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTVISON
01353         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTVISON
01354                                                                   ELTVISON
01355      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01356      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO    AND     ELTVISON
01357         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTVISON
01358                            AND                                    ELTVISON
01359         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01360         SET  PLT-INDEX2  TO  2                                    ELTVISON
01361         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTVISON
01362                                                             ZERO  ELTVISON
01363            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTVISON
01364            ADD +1  TO  WS-CIA                                     ELTVISON
01365            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTVISON
01366                                                                   ELTVISON
01367      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01368      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO    AND     ELTVISON
01369         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTVISON
01370                            AND                                    ELTVISON
01371         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTVISON
01372         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTVISON
01373         ADD +1  TO  WS-CIA                                        ELTVISON
01374         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTVISON
01375                                                                   ELTVISON
01376      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTVISON
01377         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01378         SET  PLT-INDEX2  TO  2                                    ELTVISON
01379         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTVISON
01380                                                             ZERO  ELTVISON
01381            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTVISON
01382            ADD +1  TO  WS-CIA                                     ELTVISON
01383            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTVISON
01384                                                                   ELTVISON
01385      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01386      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01387         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTVISON
01388                                                            =  ZEROELTVISON
01389            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01390                                                            =  ZEROELTVISON
01391               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTVISON
01392            ELSE                                                   ELTVISON
01393               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTVISON
01394          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01395                                                  TO  WS-PERCENTAGEELTVISON
01396         ELSE                                                      ELTVISON
01397            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTVISON
01398          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01399                                                 TO  WS-PERCENTAGE.ELTVISON
01400      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO   AND      ELTVISON
01401         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTVISON
01402                                             ZERO AND NOT =  '19'  ELTVISON
01403         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTVISON
01404         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTVISON
01405         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTVISON
01406                                                    CMF-CODE-VALUE ELTVISON
01407         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTVISON
01408         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTVISON
01409         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTVISON
01410            THRU 9600-EXIT                                         ELTVISON
01411         MOVE '7120' TO WS-PARA-ID.                                ELTVISON
01412                                                                   ELTVISON
01413      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01414      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01415         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTVISON
01416                                                               ZEROELTVISON
01417            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01418                                                            =  ZEROELTVISON
01419               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTVISON
01420            ELSE                                                   ELTVISON
01421               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTVISON
01422          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01423                                                  TO  WS-PERCENTAGEELTVISON
01424         ELSE                                                      ELTVISON
01425            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTVISON
01426          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTVISON
01427                                                 TO  WS-PERCENTAGE.ELTVISON
01428      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTVISON
01429         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTVISON
01430                                             ZERO AND NOT =  '19'  ELTVISON
01431         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTVISON
01432         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTVISON
01433         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTVISON
01434                                                    CMF-CODE-VALUE ELTVISON
01435         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTVISON
01436         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTVISON
01437         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTVISON
01438            THRU 9600-EXIT                                         ELTVISON
01439         MOVE '7120' TO WS-PARA-ID.                                ELTVISON
01440                                                                   ELTVISON
01441      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01442          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTVISON
01443          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01444             THRU 9200-EXIT                                        ELTVISON
01445         MOVE '7120' TO WS-PARA-ID.                                ELTVISON
01446                                                                   ELTVISON
01447      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01448         SET PLT-INDEX2  TO  2                                     ELTVISON
01449      ELSE                                                         ELTVISON
01450         SET PLT-INDEX2  TO  1.                                    ELTVISON
01451                                                                   ELTVISON
01452  7120-EXIT.  EXIT.                                                ELTVISON
01453      TITLE 'EXCEPTION SCHEDULE INDICATOR'.                        ELTVISON
01454  7125-EXCEPTION-SCHED.                                            ELTVISON
01455 ****************************************************************  ELTVISON
01456 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTVISON
01457 ****************************************************************  ELTVISON
01458      MOVE '7125' TO WS-PARA-ID.                                   ELTVISON
01459      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01460      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01461                          AND                                      ELTVISON
01462         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01463                                                 NOT =  ZERO       ELTVISON
01464          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01465          MOVE +2                    TO  WS-CIA                    ELTVISON
01466          MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01467                                                                   ELTVISON
01468      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01469      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01470                           AND                                     ELTVISON
01471         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01472                                                 NOT  =  ZERO      ELTVISON
01473                           AND                                     ELTVISON
01474         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01475          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01476          MOVE +2                    TO  WS-CIA                    ELTVISON
01477          MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01478                                                                   ELTVISON
01479      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01480      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01481                           AND                                     ELTVISON
01482         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01483                                                  NOT  =  ZERO     ELTVISON
01484          ADD  +1                    TO  WS-CIA                    ELTVISON
01485          STRING WS-BASIC-LIT                                      ELTVISON
01486                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTVISON
01487                 DELIMITED BY SIZE                                 ELTVISON
01488          INTO COF-DTL-LINE (WS-CIA).                              ELTVISON
01489                                                                   ELTVISON
01490      SET PLT-INDEX2  TO  2.                                       ELTVISON
01491      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01492                           AND                                     ELTVISON
01493         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTVISON
01494                                                 NOT  =  ZERO      ELTVISON
01495          ADD  +1                    TO  WS-CIA                    ELTVISON
01496          STRING WS-SUPP-LIT                                       ELTVISON
01497                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTVISON
01498                 DELIMITED BY SIZE                                 ELTVISON
01499          INTO COF-DTL-LINE (WS-CIA).                              ELTVISON
01500                                                                   ELTVISON
01501      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01502         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01503          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01504             THRU 9200-EXIT                                        ELTVISON
01505          MOVE '7125' TO WS-PARA-ID.                               ELTVISON
01506                                                                   ELTVISON
01507      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01508         SET PLT-INDEX2  TO  2                                     ELTVISON
01509      ELSE                                                         ELTVISON
01510         SET PLT-INDEX2  TO  1.                                    ELTVISON
01511                                                                   ELTVISON
01512  7125-EXIT.  EXIT.                                                ELTVISON
01513      TITLE 'CERTIFICATION INDICATOR'.                             ELTVISON
01514  7140-CERTIFICATION.                                              ELTVISON
01515 ****************************************************************  ELTVISON
01516 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTVISON
01517 ****************************************************************  ELTVISON
01518      MOVE '7140' TO WS-PARA-ID.                                   ELTVISON
01519      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01520      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01521                          AND                                      ELTVISON
01522         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTVISON
01523                                                 NOT =  '00'       ELTVISON
01524          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01525          MOVE +2                    TO  WS-CIA                    ELTVISON
01526          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01527                                                                   ELTVISON
01528      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01529      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01530                           AND                                     ELTVISON
01531         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTVISON
01532                                                 NOT  =  '00'      ELTVISON
01533                           AND                                     ELTVISON
01534         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01535          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01536          MOVE +2                    TO  WS-CIA                    ELTVISON
01537          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01538                                                                   ELTVISON
01539      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01540      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01541                           AND                                     ELTVISON
01542         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTVISON
01543                                                  NOT  =  '00'     ELTVISON
01544          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTVISON
01545          MOVE 'CERTFN-REQRM-IND'                                  ELTVISON
01546                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01547          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01548                               TO  CMF-CODE-VALUE                  ELTVISON
01549          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTVISON
01550          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01551          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01552             THRU 9500-EXIT .                                      ELTVISON
01553          MOVE '7140' TO WS-PARA-ID.                               ELTVISON
01554                                                                   ELTVISON
01555      SET PLT-INDEX2  TO  2.                                       ELTVISON
01556      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01557                           AND                                     ELTVISON
01558         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTVISON
01559                                                 NOT  =  '00'      ELTVISON
01560          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTVISON
01561          MOVE 'CERTFN-REQRM-IND'                                  ELTVISON
01562                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01563          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01564                               TO  CMF-CODE-VALUE                  ELTVISON
01565          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTVISON
01566          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01567          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01568             THRU 9500-EXIT                                        ELTVISON
01569          MOVE '7140' TO WS-PARA-ID.                               ELTVISON
01570                                                                   ELTVISON
01571      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01572         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01573          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01574             THRU 9200-EXIT                                        ELTVISON
01575          MOVE '7140' TO WS-PARA-ID.                               ELTVISON
01576                                                                   ELTVISON
01577      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01578         SET PLT-INDEX2  TO  2                                     ELTVISON
01579      ELSE                                                         ELTVISON
01580         SET PLT-INDEX2  TO  1.                                    ELTVISON
01581                                                                   ELTVISON
01582  7140-EXIT.  EXIT.                                                ELTVISON
01583      TITLE 'RECERTIFICATION IND INSTUTIONAL'.                     ELTVISON
01584  7150-RECERTIFICATION.                                            ELTVISON
01585 ****************************************************************  ELTVISON
01586 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTVISON
01587 *      U S E D   O N L Y   F O R   I N S T U T I O N A L       *  ELTVISON
01588 ****************************************************************  ELTVISON
01589      MOVE '7150' TO WS-PARA-ID.                                   ELTVISON
01590      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01591      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01592                          AND                                      ELTVISON
01593         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01594                                                 NOT =  ZERO       ELTVISON
01595          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01596          MOVE +2                    TO  WS-CIA                    ELTVISON
01597          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01598                                                                   ELTVISON
01599      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01600      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01601                           AND                                     ELTVISON
01602         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01603                                                 NOT  =  ZERO      ELTVISON
01604                           AND                                     ELTVISON
01605         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01606          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01607          MOVE +2                    TO  WS-CIA                    ELTVISON
01608          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01609                                                                   ELTVISON
01610      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01611      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01612                           AND                                     ELTVISON
01613         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01614                                                  NOT  =  ZERO     ELTVISON
01615          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTVISON
01616          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTVISON
01617                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01618          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTVISON
01619                               TO  CMF-CODE-VALUE                  ELTVISON
01620          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTVISON
01621          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01622          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01623             THRU 9500-EXIT                                        ELTVISON
01624          MOVE '7150' TO WS-PARA-ID.                               ELTVISON
01625                                                                   ELTVISON
01626      SET PLT-INDEX2  TO  2.                                       ELTVISON
01627      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01628                           AND                                     ELTVISON
01629         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01630                                                 NOT  =  ZERO      ELTVISON
01631          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTVISON
01632          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTVISON
01633                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01634          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTVISON
01635                               TO  CMF-CODE-VALUE                  ELTVISON
01636          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTVISON
01637          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01638          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01639             THRU 9500-EXIT                                        ELTVISON
01640          MOVE '7150' TO WS-PARA-ID.                               ELTVISON
01641                                                                   ELTVISON
01642      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01643         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01644          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01645             THRU 9200-EXIT                                        ELTVISON
01646                                                                   ELTVISON
01647      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01648         SET PLT-INDEX2  TO  2                                     ELTVISON
01649      ELSE                                                         ELTVISON
01650         SET PLT-INDEX2  TO  1.                                    ELTVISON
01651                                                                   ELTVISON
01652  7150-EXIT.  EXIT.                                                ELTVISON
01653      TITLE 'RECERTIFICATION IND PROFESSIONAL'.                    ELTVISON
01654  7151-RECERTIFICATION.                                            ELTVISON
01655 ****************************************************************  ELTVISON
01656 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTVISON
01657 *      U S E D   O N L Y   F O R  P R O F E S S I O N A L      *  ELTVISON
01658 ****************************************************************  ELTVISON
01659      MOVE '7151' TO WS-PARA-ID.                                   ELTVISON
01660      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01661      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01662                          AND                                      ELTVISON
01663         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01664                                                 NOT =  ZERO       ELTVISON
01665          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01666          MOVE +2                    TO  WS-CIA                    ELTVISON
01667          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01668                                                                   ELTVISON
01669      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01670      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01671                           AND                                     ELTVISON
01672         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01673                                                 NOT  =  ZERO      ELTVISON
01674                           AND                                     ELTVISON
01675         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01676          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01677          MOVE +2                    TO  WS-CIA                    ELTVISON
01678          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTVISON
01679                                                                   ELTVISON
01680      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01681      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01682                           AND                                     ELTVISON
01683         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01684                                                  NOT  =  ZERO     ELTVISON
01685          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTVISON
01686          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTVISON
01687                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01688          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTVISON
01689                               TO  CMF-CODE-VALUE                  ELTVISON
01690          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTVISON
01691          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01692          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01693             THRU 9500-EXIT                                        ELTVISON
01694          MOVE '7150' TO WS-PARA-ID.                               ELTVISON
01695                                                                   ELTVISON
01696      SET PLT-INDEX2  TO  2.                                       ELTVISON
01697      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01698                           AND                                     ELTVISON
01699         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01700                                                 NOT  =  ZERO      ELTVISON
01701          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTVISON
01702          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTVISON
01703                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTVISON
01704          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTVISON
01705                               TO  CMF-CODE-VALUE                  ELTVISON
01706          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTVISON
01707          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTVISON
01708          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTVISON
01709             THRU 9500-EXIT                                        ELTVISON
01710          MOVE '7150' TO WS-PARA-ID.                               ELTVISON
01711                                                                   ELTVISON
01712      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01713         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01714          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01715             THRU 9200-EXIT                                        ELTVISON
01716                                                                   ELTVISON
01717      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01718         SET PLT-INDEX2  TO  2                                     ELTVISON
01719      ELSE                                                         ELTVISON
01720         SET PLT-INDEX2  TO  1.                                    ELTVISON
01721                                                                   ELTVISON
01722  7151-EXIT.  EXIT.                                                ELTVISON
01723      TITLE ' SERVICE NECESSARY BIT'.                              ELTVISON
01724  7155-SERVC-NEC.                                                  ELTVISON
01725 ****************************************************************  ELTVISON
01726 *  S E V I C E   N E C E S S A R Y   B I T   I N D I C A T O R *  ELTVISON
01727 ****************************************************************  ELTVISON
01728      MOVE '7155' TO WS-PARA-ID.                                   ELTVISON
01729      SET  PLT-INDEX2  TO  1.                                      ELTVISON
01730      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01731                          AND                                      ELTVISON
01732         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTVISON
01733                                                  =  '1'           ELTVISON
01734          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01735          MOVE +3                    TO  WS-CIA                    ELTVISON
01736          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTVISON
01737          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTVISON
01738                                                                   ELTVISON
01739      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01740      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTVISON
01741                           AND                                     ELTVISON
01742         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTVISON
01743                                                 =  '1'            ELTVISON
01744                           AND                                     ELTVISON
01745         NOT  WS-ADD-A-BLANK-LINE                                  ELTVISON
01746          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTVISON
01747          MOVE +3                    TO  WS-CIA                    ELTVISON
01748          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTVISON
01749          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTVISON
01750                                                                   ELTVISON
01751      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01752         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01753          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTVISON
01754             THRU 9200-EXIT                                        ELTVISON
01755             MOVE '7155' TO WS-PARA-ID.                            ELTVISON
01756                                                                   ELTVISON
01757      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01758         SET PLT-INDEX2  TO  2                                     ELTVISON
01759      ELSE                                                         ELTVISON
01760         SET PLT-INDEX2  TO  1.                                    ELTVISON
01761                                                                   ELTVISON
01762  7155-EXIT.  EXIT.                                                ELTVISON
01763      TITLE ' SPILLOVER COINSURANCE'.                              ELTVISON
01764  7160-SPILLOVR-COINS-N-DEDUC.                                     ELTVISON
01765 ****************************************************************  ELTVISON
01766 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTVISON
01767 ****************************************************************  ELTVISON
01768      MOVE '7160' TO WS-PARA-ID.                                   ELTVISON
01769      MOVE +1  TO  WS-CIA.                                         ELTVISON
01770                                                                   ELTVISON
01771      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01772      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTVISON
01773                            AND                                    ELTVISON
01774         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTVISON
01775                                                 NOT =  '0'        ELTVISON
01776         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTVISON
01777         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTVISON
01778                                           CMF-ELEMENT-SYSTEM-NAME ELTVISON
01779         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTVISON
01780                                                TO  CMF-CODE-VALUE ELTVISON
01781         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTVISON
01782         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTVISON
01783         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTVISON
01784            THRU 9500-EXIT                                         ELTVISON
01785         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTVISON
01786            THRU 9200-EXIT                                         ELTVISON
01787         MOVE '7160' TO WS-PARA-ID.                                ELTVISON
01788 ****************************************************************  ELTVISON
01789 *          S P I L L O V E R   D E D U C T I B L E             *  ELTVISON
01790 ****************************************************************  ELTVISON
01791      MOVE +1  TO  WS-CIA.                                         ELTVISON
01792                                                                   ELTVISON
01793      SET  PLT-INDEX2  TO  2.                                      ELTVISON
01794      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTVISON
01795                            AND                                    ELTVISON
01796         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTVISON
01797                                                 NOT =  '0'        ELTVISON
01798         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTVISON
01799         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTVISON
01800                                           CMF-ELEMENT-SYSTEM-NAME ELTVISON
01801         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTVISON
01802                                                 TO  CMF-CODE-VALUEELTVISON
01803         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTVISON
01804         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTVISON
01805         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTVISON
01806            THRU 9500-EXIT                                         ELTVISON
01807         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTVISON
01808            THRU 9200-EXIT                                         ELTVISON
01809         MOVE '7160' TO WS-PARA-ID.                                ELTVISON
01810                                                                   ELTVISON
01811      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTVISON
01812         SET PLT-INDEX2  TO  2                                     ELTVISON
01813      ELSE                                                         ELTVISON
01814         SET PLT-INDEX2  TO  1.                                    ELTVISON
01815                                                                   ELTVISON
01816  7160-EXIT.  EXIT.                                                ELTVISON
01817       TITLE 'TRANSFER TO OTHR RESPONSIBILITY'.                    ELTVISON
01818  7165-TRANS-OTHR-RESPON-IND.                                      ELTVISON
01819 ******************************************************************ELTVISON
01820 *   T R A N S F E R   T O   O T H E R  R E S P O N S I B I L I T YELTVISON
01821 *                     I N D I C A T O R                     10/89 ELTVISON
01822 ******************************************************************ELTVISON
01823      MOVE '7165'            TO  WS-PARA-ID.                       ELTVISON
01824                                                                   ELTVISON
01825      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)           =  ZEROS   ELTVISON
01826          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)  NOT  = ZEROS    ELTVISON
01827              SET PLT-INDEX2  TO  2                                ELTVISON
01828          ELSE                                                     ELTVISON
01829              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTVISON
01830              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTVISON
01831                 THRU 9200-EXIT                                    ELTVISON
01832              GO TO 7165-EXIT                                      ELTVISON
01833      ELSE                                                         ELTVISON
01834          SET PLT-INDEX2  TO  1.                                   ELTVISON
01835                                                                   ELTVISON
01836      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZERO ELTVISON
01837         GO TO 7165-EXIT.                                          ELTVISON
01838                                                                   ELTVISON
01839      MOVE +1  TO  WS-CIA.                                         ELTVISON
01840      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTVISON
01841                                                                   ELTVISON
01842      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTVISON
01843                                                                   ELTVISON
01844      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTVISON
01845           TO  CMF-CODE-VALUE.                                     ELTVISON
01846                                                                   ELTVISON
01847      MOVE SPACES            TO  WS-TEMP-TEXT-AREA.                ELTVISON
01848      MOVE +0                TO  WS-TEMP-NOT-USED-CNT.             ELTVISON
01849                                                                   ELTVISON
01850      PERFORM 9500-CALL-CODES-MANUAL-LONG  THRU 9500-EXIT.         ELTVISON
01851                                                                   ELTVISON
01852      PERFORM 9200-TEXT-OUTPUT-REQUEST     THRU 9200-EXIT.         ELTVISON
01853                                                                   ELTVISON
01854  7165-EXIT.       EXIT.                                           ELTVISON
01855                                                                   ELTVISON
01856      TITLE 'ARR TABULARS'.                                        ELTVISON
01857  7190-ALL-LEVEL-TABS.                                             ELTVISON
01858 ****************************************************************  ELTVISON
01859 *                  A A R   T A B U L A R                       *  ELTVISON
01860 ****************************************************************  ELTVISON
01861      MOVE '7190' TO WS-PARA-ID.                                   ELTVISON
01862      SET PLT-INDEX2  TO  1.                                       ELTVISON
01863      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTVISON
01864         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
01865                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01866         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01867         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTVISON
01868         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTVISON
01869      ELSE                                                         ELTVISON
01870         SET PLT-INDEX2  TO  2                                     ELTVISON
01871         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
01872            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
01873                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01874            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTVISON
01875            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTVISON
01876            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTVISON
01877                                                                   ELTVISON
01878      IF WS-ADD-A-BLANK-LINE                                       ELTVISON
01879         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTVISON
01880         MOVE 1  TO  WS-CIA                                        ELTVISON
01881         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTVISON
01882             COMMAREA(DFHCOMMAREA)                                 ELTVISON
01883         END-EXEC.                                                 ELTVISON
01884      TITLE 'PPF TABULARS'.                                        ELTVISON
01885 *--------------------------------------------------------------*  ELTVISON
01886 *                  P P F   T A B U L A R                       *  ELTVISON
01887 *--------------------------------------------------------------*  ELTVISON
01888      SET PLT-INDEX2  TO  1.                                       ELTVISON
01889      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTVISON
01890         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
01891                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01892         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTVISON
01893                                             KWA-GCTABULR-KEY      ELTVISON
01894         PERFORM 9900-GET-TABULAR-RECORD                           ELTVISON
01895            THRU 9900-EXIT                                         ELTVISON
01896         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTVISON
01897               COMMAREA(DFHCOMMAREA)                               ELTVISON
01898         END-EXEC                                                  ELTVISON
01899      ELSE                                                         ELTVISON
01900         SET PLT-INDEX2  TO  2                                     ELTVISON
01901         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
01902            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
01903                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01904          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTVISON
01905                                             KWA-GCTABULR-KEY      ELTVISON
01906            PERFORM 9900-GET-TABULAR-RECORD                        ELTVISON
01907               THRU 9900-EXIT                                      ELTVISON
01908            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTVISON
01909                  COMMAREA(DFHCOMMAREA)                            ELTVISON
01910            END-EXEC.                                              ELTVISON
01911 *--------------------------------------------------------------*  ELTVISON
01912 *                  P V E   T A B U L A R                       *  ELTVISON
01913 *--------------------------------------------------------------*  ELTVISON
01914                                                                   ELTVISON
01915      MOVE  +2     TO  WS-CIA.                                     ELTVISON
01916      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTVISON
01917      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTVISON
01918         THRU 9200-EXIT.                                           ELTVISON
01919                                                                   ELTVISON
01920      TITLE 'ABM TABULARS'.                                        ELTVISON
01921 *--------------------------------------------------------------*  ELTVISON
01922 *                  A B M   T A B U L A R                       *  ELTVISON
01923 *--------------------------------------------------------------*  ELTVISON
01924      SET PLT-INDEX2  TO  1.                                       ELTVISON
01925      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTVISON
01926                              AND                                  ELTVISON
01927         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
01928                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01929         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTVISON
01930                                              KWA-GCTABULR-KEY     ELTVISON
01931         PERFORM 9900-GET-TABULAR-RECORD                           ELTVISON
01932            THRU 9900-EXIT                                         ELTVISON
01933         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTVISON
01934               COMMAREA(DFHCOMMAREA)                               ELTVISON
01935         END-EXEC                                                  ELTVISON
01936      ELSE                                                         ELTVISON
01937         SET PLT-INDEX2  TO  2                                     ELTVISON
01938         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
01939            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
01940                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01941          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTVISON
01942                                             KWA-GCTABULR-KEY      ELTVISON
01943            PERFORM 9900-GET-TABULAR-RECORD                        ELTVISON
01944               THRU 9900-EXIT                                      ELTVISON
01945            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTVISON
01946                  COMMAREA(DFHCOMMAREA)                            ELTVISON
01947            END-EXEC.                                              ELTVISON
01948      TITLE 'ACL TABULARS'.                                        ELTVISON
01949 *--------------------------------------------------------------*  ELTVISON
01950 *                  A C L   T A B U L A R                       *  ELTVISON
01951 *--------------------------------------------------------------*  ELTVISON
01952      SET PLT-INDEX2  TO  1.                                       ELTVISON
01953      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTVISON
01954         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
01955                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01956         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTVISON
01957                                              KWA-GCTABULR-KEY     ELTVISON
01958         PERFORM 9900-GET-TABULAR-RECORD                           ELTVISON
01959            THRU 9900-EXIT                                         ELTVISON
01960         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTVISON
01961               COMMAREA(DFHCOMMAREA)                               ELTVISON
01962         END-EXEC                                                  ELTVISON
01963      ELSE                                                         ELTVISON
01964         SET PLT-INDEX2  TO  2                                     ELTVISON
01965         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
01966            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
01967                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01968          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTVISON
01969                                              KWA-GCTABULR-KEY     ELTVISON
01970            PERFORM 9900-GET-TABULAR-RECORD                        ELTVISON
01971               THRU 9900-EXIT                                      ELTVISON
01972            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTVISON
01973                  COMMAREA(DFHCOMMAREA)                            ELTVISON
01974            END-EXEC.                                              ELTVISON
01975      TITLE 'ADL TABULARS'.                                        ELTVISON
01976 *--------------------------------------------------------------*  ELTVISON
01977 *                  A D L   T A B U L A R                       *  ELTVISON
01978 *--------------------------------------------------------------*  ELTVISON
01979      SET PLT-INDEX2  TO  1.                                       ELTVISON
01980      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTVISON
01981         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
01982                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01983         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTVISON
01984                                             KWA-GCTABULR-KEY      ELTVISON
01985         PERFORM 9900-GET-TABULAR-RECORD                           ELTVISON
01986            THRU 9900-EXIT                                         ELTVISON
01987         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTVISON
01988               COMMAREA(DFHCOMMAREA)                               ELTVISON
01989         END-EXEC                                                  ELTVISON
01990      ELSE                                                         ELTVISON
01991         SET PLT-INDEX2  TO  2                                     ELTVISON
01992         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
01993            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
01994                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
01995          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTVISON
01996                                               KWA-GCTABULR-KEY    ELTVISON
01997            PERFORM 9900-GET-TABULAR-RECORD                        ELTVISON
01998               THRU 9900-EXIT                                      ELTVISON
01999            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTVISON
02000                  COMMAREA(DFHCOMMAREA)                            ELTVISON
02001            END-EXEC.                                              ELTVISON
02002      TITLE 'AOL TABULARS'.                                        ELTVISON
02003 *--------------------------------------------------------------*  ELTVISON
02004 *                  A O L   T A B U L A R                       *  ELTVISON
02005 *--------------------------------------------------------------*  ELTVISON
02006      SET PLT-INDEX2  TO  1.                                       ELTVISON
02007      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTVISON
02008         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTVISON
02009                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
02010         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTVISON
02011                                             KWA-GCTABULR-KEY      ELTVISON
02012         PERFORM 9900-GET-TABULAR-RECORD                           ELTVISON
02013            THRU 9900-EXIT                                         ELTVISON
02014         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTVISON
02015               COMMAREA(DFHCOMMAREA)                               ELTVISON
02016         END-EXEC                                                  ELTVISON
02017      ELSE                                                         ELTVISON
02018         SET PLT-INDEX2  TO  2                                     ELTVISON
02019         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTVISON
02020            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTVISON
02021                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTVISON
02022          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTVISON
02023                                              KWA-GCTABULR-KEY     ELTVISON
02024            PERFORM 9900-GET-TABULAR-RECORD                        ELTVISON
02025               THRU 9900-EXIT                                      ELTVISON
02026            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTVISON
02027                  COMMAREA(DFHCOMMAREA)                            ELTVISON
02028            END-EXEC.                                              ELTVISON
02029 *--------------------------------------------------------------*  ELTVISON
02030 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTVISON
02031 *--------------------------------------------------------------*  ELTVISON
02032                                                                   ELTVISON
02033      ADD   +2     TO  WS-CIA.                                     ELTVISON
02034      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTVISON
02035      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTVISON
02036         THRU 9200-EXIT.                                           ELTVISON
02037                                                                   ELTVISON
02038  7190-EXIT.  EXIT.                                                ELTVISON
02039                                                                   ELTVISON
02040      TITLE ' OUTPUT SECTIONS - ELTVISON'.                         ELTVISON
02041 ****************************************************************  ELTVISON
02042 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTVISON
02043 ****************************************************************  ELTVISON
02044  9100-HEADER-OUTPUT-REQUEST.                                      ELTVISON
02045                                                                   ELTVISON
02046      MOVE '9100' TO WS-PARA-ID.                                   ELTVISON
02047                                                                   ELTVISON
02048      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTVISON
02049      MOVE ZERO           TO  COF-NBR-DTL-LINES.                   ELTVISON
02050      MOVE 'P'            TO  COF-FUNCTION.                        ELTVISON
02051                                                                   ELTVISON
02052      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
02053                     COMMAREA (DFHCOMMAREA)                        ELTVISON
02054                     END-EXEC.                                     ELTVISON
02055                                                                   ELTVISON
02056  9100-EXIT.  EXIT.                                                ELTVISON
02057      SKIP3                                                        ELTVISON
02058 ****************************************************************  ELTVISON
02059 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTVISON
02060 ****************************************************************  ELTVISON
02061  9200-TEXT-OUTPUT-REQUEST.                                        ELTVISON
02062                                                                   ELTVISON
02063      MOVE '9200' TO WS-PARA-ID.                                   ELTVISON
02064                                                                   ELTVISON
02065      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTVISON
02066      MOVE +0      TO  COF-NBR-HDR-LINES                           ELTVISON
02067                       WS-CIA.                                     ELTVISON
02068      MOVE ' '     TO  COF-FUNCTION.                               ELTVISON
02069                                                                   ELTVISON
02070      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTVISON
02071                     COMMAREA (DFHCOMMAREA)                        ELTVISON
02072                     END-EXEC.                                     ELTVISON
02073                                                                   ELTVISON
02074      MOVE +1 TO       WS-CIA.                                     ELTVISON
02075  9200-EXIT.  EXIT.                                                ELTVISON
02076      TITLE ' CODES MAN FOR LONG DESCRIPTION'.                     ELTVISON
02077  9500-CALL-CODES-MANUAL-LONG.                                     ELTVISON
02078 * ---------------------------------------------------------------*ELTVISON
02079 *    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTVISON
02080 * ---------------------------------------------------------------*ELTVISON
02081      MOVE '9500' TO WS-PARA-ID.                                   ELTVISON
02082      INITIALIZE CMF-RETURN-CODE                                   ELTVISON
02083                 TCAR-FROM-AREA.                                   ELTVISON
02084                                                                   ELTVISON
02085      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTVISON
02086                       COMMAREA(DFHCOMMAREA)                       ELTVISON
02087      END-EXEC.                                                    ELTVISON
02088                                                                   ELTVISON
02089      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTVISON
02090      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
02091              ADDRESS OF    CMF-DESCR.                             ELTVISON
02092                                                                   ELTVISON
02093      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTVISON
02094         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTVISON
02095         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTVISON
02096      ELSE                                                         ELTVISON
02097         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTVISON
02098                                                                   ELTVISON
02099      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTVISON
02100          VARYING WS-SUB1 FROM 1 BY 1                              ELTVISON
02101          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTVISON
02102                                                                   ELTVISON
02103      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTVISON
02104                                                                   ELTVISON
02105      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTVISON
02106      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTVISON
02107      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTVISON
02108                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTVISON
02109                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTVISON
02110      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTVISON
02111                                                                   ELTVISON
02112      IF WS-MOVE-LINES-TO-CIA                                      ELTVISON
02113         IF WS-TEMP-NOT-USED-CNT NOT =  ZERO                       ELTVISON
02114            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTVISON
02115                                             WS-TEMP-NOT-USED-CNT  ELTVISON
02116            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTVISON
02117               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTVISON
02118                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTVISON
02119            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTVISON
02120            ADD +1  TO  WS-CIA                                     ELTVISON
02121            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTVISON
02122         ELSE                                                      ELTVISON
02123            ADD +1  TO  WS-CIA                                     ELTVISON
02124            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTVISON
02125                                                                   ELTVISON
02126      IF WS-MOVE-LINES-TO-CIA                                      ELTVISON
02127         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTVISON
02128            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTVISON
02129               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTVISON
02130                              UNTIL   WS-SUB1 >                    ELTVISON
02131                              TCAR-OUTPUT-FIELDS-USED              ELTVISON
02132         ELSE                                                      ELTVISON
02133            CONTINUE                                               ELTVISON
02134      ELSE                                                         ELTVISON
02135         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTVISON
02136                                                                   ELTVISON
02137  9500-EXIT.  EXIT.                                                ELTVISON
02138                                                                   ELTVISON
02139  9540-MOVE-LINES-OUT.                                             ELTVISON
02140      MOVE '9540' TO WS-PARA-ID.                                   ELTVISON
02141      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTVISON
02142             ' ' DELIMITED BY SIZE                                 ELTVISON
02143             CMF-DESCR-LINE (WS-SUB1) DELIMITED BY SIZE            ELTVISON
02144      INTO TCAR-FROM-AREA.                                         ELTVISON
02145                                                                   ELTVISON
02146  9540-EXIT.  EXIT.                                                ELTVISON
02147                                                                   ELTVISON
02148  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTVISON
02149      MOVE '9550' TO WS-PARA-ID.                                   ELTVISON
02150      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTVISON
02151      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTVISON
02152                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTVISON
02153                                                                   ELTVISON
02154  9550-EXIT.  EXIT.                                                ELTVISON
02155                                                                   ELTVISON
02156 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTVISON
02157  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTVISON
02158      MOVE '9600' TO WS-PARA-ID.                                   ELTVISON
02159                                                                   ELTVISON
02160      INITIALIZE CMF-RETURN-CODE                                   ELTVISON
02161                 TCAR-FROM-AREA.                                   ELTVISON
02162                                                                   ELTVISON
02163      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTVISON
02164                       COMMAREA(DFHCOMMAREA)                       ELTVISON
02165      END-EXEC.                                                    ELTVISON
02166                                                                   ELTVISON
02167      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTVISON
02168      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
02169              ADDRESS OF    CMF-DESCR.                             ELTVISON
02170                                                                   ELTVISON
02171                                                                   ELTVISON
02172      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTVISON
02173         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTVISON
02174         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTVISON
02175      ELSE                                                         ELTVISON
02176         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTVISON
02177                                                                   ELTVISON
02178      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTVISON
02179             WS-PERCENT-FLD DELIMITED BY SIZE                      ELTVISON
02180      INTO TCAR-FROM-AREA.                                         ELTVISON
02181                                                                   ELTVISON
02182      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTVISON
02183          VARYING WS-SUB1 FROM 1 BY 1                              ELTVISON
02184          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTVISON
02185      MOVE '9600' TO WS-PARA-ID.                                   ELTVISON
02186                                                                   ELTVISON
02187                                                                   ELTVISON
02188      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTVISON
02189                                                                   ELTVISON
02190      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTVISON
02191      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTVISON
02192      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTVISON
02193                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTVISON
02194                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTVISON
02195      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTVISON
02196                                                                   ELTVISON
02197      IF WS-MOVE-LINES-TO-CIA                                      ELTVISON
02198         IF WS-TEMP-NOT-USED-CNT NOT =  ZERO                       ELTVISON
02199            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTVISON
02200                                             WS-TEMP-NOT-USED-CNT  ELTVISON
02201            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTVISON
02202               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTVISON
02203                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTVISON
02204            MOVE '9600' TO WS-PARA-ID                              ELTVISON
02205            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTVISON
02206            ADD +1  TO  WS-CIA                                     ELTVISON
02207            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTVISON
02208         ELSE                                                      ELTVISON
02209            ADD +1  TO  WS-CIA                                     ELTVISON
02210            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTVISON
02211                                                                   ELTVISON
02212      IF WS-MOVE-LINES-TO-CIA                                      ELTVISON
02213         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTVISON
02214            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTVISON
02215               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTVISON
02216                              UNTIL   WS-SUB1 >                    ELTVISON
02217                              TCAR-OUTPUT-FIELDS-USED              ELTVISON
02218            MOVE '9600' TO WS-PARA-ID                              ELTVISON
02219         ELSE                                                      ELTVISON
02220            CONTINUE                                               ELTVISON
02221      ELSE                                                         ELTVISON
02222         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTVISON
02223                                                                   ELTVISON
02224  9600-EXIT.  EXIT.                                                ELTVISON
02225                                                                   ELTVISON
02226  9660-MOVE-LINES-TO-CIA.                                          ELTVISON
02227      MOVE '9660' TO WS-PARA-ID                                    ELTVISON
02228      ADD +1  TO  WS-CIA.                                          ELTVISON
02229      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTVISON
02230                                                                   ELTVISON
02231  9660-EXIT.  EXIT.                                                ELTVISON
02232      TITLE 'GET TABULAR RECORD'.                                  ELTVISON
02233  9900-GET-TABULAR-RECORD.                                         ELTVISON
02234 ***************************************************************** ELTVISON
02235 *            G E T   T A B U L A R   R E C O R D                  ELTVISON
02236 *                                                                 ELTVISON
02237 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF            ELTVISON
02238 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTVISON
02239 *  TO DISPLAY.                                                    ELTVISON
02240 *                                                                 ELTVISON
02241 ***************************************************************** ELTVISON
02242      MOVE '9990' TO WS-PARA-ID                                    ELTVISON
02243                                                                   ELTVISON
02244      SET CIA-GCTABULR-DDN TO TRUE.                                ELTVISON
02245      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTVISON
02246                       ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.     ELTVISON
02247      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTVISON
02248      SET CIA-GCTABULR-DDN TO TRUE.                                ELTVISON
02249      SET IOP-RD                          TO TRUE.                 ELTVISON
02250      SET IOP-FCQ-NONE                    TO TRUE.                 ELTVISON
02251      SET IOP-KVQ-NONE                    TO TRUE.                 ELTVISON
02252      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTVISON
02253                                                                   ELTVISON
02254      EXEC CICS LINK                                               ELTVISON
02255                PROGRAM ('ELUIOPGM')                               ELTVISON
02256                COMMAREA (DFHCOMMAREA)                             ELTVISON
02257      END-EXEC.                                                    ELTVISON
02258                                                                   ELTVISON
02259      IF IOP-RC-NOTFND                                             ELTVISON
02260         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTVISON
02261         EXEC CICS ABEND                                           ELTVISON
02262                   ABCODE(CIA-ABCODE)                              ELTVISON
02263         END-EXEC                                                  ELTVISON
02264      ELSE                                                         ELTVISON
02265          IF NOT IOP-RC-OK                                         ELTVISON
02266             SET CIA-AB-CRITIO TO TRUE                             ELTVISON
02267             EXEC CICS ABEND                                       ELTVISON
02268                       ABCODE(CIA-ABCODE)                          ELTVISON
02269             END-EXEC                                              ELTVISON
02270      END-IF.                                                      ELTVISON
02271                                                                   ELTVISON
02272  9900-EXIT.  EXIT.                                                ELTVISON
02273      TITLE 'TEXT COMPRESSION AND EXPANSION'.                      ELTVISON
02274      COPY ELSTCOMP.                                               ELTVISON
02275      TITLE ' ELS VISION BEN --- ELTVISON'.                        ELTVISON
