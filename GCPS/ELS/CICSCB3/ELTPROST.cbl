00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELTPROST.                                         ELTPROST
00003  AUTHOR.        LUCY TORRES.                                         LV001
00004  DATE-WRITTEN.  07/17/86                                          ELTPROST
00005  DATE-COMPILED.                                                   ELTPROST
00006      SKIP3                                                        ELTPROST
00007 ****************************************************************  ELTPROST
00008 *      ELTPROST - ELS:  PROSTHETICS TOPIC PROGRAM              *  ELTPROST
00009 ****************************************************************  ELTPROST
00010      SKIP3                                                        ELTPROST
00011 ****************************************************************  ELTPROST
00012 *              U P D A T E   H I S T O R Y                     *  ELTPROST
00013 *                                                              *  ELTPROST
00014 *   DATE    PGM  DESCRIPTION                                   *  ELTPROST
00015 * --------  ---  --------------------------------------------- *  ELTPROST
00016 * 07/17/86  LET  ORIGINAL VERSION                              *  ELTPROST
00017 * 07/22/86  LET  CORRECTED THE SPELLING OF A BEN PROV ID.      *  ELTPROST
00018 * 08/12/86  WCH  DISCREPANCY NUMBER P1028 FIX.                 *  ELTPROST
00019 * 09/22/86  NAC  VS COBOL II CONVERSION.                       *  ELTPROST
00020 * 11/25/86  LET  CHANGED CODE TO ACCOMMODATE THE MOVING OF THE *  ELTPROST
00021 *                CERTIFICATION REQUIREMENT INDICATOR FROM THE  *  ELTPROST
00022 *                FORMAT TYPE SECTION TO THE COMMON SECTION.    *  ELTPROST
00023 * 10/21/87  EGL  CHANGED FIXED TEXT.                           *  ELTPROST
00024 * 03/22/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.              *  ELTPROST
00025 * 10/13/89  RKH  ADDED TRANSFER TO OTHER RESPONSIBILITY IND    *  ELTPROST
00026 * 08/14/90  GEM  ADDED BENEFIT PROVISIONS IDS                  *  ELTPROST
00027 * 09/10/90  GEM  CORRECTED STG MGT BOO BOO, NOW TABLE-MAX = 4. *  ELTPROST
00028 * 11/16/90  GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND COMPARE TO  *  ELTPROST
00029 *                LITERAL ZERO INSTEAD OF THE DIGIT '0'.        *  ELTPROST
00030 * 11/29/90  GEM  CHANGED CERIFICATION TO CERTIFICATION.        *  ELTPROST
00031 ****************************************************************  ELTPROST
00032      SKIP3                                                        ELTPROST
00033  ENVIRONMENT DIVISION.                                            ELTPROST
00034      SKIP3                                                        ELTPROST
00035  DATA DIVISION.                                                   ELTPROST
00036  WORKING-STORAGE SECTION.                                         ELTPROST
00037  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTPROST
00038          '** ELTPROST WS BEGINS **'.                              ELTPROST
00039 /                                                                 ELTPROST
00040 ****************************************************************  ELTPROST
00041 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTPROST
00042 ****************************************************************  ELTPROST
00043  01  WORK-FIELDS.                                                 ELTPROST
00044      05  WS-HEX-00               PIC  X(01).                      ELTPROST
00045      05  WS-CHAR-0               PIC  X(01).                      ELTPROST
00046      05  WS-SUB                  PIC S9(03) COMP VALUE +0.        ELTPROST
00047      05  WS-SUB1                 PIC S9(03) COMP VALUE +0.        ELTPROST
00048      05  WS-SUB2                 PIC S9(03) COMP VALUE +0.        ELTPROST
00049      05  WS-SUB3                 PIC S9(03) COMP VALUE +0.        ELTPROST
00050      05  WS-SUB4                 PIC S9(03) COMP VALUE +0.        ELTPROST
00051      05  WS-CIA                  PIC S9(03) COMP VALUE +0.        ELTPROST
00052      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP.                 ELTPROST
00053      05  WS-PERCENT-FLD.                                          ELTPROST
00054        10  WS-PERCENTAGE         PIC ZZ9.                         ELTPROST
00055        10  WS-PERCENT-SIGN       PIC X.                           ELTPROST
00056      05  WS-EXPLANATION-IND      PIC S9 COMP.                     ELTPROST
00057          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTPROST
00058          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTPROST
00059          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTPROST
00060          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTPROST
00061          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTPROST
00062          88  WS-NO-EXPLANATION             VALUE +0.              ELTPROST
00063      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP.                     ELTPROST
00064      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP.                     ELTPROST
00065                                                                   ELTPROST
00066  01  WS-WORK-AREA.                                                ELTPROST
00067      05  WS-MAX-AMT.                                              ELTPROST
00068          10  WS-BASIC-SUPP       PIC  X(17) VALUE SPACES.         ELTPROST
00069          10  WS-EDIT-MAX-AMT     PIC  -$$9.99.                    ELTPROST
00070                                                                   ELTPROST
00071  01  WS-EXPLAINS.                                                 ELTPROST
00072    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTPROST
00073    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTPROST
00074    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTPROST
00075    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTPROST
00076                                                                   ELTPROST
00077  01  SWITCHES.                                                    ELTPROST
00078      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTPROST
00079          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTPROST
00080      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTPROST
00081          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTPROST
00082      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTPROST
00083          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTPROST
00084      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTPROST
00085          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTPROST
00086                                                                   ELTPROST
00087 *--------------------------------------------------------------*  ELTPROST
00088 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTPROST
00089 *--------------------------------------------------------------*  ELTPROST
00090  01  TABLE-MAX                   PIC S9(03) VALUE +4 COMP.        ELTPROST
00091 * 3  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTPROST
00092                                                                   ELTPROST
00093  01  WS-BEN-PROV-IDS.                                             ELTPROST
00094      05  WS-INST-IP-CNT          PIC S9(03) VALUE +4 COMP.        ELTPROST
00095      05  WS-INST-IP-TABS.                                         ELTPROST
00096          10  FILLER              PIC  X(06) VALUE 'PRAI B'.       ELTPROST
00097          10  FILLER              PIC  X(06) VALUE 'RPAI B'.       ELTPROST
00098          10  FILLER              PIC  X(06) VALUE 'RPCI B'.       ELTPROST
00099          10  FILLER              PIC  X(06) VALUE 'RPMI B'.       ELTPROST
00100      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTPROST
00101                                  PIC  X(06) OCCURS 4 TIMES.       ELTPROST
00102                                                                   ELTPROST
00103      05  WS-INST-OP-CNT          PIC S9(03) VALUE +4 COMP.        ELTPROST
00104      05  WS-INST-OP-TABS.                                         ELTPROST
00105          10  FILLER              PIC  X(06) VALUE 'PRAO B'.       ELTPROST
00106          10  FILLER              PIC  X(06) VALUE 'RPAO B'.       ELTPROST
00107          10  FILLER              PIC  X(06) VALUE 'RPCO B'.       ELTPROST
00108          10  FILLER              PIC  X(06) VALUE 'RPMO B'.       ELTPROST
00109      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTPROST
00110                                  PIC  X(06) OCCURS 4 TIMES.       ELTPROST
00111                                                                   ELTPROST
00112      05  WS-PROF-IP-CNT          PIC S9(03) VALUE +4 COMP.        ELTPROST
00113      05  WS-PROF-IP-TABS.                                         ELTPROST
00114          10  FILLER              PIC  X(06) VALUE 'PRAI E'.       ELTPROST
00115          10  FILLER              PIC  X(06) VALUE 'RPAI E'.       ELTPROST
00116          10  FILLER              PIC  X(06) VALUE 'RPCI E'.       ELTPROST
00117          10  FILLER              PIC  X(06) VALUE 'RPMI E'.       ELTPROST
00118      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTPROST
00119                                  PIC  X(06) OCCURS 4 TIMES.       ELTPROST
00120                                                                   ELTPROST
00121      05  WS-PROF-OP-CNT          PIC S9(03) VALUE +4 COMP.        ELTPROST
00122      05  WS-PROF-OP-TABS.                                         ELTPROST
00123          10  FILLER              PIC  X(06) VALUE 'PRAO E'.       ELTPROST
00124          10  FILLER              PIC  X(06) VALUE 'RPAO E'.       ELTPROST
00125          10  FILLER              PIC  X(06) VALUE 'RPCO E'.       ELTPROST
00126          10  FILLER              PIC  X(06) VALUE 'RPMO E'.       ELTPROST
00127      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTPROST
00128                                  PIC  X(06) OCCURS 4 TIMES.       ELTPROST
00129                                                                   ELTPROST
00130 /                                                                 ELTPROST
00131 ****************************************************************  ELTPROST
00132 *              HEADER AND LITERAL TEXT AREA                    *  ELTPROST
00133 ****************************************************************  ELTPROST
00134  01  HEADER-I-IP-LINE-3.                                          ELTPROST
00135      05  FILLER                  PIC  X(17) VALUE SPACES.         ELTPROST
00136      05  FILLER                  PIC  X(44) VALUE                 ELTPROST
00137              'PROSTHETIC APPLIANCE INSTITUTIONAL INPATIENT'.      ELTPROST
00138      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTPROST
00139                                                                   ELTPROST
00140  01  HEADER-I-OP-LINE-3.                                          ELTPROST
00141      05  FILLER                  PIC  X(17) VALUE SPACES.         ELTPROST
00142      05  FILLER                  PIC  X(45) VALUE                 ELTPROST
00143              'PROSTHETIC APPLIANCE INSTITUTIONAL OUTPATIENT'.     ELTPROST
00144      05  FILLER                  PIC  X(17) VALUE LOW-VALUES.     ELTPROST
00145                                                                   ELTPROST
00146  01  HEADER-P-IP-LINE-3.                                          ELTPROST
00147      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTPROST
00148      05  FILLER                  PIC  X(43) VALUE                 ELTPROST
00149              'PROSTHETIC APPLIANCE PROFESSIONAL INPATIENT'.       ELTPROST
00150      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTPROST
00151                                                                   ELTPROST
00152  01  HEADER-P-OP-LINE-3.                                          ELTPROST
00153      05  FILLER                  PIC  X(17) VALUE SPACES.         ELTPROST
00154      05  FILLER                  PIC  X(44) VALUE                 ELTPROST
00155              'PROSTHETIC APPLIANCE PROFESSIONAL OUTPATIENT'.      ELTPROST
00156      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTPROST
00157                                                                   ELTPROST
00158  01  WS-SERVICES-RENDERED.                                        ELTPROST
00159      05  FILLER                  PIC  X(26) VALUE                 ELTPROST
00160              'SERVICES MAY BE RENDERED: '.                        ELTPROST
00161      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTPROST
00162                                                                   ELTPROST
00163  01  WS-FOLLOWING-BEN.                                            ELTPROST
00164      05  FILLER                  PIC  X(22) VALUE                 ELTPROST
00165            'COVERED SERVICES ARE: '.                              ELTPROST
00166                                                                   ELTPROST
00167  01  WS-PAYABLE-AS.                                               ELTPROST
00168      10  FILLER                  PIC  X(40) VALUE                 ELTPROST
00169          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTPROST
00170                                                                   ELTPROST
00171  01  WS-CONTRACT-RELATED.                                         ELTPROST
00172      05  FILLER                  PIC  X(48) VALUE                 ELTPROST
00173              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTPROST
00174                                                                   ELTPROST
00175  01  WS-CERT-REQ.                                                 ELTPROST
00176      05  FILLER                   PIC  X(48) VALUE                ELTPROST
00177              'THE CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.  ELTPROST
00178      05  FILLER                   PIC  X(31) VALUE LOW-VALUES.    ELTPROST
00179                                                                   ELTPROST
00180  01  WS-RECERT-REQ.                                               ELTPROST
00181      05  FILLER                   PIC  X(56) VALUE                ELTPROST
00182              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTPROST
00183 -            'IS: '.                                              ELTPROST
00184      05  FILLER                   PIC  X(23) VALUE LOW-VALUES.    ELTPROST
00185                                                                   ELTPROST
00186  01  WS-RESTRICT.                                                 ELTPROST
00187      05  FILLER                  PIC  X(64) VALUE                 ELTPROST
00188              'THE RESTRICTIONS FOR REPAIR/REPLACEMENT OF THIS APPLELTPROST
00189 -            'IANCE ARE: '.                                       ELTPROST
00190      05  FILLER                  PIC  X(15) VALUE LOW-VALUES.     ELTPROST
00191                                                                   ELTPROST
00192  01  WS-BASIC.                                                    ELTPROST
00193      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTPROST
00194              '         BASIC: '.                                  ELTPROST
00195      05  WS-DTL-BASIC-LONG.                                       ELTPROST
00196          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTPROST
00197          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTPROST
00198                                                                   ELTPROST
00199  01  WS-SUPPLEMENTAL.                                             ELTPROST
00200      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTPROST
00201              '  SUPPLEMENTAL: '.                                  ELTPROST
00202      05  WS-DTL-SUPP-LONG.                                        ELTPROST
00203          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTPROST
00204          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTPROST
00205                                                                   ELTPROST
00206  01  WS-PVE.                                                      ELTPROST
00207      05  FILLER                  PIC  X(44) VALUE                 ELTPROST
00208              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTPROST
00209      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTPROST
00210                                                                   ELTPROST
00211  01  WS-ACCUM-MSG1.                                               ELTPROST
00212      05  FILLER                  PIC  X(79) VALUE                 ELTPROST
00213      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTPROST
00214 -    'CONSIDERATIONS.'.                                           ELTPROST
00215                                                                   ELTPROST
00216  01  WS-INDICES-PROBLEM.                                          ELTPROST
00217      05  FILLER                  PIC  X(20) VALUE                 ELTPROST
00218              'PROBLEM WITH INDICES'.                              ELTPROST
00219      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTPROST
00220                                                                   ELTPROST
00221  01  WS-POSSIBLE-ERROR.                                           ELTPROST
00222      05  FILLER                   PIC  X(50) VALUE                ELTPROST
00223              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTPROST
00224      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTPROST
00225                                                                   ELTPROST
00226  01  WS-INVALID-REQ.                                              ELTPROST
00227      05  FILLER                  PIC  X(37) VALUE                 ELTPROST
00228              '*** I N V A L I D   R E Q U E S T ***'.             ELTPROST
00229      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTPROST
00230                                                                   ELTPROST
00231  01  WS-SPILLOVER.                                                ELTPROST
00232      05  FILLER                  PIC  X(10) VALUE                 ELTPROST
00233              'SPILLOVER '.                                        ELTPROST
00234                                                                   ELTPROST
00235  01  WS-OTHER-LITERALS.                                           ELTPROST
00236    05  WS-NO-TABULAR1.                                            ELTPROST
00237      10  FILLER                    PIC X(51)  VALUE               ELTPROST
00238         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTPROST
00239      10  FILLER                    PIC X(22)  VALUE               ELTPROST
00240         'GOING FROM BENEFIT ***'.                                 ELTPROST
00241                                                                   ELTPROST
00242    05  WS-NO-TABULAR2.                                            ELTPROST
00243      10  FILLER                    PIC X(15)  VALUE               ELTPROST
00244         '*** PROVISION: '.                                        ELTPROST
00245      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTPROST
00246      10  FILLER                    PIC X VALUE SPACE.             ELTPROST
00247      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTPROST
00248      10  FILLER                    PIC X(13)  VALUE               ELTPROST
00249         ' TO TABULAR: '.                                          ELTPROST
00250      10  WS-NO-TAB-ID              PIC X(6).                      ELTPROST
00251      10  FILLER                    PIC X VALUE SPACE.             ELTPROST
00252      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTPROST
00253      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTPROST
00254                                                                   ELTPROST
00255    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTPROST
00256    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTPROST
00257      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTPROST
00258                                                                   ELTPROST
00259 /                                                                 ELTPROST
00260  LINKAGE SECTION.                                                 ELTPROST
00261  01  DFHCOMMAREA.                                                 ELTPROST
00262      COPY ELSCOMMC.                                               ELTPROST
00263 /                                                                 ELTPROST
00264      COPY ELSCIA2C.                                               ELTPROST
00265 /                                                                 ELTPROST
00266      COPY ELSIOPMC.                                               ELTPROST
00267 /                                                                 ELTPROST
00268      COPY ELSKEYSC.                                               ELTPROST
00269 /                                                                 ELTPROST
00270      COPY ELSOUTPC.                                               ELTPROST
00271 /                                                                 ELTPROST
00272      COPY ELSSSCBC.                                               ELTPROST
00273 /                                                                 ELTPROST
00274      COPY ELSCMIFC.                                               ELTPROST
00275 /                                                                 ELTPROST
00276      COPY ELSCMDSC.                                               ELTPROST
00277 /                                                                 ELTPROST
00278      COPY ELSPRVNC.                                               ELTPROST
00279 /                                                                 ELTPROST
00280 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTPROST
00281      COPY ELSPLGSW.                                               ELTPROST
00282 *** BENEFIT PROVISION TABLE OF FLDS                               ELTPROST
00283      COPY ELSPLGTB.                                               ELTPROST
00284 /                                                                 ELTPROST
00285      COPY ELSTCWAC.                                               ELTPROST
00286 /                                                                 ELTPROST
00287  PROCEDURE DIVISION.                                              ELTPROST
00288  0000-MAINLINE.                                                   ELTPROST
00289                                                                   ELTPROST
00290      PERFORM 1000-INITIALIZATION                                  ELTPROST
00291         THRU 1000-EXIT.                                           ELTPROST
00292                                                                   ELTPROST
00293      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTPROST
00294                       AND                                         ELTPROST
00295         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTPROST
00296          PERFORM 2000-INSTITUTIONAL-IP THRU 2000-EXIT.            ELTPROST
00297                                                                   ELTPROST
00298      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTPROST
00299                       AND                                         ELTPROST
00300         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTPROST
00301          PERFORM 3000-INSTITUTIONAL-OP THRU 3000-EXIT.            ELTPROST
00302                                                                   ELTPROST
00303      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTPROST
00304                       AND                                         ELTPROST
00305         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTPROST
00306          PERFORM 4000-PROFESSIONAL-IP THRU 4000-EXIT.             ELTPROST
00307                                                                   ELTPROST
00308      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTPROST
00309                       AND                                         ELTPROST
00310         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTPROST
00311          PERFORM 5000-PROFESSIONAL-OP THRU 5000-EXIT.             ELTPROST
00312                                                                   ELTPROST
00313      IF (SSB-PROV-CLASS-INST OR                                   ELTPROST
00314          SSB-PROV-CLASS-BOTH OR                                   ELTPROST
00315          SSB-PROV-CLASS-PROF)                                     ELTPROST
00316                         AND                                       ELTPROST
00317         (SSB-SERV-CLASS-IP   OR                                   ELTPROST
00318          SSB-SERV-CLASS-OP   OR                                   ELTPROST
00319          SSB-SERV-CLASS-BOTH)                                     ELTPROST
00320            CONTINUE                                               ELTPROST
00321      ELSE                                                         ELTPROST
00322          MOVE ' '             TO  COF-FUNCTION                    ELTPROST
00323          MOVE +0              TO  COF-NBR-HDR-LINES               ELTPROST
00324          MOVE +2              TO  COF-NBR-DTL-LINES               ELTPROST
00325          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTPROST
00326          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTPROST
00327                           COMMAREA(DFHCOMMAREA)                   ELTPROST
00328          END-EXEC                                                 ELTPROST
00329      END-IF.                                                      ELTPROST
00330                                                                   ELTPROST
00331      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTPROST
00332                                                                   ELTPROST
00333      SET CIA-STG-FREEMAIN TO TRUE.                                ELTPROST
00334      EXEC CICS LINK                                               ELTPROST
00335                PROGRAM('ELUSTGMG')                                ELTPROST
00336                COMMAREA(DFHCOMMAREA)                              ELTPROST
00337      END-EXEC.                                                    ELTPROST
00338                                                                   ELTPROST
00339                                                                   ELTPROST
00340      MOVE 'E'   TO  COF-FUNCTION.                                 ELTPROST
00341      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTPROST
00342                                                                   ELTPROST
00343      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
00344                     COMMAREA (DFHCOMMAREA)                        ELTPROST
00345      END-EXEC.                                                    ELTPROST
00346                                                                   ELTPROST
00347      EXEC CICS RETURN                                             ELTPROST
00348      END-EXEC.                                                    ELTPROST
00349                                                                   ELTPROST
00350      GOBACK.                                                      ELTPROST
00351 /                                                                 ELTPROST
00352  1000-INITIALIZATION.                                             ELTPROST
00353 ****************************************************************  ELTPROST
00354 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTPROST
00355 ****************************************************************  ELTPROST
00356                                                                   ELTPROST
00357      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTPROST
00358          EXEC CICS ABEND                                          ELTPROST
00359                    ABCODE ('EL01')                                ELTPROST
00360          END-EXEC                                                 ELTPROST
00361      END-IF.                                                      ELTPROST
00362                                                                   ELTPROST
00363 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTPROST
00364                                                                   ELTPROST
00365      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPROST
00366          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTPROST
00367                                                                   ELTPROST
00368      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPROST
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00370          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTPROST
00371                                                                   ELTPROST
00372      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPROST
00373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00374          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTPROST
00375                                                                   ELTPROST
00376      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPROST
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00378          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTPROST
00379                                                                   ELTPROST
00380      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPROST
00381      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00382          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTPROST
00383                                                                   ELTPROST
00384      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPROST
00385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00386          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTPROST
00387                                                                   ELTPROST
00388      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTPROST
00389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00390          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTPROST
00391                                                                   ELTPROST
00392      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPROST
00393                                                                   ELTPROST
00394      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTPROST
00395              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTPROST
00396                                                                   ELTPROST
00397      SET CIA-STG-GETMAIN TO TRUE.                                 ELTPROST
00398      EXEC CICS LINK                                               ELTPROST
00399                PROGRAM('ELUSTGMG')                                ELTPROST
00400                COMMAREA(DFHCOMMAREA)                              ELTPROST
00401      END-EXEC.                                                    ELTPROST
00402                                                                   ELTPROST
00403      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPROST
00404      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00405          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTPROST
00406                                                                   ELTPROST
00407                                                                   ELTPROST
00408      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTPROST
00409                                                                   ELTPROST
00410  1000-EXIT.  EXIT.                                                ELTPROST
00411 /                                                                 ELTPROST
00412 ****************************************************************  ELTPROST
00413 *       PROSTHETICS INSTITUTIONAL INPATIENT PROCESSING         *  ELTPROST
00414 ****************************************************************  ELTPROST
00415  2000-INSTITUTIONAL-IP.                                           ELTPROST
00416                                                                   ELTPROST
00417      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTPROST
00418                                                                   ELTPROST
00419      MOVE HEADER-I-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTPROST
00420                                                                   ELTPROST
00421      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTPROST
00422         THRU 9100-EXIT.                                           ELTPROST
00423                                                                   ELTPROST
00424      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPROST
00425      PERFORM WITH TEST BEFORE                                     ELTPROST
00426              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPROST
00427              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPROST
00428         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPROST
00429         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPROST
00430         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPROST
00431         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPROST
00432      END-PERFORM.                                                 ELTPROST
00433      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPROST
00434                                                                   ELTPROST
00435                                                                   ELTPROST
00436      PERFORM 2010-MOVE-IN-INST-IP-TABS                            ELTPROST
00437         THRU 2010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
00438                        UNTIL   WS-SUB  >     WS-INST-IP-CNT.      ELTPROST
00439                                                                   ELTPROST
00440      PERFORM 2020-CALL-COVERAGE                                   ELTPROST
00441         THRU 2020-EXIT.                                           ELTPROST
00442                                                                   ELTPROST
00443      IF PVN-COVG-NONE                                             ELTPROST
00444          GO TO 2000-EXIT.                                         ELTPROST
00445                                                                   ELTPROST
00446      PERFORM 2030-FIND-FIRST-NONZERO                              ELTPROST
00447         THRU 2030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
00448                        UNTIL   WS-SUB  > WS-INST-IP-CNT.          ELTPROST
00449                                                                   ELTPROST
00450  2000-EXIT.  EXIT.                                                ELTPROST
00451 /                                                                 ELTPROST
00452  2010-MOVE-IN-INST-IP-TABS.                                       ELTPROST
00453      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
00454      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTPROST
00455                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTPROST
00456                                                                   ELTPROST
00457      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTPROST
00458                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTPROST
00459                                                                   ELTPROST
00460  2010-EXIT.  EXIT.                                                ELTPROST
00461      SKIP3                                                        ELTPROST
00462  2020-CALL-COVERAGE.                                              ELTPROST
00463                                                                   ELTPROST
00464      MOVE 'PROSTHETIC APPLIANCES      ' TO  SSB-TOPIC-PHRASE.     ELTPROST
00465                                                                   ELTPROST
00466      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTPROST
00467                     COMMAREA (DFHCOMMAREA)                        ELTPROST
00468                     END-EXEC.                                     ELTPROST
00469                                                                   ELTPROST
00470      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
00471                     COMMAREA (DFHCOMMAREA)                        ELTPROST
00472                     END-EXEC.                                     ELTPROST
00473                                                                   ELTPROST
00474      IF PVN-COVG-NONE                                             ELTPROST
00475          GO TO 2020-EXIT.                                         ELTPROST
00476                                                                   ELTPROST
00477      MOVE +1  TO  WS-CIA.                                         ELTPROST
00478                                                                   ELTPROST
00479                                                                   ELTPROST
00480      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPROST
00481                    PSP-PROVN-PRICING-METHD,                       ELTPROST
00482                    PSP-TRANSF-OTHER-RESP-IND,                     ELTPROST
00483                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTPROST
00484                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTPROST
00485                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTPROST
00486                    PSP-SPILL-OVER-DED-APL-IND,                    ELTPROST
00487                    PSP-CERTFN-REQRM-IND,                          ELTPROST
00488                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTPROST
00489                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTPROST
00490                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTPROST
00491                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTPROST
00492                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTPROST
00493                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTPROST
00494                    PSB-CERTN-REPETN-REQRD-IND,                    ELTPROST
00495                    PSB-REPR-REPLAC-RESTRN-IND.                    ELTPROST
00496                                                                   ELTPROST
00497      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTPROST
00498                     COMMAREA (DFHCOMMAREA)                        ELTPROST
00499      END-EXEC.                                                    ELTPROST
00500                                                                   ELTPROST
00501      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPROST
00502      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
00503          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTPROST
00504                                                                   ELTPROST
00505  2020-EXIT.  EXIT.                                                ELTPROST
00506 /                                                                 ELTPROST
00507  2030-FIND-FIRST-NONZERO.                                         ELTPROST
00508      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
00509                                                                   ELTPROST
00510      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTPROST
00511          CONTINUE                                                 ELTPROST
00512      ELSE                                                         ELTPROST
00513          PERFORM 2100-BUILD-SCREEN-LINES                          ELTPROST
00514             THRU 2100-EXIT.                                       ELTPROST
00515                                                                   ELTPROST
00516  2030-EXIT.  EXIT.                                                ELTPROST
00517 /                                                                 ELTPROST
00518  2100-BUILD-SCREEN-LINES.                                         ELTPROST
00519      SET PLT-INDEX1  TO                                           ELTPROST
00520              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTPROST
00521                                                                   ELTPROST
00522      IF WS-NOT-FIRST-TIME                                         ELTPROST
00523         SET COF-NEW-PAGE TO TRUE                                  ELTPROST
00524         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTPROST
00525         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTPROST
00526                   COMMAREA (DFHCOMMAREA)                          ELTPROST
00527         END-EXEC                                                  ELTPROST
00528      ELSE                                                         ELTPROST
00529         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTPROST
00530                                                                   ELTPROST
00531      MOVE  +1  TO  WS-CIA.                                        ELTPROST
00532                                                                   ELTPROST
00533      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTPROST
00534          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTPROST
00535              SET PLT-INDEX2  TO  2                                ELTPROST
00536          ELSE                                                     ELTPROST
00537              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTPROST
00538              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTPROST
00539                 THRU 9200-EXIT                                    ELTPROST
00540              GO TO 2100-EXIT                                      ELTPROST
00541      ELSE                                                         ELTPROST
00542          SET PLT-INDEX2  TO  1.                                   ELTPROST
00543                                                                   ELTPROST
00544      PERFORM 2105-LIST-BEN-PROV                                   ELTPROST
00545         THRU 2105-EXIT.                                           ELTPROST
00546                                                                   ELTPROST
00547      PERFORM 2110-PLACE-OF-TREATMENT                              ELTPROST
00548         THRU 2110-EXIT.                                           ELTPROST
00549                                                                   ELTPROST
00550      PERFORM 2120-PRIC-METH                                       ELTPROST
00551         THRU 2120-EXIT.                                           ELTPROST
00552                                                                   ELTPROST
00553      PERFORM 2140-CERTIFICATION                                   ELTPROST
00554         THRU 2140-EXIT.                                           ELTPROST
00555                                                                   ELTPROST
00556      PERFORM 2150-RECERTIFICATION                                 ELTPROST
00557         THRU 2150-EXIT.                                           ELTPROST
00558                                                                   ELTPROST
00559      PERFORM 2155-REPR-REPL                                       ELTPROST
00560         THRU 2155-EXIT.                                           ELTPROST
00561                                                                   ELTPROST
00562      PERFORM 2160-SPILLOVR-COINS-N-DEDUC                          ELTPROST
00563         THRU 2160-EXIT.                                           ELTPROST
00564                                                                   ELTPROST
00565      PERFORM 2165-TRANS-OTHER-RESP-IND                            ELTPROST
00566         THRU 2165-EXIT.                                           ELTPROST
00567                                                                   ELTPROST
00568      PERFORM 7000-ALL-LEVEL-TABS                                  ELTPROST
00569         THRU 7000-EXIT.                                           ELTPROST
00570                                                                   ELTPROST
00571  2100-EXIT.  EXIT.                                                ELTPROST
00572 /                                                                 ELTPROST
00573  2105-LIST-BEN-PROV.                                              ELTPROST
00574 ****************************************************************  ELTPROST
00575 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTPROST
00576 ****************************************************************  ELTPROST
00577      MOVE  +2               TO  WS-CIA.                           ELTPROST
00578      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTPROST
00579      MOVE ZERO              TO  WS-SUB2.                          ELTPROST
00580      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTPROST
00581                             TO  WS-SUB3.                          ELTPROST
00582                                                                   ELTPROST
00583      PERFORM 2106-ZERO-ALL-WITH-SAME-NO                           ELTPROST
00584         THRU 2106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTPROST
00585                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.ELTPROST
00586                                                                   ELTPROST
00587      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
00588      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTPROST
00589                                                                   ELTPROST
00590      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
00591                     COMMAREA (DFHCOMMAREA)                        ELTPROST
00592                     END-EXEC.                                     ELTPROST
00593                                                                   ELTPROST
00594  2105-EXIT.  EXIT.                                                ELTPROST
00595      SKIP3                                                        ELTPROST
00596  2106-ZERO-ALL-WITH-SAME-NO.                                      ELTPROST
00597                                                                   ELTPROST
00598      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTPROST
00599          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTPROST
00600          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTPROST
00601          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTPROST
00602                            TO  CMF-CODE-VALUE                     ELTPROST
00603          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTPROST
00604          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTPROST
00605          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00606             THRU 9500-EXIT                                        ELTPROST
00607          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTPROST
00608          ADD  +1    TO  WS-SUB2                                   ELTPROST
00609          IF WS-CIA  >  20  OR  =  20                              ELTPROST
00610              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTPROST
00611              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTPROST
00612                             COMMAREA (DFHCOMMAREA)                ELTPROST
00613                             END-EXEC                              ELTPROST
00614              MOVE  +1  TO  WS-CIA.                                ELTPROST
00615                                                                   ELTPROST
00616  2106-EXIT.  EXIT.                                                ELTPROST
00617 /                                                                 ELTPROST
00618  2110-PLACE-OF-TREATMENT.                                         ELTPROST
00619 ****************************************************************  ELTPROST
00620 *              P L A C E   O F   T R E A T M E N T             *  ELTPROST
00621 ****************************************************************  ELTPROST
00622      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00623      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00624                          AND                                      ELTPROST
00625         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
00626                                                  NOT =  ZERO      ELTPROST
00627          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00628          MOVE +2                    TO  WS-CIA                    ELTPROST
00629          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00630                                                                   ELTPROST
00631      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00632      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00633                           AND                                     ELTPROST
00634         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
00635                                                 NOT  =  ZERO      ELTPROST
00636                           AND                                     ELTPROST
00637         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
00638          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00639          MOVE +2                    TO  WS-CIA                    ELTPROST
00640          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00641                                                                   ELTPROST
00642      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00643      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00644                           AND                                     ELTPROST
00645         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
00646                                                  NOT  =  ZERO     ELTPROST
00647          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
00648          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
00649                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00650          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
00651                               TO  CMF-CODE-VALUE                  ELTPROST
00652          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
00653          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00654          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00655             THRU 9500-EXIT.                                       ELTPROST
00656                                                                   ELTPROST
00657      SET PLT-INDEX2  TO  2.                                       ELTPROST
00658      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00659                           AND                                     ELTPROST
00660         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
00661                                                 NOT  =  ZERO      ELTPROST
00662          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
00663          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
00664                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00665          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
00666                               TO  CMF-CODE-VALUE                  ELTPROST
00667          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
00668          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00669          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00670             THRU 9500-EXIT.                                       ELTPROST
00671                                                                   ELTPROST
00672      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
00673         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
00674          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
00675             THRU 9200-EXIT.                                       ELTPROST
00676                                                                   ELTPROST
00677      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
00678         SET PLT-INDEX2  TO  2                                     ELTPROST
00679      ELSE                                                         ELTPROST
00680         SET PLT-INDEX2  TO  1.                                    ELTPROST
00681                                                                   ELTPROST
00682  2110-EXIT.  EXIT.                                                ELTPROST
00683 /                                                                 ELTPROST
00684  2120-PRIC-METH.                                                  ELTPROST
00685 ****************************************************************  ELTPROST
00686 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTPROST
00687 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTPROST
00688 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTPROST
00689 ****************************************************************  ELTPROST
00690      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
00692         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
00693                                                              '19' ELTPROST
00694         MOVE +2             TO  WS-CIA                            ELTPROST
00695         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
00696         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
00697                                                                   ELTPROST
00698      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00699      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
00700         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
00701                                                        '19' AND   ELTPROST
00702         NOT WS-ADD-A-BLANK-LINE                                   ELTPROST
00703         MOVE +2             TO  WS-CIA                            ELTPROST
00704         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
00705         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
00706                                                                   ELTPROST
00707      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00708      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
00709         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
00710                            AND                                    ELTPROST
00711         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00712         SET  PLT-INDEX2  TO  2                                    ELTPROST
00713         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
00714                                                             ZERO  ELTPROST
00715            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
00716            ADD +1  TO  WS-CIA                                     ELTPROST
00717            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
00718                                                                   ELTPROST
00719      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00720      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
00721         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
00722                            AND                                    ELTPROST
00723         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPROST
00724         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPROST
00725         ADD +1  TO  WS-CIA                                        ELTPROST
00726         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTPROST
00727                                                                   ELTPROST
00728      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPROST
00729         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00730         SET  PLT-INDEX2  TO  2                                    ELTPROST
00731         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
00732                                                             ZERO  ELTPROST
00733            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
00734            ADD +1  TO  WS-CIA                                     ELTPROST
00735            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
00736                                                                   ELTPROST
00737      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00739         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPROST
00740                                                            =  ZEROELTPROST
00741            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00742                                                            =  ZEROELTPROST
00743               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
00744            ELSE                                                   ELTPROST
00745               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
00746          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00747                                                  TO  WS-PERCENTAGEELTPROST
00748         ELSE                                                      ELTPROST
00749            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
00750          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00751                                                 TO  WS-PERCENTAGE.ELTPROST
00752      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
00753         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
00754                                             ZERO AND  NOT =  '19' ELTPROST
00755         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
00756         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
00757         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
00758                                                    CMF-CODE-VALUE ELTPROST
00759         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
00760         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
00761         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
00762            THRU 9600-EXIT.                                        ELTPROST
00763                                                                   ELTPROST
00764      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00765      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00766         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPROST
00767                                                               ZEROELTPROST
00768            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00769                                                            =  ZEROELTPROST
00770               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
00771            ELSE                                                   ELTPROST
00772               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
00773          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00774                                                  TO  WS-PERCENTAGEELTPROST
00775         ELSE                                                      ELTPROST
00776            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
00777          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
00778                                                 TO  WS-PERCENTAGE.ELTPROST
00779      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
00780         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
00781                                             ZERO AND  NOT =  '19' ELTPROST
00782         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
00783         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
00784         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
00785                                                    CMF-CODE-VALUE ELTPROST
00786         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPROST
00787         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
00788         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
00789            THRU 9600-EXIT.                                        ELTPROST
00790                                                                   ELTPROST
00791      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
00792          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTPROST
00793          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
00794             THRU 9200-EXIT.                                       ELTPROST
00795                                                                   ELTPROST
00796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
00797         SET PLT-INDEX2  TO  2                                     ELTPROST
00798      ELSE                                                         ELTPROST
00799         SET PLT-INDEX2  TO  1.                                    ELTPROST
00800                                                                   ELTPROST
00801  2120-EXIT.  EXIT.                                                ELTPROST
00802 /                                                                 ELTPROST
00803  2140-CERTIFICATION.                                              ELTPROST
00804 ****************************************************************  ELTPROST
00805 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTPROST
00806 ****************************************************************  ELTPROST
00807      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00808      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00809                          AND                                      ELTPROST
00810         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
00811                                                  NOT =  '00'      ELTPROST
00812          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00813          MOVE +2                    TO  WS-CIA                    ELTPROST
00814          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00815                                                                   ELTPROST
00816      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00817      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00818                           AND                                     ELTPROST
00819         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
00820                                                 NOT  =  '00'      ELTPROST
00821                           AND                                     ELTPROST
00822         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
00823          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00824          MOVE +2                    TO  WS-CIA                    ELTPROST
00825          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00826                                                                   ELTPROST
00827      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00828      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00829                           AND                                     ELTPROST
00830         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
00831                                                  NOT  =  '00'     ELTPROST
00832          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTPROST
00833          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
00834                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00835          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00836                               TO  CMF-CODE-VALUE                  ELTPROST
00837          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
00838          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00839          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00840             THRU 9500-EXIT.                                       ELTPROST
00841                                                                   ELTPROST
00842      SET PLT-INDEX2  TO  2.                                       ELTPROST
00843      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00844                           AND                                     ELTPROST
00845         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
00846                                                 NOT  =  '00'      ELTPROST
00847          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTPROST
00848          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
00849                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00850          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00851                               TO  CMF-CODE-VALUE                  ELTPROST
00852          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
00853          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00854          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00855             THRU 9500-EXIT.                                       ELTPROST
00856                                                                   ELTPROST
00857      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
00858         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
00859          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
00860             THRU 9200-EXIT.                                       ELTPROST
00861                                                                   ELTPROST
00862      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
00863         SET PLT-INDEX2  TO  2                                     ELTPROST
00864      ELSE                                                         ELTPROST
00865         SET PLT-INDEX2  TO  1.                                    ELTPROST
00866                                                                   ELTPROST
00867  2140-EXIT.  EXIT.                                                ELTPROST
00868 /                                                                 ELTPROST
00869  2150-RECERTIFICATION.                                            ELTPROST
00870 ****************************************************************  ELTPROST
00871 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTPROST
00872 ****************************************************************  ELTPROST
00873      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00874      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00875                          AND                                      ELTPROST
00876         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00877                                                  NOT =  ZERO      ELTPROST
00878          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00879          MOVE +2                    TO  WS-CIA                    ELTPROST
00880          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00881                                                                   ELTPROST
00882      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00883      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00884                           AND                                     ELTPROST
00885         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00886                                                 NOT  =  ZERO      ELTPROST
00887                           AND                                     ELTPROST
00888         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
00889          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00890          MOVE +2                    TO  WS-CIA                    ELTPROST
00891          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00892                                                                   ELTPROST
00893      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00894      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00895                           AND                                     ELTPROST
00896         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00897                                                  NOT  =  ZERO     ELTPROST
00898          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTPROST
00899          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
00900                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00901          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
00902                               TO  CMF-CODE-VALUE                  ELTPROST
00903          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
00904          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00905          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00906             THRU 9500-EXIT.                                       ELTPROST
00907                                                                   ELTPROST
00908      SET PLT-INDEX2  TO  2.                                       ELTPROST
00909      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00910                           AND                                     ELTPROST
00911         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00912                                                 NOT  =  ZERO      ELTPROST
00913          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTPROST
00914          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
00915                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00916          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
00917                               TO  CMF-CODE-VALUE                  ELTPROST
00918          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
00919          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00920          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00921             THRU 9500-EXIT.                                       ELTPROST
00922                                                                   ELTPROST
00923      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
00924         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
00925          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
00926             THRU 9200-EXIT.                                       ELTPROST
00927                                                                   ELTPROST
00928      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
00929         SET PLT-INDEX2  TO  2                                     ELTPROST
00930      ELSE                                                         ELTPROST
00931         SET PLT-INDEX2  TO  1.                                    ELTPROST
00932                                                                   ELTPROST
00933  2150-EXIT.  EXIT.                                                ELTPROST
00934 /                                                                 ELTPROST
00935  2155-REPR-REPL.                                                  ELTPROST
00936 ****************************************************************  ELTPROST
00937 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTPROST
00938 ****************************************************************  ELTPROST
00939      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00940      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00941                          AND                                      ELTPROST
00942         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00943                                                  NOT =  ZERO      ELTPROST
00944          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00945          MOVE +2                    TO  WS-CIA                    ELTPROST
00946          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00947                                                                   ELTPROST
00948      SET  PLT-INDEX2  TO  2.                                      ELTPROST
00949      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00950                           AND                                     ELTPROST
00951         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00952                                                 NOT  =  ZERO      ELTPROST
00953                           AND                                     ELTPROST
00954         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
00955          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
00956          MOVE +2                    TO  WS-CIA                    ELTPROST
00957          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
00958                                                                   ELTPROST
00959      SET  PLT-INDEX2  TO  1.                                      ELTPROST
00960      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
00961                           AND                                     ELTPROST
00962         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00963                                                  NOT  =  ZERO     ELTPROST
00964          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTPROST
00965          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
00966                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00967          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
00968                               TO  CMF-CODE-VALUE                  ELTPROST
00969          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
00970          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00971          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00972             THRU 9500-EXIT.                                       ELTPROST
00973                                                                   ELTPROST
00974      SET PLT-INDEX2  TO  2.                                       ELTPROST
00975      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
00976                           AND                                     ELTPROST
00977         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
00978                                                 NOT  =  ZERO      ELTPROST
00979          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTPROST
00980          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
00981                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
00982          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
00983                               TO  CMF-CODE-VALUE                  ELTPROST
00984          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
00985          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
00986          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
00987             THRU 9500-EXIT.                                       ELTPROST
00988                                                                   ELTPROST
00989      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
00990         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
00991          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
00992             THRU 9200-EXIT.                                       ELTPROST
00993                                                                   ELTPROST
00994      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
00995         SET PLT-INDEX2  TO  2                                     ELTPROST
00996      ELSE                                                         ELTPROST
00997         SET PLT-INDEX2  TO  1.                                    ELTPROST
00998                                                                   ELTPROST
00999  2155-EXIT.  EXIT.                                                ELTPROST
01000 /                                                                 ELTPROST
01001  2160-SPILLOVR-COINS-N-DEDUC.                                     ELTPROST
01002 ****************************************************************  ELTPROST
01003 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTPROST
01004 ****************************************************************  ELTPROST
01005      MOVE +1  TO  WS-CIA.                                         ELTPROST
01006                                                                   ELTPROST
01007      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01008      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
01009                            AND                                    ELTPROST
01010         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTPROST
01011                                                  NOT =  '0'       ELTPROST
01012         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01013         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPROST
01014                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
01015         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPROST
01016                                                TO  CMF-CODE-VALUE ELTPROST
01017         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTPROST
01018         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTPROST
01019         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
01020            THRU 9500-EXIT                                         ELTPROST
01021         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
01022            THRU 9200-EXIT.                                        ELTPROST
01023 ****************************************************************  ELTPROST
01024 *          S P I L L O V E R   D E D U C T I B L E             *  ELTPROST
01025 ****************************************************************  ELTPROST
01026      MOVE +1  TO  WS-CIA.                                         ELTPROST
01027                                                                   ELTPROST
01028      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01029      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
01030                            AND                                    ELTPROST
01031         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01032                                                  NOT =  '0'       ELTPROST
01033         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01034         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPROST
01035                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
01036         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTPROST
01037                                                 TO  CMF-CODE-VALUEELTPROST
01038         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTPROST
01039         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTPROST
01040         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
01041            THRU 9500-EXIT                                         ELTPROST
01042         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
01043            THRU 9200-EXIT.                                        ELTPROST
01044                                                                   ELTPROST
01045      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01046         SET PLT-INDEX2  TO  2                                     ELTPROST
01047      ELSE                                                         ELTPROST
01048         SET PLT-INDEX2  TO  1.                                    ELTPROST
01049                                                                   ELTPROST
01050  2160-EXIT.  EXIT.                                                ELTPROST
01051 /                                                                 ELTPROST
01052  2165-TRANS-OTHER-RESP-IND.                                       ELTPROST
01053      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01054         SET PLT-INDEX2  TO  2                                     ELTPROST
01055      ELSE                                                         ELTPROST
01056         SET PLT-INDEX2  TO  1.                                    ELTPROST
01057                                                                   ELTPROST
01058      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTPROST
01059                                                  NOT =  ZERO      ELTPROST
01060         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01061         MOVE 'TRANSF-OTHER-RESP-IND'   TO                         ELTPROST
01062                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
01063         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
01064                                                 TO  CMF-CODE-VALUEELTPROST
01065         MOVE SPACES            TO  WS-TEMP-TEXT-AREA              ELTPROST
01066         MOVE +0                TO  WS-TEMP-NOT-USED-CNT           ELTPROST
01067         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
01068            THRU 9500-EXIT                                         ELTPROST
01069         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
01070            THRU 9200-EXIT.                                        ELTPROST
01071                                                                   ELTPROST
01072  2165-EXIT.     EXIT.                                             ELTPROST
01073 /***************************************************************  ELTPROST
01074 *       PROSTHETIC INSTITUTIONAL OUTPATIENT PROCESSING         *  ELTPROST
01075 ****************************************************************  ELTPROST
01076  3000-INSTITUTIONAL-OP.                                           ELTPROST
01077                                                                   ELTPROST
01078      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTPROST
01079                                                                   ELTPROST
01080      MOVE HEADER-I-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTPROST
01081                                                                   ELTPROST
01082      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTPROST
01083         THRU 9100-EXIT.                                           ELTPROST
01084                                                                   ELTPROST
01085      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPROST
01086      PERFORM WITH TEST BEFORE                                     ELTPROST
01087              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPROST
01088              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPROST
01089         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPROST
01090         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPROST
01091         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPROST
01092         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPROST
01093      END-PERFORM.                                                 ELTPROST
01094      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPROST
01095                                                                   ELTPROST
01096                                                                   ELTPROST
01097      PERFORM 3010-MOVE-IN-INST-OP-TABS                            ELTPROST
01098         THRU 3010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
01099                        UNTIL   WS-SUB  >     WS-INST-OP-CNT.      ELTPROST
01100                                                                   ELTPROST
01101      PERFORM 3020-CALL-COVERAGE                                   ELTPROST
01102         THRU 3020-EXIT.                                           ELTPROST
01103                                                                   ELTPROST
01104      IF PVN-COVG-NONE                                             ELTPROST
01105          GO TO 3000-EXIT.                                         ELTPROST
01106                                                                   ELTPROST
01107      PERFORM 3030-FIND-FIRST-NONZERO                              ELTPROST
01108         THRU 3030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
01109                        UNTIL   WS-SUB  > WS-INST-OP-CNT.          ELTPROST
01110                                                                   ELTPROST
01111  3000-EXIT.  EXIT.                                                ELTPROST
01112 /                                                                 ELTPROST
01113  3010-MOVE-IN-INST-OP-TABS.                                       ELTPROST
01114      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01115      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTPROST
01116                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTPROST
01117                                                                   ELTPROST
01118      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTPROST
01119                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTPROST
01120                                                                   ELTPROST
01121  3010-EXIT.  EXIT.                                                ELTPROST
01122 /                                                                 ELTPROST
01123  3020-CALL-COVERAGE.                                              ELTPROST
01124                                                                   ELTPROST
01125      MOVE 'PROSTHETIC APPLIANCES      ' TO SSB-TOPIC-PHRASE.      ELTPROST
01126                                                                   ELTPROST
01127      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTPROST
01128                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01129                     END-EXEC.                                     ELTPROST
01130                                                                   ELTPROST
01131      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
01132                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01133                     END-EXEC.                                     ELTPROST
01134                                                                   ELTPROST
01135      IF PVN-COVG-NONE                                             ELTPROST
01136          GO TO 3020-EXIT.                                         ELTPROST
01137                                                                   ELTPROST
01138      MOVE +1  TO  WS-CIA.                                         ELTPROST
01139                                                                   ELTPROST
01140      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPROST
01141                    PSP-PROVN-PRICING-METHD,                       ELTPROST
01142                    PSP-TRANSF-OTHER-RESP-IND,                     ELTPROST
01143                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTPROST
01144                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTPROST
01145                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTPROST
01146                    PSP-SPILL-OVER-DED-APL-IND,                    ELTPROST
01147                    PSP-CERTFN-REQRM-IND,                          ELTPROST
01148                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTPROST
01149                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTPROST
01150                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTPROST
01151                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTPROST
01152                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTPROST
01153                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTPROST
01154                    PSB-CERTN-REPETN-REQRD-IND,                    ELTPROST
01155                    PSB-REPR-REPLAC-RESTRN-IND.                    ELTPROST
01156                                                                   ELTPROST
01157      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTPROST
01158                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01159                     END-EXEC.                                     ELTPROST
01160                                                                   ELTPROST
01161      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPROST
01162      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
01163          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTPROST
01164                                                                   ELTPROST
01165  3020-EXIT.  EXIT.                                                ELTPROST
01166 /                                                                 ELTPROST
01167  3030-FIND-FIRST-NONZERO.                                         ELTPROST
01168      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01169                                                                   ELTPROST
01170      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTPROST
01171          CONTINUE                                                 ELTPROST
01172      ELSE                                                         ELTPROST
01173          PERFORM 3100-BUILD-SCREEN-LINES                          ELTPROST
01174             THRU 3100-EXIT.                                       ELTPROST
01175                                                                   ELTPROST
01176  3030-EXIT.  EXIT.                                                ELTPROST
01177 /                                                                 ELTPROST
01178  3100-BUILD-SCREEN-LINES.                                         ELTPROST
01179      SET PLT-INDEX1  TO                                           ELTPROST
01180              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTPROST
01181                                                                   ELTPROST
01182      IF WS-NOT-FIRST-TIME                                         ELTPROST
01183         SET COF-NEW-PAGE TO TRUE                                  ELTPROST
01184         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTPROST
01185         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTPROST
01186                   COMMAREA (DFHCOMMAREA)                          ELTPROST
01187         END-EXEC                                                  ELTPROST
01188      ELSE                                                         ELTPROST
01189         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTPROST
01190                                                                   ELTPROST
01191      MOVE  +1  TO  WS-CIA.                                        ELTPROST
01192                                                                   ELTPROST
01193      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTPROST
01194          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTPROST
01195              SET PLT-INDEX2  TO  2                                ELTPROST
01196          ELSE                                                     ELTPROST
01197              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTPROST
01198              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTPROST
01199                 THRU 9200-EXIT                                    ELTPROST
01200              GO TO 3100-EXIT                                      ELTPROST
01201      ELSE                                                         ELTPROST
01202          SET PLT-INDEX2  TO  1.                                   ELTPROST
01203                                                                   ELTPROST
01204      PERFORM 3105-LIST-BEN-PROV                                   ELTPROST
01205         THRU 3105-EXIT.                                           ELTPROST
01206                                                                   ELTPROST
01207      PERFORM 3110-PLACE-OF-TREATMENT                              ELTPROST
01208         THRU 3110-EXIT.                                           ELTPROST
01209                                                                   ELTPROST
01210      PERFORM 3120-PRIC-METH                                       ELTPROST
01211         THRU 3120-EXIT.                                           ELTPROST
01212                                                                   ELTPROST
01213      PERFORM 3140-CERTIFICATION                                   ELTPROST
01214         THRU 3140-EXIT.                                           ELTPROST
01215                                                                   ELTPROST
01216      PERFORM 3150-RECERTIFICATION                                 ELTPROST
01217         THRU 3150-EXIT.                                           ELTPROST
01218                                                                   ELTPROST
01219      PERFORM 3155-REPR-REPL                                       ELTPROST
01220         THRU 3155-EXIT.                                           ELTPROST
01221                                                                   ELTPROST
01222      PERFORM 3160-SPILLOVR-COINS-N-DEDUC                          ELTPROST
01223         THRU 3160-EXIT.                                           ELTPROST
01224                                                                   ELTPROST
01225      PERFORM 2165-TRANS-OTHER-RESP-IND                            ELTPROST
01226         THRU 2165-EXIT.                                           ELTPROST
01227                                                                   ELTPROST
01228      PERFORM 7000-ALL-LEVEL-TABS                                  ELTPROST
01229         THRU 7000-EXIT.                                           ELTPROST
01230                                                                   ELTPROST
01231  3100-EXIT.  EXIT.                                                ELTPROST
01232 /                                                                 ELTPROST
01233  3105-LIST-BEN-PROV.                                              ELTPROST
01234 ****************************************************************  ELTPROST
01235 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTPROST
01236 ****************************************************************  ELTPROST
01237      MOVE  +2               TO  WS-CIA.                           ELTPROST
01238      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTPROST
01239      MOVE ZERO              TO  WS-SUB2.                          ELTPROST
01240      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTPROST
01241                             TO  WS-SUB3.                          ELTPROST
01242                                                                   ELTPROST
01243      PERFORM 3106-ZERO-ALL-WITH-SAME-NO                           ELTPROST
01244         THRU 3106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTPROST
01245                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.ELTPROST
01246                                                                   ELTPROST
01247      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01248      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTPROST
01249                                                                   ELTPROST
01250      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
01251                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01252                     END-EXEC.                                     ELTPROST
01253                                                                   ELTPROST
01254  3105-EXIT.  EXIT.                                                ELTPROST
01255      SKIP3                                                        ELTPROST
01256  3106-ZERO-ALL-WITH-SAME-NO.                                      ELTPROST
01257                                                                   ELTPROST
01258      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTPROST
01259          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTPROST
01260          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTPROST
01261          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTPROST
01262                            TO  CMF-CODE-VALUE                     ELTPROST
01263          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTPROST
01264          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTPROST
01265          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01266             THRU 9500-EXIT                                        ELTPROST
01267          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTPROST
01268          ADD  +1    TO  WS-SUB2                                   ELTPROST
01269          IF WS-CIA  >  20  OR  =  20                              ELTPROST
01270              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTPROST
01271              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTPROST
01272                             COMMAREA (DFHCOMMAREA)                ELTPROST
01273                             END-EXEC                              ELTPROST
01274              MOVE  +1  TO  WS-CIA.                                ELTPROST
01275                                                                   ELTPROST
01276  3106-EXIT.  EXIT.                                                ELTPROST
01277 /                                                                 ELTPROST
01278  3110-PLACE-OF-TREATMENT.                                         ELTPROST
01279 ****************************************************************  ELTPROST
01280 *              P L A C E   O F   T R E A T M E N T             *  ELTPROST
01281 ****************************************************************  ELTPROST
01282      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01283      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01284                          AND                                      ELTPROST
01285         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01286                                                  NOT =  ZERO      ELTPROST
01287          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01288          MOVE +2                    TO  WS-CIA                    ELTPROST
01289          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01290                                                                   ELTPROST
01291      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01292      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01293                           AND                                     ELTPROST
01294         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01295                                                 NOT  =  ZERO      ELTPROST
01296                           AND                                     ELTPROST
01297         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
01298          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01299          MOVE +2                    TO  WS-CIA                    ELTPROST
01300          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01301                                                                   ELTPROST
01302      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01303      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01304                           AND                                     ELTPROST
01305         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01306                                                  NOT  =  ZERO     ELTPROST
01307          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
01308          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
01309                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01310          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
01311                               TO  CMF-CODE-VALUE                  ELTPROST
01312          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
01313          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01314          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01315             THRU 9500-EXIT.                                       ELTPROST
01316                                                                   ELTPROST
01317      SET PLT-INDEX2  TO  2.                                       ELTPROST
01318      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01319                           AND                                     ELTPROST
01320         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01321                                                 NOT  =  ZERO      ELTPROST
01322          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
01323          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
01324                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01325          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
01326                               TO  CMF-CODE-VALUE                  ELTPROST
01327          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01328          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01329          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01330             THRU 9500-EXIT.                                       ELTPROST
01331                                                                   ELTPROST
01332      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01333         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
01334          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01335             THRU 9200-EXIT.                                       ELTPROST
01336                                                                   ELTPROST
01337      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01338         SET PLT-INDEX2  TO  2                                     ELTPROST
01339      ELSE                                                         ELTPROST
01340         SET PLT-INDEX2  TO  1.                                    ELTPROST
01341                                                                   ELTPROST
01342  3110-EXIT.  EXIT.                                                ELTPROST
01343 /                                                                 ELTPROST
01344  3120-PRIC-METH.                                                  ELTPROST
01345 ****************************************************************  ELTPROST
01346 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTPROST
01347 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTPROST
01348 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTPROST
01349 ****************************************************************  ELTPROST
01350      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01351      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
01352         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
01353                                                              '19' ELTPROST
01354         MOVE +2             TO  WS-CIA                            ELTPROST
01355         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
01356         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
01357                                                                   ELTPROST
01358      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01359      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
01360         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
01361                                                        '19' AND   ELTPROST
01362         NOT WS-ADD-A-BLANK-LINE                                   ELTPROST
01363         MOVE +2             TO  WS-CIA                            ELTPROST
01364         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
01365         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
01366                                                                   ELTPROST
01367      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01368      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
01369         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
01370                            AND                                    ELTPROST
01371         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01372         SET  PLT-INDEX2  TO  2                                    ELTPROST
01373         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
01374                                                             ZERO  ELTPROST
01375            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
01376            ADD +1  TO  WS-CIA                                     ELTPROST
01377            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
01378                                                                   ELTPROST
01379      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01380      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
01381         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
01382                            AND                                    ELTPROST
01383         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPROST
01384         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPROST
01385         ADD +1  TO  WS-CIA                                        ELTPROST
01386         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTPROST
01387                                                                   ELTPROST
01388      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPROST
01389         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01390         SET  PLT-INDEX2  TO  2                                    ELTPROST
01391         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
01392                                                             ZERO  ELTPROST
01393            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
01394            ADD +1  TO  WS-CIA                                     ELTPROST
01395            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
01396                                                                   ELTPROST
01397      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01398      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01399         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPROST
01400                                                            =  ZEROELTPROST
01401            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01402                                                            =  ZEROELTPROST
01403               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
01404            ELSE                                                   ELTPROST
01405               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
01406          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01407                                                  TO  WS-PERCENTAGEELTPROST
01408         ELSE                                                      ELTPROST
01409            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
01410          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01411                                                 TO  WS-PERCENTAGE.ELTPROST
01412      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
01413         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
01414                                             ZERO AND  NOT =  '19' ELTPROST
01415         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01416         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
01417         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
01418                                                    CMF-CODE-VALUE ELTPROST
01419         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01420         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
01421         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
01422            THRU 9600-EXIT.                                        ELTPROST
01423                                                                   ELTPROST
01424      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01425      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01426         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPROST
01427                                                               ZEROELTPROST
01428            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01429                                                            =  ZEROELTPROST
01430               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
01431            ELSE                                                   ELTPROST
01432               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
01433          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01434                                                  TO  WS-PERCENTAGEELTPROST
01435         ELSE                                                      ELTPROST
01436            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
01437          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
01438                                                 TO  WS-PERCENTAGE.ELTPROST
01439      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
01440         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
01441                                             ZERO AND  NOT =  '19' ELTPROST
01442         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01443         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
01444         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
01445                                                    CMF-CODE-VALUE ELTPROST
01446         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPROST
01447         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
01448         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
01449            THRU 9600-EXIT.                                        ELTPROST
01450                                                                   ELTPROST
01451      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01452          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTPROST
01453          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01454             THRU 9200-EXIT.                                       ELTPROST
01455                                                                   ELTPROST
01456      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01457         SET PLT-INDEX2  TO  2                                     ELTPROST
01458      ELSE                                                         ELTPROST
01459         SET PLT-INDEX2  TO  1.                                    ELTPROST
01460                                                                   ELTPROST
01461  3120-EXIT.  EXIT.                                                ELTPROST
01462 /                                                                 ELTPROST
01463  3140-CERTIFICATION.                                              ELTPROST
01464 ****************************************************************  ELTPROST
01465 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTPROST
01466 ****************************************************************  ELTPROST
01467      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01468      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01469                          AND                                      ELTPROST
01470         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
01471                                                  NOT =  '00'      ELTPROST
01472          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01473          MOVE +2                    TO  WS-CIA                    ELTPROST
01474          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01475                                                                   ELTPROST
01476      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01477      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01478                           AND                                     ELTPROST
01479         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
01480                                                 NOT  =  '00'      ELTPROST
01481                           AND                                     ELTPROST
01482         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
01483          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01484          MOVE +2                    TO  WS-CIA                    ELTPROST
01485          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01486                                                                   ELTPROST
01487      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01488      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01489                           AND                                     ELTPROST
01490         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
01491                                                  NOT  =  '00'     ELTPROST
01492          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTPROST
01493          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
01494                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01495          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01496                               TO  CMF-CODE-VALUE                  ELTPROST
01497          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
01498          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01499          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01500             THRU 9500-EXIT.                                       ELTPROST
01501                                                                   ELTPROST
01502      SET PLT-INDEX2  TO  2.                                       ELTPROST
01503      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01504                           AND                                     ELTPROST
01505         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
01506                                                 NOT  =  '00'      ELTPROST
01507          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTPROST
01508          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
01509                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01510          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01511                               TO  CMF-CODE-VALUE                  ELTPROST
01512          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01513          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01514          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01515             THRU 9500-EXIT.                                       ELTPROST
01516                                                                   ELTPROST
01517      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01518         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
01519          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01520             THRU 9200-EXIT.                                       ELTPROST
01521                                                                   ELTPROST
01522      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01523         SET PLT-INDEX2  TO  2                                     ELTPROST
01524      ELSE                                                         ELTPROST
01525         SET PLT-INDEX2  TO  1.                                    ELTPROST
01526                                                                   ELTPROST
01527  3140-EXIT.  EXIT.                                                ELTPROST
01528 /                                                                 ELTPROST
01529  3150-RECERTIFICATION.                                            ELTPROST
01530 ****************************************************************  ELTPROST
01531 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTPROST
01532 ****************************************************************  ELTPROST
01533      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01534      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01535                          AND                                      ELTPROST
01536         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01537                                                  NOT =  ZERO      ELTPROST
01538          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01539          MOVE +2                    TO  WS-CIA                    ELTPROST
01540          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01541                                                                   ELTPROST
01542      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01543      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01544                           AND                                     ELTPROST
01545         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01546                                                 NOT  =  ZERO      ELTPROST
01547                           AND                                     ELTPROST
01548         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
01549          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01550          MOVE +2                    TO  WS-CIA                    ELTPROST
01551          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01552                                                                   ELTPROST
01553      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01554      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01555                           AND                                     ELTPROST
01556         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01557                                                  NOT  =  ZERO     ELTPROST
01558          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTPROST
01559          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
01560                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01561          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
01562                               TO  CMF-CODE-VALUE                  ELTPROST
01563          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
01564          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01565          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01566             THRU 9500-EXIT.                                       ELTPROST
01567                                                                   ELTPROST
01568      SET PLT-INDEX2  TO  2.                                       ELTPROST
01569      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01570                           AND                                     ELTPROST
01571         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01572                                                 NOT  =  ZERO      ELTPROST
01573          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTPROST
01574          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
01575                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01576          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
01577                               TO  CMF-CODE-VALUE                  ELTPROST
01578          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01579          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01580          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01581             THRU 9500-EXIT.                                       ELTPROST
01582                                                                   ELTPROST
01583      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01584         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
01585          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01586             THRU 9200-EXIT.                                       ELTPROST
01587                                                                   ELTPROST
01588      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01589         SET PLT-INDEX2  TO  2                                     ELTPROST
01590      ELSE                                                         ELTPROST
01591         SET PLT-INDEX2  TO  1.                                    ELTPROST
01592                                                                   ELTPROST
01593  3150-EXIT.  EXIT.                                                ELTPROST
01594 /                                                                 ELTPROST
01595  3155-REPR-REPL.                                                  ELTPROST
01596 ****************************************************************  ELTPROST
01597 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTPROST
01598 ****************************************************************  ELTPROST
01599      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01600      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01601                          AND                                      ELTPROST
01602         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01603                                                  NOT =  ZERO      ELTPROST
01604          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01605          MOVE +2                    TO  WS-CIA                    ELTPROST
01606          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01607                                                                   ELTPROST
01608      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01609      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01610                           AND                                     ELTPROST
01611         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01612                                                 NOT  =  ZERO      ELTPROST
01613                           AND                                     ELTPROST
01614         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
01615          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01616          MOVE +2                    TO  WS-CIA                    ELTPROST
01617          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01618                                                                   ELTPROST
01619      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01620      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01621                           AND                                     ELTPROST
01622         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01623                                                  NOT  =  ZERO     ELTPROST
01624          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTPROST
01625          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
01626                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01627          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
01628                               TO  CMF-CODE-VALUE                  ELTPROST
01629          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
01630          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01631          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01632             THRU 9500-EXIT.                                       ELTPROST
01633                                                                   ELTPROST
01634      SET PLT-INDEX2  TO  2.                                       ELTPROST
01635      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01636                           AND                                     ELTPROST
01637         PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01638                                                 NOT  =  ZERO      ELTPROST
01639          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTPROST
01640          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
01641                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01642          MOVE PLB-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
01643                               TO  CMF-CODE-VALUE                  ELTPROST
01644          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01645          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01646          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01647             THRU 9500-EXIT.                                       ELTPROST
01648                                                                   ELTPROST
01649      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01650         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
01651          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01652             THRU 9200-EXIT.                                       ELTPROST
01653                                                                   ELTPROST
01654      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01655         SET PLT-INDEX2  TO  2                                     ELTPROST
01656      ELSE                                                         ELTPROST
01657         SET PLT-INDEX2  TO  1.                                    ELTPROST
01658                                                                   ELTPROST
01659  3155-EXIT.  EXIT.                                                ELTPROST
01660 /                                                                 ELTPROST
01661  3160-SPILLOVR-COINS-N-DEDUC.                                     ELTPROST
01662 ****************************************************************  ELTPROST
01663 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTPROST
01664 ****************************************************************  ELTPROST
01665      MOVE +1  TO  WS-CIA.                                         ELTPROST
01666                                                                   ELTPROST
01667      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01668      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
01669                            AND                                    ELTPROST
01670         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTPROST
01671                                                  NOT =  '0'       ELTPROST
01672         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01673         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPROST
01674                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
01675         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPROST
01676                                                TO  CMF-CODE-VALUE ELTPROST
01677         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTPROST
01678         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTPROST
01679         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
01680            THRU 9500-EXIT                                         ELTPROST
01681         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
01682            THRU 9200-EXIT.                                        ELTPROST
01683 ****************************************************************  ELTPROST
01684 *          S P I L L O V E R   D E D U C T I B L E             *  ELTPROST
01685 ****************************************************************  ELTPROST
01686      MOVE +1  TO  WS-CIA.                                         ELTPROST
01687                                                                   ELTPROST
01688      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01689      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
01690                            AND                                    ELTPROST
01691         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
01692                                                  NOT =  '0'       ELTPROST
01693         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
01694         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPROST
01695                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
01696         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTPROST
01697                                                 TO  CMF-CODE-VALUEELTPROST
01698         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTPROST
01699         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTPROST
01700         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
01701            THRU 9500-EXIT                                         ELTPROST
01702         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
01703            THRU 9200-EXIT.                                        ELTPROST
01704                                                                   ELTPROST
01705      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01706         SET PLT-INDEX2  TO  2                                     ELTPROST
01707      ELSE                                                         ELTPROST
01708         SET PLT-INDEX2  TO  1.                                    ELTPROST
01709                                                                   ELTPROST
01710  3160-EXIT.  EXIT.                                                ELTPROST
01711 /                                                                 ELTPROST
01712  4000-PROFESSIONAL-IP.                                            ELTPROST
01713 ****************************************************************  ELTPROST
01714 *       PROSTHETIC PROFESSIONAL INPATIENT PROCESSING           *  ELTPROST
01715 ****************************************************************  ELTPROST
01716                                                                   ELTPROST
01717      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTPROST
01718                                                                   ELTPROST
01719      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTPROST
01720                                                                   ELTPROST
01721      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTPROST
01722         THRU 9100-EXIT.                                           ELTPROST
01723                                                                   ELTPROST
01724      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPROST
01725      PERFORM WITH TEST BEFORE                                     ELTPROST
01726              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPROST
01727              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPROST
01728         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPROST
01729         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPROST
01730         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPROST
01731         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPROST
01732      END-PERFORM.                                                 ELTPROST
01733      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPROST
01734                                                                   ELTPROST
01735                                                                   ELTPROST
01736      PERFORM 4010-MOVE-IN-PROF-IP-TABS                            ELTPROST
01737         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
01738                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTPROST
01739                                                                   ELTPROST
01740      PERFORM 4020-CALL-COVERAGE                                   ELTPROST
01741         THRU 4020-EXIT.                                           ELTPROST
01742                                                                   ELTPROST
01743      IF PVN-COVG-NONE                                             ELTPROST
01744          GO TO 4000-EXIT.                                         ELTPROST
01745                                                                   ELTPROST
01746      PERFORM 4030-FIND-FIRST-NONZERO                              ELTPROST
01747         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
01748                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTPROST
01749                                                                   ELTPROST
01750  4000-EXIT.  EXIT.                                                ELTPROST
01751 /                                                                 ELTPROST
01752  4010-MOVE-IN-PROF-IP-TABS.                                       ELTPROST
01753      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01754      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTPROST
01755                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTPROST
01756                                                                   ELTPROST
01757      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTPROST
01758                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTPROST
01759                                                                   ELTPROST
01760  4010-EXIT.  EXIT.                                                ELTPROST
01761      SKIP3                                                        ELTPROST
01762  4020-CALL-COVERAGE.                                              ELTPROST
01763                                                                   ELTPROST
01764      MOVE 'PROSTEHETIC APPLIANCES      ' TO   SSB-TOPIC-PHRASE.   ELTPROST
01765                                                                   ELTPROST
01766      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTPROST
01767                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01768                     END-EXEC.                                     ELTPROST
01769                                                                   ELTPROST
01770      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
01771                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01772                     END-EXEC.                                     ELTPROST
01773                                                                   ELTPROST
01774      IF PVN-COVG-NONE                                             ELTPROST
01775          GO TO 4020-EXIT.                                         ELTPROST
01776                                                                   ELTPROST
01777      MOVE +1  TO  WS-CIA.                                         ELTPROST
01778                                                                   ELTPROST
01779      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPROST
01780                    PSP-PROVN-PRICING-METHD,                       ELTPROST
01781                    PSP-TRANSF-OTHER-RESP-IND,                     ELTPROST
01782                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTPROST
01783                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTPROST
01784                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTPROST
01785                    PSP-SPILL-OVER-DED-APL-IND,                    ELTPROST
01786                    PSP-CERTFN-REQRM-IND,                          ELTPROST
01787                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTPROST
01788                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTPROST
01789                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTPROST
01790                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTPROST
01791                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTPROST
01792                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTPROST
01793                    PSE-CERTN-REPETN-REQRD-IND,                    ELTPROST
01794                    PSE-REPR-REPLAC-RESTRN-IND.                    ELTPROST
01795                                                                   ELTPROST
01796      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTPROST
01797                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01798                     END-EXEC.                                     ELTPROST
01799                                                                   ELTPROST
01800      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPROST
01801      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
01802          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTPROST
01803                                                                   ELTPROST
01804  4020-EXIT.  EXIT.                                                ELTPROST
01805 /                                                                 ELTPROST
01806  4030-FIND-FIRST-NONZERO.                                         ELTPROST
01807      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01808                                                                   ELTPROST
01809      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTPROST
01810          CONTINUE                                                 ELTPROST
01811      ELSE                                                         ELTPROST
01812          PERFORM 4100-BUILD-SCREEN-LINES                          ELTPROST
01813             THRU 4100-EXIT.                                       ELTPROST
01814                                                                   ELTPROST
01815  4030-EXIT.  EXIT.                                                ELTPROST
01816 /                                                                 ELTPROST
01817  4100-BUILD-SCREEN-LINES.                                         ELTPROST
01818      SET PLT-INDEX1  TO                                           ELTPROST
01819              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTPROST
01820                                                                   ELTPROST
01821      IF WS-NOT-FIRST-TIME                                         ELTPROST
01822         SET COF-NEW-PAGE TO TRUE                                  ELTPROST
01823         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTPROST
01824         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTPROST
01825                   COMMAREA (DFHCOMMAREA)                          ELTPROST
01826         END-EXEC                                                  ELTPROST
01827      ELSE                                                         ELTPROST
01828         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTPROST
01829                                                                   ELTPROST
01830      MOVE  +1  TO  WS-CIA.                                        ELTPROST
01831                                                                   ELTPROST
01832      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTPROST
01833          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTPROST
01834              SET PLT-INDEX2  TO  2                                ELTPROST
01835          ELSE                                                     ELTPROST
01836              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTPROST
01837              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTPROST
01838                 THRU 9200-EXIT                                    ELTPROST
01839              GO TO 4100-EXIT                                      ELTPROST
01840      ELSE                                                         ELTPROST
01841          SET PLT-INDEX2  TO  1.                                   ELTPROST
01842                                                                   ELTPROST
01843      PERFORM 4105-LIST-BEN-PROV                                   ELTPROST
01844         THRU 4105-EXIT.                                           ELTPROST
01845                                                                   ELTPROST
01846      PERFORM 4110-PLACE-OF-TREATMENT                              ELTPROST
01847         THRU 4110-EXIT.                                           ELTPROST
01848                                                                   ELTPROST
01849      PERFORM 4120-PRIC-METH                                       ELTPROST
01850         THRU 4120-EXIT.                                           ELTPROST
01851                                                                   ELTPROST
01852      PERFORM 4140-CERTIFICATION                                   ELTPROST
01853         THRU 4140-EXIT.                                           ELTPROST
01854                                                                   ELTPROST
01855      PERFORM 4150-RECERTIFICATION                                 ELTPROST
01856         THRU 4150-EXIT.                                           ELTPROST
01857                                                                   ELTPROST
01858      PERFORM 4155-REPR-REPL                                       ELTPROST
01859         THRU 4155-EXIT.                                           ELTPROST
01860                                                                   ELTPROST
01861      PERFORM 4160-SPILLOVR-COINS-N-DEDUC                          ELTPROST
01862         THRU 4160-EXIT.                                           ELTPROST
01863                                                                   ELTPROST
01864      PERFORM 2165-TRANS-OTHER-RESP-IND                            ELTPROST
01865         THRU 2165-EXIT.                                           ELTPROST
01866                                                                   ELTPROST
01867      PERFORM 7000-ALL-LEVEL-TABS                                  ELTPROST
01868         THRU 7000-EXIT.                                           ELTPROST
01869                                                                   ELTPROST
01870  4100-EXIT.  EXIT.                                                ELTPROST
01871 /                                                                 ELTPROST
01872  4105-LIST-BEN-PROV.                                              ELTPROST
01873 ****************************************************************  ELTPROST
01874 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTPROST
01875 ****************************************************************  ELTPROST
01876      MOVE  +2               TO  WS-CIA.                           ELTPROST
01877      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTPROST
01878      MOVE ZERO              TO  WS-SUB2.                          ELTPROST
01879      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTPROST
01880                             TO  WS-SUB3.                          ELTPROST
01881                                                                   ELTPROST
01882      PERFORM 4106-ZERO-ALL-WITH-SAME-NO                           ELTPROST
01883         THRU 4106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTPROST
01884                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.ELTPROST
01885                                                                   ELTPROST
01886      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
01887      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTPROST
01888                                                                   ELTPROST
01889      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
01890                     COMMAREA (DFHCOMMAREA)                        ELTPROST
01891                     END-EXEC.                                     ELTPROST
01892                                                                   ELTPROST
01893  4105-EXIT.  EXIT.                                                ELTPROST
01894      SKIP3                                                        ELTPROST
01895  4106-ZERO-ALL-WITH-SAME-NO.                                      ELTPROST
01896                                                                   ELTPROST
01897      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTPROST
01898          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTPROST
01899          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTPROST
01900          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTPROST
01901                            TO  CMF-CODE-VALUE                     ELTPROST
01902          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTPROST
01903          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTPROST
01904          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01905             THRU 9500-EXIT                                        ELTPROST
01906          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTPROST
01907          ADD  +1    TO  WS-SUB2                                   ELTPROST
01908          IF WS-CIA  >  20  OR  =  20                              ELTPROST
01909              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTPROST
01910              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTPROST
01911                             COMMAREA (DFHCOMMAREA)                ELTPROST
01912                             END-EXEC                              ELTPROST
01913              MOVE  +1  TO  WS-CIA.                                ELTPROST
01914                                                                   ELTPROST
01915  4106-EXIT.  EXIT.                                                ELTPROST
01916 /                                                                 ELTPROST
01917  4110-PLACE-OF-TREATMENT.                                         ELTPROST
01918 ****************************************************************  ELTPROST
01919 *              P L A C E   O F   T R E A T M E N T             *  ELTPROST
01920 ****************************************************************  ELTPROST
01921      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01922      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01923                          AND                                      ELTPROST
01924         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01925                                                  NOT =  ZERO      ELTPROST
01926          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01927          MOVE +2                    TO  WS-CIA                    ELTPROST
01928          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01929                                                                   ELTPROST
01930      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01931      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01932                           AND                                     ELTPROST
01933         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01934                                                 NOT  =  ZERO      ELTPROST
01935                           AND                                     ELTPROST
01936         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
01937          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
01938          MOVE +2                    TO  WS-CIA                    ELTPROST
01939          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
01940                                                                   ELTPROST
01941      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01942      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
01943                           AND                                     ELTPROST
01944         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01945                                                  NOT  =  ZERO     ELTPROST
01946          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
01947          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
01948                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01949          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
01950                               TO  CMF-CODE-VALUE                  ELTPROST
01951          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
01952          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01953          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01954             THRU 9500-EXIT.                                       ELTPROST
01955                                                                   ELTPROST
01956      SET PLT-INDEX2  TO  2.                                       ELTPROST
01957      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
01958                           AND                                     ELTPROST
01959         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
01960                                                 NOT  =  ZERO      ELTPROST
01961          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
01962          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
01963                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
01964          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
01965                               TO  CMF-CODE-VALUE                  ELTPROST
01966          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
01967          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
01968          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
01969             THRU 9500-EXIT.                                       ELTPROST
01970                                                                   ELTPROST
01971      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
01972         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
01973          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
01974             THRU 9200-EXIT.                                       ELTPROST
01975                                                                   ELTPROST
01976      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
01977         SET PLT-INDEX2  TO  2                                     ELTPROST
01978      ELSE                                                         ELTPROST
01979         SET PLT-INDEX2  TO  1.                                    ELTPROST
01980                                                                   ELTPROST
01981  4110-EXIT.  EXIT.                                                ELTPROST
01982 /                                                                 ELTPROST
01983  4120-PRIC-METH.                                                  ELTPROST
01984 ****************************************************************  ELTPROST
01985 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTPROST
01986 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTPROST
01987 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTPROST
01988 ****************************************************************  ELTPROST
01989      SET  PLT-INDEX2  TO  1.                                      ELTPROST
01990      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
01991         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
01992                                                              '19' ELTPROST
01993         MOVE +2             TO  WS-CIA                            ELTPROST
01994         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
01995         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
01996                                                                   ELTPROST
01997      SET  PLT-INDEX2  TO  2.                                      ELTPROST
01998      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
01999         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02000                                                        '19' AND   ELTPROST
02001         NOT WS-ADD-A-BLANK-LINE                                   ELTPROST
02002         MOVE +2             TO  WS-CIA                            ELTPROST
02003         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
02004         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
02005                                                                   ELTPROST
02006      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02007      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02008         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
02009                            AND                                    ELTPROST
02010         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02011         SET  PLT-INDEX2  TO  2                                    ELTPROST
02012         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
02013                                                             ZERO  ELTPROST
02014            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
02015            ADD +1  TO  WS-CIA                                     ELTPROST
02016            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
02017                                                                   ELTPROST
02018      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02019      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02020         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
02021                            AND                                    ELTPROST
02022         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPROST
02023         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPROST
02024         ADD +1  TO  WS-CIA                                        ELTPROST
02025         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTPROST
02026                                                                   ELTPROST
02027      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPROST
02028         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02029         SET  PLT-INDEX2  TO  2                                    ELTPROST
02030         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
02031                                                             ZERO  ELTPROST
02032            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
02033            ADD +1  TO  WS-CIA                                     ELTPROST
02034            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
02035                                                                   ELTPROST
02036      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02037      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02038         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPROST
02039                                                            =  ZEROELTPROST
02040            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02041                                                            =  ZEROELTPROST
02042               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
02043            ELSE                                                   ELTPROST
02044               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
02045          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02046                                                  TO  WS-PERCENTAGEELTPROST
02047         ELSE                                                      ELTPROST
02048            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
02049          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02050                                                 TO  WS-PERCENTAGE.ELTPROST
02051      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02052         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02053                                             ZERO AND  NOT =  '19' ELTPROST
02054         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02055         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
02056         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
02057                                                    CMF-CODE-VALUE ELTPROST
02058         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02059         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
02060         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
02061            THRU 9600-EXIT.                                        ELTPROST
02062                                                                   ELTPROST
02063      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02064      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02065         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPROST
02066                                                               ZEROELTPROST
02067            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02068                                                            =  ZEROELTPROST
02069               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
02070            ELSE                                                   ELTPROST
02071               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
02072          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02073                                                  TO  WS-PERCENTAGEELTPROST
02074         ELSE                                                      ELTPROST
02075            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
02076          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02077                                                 TO  WS-PERCENTAGE.ELTPROST
02078      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
02079         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02080                                             ZERO AND  NOT =  '19' ELTPROST
02081         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02082         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
02083         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
02084                                                    CMF-CODE-VALUE ELTPROST
02085         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPROST
02086         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
02087         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
02088            THRU 9600-EXIT.                                        ELTPROST
02089                                                                   ELTPROST
02090      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02091          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTPROST
02092          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02093             THRU 9200-EXIT.                                       ELTPROST
02094                                                                   ELTPROST
02095      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02096         SET PLT-INDEX2  TO  2                                     ELTPROST
02097      ELSE                                                         ELTPROST
02098         SET PLT-INDEX2  TO  1.                                    ELTPROST
02099                                                                   ELTPROST
02100  4120-EXIT.  EXIT.                                                ELTPROST
02101 /                                                                 ELTPROST
02102  4140-CERTIFICATION.                                              ELTPROST
02103 ****************************************************************  ELTPROST
02104 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTPROST
02105 ****************************************************************  ELTPROST
02106      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02107      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02108                          AND                                      ELTPROST
02109         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02110                                                  NOT =  '00'      ELTPROST
02111          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02112          MOVE +2                    TO  WS-CIA                    ELTPROST
02113          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02114                                                                   ELTPROST
02115      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02116      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02117                           AND                                     ELTPROST
02118         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02119                                                 NOT  =  '00'      ELTPROST
02120                           AND                                     ELTPROST
02121         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02122          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02123          MOVE +2                    TO  WS-CIA                    ELTPROST
02124          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02125                                                                   ELTPROST
02126      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02127      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02128                           AND                                     ELTPROST
02129         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02130                                                  NOT  =  '00'     ELTPROST
02131          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTPROST
02132          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
02133                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02134          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02135                               TO  CMF-CODE-VALUE                  ELTPROST
02136          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02137          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02138          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02139             THRU 9500-EXIT.                                       ELTPROST
02140                                                                   ELTPROST
02141      SET PLT-INDEX2  TO  2.                                       ELTPROST
02142      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02143                           AND                                     ELTPROST
02144         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02145                                                 NOT  =  '00'      ELTPROST
02146          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTPROST
02147          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
02148                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02149          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02150                               TO  CMF-CODE-VALUE                  ELTPROST
02151          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02152          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02153          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02154             THRU 9500-EXIT.                                       ELTPROST
02155                                                                   ELTPROST
02156      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02157         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02158          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02159             THRU 9200-EXIT.                                       ELTPROST
02160                                                                   ELTPROST
02161      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02162         SET PLT-INDEX2  TO  2                                     ELTPROST
02163      ELSE                                                         ELTPROST
02164         SET PLT-INDEX2  TO  1.                                    ELTPROST
02165                                                                   ELTPROST
02166  4140-EXIT.  EXIT.                                                ELTPROST
02167 /                                                                 ELTPROST
02168  4150-RECERTIFICATION.                                            ELTPROST
02169 ****************************************************************  ELTPROST
02170 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTPROST
02171 ****************************************************************  ELTPROST
02172      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02173      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02174                          AND                                      ELTPROST
02175         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02176                                                  NOT =  ZERO      ELTPROST
02177          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02178          MOVE +2                    TO  WS-CIA                    ELTPROST
02179          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02180                                                                   ELTPROST
02181      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02182      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02183                           AND                                     ELTPROST
02184         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02185                                                 NOT  =  ZERO      ELTPROST
02186                           AND                                     ELTPROST
02187         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02188          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02189          MOVE +2                    TO  WS-CIA                    ELTPROST
02190          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02191                                                                   ELTPROST
02192      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02193      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02194                           AND                                     ELTPROST
02195         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02196                                                  NOT  =  ZERO     ELTPROST
02197          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTPROST
02198          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
02199                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02200          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02201                               TO  CMF-CODE-VALUE                  ELTPROST
02202          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02203          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02204          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02205             THRU 9500-EXIT.                                       ELTPROST
02206                                                                   ELTPROST
02207      SET PLT-INDEX2  TO  2.                                       ELTPROST
02208      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02209                           AND                                     ELTPROST
02210         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02211                                                 NOT  =  ZERO      ELTPROST
02212          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTPROST
02213          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
02214                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02215          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02216                               TO  CMF-CODE-VALUE                  ELTPROST
02217          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02218          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02219          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02220             THRU 9500-EXIT.                                       ELTPROST
02221                                                                   ELTPROST
02222      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02223         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02224          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02225             THRU 9200-EXIT.                                       ELTPROST
02226                                                                   ELTPROST
02227      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02228         SET PLT-INDEX2  TO  2                                     ELTPROST
02229      ELSE                                                         ELTPROST
02230         SET PLT-INDEX2  TO  1.                                    ELTPROST
02231                                                                   ELTPROST
02232  4150-EXIT.  EXIT.                                                ELTPROST
02233 /                                                                 ELTPROST
02234  4155-REPR-REPL.                                                  ELTPROST
02235 ****************************************************************  ELTPROST
02236 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTPROST
02237 ****************************************************************  ELTPROST
02238      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02239      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02240                          AND                                      ELTPROST
02241         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02242                                                  NOT =  ZERO      ELTPROST
02243          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02244          MOVE +2                    TO  WS-CIA                    ELTPROST
02245          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02246                                                                   ELTPROST
02247      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02248      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02249                           AND                                     ELTPROST
02250         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02251                                                 NOT  =  ZERO      ELTPROST
02252                           AND                                     ELTPROST
02253         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02254          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02255          MOVE +2                    TO  WS-CIA                    ELTPROST
02256          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02257                                                                   ELTPROST
02258      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02259      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02260                           AND                                     ELTPROST
02261         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02262                                                  NOT  =  ZERO     ELTPROST
02263          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTPROST
02264          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
02265                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02266          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02267                               TO  CMF-CODE-VALUE                  ELTPROST
02268          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02269          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02270          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02271             THRU 9500-EXIT.                                       ELTPROST
02272                                                                   ELTPROST
02273      SET PLT-INDEX2  TO  2.                                       ELTPROST
02274      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02275                           AND                                     ELTPROST
02276         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02277                                                 NOT  =  ZERO      ELTPROST
02278          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTPROST
02279          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
02280                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02281          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02282                               TO  CMF-CODE-VALUE                  ELTPROST
02283          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02284          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02285          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02286             THRU 9500-EXIT.                                       ELTPROST
02287                                                                   ELTPROST
02288      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02289         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02290          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02291             THRU 9200-EXIT.                                       ELTPROST
02292                                                                   ELTPROST
02293      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02294         SET PLT-INDEX2  TO  2                                     ELTPROST
02295      ELSE                                                         ELTPROST
02296         SET PLT-INDEX2  TO  1.                                    ELTPROST
02297                                                                   ELTPROST
02298  4155-EXIT.  EXIT.                                                ELTPROST
02299 /                                                                 ELTPROST
02300  4160-SPILLOVR-COINS-N-DEDUC.                                     ELTPROST
02301 ****************************************************************  ELTPROST
02302 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTPROST
02303 ****************************************************************  ELTPROST
02304      MOVE +1  TO  WS-CIA.                                         ELTPROST
02305                                                                   ELTPROST
02306      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02307      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
02308                            AND                                    ELTPROST
02309         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTPROST
02310                                                  NOT =  '0'       ELTPROST
02311         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02312         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPROST
02313                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
02314         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPROST
02315                                                TO  CMF-CODE-VALUE ELTPROST
02316         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTPROST
02317         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTPROST
02318         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
02319            THRU 9500-EXIT                                         ELTPROST
02320         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
02321            THRU 9200-EXIT.                                        ELTPROST
02322 ****************************************************************  ELTPROST
02323 *          S P I L L O V E R   D E D U C T I B L E             *  ELTPROST
02324 ****************************************************************  ELTPROST
02325      MOVE +1  TO  WS-CIA.                                         ELTPROST
02326                                                                   ELTPROST
02327      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02328      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
02329                            AND                                    ELTPROST
02330         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02331                                                  NOT =  '0'       ELTPROST
02332         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02333         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPROST
02334                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
02335         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTPROST
02336                                                 TO  CMF-CODE-VALUEELTPROST
02337         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTPROST
02338         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTPROST
02339         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
02340            THRU 9500-EXIT                                         ELTPROST
02341         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
02342            THRU 9200-EXIT.                                        ELTPROST
02343                                                                   ELTPROST
02344      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02345         SET PLT-INDEX2  TO  2                                     ELTPROST
02346      ELSE                                                         ELTPROST
02347         SET PLT-INDEX2  TO  1.                                    ELTPROST
02348                                                                   ELTPROST
02349  4160-EXIT.  EXIT.                                                ELTPROST
02350      SKIP3                                                        ELTPROST
02351  5000-PROFESSIONAL-OP.                                            ELTPROST
02352 ****************************************************************  ELTPROST
02353 *      PROSTHETIC PROFESSIONAL OUTPATIENT PROCESSING           *  ELTPROST
02354 ****************************************************************  ELTPROST
02355                                                                   ELTPROST
02356      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTPROST
02357                                                                   ELTPROST
02358      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTPROST
02359                                                                   ELTPROST
02360      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTPROST
02361         THRU 9100-EXIT.                                           ELTPROST
02362                                                                   ELTPROST
02363      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPROST
02364      PERFORM WITH TEST BEFORE                                     ELTPROST
02365              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPROST
02366              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPROST
02367         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPROST
02368         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPROST
02369         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPROST
02370         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPROST
02371      END-PERFORM.                                                 ELTPROST
02372      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPROST
02373                                                                   ELTPROST
02374                                                                   ELTPROST
02375      PERFORM 5010-MOVE-IN-PROF-OP-TABS                            ELTPROST
02376         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
02377                        UNTIL   WS-SUB  >     WS-PROF-OP-CNT.      ELTPROST
02378                                                                   ELTPROST
02379      PERFORM 5020-CALL-COVERAGE                                   ELTPROST
02380         THRU 5020-EXIT.                                           ELTPROST
02381                                                                   ELTPROST
02382      IF PVN-COVG-NONE                                             ELTPROST
02383          GO TO 5000-EXIT.                                         ELTPROST
02384                                                                   ELTPROST
02385      PERFORM 5030-FIND-FIRST-NONZERO                              ELTPROST
02386         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTPROST
02387                        UNTIL   WS-SUB  > WS-PROF-OP-CNT.          ELTPROST
02388                                                                   ELTPROST
02389  5000-EXIT.  EXIT.                                                ELTPROST
02390 /                                                                 ELTPROST
02391  5010-MOVE-IN-PROF-OP-TABS.                                       ELTPROST
02392      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
02393      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTPROST
02394                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTPROST
02395                                                                   ELTPROST
02396      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTPROST
02397                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTPROST
02398                                                                   ELTPROST
02399  5010-EXIT.  EXIT.                                                ELTPROST
02400      SKIP3                                                        ELTPROST
02401  5020-CALL-COVERAGE.                                              ELTPROST
02402                                                                   ELTPROST
02403      MOVE 'PROSTHETIC APPLIANCES      ' TO  SSB-TOPIC-PHRASE.     ELTPROST
02404                                                                   ELTPROST
02405      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTPROST
02406                     COMMAREA (DFHCOMMAREA)                        ELTPROST
02407                     END-EXEC.                                     ELTPROST
02408                                                                   ELTPROST
02409      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
02410                     COMMAREA (DFHCOMMAREA)                        ELTPROST
02411                     END-EXEC.                                     ELTPROST
02412                                                                   ELTPROST
02413      IF PVN-COVG-NONE                                             ELTPROST
02414          GO TO 5020-EXIT.                                         ELTPROST
02415                                                                   ELTPROST
02416      MOVE +1  TO  WS-CIA.                                         ELTPROST
02417                                                                   ELTPROST
02418      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPROST
02419                    PSP-PROVN-PRICING-METHD,                       ELTPROST
02420                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTPROST
02421                    PSP-TRANSF-OTHER-RESP-IND,                     ELTPROST
02422                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTPROST
02423                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTPROST
02424                    PSP-SPILL-OVER-DED-APL-IND,                    ELTPROST
02425                    PSP-CERTFN-REQRM-IND,                          ELTPROST
02426                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTPROST
02427                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTPROST
02428                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTPROST
02429                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTPROST
02430                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTPROST
02431                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTPROST
02432                    PSE-CERTN-REPETN-REQRD-IND,                    ELTPROST
02433                    PSE-REPR-REPLAC-RESTRN-IND.                    ELTPROST
02434                                                                   ELTPROST
02435      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTPROST
02436                     COMMAREA (DFHCOMMAREA)                        ELTPROST
02437                     END-EXEC.                                     ELTPROST
02438                                                                   ELTPROST
02439      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPROST
02440      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
02441          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTPROST
02442                                                                   ELTPROST
02443  5020-EXIT.  EXIT.                                                ELTPROST
02444      SKIP3                                                        ELTPROST
02445  5030-FIND-FIRST-NONZERO.                                         ELTPROST
02446      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
02447                                                                   ELTPROST
02448      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTPROST
02449          CONTINUE                                                 ELTPROST
02450      ELSE                                                         ELTPROST
02451          PERFORM 5100-BUILD-SCREEN-LINES                          ELTPROST
02452             THRU 5100-EXIT.                                       ELTPROST
02453                                                                   ELTPROST
02454  5030-EXIT.  EXIT.                                                ELTPROST
02455 /                                                                 ELTPROST
02456  5100-BUILD-SCREEN-LINES.                                         ELTPROST
02457      SET PLT-INDEX1  TO                                           ELTPROST
02458              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTPROST
02459                                                                   ELTPROST
02460      IF WS-NOT-FIRST-TIME                                         ELTPROST
02461         SET COF-NEW-PAGE TO TRUE                                  ELTPROST
02462         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTPROST
02463         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTPROST
02464                   COMMAREA (DFHCOMMAREA)                          ELTPROST
02465         END-EXEC                                                  ELTPROST
02466      ELSE                                                         ELTPROST
02467         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTPROST
02468                                                                   ELTPROST
02469      MOVE  +1  TO  WS-CIA.                                        ELTPROST
02470                                                                   ELTPROST
02471      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTPROST
02472          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTPROST
02473              SET PLT-INDEX2  TO  2                                ELTPROST
02474          ELSE                                                     ELTPROST
02475              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTPROST
02476              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTPROST
02477                 THRU 9200-EXIT                                    ELTPROST
02478              GO TO 5100-EXIT                                      ELTPROST
02479      ELSE                                                         ELTPROST
02480          SET PLT-INDEX2  TO  1.                                   ELTPROST
02481                                                                   ELTPROST
02482      PERFORM 5105-LIST-BEN-PROV                                   ELTPROST
02483         THRU 5105-EXIT.                                           ELTPROST
02484                                                                   ELTPROST
02485      PERFORM 5110-PLACE-OF-TREATMENT                              ELTPROST
02486         THRU 5110-EXIT.                                           ELTPROST
02487                                                                   ELTPROST
02488      PERFORM 5120-PRIC-METH                                       ELTPROST
02489         THRU 5120-EXIT.                                           ELTPROST
02490                                                                   ELTPROST
02491      PERFORM 5140-CERTIFICATION                                   ELTPROST
02492         THRU 5140-EXIT.                                           ELTPROST
02493                                                                   ELTPROST
02494      PERFORM 5150-RECERTIFICATION                                 ELTPROST
02495         THRU 5150-EXIT.                                           ELTPROST
02496                                                                   ELTPROST
02497      PERFORM 5155-REPR-REPL                                       ELTPROST
02498         THRU 5155-EXIT.                                           ELTPROST
02499                                                                   ELTPROST
02500      PERFORM 5160-SPILLOVR-COINS-N-DEDUC                          ELTPROST
02501         THRU 5160-EXIT.                                           ELTPROST
02502                                                                   ELTPROST
02503                                                                   ELTPROST
02504      PERFORM 2165-TRANS-OTHER-RESP-IND                            ELTPROST
02505         THRU 2165-EXIT.                                           ELTPROST
02506                                                                   ELTPROST
02507      PERFORM 7000-ALL-LEVEL-TABS                                  ELTPROST
02508         THRU 7000-EXIT.                                           ELTPROST
02509                                                                   ELTPROST
02510  5100-EXIT.  EXIT.                                                ELTPROST
02511 /                                                                 ELTPROST
02512  5105-LIST-BEN-PROV.                                              ELTPROST
02513 ****************************************************************  ELTPROST
02514 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTPROST
02515 ****************************************************************  ELTPROST
02516      MOVE  +2               TO  WS-CIA.                           ELTPROST
02517      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTPROST
02518      MOVE ZERO              TO  WS-SUB2.                          ELTPROST
02519      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTPROST
02520                             TO  WS-SUB3.                          ELTPROST
02521                                                                   ELTPROST
02522      PERFORM 5106-ZERO-ALL-WITH-SAME-NO                           ELTPROST
02523         THRU 5106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTPROST
02524                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.ELTPROST
02525                                                                   ELTPROST
02526      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPROST
02527      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTPROST
02528                                                                   ELTPROST
02529      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
02530                     COMMAREA (DFHCOMMAREA)                        ELTPROST
02531                     END-EXEC.                                     ELTPROST
02532                                                                   ELTPROST
02533  5105-EXIT.  EXIT.                                                ELTPROST
02534      SKIP3                                                        ELTPROST
02535  5106-ZERO-ALL-WITH-SAME-NO.                                      ELTPROST
02536                                                                   ELTPROST
02537      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTPROST
02538          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTPROST
02539          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTPROST
02540          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTPROST
02541                            TO  CMF-CODE-VALUE                     ELTPROST
02542          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTPROST
02543          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTPROST
02544          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02545             THRU 9500-EXIT                                        ELTPROST
02546          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTPROST
02547          ADD  +1    TO  WS-SUB2                                   ELTPROST
02548          IF WS-CIA  >  20  OR  =  20                              ELTPROST
02549              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTPROST
02550              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTPROST
02551                             COMMAREA (DFHCOMMAREA)                ELTPROST
02552                             END-EXEC                              ELTPROST
02553              MOVE  +1  TO  WS-CIA.                                ELTPROST
02554                                                                   ELTPROST
02555  5106-EXIT.  EXIT.                                                ELTPROST
02556 /                                                                 ELTPROST
02557  5110-PLACE-OF-TREATMENT.                                         ELTPROST
02558 ****************************************************************  ELTPROST
02559 *              P L A C E   O F   T R E A T M E N T             *  ELTPROST
02560 ****************************************************************  ELTPROST
02561      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02562      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02563                          AND                                      ELTPROST
02564         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
02565                                                  NOT =  ZERO      ELTPROST
02566          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02567          MOVE +2                    TO  WS-CIA                    ELTPROST
02568          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02569                                                                   ELTPROST
02570      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02571      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02572                           AND                                     ELTPROST
02573         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
02574                                                 NOT  =  ZERO      ELTPROST
02575                           AND                                     ELTPROST
02576         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02577          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02578          MOVE +2                    TO  WS-CIA                    ELTPROST
02579          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02580                                                                   ELTPROST
02581      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02582      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02583                           AND                                     ELTPROST
02584         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
02585                                                  NOT  =  ZERO     ELTPROST
02586          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
02587          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
02588                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02589          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
02590                               TO  CMF-CODE-VALUE                  ELTPROST
02591          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02592          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02593          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02594             THRU 9500-EXIT.                                       ELTPROST
02595                                                                   ELTPROST
02596      SET PLT-INDEX2  TO  2.                                       ELTPROST
02597      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02598                           AND                                     ELTPROST
02599         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTPROST
02600                                                 NOT  =  ZERO      ELTPROST
02601          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTPROST
02602          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTPROST
02603                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02604          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTPROST
02605                               TO  CMF-CODE-VALUE                  ELTPROST
02606          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02607          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02608          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02609             THRU 9500-EXIT.                                       ELTPROST
02610                                                                   ELTPROST
02611      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02612         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02613          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02614             THRU 9200-EXIT.                                       ELTPROST
02615                                                                   ELTPROST
02616      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02617         SET PLT-INDEX2  TO  2                                     ELTPROST
02618      ELSE                                                         ELTPROST
02619         SET PLT-INDEX2  TO  1.                                    ELTPROST
02620                                                                   ELTPROST
02621  5110-EXIT.  EXIT.                                                ELTPROST
02622 /                                                                 ELTPROST
02623  5120-PRIC-METH.                                                  ELTPROST
02624 ****************************************************************  ELTPROST
02625 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTPROST
02626 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTPROST
02627 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTPROST
02628 ****************************************************************  ELTPROST
02629      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02630      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02631         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02632                                                              '19' ELTPROST
02633         MOVE +2             TO  WS-CIA                            ELTPROST
02634         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
02635         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
02636                                                                   ELTPROST
02637      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02638      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
02639         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02640                                                        '19' AND   ELTPROST
02641         NOT WS-ADD-A-BLANK-LINE                                   ELTPROST
02642         MOVE +2             TO  WS-CIA                            ELTPROST
02643         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTPROST
02644         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTPROST
02645                                                                   ELTPROST
02646      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02647      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02648         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
02649                            AND                                    ELTPROST
02650         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02651         SET  PLT-INDEX2  TO  2                                    ELTPROST
02652         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
02653                                                             ZERO  ELTPROST
02654            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
02655            ADD +1  TO  WS-CIA                                     ELTPROST
02656            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
02657                                                                   ELTPROST
02658      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02659      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02660         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPROST
02661                            AND                                    ELTPROST
02662         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPROST
02663         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPROST
02664         ADD +1  TO  WS-CIA                                        ELTPROST
02665         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTPROST
02666                                                                   ELTPROST
02667      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPROST
02668         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02669         SET  PLT-INDEX2  TO  2                                    ELTPROST
02670         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPROST
02671                                                             ZERO  ELTPROST
02672            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPROST
02673            ADD +1  TO  WS-CIA                                     ELTPROST
02674            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTPROST
02675                                                                   ELTPROST
02676      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02677      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02678         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPROST
02679                                                            =  ZEROELTPROST
02680            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02681                                                            =  ZEROELTPROST
02682               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
02683            ELSE                                                   ELTPROST
02684               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
02685          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02686                                                  TO  WS-PERCENTAGEELTPROST
02687         ELSE                                                      ELTPROST
02688            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
02689          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02690                                                 TO  WS-PERCENTAGE.ELTPROST
02691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02692         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02693                                             ZERO AND  NOT =  '19' ELTPROST
02694         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02695         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
02696         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
02697                                                    CMF-CODE-VALUE ELTPROST
02698         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02699         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
02700         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
02701            THRU 9600-EXIT.                                        ELTPROST
02702                                                                   ELTPROST
02703      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02704      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02705         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPROST
02706                                                               ZEROELTPROST
02707            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02708                                                            =  ZEROELTPROST
02709               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPROST
02710            ELSE                                                   ELTPROST
02711               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPROST
02712          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02713                                                  TO  WS-PERCENTAGEELTPROST
02714         ELSE                                                      ELTPROST
02715            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPROST
02716          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPROST
02717                                                 TO  WS-PERCENTAGE.ELTPROST
02718      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPROST
02719         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPROST
02720                                             ZERO AND  NOT =  '19' ELTPROST
02721         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02722         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPROST
02723         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPROST
02724                                                    CMF-CODE-VALUE ELTPROST
02725         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPROST
02726         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPROST
02727         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTPROST
02728            THRU 9600-EXIT.                                        ELTPROST
02729                                                                   ELTPROST
02730      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02731          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTPROST
02732          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02733             THRU 9200-EXIT.                                       ELTPROST
02734                                                                   ELTPROST
02735      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02736         SET PLT-INDEX2  TO  2                                     ELTPROST
02737      ELSE                                                         ELTPROST
02738         SET PLT-INDEX2  TO  1.                                    ELTPROST
02739                                                                   ELTPROST
02740  5120-EXIT.  EXIT.                                                ELTPROST
02741 /                                                                 ELTPROST
02742  5140-CERTIFICATION.                                              ELTPROST
02743 ****************************************************************  ELTPROST
02744 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTPROST
02745 ****************************************************************  ELTPROST
02746      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02747      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02748                          AND                                      ELTPROST
02749         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02750                                                  NOT =  '00'      ELTPROST
02751          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02752          MOVE +2                    TO  WS-CIA                    ELTPROST
02753          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02754                                                                   ELTPROST
02755      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02756      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02757                           AND                                     ELTPROST
02758         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02759                                                 NOT  =  '00'      ELTPROST
02760                           AND                                     ELTPROST
02761         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02762          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02763          MOVE +2                    TO  WS-CIA                    ELTPROST
02764          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02765                                                                   ELTPROST
02766      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02767      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02768                           AND                                     ELTPROST
02769         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02770                                                  NOT  =  '00'     ELTPROST
02771          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTPROST
02772          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
02773                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02774          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02775                               TO  CMF-CODE-VALUE                  ELTPROST
02776          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02777          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02778          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02779             THRU 9500-EXIT.                                       ELTPROST
02780                                                                   ELTPROST
02781      SET PLT-INDEX2  TO  2.                                       ELTPROST
02782      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02783                           AND                                     ELTPROST
02784         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTPROST
02785                                                 NOT  =  '00'      ELTPROST
02786          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTPROST
02787          MOVE 'CERTFN-REQRM-IND'                                  ELTPROST
02788                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02789          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02790                               TO  CMF-CODE-VALUE                  ELTPROST
02791          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02792          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02793          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02794             THRU 9500-EXIT.                                       ELTPROST
02795                                                                   ELTPROST
02796      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02797         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02798          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02799             THRU 9200-EXIT.                                       ELTPROST
02800                                                                   ELTPROST
02801      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02802         SET PLT-INDEX2  TO  2                                     ELTPROST
02803      ELSE                                                         ELTPROST
02804         SET PLT-INDEX2  TO  1.                                    ELTPROST
02805                                                                   ELTPROST
02806  5140-EXIT.  EXIT.                                                ELTPROST
02807 /                                                                 ELTPROST
02808  5150-RECERTIFICATION.                                            ELTPROST
02809 ****************************************************************  ELTPROST
02810 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTPROST
02811 ****************************************************************  ELTPROST
02812      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02813      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02814                          AND                                      ELTPROST
02815         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02816                                                  NOT =  ZERO      ELTPROST
02817          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02818          MOVE +2                    TO  WS-CIA                    ELTPROST
02819          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02820                                                                   ELTPROST
02821      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02822      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02823                           AND                                     ELTPROST
02824         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02825                                                 NOT  =  ZERO      ELTPROST
02826                           AND                                     ELTPROST
02827         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02828          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02829          MOVE +2                    TO  WS-CIA                    ELTPROST
02830          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02831                                                                   ELTPROST
02832      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02833      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02834                           AND                                     ELTPROST
02835         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02836                                                  NOT  =  ZERO     ELTPROST
02837          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTPROST
02838          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
02839                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02840          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02841                               TO  CMF-CODE-VALUE                  ELTPROST
02842          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02843          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02844          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02845             THRU 9500-EXIT.                                       ELTPROST
02846                                                                   ELTPROST
02847      SET PLT-INDEX2  TO  2.                                       ELTPROST
02848      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02849                           AND                                     ELTPROST
02850         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02851                                                 NOT  =  ZERO      ELTPROST
02852          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTPROST
02853          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTPROST
02854                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02855          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02856                               TO  CMF-CODE-VALUE                  ELTPROST
02857          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02858          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02859          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02860             THRU 9500-EXIT.                                       ELTPROST
02861                                                                   ELTPROST
02862      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02863         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02864          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02865             THRU 9200-EXIT.                                       ELTPROST
02866                                                                   ELTPROST
02867      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02868         SET PLT-INDEX2  TO  2                                     ELTPROST
02869      ELSE                                                         ELTPROST
02870         SET PLT-INDEX2  TO  1.                                    ELTPROST
02871                                                                   ELTPROST
02872  5150-EXIT.  EXIT.                                                ELTPROST
02873 /                                                                 ELTPROST
02874  5155-REPR-REPL.                                                  ELTPROST
02875 ****************************************************************  ELTPROST
02876 *      R E P A I R   /   R E P L A C E   I N D I C A T O R     *  ELTPROST
02877 ****************************************************************  ELTPROST
02878      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02879      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02880                          AND                                      ELTPROST
02881         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02882                                                  NOT =  ZERO      ELTPROST
02883          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02884          MOVE +2                    TO  WS-CIA                    ELTPROST
02885          MOVE WS-RESTRICT           TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02886                                                                   ELTPROST
02887      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02888      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02889                           AND                                     ELTPROST
02890         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02891                                                 NOT  =  ZERO      ELTPROST
02892                           AND                                     ELTPROST
02893         NOT  WS-ADD-A-BLANK-LINE                                  ELTPROST
02894          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTPROST
02895          MOVE +2                    TO  WS-CIA                    ELTPROST
02896          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTPROST
02897                                                                   ELTPROST
02898      SET  PLT-INDEX2  TO  1.                                      ELTPROST
02899      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPROST
02900                           AND                                     ELTPROST
02901         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02902                                                  NOT  =  ZERO     ELTPROST
02903          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTPROST
02904          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
02905                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02906          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02907                               TO  CMF-CODE-VALUE                  ELTPROST
02908          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTPROST
02909          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02910          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02911             THRU 9500-EXIT.                                       ELTPROST
02912                                                                   ELTPROST
02913      SET PLT-INDEX2  TO  2.                                       ELTPROST
02914      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPROST
02915                           AND                                     ELTPROST
02916         PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02917                                                 NOT  =  ZERO      ELTPROST
02918          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTPROST
02919          MOVE 'REPR-REPLAC-RESTRN-IND'                            ELTPROST
02920                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTPROST
02921          MOVE PLE-REPR-REPLAC-RESTRN-IND (PLT-INDEX1, PLT-INDEX2) ELTPROST
02922                               TO  CMF-CODE-VALUE                  ELTPROST
02923          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPROST
02924          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTPROST
02925          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTPROST
02926             THRU 9500-EXIT.                                       ELTPROST
02927                                                                   ELTPROST
02928      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
02929         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
02930          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTPROST
02931             THRU 9200-EXIT.                                       ELTPROST
02932                                                                   ELTPROST
02933      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02934         SET PLT-INDEX2  TO  2                                     ELTPROST
02935      ELSE                                                         ELTPROST
02936         SET PLT-INDEX2  TO  1.                                    ELTPROST
02937                                                                   ELTPROST
02938  5155-EXIT.  EXIT.                                                ELTPROST
02939 /                                                                 ELTPROST
02940  5160-SPILLOVR-COINS-N-DEDUC.                                     ELTPROST
02941 ****************************************************************  ELTPROST
02942 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTPROST
02943 ****************************************************************  ELTPROST
02944      MOVE +1  TO  WS-CIA.                                         ELTPROST
02945                                                                   ELTPROST
02946      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02947      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
02948                            AND                                    ELTPROST
02949         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTPROST
02950                                                  NOT =  '0'       ELTPROST
02951         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02952         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPROST
02953                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
02954         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPROST
02955                                                TO  CMF-CODE-VALUE ELTPROST
02956         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTPROST
02957         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTPROST
02958         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
02959            THRU 9500-EXIT                                         ELTPROST
02960         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
02961            THRU 9200-EXIT.                                        ELTPROST
02962 ****************************************************************  ELTPROST
02963 *          S P I L L O V E R   D E D U C T I B L E             *  ELTPROST
02964 ****************************************************************  ELTPROST
02965      MOVE +1  TO  WS-CIA.                                         ELTPROST
02966                                                                   ELTPROST
02967      SET  PLT-INDEX2  TO  2.                                      ELTPROST
02968      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTPROST
02969                            AND                                    ELTPROST
02970         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTPROST
02971                                                  NOT =  '0'       ELTPROST
02972         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPROST
02973         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPROST
02974                                           CMF-ELEMENT-SYSTEM-NAME ELTPROST
02975         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTPROST
02976                                                 TO  CMF-CODE-VALUEELTPROST
02977         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTPROST
02978         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTPROST
02979         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTPROST
02980            THRU 9500-EXIT                                         ELTPROST
02981         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTPROST
02982            THRU 9200-EXIT.                                        ELTPROST
02983                                                                   ELTPROST
02984      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPROST
02985         SET PLT-INDEX2  TO  2                                     ELTPROST
02986      ELSE                                                         ELTPROST
02987         SET PLT-INDEX2  TO  1.                                    ELTPROST
02988                                                                   ELTPROST
02989  5160-EXIT.  EXIT.                                                ELTPROST
02990      SKIP3                                                        ELTPROST
02991  7000-ALL-LEVEL-TABS.                                             ELTPROST
02992 ****************************************************************  ELTPROST
02993 *                  A A R   T A B U L A R                       *  ELTPROST
02994 ****************************************************************  ELTPROST
02995      SET PLT-INDEX2  TO  1.                                       ELTPROST
02996      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
02997         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
02998                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
02999         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
03000         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTPROST
03001         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTPROST
03002      ELSE                                                         ELTPROST
03003         SET PLT-INDEX2  TO  2                                     ELTPROST
03004         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03005            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03006                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03007            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTPROST
03008            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTPROST
03009            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTPROST
03010                                                                   ELTPROST
03011      IF WS-ADD-A-BLANK-LINE                                       ELTPROST
03012         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPROST
03013         MOVE 1  TO  WS-CIA                                        ELTPROST
03014         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPROST
03015             COMMAREA(DFHCOMMAREA)                                 ELTPROST
03016         END-EXEC.                                                 ELTPROST
03017 *--------------------------------------------------------------*  ELTPROST
03018 *                  P P F   T A B U L A R                       *  ELTPROST
03019 *--------------------------------------------------------------*  ELTPROST
03020      SET PLT-INDEX2  TO  1.                                       ELTPROST
03021      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
03022         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
03023                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03024         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTPROST
03025                                             KWA-GCTABULR-KEY      ELTPROST
03026         PERFORM 9900-GET-TABULAR-RECORD                           ELTPROST
03027            THRU 9900-EXIT                                         ELTPROST
03028         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTPROST
03029               COMMAREA(DFHCOMMAREA)                               ELTPROST
03030         END-EXEC                                                  ELTPROST
03031      ELSE                                                         ELTPROST
03032         SET PLT-INDEX2  TO  2                                     ELTPROST
03033         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03034            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03035                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03036          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTPROST
03037                                             KWA-GCTABULR-KEY      ELTPROST
03038            PERFORM 9900-GET-TABULAR-RECORD                        ELTPROST
03039               THRU 9900-EXIT                                      ELTPROST
03040            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTPROST
03041                  COMMAREA(DFHCOMMAREA)                            ELTPROST
03042            END-EXEC.                                              ELTPROST
03043 *--------------------------------------------------------------*  ELTPROST
03044 *                  P V E   T A B U L A R                       *  ELTPROST
03045 *--------------------------------------------------------------*  ELTPROST
03046                                                                   ELTPROST
03047      MOVE  +2     TO  WS-CIA.                                     ELTPROST
03048      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTPROST
03049      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTPROST
03050         THRU 9200-EXIT.                                           ELTPROST
03051                                                                   ELTPROST
03052 *--------------------------------------------------------------*  ELTPROST
03053 *                  A B M   T A B U L A R                       *  ELTPROST
03054 *--------------------------------------------------------------*  ELTPROST
03055      SET PLT-INDEX2  TO  1.                                       ELTPROST
03056      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTPROST
03057                              AND                                  ELTPROST
03058         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
03059                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03060         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTPROST
03061                                              KWA-GCTABULR-KEY     ELTPROST
03062         PERFORM 9900-GET-TABULAR-RECORD                           ELTPROST
03063            THRU 9900-EXIT                                         ELTPROST
03064         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTPROST
03065               COMMAREA(DFHCOMMAREA)                               ELTPROST
03066         END-EXEC                                                  ELTPROST
03067      ELSE                                                         ELTPROST
03068         SET PLT-INDEX2  TO  2                                     ELTPROST
03069         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03070            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03071                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03072          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTPROST
03073                                             KWA-GCTABULR-KEY      ELTPROST
03074            PERFORM 9900-GET-TABULAR-RECORD                        ELTPROST
03075               THRU 9900-EXIT                                      ELTPROST
03076            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTPROST
03077                  COMMAREA(DFHCOMMAREA)                            ELTPROST
03078            END-EXEC.                                              ELTPROST
03079 *--------------------------------------------------------------*  ELTPROST
03080 *                  A C L   T A B U L A R                       *  ELTPROST
03081 *--------------------------------------------------------------*  ELTPROST
03082      SET PLT-INDEX2  TO  1.                                       ELTPROST
03083      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
03084         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
03085                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03086         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTPROST
03087                                              KWA-GCTABULR-KEY     ELTPROST
03088         PERFORM 9900-GET-TABULAR-RECORD                           ELTPROST
03089            THRU 9900-EXIT                                         ELTPROST
03090         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTPROST
03091               COMMAREA(DFHCOMMAREA)                               ELTPROST
03092         END-EXEC                                                  ELTPROST
03093      ELSE                                                         ELTPROST
03094         SET PLT-INDEX2  TO  2                                     ELTPROST
03095         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03096            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03097                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03098          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTPROST
03099                                              KWA-GCTABULR-KEY     ELTPROST
03100            PERFORM 9900-GET-TABULAR-RECORD                        ELTPROST
03101               THRU 9900-EXIT                                      ELTPROST
03102            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTPROST
03103                  COMMAREA(DFHCOMMAREA)                            ELTPROST
03104            END-EXEC.                                              ELTPROST
03105 *--------------------------------------------------------------*  ELTPROST
03106 *                  A D L   T A B U L A R                       *  ELTPROST
03107 *--------------------------------------------------------------*  ELTPROST
03108      SET PLT-INDEX2  TO  1.                                       ELTPROST
03109      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
03110         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
03111                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03112         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTPROST
03113                                             KWA-GCTABULR-KEY      ELTPROST
03114         PERFORM 9900-GET-TABULAR-RECORD                           ELTPROST
03115            THRU 9900-EXIT                                         ELTPROST
03116         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTPROST
03117               COMMAREA(DFHCOMMAREA)                               ELTPROST
03118         END-EXEC                                                  ELTPROST
03119      ELSE                                                         ELTPROST
03120         SET PLT-INDEX2  TO  2                                     ELTPROST
03121         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03122            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03123                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03124          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTPROST
03125                                               KWA-GCTABULR-KEY    ELTPROST
03126            PERFORM 9900-GET-TABULAR-RECORD                        ELTPROST
03127               THRU 9900-EXIT                                      ELTPROST
03128            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTPROST
03129                  COMMAREA(DFHCOMMAREA)                            ELTPROST
03130            END-EXEC.                                              ELTPROST
03131 *--------------------------------------------------------------*  ELTPROST
03132 *                  A O L   T A B U L A R                       *  ELTPROST
03133 *--------------------------------------------------------------*  ELTPROST
03134      SET PLT-INDEX2  TO  1.                                       ELTPROST
03135      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPROST
03136         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTPROST
03137                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03138         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTPROST
03139                                             KWA-GCTABULR-KEY      ELTPROST
03140         PERFORM 9900-GET-TABULAR-RECORD                           ELTPROST
03141            THRU 9900-EXIT                                         ELTPROST
03142         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTPROST
03143               COMMAREA(DFHCOMMAREA)                               ELTPROST
03144         END-EXEC                                                  ELTPROST
03145      ELSE                                                         ELTPROST
03146         SET PLT-INDEX2  TO  2                                     ELTPROST
03147         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTPROST
03148            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTPROST
03149                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTPROST
03150          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTPROST
03151                                              KWA-GCTABULR-KEY     ELTPROST
03152            PERFORM 9900-GET-TABULAR-RECORD                        ELTPROST
03153               THRU 9900-EXIT                                      ELTPROST
03154            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTPROST
03155                  COMMAREA(DFHCOMMAREA)                            ELTPROST
03156            END-EXEC.                                              ELTPROST
03157 *--------------------------------------------------------------*  ELTPROST
03158 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTPROST
03159 *--------------------------------------------------------------*  ELTPROST
03160                                                                   ELTPROST
03161      ADD   +2     TO  WS-CIA.                                     ELTPROST
03162      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTPROST
03163      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTPROST
03164         THRU 9200-EXIT.                                           ELTPROST
03165                                                                   ELTPROST
03166  7000-EXIT.  EXIT.                                                ELTPROST
03167 /                                                                 ELTPROST
03168 ****************************************************************  ELTPROST
03169 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTPROST
03170 ****************************************************************  ELTPROST
03171  9100-HEADER-OUTPUT-REQUEST.                                      ELTPROST
03172                                                                   ELTPROST
03173      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTPROST
03174      SET COF-NEW-PAGE TO TRUE.                                    ELTPROST
03175                                                                   ELTPROST
03176      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
03177                     COMMAREA (DFHCOMMAREA)                        ELTPROST
03178                     END-EXEC.                                     ELTPROST
03179                                                                   ELTPROST
03180  9100-EXIT.  EXIT.                                                ELTPROST
03181      SKIP3                                                        ELTPROST
03182 ****************************************************************  ELTPROST
03183 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTPROST
03184 ****************************************************************  ELTPROST
03185  9200-TEXT-OUTPUT-REQUEST.                                        ELTPROST
03186                                                                   ELTPROST
03187      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTPROST
03188      MOVE +0      TO  COF-NBR-HDR-LINES                           ELTPROST
03189                       WS-CIA.                                     ELTPROST
03190      SET COF-CONTINUE TO TRUE.                                    ELTPROST
03191                                                                   ELTPROST
03192      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPROST
03193                     COMMAREA (DFHCOMMAREA)                        ELTPROST
03194                     END-EXEC.                                     ELTPROST
03195                                                                   ELTPROST
03196  9200-EXIT.  EXIT.                                                ELTPROST
03197 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPROST
03198  9500-CALL-CODES-MANUAL-LONG.                                     ELTPROST
03199                                                                   ELTPROST
03200      INITIALIZE CMF-RETURN-CODE                                   ELTPROST
03201                 TCAR-FROM-AREA.                                   ELTPROST
03202                                                                   ELTPROST
03203      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTPROST
03204                       COMMAREA(DFHCOMMAREA)                       ELTPROST
03205      END-EXEC.                                                    ELTPROST
03206                                                                   ELTPROST
03207      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPROST
03208      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
03209          ADDRESS OF CMF-DESCR.                                    ELTPROST
03210                                                                   ELTPROST
03211      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPROST
03212         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPROST
03213         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTPROST
03214      ELSE                                                         ELTPROST
03215         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTPROST
03216                                                                   ELTPROST
03217      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTPROST
03218          VARYING WS-SUB1 FROM 1 BY 1                              ELTPROST
03219          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTPROST
03220                                                                   ELTPROST
03221      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPROST
03222                                                                   ELTPROST
03223      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPROST
03224      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPROST
03225      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN                        ELTPROST
03226                    TCAR-OUTPUT-FIELD-3-LEN                        ELTPROST
03227                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPROST
03228      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPROST
03229                                                                   ELTPROST
03230      IF WS-MOVE-LINES-TO-CIA                                      ELTPROST
03231         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPROST
03232            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPROST
03233                                             WS-TEMP-NOT-USED-CNT  ELTPROST
03234            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTPROST
03235               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTPROST
03236                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTPROST
03237            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPROST
03238            ADD +1  TO  WS-CIA                                     ELTPROST
03239            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPROST
03240         ELSE                                                      ELTPROST
03241            ADD +1  TO  WS-CIA                                     ELTPROST
03242            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTPROST
03243                                                                   ELTPROST
03244      IF WS-MOVE-LINES-TO-CIA                                      ELTPROST
03245         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPROST
03246            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTPROST
03247               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTPROST
03248                              UNTIL   WS-SUB1 >                    ELTPROST
03249                              TCAR-OUTPUT-FIELDS-USED              ELTPROST
03250         ELSE                                                      ELTPROST
03251            CONTINUE                                               ELTPROST
03252      ELSE                                                         ELTPROST
03253         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPROST
03254                                                                   ELTPROST
03255  9500-EXIT.  EXIT.                                                ELTPROST
03256                                                                   ELTPROST
03257  9540-MOVE-LINES-OUT.                                             ELTPROST
03258      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTPROST
03259             ' ' DELIMITED BY SIZE                                 ELTPROST
03260             CMF-DESCR-LINE (WS-SUB1) DELIMITED BY SIZE            ELTPROST
03261      INTO TCAR-FROM-AREA.                                         ELTPROST
03262                                                                   ELTPROST
03263  9540-EXIT.  EXIT.                                                ELTPROST
03264                                                                   ELTPROST
03265  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTPROST
03266      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTPROST
03267      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTPROST
03268                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTPROST
03269                                                                   ELTPROST
03270  9550-EXIT.  EXIT.                                                ELTPROST
03271                                                                   ELTPROST
03272 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPROST
03273  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTPROST
03274                                                                   ELTPROST
03275      INITIALIZE CMF-RETURN-CODE                                   ELTPROST
03276                 TCAR-FROM-AREA.                                   ELTPROST
03277                                                                   ELTPROST
03278      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTPROST
03279                       COMMAREA(DFHCOMMAREA)                       ELTPROST
03280      END-EXEC.                                                    ELTPROST
03281                                                                   ELTPROST
03282      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPROST
03283      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
03284          ADDRESS OF CMF-DESCR.                                    ELTPROST
03285                                                                   ELTPROST
03286      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPROST
03287         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPROST
03288         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTPROST
03289      ELSE                                                         ELTPROST
03290         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTPROST
03291                                                                   ELTPROST
03292      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTPROST
03293             WS-PERCENT-FLD DELIMITED BY SIZE                      ELTPROST
03294      INTO TCAR-FROM-AREA.                                         ELTPROST
03295                                                                   ELTPROST
03296      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTPROST
03297          VARYING WS-SUB1 FROM 1 BY 1                              ELTPROST
03298          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTPROST
03299                                                                   ELTPROST
03300                                                                   ELTPROST
03301      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPROST
03302                                                                   ELTPROST
03303      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPROST
03304      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPROST
03305      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTPROST
03306                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTPROST
03307                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPROST
03308      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPROST
03309                                                                   ELTPROST
03310      IF WS-MOVE-LINES-TO-CIA                                      ELTPROST
03311         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPROST
03312            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPROST
03313                                             WS-TEMP-NOT-USED-CNT  ELTPROST
03314            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTPROST
03315               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTPROST
03316                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTPROST
03317            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPROST
03318            ADD +1  TO  WS-CIA                                     ELTPROST
03319            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPROST
03320         ELSE                                                      ELTPROST
03321            ADD +1  TO  WS-CIA                                     ELTPROST
03322            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTPROST
03323                                                                   ELTPROST
03324      IF WS-MOVE-LINES-TO-CIA                                      ELTPROST
03325         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPROST
03326            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTPROST
03327               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTPROST
03328                              UNTIL   WS-SUB1 >                    ELTPROST
03329                              TCAR-OUTPUT-FIELDS-USED              ELTPROST
03330         ELSE                                                      ELTPROST
03331            CONTINUE                                               ELTPROST
03332      ELSE                                                         ELTPROST
03333         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPROST
03334                                                                   ELTPROST
03335  9600-EXIT.  EXIT.                                                ELTPROST
03336                                                                   ELTPROST
03337  9660-MOVE-LINES-TO-CIA.                                          ELTPROST
03338      ADD +1  TO  WS-CIA.                                          ELTPROST
03339      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTPROST
03340                                                                   ELTPROST
03341  9660-EXIT.  EXIT.                                                ELTPROST
03342 /                                                                 ELTPROST
03343 ***************************************************************** ELTPROST
03344 *            G E T   T A B U L A R   R E C O R D                  ELTPROST
03345 *                                                                 ELTPROST
03346 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTPROST
03347 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTPROST
03348 *  TO DISPLAY.                                                    ELTPROST
03349 *                                                                 ELTPROST
03350 ***************************************************************** ELTPROST
03351  9900-GET-TABULAR-RECORD.                                         ELTPROST
03352                                                                   ELTPROST
03353      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPROST
03354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPROST
03355          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTPROST
03356      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTPROST
03357      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTPROST
03358      SET IOP-RD                          TO TRUE.                 ELTPROST
03359      SET IOP-FCQ-NONE                    TO TRUE.                 ELTPROST
03360      SET IOP-KVQ-NONE                    TO TRUE.                 ELTPROST
03361      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTPROST
03362                                                                   ELTPROST
03363      EXEC CICS LINK                                               ELTPROST
03364                PROGRAM ('ELUIOPGM')                               ELTPROST
03365                COMMAREA (DFHCOMMAREA)                             ELTPROST
03366      END-EXEC.                                                    ELTPROST
03367                                                                   ELTPROST
03368      IF IOP-RC-NOTFND                                             ELTPROST
03369         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTPROST
03370         EXEC CICS ABEND                                           ELTPROST
03371                   ABCODE(CIA-ABCODE)                              ELTPROST
03372         END-EXEC                                                  ELTPROST
03373      ELSE                                                         ELTPROST
03374          IF NOT IOP-RC-OK                                         ELTPROST
03375             SET CIA-AB-CRITIO TO TRUE                             ELTPROST
03376             EXEC CICS ABEND                                       ELTPROST
03377                       ABCODE(CIA-ABCODE)                          ELTPROST
03378             END-EXEC                                              ELTPROST
03379      END-IF.                                                      ELTPROST
03380  9900-EXIT.  EXIT.                                                ELTPROST
03381                                                                   ELTPROST
03382      COPY ELSTCOMP.                                               ELTPROST
