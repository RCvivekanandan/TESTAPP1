00001 *      LAST MAINTENANCE TIME: 13.44.26  DATE: 04/05/86            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTSURGR
00003  PROGRAM-ID. ELTSURGR.                                               LV002
00004  AUTHOR. D SECOR  -  A C I.                                       ELTSURGR
00005  DATE-WRITTEN.   4/01/86.                                         ELTSURGR
00006  DATE-COMPILED.                                                   ELTSURGR
00007      SKIP3                                                        ELTSURGR
00008 ******************************************************************ELTSURGR
00009 * ELTSURGR                                                        ELTSURGR
00010 *                                                                 ELTSURGR
00011 *                        PROGRAM ABSTRACT                         ELTSURGR
00012 *                                                                 ELTSURGR
00013 *   PROGRAM NAME:   E.L.S. SURGERY TOPIC                          ELTSURGR
00014 *                                                                 ELTSURGR
00015 *   PROGRAM I.D.:   ELTSURGR                                      ELTSURGR
00016 *                                                                 ELTSURGR
00017 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE SURGICALELTSURGR
00018 *              BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTSURGR
00019 *                                                                 ELTSURGR
00020 *   OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF SURGICAL COVERAGE ELTSURGR
00021 *              AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTSURGR
00022 *              GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTSURGR
00023 *              THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTSURGR
00024 *              RANGE OF DATES.                                    ELTSURGR
00025 *                                                                 ELTSURGR
00026 *   RECORDS                                                       ELTSURGR
00027 *   ACCESSED:  GROUP SPECIFIC, VARIOUS BENEFIT PROVISION, AND A   ELTSURGR
00028 *            LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTSURGR
00029 *                                                                 ELTSURGR
00030 ***************************************************************** ELTSURGR
00031     TITLE 'ELS SURGERY TOPIC   HISTORY'.                          ELTSURGR
00032 ***************************************************************** ELTSURGR
00033 *                    U P D A T E   H I S T O R Y                * ELTSURGR
00034 *                                                               * ELTSURGR
00035 * CHG NUM   DATE    PGM  DESCRIPTION                            * ELTSURGR
00036 * ------- --------  ---  -------------------------------------- * ELTSURGR
00037 *  178    07/02/86  LET  DISCREPANCY #178.  ADDED PROFESSIONAL  * ELTSURGR
00038 *                        CHARGES ON A HOSPITAL CLAIM ELEMENT.   * ELTSURGR
00039 *                                                               * ELTSURGR
00040 *  T0723  08/14/86  LET  USING THE 1ST HEADER LINE FROM THE     * ELTSURGR
00041 *                        PROLOG ON THE TOPIC SCREENS.           * ELTSURGR
00042 *                                                               * ELTSURGR
00043 *  XXXXX  10/09/86  NAC  VS COBOL II CONVERSION.                * ELTSURGR
00044 *  XXXXX  10/21/87  EGL  CHANGED FIXED TEXT.                    * ELTSURGR
00045 *  XXXXX  10/10/89  RKH  ADDED TRANSFER TO OTHER RESPONS IND    * ELTSURGR
00046 *         08/28/90  GEM  ADDED/DELETED BENEFIT PROVISION IDS.   * ELTSURGR
00047 *         11/16/90  GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND COM- * ELTSURGR
00048 *                        PARE TO THE LITERAL ZERO.              * ELTSURGR
00049 *         03/12/92  AKK  SPILLOVER DED WAS BEING ASKED FOR IN-  * ELTSURGR
00050 *                        STEAD OF TRANSFER OTHER RESPONSIBILITY * ELTSURGR
00051 *                        IND IN SECTION 7100                    * ELTSURGR
00052 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTSURGR
00053 ***************************************************************** ELTSURGR
00054 /                                                                 ELTSURGR
00055  ENVIRONMENT DIVISION.                                            ELTSURGR
00056      SKIP3                                                        ELTSURGR
00057  DATA DIVISION.                                                   ELTSURGR
00058      TITLE 'WORKING STORAGE ---- ELTSURGR'.                       ELTSURGR
00059  WORKING-STORAGE SECTION.                                         ELTSURGR
00060  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTSURGR
00061      '***ELTSURGR WS BEGINS***'.                                  ELTSURGR
00062 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTSURGR
00063  01  WS-WORK-FIELDS.                                              ELTSURGR
00064      05  WS-HEX-00                     PIC X.                     ELTSURGR
00065      05  WS-CHAR-0                     PIC X.                     ELTSURGR
00066      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTSURGR
00067      05  WS-YES                        PIC X(01) VALUE 'Y'.       ELTSURGR
00068      05  WS-NO                         PIC X(01) VALUE 'N'.       ELTSURGR
00069      05  WS-SUB                        PIC S999  COMP VALUE +0.   ELTSURGR
00070      05  WS-SUB1                       PIC S999  COMP VALUE +0.   ELTSURGR
00071      05  WS-SUB2                       PIC S999  COMP VALUE +0.   ELTSURGR
00072      05  WS-SUB3                       PIC S999  COMP VALUE +0.   ELTSURGR
00073      05  WS-SUB4                       PIC S999  COMP VALUE +0.   ELTSURGR
00074      05  WS-CIA                        PIC S999  COMP VALUE +0.   ELTSURGR
00075      05  WS-FIRSTTIME-IND              PIC X.                     ELTSURGR
00076        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTSURGR
00077      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTSURGR
00078        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTSURGR
00079      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTSURGR
00080        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTSURGR
00081      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP.             ELTSURGR
00082      05  WS-PERCENT-FLD.                                          ELTSURGR
00083        10  WS-PERCENTAGE               PIC ZZ9.                   ELTSURGR
00084        10  WS-PERCENT-SIGN             PIC X.                     ELTSURGR
00085      TITLE 'BENEFIT PROVISIONS BY TYPE -- ELTSURGR'.              ELTSURGR
00086  01  TABLE-MAX                   PIC S9(03) VALUE +10 COMP.       ELTSURGR
00087 * 10 REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTSURGR
00088                                                                   ELTSURGR
00089  01  WS-BEN-PROV-ID.                                              ELTSURGR
00090      05  WS-INST-IP-CNT                PIC S999 COMP    VALUE +6. ELTSURGR
00091      05  WS-INST-IP-TAB.                                          ELTSURGR
00092        10  FILLER                      PIC X(6)  VALUE 'ORGI W'.  ELTSURGR
00093        10  FILLER                      PIC X(6)  VALUE 'CIRC B'.  ELTSURGR
00094        10  FILLER                      PIC X(6)  VALUE 'EXPI B'.  ELTSURGR
00095        10  FILLER                      PIC X(6)  VALUE 'ISII B'.  ELTSURGR
00096        10  FILLER                      PIC X(6)  VALUE 'SSCI B'.  ELTSURGR
00097        10  FILLER                      PIC X(6)  VALUE 'PPFI B'.  ELTSURGR
00098      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTSURGR
00099                                        PIC X(6)  OCCURS 6 TIMES.  ELTSURGR
00100                                                                   ELTSURGR
00101      05  WS-INST-OP-CNT                PIC S999 COMP    VALUE +10.ELTSURGR
00102      05  WS-INST-OP-TAB.                                          ELTSURGR
00103        10  FILLER                      PIC X(6)  VALUE 'OPSG W'.  ELTSURGR
00104        10  FILLER                      PIC X(6)  VALUE 'CIRC B'.  ELTSURGR
00105        10  FILLER                      PIC X(6)  VALUE 'ISIO B'.  ELTSURGR
00106        10  FILLER                      PIC X(6)  VALUE 'OPSG B'.  ELTSURGR
00107        10  FILLER                      PIC X(6)  VALUE 'ORGO B'.  ELTSURGR
00108        10  FILLER                      PIC X(6)  VALUE 'ORGO W'.  ELTSURGR
00109        10  FILLER                      PIC X(6)  VALUE 'SSCO B'.  ELTSURGR
00110        10  FILLER                      PIC X(6)  VALUE 'EASS B'.  ELTSURGR
00111        10  FILLER                      PIC X(6)  VALUE 'EMSS B'.  ELTSURGR
00112        10  FILLER                      PIC X(6)  VALUE 'PPFO B'.  ELTSURGR
00113      05  WS-INST-OP-LIST     REDEFINES    WS-INST-OP-TAB          ELTSURGR
00114                                        PIC X(6)  OCCURS 10 TIMES. ELTSURGR
00115                                                                   ELTSURGR
00116      05  WS-PROF-IP-CNT                PIC S999 COMP    VALUE +7. ELTSURGR
00117      05  WS-PROF-IP-TAB.                                          ELTSURGR
00118        10  FILLER                      PIC X(6)  VALUE 'SRGI C'.  ELTSURGR
00119        10  FILLER                      PIC X(6)  VALUE 'ISII E'.  ELTSURGR
00120        10  FILLER                      PIC X(6)  VALUE 'ORGI C'.  ELTSURGR
00121        10  FILLER                      PIC X(6)  VALUE 'ASEM C'.  ELTSURGR
00122        10  FILLER                      PIC X(6)  VALUE 'INVI C'.  ELTSURGR
00123        10  FILLER                      PIC X(6)  VALUE 'NCRC C'.  ELTSURGR
00124        10  FILLER                      PIC X(6)  VALUE 'PTI  E'.  ELTSURGR
00125      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTSURGR
00126                                        PIC X(6)  OCCURS 7 TIMES.  ELTSURGR
00127                                                                   ELTSURGR
00128      05  WS-PROF-OP-CNT                PIC S999 COMP    VALUE +9. ELTSURGR
00129      05  WS-PROF-OP-TAB.                                          ELTSURGR
00130        10  FILLER                      PIC X(6)  VALUE 'SRGO C'.  ELTSURGR
00131        10  FILLER                      PIC X(6)  VALUE 'ISIO E'.  ELTSURGR
00132        10  FILLER                      PIC X(6)  VALUE 'ORGO C'.  ELTSURGR
00133        10  FILLER                      PIC X(6)  VALUE 'SRO  C'.  ELTSURGR
00134        10  FILLER                      PIC X(6)  VALUE 'ASEM C'.  ELTSURGR
00135        10  FILLER                      PIC X(6)  VALUE 'INVI C'.  ELTSURGR
00136        10  FILLER                      PIC X(6)  VALUE 'NCRC C'.  ELTSURGR
00137        10  FILLER                      PIC X(6)  VALUE 'PTO  E'.  ELTSURGR
00138        10  FILLER                      PIC X(6)  VALUE 'SCSO E'.  ELTSURGR
00139      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTSURGR
00140                                        PIC X(6)  OCCURS 9 TIMES.  ELTSURGR
00141                                                                   ELTSURGR
00142      TITLE 'DISPLAT LINES ------- ELTSURGR'.                      ELTSURGR
00143  01  WS-ELS-DISPLAY-LINES.                                        ELTSURGR
00144    05  WS-HDR-2-PROF-IP.                                          ELTSURGR
00145      10  FILLER                    PIC X(21) VALUE SPACES.        ELTSURGR
00146      10  FILLER                    PIC X(30)                      ELTSURGR
00147          VALUE 'SURGERY INPATIENT PROFESSIONAL'.                  ELTSURGR
00148      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTSURGR
00149                                                                   ELTSURGR
00150    05  WS-HDR-2-PROF-OP.                                          ELTSURGR
00151      10  FILLER                    PIC X(21) VALUE SPACES.        ELTSURGR
00152      10  FILLER                    PIC X(31)                      ELTSURGR
00153          VALUE 'SURGERY OUTPATIENT PROFESSIONAL'.                 ELTSURGR
00154      10  FILLER                    PIC X(27) VALUE LOW-VALUES.    ELTSURGR
00155                                                                   ELTSURGR
00156    05  WS-HDR-2-INST-IP.                                          ELTSURGR
00157      10  FILLER                    PIC X(21) VALUE SPACES.        ELTSURGR
00158      10  FILLER                    PIC X(31)                      ELTSURGR
00159          VALUE 'SURGERY INPATIENT INSTITUTIONAL'.                 ELTSURGR
00160      10  FILLER                    PIC X(27) VALUE LOW-VALUES.    ELTSURGR
00161                                                                   ELTSURGR
00162    05  WS-HDR-2-INST-OP.                                          ELTSURGR
00163      10  FILLER                    PIC X(21) VALUE SPACES.        ELTSURGR
00164      10  FILLER                    PIC X(32)                      ELTSURGR
00165          VALUE 'SURGERY OUTPATIENT INSTITUTIONAL'.                ELTSURGR
00166      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTSURGR
00167                                                                   ELTSURGR
00168    05  WS-HDR-2-COINS-BEN-LVL.                                    ELTSURGR
00169      10  FILLER                    PIC X(30) VALUE SPACES.        ELTSURGR
00170      10  FILLER                    PIC X(28)                      ELTSURGR
00171          VALUE 'COINSURANCE AT BENEFIT LEVEL'.                    ELTSURGR
00172      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTSURGR
00173                                                                   ELTSURGR
00174    05  WS-SEE-CCP.                                                ELTSURGR
00175      10  FILLER                    PIC X(31)                      ELTSURGR
00176          VALUE 'SEE COST CONTAINMENT TOPIC FOR '.                 ELTSURGR
00177      10  FILLER                    PIC X(22)                      ELTSURGR
00178          VALUE 'ADDITIONAL LIMITATIONS'.                          ELTSURGR
00179      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTSURGR
00180                                                                   ELTSURGR
00181    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTSURGR
00182          VALUE 'SERVICES MAY BE RENDERED '.                       ELTSURGR
00183                                                                   ELTSURGR
00184    05  WS-FOLLOWING-BEN.                                          ELTSURGR
00185      10  FILLER                  PIC  X(22) VALUE                 ELTSURGR
00186            'COVERED SERVICES ARE: '.                              ELTSURGR
00187                                                                   ELTSURGR
00188    05  WS-PAYMNT-BASED.                                           ELTSURGR
00189      10  FILLER                  PIC  X(20) VALUE                 ELTSURGR
00190          'PAYMENT IS BASED ON '.                                  ELTSURGR
00191                                                                   ELTSURGR
00192    05  WS-BASIC.                                                  ELTSURGR
00193      10  WS-BASIC-LIT              PIC X(16)                      ELTSURGR
00194          VALUE '         BASIC: '.                                ELTSURGR
00195      10  WS-DTL-BASIC              PIC X(50) VALUE SPACES.        ELTSURGR
00196      10  FILLER                    PIC X(13) VALUE LOW-VALUES.    ELTSURGR
00197                                                                   ELTSURGR
00198    05  WS-SUPPLEMENTAL.                                           ELTSURGR
00199      10  WS-SUPP-LIT               PIC X(16)                      ELTSURGR
00200          VALUE '  SUPPLEMENTAL: '.                                ELTSURGR
00201      10  WS-DTL-SUPPLEMENTAL       PIC X(50) VALUE SPACES.        ELTSURGR
00202      10  FILLER                    PIC X(3)  VALUE LOW-VALUES.    ELTSURGR
00203                                                                   ELTSURGR
00204    05  WS-PAYABLE-AS.                                             ELTSURGR
00205      10  FILLER                  PIC  X(40) VALUE                 ELTSURGR
00206          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTSURGR
00207                                                                   ELTSURGR
00208                                                                   ELTSURGR
00209    05  WS-PROF-INPT-CHRGES         PIC  X(61) VALUE               ELTSURGR
00210            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTSURGR
00211 -          'EPORT: '.                                             ELTSURGR
00212                                                                   ELTSURGR
00213    05  WS-PROF-OUTPT-CHRGES        PIC  X(62) VALUE               ELTSURGR
00214            'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE ELTSURGR
00215 -          'REPORT: '.                                            ELTSURGR
00216                                                                   ELTSURGR
00217    05  WS-SEX-CHANGE.                                             ELTSURGR
00218      10  FILLER                    PIC X(31)                      ELTSURGR
00219          VALUE 'SERVICES RELATED TO SEX CHANGE '.                 ELTSURGR
00220      10  FILLER                    PIC X(48) VALUE LOW-VALUES.    ELTSURGR
00221                                                                   ELTSURGR
00222    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER'.  ELTSURGR
00223                                                                   ELTSURGR
00224    05  WS-BASIC-INDEMNITY.                                        ELTSURGR
00225      10  FILLER                    PIC X(49)                      ELTSURGR
00226        VALUE 'BASIC INDEMNITY EXCESS SPILLS TO SUPPLEMENTAL MM '. ELTSURGR
00227      10  WS-DTL-BASIC-INDEMNITY    PIC X(27) VALUE SPACES.        ELTSURGR
00228      10  FILLER                    PIC X(3) VALUE LOW-VALUES.     ELTSURGR
00229                                                                   ELTSURGR
00230    05  WS-INELIG-SEX-CHANGE.                                      ELTSURGR
00231      10  FILLER                    PIC X(48)                      ELTSURGR
00232        VALUE 'IF SEX CHANGE CHARGES ARE INELIGIBLE, REMAINING '.  ELTSURGR
00233      10  FILLER                    PIC X(8) VALUE 'CHARGES '.     ELTSURGR
00234      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTSURGR
00235                                                                   ELTSURGR
00236    05  WS-STANDARD-RM-BOARD.                                      ELTSURGR
00237      10  FILLER                    PIC X(51)                      ELTSURGR
00238       VALUE 'STANDARDLY ROOM AND BOARD IS COVERED IF SURGERY IS '.ELTSURGR
00239      10  FILLER                    PIC X(28)                      ELTSURGR
00240        VALUE 'ELIGIBLE-SEE ROOM AND BOARD '.                      ELTSURGR
00241                                                                   ELTSURGR
00242    05  WS-CONTRACT-RELATED.                                       ELTSURGR
00243      10  FILLER                    PIC X(50)   VALUE              ELTSURGR
00244         'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.     ELTSURGR
00245                                                                   ELTSURGR
00246    05  WS-SURGICAL-TOPIC.                                         ELTSURGR
00247      10  FILLER                    PIC X(23)                      ELTSURGR
00248        VALUE 'SURGICAL BENEFIT TOPIC '.                           ELTSURGR
00249      10  FILLER                    PIC X(54) VALUE LOW-VALUES.    ELTSURGR
00250                                                                   ELTSURGR
00251    05  WS-NO-TABULAR1.                                            ELTSURGR
00252      10  FILLER                    PIC X(51)  VALUE               ELTSURGR
00253         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTSURGR
00254      10  FILLER                    PIC X(22)  VALUE               ELTSURGR
00255         'GOING FROM BENEFIT ***'.                                 ELTSURGR
00256                                                                   ELTSURGR
00257    05  WS-NO-TABULAR2.                                            ELTSURGR
00258      10  FILLER                    PIC X(15)  VALUE               ELTSURGR
00259         '*** PROVISION: '.                                        ELTSURGR
00260      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTSURGR
00261      10  FILLER                    PIC X VALUE SPACE.             ELTSURGR
00262      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTSURGR
00263      10  FILLER                    PIC X(13)  VALUE               ELTSURGR
00264         ' TO TABULAR: '.                                          ELTSURGR
00265      10  WS-NO-TAB-ID              PIC X(6).                      ELTSURGR
00266      10  FILLER                    PIC X VALUE SPACE.             ELTSURGR
00267      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTSURGR
00268      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTSURGR
00269                                                                   ELTSURGR
00270    05  WS-PGM-ERROR.                                              ELTSURGR
00271      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTSURGR
00272      10  FILLER                    PIC X(35)  VALUE               ELTSURGR
00273         '***  P R O G R A M   E R R O R  ***'.                    ELTSURGR
00274      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTSURGR
00275                                                                   ELTSURGR
00276    05  WS-BAD-INST-PROF-SEL.                                      ELTSURGR
00277      10  FILLER                    PIC XX VALUE SPACE.            ELTSURGR
00278      10  FILLER                    PIC X(47) VALUE                ELTSURGR
00279         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTSURGR
00280      10  FILLER                    PIC X(48) VALUE                ELTSURGR
00281         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTSURGR
00282      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTSURGR
00283                                                                   ELTSURGR
00284    05  WS-BAD-IN-OUT-SEL.                                         ELTSURGR
00285      10  FILLER                    PIC X(08) VALUE SPACE.         ELTSURGR
00286      10  FILLER                    PIC X(51) VALUE                ELTSURGR
00287         '*** I N V A L I D   I N P U T   /   O U T P U T ***'.    ELTSURGR
00288      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTSURGR
00289                                                                   ELTSURGR
00290    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTSURGR
00291       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTSURGR
00292                                                                   ELTSURGR
00293    05  WS-PVE.                                                    ELTSURGR
00294      10  FILLER                    PIC X(44)                      ELTSURGR
00295       VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.       ELTSURGR
00296                                                                   ELTSURGR
00297    05  WS-ACCUM-MSG1.                                             ELTSURGR
00298      10  FILLER                  PIC  X(79) VALUE                 ELTSURGR
00299      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTSURGR
00300 -    'CONSIDERATIONS.'.                                           ELTSURGR
00301                                                                   ELTSURGR
00302    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTSURGR
00303    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTSURGR
00304      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTSURGR
00305                                                                   ELTSURGR
00306  01  WS-END                            PIC X(16)  VALUE           ELTSURGR
00307      '*** W/S ENDS ***'.                                          ELTSURGR
00308      TITLE 'LINKAGE SECTION ----- ELTSURGR'.                      ELTSURGR
00309  LINKAGE SECTION.                                                 ELTSURGR
00310  01  DFHCOMMAREA.                                                 ELTSURGR
00311      COPY ELSCOMMC.                                               ELTSURGR
00312 /**************************************************************** ELTSURGR
00313 *    ----->        C I A         AREA                           * ELTSURGR
00314 ***************************************************************** ELTSURGR
00315      COPY ELSCIA2C.                                               ELTSURGR
00316 /**************************************************************** ELTSURGR
00317 *    ----->        I/O PARM AREA                                * ELTSURGR
00318 ***************************************************************** ELTSURGR
00319      COPY ELSIOPMC.                                               ELTSURGR
00320 /**************************************************************** ELTSURGR
00321 *    ----->        KEY      AREA                                * ELTSURGR
00322 ***************************************************************** ELTSURGR
00323      COPY ELSKEYSC.                                               ELTSURGR
00324 /**************************************************************** ELTSURGR
00325 *    ----->        OUTPUT TEXT AREA                             * ELTSURGR
00326 ***************************************************************** ELTSURGR
00327      COPY ELSOUTPC.                                               ELTSURGR
00328 /**************************************************************** ELTSURGR
00329 *    ----->        TOPIC SELECTION AREA                         * ELTSURGR
00330 ***************************************************************** ELTSURGR
00331      COPY ELSSSCBC.                                               ELTSURGR
00332 /**************************************************************** ELTSURGR
00333 *    ----->        CODES MANUAL INTERFACE                       * ELTSURGR
00334 ***************************************************************** ELTSURGR
00335      COPY ELSCMIFC.                                               ELTSURGR
00336 /**************************************************************** ELTSURGR
00337 *    ----->        CODES MANUAL DESCRIPTION AREA                * ELTSURGR
00338 ***************************************************************** ELTSURGR
00339      COPY ELSCMDSC.                                               ELTSURGR
00340 /**************************************************************** ELTSURGR
00341 *    ----->        BENEFIT PROVISION TABLE                      * ELTSURGR
00342 ***************************************************************** ELTSURGR
00343      COPY ELSPRVNC.                                               ELTSURGR
00344 /**************************************************************** ELTSURGR
00345 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTSURGR
00346 ***************************************************************** ELTSURGR
00347      COPY ELSPLGSW.                                               ELTSURGR
00348 /**************************************************************** ELTSURGR
00349 *** BENEFIT PROVISION TABLE OF FLDS                               ELTSURGR
00350 ***************************************************************** ELTSURGR
00351      COPY ELSPLGTB.                                               ELTSURGR
00352 /**************************************************************** ELTSURGR
00353 *** ---->    TEXT COMPRESSION TABLE                               ELTSURGR
00354 ***************************************************************** ELTSURGR
00355      COPY ELSTCWAC.                                               ELTSURGR
00356 /**************************************************************** ELTSURGR
00357 *        G R O U P   S P E C I F I C   R E C O R D                ELTSURGR
00358 ***************************************************************** ELTSURGR
00359  01  GROUP-SPECIFIC-RECORD.                                       ELTSURGR
00360      COPY GCGROUPC.                                               ELTSURGR
00361      TITLE ' PROCEDURE DIVISION  ---  SURGERY TOPIC'.             ELTSURGR
00362  PROCEDURE DIVISION.                                              ELTSURGR
00363                                                                   ELTSURGR
00364 ******************************************************************ELTSURGR
00365 *                                                                 ELTSURGR
00366 *   PERFORM THE MAINLINE OPERATIONS.                              ELTSURGR
00367 *                                                                 ELTSURGR
00368 ******************************************************************ELTSURGR
00369  0000-MAINLINE.                                                   ELTSURGR
00370                                                                   ELTSURGR
00371                                                                   ELTSURGR
00372      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTSURGR
00373          EXEC CICS ABEND                                          ELTSURGR
00374                    ABCODE ('EL01')                                ELTSURGR
00375          END-EXEC                                                 ELTSURGR
00376      END-IF.                                                      ELTSURGR
00377                                                                   ELTSURGR
00378 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTSURGR
00379                                                                   ELTSURGR
00380      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTSURGR
00381          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTSURGR
00382                                                                   ELTSURGR
00383      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTSURGR
00384      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00385          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTSURGR
00386                                                                   ELTSURGR
00387      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTSURGR
00388      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00389          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTSURGR
00390                                                                   ELTSURGR
00391      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTSURGR
00392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00393          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTSURGR
00394                                                                   ELTSURGR
00395      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTSURGR
00396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00397          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTSURGR
00398                                                                   ELTSURGR
00399      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTSURGR
00400      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00401          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTSURGR
00402                                                                   ELTSURGR
00403      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTSURGR
00404      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00405          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTSURGR
00406                                                                   ELTSURGR
00407      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTSURGR
00408                                                                   ELTSURGR
00409      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTSURGR
00410              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTSURGR
00411                                                                   ELTSURGR
00412      SET CIA-STG-GETMAIN TO TRUE.                                 ELTSURGR
00413      EXEC CICS LINK                                               ELTSURGR
00414                PROGRAM('ELUSTGMG')                                ELTSURGR
00415                COMMAREA(DFHCOMMAREA)                              ELTSURGR
00416      END-EXEC.                                                    ELTSURGR
00417                                                                   ELTSURGR
00418      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTSURGR
00419      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00420          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTSURGR
00421                                                                   ELTSURGR
00422                                                                   ELTSURGR
00423      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTSURGR
00424                                                                   ELTSURGR
00425      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTSURGR
00426                       AND                                         ELTSURGR
00427         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTSURGR
00428         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTSURGR
00429                                                                   ELTSURGR
00430      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTSURGR
00431                       AND                                         ELTSURGR
00432         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTSURGR
00433         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTSURGR
00434                                                                   ELTSURGR
00435      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTSURGR
00436                       AND                                         ELTSURGR
00437         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTSURGR
00438         PERFORM 3000-INSTITUTIONAL-OP-RTNE.                       ELTSURGR
00439                                                                   ELTSURGR
00440      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTSURGR
00441                       AND                                         ELTSURGR
00442         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTSURGR
00443         PERFORM 4000-PROFESSIONAL-OP-RTNE.                        ELTSURGR
00444                                                                   ELTSURGR
00445      IF (SSB-PROV-CLASS-INST OR                                   ELTSURGR
00446          SSB-PROV-CLASS-BOTH OR                                   ELTSURGR
00447          SSB-PROV-CLASS-PROF)                                     ELTSURGR
00448                         AND                                       ELTSURGR
00449         (SSB-SERV-CLASS-IP   OR                                   ELTSURGR
00450          SSB-SERV-CLASS-OP   OR                                   ELTSURGR
00451          SSB-SERV-CLASS-BOTH)                                     ELTSURGR
00452            CONTINUE                                               ELTSURGR
00453      ELSE                                                         ELTSURGR
00454          SET CIA-AB-UNDEF TO TRUE                                 ELTSURGR
00455          EXEC CICS ABEND                                          ELTSURGR
00456                    ABCODE(CIA-ABCODE)                             ELTSURGR
00457          END-EXEC                                                 ELTSURGR
00458      END-IF.                                                      ELTSURGR
00459                                                                   ELTSURGR
00460      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTSURGR
00461                                                                   ELTSURGR
00462      SET CIA-STG-FREEMAIN TO TRUE.                                ELTSURGR
00463      EXEC CICS LINK                                               ELTSURGR
00464                PROGRAM('ELUSTGMG')                                ELTSURGR
00465                COMMAREA(DFHCOMMAREA)                              ELTSURGR
00466      END-EXEC.                                                    ELTSURGR
00467                                                                   ELTSURGR
00468      MOVE 'E'   TO  COF-FUNCTION.                                 ELTSURGR
00469      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTSURGR
00470                                                                   ELTSURGR
00471      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
00472                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00473                     END-EXEC.                                     ELTSURGR
00474                                                                   ELTSURGR
00475      EXEC CICS RETURN END-EXEC.                                   ELTSURGR
00476                                                                   ELTSURGR
00477                                                                   ELTSURGR
00478      GOBACK.                                                      ELTSURGR
00479      TITLE 'INSTITUTIONAL INPATIENT'.                             ELTSURGR
00480 ***************************************************************** ELTSURGR
00481 *        I N S T I T U T I O N A L   I P   R T N E                ELTSURGR
00482 *                                                                 ELTSURGR
00483 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTSURGR
00484 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTSURGR
00485 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTSURGR
00486 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTSURGR
00487 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTSURGR
00488 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTSURGR
00489 *  MODULE.                                                        ELTSURGR
00490 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTSURGR
00491 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTSURGR
00492 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTSURGR
00493 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTSURGR
00494 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTSURGR
00495 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTSURGR
00496 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTSURGR
00497 *                                                                 ELTSURGR
00498 ***************************************************************** ELTSURGR
00499  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTSURGR
00500      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSURGR
00501                                                                   ELTSURGR
00502      MOVE WS-HDR-2-INST-IP    TO  COF-HDR-LINE (2).               ELTSURGR
00503                                                                   ELTSURGR
00504      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTSURGR
00505      MOVE 'P'            TO  COF-FUNCTION.                        ELTSURGR
00506                                                                   ELTSURGR
00507      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
00508                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00509                     END-EXEC.                                     ELTSURGR
00510                                                                   ELTSURGR
00511      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
00512      PERFORM WITH TEST BEFORE                                     ELTSURGR
00513              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSURGR
00514              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSURGR
00515         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSURGR
00516         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSURGR
00517         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSURGR
00518         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSURGR
00519      END-PERFORM.                                                 ELTSURGR
00520      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
00521                                                                   ELTSURGR
00522                                                                   ELTSURGR
00523      PERFORM WITH TEST BEFORE                                     ELTSURGR
00524         VARYING WS-SUB FROM +1 BY +1                              ELTSURGR
00525         UNTIL   WS-SUB  >     WS-INST-IP-CNT                      ELTSURGR
00526           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTSURGR
00527           MOVE WS-INST-IP-LIST (WS-SUB)                           ELTSURGR
00528                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTSURGR
00529            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTSURGR
00530                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTSURGR
00531      END-PERFORM.                                                 ELTSURGR
00532                                                                   ELTSURGR
00533      MOVE 'SURGICAL SERVICES     '  TO  SSB-TOPIC-PHRASE.         ELTSURGR
00534                                                                   ELTSURGR
00535                                                                   ELTSURGR
00536      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTSURGR
00537                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00538                     END-EXEC.                                     ELTSURGR
00539                                                                   ELTSURGR
00540      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
00541                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00542                     END-EXEC.                                     ELTSURGR
00543                                                                   ELTSURGR
00544      IF PVN-COVG-NONE                                             ELTSURGR
00545         GO TO 1099-EXIT.                                          ELTSURGR
00546                                                                   ELTSURGR
00547      MOVE +1  TO  WS-CIA.                                         ELTSURGR
00548                                                                   ELTSURGR
00549      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTSURGR
00550            PSP-PROVN-PRICING-METHD,                               ELTSURGR
00551            PSP-TRANSF-OTHER-RESP-IND,                             ELTSURGR
00552            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSURGR
00553            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSURGR
00554            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSURGR
00555            PSP-SPILL-OVER-DED-APL-IND,                            ELTSURGR
00556            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSURGR
00557            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSURGR
00558            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSURGR
00559            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSURGR
00560            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSURGR
00561            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSURGR
00562            PSB-PROF-CHRG-HSP-CLM,                                 ELTSURGR
00563            PSB-TRANSSXL-PMT-RESTR-OVRD,                           ELTSURGR
00564            PSW-TRNS-SEX-REST-OVRD-IND.                            ELTSURGR
00565                                                                   ELTSURGR
00566      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSURGR
00567                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00568                     END-EXEC.                                     ELTSURGR
00569                                                                   ELTSURGR
00570      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSURGR
00571      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
00572          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSURGR
00573                                                                   ELTSURGR
00574      PERFORM WITH TEST BEFORE                                     ELTSURGR
00575         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSURGR
00576         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTSURGR
00577              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTSURGR
00578              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTSURGR
00579                   PERFORM 1040-BUILD-SCREEN-LINES THRU 1040-EXIT  ELTSURGR
00580              END-IF                                               ELTSURGR
00581      END-PERFORM.                                                 ELTSURGR
00582                                                                   ELTSURGR
00583      GO TO 1099-EXIT.                                             ELTSURGR
00584                                                                   ELTSURGR
00585  1040-BUILD-SCREEN-LINES.                                         ELTSURGR
00586                                                                   ELTSURGR
00587      SET PLT-INDEX1  TO                                           ELTSURGR
00588                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTSURGR
00589      IF WS-NOT-FIRST-TIME                                         ELTSURGR
00590         MOVE 'P'  TO  COF-FUNCTION                                ELTSURGR
00591         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTSURGR
00592         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
00593             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
00594             END-EXEC                                              ELTSURGR
00595      ELSE                                                         ELTSURGR
00596         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTSURGR
00597                                                                   ELTSURGR
00598      MOVE 1  TO  WS-CIA.                                          ELTSURGR
00599      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
00600         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTSURGR
00601            SET PLT-INDEX2  TO  2                                  ELTSURGR
00602         ELSE                                                      ELTSURGR
00603            MOVE TABLE-MAX TO WS-SUB                               ELTSURGR
00604            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTSURGR
00605            GO TO 1040-EXIT                                        ELTSURGR
00606      ELSE                                                         ELTSURGR
00607         SET PLT-INDEX2  TO  1.                                    ELTSURGR
00608                                                                   ELTSURGR
00609 **---------------------------------------------------------------+ELTSURGR
00610 **                                                               |ELTSURGR
00611 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTSURGR
00612      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTSURGR
00613      ADD  +1  TO  WS-CIA.                                         ELTSURGR
00614      MOVE ZERO  TO  WS-SUB2.                                      ELTSURGR
00615      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTSURGR
00616      MOVE WS-NO   TO  WS-DISPLAY-B-FORMAT-TEXT.                   ELTSURGR
00617                                                                   ELTSURGR
00618      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTSURGR
00619         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTSURGR
00620         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.                ELTSURGR
00621      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSURGR
00622                                                                   ELTSURGR
00623      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTSURGR
00624      MOVE +1  TO  WS-CIA                                          ELTSURGR
00625      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
00626              END-EXEC.                                            ELTSURGR
00627 **                                                               |ELTSURGR
00628 **---------------------------------------------------------------+ELTSURGR
00629                                                                   ELTSURGR
00630 **---------------------------------------------------------------+ELTSURGR
00631 **                                                               |ELTSURGR
00632 **        P L A C E   O F   T R E A T M E N T                    |ELTSURGR
00633      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTSURGR
00634                                                              ZERO ELTSURGR
00635         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00636         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTSURGR
00637         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
00638                                                   CMF-CODE-VALUE  ELTSURGR
00639         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTSURGR
00640         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
00641         ADD +1  TO  WS-CIA.                                       ELTSURGR
00642 **                                                               |ELTSURGR
00643 **---------------------------------------------------------------+ELTSURGR
00644                                                                   ELTSURGR
00645 **---------------------------------------------------------------+ELTSURGR
00646 **                                                               |ELTSURGR
00647 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTSURGR
00648 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTSURGR
00649 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTSURGR
00650      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
00651      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
00652         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
00653                                                              '19' ELTSURGR
00654         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
00655         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
00656         ADD +1  TO  WS-CIA.                                       ELTSURGR
00657                                                                   ELTSURGR
00658      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
00659      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
00660         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
00661                                                        '19' AND   ELTSURGR
00662         NOT WS-ADD-A-BLANK-LINE                                   ELTSURGR
00663         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
00664         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
00665         ADD +1  TO  WS-CIA.                                       ELTSURGR
00666                                                                   ELTSURGR
00667      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
00668      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
00669         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
00670                           AND                                     ELTSURGR
00671         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
00672         SET  PLT-INDEX2  TO  2                                    ELTSURGR
00673         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
00674                                                              ZERO ELTSURGR
00675            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
00676            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
00677            ADD +1  TO  WS-CIA.                                    ELTSURGR
00678                                                                   ELTSURGR
00679      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
00680      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
00681         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
00682                           AND                                     ELTSURGR
00683         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTSURGR
00684         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTSURGR
00685         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTSURGR
00686         ADD +1  TO  WS-CIA.                                       ELTSURGR
00687                                                                   ELTSURGR
00688      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTSURGR
00689         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
00690         SET  PLT-INDEX2  TO  2                                    ELTSURGR
00691         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
00692                                                              ZERO ELTSURGR
00693            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
00694            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
00695            ADD +1  TO  WS-CIA.                                    ELTSURGR
00696                                                                   ELTSURGR
00697      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
00698      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
00699         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
00700                                                            =  ZEROELTSURGR
00701            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00702                                                            =  ZEROELTSURGR
00703               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
00704            ELSE                                                   ELTSURGR
00705               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
00706          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00707                                                  TO  WS-PERCENTAGEELTSURGR
00708         ELSE                                                      ELTSURGR
00709            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
00710          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00711                                                 TO  WS-PERCENTAGE.ELTSURGR
00712      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
00713         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
00714                                             ZERO AND  NOT =  '19' ELTSURGR
00715         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00716         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
00717         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTSURGR
00718                                               TO   CMF-CODE-VALUE ELTSURGR
00719         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
00720         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
00721         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
00722                                                                   ELTSURGR
00723      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
00724      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
00725         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTSURGR
00726                                                               ZEROELTSURGR
00727            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00728                                                             = ZEROELTSURGR
00729               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
00730            ELSE                                                   ELTSURGR
00731               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
00732          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00733                                                 TO   WS-PERCENTAGEELTSURGR
00734         ELSE                                                      ELTSURGR
00735            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
00736          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
00737                                                 TO  WS-PERCENTAGE.ELTSURGR
00738      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
00739         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
00740                                             ZERO AND  NOT =  '19' ELTSURGR
00741         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00742         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
00743         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
00744                                                    CMF-CODE-VALUE ELTSURGR
00745         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTSURGR
00746         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
00747         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
00748                                                                   ELTSURGR
00749      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
00750         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
00751         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
00752         MOVE +1  TO  WS-CIA                                       ELTSURGR
00753         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
00754             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
00755             END-EXEC.                                             ELTSURGR
00756 **                                                               |ELTSURGR
00757 **---------------------------------------------------------------+ELTSURGR
00758                                                                   ELTSURGR
00759 **---------------------------------------------------------------+ELTSURGR
00760 **        P R O F E S S I O N A L   C H A R G E S   O N          |ELTSURGR
00761 **                H O S P I T A L   B I L L                       ELTSURGR
00762                                                                   ELTSURGR
00763      SET PLT-INDEX2  TO  1.                                       ELTSURGR
00764                                                                   ELTSURGR
00765      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTSURGR
00766          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
00767              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
00768                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
00769                  MOVE WS-PROF-INPT-CHRGES                         ELTSURGR
00770                                 TO  COF-DTL-LINE (WS-CIA)         ELTSURGR
00771                  MOVE 'Y'       TO  WS-ADD-A-BLANK-IND            ELTSURGR
00772                  ADD  +1        TO  WS-CIA.                       ELTSURGR
00773                                                                   ELTSURGR
00774      SET PLT-INDEX2  TO  2.                                       ELTSURGR
00775                                                                   ELTSURGR
00776      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTSURGR
00777          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
00778                      AND                                          ELTSURGR
00779             NOT WS-ADD-A-BLANK-LINE                               ELTSURGR
00780              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
00781                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
00782                  MOVE WS-PROF-INPT-CHRGES                         ELTSURGR
00783                                 TO  COF-DTL-LINE (WS-CIA)         ELTSURGR
00784                  MOVE 'Y'       TO  WS-ADD-A-BLANK-IND            ELTSURGR
00785                  ADD  +1        TO  WS-CIA.                       ELTSURGR
00786                                                                   ELTSURGR
00787      SET PLT-INDEX2  TO  1.                                       ELTSURGR
00788                                                                   ELTSURGR
00789      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTSURGR
00790          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
00791              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
00792                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
00793                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTSURGR
00794                MOVE 'PROF-CHRG-HSP-CLM'                           ELTSURGR
00795                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTSURGR
00796                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTSURGR
00797                                   TO  CMF-CODE-VALUE              ELTSURGR
00798                MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA           ELTSURGR
00799                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTSURGR
00800                PERFORM 2100-CALL-CODES-MANUAL-LONG.               ELTSURGR
00801                                                                   ELTSURGR
00802      SET PLT-INDEX2  TO  2.                                       ELTSURGR
00803                                                                   ELTSURGR
00804      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTSURGR
00805          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
00806              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
00807                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
00808                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTSURGR
00809                MOVE 'PROF-CHRG-HSP-CLM'                           ELTSURGR
00810                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTSURGR
00811                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTSURGR
00812                                   TO  CMF-CODE-VALUE              ELTSURGR
00813                MOVE WS-SUPP-LIT   TO  WS-TEMP-TEXT-AREA           ELTSURGR
00814                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTSURGR
00815                PERFORM 2100-CALL-CODES-MANUAL-LONG.               ELTSURGR
00816                                                                   ELTSURGR
00817      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
00818          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTSURGR
00819          ADD  +1, WS-CIA  GIVING  COF-NBR-DTL-LINES               ELTSURGR
00820          MOVE +1   TO  WS-CIA                                     ELTSURGR
00821          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTSURGR
00822                         COMMAREA (DFHCOMMAREA)                    ELTSURGR
00823                         END-EXEC.                                 ELTSURGR
00824                                                                   ELTSURGR
00825 **---------------------------------------------------------------+ELTSURGR
00826 **                                                               |ELTSURGR
00827 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTSURGR
00828      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
00829      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
00830         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSURGR
00831                                                       NOT =  '0'  ELTSURGR
00832         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00833         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTSURGR
00834                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
00835         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSURGR
00836                                           TO   CMF-CODE-VALUE     ELTSURGR
00837         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
00838         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
00839         ADD +1  TO  WS-CIA.                                       ELTSURGR
00840 **                                                               |ELTSURGR
00841 **---------------------------------------------------------------+ELTSURGR
00842                                                                   ELTSURGR
00843 **---------------------------------------------------------------+ELTSURGR
00844 **                                                               |ELTSURGR
00845 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTSURGR
00846      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
00847      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
00848         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSURGR
00849                                                       NOT =  '0'  ELTSURGR
00850         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00851         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTSURGR
00852                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
00853         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTSURGR
00854                                                     CMF-CODE-VALUEELTSURGR
00855         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
00856         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
00857         ADD +1  TO  WS-CIA.                                       ELTSURGR
00858 **                                                               |ELTSURGR
00859 **---------------------------------------------------------------+ELTSURGR
00860                                                                   ELTSURGR
00861      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
00862         SET PLT-INDEX2  TO  2                                     ELTSURGR
00863      ELSE                                                         ELTSURGR
00864         SET PLT-INDEX2  TO  1.                                    ELTSURGR
00865                                                                   ELTSURGR
00866 **---------------------------------------------------------------+ELTSURGR
00867 **                                                               |ELTSURGR
00868 **        S T A N D A R D   R O O M   A N D   B O A R D          |ELTSURGR
00869      MOVE WS-STANDARD-RM-BOARD  TO  COF-DTL-LINE(WS-CIA)          ELTSURGR
00870      ADD +2,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTSURGR
00871      MOVE +1  TO  WS-CIA                                          ELTSURGR
00872      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
00873               END-EXEC.                                           ELTSURGR
00874 **                                                               |ELTSURGR
00875 **---------------------------------------------------------------+ELTSURGR
00876      PERFORM 7100-TRANSFER-OTHR-RESP.                             ELTSURGR
00877      PERFORM 7000-ALL-LEVEL-TABS.                                 ELTSURGR
00878                                                                   ELTSURGR
00879  1040-EXIT.  EXIT.                                                ELTSURGR
00880 /                                                                 ELTSURGR
00881  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTSURGR
00882                                                                   ELTSURGR
00883      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTSURGR
00884          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTSURGR
00885              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTSURGR
00886                                                                   ELTSURGR
00887      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTSURGR
00888         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
00889         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTSURGR
00890         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTSURGR
00891                                                   CMF-CODE-VALUE  ELTSURGR
00892         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTSURGR
00893         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
00894         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
00895         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTSURGR
00896         ADD  1  TO  WS-SUB2                                       ELTSURGR
00897         IF WS-CIA  >  20 OR  =  20                                ELTSURGR
00898            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTSURGR
00899            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTSURGR
00900                COMMAREA(DFHCOMMAREA)                              ELTSURGR
00901                END-EXEC                                           ELTSURGR
00902            MOVE +1  TO  WS-CIA.                                   ELTSURGR
00903                                                                   ELTSURGR
00904  1090-PROBLEM-WITH-INDICES.                                       ELTSURGR
00905                                                                   ELTSURGR
00906      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTSURGR
00907      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTSURGR
00908                                                                   ELTSURGR
00909      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSURGR
00910      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSURGR
00911                                                                   ELTSURGR
00912      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
00913              END-EXEC.                                            ELTSURGR
00914                                                                   ELTSURGR
00915  1099-EXIT.            EXIT.                                      ELTSURGR
00916      TITLE 'PROFESSIONAL INPATIENT'.                              ELTSURGR
00917 ***************************************************************** ELTSURGR
00918 *        P R O F E S S I O N A L   I P   R T N E                  ELTSURGR
00919 *                                                                 ELTSURGR
00920 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTSURGR
00921 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTSURGR
00922 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTSURGR
00923 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTSURGR
00924 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTSURGR
00925 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTSURGR
00926 *  MODULE.                                                        ELTSURGR
00927 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTSURGR
00928 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTSURGR
00929 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTSURGR
00930 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTSURGR
00931 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTSURGR
00932 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTSURGR
00933 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTSURGR
00934 *                                                                 ELTSURGR
00935 ***************************************************************** ELTSURGR
00936  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTSURGR
00937      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSURGR
00938                                                                   ELTSURGR
00939      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE (2).               ELTSURGR
00940                                                                   ELTSURGR
00941      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTSURGR
00942      MOVE 'P'            TO  COF-FUNCTION.                        ELTSURGR
00943                                                                   ELTSURGR
00944      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
00945                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00946                     END-EXEC.                                     ELTSURGR
00947                                                                   ELTSURGR
00948      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
00949      PERFORM WITH TEST BEFORE                                     ELTSURGR
00950              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSURGR
00951              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSURGR
00952         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSURGR
00953         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSURGR
00954         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSURGR
00955         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSURGR
00956      END-PERFORM.                                                 ELTSURGR
00957      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
00958                                                                   ELTSURGR
00959                                                                   ELTSURGR
00960      PERFORM WITH TEST BEFORE                                     ELTSURGR
00961         VARYING WS-SUB FROM +1 BY +1                              ELTSURGR
00962         UNTIL   WS-SUB  >     WS-PROF-IP-CNT                      ELTSURGR
00963           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTSURGR
00964           MOVE WS-PROF-IP-LIST (WS-SUB)                           ELTSURGR
00965                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTSURGR
00966            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTSURGR
00967                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTSURGR
00968      END-PERFORM.                                                 ELTSURGR
00969                                                                   ELTSURGR
00970      MOVE 'SURGICAL SERVICES     '  TO  SSB-TOPIC-PHRASE.         ELTSURGR
00971                                                                   ELTSURGR
00972                                                                   ELTSURGR
00973      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTSURGR
00974                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00975                     END-EXEC.                                     ELTSURGR
00976                                                                   ELTSURGR
00977      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
00978                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
00979                     END-EXEC.                                     ELTSURGR
00980                                                                   ELTSURGR
00981      IF PVN-COVG-NONE                                             ELTSURGR
00982         GO TO 2099-EXIT.                                          ELTSURGR
00983                                                                   ELTSURGR
00984      MOVE +1  TO  WS-CIA.                                         ELTSURGR
00985                                                                   ELTSURGR
00986      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTSURGR
00987            PSP-PROVN-PRICING-METHD,                               ELTSURGR
00988            PSP-TRANSF-OTHER-RESP-IND,                             ELTSURGR
00989            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSURGR
00990            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSURGR
00991            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSURGR
00992            PSP-SPILL-OVER-DED-APL-IND,                            ELTSURGR
00993            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSURGR
00994            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSURGR
00995            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSURGR
00996            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSURGR
00997            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSURGR
00998            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSURGR
00999            PSP-BEN-TAB-PROVN-ID-PVE,                              ELTSURGR
01000            PSC-BEN-SCOPE-ID,                                      ELTSURGR
01001            PSE-BEN-SCOPE-ID,                                      ELTSURGR
01002            PSB-TRANSSXL-PMT-RESTR-OVRD,                           ELTSURGR
01003            PSW-TRNS-SEX-REST-OVRD-IND.                            ELTSURGR
01004                                                                   ELTSURGR
01005      MOVE ZEROS  TO  PSB-PROF-CHRG-HSP-CLM.                       ELTSURGR
01006                                                                   ELTSURGR
01007      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSURGR
01008                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01009                     END-EXEC.                                     ELTSURGR
01010                                                                   ELTSURGR
01011      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSURGR
01012      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
01013          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSURGR
01014                                                                   ELTSURGR
01015      PERFORM WITH TEST BEFORE                                     ELTSURGR
01016         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSURGR
01017         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTSURGR
01018              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTSURGR
01019              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTSURGR
01020                   PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT  ELTSURGR
01021              END-IF                                               ELTSURGR
01022      END-PERFORM.                                                 ELTSURGR
01023                                                                   ELTSURGR
01024      GO TO 2099-EXIT.                                             ELTSURGR
01025                                                                   ELTSURGR
01026  2040-BUILD-SCREEN-LINES.                                         ELTSURGR
01027                                                                   ELTSURGR
01028      SET PLT-INDEX1   TO                                          ELTSURGR
01029                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTSURGR
01030      IF WS-NOT-FIRST-TIME                                         ELTSURGR
01031         MOVE 'P'  TO  COF-FUNCTION                                ELTSURGR
01032         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
01033             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
01034             END-EXEC                                              ELTSURGR
01035      ELSE                                                         ELTSURGR
01036         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTSURGR
01037                                                                   ELTSURGR
01038      MOVE +1  TO  WS-CIA.                                         ELTSURGR
01039      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
01040         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTSURGR
01041            SET PLT-INDEX2  TO  2                                  ELTSURGR
01042         ELSE                                                      ELTSURGR
01043            MOVE TABLE-MAX TO WS-SUB                               ELTSURGR
01044            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTSURGR
01045            GO TO 2040-EXIT                                        ELTSURGR
01046      ELSE                                                         ELTSURGR
01047         SET PLT-INDEX2  TO  1.                                    ELTSURGR
01048                                                                   ELTSURGR
01049 **---------------------------------------------------------------+ELTSURGR
01050 **                                                               |ELTSURGR
01051 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTSURGR
01052      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTSURGR
01053      ADD  +1  TO  WS-CIA.                                         ELTSURGR
01054      MOVE ZERO  TO  WS-SUB2.                                      ELTSURGR
01055      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTSURGR
01056      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTSURGR
01057         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTSURGR
01058         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTSURGR
01059      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSURGR
01060                                                                   ELTSURGR
01061      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTSURGR
01062      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
01063              END-EXEC.                                            ELTSURGR
01064      MOVE +1  TO  WS-CIA.                                         ELTSURGR
01065 **                                                               |ELTSURGR
01066 **---------------------------------------------------------------+ELTSURGR
01067                                                                   ELTSURGR
01068 **---------------------------------------------------------------+ELTSURGR
01069 **                                                               |ELTSURGR
01070 **        P L A C E   O F   T R E A T M E N T                    |ELTSURGR
01071      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
01072                                                              ZERO ELTSURGR
01073         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01074         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTSURGR
01075         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTSURGR
01076                                               TO  CMF-CODE-VALUE  ELTSURGR
01077         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTSURGR
01078         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
01079         ADD  +1  TO  WS-CIA.                                      ELTSURGR
01080 **                                                               |ELTSURGR
01081 **---------------------------------------------------------------+ELTSURGR
01082                                                                   ELTSURGR
01083 **---------------------------------------------------------------+ELTSURGR
01084 **                                                               |ELTSURGR
01085 **            B E N E F I T   S C O P E   I D                    |ELTSURGR
01086      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01087      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
01088         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
01089            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01090                                         '0000' AND  NOT =  '00  ' ELTSURGR
01091               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
01092               ADD  +1  TO  WS-CIA                                 ELTSURGR
01093               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
01094            ELSE                                                   ELTSURGR
01095               NEXT SENTENCE                                       ELTSURGR
01096         ELSE                                                      ELTSURGR
01097            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01098                                         '0000' AND  NOT =  '00  ' ELTSURGR
01099               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
01100               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
01101               ADD  +1  TO  WS-CIA.                                ELTSURGR
01102                                                                   ELTSURGR
01103      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01104      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01105         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
01106            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01107                                         '0000' AND  NOT =  '00  ' ELTSURGR
01108               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
01109               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
01110               ADD  +1  TO  WS-CIA                                 ELTSURGR
01111            ELSE                                                   ELTSURGR
01112               NEXT SENTENCE                                       ELTSURGR
01113         ELSE                                                      ELTSURGR
01114            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01115                                         '0000' AND  NOT =  '00  ' ELTSURGR
01116               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
01117               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
01118               ADD  +1  TO  WS-CIA.                                ELTSURGR
01119                                                                   ELTSURGR
01120      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01121      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
01122         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
01123            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01124                                         '0000' AND  NOT =  '00  ' ELTSURGR
01125               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTSURGR
01126               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
01127               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
01128                                                    CMF-CODE-VALUE ELTSURGR
01129               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTSURGR
01130               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
01131               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTSURGR
01132            ELSE                                                   ELTSURGR
01133               NEXT SENTENCE                                       ELTSURGR
01134         ELSE                                                      ELTSURGR
01135            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01136                                         '0000' AND  NOT =  '00  ' ELTSURGR
01137               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTSURGR
01138               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
01139               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
01140                                                    CMF-CODE-VALUE ELTSURGR
01141               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTSURGR
01142               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
01143               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSURGR
01144                                                                   ELTSURGR
01145      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01146         SET PLT-INDEX2  TO  2                                     ELTSURGR
01147         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
01148            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01149                                         '0000' AND  NOT =  '00  ' ELTSURGR
01150               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTSURGR
01151               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
01152               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
01153                                                    CMF-CODE-VALUE ELTSURGR
01154               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTSURGR
01155               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
01156               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTSURGR
01157            ELSE                                                   ELTSURGR
01158               NEXT SENTENCE                                       ELTSURGR
01159         ELSE                                                      ELTSURGR
01160            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
01161                                         '0000' AND  NOT =  '00  ' ELTSURGR
01162               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTSURGR
01163               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
01164               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
01165                                                    CMF-CODE-VALUE ELTSURGR
01166               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTSURGR
01167               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
01168               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSURGR
01169                                                                   ELTSURGR
01170      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
01171         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01172         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
01173         MOVE 1  TO  WS-CIA                                        ELTSURGR
01174         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
01175                COMMAREA(DFHCOMMAREA)                              ELTSURGR
01176                END-EXEC.                                          ELTSURGR
01177 **                                                               |ELTSURGR
01178 **---------------------------------------------------------------+ELTSURGR
01179                                                                   ELTSURGR
01180      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
01181         SET PLT-INDEX2  TO  2                                     ELTSURGR
01182      ELSE                                                         ELTSURGR
01183         SET PLT-INDEX2  TO  1.                                    ELTSURGR
01184                                                                   ELTSURGR
01185 **---------------------------------------------------------------+ELTSURGR
01186 **                                                               |ELTSURGR
01187 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTSURGR
01188 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTSURGR
01189 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTSURGR
01190      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01191      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01192         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01193                                                              '19' ELTSURGR
01194         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
01195         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01196         ADD +1  TO  WS-CIA.                                       ELTSURGR
01197                                                                   ELTSURGR
01198      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01199      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01200         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01201                                                        '19' AND   ELTSURGR
01202         NOT WS-ADD-A-BLANK-LINE                                   ELTSURGR
01203         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
01204         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01205         ADD +1  TO  WS-CIA.                                       ELTSURGR
01206                                                                   ELTSURGR
01207      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01208      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01209         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
01210                            AND                                    ELTSURGR
01211         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01212         SET  PLT-INDEX2  TO  2                                    ELTSURGR
01213         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
01214                                                             ZERO  ELTSURGR
01215            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
01216            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
01217            ADD +1  TO  WS-CIA.                                    ELTSURGR
01218                                                                   ELTSURGR
01219      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01220      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01221         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
01222                            AND                                    ELTSURGR
01223         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTSURGR
01224         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTSURGR
01225         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTSURGR
01226         ADD +1  TO  WS-CIA.                                       ELTSURGR
01227                                                                   ELTSURGR
01228      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTSURGR
01229         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01230         SET  PLT-INDEX2  TO  2                                    ELTSURGR
01231         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
01232                                                             ZERO  ELTSURGR
01233            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
01234            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
01235            ADD +1  TO  WS-CIA.                                    ELTSURGR
01236                                                                   ELTSURGR
01237      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01238      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
01239         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
01240                                                            =  ZEROELTSURGR
01241            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01242                                                            =  ZEROELTSURGR
01243               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
01244            ELSE                                                   ELTSURGR
01245               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
01246          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01247                                                  TO  WS-PERCENTAGEELTSURGR
01248         ELSE                                                      ELTSURGR
01249            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
01250          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01251                                                 TO  WS-PERCENTAGE.ELTSURGR
01252      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01253         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01254                                             ZERO AND  NOT =  '19' ELTSURGR
01255         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01256         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
01257         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
01258                                                    CMF-CODE-VALUE ELTSURGR
01259         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01260         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01261         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
01262                                                                   ELTSURGR
01263      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01264      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01265         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTSURGR
01266                                                               ZEROELTSURGR
01267            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01268                                                            =  ZEROELTSURGR
01269               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
01270            ELSE                                                   ELTSURGR
01271               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
01272          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01273                                                  TO  WS-PERCENTAGEELTSURGR
01274         ELSE                                                      ELTSURGR
01275            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
01276          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01277                                                 TO  WS-PERCENTAGE.ELTSURGR
01278      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01279         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01280                                             ZERO AND  NOT =  '19' ELTSURGR
01281         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01282         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
01283         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
01284                                                    CMF-CODE-VALUE ELTSURGR
01285         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTSURGR
01286         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01287         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
01288                                                                   ELTSURGR
01289      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
01290         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01291         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
01292         MOVE 1  TO  WS-CIA                                        ELTSURGR
01293         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
01294                COMMAREA(DFHCOMMAREA)                              ELTSURGR
01295                 END-EXEC.                                         ELTSURGR
01296 **                                                               |ELTSURGR
01297 **---------------------------------------------------------------+ELTSURGR
01298                                                                   ELTSURGR
01299      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
01300         SET PLT-INDEX2  TO  2                                     ELTSURGR
01301      ELSE                                                         ELTSURGR
01302         SET PLT-INDEX2  TO  1.                                    ELTSURGR
01303                                                                   ELTSURGR
01304                                                                   ELTSURGR
01305 **---------------------------------------------------------------+ELTSURGR
01306 **                                                               |ELTSURGR
01307 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTSURGR
01308      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01309      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01310         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSURGR
01311                                                         NOT =  '0'ELTSURGR
01312         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01313         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTSURGR
01314                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
01315         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSURGR
01316                                                TO  CMF-CODE-VALUE ELTSURGR
01317         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01318         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
01319 **                                                               |ELTSURGR
01320 **---------------------------------------------------------------+ELTSURGR
01321                                                                   ELTSURGR
01322 **---------------------------------------------------------------+ELTSURGR
01323 **                                                               |ELTSURGR
01324 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTSURGR
01325      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01326      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01327         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSURGR
01328                                                         NOT =  '0'ELTSURGR
01329         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01330         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTSURGR
01331                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
01332         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
01333                                                 TO  CMF-CODE-VALUEELTSURGR
01334         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01335         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
01336 **                                                               |ELTSURGR
01337 **---------------------------------------------------------------+ELTSURGR
01338                                                                   ELTSURGR
01339      PERFORM 7100-TRANSFER-OTHR-RESP.                             ELTSURGR
01340      PERFORM 7000-ALL-LEVEL-TABS.                                 ELTSURGR
01341                                                                   ELTSURGR
01342  2040-EXIT.  EXIT.                                                ELTSURGR
01343 /                                                                 ELTSURGR
01344                                                                   ELTSURGR
01345  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTSURGR
01346                                                                   ELTSURGR
01347      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTSURGR
01348         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01349         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTSURGR
01350         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTSURGR
01351                                                    CMF-CODE-VALUE ELTSURGR
01352         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTSURGR
01353         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01354         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
01355         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTSURGR
01356         ADD  1  TO  WS-SUB2                                       ELTSURGR
01357         IF WS-CIA  >  20 OR  =  20                                ELTSURGR
01358            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTSURGR
01359            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTSURGR
01360                COMMAREA(DFHCOMMAREA)                              ELTSURGR
01361                 END-EXEC                                          ELTSURGR
01362            MOVE +1  TO  WS-CIA.                                   ELTSURGR
01363                                                                   ELTSURGR
01364  2090-PROBLEM-WITH-INDICES.                                       ELTSURGR
01365                                                                   ELTSURGR
01366      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTSURGR
01367      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTSURGR
01368                                                                   ELTSURGR
01369      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSURGR
01370      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSURGR
01371                                                                   ELTSURGR
01372      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
01373              END-EXEC.                                            ELTSURGR
01374                                                                   ELTSURGR
01375  2099-EXIT.            EXIT.                                      ELTSURGR
01376                                                                   ELTSURGR
01377 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTSURGR
01378  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTSURGR
01379                                                                   ELTSURGR
01380                                                                   ELTSURGR
01381      INITIALIZE CMF-RETURN-CODE                                   ELTSURGR
01382                 TCAR-FROM-AREA.                                   ELTSURGR
01383                                                                   ELTSURGR
01384      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTSURGR
01385                       COMMAREA(DFHCOMMAREA)                       ELTSURGR
01386      END-EXEC.                                                    ELTSURGR
01387                                                                   ELTSURGR
01388      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTSURGR
01389      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
01390          ADDRESS OF CMF-DESCR.                                    ELTSURGR
01391                                                                   ELTSURGR
01392      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTSURGR
01393         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTSURGR
01394         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTSURGR
01395            CMF-DESCR-LINE(1),        ' ',                         ELTSURGR
01396            CMF-DESCR-LINE(2),        ' ',                         ELTSURGR
01397            CMF-DESCR-LINE(3)                                      ELTSURGR
01398            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTSURGR
01399      ELSE                                                         ELTSURGR
01400         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTSURGR
01401         STRING CMF-DESCR-LINE(1),        ' ',                     ELTSURGR
01402            CMF-DESCR-LINE(2),        ' ',                         ELTSURGR
01403            CMF-DESCR-LINE(3)                                      ELTSURGR
01404            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTSURGR
01405                                                                   ELTSURGR
01406      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSURGR
01407                                                                   ELTSURGR
01408      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTSURGR
01409      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTSURGR
01410      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTSURGR
01411      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSURGR
01412                                                                   ELTSURGR
01413      IF WS-MOVE-LINES-TO-CIA                                      ELTSURGR
01414         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTSURGR
01415            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTSURGR
01416                                             WS-TEMP-NOT-USED-CNT  ELTSURGR
01417            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTSURGR
01418               VARYING  WS-SUB1  FROM  1  BY  1                    ELTSURGR
01419               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTSURGR
01420            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTSURGR
01421            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTSURGR
01422            ADD +1  TO  WS-CIA                                     ELTSURGR
01423         ELSE                                                      ELTSURGR
01424            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTSURGR
01425            ADD +1  TO  WS-CIA.                                    ELTSURGR
01426                                                                   ELTSURGR
01427      IF WS-MOVE-LINES-TO-CIA                                      ELTSURGR
01428         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTSURGR
01429            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTSURGR
01430            ADD +1  TO  WS-CIA                                     ELTSURGR
01431         ELSE                                                      ELTSURGR
01432            NEXT SENTENCE                                          ELTSURGR
01433      ELSE                                                         ELTSURGR
01434         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTSURGR
01435                                                                   ELTSURGR
01436      GO TO 2199-EXIT.                                             ELTSURGR
01437                                                                   ELTSURGR
01438  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTSURGR
01439      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTSURGR
01440      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTSURGR
01441                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTSURGR
01442                                                                   ELTSURGR
01443  2199-EXIT.           EXIT.                                       ELTSURGR
01444                                                                   ELTSURGR
01445 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTSURGR
01446  2200-CODES-MANUAL-WITH-PERCENT SECTION.                          ELTSURGR
01447                                                                   ELTSURGR
01448      INITIALIZE CMF-RETURN-CODE                                   ELTSURGR
01449                 TCAR-FROM-AREA.                                   ELTSURGR
01450                                                                   ELTSURGR
01451      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTSURGR
01452                       COMMAREA(DFHCOMMAREA)                       ELTSURGR
01453      END-EXEC.                                                    ELTSURGR
01454                                                                   ELTSURGR
01455      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTSURGR
01456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
01457          ADDRESS OF CMF-DESCR.                                    ELTSURGR
01458                                                                   ELTSURGR
01459      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTSURGR
01460         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTSURGR
01461         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTSURGR
01462            CMF-DESCR-LINE(1),        ' ',                         ELTSURGR
01463            CMF-DESCR-LINE(2),        ' ',                         ELTSURGR
01464            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTSURGR
01465            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTSURGR
01466      ELSE                                                         ELTSURGR
01467         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTSURGR
01468         STRING CMF-DESCR-LINE(1),        ' ',                     ELTSURGR
01469            CMF-DESCR-LINE(2),        ' ',                         ELTSURGR
01470            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTSURGR
01471            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTSURGR
01472                                                                   ELTSURGR
01473      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSURGR
01474                                                                   ELTSURGR
01475      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTSURGR
01476      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTSURGR
01477      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTSURGR
01478                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTSURGR
01479                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTSURGR
01480      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSURGR
01481                                                                   ELTSURGR
01482      IF WS-MOVE-LINES-TO-CIA                                      ELTSURGR
01483         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTSURGR
01484            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTSURGR
01485                                             WS-TEMP-NOT-USED-CNT  ELTSURGR
01486            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTSURGR
01487               VARYING  WS-SUB1  FROM  1  BY  1                    ELTSURGR
01488               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTSURGR
01489            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTSURGR
01490            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTSURGR
01491            ADD +1  TO  WS-CIA                                     ELTSURGR
01492         ELSE                                                      ELTSURGR
01493            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTSURGR
01494            ADD +1  TO  WS-CIA.                                    ELTSURGR
01495                                                                   ELTSURGR
01496      IF WS-MOVE-LINES-TO-CIA                                      ELTSURGR
01497         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTSURGR
01498            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTSURGR
01499               VARYING  WS-SUB1  FROM  2  BY  1                    ELTSURGR
01500               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTSURGR
01501         ELSE                                                      ELTSURGR
01502            NEXT SENTENCE                                          ELTSURGR
01503      ELSE                                                         ELTSURGR
01504         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTSURGR
01505                                                                   ELTSURGR
01506      GO TO 2299-EXIT.                                             ELTSURGR
01507                                                                   ELTSURGR
01508  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTSURGR
01509      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTSURGR
01510      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTSURGR
01511                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTSURGR
01512                                                                   ELTSURGR
01513  2260-MOVE-LINES-TO-CIA.                                          ELTSURGR
01514      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTSURGR
01515      ADD +1  TO  WS-CIA.                                          ELTSURGR
01516                                                                   ELTSURGR
01517  2299-EXIT.           EXIT.                                       ELTSURGR
01518                                                                   ELTSURGR
01519      TITLE 'INSTITUTIONAL   OUTPATIENT'.                          ELTSURGR
01520 ***************************************************************** ELTSURGR
01521 *            I N S T I T U T I O N A L   O P   R T N E            ELTSURGR
01522 *                                                                 ELTSURGR
01523 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTSURGR
01524 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTSURGR
01525 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTSURGR
01526 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTSURGR
01527 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTSURGR
01528 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTSURGR
01529 *  MODULE.                                                        ELTSURGR
01530 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTSURGR
01531 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTSURGR
01532 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTSURGR
01533 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTSURGR
01534 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTSURGR
01535 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTSURGR
01536 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTSURGR
01537 *                                                                 ELTSURGR
01538 ***************************************************************** ELTSURGR
01539  3000-INSTITUTIONAL-OP-RTNE SECTION.                              ELTSURGR
01540      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSURGR
01541                                                                   ELTSURGR
01542      MOVE WS-HDR-2-INST-OP    TO  COF-HDR-LINE (2).               ELTSURGR
01543                                                                   ELTSURGR
01544      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTSURGR
01545      MOVE 'P'            TO  COF-FUNCTION.                        ELTSURGR
01546                                                                   ELTSURGR
01547      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
01548                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01549                     END-EXEC.                                     ELTSURGR
01550                                                                   ELTSURGR
01551      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
01552      PERFORM WITH TEST BEFORE                                     ELTSURGR
01553              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSURGR
01554              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSURGR
01555         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSURGR
01556         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSURGR
01557         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSURGR
01558         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSURGR
01559      END-PERFORM.                                                 ELTSURGR
01560      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
01561                                                                   ELTSURGR
01562                                                                   ELTSURGR
01563      PERFORM WITH TEST BEFORE                                     ELTSURGR
01564         VARYING WS-SUB FROM +1 BY +1                              ELTSURGR
01565         UNTIL   WS-SUB  >     WS-INST-OP-CNT                      ELTSURGR
01566           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTSURGR
01567           MOVE WS-INST-OP-LIST (WS-SUB)                           ELTSURGR
01568                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTSURGR
01569            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTSURGR
01570                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTSURGR
01571      END-PERFORM.                                                 ELTSURGR
01572                                                                   ELTSURGR
01573      MOVE 'SURGICAL SERVICES     '  TO  SSB-TOPIC-PHRASE.         ELTSURGR
01574                                                                   ELTSURGR
01575                                                                   ELTSURGR
01576      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTSURGR
01577                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01578                     END-EXEC.                                     ELTSURGR
01579                                                                   ELTSURGR
01580      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
01581                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01582                     END-EXEC.                                     ELTSURGR
01583                                                                   ELTSURGR
01584      IF PVN-COVG-NONE                                             ELTSURGR
01585         GO TO 3099-EXIT.                                          ELTSURGR
01586                                                                   ELTSURGR
01587      MOVE +1  TO  WS-CIA.                                         ELTSURGR
01588                                                                   ELTSURGR
01589      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTSURGR
01590            PSP-PROVN-PRICING-METHD,                               ELTSURGR
01591            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSURGR
01592            PSP-TRANSF-OTHER-RESP-IND,                             ELTSURGR
01593            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSURGR
01594            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSURGR
01595            PSP-SPILL-OVER-DED-APL-IND,                            ELTSURGR
01596            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSURGR
01597            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSURGR
01598            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSURGR
01599            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSURGR
01600            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSURGR
01601            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSURGR
01602            PSP-BEN-TAB-PROVN-ID-PVE,                              ELTSURGR
01603            PSB-PROF-CHRG-HSP-CLM,                                 ELTSURGR
01604            PSB-TRANSSXL-PMT-RESTR-OVRD,                           ELTSURGR
01605            PSW-TRNS-SEX-REST-OVRD-IND.                            ELTSURGR
01606                                                                   ELTSURGR
01607      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSURGR
01608                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01609                     END-EXEC.                                     ELTSURGR
01610                                                                   ELTSURGR
01611      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSURGR
01612      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
01613          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSURGR
01614                                                                   ELTSURGR
01615      PERFORM WITH TEST BEFORE                                     ELTSURGR
01616         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSURGR
01617         UNTIL WS-SUB  >  WS-INST-OP-CNT                           ELTSURGR
01618              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTSURGR
01619              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTSURGR
01620                   PERFORM 3040-BUILD-SCREEN-LINES THRU 3040-EXIT  ELTSURGR
01621              END-IF                                               ELTSURGR
01622      END-PERFORM.                                                 ELTSURGR
01623                                                                   ELTSURGR
01624      GO TO 3099-EXIT.                                             ELTSURGR
01625                                                                   ELTSURGR
01626  3040-BUILD-SCREEN-LINES.                                         ELTSURGR
01627                                                                   ELTSURGR
01628      SET PLT-INDEX1  TO                                           ELTSURGR
01629                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTSURGR
01630      IF WS-NOT-FIRST-TIME                                         ELTSURGR
01631         MOVE 'P'  TO  COF-FUNCTION                                ELTSURGR
01632         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
01633             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
01634              END-EXEC                                             ELTSURGR
01635      ELSE                                                         ELTSURGR
01636         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTSURGR
01637                                                                   ELTSURGR
01638      MOVE +1  TO  WS-CIA.                                         ELTSURGR
01639      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
01640         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTSURGR
01641            SET PLT-INDEX2  TO  2                                  ELTSURGR
01642         ELSE                                                      ELTSURGR
01643            MOVE TABLE-MAX TO WS-SUB                               ELTSURGR
01644            PERFORM 3090-PROBLEM-WITH-INDICES                      ELTSURGR
01645            GO TO 3040-EXIT                                        ELTSURGR
01646      ELSE                                                         ELTSURGR
01647         SET PLT-INDEX2  TO  1.                                    ELTSURGR
01648                                                                   ELTSURGR
01649 **---------------------------------------------------------------+ELTSURGR
01650 **                                                               |ELTSURGR
01651 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTSURGR
01652      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTSURGR
01653      ADD  +1  TO  WS-CIA.                                         ELTSURGR
01654      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO          WS-SUB3.ELTSURGR
01655      MOVE ZERO  TO  WS-SUB2.                                      ELTSURGR
01656                                                                   ELTSURGR
01657      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTSURGR
01658                                                                   ELTSURGR
01659      PERFORM 3050-ZERO-ALL-WITH-SAME-NO                           ELTSURGR
01660         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTSURGR
01661         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.                ELTSURGR
01662      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSURGR
01663                                                                   ELTSURGR
01664      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTSURGR
01665      MOVE 1  TO  WS-CIA.                                          ELTSURGR
01666      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTSURGR
01667             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
01668              END-EXEC.                                            ELTSURGR
01669 **                                                               |ELTSURGR
01670 **---------------------------------------------------------------+ELTSURGR
01671                                                                   ELTSURGR
01672 **---------------------------------------------------------------+ELTSURGR
01673 **                                                               |ELTSURGR
01674 **        P L A C E   O F   T R E A T M E N T                    |ELTSURGR
01675      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
01676                                                              ZERO ELTSURGR
01677         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01678         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTSURGR
01679         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
01680                                                   CMF-CODE-VALUE  ELTSURGR
01681         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTSURGR
01682         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
01683         ADD  +1  TO  WS-CIA.                                      ELTSURGR
01684 **                                                               |ELTSURGR
01685 **---------------------------------------------------------------+ELTSURGR
01686                                                                   ELTSURGR
01687 **---------------------------------------------------------------+ELTSURGR
01688 **                                                               |ELTSURGR
01689 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTSURGR
01690 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTSURGR
01691 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTSURGR
01692      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01693      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01694         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01695                                                              '19' ELTSURGR
01696         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
01697         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01698         ADD +1  TO  WS-CIA.                                       ELTSURGR
01699                                                                   ELTSURGR
01700      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01701      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01702         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01703                                                        '19' AND   ELTSURGR
01704         NOT WS-ADD-A-BLANK-LINE                                   ELTSURGR
01705         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
01706         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01707         ADD +1  TO  WS-CIA.                                       ELTSURGR
01708                                                                   ELTSURGR
01709      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01710      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01711         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
01712                              AND                                  ELTSURGR
01713         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01714         SET  PLT-INDEX2  TO  2                                    ELTSURGR
01715         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
01716                                                             ZERO  ELTSURGR
01717            MOVE WS-POSSIBLE-ERROR   TO  WS-DTL-BASIC              ELTSURGR
01718            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
01719            ADD +1  TO  WS-CIA.                                    ELTSURGR
01720                                                                   ELTSURGR
01721      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01722      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01723         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
01724                              AND                                  ELTSURGR
01725         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTSURGR
01726         MOVE WS-POSSIBLE-ERROR   TO  WS-DTL-BASIC                 ELTSURGR
01727         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTSURGR
01728         ADD +1  TO  WS-CIA.                                       ELTSURGR
01729                                                                   ELTSURGR
01730      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTSURGR
01731         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01732         SET  PLT-INDEX2  TO  2                                    ELTSURGR
01733         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
01734                                                             ZERO  ELTSURGR
01735            MOVE WS-POSSIBLE-ERROR   TO  WS-DTL-BASIC              ELTSURGR
01736            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
01737            ADD +1  TO  WS-CIA.                                    ELTSURGR
01738                                                                   ELTSURGR
01739      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
01740      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
01741         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTSURGR
01742                                                               ZEROELTSURGR
01743            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01744                                                            =  ZEROELTSURGR
01745               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
01746            ELSE                                                   ELTSURGR
01747               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
01748          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01749                                                  TO  WS-PERCENTAGEELTSURGR
01750         ELSE                                                      ELTSURGR
01751            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
01752          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01753                                                 TO  WS-PERCENTAGE.ELTSURGR
01754      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
01755         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01756                                             ZERO AND  NOT =  '19' ELTSURGR
01757         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01758         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
01759         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
01760                                                    CMF-CODE-VALUE ELTSURGR
01761         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01762         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01763         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
01764                                                                   ELTSURGR
01765      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01766      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
01767         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTSURGR
01768                                                               ZEROELTSURGR
01769            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01770                                                            =  ZEROELTSURGR
01771               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
01772            ELSE                                                   ELTSURGR
01773               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
01774          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01775                                                 TO   WS-PERCENTAGEELTSURGR
01776         ELSE                                                      ELTSURGR
01777            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
01778          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
01779                                                 TO  WS-PERCENTAGE.ELTSURGR
01780      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01781         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
01782                                             ZERO AND  NOT =  '19' ELTSURGR
01783         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01784         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
01785         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
01786                                                    CMF-CODE-VALUE ELTSURGR
01787         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTSURGR
01788         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01789         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
01790                                                                   ELTSURGR
01791      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
01792         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
01793         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
01794         MOVE 1  TO  WS-CIA                                        ELTSURGR
01795         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
01796                COMMAREA(DFHCOMMAREA)                              ELTSURGR
01797                 END-EXEC.                                         ELTSURGR
01798 **                                                               |ELTSURGR
01799 **---------------------------------------------------------------+ELTSURGR
01800                                                                   ELTSURGR
01801 **---------------------------------------------------------------+ELTSURGR
01802 **        P R O F E S S I O N A L   C H A R G E S   O N          |ELTSURGR
01803 **                H O S P I T A L   B I L L                       ELTSURGR
01804                                                                   ELTSURGR
01805      SET PLT-INDEX2  TO  1.                                       ELTSURGR
01806                                                                   ELTSURGR
01807      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTSURGR
01808          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
01809              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
01810                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
01811                  MOVE WS-PROF-OUTPT-CHRGES                        ELTSURGR
01812                                 TO  COF-DTL-LINE (WS-CIA)         ELTSURGR
01813                  MOVE 'Y'       TO  WS-ADD-A-BLANK-IND            ELTSURGR
01814                  ADD  +1        TO  WS-CIA.                       ELTSURGR
01815                                                                   ELTSURGR
01816      SET PLT-INDEX2  TO  2.                                       ELTSURGR
01817                                                                   ELTSURGR
01818      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTSURGR
01819          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
01820                      AND                                          ELTSURGR
01821             NOT WS-ADD-A-BLANK-LINE                               ELTSURGR
01822              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
01823                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
01824                  MOVE WS-PROF-INPT-CHRGES                         ELTSURGR
01825                                 TO  COF-DTL-LINE (WS-CIA)         ELTSURGR
01826                  MOVE 'Y'       TO  WS-ADD-A-BLANK-IND            ELTSURGR
01827                  ADD  +1        TO  WS-CIA.                       ELTSURGR
01828                                                                   ELTSURGR
01829      SET PLT-INDEX2  TO  1.                                       ELTSURGR
01830                                                                   ELTSURGR
01831      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTSURGR
01832          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
01833              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
01834                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
01835                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTSURGR
01836                MOVE 'PROF-CHRG-HSP-CLM'                           ELTSURGR
01837                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTSURGR
01838                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTSURGR
01839                                   TO  CMF-CODE-VALUE              ELTSURGR
01840                MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA           ELTSURGR
01841                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTSURGR
01842                PERFORM 2100-CALL-CODES-MANUAL-LONG.               ELTSURGR
01843                                                                   ELTSURGR
01844      SET PLT-INDEX2  TO  2.                                       ELTSURGR
01845                                                                   ELTSURGR
01846      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTSURGR
01847          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTSURGR
01848              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTSURGR
01849                        NOT  =  '0'  AND  NOT  =  LOW-VALUES       ELTSURGR
01850                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTSURGR
01851                MOVE 'PROF-CHRG-HSP-CLM'                           ELTSURGR
01852                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTSURGR
01853                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTSURGR
01854                                   TO  CMF-CODE-VALUE              ELTSURGR
01855                MOVE WS-SUPP-LIT   TO  WS-TEMP-TEXT-AREA           ELTSURGR
01856                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTSURGR
01857                PERFORM 2100-CALL-CODES-MANUAL-LONG.               ELTSURGR
01858                                                                   ELTSURGR
01859      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
01860          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTSURGR
01861          ADD  +1, WS-CIA  GIVING  COF-NBR-DTL-LINES               ELTSURGR
01862          MOVE +1   TO  WS-CIA                                     ELTSURGR
01863          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTSURGR
01864                         COMMAREA (DFHCOMMAREA)                    ELTSURGR
01865                         END-EXEC.                                 ELTSURGR
01866                                                                   ELTSURGR
01867 **---------------------------------------------------------------+ELTSURGR
01868 **                                                               |ELTSURGR
01869 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTSURGR
01870      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01871      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01872         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSURGR
01873                                                       NOT =  '0'  ELTSURGR
01874         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01875         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTSURGR
01876                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
01877         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSURGR
01878                                           TO   CMF-CODE-VALUE     ELTSURGR
01879         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01880         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
01881 **                                                               |ELTSURGR
01882 **---------------------------------------------------------------+ELTSURGR
01883                                                                   ELTSURGR
01884 **---------------------------------------------------------------+ELTSURGR
01885 **                                                               |ELTSURGR
01886 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTSURGR
01887      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
01888      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
01889         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSURGR
01890                                                       NOT =  '0'  ELTSURGR
01891         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01892         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTSURGR
01893                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
01894         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
01895                                                 TO  CMF-CODE-VALUEELTSURGR
01896         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
01897         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
01898 **                                                               |ELTSURGR
01899 **---------------------------------------------------------------+ELTSURGR
01900                                                                   ELTSURGR
01901      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTSURGR
01902      MOVE 1  TO  WS-CIA.                                          ELTSURGR
01903                                                                   ELTSURGR
01904      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
01905              END-EXEC.                                            ELTSURGR
01906                                                                   ELTSURGR
01907                                                                   ELTSURGR
01908      PERFORM 7100-TRANSFER-OTHR-RESP.                             ELTSURGR
01909      PERFORM 7000-ALL-LEVEL-TABS.                                 ELTSURGR
01910                                                                   ELTSURGR
01911  3040-EXIT.  EXIT.                                                ELTSURGR
01912 /                                                                 ELTSURGR
01913                                                                   ELTSURGR
01914  3050-ZERO-ALL-WITH-SAME-NO.                                      ELTSURGR
01915                                                                   ELTSURGR
01916      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) =          WS-SUB3   ELTSURGR
01917          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTSURGR
01918              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTSURGR
01919                                                                   ELTSURGR
01920      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) =          WS-SUB3    ELTSURGR
01921         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
01922         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTSURGR
01923         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTSURGR
01924                                                   CMF-CODE-VALUE  ELTSURGR
01925         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTSURGR
01926         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
01927         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
01928         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTSURGR
01929         ADD  1  TO  WS-SUB2                                       ELTSURGR
01930         IF WS-CIA  >  20 OR  =  20                                ELTSURGR
01931            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTSURGR
01932            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTSURGR
01933                COMMAREA(DFHCOMMAREA)                              ELTSURGR
01934                END-EXEC                                           ELTSURGR
01935            MOVE +1  TO  WS-CIA.                                   ELTSURGR
01936                                                                   ELTSURGR
01937  3090-PROBLEM-WITH-INDICES.                                       ELTSURGR
01938                                                                   ELTSURGR
01939      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTSURGR
01940      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTSURGR
01941                                                                   ELTSURGR
01942      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSURGR
01943      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSURGR
01944                                                                   ELTSURGR
01945      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
01946              END-EXEC.                                            ELTSURGR
01947                                                                   ELTSURGR
01948  3099-EXIT.           EXIT.                                       ELTSURGR
01949                                                                   ELTSURGR
01950       TITLE 'PROFESSIONAL  OUT PATIENT'.                          ELTSURGR
01951 ***************************************************************** ELTSURGR
01952 *            P R O F E S S I O N A L   O P   R T N E              ELTSURGR
01953 *                                                                 ELTSURGR
01954 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTSURGR
01955 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTSURGR
01956 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTSURGR
01957 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTSURGR
01958 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTSURGR
01959 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTSURGR
01960 *  MODULE.                                                        ELTSURGR
01961 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTSURGR
01962 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTSURGR
01963 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTSURGR
01964 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTSURGR
01965 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTSURGR
01966 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTSURGR
01967 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTSURGR
01968 *                                                                 ELTSURGR
01969 ***************************************************************** ELTSURGR
01970  4000-PROFESSIONAL-OP-RTNE SECTION.                               ELTSURGR
01971      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSURGR
01972                                                                   ELTSURGR
01973      MOVE WS-HDR-2-PROF-OP    TO  COF-HDR-LINE (2).               ELTSURGR
01974                                                                   ELTSURGR
01975      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTSURGR
01976      MOVE 'P'            TO  COF-FUNCTION.                        ELTSURGR
01977                                                                   ELTSURGR
01978      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
01979                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
01980                     END-EXEC.                                     ELTSURGR
01981                                                                   ELTSURGR
01982      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
01983      PERFORM WITH TEST BEFORE                                     ELTSURGR
01984              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSURGR
01985              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSURGR
01986         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSURGR
01987         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSURGR
01988         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSURGR
01989         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSURGR
01990      END-PERFORM.                                                 ELTSURGR
01991      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSURGR
01992                                                                   ELTSURGR
01993                                                                   ELTSURGR
01994      PERFORM WITH TEST BEFORE                                     ELTSURGR
01995         VARYING WS-SUB FROM +1 BY +1                              ELTSURGR
01996         UNTIL   WS-SUB  >     WS-PROF-OP-CNT                      ELTSURGR
01997           SET PVN-BEN-PROVN-IDX TO WS-SUB                         ELTSURGR
01998           MOVE WS-PROF-OP-LIST (WS-SUB)                           ELTSURGR
01999                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTSURGR
02000            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTSURGR
02001                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTSURGR
02002      END-PERFORM.                                                 ELTSURGR
02003                                                                   ELTSURGR
02004      MOVE 'SURGICAL SERVICES     '  TO  SSB-TOPIC-PHRASE.         ELTSURGR
02005                                                                   ELTSURGR
02006                                                                   ELTSURGR
02007      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTSURGR
02008                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
02009                     END-EXEC.                                     ELTSURGR
02010                                                                   ELTSURGR
02011      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSURGR
02012                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
02013                     END-EXEC.                                     ELTSURGR
02014                                                                   ELTSURGR
02015      IF PVN-COVG-NONE                                             ELTSURGR
02016         GO TO 4099-EXIT.                                          ELTSURGR
02017                                                                   ELTSURGR
02018      MOVE +1  TO  WS-CIA.                                         ELTSURGR
02019      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTSURGR
02020            PSP-PROVN-PRICING-METHD,                               ELTSURGR
02021            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSURGR
02022            PSP-TRANSF-OTHER-RESP-IND,                             ELTSURGR
02023            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSURGR
02024            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSURGR
02025            PSP-SPILL-OVER-DED-APL-IND,                            ELTSURGR
02026            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSURGR
02027            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSURGR
02028            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSURGR
02029            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSURGR
02030            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSURGR
02031            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSURGR
02032            PSP-BEN-TAB-PROVN-ID-PVE,                              ELTSURGR
02033            PSC-BEN-SCOPE-ID,                                      ELTSURGR
02034            PSE-BEN-SCOPE-ID,                                      ELTSURGR
02035            PSB-TRANSSXL-PMT-RESTR-OVRD,                           ELTSURGR
02036            PSW-TRNS-SEX-REST-OVRD-IND.                            ELTSURGR
02037                                                                   ELTSURGR
02038      MOVE ZEROS  TO  PSB-PROF-CHRG-HSP-CLM.                       ELTSURGR
02039                                                                   ELTSURGR
02040      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSURGR
02041                     COMMAREA (DFHCOMMAREA)                        ELTSURGR
02042                     END-EXEC.                                     ELTSURGR
02043                                                                   ELTSURGR
02044      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSURGR
02045      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
02046          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSURGR
02047                                                                   ELTSURGR
02048      PERFORM WITH TEST BEFORE                                     ELTSURGR
02049         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSURGR
02050         UNTIL WS-SUB  >  WS-PROF-OP-CNT                           ELTSURGR
02051              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTSURGR
02052              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTSURGR
02053                   PERFORM 4040-BUILD-SCREEN-LINES THRU 4040-EXIT  ELTSURGR
02054              END-IF                                               ELTSURGR
02055      END-PERFORM.                                                 ELTSURGR
02056                                                                   ELTSURGR
02057      GO TO 4099-EXIT.                                             ELTSURGR
02058                                                                   ELTSURGR
02059  4040-BUILD-SCREEN-LINES.                                         ELTSURGR
02060                                                                   ELTSURGR
02061      SET PLT-INDEX1   TO                                          ELTSURGR
02062                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTSURGR
02063      IF WS-NOT-FIRST-TIME                                         ELTSURGR
02064         MOVE 'P'  TO  COF-FUNCTION                                ELTSURGR
02065         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
02066             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
02067              END-EXEC                                             ELTSURGR
02068      ELSE                                                         ELTSURGR
02069         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTSURGR
02070                                                                   ELTSURGR
02071      MOVE +1  TO  WS-CIA.                                         ELTSURGR
02072      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
02073         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTSURGR
02074            SET PLT-INDEX2  TO  2                                  ELTSURGR
02075         ELSE                                                      ELTSURGR
02076            MOVE TABLE-MAX TO WS-SUB                               ELTSURGR
02077            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTSURGR
02078            GO TO 4040-EXIT                                        ELTSURGR
02079      ELSE                                                         ELTSURGR
02080         SET PLT-INDEX2  TO  1.                                    ELTSURGR
02081                                                                   ELTSURGR
02082 **---------------------------------------------------------------+ELTSURGR
02083 **                                                               |ELTSURGR
02084 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTSURGR
02085      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTSURGR
02086      ADD  +1  TO  WS-CIA.                                         ELTSURGR
02087      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTSURGR
02088      MOVE ZERO  TO  WS-SUB2.                                      ELTSURGR
02089      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTSURGR
02090         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTSURGR
02091         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.                ELTSURGR
02092      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSURGR
02093                                                                   ELTSURGR
02094      ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTSURGR
02095      MOVE 1  TO  WS-CIA.                                          ELTSURGR
02096      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTSURGR
02097             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
02098               END-EXEC.                                           ELTSURGR
02099 **                                                               |ELTSURGR
02100 **---------------------------------------------------------------+ELTSURGR
02101                                                                   ELTSURGR
02102 **---------------------------------------------------------------+ELTSURGR
02103 **                                                               |ELTSURGR
02104 **        P L A C E   O F   T R E A T M E N T                    |ELTSURGR
02105      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02106                                                              ZERO ELTSURGR
02107         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02108         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTSURGR
02109         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTSURGR
02110                                               TO  CMF-CODE-VALUE  ELTSURGR
02111         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTSURGR
02112         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
02113         ADD  +1  TO  WS-CIA.                                      ELTSURGR
02114 **                                                               |ELTSURGR
02115 **---------------------------------------------------------------+ELTSURGR
02116                                                                   ELTSURGR
02117 **---------------------------------------------------------------+ELTSURGR
02118 **                                                               |ELTSURGR
02119 **            B E N E F I T   S C O P E   I D                    |ELTSURGR
02120      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02121      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
02122         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
02123            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02124                                         '0000' AND  NOT =  '00  ' ELTSURGR
02125               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
02126               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
02127               ADD  +1  TO  WS-CIA                                 ELTSURGR
02128            ELSE                                                   ELTSURGR
02129               NEXT SENTENCE                                       ELTSURGR
02130         ELSE                                                      ELTSURGR
02131            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02132                                         '0000' AND  NOT =  '00  ' ELTSURGR
02133               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
02134               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
02135               ADD  +1  TO  WS-CIA.                                ELTSURGR
02136                                                                   ELTSURGR
02137      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
02138      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
02139         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
02140            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02141                                         '0000' AND  NOT =  '00  ' ELTSURGR
02142               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
02143               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
02144               ADD  +1  TO  WS-CIA                                 ELTSURGR
02145            ELSE                                                   ELTSURGR
02146               NEXT SENTENCE                                       ELTSURGR
02147         ELSE                                                      ELTSURGR
02148            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02149                                         '0000' AND  NOT =  '00  ' ELTSURGR
02150               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTSURGR
02151               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTSURGR
02152               ADD  +1  TO  WS-CIA.                                ELTSURGR
02153                                                                   ELTSURGR
02154      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02155      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
02156         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
02157            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02158                                         '0000' AND  NOT =  '00  ' ELTSURGR
02159               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTSURGR
02160               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
02161               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
02162                                                    CMF-CODE-VALUE ELTSURGR
02163               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTSURGR
02164               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
02165               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTSURGR
02166            ELSE                                                   ELTSURGR
02167               NEXT SENTENCE                                       ELTSURGR
02168         ELSE                                                      ELTSURGR
02169            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02170                                         '0000' AND  NOT =  '00  ' ELTSURGR
02171               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTSURGR
02172               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
02173               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
02174                                                    CMF-CODE-VALUE ELTSURGR
02175               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTSURGR
02176               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
02177               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSURGR
02178                                                                   ELTSURGR
02179      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
02180         SET PLT-INDEX2  TO  2                                     ELTSURGR
02181         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSURGR
02182            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02183                                         '0000' AND  NOT =  '00  ' ELTSURGR
02184               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTSURGR
02185               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
02186               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
02187                                                    CMF-CODE-VALUE ELTSURGR
02188               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTSURGR
02189               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
02190               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTSURGR
02191            ELSE                                                   ELTSURGR
02192               NEXT SENTENCE                                       ELTSURGR
02193         ELSE                                                      ELTSURGR
02194            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTSURGR
02195                                         '0000' AND  NOT =  '00  ' ELTSURGR
02196               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTSURGR
02197               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTSURGR
02198               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTSURGR
02199                                                    CMF-CODE-VALUE ELTSURGR
02200               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTSURGR
02201               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTSURGR
02202               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSURGR
02203                                                                   ELTSURGR
02204      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
02205         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02206         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
02207         MOVE 1  TO  WS-CIA                                        ELTSURGR
02208         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
02209                COMMAREA(DFHCOMMAREA)                              ELTSURGR
02210                 END-EXEC.                                         ELTSURGR
02211 **                                                               |ELTSURGR
02212 **---------------------------------------------------------------+ELTSURGR
02213                                                                   ELTSURGR
02214      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
02215         SET PLT-INDEX2  TO  2                                     ELTSURGR
02216      ELSE                                                         ELTSURGR
02217         SET PLT-INDEX2  TO  1.                                    ELTSURGR
02218                                                                   ELTSURGR
02219 **---------------------------------------------------------------+ELTSURGR
02220 **                                                               |ELTSURGR
02221 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTSURGR
02222 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTSURGR
02223 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTSURGR
02224      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02225      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02226         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
02227                                                              '19' ELTSURGR
02228         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02229         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
02230         ADD +1  TO  WS-CIA.                                       ELTSURGR
02231                                                                   ELTSURGR
02232      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
02233      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
02234         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
02235                                                        '19' AND   ELTSURGR
02236         NOT WS-ADD-A-BLANK-LINE                                   ELTSURGR
02237         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02238         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTSURGR
02239         ADD +1  TO  WS-CIA.                                       ELTSURGR
02240                                                                   ELTSURGR
02241      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02242      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02243         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
02244                            AND                                    ELTSURGR
02245         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
02246         SET  PLT-INDEX2  TO  2                                    ELTSURGR
02247         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
02248                                                              ZERO ELTSURGR
02249            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
02250            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
02251            ADD +1  TO  WS-CIA.                                    ELTSURGR
02252                                                                   ELTSURGR
02253      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02254      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02255         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTSURGR
02256                            AND                                    ELTSURGR
02257         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTSURGR
02258         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTSURGR
02259         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTSURGR
02260         ADD +1  TO  WS-CIA.                                       ELTSURGR
02261                                                                   ELTSURGR
02262      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTSURGR
02263         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
02264         SET  PLT-INDEX2  TO  2                                    ELTSURGR
02265         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTSURGR
02266                                                              ZERO ELTSURGR
02267            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTSURGR
02268            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTSURGR
02269            ADD +1  TO  WS-CIA.                                    ELTSURGR
02270                                                                   ELTSURGR
02271      SET  PLT-INDEX2  TO  1.                                      ELTSURGR
02272      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTSURGR
02273         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
02274                                                            =  ZEROELTSURGR
02275            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02276                                                            =  ZEROELTSURGR
02277               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
02278            ELSE                                                   ELTSURGR
02279               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
02280          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02281                                                  TO  WS-PERCENTAGEELTSURGR
02282         ELSE                                                      ELTSURGR
02283            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
02284          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02285                                                 TO  WS-PERCENTAGE.ELTSURGR
02286      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02287         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
02288                                             ZERO AND  NOT =  '19' ELTSURGR
02289         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02290         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
02291         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
02292                                                    CMF-CODE-VALUE ELTSURGR
02293         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
02294         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
02295         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
02296                                                                   ELTSURGR
02297      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
02298      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTSURGR
02299         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTSURGR
02300                                                               ZEROELTSURGR
02301            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02302                                                            =  ZEROELTSURGR
02303               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTSURGR
02304            ELSE                                                   ELTSURGR
02305               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTSURGR
02306          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02307                                                  TO  WS-PERCENTAGEELTSURGR
02308         ELSE                                                      ELTSURGR
02309            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTSURGR
02310          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSURGR
02311                                                 TO  WS-PERCENTAGE.ELTSURGR
02312      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
02313         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTSURGR
02314                                             ZERO AND  NOT =  '19' ELTSURGR
02315         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02316         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTSURGR
02317         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTSURGR
02318                                                    CMF-CODE-VALUE ELTSURGR
02319         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTSURGR
02320         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
02321         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSURGR
02322                                                                   ELTSURGR
02323      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
02324         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02325         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTSURGR
02326         MOVE 1  TO  WS-CIA                                        ELTSURGR
02327         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
02328                COMMAREA(DFHCOMMAREA)                              ELTSURGR
02329                 END-EXEC.                                         ELTSURGR
02330 **                                                               |ELTSURGR
02331 **---------------------------------------------------------------+ELTSURGR
02332                                                                   ELTSURGR
02333      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
02334         SET PLT-INDEX2  TO  2                                     ELTSURGR
02335      ELSE                                                         ELTSURGR
02336         SET PLT-INDEX2  TO  1.                                    ELTSURGR
02337                                                                   ELTSURGR
02338 **---------------------------------------------------------------+ELTSURGR
02339 **                                                               |ELTSURGR
02340 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTSURGR
02341      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
02342      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
02343         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSURGR
02344                                                         NOT =  '0'ELTSURGR
02345         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02346         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTSURGR
02347                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
02348         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSURGR
02349                                                TO  CMF-CODE-VALUE ELTSURGR
02350         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
02351         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
02352 **                                                               |ELTSURGR
02353 **---------------------------------------------------------------+ELTSURGR
02354                                                                   ELTSURGR
02355 **---------------------------------------------------------------+ELTSURGR
02356 **                                                               |ELTSURGR
02357 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTSURGR
02358      SET  PLT-INDEX2  TO  2.                                      ELTSURGR
02359      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTSURGR
02360         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSURGR
02361                                                         NOT =  '0'ELTSURGR
02362         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02363         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTSURGR
02364                                           CMF-ELEMENT-SYSTEM-NAME ELTSURGR
02365         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTSURGR
02366                                                 TO  CMF-CODE-VALUEELTSURGR
02367         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTSURGR
02368         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSURGR
02369 **                                                               |ELTSURGR
02370 **---------------------------------------------------------------+ELTSURGR
02371                                                                   ELTSURGR
02372      PERFORM 7100-TRANSFER-OTHR-RESP.                             ELTSURGR
02373      PERFORM 7000-ALL-LEVEL-TABS.                                 ELTSURGR
02374                                                                   ELTSURGR
02375  4040-EXIT.  EXIT.                                                ELTSURGR
02376 /                                                                 ELTSURGR
02377                                                                   ELTSURGR
02378  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTSURGR
02379                                                                   ELTSURGR
02380      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTSURGR
02381         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTSURGR
02382         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTSURGR
02383         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)        TO              ELTSURGR
02384                                                   CMF-CODE-VALUE  ELTSURGR
02385         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTSURGR
02386         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTSURGR
02387         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSURGR
02388         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTSURGR
02389         ADD  1  TO  WS-SUB2                                       ELTSURGR
02390         IF WS-CIA  >  20 OR  =  20                                ELTSURGR
02391            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTSURGR
02392            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTSURGR
02393                COMMAREA(DFHCOMMAREA)                              ELTSURGR
02394                END-EXEC                                           ELTSURGR
02395            MOVE +1  TO  WS-CIA.                                   ELTSURGR
02396                                                                   ELTSURGR
02397  4090-PROBLEM-WITH-INDICES.                                       ELTSURGR
02398                                                                   ELTSURGR
02399      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTSURGR
02400      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTSURGR
02401                                                                   ELTSURGR
02402      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSURGR
02403      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSURGR
02404                                                                   ELTSURGR
02405      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSURGR
02406              END-EXEC.                                            ELTSURGR
02407                                                                   ELTSURGR
02408  4099-EXIT.           EXIT.                                       ELTSURGR
02409      TITLE ' ALL LEVEL TABULARS'.                                 ELTSURGR
02410  7000-ALL-LEVEL-TABS  SECTION.                                    ELTSURGR
02411 ****************************************************************  ELTSURGR
02412 *                  A A R   T A B U L A R                       *  ELTSURGR
02413 ****************************************************************  ELTSURGR
02414      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02415      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02416         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02417                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02418         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02419         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTSURGR
02420         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTSURGR
02421      ELSE                                                         ELTSURGR
02422         SET PLT-INDEX2  TO  2                                     ELTSURGR
02423         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02424            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02425                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02426            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTSURGR
02427            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTSURGR
02428            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTSURGR
02429                                                                   ELTSURGR
02430      IF WS-ADD-A-BLANK-LINE                                       ELTSURGR
02431         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTSURGR
02432         MOVE 1  TO  WS-CIA                                        ELTSURGR
02433         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTSURGR
02434             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
02435         END-EXEC.                                                 ELTSURGR
02436 *--------------------------------------------------------------*  ELTSURGR
02437 *                  P P F   T A B U L A R                       *  ELTSURGR
02438 *--------------------------------------------------------------*  ELTSURGR
02439      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02440      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02441         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02442                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02443         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
02444                                             KWA-GCTABULR-KEY      ELTSURGR
02445         PERFORM 9900-GET-TABULAR-RECORD                           ELTSURGR
02446            THRU 9900-EXIT                                         ELTSURGR
02447         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTSURGR
02448               COMMAREA(DFHCOMMAREA)                               ELTSURGR
02449         END-EXEC                                                  ELTSURGR
02450      ELSE                                                         ELTSURGR
02451         SET PLT-INDEX2  TO  2                                     ELTSURGR
02452         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02453            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02454                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02455          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTSURGR
02456                                             KWA-GCTABULR-KEY      ELTSURGR
02457            PERFORM 9900-GET-TABULAR-RECORD                        ELTSURGR
02458               THRU 9900-EXIT                                      ELTSURGR
02459            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTSURGR
02460                  COMMAREA(DFHCOMMAREA)                            ELTSURGR
02461            END-EXEC.                                              ELTSURGR
02462 *--------------------------------------------------------------*  ELTSURGR
02463 *                  P V E   T A B U L A R                       *  ELTSURGR
02464 *--------------------------------------------------------------*  ELTSURGR
02465                                                                   ELTSURGR
02466      MOVE  +2     TO  WS-CIA.                                     ELTSURGR
02467      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTSURGR
02468      ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTSURGR
02469      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTSURGR
02470             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
02471      END-EXEC.                                                    ELTSURGR
02472                                                                   ELTSURGR
02473 *--------------------------------------------------------------*  ELTSURGR
02474 *                  A B M   T A B U L A R                       *  ELTSURGR
02475 *--------------------------------------------------------------*  ELTSURGR
02476      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02477      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTSURGR
02478                              AND                                  ELTSURGR
02479         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02480                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02481         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
02482                                              KWA-GCTABULR-KEY     ELTSURGR
02483         PERFORM 9900-GET-TABULAR-RECORD                           ELTSURGR
02484            THRU 9900-EXIT                                         ELTSURGR
02485         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTSURGR
02486               COMMAREA(DFHCOMMAREA)                               ELTSURGR
02487         END-EXEC                                                  ELTSURGR
02488      ELSE                                                         ELTSURGR
02489         SET PLT-INDEX2  TO  2                                     ELTSURGR
02490         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02491            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02492                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02493          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTSURGR
02494                                             KWA-GCTABULR-KEY      ELTSURGR
02495            PERFORM 9900-GET-TABULAR-RECORD                        ELTSURGR
02496               THRU 9900-EXIT                                      ELTSURGR
02497            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTSURGR
02498                  COMMAREA(DFHCOMMAREA)                            ELTSURGR
02499            END-EXEC.                                              ELTSURGR
02500 *--------------------------------------------------------------*  ELTSURGR
02501 *                  A C L   T A B U L A R                       *  ELTSURGR
02502 *--------------------------------------------------------------*  ELTSURGR
02503      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02504      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02505         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02506                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02507         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
02508                                              KWA-GCTABULR-KEY     ELTSURGR
02509         PERFORM 9900-GET-TABULAR-RECORD                           ELTSURGR
02510            THRU 9900-EXIT                                         ELTSURGR
02511         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTSURGR
02512               COMMAREA(DFHCOMMAREA)                               ELTSURGR
02513         END-EXEC                                                  ELTSURGR
02514      ELSE                                                         ELTSURGR
02515         SET PLT-INDEX2  TO  2                                     ELTSURGR
02516         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02517            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02518                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02519          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTSURGR
02520                                              KWA-GCTABULR-KEY     ELTSURGR
02521            PERFORM 9900-GET-TABULAR-RECORD                        ELTSURGR
02522               THRU 9900-EXIT                                      ELTSURGR
02523            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTSURGR
02524                  COMMAREA(DFHCOMMAREA)                            ELTSURGR
02525            END-EXEC.                                              ELTSURGR
02526 *--------------------------------------------------------------*  ELTSURGR
02527 *                  A D L   T A B U L A R                       *  ELTSURGR
02528 *--------------------------------------------------------------*  ELTSURGR
02529      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02530      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02531         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02532                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02533         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
02534                                             KWA-GCTABULR-KEY      ELTSURGR
02535         PERFORM 9900-GET-TABULAR-RECORD                           ELTSURGR
02536            THRU 9900-EXIT                                         ELTSURGR
02537         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTSURGR
02538               COMMAREA(DFHCOMMAREA)                               ELTSURGR
02539         END-EXEC                                                  ELTSURGR
02540      ELSE                                                         ELTSURGR
02541         SET PLT-INDEX2  TO  2                                     ELTSURGR
02542         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02543            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02544                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02545          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTSURGR
02546                                               KWA-GCTABULR-KEY    ELTSURGR
02547            PERFORM 9900-GET-TABULAR-RECORD                        ELTSURGR
02548               THRU 9900-EXIT                                      ELTSURGR
02549            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTSURGR
02550                  COMMAREA(DFHCOMMAREA)                            ELTSURGR
02551            END-EXEC.                                              ELTSURGR
02552 *--------------------------------------------------------------*  ELTSURGR
02553 *                  A O L   T A B U L A R                       *  ELTSURGR
02554 *--------------------------------------------------------------*  ELTSURGR
02555      SET PLT-INDEX2  TO  1.                                       ELTSURGR
02556      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTSURGR
02557         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTSURGR
02558                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02559         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTSURGR
02560                                             KWA-GCTABULR-KEY      ELTSURGR
02561         PERFORM 9900-GET-TABULAR-RECORD                           ELTSURGR
02562            THRU 9900-EXIT                                         ELTSURGR
02563         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTSURGR
02564               COMMAREA(DFHCOMMAREA)                               ELTSURGR
02565         END-EXEC                                                  ELTSURGR
02566      ELSE                                                         ELTSURGR
02567         SET PLT-INDEX2  TO  2                                     ELTSURGR
02568         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTSURGR
02569            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTSURGR
02570                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTSURGR
02571          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTSURGR
02572                                              KWA-GCTABULR-KEY     ELTSURGR
02573            PERFORM 9900-GET-TABULAR-RECORD                        ELTSURGR
02574               THRU 9900-EXIT                                      ELTSURGR
02575            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTSURGR
02576                  COMMAREA(DFHCOMMAREA)                            ELTSURGR
02577            END-EXEC.                                              ELTSURGR
02578 *--------------------------------------------------------------*  ELTSURGR
02579 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTSURGR
02580 *--------------------------------------------------------------*  ELTSURGR
02581                                                                   ELTSURGR
02582      MOVE  +2     TO  WS-CIA.                                     ELTSURGR
02583      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTSURGR
02584      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTSURGR
02585      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTSURGR
02586             COMMAREA(DFHCOMMAREA)                                 ELTSURGR
02587      END-EXEC.                                                    ELTSURGR
02588                                                                   ELTSURGR
02589  7000-EXIT.  EXIT.                                                ELTSURGR
02590      TITLE 'TRANSFER TO OTHER RESPONSIBILITY IND'.                ELTSURGR
02591  7100-TRANSFER-OTHR-RESP SECTION.                                 ELTSURGR
02592                                                                   ELTSURGR
02593      MOVE 1  TO  WS-CIA.                                          ELTSURGR
02594      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTSURGR
02595         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTSURGR
02596            SET PLT-INDEX2  TO  2                                  ELTSURGR
02597         ELSE                                                      ELTSURGR
02598            MOVE TABLE-MAX TO WS-SUB                               ELTSURGR
02599            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTSURGR
02600            GO TO 7100-EXIT                                        ELTSURGR
02601      ELSE                                                         ELTSURGR
02602         SET PLT-INDEX2  TO  1.                                    ELTSURGR
02603                                                                   ELTSURGR
02604      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZERO ELTSURGR
02605         GO TO 7100-EXIT.                                          ELTSURGR
02606                                                                   ELTSURGR
02607      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTSURGR
02608      MOVE 'TRANSF-OTHER-RESP-IND' TO  CMF-ELEMENT-SYSTEM-NAME.    ELTSURGR
02609      MOVE PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)       ELTSURGR
02610           TO  CMF-CODE-VALUE.                                     ELTSURGR
02611      MOVE SPACES                  TO  WS-TEMP-TEXT-AREA.          ELTSURGR
02612      PERFORM 2100-CALL-CODES-MANUAL-LONG.                         ELTSURGR
02613      ADD +1  TO  WS-CIA.                                          ELTSURGR
02614  7100-EXIT.  EXIT.                                                ELTSURGR
02615                                                                   ELTSURGR
02616      TITLE 'READ TABULAR RECORD'.                                 ELTSURGR
02617 ***************************************************************** ELTSURGR
02618 *            G E T   T A B U L A R   R E C O R D                  ELTSURGR
02619 *                                                                 ELTSURGR
02620 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF            ELTSURGR
02621 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTSURGR
02622 *  TO DISPLAY.                                                    ELTSURGR
02623 *                                                                 ELTSURGR
02624 ***************************************************************** ELTSURGR
02625  9900-GET-TABULAR-RECORD SECTION.                                 ELTSURGR
02626                                                                   ELTSURGR
02627      SET CIA-GCTABULR-DDN TO TRUE.                                ELTSURGR
02628      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSURGR
02629          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTSURGR
02630      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTSURGR
02631      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTSURGR
02632      SET IOP-RD                          TO TRUE.                 ELTSURGR
02633      SET IOP-FCQ-NONE                    TO TRUE.                 ELTSURGR
02634      SET IOP-KVQ-NONE                    TO TRUE.                 ELTSURGR
02635      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTSURGR
02636                                                                   ELTSURGR
02637      EXEC CICS LINK                                               ELTSURGR
02638                PROGRAM ('ELUIOPGM')                               ELTSURGR
02639                COMMAREA (DFHCOMMAREA)                             ELTSURGR
02640      END-EXEC.                                                    ELTSURGR
02641                                                                   ELTSURGR
02642      IF IOP-RC-NOTFND                                             ELTSURGR
02643         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTSURGR
02644         EXEC CICS ABEND                                           ELTSURGR
02645                   ABCODE(CIA-ABCODE)                              ELTSURGR
02646         END-EXEC                                                  ELTSURGR
02647      ELSE                                                         ELTSURGR
02648          IF NOT IOP-RC-OK                                         ELTSURGR
02649             SET CIA-AB-CRITIO TO TRUE                             ELTSURGR
02650             EXEC CICS ABEND                                       ELTSURGR
02651                       ABCODE(CIA-ABCODE)                          ELTSURGR
02652             END-EXEC                                              ELTSURGR
02653          END-IF                                                   ELTSURGR
02654      END-IF.                                                      ELTSURGR
02655  9900-EXIT.  EXIT.                                                ELTSURGR
02656 /                                                                 ELTSURGR
02657      COPY ELSTCOMP.                                               ELTSURGR
02658                                                                   ELTSURGR
02659      TITLE 'E.L.S. SURGERY TOPIC ----  ELTSURGR'.                 ELTSURGR
02660                                                                   ELTSURGR
