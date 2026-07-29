00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTDENTS.                                            ELTDENTS
00003  AUTHOR. JOHN CURIN - KEANE.                                         LV002
00004  DATE-WRITTEN.   5/12/86.                                         ELTDENTS
00005  DATE-COMPILED.                                                   ELTDENTS
00006      SKIP3                                                        ELTDENTS
00007 ******************************************************************ELTDENTS
00008 *@>ELTDENTS                                                       ELTDENTS
00009 *@¬                                                               ELTDENTS
00010 *                        PROGRAM ABSTRACT                         ELTDENTS
00011 *                                                                 ELTDENTS
00012 *@¬ PROGRAM NAME:   E.L.S. DENTAL SERVICES TOPIC                  ELTDENTS
00013 *@¬                                                               ELTDENTS
00014 *@¬ PROGRAM I.D.:   ELTDENTS                                      ELTDENTS
00015 *@¬                                                               ELTDENTS
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDENTS
00017 *@¬            DENTAL SERVICES                                    ELTDENTS
00018 *@¬            BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTDENTS
00019 *@¬                                                               ELTDENTS
00020 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DENTAL            ELTDENTS
00021 *@¬            SERVICES AFFORDED A MEMBER BY HIS GROUP.           ELTDENTS
00022 *@¬            THIS INFORMATION IS GOTTEN BY INTEROGATING THE     ELTDENTS
00023 *@¬            BENEFIT PROVISIONS FOR THE GROUP WITHIN THE        ELTDENTS
00024 *@¬            CONTRACT FOR A PARTICULAR RANGE OF DATES.          ELTDENTS
00025 *@¬                                                               ELTDENTS
00026 *@¬ RECORDS                                                       ELTDENTS
00027 *@¬ ACCESSED:  CONTRACT, GROUP SPECIFIC, VARIOUS BENEFIT          ELTDENTS
00028 *@¬            PROVISION, AND A LARGE NUMBER OF DATA ELEMENT      ELTDENTS
00029 *@¬            AND CODE VALUE RECORDS.                            ELTDENTS
00030 *@¬                                                               ELTDENTS
00031 *@¬ PROCESSING                                                    ELTDENTS
00032 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTDENTS
00033 *@¬                                                               ELTDENTS
00034 *@¬                                                               ELTDENTS
00035 ***************************************************************** ELTDENTS
00036 *                                                                 ELTDENTS
00037 *                                                                 ELTDENTS
00038 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*            ELTDENTS
00039 *        *-*     U P D A T E  H I S T O R Y        *-*            ELTDENTS
00040 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*            ELTDENTS
00041 *                                                                 ELTDENTS
00042 **-CHG NUM-* *-DATE-* *WHO* *--------DESCRIPTION-------------     ELTDENTS
00043 *    XXXX    04/28/86  JTC   ORIGINAL IMPLEMENTATION              ELTDENTS
00044 *    0001    08/14/86  JTC   REMOVE THE SETUP OF HEADING LINE 1   ELTDENTS
00045 *                            WS-HDR-1.                            ELTDENTS
00046 *                                                                 ELTDENTS
00047 *    ----    10/06/86  JTC   VS COBOL II CONVERSION               ELTDENTS
00048 *                                                                 ELTDENTS
00049 *    ----    10/19/87  NAC   REWORD PHRASE FOR COVERED BENEFITS.  ELTDENTS
00050 *                                                                 ELTDENTS
00051 *    ----    04/10/89  GEM   STORAGE MANAGEMENT ENHANCEMENTS.     ELTDENTS
00052 *                                                                 ELTDENTS
00053 *         05-FEB-1990  RJL   CORRECTED SECTION FALL-THROUGH       ELTDENTS
00054 *                            ADDED MISSING SWITCH FOR TRANSFER TO ELTDENTS
00055 *                            OTHER RESPONSIBILITY INDICATOR       ELTDENTS
00056 *                                                                 ELTDENTS
00057 *    ---- 14-AUG-1990  GEM   ADDED BENEFIT PROVISIONS.            ELTDENTS
00058 *    ---- 23-AUG-1990  GEM   WS-FIXED-INST-OUTPATIENT BEGAN IN    ELTDENTS
00059 *                            AREA 'A'.                            ELTDENTS
00060 *    ---- 15-NOV-1990  GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND COM-ELTDENTS
00061 *                           PARE TO THE LITERAL ZERO, INSTEAD OF  ELTDENTS
00062 *                           THE DIGIT '0'.                        ELTDENTS
00063 *                                                                 ELTDENTS
00064 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTDENTS
00065 ******************************************************************ELTDENTS
00066 /                                                                 ELTDENTS
00067  ENVIRONMENT DIVISION.                                            ELTDENTS
00068      SKIP3                                                        ELTDENTS
00069  DATA DIVISION.                                                   ELTDENTS
00070  WORKING-STORAGE SECTION.                                         ELTDENTS
00071  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDENTS
00072      '***ELTDENTSWS BEGINS***'.                                   ELTDENTS
00073  01  WS-PARA-COMMENTS.                                            ELTDENTS
00074    05  WS-PARA-ID1               PIC X(4) VALUE 'XXXX'.           ELTDENTS
00075    05  WS-PARA-ID2               PIC X(4) VALUE 'XXXX'.           ELTDENTS
00076                                                                   ELTDENTS
00077  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTDENTS
00078                                                                   ELTDENTS
00079 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDENTS
00080  01  WS-WORK-FIELDS.                                              ELTDENTS
00081      05  WS-CHAR-0                     PIC X.                     ELTDENTS
00082      05  WS-HOLD1                      PIC X(10).                 ELTDENTS
00083      05  WS-HOLD2                      PIC X(10).                 ELTDENTS
00084      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTDENTS
00085      05  WS-COVERED-BASIC              PIC X.                     ELTDENTS
00086      05  WS-COVERED-SUPP               PIC X.                     ELTDENTS
00087      05  WS-BASIC-DENTAL-IND           PIC XX.                    ELTDENTS
00088      05  WS-SUPP-DENTAL-IND            PIC XX.                    ELTDENTS
00089      05  WS-YES                        PIC X     VALUE 'Y'.       ELTDENTS
00090      05  WS-NO                         PIC X     VALUE 'N'.       ELTDENTS
00091      05  WS-4096                       PIC S9(8) VALUE +4096.     ELTDENTS
00092      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTDENTS
00093      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTDENTS
00094      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTDENTS
00095      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTDENTS
00096      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTDENTS
00097      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTDENTS
00098      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDENTS
00099      05  WS-FIRSTTIME-IND              PIC X.                     ELTDENTS
00100        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTDENTS
00101      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTDENTS
00102        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTDENTS
00103      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTDENTS
00104        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTDENTS
00105      05  WS-INDENT-IND                 PIC X.                     ELTDENTS
00106        88  WS-INDENT-ON                    VALUE 'Y'.             ELTDENTS
00107      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTDENTS
00108      05  WS-PRCNT-PERDM-ALLOW          PIC X(9).                  ELTDENTS
00109      05  WS-PERCENT-FLD.                                          ELTDENTS
00110        10  WS-PERCENTAGE               PIC ZZ9.                   ELTDENTS
00111        10  WS-PERCENT-SIGN             PIC X.                     ELTDENTS
00112                                                                   ELTDENTS
00113 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTDENTS
00114  01  WS-BEN-PROV-ID.                                              ELTDENTS
00115      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +04.ELTDENTS
00116      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +04.ELTDENTS
00117      05  WS-PROF-IP-TAB.                                          ELTDENTS
00118        10  FILLER                      PIC X(6)  VALUE 'DENI C'.  ELTDENTS
00119        10  FILLER                      PIC X(6)  VALUE 'PRIJ C'.  ELTDENTS
00120        10  FILLER                      PIC X(6)  VALUE 'PRIK C'.  ELTDENTS
00121        10  FILLER                      PIC X(6)  VALUE 'PRIL C'.  ELTDENTS
00122      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTDENTS
00123                                        PIC X(6)  OCCURS 4 TIMES.  ELTDENTS
00124                                                                   ELTDENTS
00125      05  WS-PROF-OP-CNT                PIC S9(4)  COMP  VALUE +04.ELTDENTS
00126      05  WS-PROF-OP-TAB.                                          ELTDENTS
00127        10  FILLER                      PIC X(6)  VALUE 'DENO C'.  ELTDENTS
00128        10  FILLER                      PIC X(6)  VALUE 'PRIJ C'.  ELTDENTS
00129        10  FILLER                      PIC X(6)  VALUE 'PRIK C'.  ELTDENTS
00130        10  FILLER                      PIC X(6)  VALUE 'PRIL C'.  ELTDENTS
00131      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTDENTS
00132                                        PIC X(6)  OCCURS 4 TIMES.  ELTDENTS
00133                                                                   ELTDENTS
00134 /            D I S P L A Y   L I N E S                            ELTDENTS
00135  01  WS-ELS-DISPLAY-LINES.                                        ELTDENTS
00136                                                                   ELTDENTS
00137    05  WS-HDR-2-PROF-IP.                                          ELTDENTS
00138      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDENTS
00139      10  FILLER                    PIC X(38)                      ELTDENTS
00140          VALUE 'DENTAL SERVICES INPATIENT PROFESSIONAL'.          ELTDENTS
00141      10  FILLER                    PIC X(25) VALUE LOW-VALUES.    ELTDENTS
00142                                                                   ELTDENTS
00143    05  WS-HDR-2-PROF-OP.                                          ELTDENTS
00144      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDENTS
00145      10  FILLER                    PIC X(39)                      ELTDENTS
00146          VALUE 'DENTAL SERVICES OUTPATIENT PROFESSIONAL'.         ELTDENTS
00147      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTDENTS
00148                                                                   ELTDENTS
00149    05  WS-HDR-2-INST-IP.                                          ELTDENTS
00150      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDENTS
00151      10  FILLER                    PIC X(39)                      ELTDENTS
00152          VALUE 'DENTAL SERVICES INPATIENT INSTITUTIONAL'.         ELTDENTS
00153      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTDENTS
00154                                                                   ELTDENTS
00155    05  WS-HDR-2-INST-OP.                                          ELTDENTS
00156      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDENTS
00157      10  FILLER                    PIC X(40)                      ELTDENTS
00158          VALUE 'DENTAL SERVICES OUTPATIENT INSTITUTIONAL'.        ELTDENTS
00159      10  FILLER                    PIC X(23) VALUE LOW-VALUES.    ELTDENTS
00160                                                                   ELTDENTS
00161    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTDENTS
00162          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTDENTS
00163                                                                   ELTDENTS
00164    05  WS-FOLLOWING-BEN.                                          ELTDENTS
00165      10  FILLER                    PIC X(21)                      ELTDENTS
00166          VALUE 'COVERED SERVICES ARE:'.                           ELTDENTS
00167                                                                   ELTDENTS
00168    05  WS-PAY-CONSDR-TEXT1.                                       ELTDENTS
00169      10  FILLER                    PIC X(45)                      ELTDENTS
00170        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDENTS
00171                                                                   ELTDENTS
00172    05  WS-PAY-CONSDR-TEXT2.                                       ELTDENTS
00173      10  FILLER                    PIC X(45)                      ELTDENTS
00174        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDENTS
00175                                                                   ELTDENTS
00176    05  WS-PAYMNT-BASED.                                           ELTDENTS
00177      10  FILLER                    PIC X(20)                      ELTDENTS
00178          VALUE 'PAYMENT IS BASED ON:'.                            ELTDENTS
00179      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTDENTS
00180                                                                   ELTDENTS
00181    05  WS-BASIC.                                                  ELTDENTS
00182      10  WS-BASIC-LIT              PIC X(16)                      ELTDENTS
00183          VALUE '         BASIC: '.                                ELTDENTS
00184      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTDENTS
00185                                                                   ELTDENTS
00186    05  WS-SUPPLEMENTAL.                                           ELTDENTS
00187      10  WS-SUPP-LIT               PIC X(16)                      ELTDENTS
00188          VALUE '  SUPPLEMENTAL: '.                                ELTDENTS
00189                                                                   ELTDENTS
00190    05  WS-BASIC-LIT-LEFT           PIC X(08) VALUE                ELTDENTS
00191        '  BASIC:'.                                                ELTDENTS
00192                                                                   ELTDENTS
00193    05  WS-INDENTED.                                               ELTDENTS
00194      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDENTS
00195      10  WS-DTL-INDENTED           PIC X(63) VALUE SPACES.        ELTDENTS
00196                                                                   ELTDENTS
00197    05  WS-INDENTED-FOUR.                                          ELTDENTS
00198      10  FILLER                    PIC X(04) VALUE SPACES.        ELTDENTS
00199      10  WS-DTL-INDENTED-FOUR      PIC X(75) VALUE SPACES.        ELTDENTS
00200                                                                   ELTDENTS
00201    05  WS-PAYABLE-AS.                                             ELTDENTS
00202      10  FILLER                    PIC X(45)                      ELTDENTS
00203          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTDENTS
00204      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDENTS
00205                                                                   ELTDENTS
00206    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTDENTS
00207        'SPILLOVER COINSURANCE: '.                                 ELTDENTS
00208                                                                   ELTDENTS
00209    05  WS-SPILLOVER-DEDUCT         PIC X(22)  VALUE               ELTDENTS
00210        'SPILLOVER DEDUCTIBLE: '.                                  ELTDENTS
00211                                                                   ELTDENTS
00212    05  WS-FIXED-INST-INPATIENT     PIC X(53) VALUE                ELTDENTS
00213        'SEE ROOM AND BOARD FOR ADDITIONAL INPATIENT BENEFITS.'.   ELTDENTS
00214                                                                   ELTDENTS
00215    05  WS-FIXED-INST-OUTPATIENT.                                  ELTDENTS
00216        10  FILLER                  PIC X(27) VALUE                ELTDENTS
00217            'SEE OUTPATIENT SURGERY FOR '.                         ELTDENTS
00218        10  FILLER                  PIC X(31) VALUE                ELTDENTS
00219            'ADDITIONAL OUTPATIENT BENEFITS.'.                     ELTDENTS
00220                                                                   ELTDENTS
00221    05  WS-COVERED                  PIC X(38) VALUE                ELTDENTS
00222        'DENTAL SURGICAL SERVICES ARE COVERED.'.                   ELTDENTS
00223                                                                   ELTDENTS
00224    05  WS-NOT-COVERED              PIC X(42) VALUE                ELTDENTS
00225        'DENTAL SURGICAL SERVICES ARE NOT COVERED.'.               ELTDENTS
00226                                                                   ELTDENTS
00227    05  WS-INPATIENT-DENT-IND       PIC X(35)  VALUE               ELTDENTS
00228        'IF HOSPTIALIZED, DENTAL SURGERY IS:'.                     ELTDENTS
00229                                                                   ELTDENTS
00230    05  WS-OUTPATIENT-DENT-IND      PIC X(43)  VALUE               ELTDENTS
00231        'ELIGIBILITY FOR OUTPATIENT DENTAL SERVICES:'.             ELTDENTS
00232                                                                   ELTDENTS
00233    05  WS-CONTRACT-RELATED.                                       ELTDENTS
00234      10  FILLER                    PIC X(49)                      ELTDENTS
00235        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTDENTS
00236      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDENTS
00237                                                                   ELTDENTS
00238    05  WS-PVE-TEXT.                                               ELTDENTS
00239      10  FILLER                    PIC X(44)  VALUE               ELTDENTS
00240        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTDENTS
00241                                                                   ELTDENTS
00242    05  WS-NO-TABULAR1.                                            ELTDENTS
00243      10  FILLER                    PIC X(51)  VALUE               ELTDENTS
00244         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTDENTS
00245      10  FILLER                    PIC X(22)  VALUE               ELTDENTS
00246         'GOING FROM BENEFIT ***'.                                 ELTDENTS
00247                                                                   ELTDENTS
00248    05  WS-NO-TABULAR2.                                            ELTDENTS
00249      10  FILLER                    PIC X(15)  VALUE               ELTDENTS
00250         '*** PROVISION: '.                                        ELTDENTS
00251      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTDENTS
00252      10  FILLER                    PIC X VALUE SPACE.             ELTDENTS
00253      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTDENTS
00254      10  FILLER                    PIC X(13)  VALUE               ELTDENTS
00255         ' TO TABULAR: '.                                          ELTDENTS
00256      10  WS-NO-TAB-ID              PIC X(6).                      ELTDENTS
00257      10  FILLER                    PIC X VALUE SPACE.             ELTDENTS
00258      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTDENTS
00259      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTDENTS
00260                                                                   ELTDENTS
00261    05  WS-PGM-ERROR.                                              ELTDENTS
00262      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTDENTS
00263      10  FILLER                    PIC X(35)  VALUE               ELTDENTS
00264         '***  P R O G R A M   E R R O R  ***'.                    ELTDENTS
00265      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTDENTS
00266                                                                   ELTDENTS
00267    05  WS-BAD-INST-PROF-SEL.                                      ELTDENTS
00268      10  FILLER                    PIC XX VALUE SPACE.            ELTDENTS
00269      10  FILLER                    PIC X(47) VALUE                ELTDENTS
00270         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTDENTS
00271      10  FILLER                    PIC X(48) VALUE                ELTDENTS
00272         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTDENTS
00273      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTDENTS
00274                                                                   ELTDENTS
00275    05  WS-BAD-IN-OUT-SEL.                                         ELTDENTS
00276      10  FILLER                    PIC X(08) VALUE SPACE.         ELTDENTS
00277      10  FILLER                    PIC X(51) VALUE                ELTDENTS
00278         '*** I N V A L I D   I N P U T   /   O U T P U T ***'.    ELTDENTS
00279      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTDENTS
00280                                                                   ELTDENTS
00281    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTDENTS
00282       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTDENTS
00283                                                                   ELTDENTS
00284    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTDENTS
00285    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTDENTS
00286      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTDENTS
00287                                                                   ELTDENTS
00288  01  WS-END                            PIC X(16)  VALUE           ELTDENTS
00289      '*** W/S ENDS ***'.                                          ELTDENTS
00290 /             L I N K A G E   S E C T I O N                       ELTDENTS
00291  LINKAGE SECTION.                                                 ELTDENTS
00292  01  DFHCOMMAREA.                                                 ELTDENTS
00293      COPY ELSCOMMC.                                               ELTDENTS
00294 /  *** CIA  AREA ***                                              ELTDENTS
00295      COPY ELSCIA2C.                                               ELTDENTS
00296 /  *** IO PARM AREA ***                                           ELTDENTS
00297      COPY ELSIOPMC.                                               ELTDENTS
00298 /  *** KEY AREA ***                                               ELTDENTS
00299      COPY ELSKEYSC.                                               ELTDENTS
00300 /  *** OUTPUT TEXT AREA ***                                       ELTDENTS
00301      COPY ELSOUTPC.                                               ELTDENTS
00302 /  *** TOPIC SELECTION AREA ***                                   ELTDENTS
00303      COPY ELSSSCBC.                                               ELTDENTS
00304 /  *** CODE MANUAL INTERFACE ***                                  ELTDENTS
00305      COPY ELSCMIFC.                                               ELTDENTS
00306 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDENTS
00307      COPY ELSCMDSC.                                               ELTDENTS
00308 /  *** BENEFIT PROVISION TABLE ***                                ELTDENTS
00309      COPY ELSPRVNC.                                               ELTDENTS
00310 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDENTS
00311      COPY ELSTCWAC.                                               ELTDENTS
00312 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDENTS
00313      COPY ELSPLGSW.                                               ELTDENTS
00314                                                                   ELTDENTS
00315 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDENTS
00316      COPY ELSPLGTB.                                               ELTDENTS
00317 /        C O N T R A C T   R E C O R D                            ELTDENTS
00318  01  CONTRACT-RECORD.                                             ELTDENTS
00319      COPY GCCONTRC.                                               ELTDENTS
00320 /                  M A I N L I N E                                ELTDENTS
00321  PROCEDURE DIVISION.                                              ELTDENTS
00322                                                                   ELTDENTS
00323 ******************************************************************ELTDENTS
00324 *                                                                 ELTDENTS
00325 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDENTS
00326 *                                                                 ELTDENTS
00327 ******************************************************************ELTDENTS
00328  0000-MAINLINE.                                                   ELTDENTS
00329                                                                   ELTDENTS
00330      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDENTS
00331         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTDENTS
00332         EXEC CICS  ABEND ABCODE(CIA-ABCODE)  END-EXEC.            ELTDENTS
00333                                                                   ELTDENTS
00334      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDENTS
00335          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTDENTS
00336                                                                   ELTDENTS
00337      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDENTS
00338      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00339          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDENTS
00340                                                                   ELTDENTS
00341      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDENTS
00342      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00343          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDENTS
00344                                                                   ELTDENTS
00345      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDENTS
00346      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00347          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDENTS
00348                                                                   ELTDENTS
00349      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDENTS
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00351          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDENTS
00352                                                                   ELTDENTS
00353      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDENTS
00354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00355          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDENTS
00356                                                                   ELTDENTS
00357      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDENTS
00358      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00359          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDENTS
00360                                                                   ELTDENTS
00361      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDENTS
00362                                                                   ELTDENTS
00363      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDENTS
00364              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDENTS
00365                                                                   ELTDENTS
00366      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTDENTS
00367                                                                   ELTDENTS
00368      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDENTS
00369                                                                   ELTDENTS
00370      EXEC CICS LINK                                               ELTDENTS
00371                PROGRAM('ELUSTGMG')                                ELTDENTS
00372                COMMAREA(DFHCOMMAREA)                              ELTDENTS
00373      END-EXEC.                                                    ELTDENTS
00374                                                                   ELTDENTS
00375      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDENTS
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00377          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDENTS
00378                                                                   ELTDENTS
00379      MOVE 'N'         TO  WS-INDENT-IND.                          ELTDENTS
00380      MOVE '0'  TO  WS-CHAR-0.                                     ELTDENTS
00381                                                                   ELTDENTS
00382      IF (SSB-PROV-CLASS-INST OR  SSB-PROV-CLASS-BOTH) AND         ELTDENTS
00383        (SSB-SERV-CLASS-IP OR SSB-SERV-CLASS-BOTH)                 ELTDENTS
00384         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTDENTS
00385                                                                   ELTDENTS
00386      IF (SSB-PROV-CLASS-PROF OR  SSB-PROV-CLASS-BOTH) AND         ELTDENTS
00387        (SSB-SERV-CLASS-IP OR SSB-SERV-CLASS-BOTH)                 ELTDENTS
00388         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTDENTS
00389                                                                   ELTDENTS
00390      IF (SSB-PROV-CLASS-INST OR  SSB-PROV-CLASS-BOTH) AND         ELTDENTS
00391        (SSB-SERV-CLASS-OP OR SSB-SERV-CLASS-BOTH)                 ELTDENTS
00392         PERFORM 3000-INSTITUTIONAL-OP-RTNE.                       ELTDENTS
00393                                                                   ELTDENTS
00394      IF (SSB-PROV-CLASS-PROF OR  SSB-PROV-CLASS-BOTH) AND         ELTDENTS
00395        (SSB-SERV-CLASS-OP OR SSB-SERV-CLASS-BOTH)                 ELTDENTS
00396         PERFORM 4000-PROFESSIONAL-OP-RTNE.                        ELTDENTS
00397                                                                   ELTDENTS
00398                                                                   ELTDENTS
00399      IF NOT SSB-PROV-CLASS-INST AND  NOT SSB-PROV-CLASS-PROF      ELTDENTS
00400                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTDENTS
00401         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTDENTS
00402         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTDENTS
00403         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTDENTS
00404         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTDENTS
00405         MOVE SPACE  TO  COF-FUNCTION                              ELTDENTS
00406         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDENTS
00407               COMMAREA(DFHCOMMAREA)                               ELTDENTS
00408         END-EXEC.                                                 ELTDENTS
00409                                                                   ELTDENTS
00410      IF NOT SSB-SERV-CLASS-IP AND NOT SSB-SERV-CLASS-OP           ELTDENTS
00411            AND NOT SSB-SERV-CLASS-BOTH                            ELTDENTS
00412         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTDENTS
00413         MOVE WS-BAD-IN-OUT-SEL  TO  COF-DTL-LINE(5)               ELTDENTS
00414         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTDENTS
00415         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTDENTS
00416         MOVE SPACE  TO  COF-FUNCTION                              ELTDENTS
00417         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDENTS
00418               COMMAREA(DFHCOMMAREA)                               ELTDENTS
00419         END-EXEC.                                                 ELTDENTS
00420                                                                   ELTDENTS
00421                                                                   ELTDENTS
00422      MOVE 'E'  TO  COF-FUNCTION.                                  ELTDENTS
00423      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDENTS
00424                     COF-NBR-DTL-LINES.                            ELTDENTS
00425      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00426      END-EXEC.                                                    ELTDENTS
00427                                                                   ELTDENTS
00428                                                                   ELTDENTS
00429  0099-RETURN.                                                     ELTDENTS
00430      EXEC CICS RETURN   END-EXEC.                                 ELTDENTS
00431                                                                   ELTDENTS
00432      GOBACK.                                                      ELTDENTS
00433 /        I N S T I T U T I O N A L   I P   R T N E                ELTDENTS
00434 ***************************************************************** ELTDENTS
00435 *        I N S T I T U T I O N A L   I P   R T N E                ELTDENTS
00436 *                                                                 ELTDENTS
00437 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDENTS
00438 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDENTS
00439 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDENTS
00440 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDENTS
00441 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDENTS
00442 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDENTS
00443 *  MODULE.                                                        ELTDENTS
00444 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDENTS
00445 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDENTS
00446 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDENTS
00447 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDENTS
00448 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDENTS
00449 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDENTS
00450 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDENTS
00451 *                                                                 ELTDENTS
00452 ***************************************************************** ELTDENTS
00453  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTDENTS
00454      MOVE '1000'  TO  WS-PARA-ID1.                                ELTDENTS
00455                                                                   ELTDENTS
00456      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
00457      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDENTS
00458      MOVE ZERO  TO  COF-NBR-DTL-LINES.                            ELTDENTS
00459      MOVE WS-HDR-2-INST-IP  TO  COF-HDR-LINE(2).                  ELTDENTS
00460                                                                   ELTDENTS
00461      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00462      END-EXEC.                                                    ELTDENTS
00463                                                                   ELTDENTS
00464      MOVE +1  TO  WS-CIA.                                         ELTDENTS
00465                                                                   ELTDENTS
00466      PERFORM 5200-DETER-INPAT-COVER.                              ELTDENTS
00467                                                                   ELTDENTS
00468      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
00469          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
00470              MOVE WS-COVERED   TO COF-DTL-LINE(WS-CIA)            ELTDENTS
00471              ADD +2            TO WS-CIA                          ELTDENTS
00472              MOVE WS-INPATIENT-DENT-IND                           ELTDENTS
00473                                TO COF-DTL-LINE(WS-CIA)            ELTDENTS
00474              ADD +1            TO WS-CIA                          ELTDENTS
00475      ELSE                                                         ELTDENTS
00476       ADD +1              TO WS-CIA                               ELTDENTS
00477       MOVE WS-NOT-COVERED TO COF-DTL-LINE(WS-CIA)                 ELTDENTS
00478       PERFORM 8000-OUTPUT-TEXT                                    ELTDENTS
00479       GO TO 1099-EXIT.                                            ELTDENTS
00480                                                                   ELTDENTS
00481      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
00482          MOVE WS-BASIC-LIT-LEFT TO COF-DTL-LINE(WS-CIA)           ELTDENTS
00483          ADD +2                 TO WS-CIA                         ELTDENTS
00484          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00485          MOVE 'CONTRACT'        TO CMF-RECORD-PREFIX              ELTDENTS
00486          MOVE 'DENT-SURG-PMT-ELIG-IP-IND'                         ELTDENTS
00487                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTDENTS
00488          MOVE WS-BASIC-DENTAL-IND                                 ELTDENTS
00489                                 TO CMF-CODE-VALUE                 ELTDENTS
00490          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
00491                                                                   ELTDENTS
00492      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
00493        IF WS-CIA > 17                                             ELTDENTS
00494          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00495        ELSE                                                       ELTDENTS
00496         ADD +1  TO WS-CIA.                                        ELTDENTS
00497                                                                   ELTDENTS
00498      IF WS-COVERED-SUPP = WS-YES                                  ELTDENTS
00499          MOVE WS-SUPP-LIT  TO COF-DTL-LINE(WS-CIA)                ELTDENTS
00500          ADD +2            TO WS-CIA                              ELTDENTS
00501          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00502          MOVE 'CONTRACT'   TO CMF-RECORD-PREFIX                   ELTDENTS
00503          MOVE 'DENT-SURG-PMT-ELIG-IP-IND'                         ELTDENTS
00504                            TO CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
00505          MOVE WS-SUPP-DENTAL-IND                                  ELTDENTS
00506                            TO CMF-CODE-VALUE                      ELTDENTS
00507          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
00508                                                                   ELTDENTS
00509      IF WS-CIA > 15                                               ELTDENTS
00510          PERFORM 8000-OUTPUT-TEXT.                                ELTDENTS
00511                                                                   ELTDENTS
00512      ADD  +1                      TO WS-CIA.                      ELTDENTS
00513      MOVE WS-FIXED-INST-INPATIENT TO COF-DTL-LINE(WS-CIA).        ELTDENTS
00514                                                                   ELTDENTS
00515      ADD +2            TO WS-CIA.                                 ELTDENTS
00516      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDENTS
00517      ADD +2            TO WS-CIA.                                 ELTDENTS
00518                                                                   ELTDENTS
00519      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
00520                                                                   ELTDENTS
00521      INITIALIZE TCAR-FROM-AREA.                                   ELTDENTS
00522      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDENTS
00523             WS-PAY-CONSDR-TEXT2                                   ELTDENTS
00524                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDENTS
00525      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDENTS
00526      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDENTS
00527      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDENTS
00528                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDENTS
00529      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDENTS
00530      ADD +1                TO  WS-CIA.                            ELTDENTS
00531      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
00532      ADD +1                TO  WS-CIA.                            ELTDENTS
00533      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
00534                                                                   ELTDENTS
00535      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
00536                                                                   ELTDENTS
00537  1099-EXIT.            EXIT.                                      ELTDENTS
00538 /        P R O F E S S I O N A L   I P   R T N E                  ELTDENTS
00539 ***************************************************************** ELTDENTS
00540 *        P R O F E S S I O N A L   I P   R T N E                  ELTDENTS
00541 *                                                                 ELTDENTS
00542 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDENTS
00543 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDENTS
00544 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDENTS
00545 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDENTS
00546 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDENTS
00547 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDENTS
00548 *  MODULE.                                                        ELTDENTS
00549 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDENTS
00550 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDENTS
00551 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDENTS
00552 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDENTS
00553 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDENTS
00554 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDENTS
00555 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDENTS
00556 *                                                                 ELTDENTS
00557 ***************************************************************** ELTDENTS
00558  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTDENTS
00559      MOVE '2000'  TO  WS-PARA-ID1.                                ELTDENTS
00560                                                                   ELTDENTS
00561      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
00562      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTDENTS
00563      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDENTS
00564                     COF-NBR-DTL-LINES.                            ELTDENTS
00565      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00566      END-EXEC.                                                    ELTDENTS
00567      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDENTS
00568      MOVE WS-HDR-2-PROF-IP  TO  COF-HDR-LINE(2).                  ELTDENTS
00569      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDENTS
00570      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTDENTS
00571         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDENTS
00572         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTDENTS
00573                                                                   ELTDENTS
00574      GO TO 2020-CALL-COVERAGE.                                    ELTDENTS
00575  2010-MOVE-IN-PROF-IP.                                            ELTDENTS
00576      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDENTS
00577      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTDENTS
00578                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDENTS
00579      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTDENTS
00580                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTDENTS
00581                                                                   ELTDENTS
00582  2020-CALL-COVERAGE.                                              ELTDENTS
00583      MOVE '2020'  TO  WS-PARA-ID1.                                ELTDENTS
00584      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDENTS
00585      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00586      END-EXEC.                                                    ELTDENTS
00587                                                                   ELTDENTS
00588      MOVE 'DENTAL SURGICAL SERVICES ARE ' TO SSB-TOPIC-PHRASE.    ELTDENTS
00589                                                                   ELTDENTS
00590      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDENTS
00591      END-EXEC.                                                    ELTDENTS
00592                                                                   ELTDENTS
00593      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTDENTS
00594      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00595      END-EXEC.                                                    ELTDENTS
00596                                                                   ELTDENTS
00597      IF PVN-COVG-NONE                                             ELTDENTS
00598         GO TO 2099-EXIT.                                          ELTDENTS
00599                                                                   ELTDENTS
00600      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDENTS
00601                                                                   ELTDENTS
00602      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDENTS
00603            PSP-PROVN-PRICING-METHD,                               ELTDENTS
00604            PSP-TRANSF-OTHER-RESP-IND,                             ELTDENTS
00605            PSP-DENT-SURG-PMT-ELG-IP-IND,                          ELTDENTS
00606            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDENTS
00607            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDENTS
00608            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDENTS
00609            PSP-SPILL-OVER-DED-APL-IND,                            ELTDENTS
00610            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDENTS
00611            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDENTS
00612            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDENTS
00613            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDENTS
00614            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDENTS
00615            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDENTS
00616            PSC-BEN-SCOPE-ID.                                      ELTDENTS
00617                                                                   ELTDENTS
00618      MOVE '0' TO  PSP-DENT-SURG-PMT-ELG-OP-IND.                   ELTDENTS
00619                                                                   ELTDENTS
00620      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDENTS
00621      END-EXEC.                                                    ELTDENTS
00622                                                                   ELTDENTS
00623      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDENTS
00624      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
00625          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDENTS
00626                                                                   ELTDENTS
00627      PERFORM 2030-FIND-FIRST-NONZERO                              ELTDENTS
00628         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDENTS
00629         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTDENTS
00630                                                                   ELTDENTS
00631      GO TO 2099-EXIT.                                             ELTDENTS
00632  2030-FIND-FIRST-NONZERO.                                         ELTDENTS
00633      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDENTS
00634      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDENTS
00635         NEXT SENTENCE                                             ELTDENTS
00636      ELSE                                                         ELTDENTS
00637         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTDENTS
00638                                                                   ELTDENTS
00639  2040-BUILD-SCREEN-LINES.                                         ELTDENTS
00640      MOVE '2040'  TO  WS-PARA-ID1.                                ELTDENTS
00641                                                                   ELTDENTS
00642      SET PLT-INDEX1   TO                                          ELTDENTS
00643                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTDENTS
00644      IF WS-NOT-FIRST-TIME                                         ELTDENTS
00645         MOVE 'P'  TO  COF-FUNCTION                                ELTDENTS
00646         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDENTS
00647             COMMAREA(DFHCOMMAREA)                                 ELTDENTS
00648         END-EXEC                                                  ELTDENTS
00649      ELSE                                                         ELTDENTS
00650         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTDENTS
00651                                                                   ELTDENTS
00652      MOVE +1  TO  WS-CIA.                                         ELTDENTS
00653      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDENTS
00654         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDENTS
00655            SET PLT-INDEX2  TO  2                                  ELTDENTS
00656         ELSE                                                      ELTDENTS
00657            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTDENTS
00658            GO TO 2099-EXIT                                        ELTDENTS
00659      ELSE                                                         ELTDENTS
00660         SET PLT-INDEX2  TO  1.                                    ELTDENTS
00661                                                                   ELTDENTS
00662 **---------------------------------------------------------------+ELTDENTS
00663 **                                                               |ELTDENTS
00664 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTDENTS
00665      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTDENTS
00666      ADD  +1  TO  WS-CIA.                                         ELTDENTS
00667      MOVE ZERO  TO  WS-SUB2.                                      ELTDENTS
00668      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDENTS
00669      MOVE '2050'  TO  WS-PARA-ID1.                                ELTDENTS
00670                                                                   ELTDENTS
00671      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTDENTS
00672         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTDENTS
00673         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTDENTS
00674      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDENTS
00675                                                                   ELTDENTS
00676      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTDENTS
00677      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00678      END-EXEC.                                                    ELTDENTS
00679      MOVE +1  TO  WS-CIA.                                         ELTDENTS
00680 **                                                               |ELTDENTS
00681 **---------------------------------------------------------------+ELTDENTS
00682                                                                   ELTDENTS
00683      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00684      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00685        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDENTS
00686               NOT = ZERO                                          ELTDENTS
00687         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTDENTS
00688         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00689         ADD +1  TO  WS-CIA.                                       ELTDENTS
00690                                                                   ELTDENTS
00691      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
00692      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00693       AND  NOT WS-ADD-A-BLANK-LINE                                ELTDENTS
00694        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDENTS
00695               NOT = ZERO                                          ELTDENTS
00696         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTDENTS
00697         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00698         ADD +1  TO  WS-CIA.                                       ELTDENTS
00699                                                                   ELTDENTS
00700      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00701             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTDENTS
00702                                                                   ELTDENTS
00703      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00704             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTDENTS
00705                                                                   ELTDENTS
00706      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
00707          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTDENTS
00708          ADD  +1   TO  WS-CIA                                     ELTDENTS
00709          PERFORM 8000-OUTPUT-TEXT.                                ELTDENTS
00710                                                                   ELTDENTS
00711 **---------------------------------------------------------------+ELTDENTS
00712 **                                                               |ELTDENTS
00713 **            D E N T A L   I N D I C A T O R                    |ELTDENTS
00714                                                                   ELTDENTS
00715      MOVE WS-NO  TO WS-COVERED-BASIC,                             ELTDENTS
00716                     WS-COVERED-SUPP.                              ELTDENTS
00717                                                                   ELTDENTS
00718      SET PLT-INDEX2  TO  1.                                       ELTDENTS
00719      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00720        IF PLP-DENT-SURG-PMT-ELG-IP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
00721                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
00722                  MOVE WS-YES TO WS-COVERED-BASIC.                 ELTDENTS
00723                                                                   ELTDENTS
00724      SET PLT-INDEX2  TO  2.                                       ELTDENTS
00725      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00726        IF PLP-DENT-SURG-PMT-ELG-IP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
00727                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
00728                  MOVE WS-YES TO WS-COVERED-SUPP.                  ELTDENTS
00729                                                                   ELTDENTS
00730      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
00731          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
00732              ADD +1            TO WS-CIA                          ELTDENTS
00733              MOVE WS-INPATIENT-DENT-IND                           ELTDENTS
00734                                TO COF-DTL-LINE(WS-CIA)            ELTDENTS
00735              ADD +1            TO WS-CIA.                         ELTDENTS
00736                                                                   ELTDENTS
00737      SET PLT-INDEX2  TO  1.                                       ELTDENTS
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00739        IF PLP-DENT-SURG-PMT-ELG-IP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
00740                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
00741          MOVE WS-BASIC-LIT-LEFT TO COF-DTL-LINE(WS-CIA)           ELTDENTS
00742          ADD  +2                TO WS-CIA                         ELTDENTS
00743          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00744          MOVE 'BP'              TO CMF-RECORD-PREFIX              ELTDENTS
00745          MOVE 'DENT-SURG-PMT-ELG-IP-IND'                          ELTDENTS
00746                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTDENTS
00747          MOVE PLP-DENT-SURG-PMT-ELG-IP-IND(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00748                                 TO CMF-CODE-VALUE                 ELTDENTS
00749          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
00750                                                                   ELTDENTS
00751      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
00752        IF WS-CIA > 17                                             ELTDENTS
00753          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00754        ELSE                                                       ELTDENTS
00755         ADD +1  TO WS-CIA.                                        ELTDENTS
00756                                                                   ELTDENTS
00757      SET PLT-INDEX2  TO  2.                                       ELTDENTS
00758      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00759        IF PLP-DENT-SURG-PMT-ELG-IP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
00760                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
00761          MOVE WS-SUPP-LIT  TO COF-DTL-LINE(WS-CIA)                ELTDENTS
00762          ADD  +2           TO WS-CIA                              ELTDENTS
00763          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
00764          MOVE 'BP'         TO CMF-RECORD-PREFIX                   ELTDENTS
00765          MOVE 'DENT-SURG-PMT-ELG-IP-IND'                          ELTDENTS
00766                            TO CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
00767          MOVE PLP-DENT-SURG-PMT-ELG-IP-IND(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00768                            TO CMF-CODE-VALUE                      ELTDENTS
00769          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
00770                                                                   ELTDENTS
00771      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
00772          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
00773               PERFORM 8000-OUTPUT-TEXT.                           ELTDENTS
00774                                                                   ELTDENTS
00775 **                                                               |ELTDENTS
00776 **---------------------------------------------------------------+ELTDENTS
00777                                                                   ELTDENTS
00778 **---------------------------------------------------------------+ELTDENTS
00779 **                                                               |ELTDENTS
00780 **            B E N E F I T   S C O P E   I D                    |ELTDENTS
00781      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00782      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00783        IF NOT WS-ADD-A-BLANK-LINE                                 ELTDENTS
00784            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTDENTS
00785                                         '0000' AND  NOT =  '00  ' ELTDENTS
00786               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTDENTS
00787               ADD  +1  TO  WS-CIA                                 ELTDENTS
00788               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTDENTS
00789                                                                   ELTDENTS
00790      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
00791      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00792        IF NOT WS-ADD-A-BLANK-LINE                                 ELTDENTS
00793            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTDENTS
00794                                         '0000' AND  NOT =  '00  ' ELTDENTS
00795               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTDENTS
00796               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTDENTS
00797               ADD  +1  TO  WS-CIA.                                ELTDENTS
00798                                                                   ELTDENTS
00799      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00800      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00801        IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =         ELTDENTS
00802                                         '0000' AND  NOT =  '00  ' ELTDENTS
00803               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTDENTS
00804               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTDENTS
00805               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTDENTS
00806                                                    CMF-CODE-VALUE ELTDENTS
00807               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTDENTS
00808               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTDENTS
00809               MOVE 'Y'          TO WS-INDENT-IND                  ELTDENTS
00810               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTDENTS
00811                                                                   ELTDENTS
00812      SET PLT-INDEX2  TO  2.                                       ELTDENTS
00813      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00814          IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =       ELTDENTS
00815                                         '0000' AND  NOT =  '00  ' ELTDENTS
00816               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTDENTS
00817               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTDENTS
00818               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTDENTS
00819                                                    CMF-CODE-VALUE ELTDENTS
00820               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTDENTS
00821               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTDENTS
00822               MOVE 'Y'          TO WS-INDENT-IND                  ELTDENTS
00823               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTDENTS
00824                                                                   ELTDENTS
00825      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
00826         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00827         ADD  +1   TO  WS-CIA                                      ELTDENTS
00828         PERFORM 8000-OUTPUT-TEXT.                                 ELTDENTS
00829 **                                                               |ELTDENTS
00830 **---------------------------------------------------------------+ELTDENTS
00831                                                                   ELTDENTS
00832 **---------------------------------------------------------------+ELTDENTS
00833 **                                                               |ELTDENTS
00834 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTDENTS
00835 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTDENTS
00836 **     A D D I T I O N A L   P R I C I N G   P E R C E N T       |ELTDENTS
00837      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00838      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
00839         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
00840                                                              '19' ELTDENTS
00841         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDENTS
00842         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00843         ADD +1  TO  WS-CIA.                                       ELTDENTS
00844                                                                   ELTDENTS
00845      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
00846      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDENTS
00847         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
00848                                                        '19' AND   ELTDENTS
00849         NOT WS-ADD-A-BLANK-LINE                                   ELTDENTS
00850         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDENTS
00851         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00852         ADD +1  TO  WS-CIA.                                       ELTDENTS
00853                                                                   ELTDENTS
00854      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00855      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
00856         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDENTS
00857                            AND                                    ELTDENTS
00858         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00859         SET  PLT-INDEX2  TO  2                                    ELTDENTS
00860         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDENTS
00861                                                             ZERO  ELTDENTS
00862            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDENTS
00863            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDENTS
00864            ADD +1  TO  WS-CIA.                                    ELTDENTS
00865                                                                   ELTDENTS
00866      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00867      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
00868         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDENTS
00869                            AND                                    ELTDENTS
00870         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTDENTS
00871         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTDENTS
00872         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTDENTS
00873         ADD +1  TO  WS-CIA.                                       ELTDENTS
00874                                                                   ELTDENTS
00875      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTDENTS
00876         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00877         SET  PLT-INDEX2  TO  2                                    ELTDENTS
00878         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDENTS
00879                                                             ZERO  ELTDENTS
00880            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDENTS
00881            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDENTS
00882            ADD +1  TO  WS-CIA.                                    ELTDENTS
00883                                                                   ELTDENTS
00884      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
00885      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
00886         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTDENTS
00887                                                            =  ZEROELTDENTS
00888            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00889                                                            =  ZEROELTDENTS
00890               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTDENTS
00891            ELSE                                                   ELTDENTS
00892               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDENTS
00893          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00894                                                  TO  WS-PERCENTAGEELTDENTS
00895          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTDENTS
00896         ELSE                                                      ELTDENTS
00897          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTDENTS
00898          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00899                                                 TO  WS-PERCENTAGE ELTDENTS
00900          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTDENTS
00901                                                                   ELTDENTS
00902                                                                   ELTDENTS
00903      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
00904         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
00905                                             ZERO AND  NOT =  '19' ELTDENTS
00906         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
00907         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDENTS
00908         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDENTS
00909                                                    CMF-CODE-VALUE ELTDENTS
00910         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTDENTS
00911         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
00912         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTDENTS
00913                                                                   ELTDENTS
00914      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
00915      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00916         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTDENTS
00917                                                               ZEROELTDENTS
00918            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00919                                                            =  ZEROELTDENTS
00920               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTDENTS
00921            ELSE                                                   ELTDENTS
00922               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDENTS
00923          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00924                                                  TO  WS-PERCENTAGEELTDENTS
00925          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTDENTS
00926         ELSE                                                      ELTDENTS
00927            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTDENTS
00928          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
00929                                                 TO  WS-PERCENTAGE ELTDENTS
00930          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTDENTS
00931                                                                   ELTDENTS
00932      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDENTS
00933         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
00934                                             ZERO AND  NOT =  '19' ELTDENTS
00935         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
00936         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDENTS
00937         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDENTS
00938                                                    CMF-CODE-VALUE ELTDENTS
00939         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTDENTS
00940         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
00941         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTDENTS
00942                                                                   ELTDENTS
00943      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
00944         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
00945         ADD  +1   TO  WS-CIA                                      ELTDENTS
00946         PERFORM 8000-OUTPUT-TEXT                                  ELTDENTS
00947      ELSE                                                         ELTDENTS
00948       PERFORM 8000-OUTPUT-TEXT.                                   ELTDENTS
00949 **                                                               |ELTDENTS
00950 **---------------------------------------------------------------+ELTDENTS
00951                                                                   ELTDENTS
00952      SET PLT-INDEX2 TO 2.                                         ELTDENTS
00953      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
00954           PERFORM 7000-SPILLOVER-COINS                            ELTDENTS
00955           PERFORM 7200-SPILLOVER-DEDUCT.                          ELTDENTS
00956                                                                   ELTDENTS
00957      PERFORM 7100-TRANS-OTHER-RESP-IND.                           ELTDENTS
00958      PERFORM 6000-SCAN-TAB.                                       ELTDENTS
00959      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDENTS
00960                                                                   ELTDENTS
00961  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTDENTS
00962                                                                   ELTDENTS
00963      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDENTS
00964         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
00965         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
00966         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDENTS
00967                                                    CMF-CODE-VALUE ELTDENTS
00968         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTDENTS
00969         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
00970         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTDENTS
00971         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDENTS
00972         ADD  1  TO  WS-SUB2                                       ELTDENTS
00973         IF WS-CIA  >  20 OR  =  20                                ELTDENTS
00974            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTDENTS
00975            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDENTS
00976                COMMAREA(DFHCOMMAREA)                              ELTDENTS
00977            END-EXEC                                               ELTDENTS
00978            MOVE +1  TO  WS-CIA.                                   ELTDENTS
00979                                                                   ELTDENTS
00980  2090-PROBLEM-WITH-INDICES.                                       ELTDENTS
00981                                                                   ELTDENTS
00982      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDENTS
00983      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDENTS
00984                                                                   ELTDENTS
00985      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDENTS
00986      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
00987                                                                   ELTDENTS
00988      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
00989      END-EXEC.                                                    ELTDENTS
00990                                                                   ELTDENTS
00991  2099-EXIT.            EXIT.                                      ELTDENTS
00992                                                                   ELTDENTS
00993 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTDENTS
00994  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTDENTS
00995      MOVE '2100'  TO  WS-PARA-ID2.                                ELTDENTS
00996                                                                   ELTDENTS
00997      INITIALIZE CMF-RETURN-CODE,                                  ELTDENTS
00998                 TCAR-FROM-AREA.                                   ELTDENTS
00999                                                                   ELTDENTS
01000      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDENTS
01001      END-EXEC.                                                    ELTDENTS
01002                                                                   ELTDENTS
01003      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDENTS
01004      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01005          ADDRESS OF CMF-DESCR.                                    ELTDENTS
01006                                                                   ELTDENTS
01007      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTDENTS
01008         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTDENTS
01009         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTDENTS
01010            CMF-DESCR-LINE(1),        ' ',                         ELTDENTS
01011            CMF-DESCR-LINE(2),        ' ',                         ELTDENTS
01012            CMF-DESCR-LINE(3),        ' ',                         ELTDENTS
01013            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTDENTS
01014      ELSE                                                         ELTDENTS
01015         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTDENTS
01016         STRING CMF-DESCR-LINE(1),        ' ',                     ELTDENTS
01017            CMF-DESCR-LINE(2),        ' ',                         ELTDENTS
01018            CMF-DESCR-LINE(3),        ' ',                         ELTDENTS
01019            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTDENTS
01020                                                                   ELTDENTS
01021      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDENTS
01022                                                                   ELTDENTS
01023      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTDENTS
01024      MOVE +04  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELTDENTS
01025      IF WS-INDENT-ON                                              ELTDENTS
01026           MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN,                  ELTDENTS
01027                         TCAR-OUTPUT-FIELD-3-LEN,                  ELTDENTS
01028                         TCAR-OUTPUT-FIELD-4-LEN                   ELTDENTS
01029      ELSE                                                         ELTDENTS
01030       MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                      ELTDENTS
01031                     TCAR-OUTPUT-FIELD-3-LEN,                      ELTDENTS
01032                     TCAR-OUTPUT-FIELD-4-LEN.                      ELTDENTS
01033                                                                   ELTDENTS
01034      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDENTS
01035                                                                   ELTDENTS
01036      IF WS-MOVE-LINES-TO-CIA                                      ELTDENTS
01037         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTDENTS
01038            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTDENTS
01039                                             WS-TEMP-NOT-USED-CNT  ELTDENTS
01040            MOVE '2150'  TO  WS-PARA-ID2                           ELTDENTS
01041            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTDENTS
01042               VARYING  WS-SUB1  FROM  1  BY  1                    ELTDENTS
01043               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTDENTS
01044            MOVE '2100'  TO  WS-PARA-ID2                           ELTDENTS
01045            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTDENTS
01046            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTDENTS
01047            ADD +1  TO  WS-CIA                                     ELTDENTS
01048         ELSE                                                      ELTDENTS
01049            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTDENTS
01050            ADD +1  TO  WS-CIA.                                    ELTDENTS
01051                                                                   ELTDENTS
01052      IF WS-MOVE-LINES-TO-CIA                                      ELTDENTS
01053         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTDENTS
01054            MOVE '2160'  TO  WS-PARA-ID2                           ELTDENTS
01055            PERFORM 2160-MOVE-LINES-TO-CIA                         ELTDENTS
01056               VARYING  WS-SUB1  FROM  2  BY  1                    ELTDENTS
01057               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTDENTS
01058         ELSE                                                      ELTDENTS
01059            NEXT SENTENCE                                          ELTDENTS
01060      ELSE                                                         ELTDENTS
01061         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTDENTS
01062                                                                   ELTDENTS
01063      MOVE 'N'  TO  WS-INDENT-IND.                                 ELTDENTS
01064      GO TO 2199-EXIT.                                             ELTDENTS
01065                                                                   ELTDENTS
01066  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTDENTS
01067      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTDENTS
01068      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDENTS
01069                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDENTS
01070                                                                   ELTDENTS
01071  2160-MOVE-LINES-TO-CIA.                                          ELTDENTS
01072      IF WS-INDENT-ON                                              ELTDENTS
01073         MOVE TCAR-OPF-DATA(WS-SUB1) TO WS-DTL-INDENTED            ELTDENTS
01074         MOVE WS-INDENTED            TO COF-DTL-LINE(WS-CIA)       ELTDENTS
01075      ELSE                                                         ELTDENTS
01076         MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).    ELTDENTS
01077      IF WS-CIA > 20 OR = 20                                       ELTDENTS
01078         PERFORM 8000-OUTPUT-TEXT                                  ELTDENTS
01079      ELSE                                                         ELTDENTS
01080       ADD +1  TO  WS-CIA.                                         ELTDENTS
01081                                                                   ELTDENTS
01082  2199-EXIT.           EXIT.                                       ELTDENTS
01083                                                                   ELTDENTS
01084 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTDENTS
01085  2200-CODES-MANUAL-WITH-AMOUNT SECTION.                           ELTDENTS
01086      MOVE '2200'  TO  WS-PARA-ID2.                                ELTDENTS
01087                                                                   ELTDENTS
01088      INITIALIZE CMF-RETURN-CODE,                                  ELTDENTS
01089                 TCAR-FROM-AREA.                                   ELTDENTS
01090                                                                   ELTDENTS
01091      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDENTS
01092      END-EXEC.                                                    ELTDENTS
01093                                                                   ELTDENTS
01094      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDENTS
01095      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01096          ADDRESS OF CMF-DESCR.                                    ELTDENTS
01097                                                                   ELTDENTS
01098      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTDENTS
01099         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTDENTS
01100         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTDENTS
01101            CMF-DESCR-LINE(1),        ' ',                         ELTDENTS
01102            CMF-DESCR-LINE(2),        ' ',                         ELTDENTS
01103            CMF-DESCR-LINE(3), ' ',        WS-PRCNT-PERDM-ALLOW    ELTDENTS
01104            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTDENTS
01105      ELSE                                                         ELTDENTS
01106         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTDENTS
01107         STRING CMF-DESCR-LINE(1),        ' ',                     ELTDENTS
01108            CMF-DESCR-LINE(2),        ' ',                         ELTDENTS
01109            CMF-DESCR-LINE(3),        ' ',  WS-PRCNT-PERDM-ALLOW   ELTDENTS
01110            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTDENTS
01111                                                                   ELTDENTS
01112      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDENTS
01113                                                                   ELTDENTS
01114      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTDENTS
01115      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTDENTS
01116      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTDENTS
01117                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTDENTS
01118                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTDENTS
01119      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDENTS
01120                                                                   ELTDENTS
01121      IF WS-MOVE-LINES-TO-CIA                                      ELTDENTS
01122         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTDENTS
01123            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTDENTS
01124                                             WS-TEMP-NOT-USED-CNT  ELTDENTS
01125            MOVE '2250'  TO  WS-PARA-ID2                           ELTDENTS
01126            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTDENTS
01127               VARYING  WS-SUB1  FROM  1  BY  1                    ELTDENTS
01128               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTDENTS
01129            MOVE '2200'  TO  WS-PARA-ID2                           ELTDENTS
01130            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTDENTS
01131            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTDENTS
01132            ADD +1  TO  WS-CIA                                     ELTDENTS
01133         ELSE                                                      ELTDENTS
01134            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTDENTS
01135            ADD +1  TO  WS-CIA.                                    ELTDENTS
01136                                                                   ELTDENTS
01137      IF WS-MOVE-LINES-TO-CIA                                      ELTDENTS
01138         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTDENTS
01139            MOVE '2260'  TO  WS-PARA-ID2                           ELTDENTS
01140            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTDENTS
01141               VARYING  WS-SUB1  FROM  2  BY  1                    ELTDENTS
01142               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTDENTS
01143         ELSE                                                      ELTDENTS
01144            NEXT SENTENCE                                          ELTDENTS
01145      ELSE                                                         ELTDENTS
01146         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTDENTS
01147                                                                   ELTDENTS
01148      GO TO 2299-EXIT.                                             ELTDENTS
01149                                                                   ELTDENTS
01150  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTDENTS
01151      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTDENTS
01152      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDENTS
01153                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDENTS
01154                                                                   ELTDENTS
01155  2260-MOVE-LINES-TO-CIA.                                          ELTDENTS
01156      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTDENTS
01157      ADD +1  TO  WS-CIA.                                          ELTDENTS
01158                                                                   ELTDENTS
01159  2299-EXIT.           EXIT.                                       ELTDENTS
01160                                                                   ELTDENTS
01161 /            G E T   T A B U L A R   R E C O R D                  ELTDENTS
01162 ***************************************************************** ELTDENTS
01163 *            G E T   T A B U L A R   R E C O R D                  ELTDENTS
01164 *                                                                 ELTDENTS
01165 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTDENTS
01166 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTDENTS
01167 *  TO DISPLAY.                                                    ELTDENTS
01168 *                                                                 ELTDENTS
01169 ***************************************************************** ELTDENTS
01170  2300-GET-TABULAR-RECORD SECTION.                                 ELTDENTS
01171      MOVE '2300'  TO  WS-PARA-ID2.                                ELTDENTS
01172                                                                   ELTDENTS
01173      SET CIA-GCTABULR-DDN TO TRUE.                                ELTDENTS
01174      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01175          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTDENTS
01176                                                                   ELTDENTS
01177      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTDENTS
01178      SET CIA-GCTABULR-DDN           TO TRUE.                      ELTDENTS
01179                                                                   ELTDENTS
01180      SET IOP-RD                     TO TRUE.                      ELTDENTS
01181      SET IOP-FCQ-NONE               TO TRUE.                      ELTDENTS
01182      SET IOP-KVQ-NONE               TO TRUE.                      ELTDENTS
01183                                                                   ELTDENTS
01184      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTDENTS
01185             COMMAREA(DFHCOMMAREA)                                 ELTDENTS
01186      END-EXEC.                                                    ELTDENTS
01187                                                                   ELTDENTS
01188      IF IOP-RC-NOTFND                                             ELTDENTS
01189         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTDENTS
01190         EXEC CICS ABEND                                           ELTDENTS
01191                   ABCODE(CIA-ABCODE)                              ELTDENTS
01192         END-EXEC.                                                 ELTDENTS
01193                                                                   ELTDENTS
01194      IF NOT IOP-RC-OK                                             ELTDENTS
01195         SET CIA-AB-CRITIO          TO TRUE                        ELTDENTS
01196         EXEC CICS ABEND                                           ELTDENTS
01197                   ABCODE(CIA-ABCODE)                              ELTDENTS
01198         END-EXEC.                                                 ELTDENTS
01199                                                                   ELTDENTS
01200  2399-EXIT.           EXIT.                                       ELTDENTS
01201 /                                                                 ELTDENTS
01202  2400-CODES-MANAUL-TRANS-DENTAL  SECTION.                         ELTDENTS
01203                                                                   ELTDENTS
01204      INITIALIZE CMF-RETURN-CODE,                                  ELTDENTS
01205                 TCAR-FROM-AREA.                                   ELTDENTS
01206                                                                   ELTDENTS
01207      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDENTS
01208      END-EXEC.                                                    ELTDENTS
01209                                                                   ELTDENTS
01210      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDENTS
01211      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01212          ADDRESS OF CMF-DESCR.                                    ELTDENTS
01213                                                                   ELTDENTS
01214      MOVE '2460'  TO  WS-PARA-ID2                                 ELTDENTS
01215      PERFORM 2460-MOVE-LINES-TO-CIA                               ELTDENTS
01216         VARYING  WS-SUB1  FROM  1  BY  1                          ELTDENTS
01217           UNTIL  WS-SUB1  >  CMF-NBR-DESCR-LINES.                 ELTDENTS
01218      MOVE '2400'  TO  WS-PARA-ID2                                 ELTDENTS
01219      GO TO 2499-EXIT.                                             ELTDENTS
01220                                                                   ELTDENTS
01221  2460-MOVE-LINES-TO-CIA.                                          ELTDENTS
01222      MOVE CMF-DESCR-LINE(WS-SUB1) TO WS-DTL-INDENTED-FOUR.        ELTDENTS
01223      MOVE WS-INDENTED-FOUR             TO COF-DTL-LINE(WS-CIA).   ELTDENTS
01224      IF WS-CIA > 19                                               ELTDENTS
01225         PERFORM 8000-OUTPUT-TEXT                                  ELTDENTS
01226      ELSE                                                         ELTDENTS
01227       ADD +1  TO  WS-CIA.                                         ELTDENTS
01228  2499-EXIT.   EXIT.                                               ELTDENTS
01229 /            I N S T I T U T I O N A L   O P   R T N E            ELTDENTS
01230 ***************************************************************** ELTDENTS
01231 *            I N S T I T U T I O N A L   O P   R T N E            ELTDENTS
01232 *                                                                 ELTDENTS
01233 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDENTS
01234 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDENTS
01235 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDENTS
01236 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDENTS
01237 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDENTS
01238 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDENTS
01239 *  MODULE.                                                        ELTDENTS
01240 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDENTS
01241 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDENTS
01242 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDENTS
01243 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDENTS
01244 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDENTS
01245 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDENTS
01246 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDENTS
01247 *                                                                 ELTDENTS
01248 ***************************************************************** ELTDENTS
01249  3000-INSTITUTIONAL-OP-RTNE SECTION.                              ELTDENTS
01250      MOVE '3000'  TO  WS-PARA-ID1.                                ELTDENTS
01251                                                                   ELTDENTS
01252      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
01253      MOVE ZERO  TO  COF-NBR-DTL-LINES.                            ELTDENTS
01254      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDENTS
01255      MOVE WS-HDR-2-INST-OP  TO  COF-HDR-LINE(2).                  ELTDENTS
01256                                                                   ELTDENTS
01257      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
01258      END-EXEC.                                                    ELTDENTS
01259                                                                   ELTDENTS
01260      MOVE +1  TO WS-CIA.                                          ELTDENTS
01261                                                                   ELTDENTS
01262      PERFORM 5400-DETER-OUTPAT-COVER.                             ELTDENTS
01263                                                                   ELTDENTS
01264      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
01265          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
01266              MOVE WS-COVERED   TO COF-DTL-LINE(WS-CIA)            ELTDENTS
01267              ADD +2            TO WS-CIA                          ELTDENTS
01268              MOVE WS-OUTPATIENT-DENT-IND                          ELTDENTS
01269                                TO COF-DTL-LINE(WS-CIA)            ELTDENTS
01270              ADD +1            TO WS-CIA                          ELTDENTS
01271      ELSE                                                         ELTDENTS
01272       ADD +1              TO WS-CIA                               ELTDENTS
01273       MOVE WS-NOT-COVERED TO COF-DTL-LINE(WS-CIA)                 ELTDENTS
01274       PERFORM 8000-OUTPUT-TEXT                                    ELTDENTS
01275       GO TO 3099-EXIT.                                            ELTDENTS
01276                                                                   ELTDENTS
01277      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
01278          MOVE WS-BASIC-LIT-LEFT TO COF-DTL-LINE(WS-CIA)           ELTDENTS
01279          ADD +2                 TO WS-CIA                         ELTDENTS
01280          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01281          MOVE 'CONTRACT'        TO CMF-RECORD-PREFIX              ELTDENTS
01282          MOVE 'DENT-SURG-PMT-ELIG-OP-IND'                         ELTDENTS
01283                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTDENTS
01284          MOVE WS-BASIC-DENTAL-IND                                 ELTDENTS
01285                                 TO CMF-CODE-VALUE                 ELTDENTS
01286          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
01287                                                                   ELTDENTS
01288      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
01289        IF WS-CIA > 17                                             ELTDENTS
01290          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01291        ELSE                                                       ELTDENTS
01292         ADD +1  TO WS-CIA.                                        ELTDENTS
01293                                                                   ELTDENTS
01294      IF WS-COVERED-SUPP = WS-YES                                  ELTDENTS
01295          MOVE WS-SUPP-LIT  TO COF-DTL-LINE(WS-CIA)                ELTDENTS
01296          ADD +2            TO WS-CIA                              ELTDENTS
01297          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01298          MOVE 'CONTRACT'   TO CMF-RECORD-PREFIX                   ELTDENTS
01299          MOVE 'DENT-SURG-PMT-ELIG-OP-IND'                         ELTDENTS
01300                            TO CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
01301          MOVE WS-SUPP-DENTAL-IND                                  ELTDENTS
01302                            TO CMF-CODE-VALUE                      ELTDENTS
01303          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
01304                                                                   ELTDENTS
01305      IF WS-CIA > 15                                               ELTDENTS
01306          PERFORM 8000-OUTPUT-TEXT.                                ELTDENTS
01307                                                                   ELTDENTS
01308      ADD  +1                      TO WS-CIA.                      ELTDENTS
01309      MOVE WS-FIXED-INST-OUTPATIENT TO COF-DTL-LINE(WS-CIA)        ELTDENTS
01310                                                                   ELTDENTS
01311      ADD +2            TO WS-CIA.                                 ELTDENTS
01312      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDENTS
01313      ADD +2            TO WS-CIA.                                 ELTDENTS
01314                                                                   ELTDENTS
01315      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
01316                                                                   ELTDENTS
01317      INITIALIZE TCAR-FROM-AREA.                                   ELTDENTS
01318      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDENTS
01319             WS-PAY-CONSDR-TEXT2                                   ELTDENTS
01320                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDENTS
01321      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDENTS
01322      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDENTS
01323      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDENTS
01324                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDENTS
01325      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDENTS
01326      ADD +1                TO  WS-CIA.                            ELTDENTS
01327      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
01328      ADD +1                TO  WS-CIA.                            ELTDENTS
01329      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
01330                                                                   ELTDENTS
01331      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
01332                                                                   ELTDENTS
01333  3099-EXIT.           EXIT.                                       ELTDENTS
01334                                                                   ELTDENTS
01335 /            P R O F E S S I O N A L   O P   R T N E              ELTDENTS
01336 ***************************************************************** ELTDENTS
01337 *            P R O F E S S I O N A L   O P   R T N E              ELTDENTS
01338 *                                                                 ELTDENTS
01339 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDENTS
01340 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDENTS
01341 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDENTS
01342 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDENTS
01343 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDENTS
01344 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDENTS
01345 *  MODULE.                                                        ELTDENTS
01346 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDENTS
01347 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDENTS
01348 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDENTS
01349 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDENTS
01350 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDENTS
01351 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDENTS
01352 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDENTS
01353 *                                                                 ELTDENTS
01354 ***************************************************************** ELTDENTS
01355  4000-PROFESSIONAL-OP-RTNE SECTION.                               ELTDENTS
01356      MOVE '4000'  TO  WS-PARA-ID1.                                ELTDENTS
01357                                                                   ELTDENTS
01358      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTDENTS
01359      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
01360      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDENTS
01361                     COF-NBR-DTL-LINES.                            ELTDENTS
01362      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
01363      END-EXEC.                                                    ELTDENTS
01364      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDENTS
01365      MOVE WS-HDR-2-PROF-OP  TO  COF-HDR-LINE(2).                  ELTDENTS
01366                                                                   ELTDENTS
01367      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTDENTS
01368      PERFORM 4010-MOVE-IN-PROF-OP                                 ELTDENTS
01369         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDENTS
01370         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTDENTS
01371                                                                   ELTDENTS
01372      GO TO 4020-CALL-COVERAGE.                                    ELTDENTS
01373  4010-MOVE-IN-PROF-OP.                                            ELTDENTS
01374      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDENTS
01375      MOVE WS-PROF-OP-LIST(WS-SUB)  TO                             ELTDENTS
01376                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDENTS
01377      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTDENTS
01378                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTDENTS
01379                                                                   ELTDENTS
01380  4020-CALL-COVERAGE.                                              ELTDENTS
01381      MOVE '4020'  TO  WS-PARA-ID1.                                ELTDENTS
01382      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDENTS
01383      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
01384      END-EXEC.                                                    ELTDENTS
01385                                                                   ELTDENTS
01386      MOVE 'DENTAL SURGICAL SERVICES ARE ' TO SSB-TOPIC-PHRASE.    ELTDENTS
01387                                                                   ELTDENTS
01388      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDENTS
01389      END-EXEC.                                                    ELTDENTS
01390                                                                   ELTDENTS
01391      ADD +1  TO   COF-NBR-DTL-LINES.                              ELTDENTS
01392      MOVE LOW-VALUES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).        ELTDENTS
01393      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
01394      END-EXEC.                                                    ELTDENTS
01395                                                                   ELTDENTS
01396      IF PVN-COVG-NONE                                             ELTDENTS
01397         GO TO 4099-EXIT.                                          ELTDENTS
01398                                                                   ELTDENTS
01399      MOVE +1  TO  WS-CIA.                                         ELTDENTS
01400                                                                   ELTDENTS
01401      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDENTS
01402                                                                   ELTDENTS
01403      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDENTS
01404            PSP-PROVN-PRICING-METHD,                               ELTDENTS
01405            PSP-TRANSF-OTHER-RESP-IND,                             ELTDENTS
01406            PSP-DENT-SURG-PMT-ELG-OP-IND,                          ELTDENTS
01407            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDENTS
01408            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDENTS
01409            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDENTS
01410            PSP-SPILL-OVER-DED-APL-IND,                            ELTDENTS
01411            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDENTS
01412            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDENTS
01413            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDENTS
01414            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDENTS
01415            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDENTS
01416            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDENTS
01417            PSC-BEN-SCOPE-ID.                                      ELTDENTS
01418                                                                   ELTDENTS
01419                                                                   ELTDENTS
01420      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDENTS
01421      END-EXEC.                                                    ELTDENTS
01422                                                                   ELTDENTS
01423      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDENTS
01424      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01425          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDENTS
01426                                                                   ELTDENTS
01427      PERFORM 4030-FIND-FIRST-NONZERO                              ELTDENTS
01428         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDENTS
01429         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTDENTS
01430                                                                   ELTDENTS
01431      GO TO 4099-EXIT.                                             ELTDENTS
01432  4030-FIND-FIRST-NONZERO.                                         ELTDENTS
01433      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDENTS
01434      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDENTS
01435         NEXT SENTENCE                                             ELTDENTS
01436      ELSE                                                         ELTDENTS
01437         PERFORM 4040-BUILD-SCREEN-LINES.                          ELTDENTS
01438                                                                   ELTDENTS
01439  4040-BUILD-SCREEN-LINES.                                         ELTDENTS
01440      MOVE '4040'  TO  WS-PARA-ID1.                                ELTDENTS
01441                                                                   ELTDENTS
01442      SET PLT-INDEX1   TO                                          ELTDENTS
01443                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTDENTS
01444      IF WS-NOT-FIRST-TIME                                         ELTDENTS
01445         MOVE 'P'  TO  COF-FUNCTION                                ELTDENTS
01446         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDENTS
01447             COMMAREA(DFHCOMMAREA)                                 ELTDENTS
01448         END-EXEC                                                  ELTDENTS
01449      ELSE                                                         ELTDENTS
01450         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTDENTS
01451                                                                   ELTDENTS
01452      MOVE +1  TO  WS-CIA.                                         ELTDENTS
01453      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDENTS
01454         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDENTS
01455            SET PLT-INDEX2  TO  2                                  ELTDENTS
01456         ELSE                                                      ELTDENTS
01457            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTDENTS
01458            GO TO 4099-EXIT                                        ELTDENTS
01459      ELSE                                                         ELTDENTS
01460         SET PLT-INDEX2  TO  1.                                    ELTDENTS
01461                                                                   ELTDENTS
01462 **---------------------------------------------------------------+ELTDENTS
01463 **                                                               |ELTDENTS
01464 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTDENTS
01465      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTDENTS
01466      ADD  +1  TO  WS-CIA.                                         ELTDENTS
01467      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDENTS
01468      MOVE ZERO  TO  WS-SUB2.                                      ELTDENTS
01469      MOVE '4050'  TO  WS-PARA-ID1.                                ELTDENTS
01470      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTDENTS
01471         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTDENTS
01472         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.                ELTDENTS
01473      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDENTS
01474                                                                   ELTDENTS
01475      ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTDENTS
01476      MOVE 1  TO  WS-CIA.                                          ELTDENTS
01477      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDENTS
01478             COMMAREA(DFHCOMMAREA)                                 ELTDENTS
01479      END-EXEC.                                                    ELTDENTS
01480 **                                                               |ELTDENTS
01481 **---------------------------------------------------------------+ELTDENTS
01482                                                                   ELTDENTS
01483      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01484      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01485        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDENTS
01486               NOT = ZERO                                          ELTDENTS
01487         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTDENTS
01488         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01489         ADD +1  TO  WS-CIA.                                       ELTDENTS
01490                                                                   ELTDENTS
01491      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
01492      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01493       AND  NOT WS-ADD-A-BLANK-LINE                                ELTDENTS
01494        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDENTS
01495               NOT = ZERO                                          ELTDENTS
01496         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTDENTS
01497         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01498         ADD +1  TO  WS-CIA.                                       ELTDENTS
01499                                                                   ELTDENTS
01500      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01501             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTDENTS
01502                                                                   ELTDENTS
01503      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01504             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTDENTS
01505                                                                   ELTDENTS
01506      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
01507          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTDENTS
01508          ADD  +1   TO  WS-CIA                                     ELTDENTS
01509          PERFORM 8000-OUTPUT-TEXT.                                ELTDENTS
01510                                                                   ELTDENTS
01511 **---------------------------------------------------------------+ELTDENTS
01512 **                                                               |ELTDENTS
01513 **            D E N T A L   I N D I C A T O R                    |ELTDENTS
01514                                                                   ELTDENTS
01515      MOVE WS-NO  TO WS-COVERED-BASIC,                             ELTDENTS
01516                     WS-COVERED-SUPP.                              ELTDENTS
01517                                                                   ELTDENTS
01518      SET PLT-INDEX2  TO  1.                                       ELTDENTS
01519      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01520        IF PLP-DENT-SURG-PMT-ELG-OP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
01521                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
01522                  MOVE WS-YES TO WS-COVERED-BASIC.                 ELTDENTS
01523                                                                   ELTDENTS
01524      SET PLT-INDEX2  TO  2.                                       ELTDENTS
01525      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01526        IF PLP-DENT-SURG-PMT-ELG-OP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
01527                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
01528                  MOVE WS-YES TO WS-COVERED-SUPP.                  ELTDENTS
01529                                                                   ELTDENTS
01530      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
01531          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
01532              ADD +1            TO WS-CIA                          ELTDENTS
01533              MOVE WS-OUTPATIENT-DENT-IND                          ELTDENTS
01534                                TO COF-DTL-LINE(WS-CIA)            ELTDENTS
01535              ADD +1            TO WS-CIA.                         ELTDENTS
01536                                                                   ELTDENTS
01537      SET PLT-INDEX2  TO  1.                                       ELTDENTS
01538      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01539        IF PLP-DENT-SURG-PMT-ELG-OP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
01540                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
01541          MOVE WS-BASIC-LIT-LEFT TO COF-DTL-LINE(WS-CIA)           ELTDENTS
01542          ADD +2                 TO WS-CIA                         ELTDENTS
01543          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01544          MOVE 'BP'              TO CMF-RECORD-PREFIX              ELTDENTS
01545          MOVE 'DENT-SURG-PMT-ELG-OP-IND'                          ELTDENTS
01546                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTDENTS
01547          MOVE PLP-DENT-SURG-PMT-ELG-OP-IND(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01548                                 TO CMF-CODE-VALUE                 ELTDENTS
01549          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
01550                                                                   ELTDENTS
01551      IF WS-COVERED-BASIC = WS-YES                                 ELTDENTS
01552        IF WS-CIA > 17                                             ELTDENTS
01553          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01554        ELSE                                                       ELTDENTS
01555         ADD +1  TO WS-CIA.                                        ELTDENTS
01556                                                                   ELTDENTS
01557      SET PLT-INDEX2  TO  2.                                       ELTDENTS
01558      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01559        IF PLP-DENT-SURG-PMT-ELG-OP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDENTS
01560                NOT = '00' AND NOT = 'ZZ'                          ELTDENTS
01561          MOVE WS-SUPP-LIT  TO COF-DTL-LINE(WS-CIA)                ELTDENTS
01562          ADD +2            TO WS-CIA                              ELTDENTS
01563          PERFORM 8000-OUTPUT-TEXT                                 ELTDENTS
01564          MOVE 'BP'         TO CMF-RECORD-PREFIX                   ELTDENTS
01565          MOVE 'DENT-SURG-PMT-ELG-OP-IND'                          ELTDENTS
01566                            TO CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
01567          MOVE PLP-DENT-SURG-PMT-ELG-OP-IND(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01568                            TO CMF-CODE-VALUE                      ELTDENTS
01569          PERFORM 2400-CODES-MANAUL-TRANS-DENTAL.                  ELTDENTS
01570                                                                   ELTDENTS
01571      IF WS-COVERED-BASIC = WS-YES OR                              ELTDENTS
01572          WS-COVERED-SUPP = WS-YES                                 ELTDENTS
01573               PERFORM 8000-OUTPUT-TEXT.                           ELTDENTS
01574                                                                   ELTDENTS
01575 **                                                               |ELTDENTS
01576 **---------------------------------------------------------------+ELTDENTS
01577                                                                   ELTDENTS
01578 **---------------------------------------------------------------+ELTDENTS
01579 **                                                               |ELTDENTS
01580 **            B E N E F I T   S C O P E   I D                    |ELTDENTS
01581      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01582      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01583            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTDENTS
01584                                         '0000' AND  NOT =  '00  ' ELTDENTS
01585               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTDENTS
01586               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTDENTS
01587               ADD  +1  TO  WS-CIA.                                ELTDENTS
01588                                                                   ELTDENTS
01589      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
01590      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01591            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTDENTS
01592                                         '0000' AND  NOT =  '00  ' ELTDENTS
01593                                   AND NOT WS-ADD-A-BLANK-LINE     ELTDENTS
01594               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTDENTS
01595               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTDENTS
01596               ADD  +1  TO  WS-CIA.                                ELTDENTS
01597                                                                   ELTDENTS
01598      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01599      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01600            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTDENTS
01601                                         '0000' AND  NOT =  '00  ' ELTDENTS
01602               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTDENTS
01603               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTDENTS
01604               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTDENTS
01605                                                    CMF-CODE-VALUE ELTDENTS
01606               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTDENTS
01607               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTDENTS
01608               MOVE 'Y'          TO WS-INDENT-IND                  ELTDENTS
01609               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTDENTS
01610                                                                   ELTDENTS
01611      SET PLT-INDEX2  TO  2.                                       ELTDENTS
01612      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01613            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTDENTS
01614                                         '0000' AND  NOT =  '00  ' ELTDENTS
01615               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTDENTS
01616               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTDENTS
01617               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTDENTS
01618                                                    CMF-CODE-VALUE ELTDENTS
01619               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTDENTS
01620               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTDENTS
01621               MOVE 'Y'          TO WS-INDENT-IND                  ELTDENTS
01622               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTDENTS
01623                                                                   ELTDENTS
01624      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
01625         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01626         ADD  +1   TO  WS-CIA                                      ELTDENTS
01627         PERFORM 8000-OUTPUT-TEXT.                                 ELTDENTS
01628 **                                                               |ELTDENTS
01629 **---------------------------------------------------------------+ELTDENTS
01630                                                                   ELTDENTS
01631 **---------------------------------------------------------------+ELTDENTS
01632 **                                                               |ELTDENTS
01633 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTDENTS
01634 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTDENTS
01635 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTDENTS
01636      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01637      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
01638         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
01639                                                              '19' ELTDENTS
01640         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01641         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDENTS
01642         ADD +1  TO  WS-CIA.                                       ELTDENTS
01643                                                                   ELTDENTS
01644      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
01645      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDENTS
01646         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
01647                                                        '19' AND   ELTDENTS
01648         NOT WS-ADD-A-BLANK-LINE                                   ELTDENTS
01649         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01650         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDENTS
01651         ADD +1  TO  WS-CIA.                                       ELTDENTS
01652                                                                   ELTDENTS
01653      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01654      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
01655         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDENTS
01656                            AND                                    ELTDENTS
01657         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01658         SET  PLT-INDEX2  TO  2                                    ELTDENTS
01659         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDENTS
01660                                                              ZERO ELTDENTS
01661            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDENTS
01662            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDENTS
01663            ADD +1  TO  WS-CIA.                                    ELTDENTS
01664                                                                   ELTDENTS
01665      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01666      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
01667         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDENTS
01668                            AND                                    ELTDENTS
01669         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTDENTS
01670         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTDENTS
01671         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTDENTS
01672         ADD +1  TO  WS-CIA.                                       ELTDENTS
01673                                                                   ELTDENTS
01674      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTDENTS
01675         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01676         SET  PLT-INDEX2  TO  2                                    ELTDENTS
01677         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDENTS
01678                                                              ZERO ELTDENTS
01679            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDENTS
01680            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDENTS
01681            ADD +1  TO  WS-CIA.                                    ELTDENTS
01682                                                                   ELTDENTS
01683      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01684      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDENTS
01685         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTDENTS
01686                                                            =  ZEROELTDENTS
01687            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01688                                                            =  ZEROELTDENTS
01689               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTDENTS
01690            ELSE                                                   ELTDENTS
01691               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDENTS
01692          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01693                                                  TO  WS-PERCENTAGEELTDENTS
01694         ELSE                                                      ELTDENTS
01695            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTDENTS
01696          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01697                                                 TO  WS-PERCENTAGE.ELTDENTS
01698                                                                   ELTDENTS
01699      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDENTS
01700         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
01701                                             ZERO AND  NOT =  '19' ELTDENTS
01702         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
01703         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDENTS
01704         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDENTS
01705                                                    CMF-CODE-VALUE ELTDENTS
01706         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTDENTS
01707         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
01708         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTDENTS
01709                                                                   ELTDENTS
01710      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
01711      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01712         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTDENTS
01713                                                               ZEROELTDENTS
01714            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01715                                                            =  ZEROELTDENTS
01716               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTDENTS
01717            ELSE                                                   ELTDENTS
01718               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTDENTS
01719          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01720                                                  TO  WS-PERCENTAGEELTDENTS
01721         ELSE                                                      ELTDENTS
01722            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTDENTS
01723          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDENTS
01724                                                 TO  WS-PERCENTAGE.ELTDENTS
01725                                                                   ELTDENTS
01726      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDENTS
01727         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDENTS
01728                                             ZERO AND  NOT =  '19' ELTDENTS
01729         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
01730         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDENTS
01731         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDENTS
01732                                                    CMF-CODE-VALUE ELTDENTS
01733         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTDENTS
01734         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
01735         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT.                    ELTDENTS
01736                                                                   ELTDENTS
01737      IF WS-ADD-A-BLANK-LINE                                       ELTDENTS
01738         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTDENTS
01739         ADD  1    TO  WS-CIA                                      ELTDENTS
01740         PERFORM 8000-OUTPUT-TEXT                                  ELTDENTS
01741      ELSE                                                         ELTDENTS
01742       PERFORM 8000-OUTPUT-TEXT.                                   ELTDENTS
01743 **                                                               |ELTDENTS
01744 **---------------------------------------------------------------+ELTDENTS
01745                                                                   ELTDENTS
01746      SET PLT-INDEX2 TO 2.                                         ELTDENTS
01747      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDENTS
01748           PERFORM 7000-SPILLOVER-COINS                            ELTDENTS
01749           PERFORM 7200-SPILLOVER-DEDUCT.                          ELTDENTS
01750                                                                   ELTDENTS
01751      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDENTS
01752         SET PLT-INDEX2  TO  2                                     ELTDENTS
01753      ELSE                                                         ELTDENTS
01754         SET PLT-INDEX2  TO  1.                                    ELTDENTS
01755                                                                   ELTDENTS
01756      PERFORM 7100-TRANS-OTHER-RESP-IND.                           ELTDENTS
01757      PERFORM 6000-SCAN-TAB.                                       ELTDENTS
01758      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDENTS
01759                                                                   ELTDENTS
01760  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTDENTS
01761                                                                   ELTDENTS
01762      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDENTS
01763         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
01764         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDENTS
01765         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDENTS
01766                                                   CMF-CODE-VALUE  ELTDENTS
01767         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTDENTS
01768         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTDENTS
01769         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTDENTS
01770         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDENTS
01771         ADD  1  TO  WS-SUB2                                       ELTDENTS
01772         IF WS-CIA  >  20 OR  =  20                                ELTDENTS
01773            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTDENTS
01774            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDENTS
01775                COMMAREA(DFHCOMMAREA)                              ELTDENTS
01776            END-EXEC                                               ELTDENTS
01777            MOVE +1  TO  WS-CIA.                                   ELTDENTS
01778                                                                   ELTDENTS
01779  4090-PROBLEM-WITH-INDICES.                                       ELTDENTS
01780      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDENTS
01781      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDENTS
01782                                                                   ELTDENTS
01783      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDENTS
01784      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDENTS
01785                                                                   ELTDENTS
01786      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDENTS
01787      END-EXEC.                                                    ELTDENTS
01788                                                                   ELTDENTS
01789  4099-EXIT.           EXIT.                                       ELTDENTS
01790 /                                                                 ELTDENTS
01791  4675-PAY-CONSID-TEXT SECTION.                                    ELTDENTS
01792      INITIALIZE TCAR-FROM-AREA.                                   ELTDENTS
01793      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDENTS
01794             WS-PAY-CONSDR-TEXT2                                   ELTDENTS
01795                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDENTS
01796      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDENTS
01797      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDENTS
01798      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDENTS
01799                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDENTS
01800      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDENTS
01801      IF WS-CIA > 17                                               ELTDENTS
01802            PERFORM 8000-OUTPUT-TEXT                               ELTDENTS
01803            MOVE +1            TO WS-CIA.                          ELTDENTS
01804      ADD +1                TO  WS-CIA.                            ELTDENTS
01805      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
01806      ADD +1                TO  WS-CIA.                            ELTDENTS
01807      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDENTS
01808      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
01809  4675-EXIT.   EXIT.                                               ELTDENTS
01810 /                                                                 ELTDENTS
01811  5000-PLACE-OF-TREATMENT-BASIC SECTION.                           ELTDENTS
01812 **---------------------------------------------------------------+ELTDENTS
01813 **                                                               |ELTDENTS
01814 **        P L A C E   O F   T R E A T M E N T                    |ELTDENTS
01815      SET  PLT-INDEX2  TO  1.                                      ELTDENTS
01816      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTDENTS
01817                                                              ZERO ELTDENTS
01818         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
01819         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTDENTS
01820         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTDENTS
01821                                                   CMF-CODE-VALUE  ELTDENTS
01822         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTDENTS
01823         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTDENTS
01824         MOVE 'Y'          TO WS-INDENT-IND                        ELTDENTS
01825         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTDENTS
01826  5099-EXIT.  EXIT.                                                ELTDENTS
01827 /                                                                 ELTDENTS
01828  5100-PLACE-OF-TREATMENT-SUPP SECTION.                            ELTDENTS
01829 **---------------------------------------------------------------+ELTDENTS
01830 **                                                               |ELTDENTS
01831 **        P L A C E   O F   T R E A T M E N T                    |ELTDENTS
01832      SET  PLT-INDEX2  TO  2.                                      ELTDENTS
01833      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTDENTS
01834                                                              ZERO ELTDENTS
01835         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
01836         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTDENTS
01837         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTDENTS
01838                                                   CMF-CODE-VALUE  ELTDENTS
01839         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTDENTS
01840         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTDENTS
01841         MOVE 'Y'          TO WS-INDENT-IND                        ELTDENTS
01842         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTDENTS
01843         ADD  +1      TO  WS-CIA.                                  ELTDENTS
01844  5199-EXIT.  EXIT.                                                ELTDENTS
01845 /                                                                 ELTDENTS
01846  5200-DETER-INPAT-COVER  SECTION.                                 ELTDENTS
01847                                                                   ELTDENTS
01848      MOVE WS-NO TO WS-COVERED-BASIC,                              ELTDENTS
01849                    WS-COVERED-SUPP.                               ELTDENTS
01850                                                                   ELTDENTS
01851      MOVE ZEROS TO WS-BASIC-DENTAL-IND,                           ELTDENTS
01852                    WS-SUPP-DENTAL-IND.                            ELTDENTS
01853                                                                   ELTDENTS
01854      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDENTS
01855      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01856          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01857      IF CIA-RC-PTR-NULL                                           ELTDENTS
01858         SET CIA-ELSCONIS-DDN TO TRUE                              ELTDENTS
01859         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDENTS
01860             ADDRESS OF CONTRACT-RECORD                            ELTDENTS
01861         IF CIA-RC-PTR-NULL                                        ELTDENTS
01862            GO TO 5299-EXIT.                                       ELTDENTS
01863                                                                   ELTDENTS
01864      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDENTS
01865      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01866          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01867      IF CIA-RC-PTR-NULL                                           ELTDENTS
01868         MOVE GCT-DENT-SURG-PMT-ELIG-IP-IND TO WS-BASIC-DENTAL-IND.ELTDENTS
01869                                                                   ELTDENTS
01870      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDENTS
01871      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01872          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01873      IF CIA-RC-PTR-NULL                                           ELTDENTS
01874         MOVE GCT-DENT-SURG-PMT-ELIG-IP-IND TO WS-SUPP-DENTAL-IND. ELTDENTS
01875                                                                   ELTDENTS
01876      IF WS-BASIC-DENTAL-IND = '00' OR 'ZZ'                        ELTDENTS
01877           AND WS-SUPP-DENTAL-IND = '00' OR 'ZZ'                   ELTDENTS
01878                GO TO 5299-EXIT.                                   ELTDENTS
01879                                                                   ELTDENTS
01880      IF WS-BASIC-DENTAL-IND NOT = '00' AND NOT = 'ZZ'             ELTDENTS
01881          MOVE WS-YES TO WS-COVERED-BASIC.                         ELTDENTS
01882                                                                   ELTDENTS
01883      IF WS-SUPP-DENTAL-IND NOT = '00' AND NOT = 'ZZ'              ELTDENTS
01884          MOVE WS-YES TO WS-COVERED-SUPP.                          ELTDENTS
01885  5299-EXIT.       EXIT.                                           ELTDENTS
01886 /                                                                 ELTDENTS
01887  5400-DETER-OUTPAT-COVER  SECTION.                                ELTDENTS
01888                                                                   ELTDENTS
01889      MOVE WS-NO TO WS-COVERED-BASIC,                              ELTDENTS
01890                    WS-COVERED-SUPP.                               ELTDENTS
01891                                                                   ELTDENTS
01892      MOVE ZEROS TO WS-BASIC-DENTAL-IND,                           ELTDENTS
01893                    WS-SUPP-DENTAL-IND.                            ELTDENTS
01894                                                                   ELTDENTS
01895      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDENTS
01896      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01897          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01898      IF CIA-RC-PTR-NULL                                           ELTDENTS
01899         SET CIA-ELSCONIS-DDN TO TRUE                              ELTDENTS
01900         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTDENTS
01901             ADDRESS OF CONTRACT-RECORD                            ELTDENTS
01902         IF CIA-RC-PTR-NULL                                        ELTDENTS
01903            GO TO 5499-EXIT.                                       ELTDENTS
01904                                                                   ELTDENTS
01905      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDENTS
01906      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01907          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01908      IF CIA-RC-PTR-NULL                                           ELTDENTS
01909         MOVE GCT-DENT-SURG-PMT-ELIG-OP-IND TO WS-BASIC-DENTAL-IND.ELTDENTS
01910                                                                   ELTDENTS
01911      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDENTS
01912      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDENTS
01913          ADDRESS OF CONTRACT-RECORD.                              ELTDENTS
01914      IF CIA-RC-PTR-NULL                                           ELTDENTS
01915         MOVE GCT-DENT-SURG-PMT-ELIG-OP-IND TO WS-SUPP-DENTAL-IND. ELTDENTS
01916                                                                   ELTDENTS
01917      IF WS-BASIC-DENTAL-IND = '00' OR 'ZZ'                        ELTDENTS
01918           AND WS-SUPP-DENTAL-IND = '00' OR 'ZZ'                   ELTDENTS
01919                GO TO 5499-EXIT.                                   ELTDENTS
01920                                                                   ELTDENTS
01921      IF WS-BASIC-DENTAL-IND NOT = '00' AND NOT = 'ZZ'             ELTDENTS
01922          MOVE WS-YES TO WS-COVERED-BASIC.                         ELTDENTS
01923                                                                   ELTDENTS
01924      IF WS-SUPP-DENTAL-IND NOT = '00' AND NOT = 'ZZ'              ELTDENTS
01925          MOVE WS-YES TO WS-COVERED-SUPP.                          ELTDENTS
01926  5499-EXIT.       EXIT.                                           ELTDENTS
01927 /                                                                 ELTDENTS
01928  6000-SCAN-TAB SECTION.                                           ELTDENTS
01929      PERFORM 6200-BEN-TAB-AAR.                                    ELTDENTS
01930      PERFORM 6300-BEN-TAB-PPF.                                    ELTDENTS
01931                                                                   ELTDENTS
01932      ADD +1            TO WS-CIA.                                 ELTDENTS
01933      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDENTS
01934      ADD +1            TO WS-CIA.                                 ELTDENTS
01935      PERFORM 8000-OUTPUT-TEXT.                                    ELTDENTS
01936                                                                   ELTDENTS
01937      PERFORM 6500-BEN-TAB-ADL.                                    ELTDENTS
01938      PERFORM 6600-BEN-TAB-ABM.                                    ELTDENTS
01939      PERFORM 6700-BEN-TAB-ACL.                                    ELTDENTS
01940      PERFORM 6800-BEN-TAB-AOL.                                    ELTDENTS
01941                                                                   ELTDENTS
01942  6099-EXIT.  EXIT.                                                ELTDENTS
01943 /                                                                 ELTDENTS
01944  6200-BEN-TAB-AAR SECTION.                                        ELTDENTS
01945      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTDENTS
01946      SET PLT-INDEX2 TO 1.                                         ELTDENTS
01947                                                                   ELTDENTS
01948      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
01949          NOT = LOW-VALUES                                         ELTDENTS
01950       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
01951          NOT = SPACE                                              ELTDENTS
01952                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDENTS
01953                                                                   ELTDENTS
01954      SET PLT-INDEX2 TO 2.                                         ELTDENTS
01955                                                                   ELTDENTS
01956      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
01957          NOT = LOW-VALUES                                         ELTDENTS
01958       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
01959          NOT = SPACE                                              ELTDENTS
01960                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDENTS
01961                                                                   ELTDENTS
01962      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTDENTS
01963             MOVE +2                  TO WS-CIA                    ELTDENTS
01964             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTDENTS
01965             PERFORM 8000-OUTPUT-TEXT.                             ELTDENTS
01966  6200-EXIT.  EXIT.                                                ELTDENTS
01967 /                                                                 ELTDENTS
01968  6300-BEN-TAB-PPF SECTION.                                        ELTDENTS
01969      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDENTS
01970                       WS-HOLD2.                                   ELTDENTS
01971      SET PLT-INDEX2 TO 1.                                         ELTDENTS
01972      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
01973          NOT = LOW-VALUES                                         ELTDENTS
01974       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
01975          NOT = SPACE                                              ELTDENTS
01976             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDENTS
01977                        TO  WS-HOLD1.                              ELTDENTS
01978                                                                   ELTDENTS
01979      SET PLT-INDEX2 TO 2.                                         ELTDENTS
01980      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
01981          NOT = LOW-VALUES                                         ELTDENTS
01982       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
01983          NOT = SPACE                                              ELTDENTS
01984             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDENTS
01985                        TO  WS-HOLD2.                              ELTDENTS
01986                                                                   ELTDENTS
01987      IF WS-HOLD1 = WS-HOLD2                                       ELTDENTS
01988         IF WS-HOLD1 = ZEROS                                       ELTDENTS
01989                 GO TO 6399-EXIT                                   ELTDENTS
01990         ELSE                                                      ELTDENTS
01991             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDENTS
01992             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
01993             EXEC CICS  LINK                                       ELTDENTS
01994                        PROGRAM('ELGPPF')                          ELTDENTS
01995                        COMMAREA(DFHCOMMAREA)                      ELTDENTS
01996             END-EXEC                                              ELTDENTS
01997             GO TO 6399-EXIT.                                      ELTDENTS
01998                                                                   ELTDENTS
01999      IF WS-HOLD1 = ZEROS                                          ELTDENTS
02000             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02001             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02002             EXEC CICS  LINK                                       ELTDENTS
02003                        PROGRAM('ELGPPF')                          ELTDENTS
02004                        COMMAREA(DFHCOMMAREA)                      ELTDENTS
02005             END-EXEC                                              ELTDENTS
02006      ELSE                                                         ELTDENTS
02007       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDENTS
02008       PERFORM 2300-GET-TABULAR-RECORD                             ELTDENTS
02009       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTDENTS
02010                       COMMAREA(DFHCOMMAREA)                       ELTDENTS
02011       END-EXEC                                                    ELTDENTS
02012       IF WS-HOLD2 = ZEROS                                         ELTDENTS
02013            GO TO 6399-EXIT                                        ELTDENTS
02014       ELSE                                                        ELTDENTS
02015          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDENTS
02016          PERFORM 2300-GET-TABULAR-RECORD                          ELTDENTS
02017          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTDENTS
02018                     COMMAREA(DFHCOMMAREA)                         ELTDENTS
02019          END-EXEC.                                                ELTDENTS
02020  6399-EXIT.    EXIT.                                              ELTDENTS
02021 /                                                                 ELTDENTS
02022  6500-BEN-TAB-ADL SECTION.                                        ELTDENTS
02023      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDENTS
02024                       WS-HOLD2.                                   ELTDENTS
02025      SET PLT-INDEX2 TO 1.                                         ELTDENTS
02026      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02027          NOT = LOW-VALUES                                         ELTDENTS
02028       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02029          NOT = SPACE                                              ELTDENTS
02030             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02031                        TO  WS-HOLD1.                              ELTDENTS
02032                                                                   ELTDENTS
02033      SET PLT-INDEX2 TO 2.                                         ELTDENTS
02034      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02035          NOT = LOW-VALUES                                         ELTDENTS
02036       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02037          NOT = SPACE                                              ELTDENTS
02038             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02039                        TO  WS-HOLD2.                              ELTDENTS
02040                                                                   ELTDENTS
02041      IF WS-HOLD1 = WS-HOLD2                                       ELTDENTS
02042         IF WS-HOLD1 = ZEROS                                       ELTDENTS
02043                 GO TO 6599-EXIT                                   ELTDENTS
02044         ELSE                                                      ELTDENTS
02045             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDENTS
02046             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02047             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDENTS
02048                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02049             END-EXEC                                              ELTDENTS
02050             GO TO 6599-EXIT.                                      ELTDENTS
02051                                                                   ELTDENTS
02052      IF WS-HOLD1 = ZEROS                                          ELTDENTS
02053             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02054             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02055             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDENTS
02056                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02057             END-EXEC                                              ELTDENTS
02058      ELSE                                                         ELTDENTS
02059       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDENTS
02060       PERFORM 2300-GET-TABULAR-RECORD                             ELTDENTS
02061       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTDENTS
02062                       COMMAREA(DFHCOMMAREA)                       ELTDENTS
02063       END-EXEC                                                    ELTDENTS
02064       IF WS-HOLD2 = ZEROS                                         ELTDENTS
02065            GO TO 6599-EXIT                                        ELTDENTS
02066       ELSE                                                        ELTDENTS
02067          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDENTS
02068          PERFORM 2300-GET-TABULAR-RECORD                          ELTDENTS
02069          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTDENTS
02070                          COMMAREA(DFHCOMMAREA)                    ELTDENTS
02071          END-EXEC.                                                ELTDENTS
02072  6599-EXIT.     EXIT.                                             ELTDENTS
02073 /                                                                 ELTDENTS
02074  6600-BEN-TAB-ABM SECTION.                                        ELTDENTS
02075      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDENTS
02076                       WS-HOLD2.                                   ELTDENTS
02077      SET PLT-INDEX2 TO 1.                                         ELTDENTS
02078      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02079          NOT = LOW-VALUES                                         ELTDENTS
02080       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02081          NOT = SPACE                                              ELTDENTS
02082             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02083                        TO  WS-HOLD1.                              ELTDENTS
02084                                                                   ELTDENTS
02085      SET PLT-INDEX2 TO 2.                                         ELTDENTS
02086      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02087          NOT = LOW-VALUES                                         ELTDENTS
02088       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02089          NOT = SPACE                                              ELTDENTS
02090             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02091                        TO  WS-HOLD2.                              ELTDENTS
02092                                                                   ELTDENTS
02093      IF WS-HOLD1 = WS-HOLD2                                       ELTDENTS
02094         IF WS-HOLD1 = ZEROS                                       ELTDENTS
02095                 GO TO 6699-EXIT                                   ELTDENTS
02096         ELSE                                                      ELTDENTS
02097             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDENTS
02098             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02099             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDENTS
02100                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02101             END-EXEC                                              ELTDENTS
02102             GO TO 6699-EXIT.                                      ELTDENTS
02103                                                                   ELTDENTS
02104      IF WS-HOLD1 = ZEROS                                          ELTDENTS
02105             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02106             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02107             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDENTS
02108                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02109             END-EXEC                                              ELTDENTS
02110      ELSE                                                         ELTDENTS
02111       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDENTS
02112       PERFORM 2300-GET-TABULAR-RECORD                             ELTDENTS
02113       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTDENTS
02114                       COMMAREA(DFHCOMMAREA)                       ELTDENTS
02115       END-EXEC                                                    ELTDENTS
02116       IF WS-HOLD2 = ZEROS                                         ELTDENTS
02117         GO TO 6699-EXIT                                           ELTDENTS
02118       ELSE                                                        ELTDENTS
02119          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDENTS
02120          PERFORM 2300-GET-TABULAR-RECORD                          ELTDENTS
02121          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTDENTS
02122                          COMMAREA(DFHCOMMAREA)                    ELTDENTS
02123          END-EXEC.                                                ELTDENTS
02124  6699-EXIT.     EXIT.                                             ELTDENTS
02125 /                                                                 ELTDENTS
02126  6700-BEN-TAB-ACL SECTION.                                        ELTDENTS
02127      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDENTS
02128                       WS-HOLD2.                                   ELTDENTS
02129      SET PLT-INDEX2 TO 1.                                         ELTDENTS
02130      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02131          NOT = LOW-VALUES                                         ELTDENTS
02132       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02133          NOT = SPACE                                              ELTDENTS
02134             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02135                        TO  WS-HOLD1.                              ELTDENTS
02136                                                                   ELTDENTS
02137      SET PLT-INDEX2 TO 2.                                         ELTDENTS
02138      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02139          NOT = LOW-VALUES                                         ELTDENTS
02140       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02141          NOT = SPACE                                              ELTDENTS
02142             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02143                        TO  WS-HOLD2.                              ELTDENTS
02144                                                                   ELTDENTS
02145      IF WS-HOLD1 = WS-HOLD2                                       ELTDENTS
02146         IF WS-HOLD1 = ZEROS                                       ELTDENTS
02147                 GO TO 6799-EXIT                                   ELTDENTS
02148         ELSE                                                      ELTDENTS
02149             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDENTS
02150             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02151             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDENTS
02152                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02153             END-EXEC                                              ELTDENTS
02154             GO TO 6799-EXIT.                                      ELTDENTS
02155                                                                   ELTDENTS
02156      IF WS-HOLD1 = ZEROS                                          ELTDENTS
02157             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02158             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02159             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDENTS
02160                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02161             END-EXEC                                              ELTDENTS
02162      ELSE                                                         ELTDENTS
02163       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDENTS
02164       PERFORM 2300-GET-TABULAR-RECORD                             ELTDENTS
02165       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTDENTS
02166                       COMMAREA(DFHCOMMAREA)                       ELTDENTS
02167       END-EXEC                                                    ELTDENTS
02168       IF WS-HOLD2 = ZEROS                                         ELTDENTS
02169            GO TO 6799-EXIT                                        ELTDENTS
02170       ELSE                                                        ELTDENTS
02171             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02172             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02173             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDENTS
02174                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02175             END-EXEC.                                             ELTDENTS
02176  6799-EXIT.     EXIT.                                             ELTDENTS
02177 /                                                                 ELTDENTS
02178  6800-BEN-TAB-AOL SECTION.                                        ELTDENTS
02179      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDENTS
02180                       WS-HOLD2.                                   ELTDENTS
02181      SET PLT-INDEX2 TO 1.                                         ELTDENTS
02182      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02183          NOT = LOW-VALUES                                         ELTDENTS
02184       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02185          NOT = SPACE                                              ELTDENTS
02186             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02187                        TO  WS-HOLD1.                              ELTDENTS
02188                                                                   ELTDENTS
02189      SET PLT-INDEX2 TO 2.                                         ELTDENTS
02190      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDENTS
02191          NOT = LOW-VALUES                                         ELTDENTS
02192       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02193          NOT = SPACE                                              ELTDENTS
02194             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02195                        TO  WS-HOLD2.                              ELTDENTS
02196                                                                   ELTDENTS
02197      IF WS-HOLD1 = WS-HOLD2                                       ELTDENTS
02198         IF WS-HOLD1 = ZEROS                                       ELTDENTS
02199                 GO TO 6899-EXIT                                   ELTDENTS
02200         ELSE                                                      ELTDENTS
02201             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDENTS
02202             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02203             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDENTS
02204                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02205             END-EXEC                                              ELTDENTS
02206             GO TO 6899-EXIT.                                      ELTDENTS
02207                                                                   ELTDENTS
02208      IF WS-HOLD1 = ZEROS                                          ELTDENTS
02209             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDENTS
02210             PERFORM 2300-GET-TABULAR-RECORD                       ELTDENTS
02211             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDENTS
02212                             COMMAREA(DFHCOMMAREA)                 ELTDENTS
02213             END-EXEC                                              ELTDENTS
02214      ELSE                                                         ELTDENTS
02215       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDENTS
02216       PERFORM 2300-GET-TABULAR-RECORD                             ELTDENTS
02217       EXEC CICS  LINK PROGRAM('ELGOUTPX')                         ELTDENTS
02218                       COMMAREA(DFHCOMMAREA)                       ELTDENTS
02219       END-EXEC                                                    ELTDENTS
02220       IF WS-HOLD2 = ZEROS                                         ELTDENTS
02221            GO TO 6899-EXIT                                        ELTDENTS
02222       ELSE                                                        ELTDENTS
02223          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDENTS
02224          PERFORM 2300-GET-TABULAR-RECORD                          ELTDENTS
02225          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTDENTS
02226                          COMMAREA(DFHCOMMAREA)                    ELTDENTS
02227          END-EXEC.                                                ELTDENTS
02228  6899-EXIT.     EXIT.                                             ELTDENTS
02229 /                                                                 ELTDENTS
02230  7000-SPILLOVER-COINS SECTION.                                    ELTDENTS
02231 **---------------------------------------------------------------+ELTDENTS
02232 **                                                               |ELTDENTS
02233 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTDENTS
02234      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDENTS
02235                                                       NOT =  '0'  ELTDENTS
02236         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
02237         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTDENTS
02238                                           CMF-ELEMENT-SYSTEM-NAME ELTDENTS
02239         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTDENTS
02240                                           TO   CMF-CODE-VALUE     ELTDENTS
02241         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTDENTS
02242         MOVE 56 TO WS-TEMP-NOT-USED-CNT                           ELTDENTS
02243         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTDENTS
02244         ADD +1  TO  WS-CIA.                                       ELTDENTS
02245  7099-EXIT.    EXIT.                                              ELTDENTS
02246 /                                                                 ELTDENTS
02247  7100-TRANS-OTHER-RESP-IND SECTION.                               ELTDENTS
02248 **---------------------------------------------------------------+ELTDENTS
02249 **                                                               |ELTDENTS
02250 **   TRANSFER TO OTHER RESPONSIBILITY INDICATOR                  |ELTDENTS
02251 **                                                               |ELTDENTS
02252      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)     NOT   =  ZEROES   ELTDENTS
02253         SET PLT-INDEX2  TO  1.                                    ELTDENTS
02254                                                                   ELTDENTS
02255      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES     ELTDENTS
02256         SET PLT-INDEX2  TO  2.                                    ELTDENTS
02257                                                                   ELTDENTS
02258      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZERO ELTDENTS
02259         GO TO 7199-EXIT.                                          ELTDENTS
02260                                                                   ELTDENTS
02261      MOVE 'BP'                     TO  CMF-RECORD-PREFIX.         ELTDENTS
02262      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTDENTS
02263      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDENTS
02264           TO   CMF-CODE-VALUE.                                    ELTDENTS
02265      MOVE SPACES                   TO WS-TEMP-TEXT-AREA.          ELTDENTS
02266      MOVE +0                       TO WS-TEMP-NOT-USED-CNT.       ELTDENTS
02267      PERFORM 2100-CALL-CODES-MANUAL-LONG                          ELTDENTS
02268      ADD +1  TO  WS-CIA.                                          ELTDENTS
02269 **                                                               |ELTDENTS
02270 **---------------------------------------------------------------+ELTDENTS
02271  7199-EXIT.    EXIT.                                              ELTDENTS
02272 /                                                                 ELTDENTS
02273  7200-SPILLOVER-DEDUCT SECTION.                                   ELTDENTS
02274 **---------------------------------------------------------------+ELTDENTS
02275 **                                                               |ELTDENTS
02276 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTDENTS
02277      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDENTS
02278                                                       NOT =  '0'  ELTDENTS
02279         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDENTS
02280         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTDENTS
02281                                           CMF-ELEMENT-SYSTEM-NAME ELTDENTS
02282         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTDENTS
02283                                                     CMF-CODE-VALUEELTDENTS
02284         MOVE WS-SPILLOVER-DEDUCT TO WS-TEMP-TEXT-AREA             ELTDENTS
02285         MOVE 57 TO WS-TEMP-NOT-USED-CNT                           ELTDENTS
02286         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTDENTS
02287         ADD +1  TO  WS-CIA.                                       ELTDENTS
02288 **                                                               |ELTDENTS
02289 **---------------------------------------------------------------+ELTDENTS
02290  7299-EXIT.    EXIT.                                              ELTDENTS
02291                                                                   ELTDENTS
02292 /                                                                 ELTDENTS
02293  8000-OUTPUT-TEXT SECTION.                                        ELTDENTS
02294       MOVE +0     TO COF-NBR-HDR-LINES.                           ELTDENTS
02295       MOVE WS-CIA TO COF-NBR-DTL-LINES.                           ELTDENTS
02296       MOVE ' '    TO  COF-FUNCTION.                               ELTDENTS
02297       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTDENTS
02298              COMMAREA(DFHCOMMAREA)                                ELTDENTS
02299       END-EXEC.                                                   ELTDENTS
02300       MOVE 1  TO  WS-CIA.                                         ELTDENTS
02301  8099-EXIT.   EXIT.                                               ELTDENTS
02302                                                                   ELTDENTS
02303  8999-DUMMY SECTION.                                              ELTDENTS
02304      COPY ELSTCOMP.                                               ELTDENTS
02305                                                                   ELTDENTS
02306 /              A B E N D                                          ELTDENTS
02307 ******************************************************************ELTDENTS
02308 *                        A B E N D                                ELTDENTS
02309 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTDENTS
02310 *                                                                 ELTDENTS
02311 ******************************************************************ELTDENTS
02312  9999-ABEND SECTION.                                              ELTDENTS
02313                                                                   ELTDENTS
02314      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTDENTS
02315                                                                   ELTDENTS
02316  9999-EXIT.     EXIT.                                             ELTDENTS
