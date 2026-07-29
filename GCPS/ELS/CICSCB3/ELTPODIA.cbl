00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTPODIA.                                            ELTPODIA
00003  AUTHOR. JOHN CURIN - KEANE.                                         LV001
00004  DATE-WRITTEN.   5/13/86.                                         ELTPODIA
00005  DATE-COMPILED.                                                   ELTPODIA
00006      SKIP3                                                        ELTPODIA
00007 ******************************************************************ELTPODIA
00008 *@>ELTPODIA                                                       ELTPODIA
00009 *@¬                                                               ELTPODIA
00010 *                        PROGRAM ABSTRACT                         ELTPODIA
00011 *                                                                 ELTPODIA
00012 *@¬ PROGRAM NAME:   E.L.S. PODIATRY SURGERY TOPIC                 ELTPODIA
00013 *@¬                                                               ELTPODIA
00014 *@¬ PROGRAM I.D.:   ELTPODIA                                      ELTPODIA
00015 *@¬                                                               ELTPODIA
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTPODIA
00017 *@¬            PODIATRY SURGERY                                   ELTPODIA
00018 *@¬            BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTPODIA
00019 *@¬                                                               ELTPODIA
00020 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF PODIATRY          ELTPODIA
00021 *@¬            SURGERY AFFORDED A MEMBER BY HIS GROUP.            ELTPODIA
00022 *@¬            THIS INFORMATION IS GOTTEN BY INTEROGATING THE     ELTPODIA
00023 *@¬            BENEFIT PROVISIONS FOR THE GROUP WITHIN THE        ELTPODIA
00024 *@¬            CONTRACT FOR A PARTICULAR RANGE OF DATES.          ELTPODIA
00025 *@¬                                                               ELTPODIA
00026 *@¬ RECORDS                                                       ELTPODIA
00027 *@¬ ACCESSED:  CONTRACT, GROUP SPECIFIC, VARIOUS BENEFIT          ELTPODIA
00028 *@¬            PROVISION, AND A LARGE NUMBER OF DATA ELEMENT      ELTPODIA
00029 *@¬            AND CODE VALUE RECORDS.                            ELTPODIA
00030 *@¬                                                               ELTPODIA
00031 *@¬ PROCESSING                                                    ELTPODIA
00032 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTPODIA
00033 *@¬                                                               ELTPODIA
00034 *@¬                                                               ELTPODIA
00035 ***************************************************************** ELTPODIA
00036 *                                                                 ELTPODIA
00037 *                                                                 ELTPODIA
00038 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*                ELTPODIA
00039 *      *-*     U P D A T E  H I S T O R Y      *-*                ELTPODIA
00040 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*                ELTPODIA
00041 *                                                                 ELTPODIA
00042 **-CHG NUM-* *-DATE-* *WHO* *-----DESCRIPTION---------------      ELTPODIA
00043 *    XXXX    05/20/86  JTC   ORIGINAL IMPLEMENTATION              ELTPODIA
00044 *    0001    08/14/86  JTC   REMOVED THE SET UP OF HEADING LINE 1 ELTPODIA
00045 *                            WS-HDR-1.                            ELTPODIA
00046 *    0002    09/26/86  NAC   VS COBOL II CONVERSION.              ELTPODIA
00047 *    0003    10/21/87  EGL   CHANGED FIXED TEXT.                  ELTPODIA
00048 *    0004    03/22/89  GEM   STORAGE MANAGEMENT ENHANCEMENTS.     ELTPODIA
00049 *    0005    10/13/89  RKH   ADDED TRANSF TO OTHER RESPONSIB IND  ELTPODIA
00050 *                                                                 ELTPODIA
00051 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY INELTPODIA
00052 *                      FROM A SINGLE POSITION TO ZEROS            ELTPODIA
00053 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTPODIA
00054 ***************************************************************** ELTPODIA
00055 /                                                                 ELTPODIA
00056  ENVIRONMENT DIVISION.                                            ELTPODIA
00057      SKIP3                                                        ELTPODIA
00058  DATA DIVISION.                                                   ELTPODIA
00059  WORKING-STORAGE SECTION.                                         ELTPODIA
00060  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTPODIA
00061      '***ELTPODIA WS BEGINS***'.                                  ELTPODIA
00062 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTPODIA
00063  01  WS-WORK-FIELDS.                                              ELTPODIA
00064      05  WS-HEX-00                     PIC X.                     ELTPODIA
00065      05  WS-CHAR-0                     PIC X.                     ELTPODIA
00066      05  WS-HOLD1                      PIC X(10).                 ELTPODIA
00067      05  WS-HOLD2                      PIC X(10).                 ELTPODIA
00068      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTPODIA
00069      05  WS-YES                        PIC X     VALUE 'Y'.       ELTPODIA
00070      05  WS-NO                         PIC X     VALUE 'N'.       ELTPODIA
00071      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTPODIA
00072      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTPODIA
00073      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTPODIA
00074      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTPODIA
00075      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTPODIA
00076      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTPODIA
00077      05  WS-FIRSTTIME-IND              PIC X.                     ELTPODIA
00078        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTPODIA
00079      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTPODIA
00080        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTPODIA
00081      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTPODIA
00082        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTPODIA
00083      05  WS-INDENT-IND                 PIC X.                     ELTPODIA
00084        88  WS-INDENT-ON                    VALUE 'Y'.             ELTPODIA
00085      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTPODIA
00086      05  WS-PRCNT-PERDM-ALLOW          PIC X(9).                  ELTPODIA
00087      05  WS-PERCENT-FLD.                                          ELTPODIA
00088        10  WS-PERCENTAGE               PIC ZZ9.                   ELTPODIA
00089        10  WS-PERCENT-SIGN             PIC X.                     ELTPODIA
00090                                                                   ELTPODIA
00091 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTPODIA
00092  01  TABLE-MAX                   PIC S9(03) VALUE +2 COMP.        ELTPODIA
00093 * 2  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTPODIA
00094  01  WS-BEN-PROV-ID.                                              ELTPODIA
00095      05  WS-PROF-IP-CNT                PIC S999 COMP-3  VALUE +02.ELTPODIA
00096      05  WS-PROF-IP-TAB.                                          ELTPODIA
00097        10  FILLER                      PIC X(6)  VALUE 'PODI C'.  ELTPODIA
00098        10  FILLER                      PIC X(6)  VALUE 'PODO C'.  ELTPODIA
00099      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTPODIA
00100                                        PIC X(6)  OCCURS 2 TIMES.  ELTPODIA
00101                                                                   ELTPODIA
00102 /            D I S P L A Y   L I N E S                            ELTPODIA
00103  01  WS-ELS-DISPLAY-LINES.                                        ELTPODIA
00104                                                                   ELTPODIA
00105    05  WS-HDR-2-PROF.                                             ELTPODIA
00106      10  FILLER                    PIC X(22) VALUE SPACES.        ELTPODIA
00107      10  FILLER                    PIC X(29)                      ELTPODIA
00108          VALUE 'PODIATRY SURGERY PROFESSIONAL'.                   ELTPODIA
00109      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTPODIA
00110                                                                   ELTPODIA
00111    05  WS-HDR-2-INST.                                             ELTPODIA
00112      10  FILLER                    PIC X(22) VALUE SPACES.        ELTPODIA
00113      10  FILLER                    PIC X(30)                      ELTPODIA
00114          VALUE 'PODIATRY SURGERY INSTITUTIONAL'.                  ELTPODIA
00115      10  FILLER                    PIC X(27) VALUE LOW-VALUES.    ELTPODIA
00116                                                                   ELTPODIA
00117    05  WS-INST-FIXED-ONE           PIC X(72) VALUE                ELTPODIA
00118        'ROOM AND BOARD IS COVERED FOR ELIGIBLE PODIATRY SURGERIES,ELTPODIA
00119 -      ' SEE ROOM AND'.                                           ELTPODIA
00120                                                                   ELTPODIA
00121    05  WS-INST-FIXED-TWO           PIC X(26) VALUE                ELTPODIA
00122        'BOARD TOPIC FOR INPATIENT.'.                              ELTPODIA
00123                                                                   ELTPODIA
00124    05  WS-INST-FIXED-THREE         PIC X(53) VALUE                ELTPODIA
00125        'SEE OUTPATIENT SURGERY TOPIC FOR OUTPATIENT BENEFITS.'.   ELTPODIA
00126                                                                   ELTPODIA
00127    05  WS-PROF-FIXED-ONE           PIC X(75) VALUE                ELTPODIA
00128        'PODIATRY SURGERY IN BENEFIT ARE PROCEDURES PERFORMED ON THELTPODIA
00129 -      'E FOOT AND ANKLE.'.                                       ELTPODIA
00130                                                                   ELTPODIA
00131    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTPODIA
00132          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTPODIA
00133                                                                   ELTPODIA
00134    05  WS-FOLLOWING-BEN.                                          ELTPODIA
00135      10  FILLER                  PIC  X(22) VALUE                 ELTPODIA
00136            'COVERED SERVICES ARE: '.                              ELTPODIA
00137                                                                   ELTPODIA
00138    05  WS-PAYMNT-BASED.                                           ELTPODIA
00139      10  FILLER                  PIC  X(20) VALUE                 ELTPODIA
00140          'PAYMENT IS BASED ON:'.                                  ELTPODIA
00141                                                                   ELTPODIA
00142    05  WS-BASIC.                                                  ELTPODIA
00143      10  WS-BASIC-LIT              PIC X(16)                      ELTPODIA
00144          VALUE '         BASIC: '.                                ELTPODIA
00145      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTPODIA
00146                                                                   ELTPODIA
00147    05  WS-SUPPLEMENTAL.                                           ELTPODIA
00148      10  WS-SUPP-LIT               PIC X(16)                      ELTPODIA
00149          VALUE '  SUPPLEMENTAL: '.                                ELTPODIA
00150                                                                   ELTPODIA
00151    05  WS-INDENTED.                                               ELTPODIA
00152      10  FILLER                    PIC X(16) VALUE SPACES.        ELTPODIA
00153      10  WS-DTL-INDENTED           PIC X(63) VALUE SPACES.        ELTPODIA
00154                                                                   ELTPODIA
00155    05  WS-PAYABLE-AS.                                             ELTPODIA
00156      10  FILLER                  PIC  X(40) VALUE                 ELTPODIA
00157              'THESE SERVICES ARE PRICED ACCORDING TO:'.           ELTPODIA
00158                                                                   ELTPODIA
00159    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTPODIA
00160        'SPILLOVER COINSURANCE: '.                                 ELTPODIA
00161                                                                   ELTPODIA
00162    05  WS-SPILLOVER-DEDUCT         PIC X(22)  VALUE               ELTPODIA
00163        'SPILLOVER DEDUCTIBLE: '.                                  ELTPODIA
00164                                                                   ELTPODIA
00165    05  WS-CONTRACT-RELATED.                                       ELTPODIA
00166      10  FILLER                    PIC X(49)                      ELTPODIA
00167        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTPODIA
00168      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTPODIA
00169                                                                   ELTPODIA
00170    05  WS-PVE-TEXT.                                               ELTPODIA
00171      10  FILLER                    PIC X(44)     VALUE            ELTPODIA
00172        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTPODIA
00173                                                                   ELTPODIA
00174    05  WS-ACCUM-MSG1.                                             ELTPODIA
00175        10  FILLER                  PIC  X(79) VALUE               ELTPODIA
00176      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTPODIA
00177 -      'CONSIDERATIONS.'.                                         ELTPODIA
00178                                                                   ELTPODIA
00179    05  WS-NO-TABULAR1.                                            ELTPODIA
00180      10  FILLER                    PIC X(51)  VALUE               ELTPODIA
00181         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTPODIA
00182      10  FILLER                    PIC X(22)  VALUE               ELTPODIA
00183         'GOING FROM BENEFIT ***'.                                 ELTPODIA
00184                                                                   ELTPODIA
00185    05  WS-NO-TABULAR2.                                            ELTPODIA
00186      10  FILLER                    PIC X(15)  VALUE               ELTPODIA
00187         '*** PROVISION: '.                                        ELTPODIA
00188      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTPODIA
00189      10  FILLER                    PIC X VALUE SPACE.             ELTPODIA
00190      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTPODIA
00191      10  FILLER                    PIC X(13)  VALUE               ELTPODIA
00192         ' TO TABULAR: '.                                          ELTPODIA
00193      10  WS-NO-TAB-ID              PIC X(6).                      ELTPODIA
00194      10  FILLER                    PIC X VALUE SPACE.             ELTPODIA
00195      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTPODIA
00196      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTPODIA
00197                                                                   ELTPODIA
00198    05  WS-PGM-ERROR.                                              ELTPODIA
00199      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTPODIA
00200      10  FILLER                    PIC X(35)  VALUE               ELTPODIA
00201         '***  P R O G R A M   E R R O R  ***'.                    ELTPODIA
00202      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTPODIA
00203                                                                   ELTPODIA
00204    05  WS-BAD-INST-PROF-SEL.                                      ELTPODIA
00205      10  FILLER                    PIC XX VALUE SPACE.            ELTPODIA
00206      10  FILLER                    PIC X(47) VALUE                ELTPODIA
00207         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTPODIA
00208      10  FILLER                    PIC X(48) VALUE                ELTPODIA
00209         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTPODIA
00210      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTPODIA
00211                                                                   ELTPODIA
00212    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTPODIA
00213       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTPODIA
00214                                                                   ELTPODIA
00215    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTPODIA
00216    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTPODIA
00217      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTPODIA
00218                                                                   ELTPODIA
00219  01  WS-END                            PIC X(16)  VALUE           ELTPODIA
00220      '*** W/S ENDS ***'.                                          ELTPODIA
00221 /             L I N K A G E   S E C T I O N                       ELTPODIA
00222  LINKAGE SECTION.                                                 ELTPODIA
00223  01  DFHCOMMAREA.                                                 ELTPODIA
00224      COPY ELSCOMMC.                                               ELTPODIA
00225 /                                                                 ELTPODIA
00226      COPY ELSCIA2C.                                               ELTPODIA
00227 /                                                                 ELTPODIA
00228 ***  IO PARM AREA  ***                                            ELTPODIA
00229      COPY ELSIOPMC.                                               ELTPODIA
00230 /                                                                 ELTPODIA
00231      COPY ELSKEYSC.                                               ELTPODIA
00232 /                                                                 ELTPODIA
00233      COPY ELSOUTPC.                                               ELTPODIA
00234 /                                                                 ELTPODIA
00235      COPY ELSSSCBC.                                               ELTPODIA
00236 /                                                                 ELTPODIA
00237      COPY ELSCMIFC.                                               ELTPODIA
00238 /                                                                 ELTPODIA
00239      COPY ELSCMDSC.                                               ELTPODIA
00240 /                                                                 ELTPODIA
00241      COPY ELSPRVNC.                                               ELTPODIA
00242 /                                                                 ELTPODIA
00243 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTPODIA
00244      COPY ELSPLGSW.                                               ELTPODIA
00245 *** BENEFIT PROVISION TABLE OF FLDS                               ELTPODIA
00246      COPY ELSPLGTB.                                               ELTPODIA
00247 /                                                                 ELTPODIA
00248      COPY ELSTCWAC.                                               ELTPODIA
00249 /                                                                 ELTPODIA
00250 /                  M A I N L I N E                                ELTPODIA
00251  PROCEDURE DIVISION.                                              ELTPODIA
00252                                                                   ELTPODIA
00253 ******************************************************************ELTPODIA
00254 *                                                                 ELTPODIA
00255 *   PERFORM THE MAINLINE OPERATIONS.                              ELTPODIA
00256 *                                                                 ELTPODIA
00257 ******************************************************************ELTPODIA
00258  0000-MAINLINE.                                                   ELTPODIA
00259                                                                   ELTPODIA
00260      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTPODIA
00261          EXEC CICS ABEND                                          ELTPODIA
00262                    ABCODE ('EL01')                                ELTPODIA
00263          END-EXEC                                                 ELTPODIA
00264      END-IF.                                                      ELTPODIA
00265                                                                   ELTPODIA
00266 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTPODIA
00267                                                                   ELTPODIA
00268      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPODIA
00269          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTPODIA
00270                                                                   ELTPODIA
00271      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPODIA
00272      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00273          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTPODIA
00274                                                                   ELTPODIA
00275      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPODIA
00276      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00277          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTPODIA
00278                                                                   ELTPODIA
00279      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPODIA
00280      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00281          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTPODIA
00282                                                                   ELTPODIA
00283      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPODIA
00284      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00285          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTPODIA
00286                                                                   ELTPODIA
00287      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTPODIA
00288      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00289          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTPODIA
00290                                                                   ELTPODIA
00291      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPODIA
00292      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00293          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTPODIA
00294                                                                   ELTPODIA
00295      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPODIA
00296                                                                   ELTPODIA
00297      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTPODIA
00298              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTPODIA
00299                                                                   ELTPODIA
00300      SET CIA-STG-GETMAIN TO TRUE.                                 ELTPODIA
00301      EXEC CICS LINK                                               ELTPODIA
00302                PROGRAM('ELUSTGMG')                                ELTPODIA
00303                COMMAREA(DFHCOMMAREA)                              ELTPODIA
00304      END-EXEC.                                                    ELTPODIA
00305      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPODIA
00306      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00307          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTPODIA
00308                                                                   ELTPODIA
00309                                                                   ELTPODIA
00310      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTPODIA
00311                                                                   ELTPODIA
00312      IF (SSB-PROV-CLASS-INST   OR  SSB-PROV-CLASS-BOTH)           ELTPODIA
00313         PERFORM 1000-INSTITUTIONAL-IP-RTNE THRU 1000-EXIT.        ELTPODIA
00314                                                                   ELTPODIA
00315      IF (SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH)            ELTPODIA
00316         PERFORM 2000-PROFESSIONAL-IP-RTNE THRU 2000-EXIT.         ELTPODIA
00317                                                                   ELTPODIA
00318      IF NOT SSB-PROV-CLASS-INST   AND  NOT SSB-PROV-CLASS-PROF    ELTPODIA
00319                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTPODIA
00320         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTPODIA
00321         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTPODIA
00322         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTPODIA
00323         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTPODIA
00324         MOVE SPACE  TO  COF-FUNCTION                              ELTPODIA
00325         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPODIA
00326               COMMAREA(DFHCOMMAREA)                               ELTPODIA
00327         END-EXEC.                                                 ELTPODIA
00328                                                                   ELTPODIA
00329      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTPODIA
00330      SET CIA-STG-FREEMAIN TO TRUE.                                ELTPODIA
00331      EXEC CICS LINK                                               ELTPODIA
00332                PROGRAM('ELUSTGMG')                                ELTPODIA
00333                COMMAREA(DFHCOMMAREA)                              ELTPODIA
00334      END-EXEC.                                                    ELTPODIA
00335                                                                   ELTPODIA
00336 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTPODIA
00337                                                                   ELTPODIA
00338      MOVE 'E'   TO  COF-FUNCTION.                                 ELTPODIA
00339      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTPODIA
00340                                                                   ELTPODIA
00341      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTPODIA
00342                     COMMAREA (DFHCOMMAREA)                        ELTPODIA
00343      END-EXEC.                                                    ELTPODIA
00344                                                                   ELTPODIA
00345      EXEC CICS RETURN   END-EXEC.                                 ELTPODIA
00346                                                                   ELTPODIA
00347      GOBACK.                                                      ELTPODIA
00348                                                                   ELTPODIA
00349 /        I N S T I T U T I O N A L   I P   R T N E                ELTPODIA
00350 ***************************************************************** ELTPODIA
00351 *        I N S T I T U T I O N A L   I P   R T N E                ELTPODIA
00352 *                                                                 ELTPODIA
00353 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPODIA
00354 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPODIA
00355 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPODIA
00356 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPODIA
00357 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPODIA
00358 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPODIA
00359 *  MODULE.                                                        ELTPODIA
00360 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPODIA
00361 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPODIA
00362 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPODIA
00363 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPODIA
00364 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPODIA
00365 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPODIA
00366 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPODIA
00367 *                                                                 ELTPODIA
00368 ***************************************************************** ELTPODIA
00369  1000-INSTITUTIONAL-IP-RTNE.                                      ELTPODIA
00370                                                                   ELTPODIA
00371      MOVE 'P'       TO  COF-FUNCTION.                             ELTPODIA
00372      MOVE ZERO      TO  COF-NBR-DTL-LINES.                        ELTPODIA
00373      MOVE +2        TO  COF-NBR-HDR-LINES.                        ELTPODIA
00374      MOVE WS-HDR-2-INST TO  COF-HDR-LINE(2).                      ELTPODIA
00375                                                                   ELTPODIA
00376      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00377             END-EXEC.                                             ELTPODIA
00378                                                                   ELTPODIA
00379      MOVE +1  TO  WS-CIA.                                         ELTPODIA
00380                                                                   ELTPODIA
00381      MOVE WS-INST-FIXED-ONE TO COF-DTL-LINE(WS-CIA).              ELTPODIA
00382      ADD +1   TO  WS-CIA.                                         ELTPODIA
00383      MOVE WS-INST-FIXED-TWO TO COF-DTL-LINE(WS-CIA).              ELTPODIA
00384                                                                   ELTPODIA
00385      ADD +3   TO  WS-CIA.                                         ELTPODIA
00386      MOVE WS-INST-FIXED-THREE TO COF-DTL-LINE(WS-CIA).            ELTPODIA
00387                                                                   ELTPODIA
00388      PERFORM 8000-OUTPUT-TEXT.                                    ELTPODIA
00389                                                                   ELTPODIA
00390  1000-EXIT.            EXIT.                                      ELTPODIA
00391 /        P R O F E S S I O N A L   I P   R T N E                  ELTPODIA
00392 ***************************************************************** ELTPODIA
00393 *        P R O F E S S I O N A L   I P   R T N E                  ELTPODIA
00394 *                                                                 ELTPODIA
00395 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPODIA
00396 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPODIA
00397 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPODIA
00398 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPODIA
00399 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPODIA
00400 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPODIA
00401 *  MODULE.                                                        ELTPODIA
00402 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPODIA
00403 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPODIA
00404 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPODIA
00405 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPODIA
00406 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPODIA
00407 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPODIA
00408 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPODIA
00409 *                                                                 ELTPODIA
00410 ***************************************************************** ELTPODIA
00411  2000-PROFESSIONAL-IP-RTNE.                                       ELTPODIA
00412                                                                   ELTPODIA
00413      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPODIA
00414      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTPODIA
00415      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTPODIA
00416                     COF-NBR-DTL-LINES.                            ELTPODIA
00417      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00418              END-EXEC.                                            ELTPODIA
00419      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTPODIA
00420      MOVE WS-HDR-2-PROF  TO  COF-HDR-LINE(2).                     ELTPODIA
00421                                                                   ELTPODIA
00422                                                                   ELTPODIA
00423      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPODIA
00424      PERFORM WITH TEST BEFORE                                     ELTPODIA
00425              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPODIA
00426              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPODIA
00427         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPODIA
00428         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPODIA
00429         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPODIA
00430         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPODIA
00431      END-PERFORM.                                                 ELTPODIA
00432      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPODIA
00433                                                                   ELTPODIA
00434      PERFORM WITH TEST BEFORE                                     ELTPODIA
00435         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTPODIA
00436         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTPODIA
00437            SET PVN-BEN-PROVN-IDX TO WS-SUB                        ELTPODIA
00438            MOVE WS-PROF-IP-LIST (WS-SUB)                          ELTPODIA
00439                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTPODIA
00440            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTPODIA
00441                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)          ELTPODIA
00442      END-PERFORM.                                                 ELTPODIA
00443                                                                   ELTPODIA
00444      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPODIA
00445      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00446             END-EXEC.                                             ELTPODIA
00447                                                                   ELTPODIA
00448      MOVE 'PODIATRY SURGERY     '     TO  SSB-TOPIC-PHRASE.       ELTPODIA
00449                                                                   ELTPODIA
00450      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTPODIA
00451          END-EXEC.                                                ELTPODIA
00452                                                                   ELTPODIA
00453      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTPODIA
00454      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00455              END-EXEC.                                            ELTPODIA
00456                                                                   ELTPODIA
00457      IF PVN-COVG-NONE                                             ELTPODIA
00458          GO TO 2000-EXIT.                                         ELTPODIA
00459                                                                   ELTPODIA
00460      MOVE +1  TO  WS-CIA.                                         ELTPODIA
00461                                                                   ELTPODIA
00462                                                                   ELTPODIA
00463      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPODIA
00464            PSP-PROVN-PRICING-METHD,                               ELTPODIA
00465            PSP-TRANSF-OTHER-RESP-IND,                             ELTPODIA
00466            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTPODIA
00467            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTPODIA
00468            PSP-SPILL-OVER-COINS-APL-IND,                          ELTPODIA
00469            PSP-SPILL-OVER-DED-APL-IND,                            ELTPODIA
00470            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTPODIA
00471            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTPODIA
00472            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTPODIA
00473            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTPODIA
00474            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTPODIA
00475            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTPODIA
00476            PSC-BEN-SCOPE-ID.                                      ELTPODIA
00477                                                                   ELTPODIA
00478      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTPODIA
00479                     COMMAREA (DFHCOMMAREA)                        ELTPODIA
00480      END-EXEC.                                                    ELTPODIA
00481                                                                   ELTPODIA
00482      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPODIA
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00484          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTPODIA
00485                                                                   ELTPODIA
00486      PERFORM WITH TEST BEFORE                                     ELTPODIA
00487         VARYING WS-SUB  FROM  +1  BY  +1                          ELTPODIA
00488         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTPODIA
00489             SET PVN-BEN-PROVN-IDX TO WS-SUB                       ELTPODIA
00490             IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO    ELTPODIA
00491                 PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT    ELTPODIA
00492             END-IF                                                ELTPODIA
00493      END-PERFORM.                                                 ELTPODIA
00494                                                                   ELTPODIA
00495  2000-EXIT.  EXIT.                                                ELTPODIA
00496 /                                                                 ELTPODIA
00497  2040-BUILD-SCREEN-LINES.                                         ELTPODIA
00498                                                                   ELTPODIA
00499      SET PLT-INDEX1  TO                                           ELTPODIA
00500              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTPODIA
00501                                                                   ELTPODIA
00502      IF WS-NOT-FIRST-TIME                                         ELTPODIA
00503         MOVE 'P' TO COF-FUNCTION                                  ELTPODIA
00504         MOVE +0  TO COF-NBR-DTL-LINES                             ELTPODIA
00505         EXEC CICS LINK PROGRAM ('ELUOUTPT')                       ELTPODIA
00506                        COMMAREA (DFHCOMMAREA)                     ELTPODIA
00507         END-EXEC                                                  ELTPODIA
00508                                                                   ELTPODIA
00509      ELSE                                                         ELTPODIA
00510        MOVE 'N' TO WS-FIRSTTIME-IND.                              ELTPODIA
00511                                                                   ELTPODIA
00512      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPODIA
00513         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPODIA
00514            SET PLT-INDEX2  TO  2                                  ELTPODIA
00515         ELSE                                                      ELTPODIA
00516            MOVE TABLE-MAX TO WS-SUB                               ELTPODIA
00517            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTPODIA
00518            GO TO 2040-EXIT                                        ELTPODIA
00519      ELSE                                                         ELTPODIA
00520         SET PLT-INDEX2  TO  1.                                    ELTPODIA
00521                                                                   ELTPODIA
00522 **---------------------------------------------------------------+ELTPODIA
00523 **                                                               |ELTPODIA
00524 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTPODIA
00525      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTPODIA
00526      ADD  +1  TO  WS-CIA.                                         ELTPODIA
00527      MOVE ZERO  TO  WS-SUB2.                                      ELTPODIA
00528      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTPODIA
00529                                                                   ELTPODIA
00530      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTPODIA
00531         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTPODIA
00532         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTPODIA
00533      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPODIA
00534                                                                   ELTPODIA
00535      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTPODIA
00536      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00537             END-EXEC.                                             ELTPODIA
00538      MOVE +1  TO  WS-CIA.                                         ELTPODIA
00539 **                                                               |ELTPODIA
00540 **---------------------------------------------------------------+ELTPODIA
00541                                                                   ELTPODIA
00542      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00543      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00544        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPODIA
00545               NOT = ZERO                                          ELTPODIA
00546         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTPODIA
00547         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00548         ADD +1  TO  WS-CIA.                                       ELTPODIA
00549                                                                   ELTPODIA
00550      SET  PLT-INDEX2  TO  2.                                      ELTPODIA
00551      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00552       AND  NOT WS-ADD-A-BLANK-LINE                                ELTPODIA
00553        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPODIA
00554               NOT = ZERO                                          ELTPODIA
00555         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTPODIA
00556         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00557         ADD +1  TO  WS-CIA.                                       ELTPODIA
00558                                                                   ELTPODIA
00559      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00560      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00561             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTPODIA
00562                                                                   ELTPODIA
00563      SET  PLT-INDEX2  TO  2.                                      ELTPODIA
00564      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00565             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTPODIA
00566                                                                   ELTPODIA
00567      IF WS-ADD-A-BLANK-LINE                                       ELTPODIA
00568          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTPODIA
00569          ADD  +1   TO  WS-CIA                                     ELTPODIA
00570          PERFORM 8000-OUTPUT-TEXT.                                ELTPODIA
00571                                                                   ELTPODIA
00572 **---------------------------------------------------------------+ELTPODIA
00573 **                                                               |ELTPODIA
00574 **            B E N E F I T   S C O P E   I D                    |ELTPODIA
00575      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00576      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00577            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPODIA
00578                                         '0000' AND  NOT =  '00  ' ELTPODIA
00579               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPODIA
00580               ADD  +1  TO  WS-CIA                                 ELTPODIA
00581               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTPODIA
00582                                                                   ELTPODIA
00583      SET  PLT-INDEX2  TO  2.                                      ELTPODIA
00584      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00585        AND NOT WS-ADD-A-BLANK-LINE                                ELTPODIA
00586         AND PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =       ELTPODIA
00587                                         '0000' AND  NOT =  '00  ' ELTPODIA
00588               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPODIA
00589               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPODIA
00590               ADD  +1  TO  WS-CIA.                                ELTPODIA
00591                                                                   ELTPODIA
00592      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00593      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00594            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPODIA
00595                                         '0000' AND  NOT =  '00  ' ELTPODIA
00596               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPODIA
00597               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPODIA
00598               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPODIA
00599                                                    CMF-CODE-VALUE ELTPODIA
00600               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPODIA
00601               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPODIA
00602               MOVE 'Y' TO WS-INDENT-IND                           ELTPODIA
00603               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPODIA
00604                                                                   ELTPODIA
00605      SET PLT-INDEX2  TO  2.                                       ELTPODIA
00606      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00607            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPODIA
00608                                         '0000' AND  NOT =  '00  ' ELTPODIA
00609               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPODIA
00610               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPODIA
00611               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPODIA
00612                                                    CMF-CODE-VALUE ELTPODIA
00613               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPODIA
00614               MOVE 'Y' TO WS-INDENT-IND                           ELTPODIA
00615               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPODIA
00616               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPODIA
00617                                                                   ELTPODIA
00618      IF WS-ADD-A-BLANK-LINE                                       ELTPODIA
00619         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00620         ADD  +1   TO  WS-CIA                                      ELTPODIA
00621         PERFORM 8000-OUTPUT-TEXT.                                 ELTPODIA
00622 **                                                               |ELTPODIA
00623 **---------------------------------------------------------------+ELTPODIA
00624                                                                   ELTPODIA
00625 **---------------------------------------------------------------+ELTPODIA
00626 **                                                               |ELTPODIA
00627 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTPODIA
00628 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTPODIA
00629 **     A D D I T I O N A L   P R I C I N G   P E R C E N T       |ELTPODIA
00630      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00631      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00632         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPODIA
00633                                                              '19' ELTPODIA
00634         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPODIA
00635         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00636         ADD +1  TO  WS-CIA.                                       ELTPODIA
00637                                                                   ELTPODIA
00638      SET  PLT-INDEX2  TO  2.                                      ELTPODIA
00639      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00640         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPODIA
00641                                                        '19' AND   ELTPODIA
00642         NOT WS-ADD-A-BLANK-LINE                                   ELTPODIA
00643         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPODIA
00644         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00645         ADD +1  TO  WS-CIA.                                       ELTPODIA
00646                                                                   ELTPODIA
00647      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00648      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00649         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPODIA
00650                            AND                                    ELTPODIA
00651         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00652         SET  PLT-INDEX2  TO  2                                    ELTPODIA
00653         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPODIA
00654                                                             ZERO  ELTPODIA
00655            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPODIA
00656            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPODIA
00657            ADD +1  TO  WS-CIA.                                    ELTPODIA
00658                                                                   ELTPODIA
00659      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00660      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00661         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPODIA
00662                            AND                                    ELTPODIA
00663         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTPODIA
00664         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPODIA
00665         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTPODIA
00666         ADD +1  TO  WS-CIA.                                       ELTPODIA
00667                                                                   ELTPODIA
00668      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTPODIA
00669         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00670         SET  PLT-INDEX2  TO  2                                    ELTPODIA
00671         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPODIA
00672                                                             ZERO  ELTPODIA
00673            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPODIA
00674            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPODIA
00675            ADD +1  TO  WS-CIA.                                    ELTPODIA
00676                                                                   ELTPODIA
00677      SET  PLT-INDEX2  TO  1.                                      ELTPODIA
00678      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00679         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPODIA
00680                                                            =  ZEROELTPODIA
00681            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00682                                                            =  ZEROELTPODIA
00683               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPODIA
00684            ELSE                                                   ELTPODIA
00685               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPODIA
00686          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00687                                                  TO  WS-PERCENTAGEELTPODIA
00688          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPODIA
00689         ELSE                                                      ELTPODIA
00690          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTPODIA
00691          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00692                                                 TO  WS-PERCENTAGE ELTPODIA
00693          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPODIA
00694                                                                   ELTPODIA
00695                                                                   ELTPODIA
00696      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00697         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPODIA
00698                                             ZERO AND  NOT =  '19' ELTPODIA
00699         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
00700         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPODIA
00701         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPODIA
00702                                                    CMF-CODE-VALUE ELTPODIA
00703         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPODIA
00704         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPODIA
00705         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPODIA
00706                                                                   ELTPODIA
00707      SET  PLT-INDEX2  TO  2.                                      ELTPODIA
00708      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00709         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPODIA
00710                                                               ZEROELTPODIA
00711            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00712                                                            =  ZEROELTPODIA
00713               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPODIA
00714            ELSE                                                   ELTPODIA
00715               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPODIA
00716          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00717                                                  TO  WS-PERCENTAGEELTPODIA
00718          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPODIA
00719         ELSE                                                      ELTPODIA
00720            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPODIA
00721          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPODIA
00722                                                 TO  WS-PERCENTAGE ELTPODIA
00723          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPODIA
00724                                                                   ELTPODIA
00725      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPODIA
00726         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPODIA
00727                                             ZERO AND  NOT =  '19' ELTPODIA
00728         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
00729         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPODIA
00730         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPODIA
00731                                                    CMF-CODE-VALUE ELTPODIA
00732         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPODIA
00733         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPODIA
00734         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPODIA
00735                                                                   ELTPODIA
00736      IF WS-ADD-A-BLANK-LINE                                       ELTPODIA
00737         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPODIA
00738         ADD  +1   TO  WS-CIA                                      ELTPODIA
00739         PERFORM 8000-OUTPUT-TEXT                                  ELTPODIA
00740      ELSE                                                         ELTPODIA
00741       PERFORM 8000-OUTPUT-TEXT.                                   ELTPODIA
00742 **                                                               |ELTPODIA
00743 **---------------------------------------------------------------+ELTPODIA
00744                                                                   ELTPODIA
00745      SET PLT-INDEX2 TO 2.                                         ELTPODIA
00746      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPODIA
00747           PERFORM 7000-SPILLOVER-COINS                            ELTPODIA
00748           PERFORM 7200-SPILLOVER-DEDUCT.                          ELTPODIA
00749                                                                   ELTPODIA
00750      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)  NOT     =  ZEROS    ELTPODIA
00751          SET PLT-INDEX2  TO  1                                    ELTPODIA
00752          PERFORM 7300-TRANS-OTHER-RESP-IND  THRU                  ELTPODIA
00753                 7399-EXIT.                                        ELTPODIA
00754                                                                   ELTPODIA
00755      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   NOT  = ZEROS       ELTPODIA
00756         SET PLT-INDEX2  TO  2                                     ELTPODIA
00757         PERFORM 7300-TRANS-OTHER-RESP-IND  THRU                   ELTPODIA
00758                7399-EXIT.                                         ELTPODIA
00759                                                                   ELTPODIA
00760      ADD +1  TO  WS-CIA.                                          ELTPODIA
00761      MOVE WS-PROF-FIXED-ONE TO COF-DTL-LINE(WS-CIA).              ELTPODIA
00762      PERFORM 8000-OUTPUT-TEXT.                                    ELTPODIA
00763                                                                   ELTPODIA
00764      PERFORM 6000-SCAN-TAB.                                       ELTPODIA
00765                                                                   ELTPODIA
00766  2040-EXIT.  EXIT.                                                ELTPODIA
00767 /                                                                 ELTPODIA
00768  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTPODIA
00769                                                                   ELTPODIA
00770      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPODIA
00771         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
00772         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTPODIA
00773         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTPODIA
00774                                                    CMF-CODE-VALUE ELTPODIA
00775         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTPODIA
00776         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTPODIA
00777         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPODIA
00778         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTPODIA
00779         ADD  1  TO  WS-SUB2                                       ELTPODIA
00780         IF WS-CIA  >  20 OR  =  20                                ELTPODIA
00781            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTPODIA
00782            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTPODIA
00783                COMMAREA(DFHCOMMAREA)                              ELTPODIA
00784                 END-EXEC                                          ELTPODIA
00785            MOVE +1  TO  WS-CIA.                                   ELTPODIA
00786                                                                   ELTPODIA
00787  2090-PROBLEM-WITH-INDICES.                                       ELTPODIA
00788                                                                   ELTPODIA
00789      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTPODIA
00790      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPODIA
00791                                                                   ELTPODIA
00792      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTPODIA
00793      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPODIA
00794                                                                   ELTPODIA
00795      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPODIA
00796             END-EXEC.                                             ELTPODIA
00797                                                                   ELTPODIA
00798                                                                   ELTPODIA
00799 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPODIA
00800  2100-CALL-CODES-MANUAL-LONG.                                     ELTPODIA
00801                                                                   ELTPODIA
00802      INITIALIZE CMF-RETURN-CODE                                   ELTPODIA
00803                 TCAR-FROM-AREA.                                   ELTPODIA
00804                                                                   ELTPODIA
00805      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTPODIA
00806             END-EXEC.                                             ELTPODIA
00807                                                                   ELTPODIA
00808      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPODIA
00809      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00810          ADDRESS OF CMF-DESCR.                                    ELTPODIA
00811                                                                   ELTPODIA
00812      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPODIA
00813         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPODIA
00814         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTPODIA
00815            CMF-DESCR-LINE(1),        ' ',                         ELTPODIA
00816            CMF-DESCR-LINE(2),        ' ',                         ELTPODIA
00817            CMF-DESCR-LINE(3)                                      ELTPODIA
00818            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTPODIA
00819      ELSE                                                         ELTPODIA
00820         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTPODIA
00821         STRING CMF-DESCR-LINE(1),        ' ',                     ELTPODIA
00822            CMF-DESCR-LINE(2),        ' ',                         ELTPODIA
00823            CMF-DESCR-LINE(3)                                      ELTPODIA
00824            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTPODIA
00825                                                                   ELTPODIA
00826      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPODIA
00827                                                                   ELTPODIA
00828      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPODIA
00829      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPODIA
00830      MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTPODIA
00831                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTPODIA
00832                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPODIA
00833      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPODIA
00834                                                                   ELTPODIA
00835      IF WS-MOVE-LINES-TO-CIA                                      ELTPODIA
00836         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPODIA
00837            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPODIA
00838                                             WS-TEMP-NOT-USED-CNT  ELTPODIA
00839            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTPODIA
00840               VARYING  WS-SUB1  FROM  1  BY  1                    ELTPODIA
00841               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTPODIA
00842            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPODIA
00843            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPODIA
00844            ADD +1  TO  WS-CIA                                     ELTPODIA
00845         ELSE                                                      ELTPODIA
00846            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTPODIA
00847            ADD +1  TO  WS-CIA.                                    ELTPODIA
00848                                                                   ELTPODIA
00849      IF WS-MOVE-LINES-TO-CIA                                      ELTPODIA
00850         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPODIA
00851            PERFORM 2160-MOVE-LINES-TO-CIA                         ELTPODIA
00852               VARYING  WS-SUB1  FROM  2  BY  1                    ELTPODIA
00853               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTPODIA
00854         ELSE                                                      ELTPODIA
00855            NEXT SENTENCE                                          ELTPODIA
00856      ELSE                                                         ELTPODIA
00857         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPODIA
00858                                                                   ELTPODIA
00859      MOVE 'N' TO WS-INDENT-IND.                                   ELTPODIA
00860      GO TO 2199-EXIT.                                             ELTPODIA
00861                                                                   ELTPODIA
00862  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTPODIA
00863      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTPODIA
00864      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTPODIA
00865                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTPODIA
00866                                                                   ELTPODIA
00867  2160-MOVE-LINES-TO-CIA.                                          ELTPODIA
00868      IF WS-INDENT-ON                                              ELTPODIA
00869        MOVE TCAR-OPF-DATA(WS-SUB1)  TO  WS-DTL-INDENTED           ELTPODIA
00870        MOVE WS-INDENTED             TO  COF-DTL-LINE(WS-CIA)      ELTPODIA
00871      ELSE                                                         ELTPODIA
00872       MOVE TCAR-OPF-DATA(WS-SUB1)  TO COF-DTL-LINE(WS-CIA).       ELTPODIA
00873      ADD +1  TO  WS-CIA.                                          ELTPODIA
00874                                                                   ELTPODIA
00875  2199-EXIT.           EXIT.                                       ELTPODIA
00876                                                                   ELTPODIA
00877 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPODIA
00878  2200-CODES-MANUAL-WITH-AMOUNT.                                   ELTPODIA
00879                                                                   ELTPODIA
00880      INITIALIZE CMF-RETURN-CODE                                   ELTPODIA
00881                 TCAR-FROM-AREA.                                   ELTPODIA
00882                                                                   ELTPODIA
00883                                                                   ELTPODIA
00884      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTPODIA
00885             END-EXEC.                                             ELTPODIA
00886                                                                   ELTPODIA
00887      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPODIA
00888      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00889          ADDRESS OF CMF-DESCR.                                    ELTPODIA
00890                                                                   ELTPODIA
00891      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPODIA
00892         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPODIA
00893         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTPODIA
00894            CMF-DESCR-LINE(1),        ' ',                         ELTPODIA
00895            CMF-DESCR-LINE(2),        ' ',                         ELTPODIA
00896            CMF-DESCR-LINE(3), ' ',        WS-PRCNT-PERDM-ALLOW    ELTPODIA
00897            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTPODIA
00898      ELSE                                                         ELTPODIA
00899         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTPODIA
00900         STRING CMF-DESCR-LINE(1),        ' ',                     ELTPODIA
00901            CMF-DESCR-LINE(2),        ' ',                         ELTPODIA
00902            CMF-DESCR-LINE(3),        ' ',  WS-PRCNT-PERDM-ALLOW   ELTPODIA
00903            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTPODIA
00904                                                                   ELTPODIA
00905      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPODIA
00906                                                                   ELTPODIA
00907      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPODIA
00908      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPODIA
00909      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTPODIA
00910                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTPODIA
00911                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPODIA
00912      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPODIA
00913                                                                   ELTPODIA
00914      IF WS-MOVE-LINES-TO-CIA                                      ELTPODIA
00915         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPODIA
00916            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPODIA
00917                                             WS-TEMP-NOT-USED-CNT  ELTPODIA
00918            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTPODIA
00919               VARYING  WS-SUB1  FROM  1  BY  1                    ELTPODIA
00920               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTPODIA
00921            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPODIA
00922            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPODIA
00923            ADD +1  TO  WS-CIA                                     ELTPODIA
00924         ELSE                                                      ELTPODIA
00925            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTPODIA
00926            ADD +1  TO  WS-CIA.                                    ELTPODIA
00927                                                                   ELTPODIA
00928      IF WS-MOVE-LINES-TO-CIA                                      ELTPODIA
00929         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPODIA
00930            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTPODIA
00931               VARYING  WS-SUB1  FROM  2  BY  1                    ELTPODIA
00932               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTPODIA
00933         ELSE                                                      ELTPODIA
00934            NEXT SENTENCE                                          ELTPODIA
00935      ELSE                                                         ELTPODIA
00936         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPODIA
00937                                                                   ELTPODIA
00938      GO TO 2299-EXIT.                                             ELTPODIA
00939                                                                   ELTPODIA
00940  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTPODIA
00941      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTPODIA
00942      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTPODIA
00943                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTPODIA
00944                                                                   ELTPODIA
00945  2260-MOVE-LINES-TO-CIA.                                          ELTPODIA
00946      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTPODIA
00947      ADD +1  TO  WS-CIA.                                          ELTPODIA
00948                                                                   ELTPODIA
00949  2299-EXIT.           EXIT.                                       ELTPODIA
00950                                                                   ELTPODIA
00951 /            G E T   T A B U L A R   R E C O R D                  ELTPODIA
00952 ***************************************************************** ELTPODIA
00953 *            G E T   T A B U L A R   R E C O R D                  ELTPODIA
00954 *                                                                 ELTPODIA
00955 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTPODIA
00956 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTPODIA
00957 *  TO DISPLAY.                                                    ELTPODIA
00958 *                                                                 ELTPODIA
00959 ***************************************************************** ELTPODIA
00960  2300-GET-TABULAR-RECORD.                                         ELTPODIA
00961                                                                   ELTPODIA
00962      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPODIA
00963      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPODIA
00964          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTPODIA
00965      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTPODIA
00966      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTPODIA
00967      SET IOP-RD                          TO TRUE.                 ELTPODIA
00968      SET IOP-FCQ-NONE                    TO TRUE.                 ELTPODIA
00969      SET IOP-KVQ-NONE                    TO TRUE.                 ELTPODIA
00970      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTPODIA
00971                                                                   ELTPODIA
00972      EXEC CICS LINK                                               ELTPODIA
00973                PROGRAM ('ELUIOPGM')                               ELTPODIA
00974                COMMAREA (DFHCOMMAREA)                             ELTPODIA
00975      END-EXEC.                                                    ELTPODIA
00976                                                                   ELTPODIA
00977                                                                   ELTPODIA
00978      IF IOP-RC-NOTFND                                             ELTPODIA
00979         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTPODIA
00980         EXEC CICS ABEND                                           ELTPODIA
00981                   ABCODE(CIA-ABCODE)                              ELTPODIA
00982         END-EXEC                                                  ELTPODIA
00983      ELSE                                                         ELTPODIA
00984          IF NOT IOP-RC-OK                                         ELTPODIA
00985             SET CIA-AB-CRITIO TO TRUE                             ELTPODIA
00986             EXEC CICS ABEND                                       ELTPODIA
00987                       ABCODE(CIA-ABCODE)                          ELTPODIA
00988             END-EXEC                                              ELTPODIA
00989      END-IF.                                                      ELTPODIA
00990  2399-EXIT.           EXIT.                                       ELTPODIA
00991 /                                                                 ELTPODIA
00992  5000-PLACE-OF-TREATMENT-BASIC.                                   ELTPODIA
00993 **---------------------------------------------------------------+ELTPODIA
00994 **                                                               |ELTPODIA
00995 **        P L A C E   O F   T R E A T M E N T                    |ELTPODIA
00996      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTPODIA
00997                                                              ZERO ELTPODIA
00998         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
00999         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTPODIA
01000         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTPODIA
01001                                                   CMF-CODE-VALUE  ELTPODIA
01002         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTPODIA
01003         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPODIA
01004         MOVE 'Y'                   TO  WS-INDENT-IND              ELTPODIA
01005         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.       ELTPODIA
01006  5099-EXIT.  EXIT.                                                ELTPODIA
01007 /                                                                 ELTPODIA
01008  5100-PLACE-OF-TREATMENT-SUPP.                                    ELTPODIA
01009 **---------------------------------------------------------------+ELTPODIA
01010 **                                                               |ELTPODIA
01011 **        P L A C E   O F   T R E A T M E N T                    |ELTPODIA
01012      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTPODIA
01013                                                              ZERO ELTPODIA
01014         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
01015         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTPODIA
01016         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTPODIA
01017                                                   CMF-CODE-VALUE  ELTPODIA
01018         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTPODIA
01019         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPODIA
01020         MOVE 'Y'                   TO  WS-INDENT-IND              ELTPODIA
01021         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPODIA
01022         ADD  +1      TO  WS-CIA.                                  ELTPODIA
01023  5199-EXIT.  EXIT.                                                ELTPODIA
01024 /                                                                 ELTPODIA
01025  6000-SCAN-TAB.                                                   ELTPODIA
01026                                                                   ELTPODIA
01027      PERFORM 6200-BEN-TAB-AAR THRU 6299-EXIT.                     ELTPODIA
01028      PERFORM 6300-BEN-TAB-PPF THRU 6399-EXIT.                     ELTPODIA
01029      ADD +1           TO WS-CIA.                                  ELTPODIA
01030      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTPODIA
01031      ADD +2           TO WS-CIA.                                  ELTPODIA
01032      PERFORM 8000-OUTPUT-TEXT THRU 8099-EXIT.                     ELTPODIA
01033      PERFORM 6500-BEN-TAB-ADL THRU 6599-EXIT.                     ELTPODIA
01034      PERFORM 6600-BEN-TAB-ABM THRU 6699-EXIT.                     ELTPODIA
01035      PERFORM 6700-BEN-TAB-ACL THRU 6799-EXIT.                     ELTPODIA
01036      PERFORM 6800-BEN-TAB-AOL THRU 6899-EXIT.                     ELTPODIA
01037                                                                   ELTPODIA
01038      ADD   +2     TO  WS-CIA.                                     ELTPODIA
01039      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTPODIA
01040      PERFORM 8000-OUTPUT-TEXT THRU 8099-EXIT.                     ELTPODIA
01041                                                                   ELTPODIA
01042                                                                   ELTPODIA
01043 /                                                                 ELTPODIA
01044  6200-BEN-TAB-AAR.                                                ELTPODIA
01045      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTPODIA
01046      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01047                                                                   ELTPODIA
01048      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01049          NOT = LOW-VALUES                                         ELTPODIA
01050       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01051          NOT = SPACE                                              ELTPODIA
01052                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTPODIA
01053                                                                   ELTPODIA
01054      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01055                                                                   ELTPODIA
01056      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01057          NOT = LOW-VALUES                                         ELTPODIA
01058       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01059          NOT = SPACE                                              ELTPODIA
01060                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTPODIA
01061                                                                   ELTPODIA
01062      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTPODIA
01063             MOVE +2                  TO WS-CIA                    ELTPODIA
01064             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTPODIA
01065             PERFORM 8000-OUTPUT-TEXT.                             ELTPODIA
01066  6299-EXIT.  EXIT.                                                ELTPODIA
01067 /                                                                 ELTPODIA
01068  6300-BEN-TAB-PPF.                                                ELTPODIA
01069      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPODIA
01070                       WS-HOLD2.                                   ELTPODIA
01071      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01072      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01073          NOT = LOW-VALUES                                         ELTPODIA
01074       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01075          NOT = SPACE                                              ELTPODIA
01076             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01077                        TO  WS-HOLD1.                              ELTPODIA
01078                                                                   ELTPODIA
01079      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01080      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01081          NOT = LOW-VALUES                                         ELTPODIA
01082       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01083          NOT = SPACE                                              ELTPODIA
01084             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01085                        TO  WS-HOLD2.                              ELTPODIA
01086                                                                   ELTPODIA
01087      IF WS-HOLD1 = WS-HOLD2                                       ELTPODIA
01088         IF WS-HOLD1 = ZEROS                                       ELTPODIA
01089                 GO TO 6399-EXIT                                   ELTPODIA
01090         ELSE                                                      ELTPODIA
01091             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPODIA
01092             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01093             EXEC CICS  LINK                                       ELTPODIA
01094                        PROGRAM('ELGPPF')                          ELTPODIA
01095                        COMMAREA(DFHCOMMAREA)                      ELTPODIA
01096             END-EXEC                                              ELTPODIA
01097             GO TO 6399-EXIT.                                      ELTPODIA
01098                                                                   ELTPODIA
01099      IF WS-HOLD1 = ZEROS                                          ELTPODIA
01100          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPODIA
01101          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01102          EXEC CICS  LINK                                          ELTPODIA
01103                     PROGRAM('ELGPPF')                             ELTPODIA
01104                     COMMAREA(DFHCOMMAREA)                         ELTPODIA
01105          END-EXEC                                                 ELTPODIA
01106      ELSE                                                         ELTPODIA
01107          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPODIA
01108          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01109          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTPODIA
01110                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01111          END-EXEC                                                 ELTPODIA
01112          IF WS-HOLD2 = ZEROS                                      ELTPODIA
01113            GO TO 6399-EXIT                                        ELTPODIA
01114          ELSE                                                     ELTPODIA
01115             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPODIA
01116             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01117             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTPODIA
01118                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01119             END-EXEC.                                             ELTPODIA
01120  6399-EXIT.    EXIT.                                              ELTPODIA
01121 /                                                                 ELTPODIA
01122  6500-BEN-TAB-ADL.                                                ELTPODIA
01123      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPODIA
01124                       WS-HOLD2.                                   ELTPODIA
01125      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01126      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01127          NOT = LOW-VALUES                                         ELTPODIA
01128       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01129          NOT = SPACE                                              ELTPODIA
01130             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01131                        TO  WS-HOLD1.                              ELTPODIA
01132                                                                   ELTPODIA
01133      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01134      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01135          NOT = LOW-VALUES                                         ELTPODIA
01136       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01137          NOT = SPACE                                              ELTPODIA
01138             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01139                        TO  WS-HOLD2.                              ELTPODIA
01140                                                                   ELTPODIA
01141      IF WS-HOLD1 = WS-HOLD2                                       ELTPODIA
01142         IF WS-HOLD1 = ZEROS                                       ELTPODIA
01143                 GO TO 6599-EXIT                                   ELTPODIA
01144         ELSE                                                      ELTPODIA
01145             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPODIA
01146             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01147              EXEC CICS  LINK PROGRAM('ELGDEDBL')                  ELTPODIA
01148                              COMMAREA(DFHCOMMAREA)                ELTPODIA
01149              END-EXEC                                             ELTPODIA
01150              GO TO 6599-EXIT.                                     ELTPODIA
01151                                                                   ELTPODIA
01152      IF WS-HOLD1 = ZEROS                                          ELTPODIA
01153          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPODIA
01154          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01155          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTPODIA
01156                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01157          END-EXEC                                                 ELTPODIA
01158      ELSE                                                         ELTPODIA
01159          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPODIA
01160          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01161          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTPODIA
01162                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01163          END-EXEC                                                 ELTPODIA
01164          IF WS-HOLD2 = ZEROS                                      ELTPODIA
01165              GO TO 6599-EXIT                                      ELTPODIA
01166          ELSE                                                     ELTPODIA
01167             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPODIA
01168             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01169             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTPODIA
01170                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01171             END-EXEC.                                             ELTPODIA
01172  6599-EXIT.     EXIT.                                             ELTPODIA
01173 /                                                                 ELTPODIA
01174  6600-BEN-TAB-ABM.                                                ELTPODIA
01175      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPODIA
01176                       WS-HOLD2.                                   ELTPODIA
01177      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01178      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01179          NOT = LOW-VALUES                                         ELTPODIA
01180       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01181          NOT = SPACE                                              ELTPODIA
01182             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01183                        TO  WS-HOLD1.                              ELTPODIA
01184                                                                   ELTPODIA
01185      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01186      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01187          NOT = LOW-VALUES                                         ELTPODIA
01188       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01189          NOT = SPACE                                              ELTPODIA
01190             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01191                        TO  WS-HOLD2.                              ELTPODIA
01192                                                                   ELTPODIA
01193      IF WS-HOLD1 = WS-HOLD2                                       ELTPODIA
01194         IF WS-HOLD1 = ZEROS                                       ELTPODIA
01195                 GO TO 6699-EXIT                                   ELTPODIA
01196         ELSE                                                      ELTPODIA
01197             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPODIA
01198             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01199             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTPODIA
01200                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01201             END-EXEC                                              ELTPODIA
01202             GO TO 6699-EXIT.                                      ELTPODIA
01203                                                                   ELTPODIA
01204      IF WS-HOLD1 = ZEROS                                          ELTPODIA
01205          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPODIA
01206          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01207          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTPODIA
01208                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01209          END-EXEC                                                 ELTPODIA
01210      ELSE                                                         ELTPODIA
01211          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPODIA
01212          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01213          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTPODIA
01214                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01215          END-EXEC                                                 ELTPODIA
01216          IF WS-HOLD2 = ZEROS                                      ELTPODIA
01217            GO TO 6699-EXIT                                        ELTPODIA
01218          ELSE                                                     ELTPODIA
01219              MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                   ELTPODIA
01220              PERFORM 2300-GET-TABULAR-RECORD                      ELTPODIA
01221              EXEC CICS  LINK PROGRAM('ELGMAXIM')                  ELTPODIA
01222                              COMMAREA(DFHCOMMAREA)                ELTPODIA
01223              END-EXEC.                                            ELTPODIA
01224  6699-EXIT.     EXIT.                                             ELTPODIA
01225 /                                                                 ELTPODIA
01226  6700-BEN-TAB-ACL.                                                ELTPODIA
01227      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPODIA
01228                       WS-HOLD2.                                   ELTPODIA
01229      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01230      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01231          NOT = LOW-VALUES                                         ELTPODIA
01232       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01233          NOT = SPACE                                              ELTPODIA
01234             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01235                        TO  WS-HOLD1.                              ELTPODIA
01236                                                                   ELTPODIA
01237      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01238      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01239          NOT = LOW-VALUES                                         ELTPODIA
01240       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01241          NOT = SPACE                                              ELTPODIA
01242             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01243                        TO  WS-HOLD2.                              ELTPODIA
01244                                                                   ELTPODIA
01245      IF WS-HOLD1 = WS-HOLD2                                       ELTPODIA
01246         IF WS-HOLD1 = ZEROS                                       ELTPODIA
01247                 GO TO 6799-EXIT                                   ELTPODIA
01248         ELSE                                                      ELTPODIA
01249             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPODIA
01250             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01251             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTPODIA
01252                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01253             END-EXEC                                              ELTPODIA
01254             GO TO 6799-EXIT.                                      ELTPODIA
01255                                                                   ELTPODIA
01256      IF WS-HOLD1 = ZEROS                                          ELTPODIA
01257          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPODIA
01258          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01259          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTPODIA
01260                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01261          END-EXEC                                                 ELTPODIA
01262      ELSE                                                         ELTPODIA
01263          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPODIA
01264          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01265          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTPODIA
01266                     COMMAREA(DFHCOMMAREA)                         ELTPODIA
01267          END-EXEC                                                 ELTPODIA
01268          IF WS-HOLD2 = ZEROS                                      ELTPODIA
01269            GO TO 6799-EXIT                                        ELTPODIA
01270          ELSE                                                     ELTPODIA
01271             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPODIA
01272             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01273             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTPODIA
01274                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01275             END-EXEC.                                             ELTPODIA
01276  6799-EXIT.     EXIT.                                             ELTPODIA
01277 /                                                                 ELTPODIA
01278  6800-BEN-TAB-AOL.                                                ELTPODIA
01279      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPODIA
01280                       WS-HOLD2.                                   ELTPODIA
01281      SET PLT-INDEX2 TO 1.                                         ELTPODIA
01282      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01283          NOT = LOW-VALUES                                         ELTPODIA
01284       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01285          NOT = SPACE                                              ELTPODIA
01286             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01287                        TO  WS-HOLD1.                              ELTPODIA
01288                                                                   ELTPODIA
01289      SET PLT-INDEX2 TO 2.                                         ELTPODIA
01290      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01291          NOT = LOW-VALUES                                         ELTPODIA
01292       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01293          NOT = SPACE                                              ELTPODIA
01294             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01295                        TO  WS-HOLD2.                              ELTPODIA
01296                                                                   ELTPODIA
01297      IF WS-HOLD1 = WS-HOLD2                                       ELTPODIA
01298         IF WS-HOLD1 = ZEROS                                       ELTPODIA
01299                 GO TO 6899-EXIT                                   ELTPODIA
01300         ELSE                                                      ELTPODIA
01301             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPODIA
01302             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01303             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTPODIA
01304                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01305             END-EXEC                                              ELTPODIA
01306             GO TO 6899-EXIT.                                      ELTPODIA
01307                                                                   ELTPODIA
01308      IF WS-HOLD1 = ZEROS                                          ELTPODIA
01309          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPODIA
01310          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01311          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTPODIA
01312                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01313          END-EXEC                                                 ELTPODIA
01314      ELSE                                                         ELTPODIA
01315          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPODIA
01316          PERFORM 2300-GET-TABULAR-RECORD                          ELTPODIA
01317          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTPODIA
01318                          COMMAREA(DFHCOMMAREA)                    ELTPODIA
01319          END-EXEC                                                 ELTPODIA
01320          IF WS-HOLD2 = ZEROS                                      ELTPODIA
01321            GO TO 6899-EXIT                                        ELTPODIA
01322          ELSE                                                     ELTPODIA
01323             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPODIA
01324             PERFORM 2300-GET-TABULAR-RECORD                       ELTPODIA
01325             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTPODIA
01326                             COMMAREA(DFHCOMMAREA)                 ELTPODIA
01327             END-EXEC.                                             ELTPODIA
01328  6899-EXIT.     EXIT.                                             ELTPODIA
01329 /                                                                 ELTPODIA
01330  7000-SPILLOVER-COINS.                                            ELTPODIA
01331 **---------------------------------------------------------------+ELTPODIA
01332 **                                                               |ELTPODIA
01333 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTPODIA
01334      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTPODIA
01335                                                       NOT =  '0'  ELTPODIA
01336         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
01337         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPODIA
01338                                           CMF-ELEMENT-SYSTEM-NAME ELTPODIA
01339         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPODIA
01340                                           TO   CMF-CODE-VALUE     ELTPODIA
01341         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTPODIA
01342         MOVE 56 TO WS-TEMP-NOT-USED-CNT                           ELTPODIA
01343         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPODIA
01344         ADD +1  TO  WS-CIA.                                       ELTPODIA
01345  7099-EXIT.    EXIT.                                              ELTPODIA
01346 /                                                                 ELTPODIA
01347  7200-SPILLOVER-DEDUCT.                                           ELTPODIA
01348 **---------------------------------------------------------------+ELTPODIA
01349 **                                                               |ELTPODIA
01350 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTPODIA
01351      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTPODIA
01352                                                       NOT =  '0'  ELTPODIA
01353         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
01354         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPODIA
01355                                           CMF-ELEMENT-SYSTEM-NAME ELTPODIA
01356         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTPODIA
01357                                                     CMF-CODE-VALUEELTPODIA
01358         MOVE WS-SPILLOVER-DEDUCT TO WS-TEMP-TEXT-AREA             ELTPODIA
01359         MOVE 57 TO WS-TEMP-NOT-USED-CNT                           ELTPODIA
01360         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPODIA
01361         ADD +1  TO  WS-CIA.                                       ELTPODIA
01362 **                                                               |ELTPODIA
01363 **---------------------------------------------------------------+ELTPODIA
01364  7299-EXIT.    EXIT.                                              ELTPODIA
01365                                                                   ELTPODIA
01366  7300-TRANS-OTHER-RESP-IND.                                       ELTPODIA
01367 **---------------------------------------------------------------+ELTPODIA
01368 **                                                               |ELTPODIA
01369 **   TRANSFER TO OTHER RESPONSIBILITY INDICATOR                  |ELTPODIA
01370                                                                   ELTPODIA
01371      IF PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)         ELTPODIA
01372         NOT EQUAL ZEROS                                           ELTPODIA
01373         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPODIA
01374         MOVE 'TRANSF-OTHER-RESP-IND'  TO                          ELTPODIA
01375                                           CMF-ELEMENT-SYSTEM-NAME ELTPODIA
01376         MOVE PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)    ELTPODIA
01377              TO   CMF-CODE-VALUE                                  ELTPODIA
01378         MOVE SPACES              TO WS-TEMP-TEXT-AREA             ELTPODIA
01379         MOVE +0 TO WS-TEMP-NOT-USED-CNT                           ELTPODIA
01380         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPODIA
01381         ADD +1  TO  WS-CIA.                                       ELTPODIA
01382 **                                                               |ELTPODIA
01383 **---------------------------------------------------------------+ELTPODIA
01384  7399-EXIT.    EXIT.                                              ELTPODIA
01385                                                                   ELTPODIA
01386 /                                                                 ELTPODIA
01387  8000-OUTPUT-TEXT.                                                ELTPODIA
01388       MOVE +0     TO COF-NBR-HDR-LINES.                           ELTPODIA
01389       MOVE WS-CIA TO COF-NBR-DTL-LINES.                           ELTPODIA
01390       MOVE ' '    TO  COF-FUNCTION.                               ELTPODIA
01391       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTPODIA
01392              COMMAREA(DFHCOMMAREA)                                ELTPODIA
01393               END-EXEC.                                           ELTPODIA
01394       MOVE 1  TO  WS-CIA.                                         ELTPODIA
01395  8099-EXIT.   EXIT.                                               ELTPODIA
01396 / C O M P R E S S  A N D  E X P A N D  S U B R O U T I N E S      ELTPODIA
01397      COPY ELSTCOMP.                                               ELTPODIA
