00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTCOSMS.                                            ELTCOSMS
00003  AUTHOR. NINA CERVANTES.                                             LV002
00004  DATE-WRITTEN.  05/15/86.                                         ELTCOSMS
00005  DATE-COMPILED.                                                   ELTCOSMS
00006      SKIP3                                                        ELTCOSMS
00007 ******************************************************************ELTCOSMS
00008 *  ELTCOSMS                                                       ELTCOSMS
00009 *                                                                 ELTCOSMS
00010 *                        PROGRAM ABSTRACT                         ELTCOSMS
00011 *                                                                 ELTCOSMS
00012 *   PROGRAM NAME:   E.L.S. COSMETIC SURGERY TOPIC                 ELTCOSMS
00013 *                                                                 ELTCOSMS
00014 *   PROGRAM I.D.:   ELTCOSMS                                      ELTCOSMS
00015 *                                                                 ELTCOSMS
00016 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE COSMETICELTCOSMS
00017 *              BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTCOSMS
00018 *                                                                 ELTCOSMS
00019 *   OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF COSMETIC COVERAGE ELTCOSMS
00020 *              AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTCOSMS
00021 *              GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTCOSMS
00022 *              THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTCOSMS
00023 *              RANGE OF DATES.                                    ELTCOSMS
00024 *                                                                 ELTCOSMS
00025 *   RECORDS                                                       ELTCOSMS
00026 *   ACCESSED:  GROUP SPECIFIC                                     ELTCOSMS
00027 *              CONTRACT                                           ELTCOSMS
00028 *              VARIOUS BENEFIT PROVISIONS                         ELTCOSMS
00029 *              LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDSELTCOSMS
00030 *                                                                 ELTCOSMS
00031 *  NUM    DATE   PGMR  REASON                                     ELTCOSMS
00032 * XXXXX 08/14/86  NAC  1. REMOVE CODE THAT FORMS SECOND HEADER    ELTCOSMS
00033 *                         LINE AND USE THE ONE FORMED IN ELPROLOG.ELTCOSMS
00034 *                                                                 ELTCOSMS
00035 * XXXXX 10/02/86  JTC  VS COBOL II CONVERSION                     ELTCOSMS
00036 *                                                                 ELTCOSMS
00037 * XXXXX 10/19/87  NAC  REWORD PHRASE FOR COVERED BENEFITS.        ELTCOSMS
00038 *                                                                 ELTCOSMS
00039 * XXXXX 04/10/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.           ELTCOSMS
00040 *                                                                 ELTCOSMS
00041 * XXXXX 10/23/89  RKH  ADDED TRANSFER TO OTHER RESPONSIBILITY IND ELTCOSMS
00042 *                                                                 ELTCOSMS
00043 *       02-FEB-90 RJL CORRECTED TEST OF CONTRACT POINTERS         ELTCOSMS
00044 *                                                                 ELTCOSMS
00045 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY INELTCOSMS
00046 *                      FROM A SINGLE POSITION TO ZEROS            ELTCOSMS
00047 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTCOSMS
00048 *                                                                 ELTCOSMS
00049 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTCOSMS
00050 ***************************************************************** ELTCOSMS
00051 /                                                                 ELTCOSMS
00052  ENVIRONMENT DIVISION.                                            ELTCOSMS
00053      SKIP3                                                        ELTCOSMS
00054  DATA DIVISION.                                                   ELTCOSMS
00055  WORKING-STORAGE SECTION.                                         ELTCOSMS
00056  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTCOSMS
00057      '***ELTCOSMS WS BEGINS***'.                                  ELTCOSMS
00058  01  WS-PARA-COMMENTS.                                            ELTCOSMS
00059    05  WS-PARA-ID1               PIC X(4) VALUE 'XXXX'.           ELTCOSMS
00060    05  WS-PARA-ID2               PIC X(4) VALUE 'XXXX'.           ELTCOSMS
00061                                                                   ELTCOSMS
00062  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTCOSMS
00063                                                                   ELTCOSMS
00064 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTCOSMS
00065  01  WS-WORK-FIELDS.                                              ELTCOSMS
00066      05  WS-CHAR-0                     PIC X.                     ELTCOSMS
00067      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTCOSMS
00068      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTCOSMS
00069      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTCOSMS
00070      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTCOSMS
00071      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTCOSMS
00072      05  WS-SUB5                       PIC S999  COMP-3 VALUE +0. ELTCOSMS
00073      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTCOSMS
00074      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTCOSMS
00075      05  WS-FIRSTTIME-IND              PIC X.                     ELTCOSMS
00076        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTCOSMS
00077      05  WS-BASIC-SUPP                 PIC X.                     ELTCOSMS
00078        88  BASIC-SUPP-LINE                 VALUE 'Y'.             ELTCOSMS
00079      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTCOSMS
00080      05  WS-PERCENT-FLD.                                          ELTCOSMS
00081        10  WS-PERCENTAGE               PIC ZZ9.                   ELTCOSMS
00082        10  WS-PERCENT-SIGN             PIC X.                     ELTCOSMS
00083                                                                   ELTCOSMS
00084      05  WS-PRINT-COMMON-LINE          PIC X.                     ELTCOSMS
00085        88  PRINT-COMMON-LINE               VALUE 'Y'.             ELTCOSMS
00086                                                                   ELTCOSMS
00087 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTCOSMS
00088  01  WS-BEN-PROV-ID.                                              ELTCOSMS
00089      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +1. ELTCOSMS
00090      05  WS-PROF-IP-CNT                PIC S999 COMP-3  VALUE +1. ELTCOSMS
00091      05  WS-PROF-IP-TAB.                                          ELTCOSMS
00092        10  FILLER                      PIC X(6)  VALUE 'COSI C'.  ELTCOSMS
00093      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTCOSMS
00094                                        PIC X(6)  OCCURS 1 TIMES.  ELTCOSMS
00095                                                                   ELTCOSMS
00096      05  WS-PROF-OP-CNT                PIC S999 COMP-3  VALUE +1. ELTCOSMS
00097      05  WS-PROF-OP-TAB.                                          ELTCOSMS
00098        10  FILLER                      PIC X(6)  VALUE 'COSO C'.  ELTCOSMS
00099      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTCOSMS
00100                                        PIC X(6)  OCCURS 1 TIMES.  ELTCOSMS
00101                                                                   ELTCOSMS
00102 /            D I S P L A Y   L I N E S                            ELTCOSMS
00103  01  WS-ELS-DISPLAY-LINES.                                        ELTCOSMS
00104                                                                   ELTCOSMS
00105    05  WS-HDR-2-PROF-IP.                                          ELTCOSMS
00106      10  FILLER                    PIC X(20) VALUE SPACES.        ELTCOSMS
00107      10  FILLER                    PIC X(39)                      ELTCOSMS
00108          VALUE 'COSMETIC SURGERY INPATIENT PROFESSIONAL'.         ELTCOSMS
00109      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTCOSMS
00110                                                                   ELTCOSMS
00111    05  WS-HDR-2-PROF-OP.                                          ELTCOSMS
00112      10  FILLER                    PIC X(20) VALUE SPACES.        ELTCOSMS
00113      10  FILLER                    PIC X(40)                      ELTCOSMS
00114          VALUE 'COSMETIC SURGERY OUTPATIENT PROFESSIONAL'.        ELTCOSMS
00115      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTCOSMS
00116                                                                   ELTCOSMS
00117    05  WS-HDR-2-INST.                                             ELTCOSMS
00118      10  FILLER                    PIC X(23) VALUE SPACES.        ELTCOSMS
00119      10  FILLER                    PIC X(30)                      ELTCOSMS
00120          VALUE 'COSMETIC SURGERY INSTITUTIONAL'.                  ELTCOSMS
00121      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTCOSMS
00122                                                                   ELTCOSMS
00123    05  WS-HDR-2-COINS-BEN-LVL.                                    ELTCOSMS
00124      10  FILLER                    PIC X(30) VALUE SPACES.        ELTCOSMS
00125      10  FILLER                    PIC X(28)                      ELTCOSMS
00126          VALUE 'COINSURANCE AT BENEFIT LEVEL'.                    ELTCOSMS
00127      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTCOSMS
00128                                                                   ELTCOSMS
00129                                                                   ELTCOSMS
00130    05  WS-COSMETIC-SURGERY         PIC X(21)                      ELTCOSMS
00131          VALUE 'COSMETIC SURGERY IS  '.                           ELTCOSMS
00132                                                                   ELTCOSMS
00133    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTCOSMS
00134          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTCOSMS
00135                                                                   ELTCOSMS
00136    05  WS-CRITERIA-FOR             PIC X(33)   VALUE              ELTCOSMS
00137        'THE CRITERIA FOR ELIGIBILITY IS: '.                       ELTCOSMS
00138                                                                   ELTCOSMS
00139    05  WS-FOLLOWING-BEN.                                          ELTCOSMS
00140      10  FILLER                    PIC X(21) VALUE                ELTCOSMS
00141          'COVERED SERVICES ARE:'.                                 ELTCOSMS
00142                                                                   ELTCOSMS
00143    05  WS-SERVICES-2ND.                                           ELTCOSMS
00144      10  FILLER                    PIC X(21) VALUE SPACES.        ELTCOSMS
00145      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTCOSMS
00146      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTCOSMS
00147                                                                   ELTCOSMS
00148    05  WS-PAY-CONSDR-TEXT1.                                       ELTCOSMS
00149      10  FILLER                    PIC X(45)                      ELTCOSMS
00150        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTCOSMS
00151                                                                   ELTCOSMS
00152    05  WS-PAY-CONSDR-TEXT2.                                       ELTCOSMS
00153      10  FILLER                    PIC X(44)                      ELTCOSMS
00154        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTCOSMS
00155                                                                   ELTCOSMS
00156    05  WS-PAYMNT-BASED.                                           ELTCOSMS
00157      10  FILLER                    PIC X(20)                      ELTCOSMS
00158          VALUE 'PAYMENT IS BASED ON:'.                            ELTCOSMS
00159      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTCOSMS
00160                                                                   ELTCOSMS
00161    05  WS-BASIC-LIT              PIC X(15)                        ELTCOSMS
00162          VALUE '        BASIC: '.                                 ELTCOSMS
00163                                                                   ELTCOSMS
00164    05  WS-BASIC-SUPP-LINE.                                        ELTCOSMS
00165      10  FILLER                    PIC X(15) VALUE SPACES.        ELTCOSMS
00166      10  WS-DTL-BASIC-SUPP         PIC X(50) VALUE SPACES.        ELTCOSMS
00167      10  FILLER                    PIC X(13) VALUE LOW-VALUES.    ELTCOSMS
00168                                                                   ELTCOSMS
00169    05  WS-SUPP-LIT               PIC X(15)                        ELTCOSMS
00170          VALUE ' SUPPLEMENTAL: '.                                 ELTCOSMS
00171                                                                   ELTCOSMS
00172    05  WS-PAYABLE-AS.                                             ELTCOSMS
00173      10  FILLER                    PIC X(40) VALUE                ELTCOSMS
00174          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTCOSMS
00175      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTCOSMS
00176                                                                   ELTCOSMS
00177    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER'.  ELTCOSMS
00178                                                                   ELTCOSMS
00179                                                                   ELTCOSMS
00180    05  WS-SEE-ROOM-AND-BOARD.                                     ELTCOSMS
00181      10  FILLER                    PIC X(79)    VALUE             ELTCOSMS
00182        'SEE ROOM AND BOARD FOR ADDITIONAL INPATIENT BENEFIT INFORMELTCOSMS
00183 -      'ATION.'.                                                  ELTCOSMS
00184                                                                   ELTCOSMS
00185    05  WS-SEE-OUTPATIENT-SURGERY.                                 ELTCOSMS
00186      10  FILLER                    PIC X(79)    VALUE             ELTCOSMS
00187        'SEE OUTPATIENT SURGERY TOPIC FOR OUTPATIENT BENEFIT INFORMELTCOSMS
00188 -      'ATION.'.                                                  ELTCOSMS
00189                                                                   ELTCOSMS
00190    05  WS-CONTRACT-RELATED.                                       ELTCOSMS
00191      10  FILLER                    PIC X(49)                      ELTCOSMS
00192        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTCOSMS
00193      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTCOSMS
00194                                                                   ELTCOSMS
00195    05  WS-PVE.                                                    ELTCOSMS
00196      10  FILLER                    PIC X(79)   VALUE              ELTCOSMS
00197        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTCOSMS
00198                                                                   ELTCOSMS
00199    05  WS-COSMETIC-TOPIC.                                         ELTCOSMS
00200      10  FILLER                    PIC X(23)                      ELTCOSMS
00201        VALUE 'COSMETIC BENEFIT TOPIC '.                           ELTCOSMS
00202      10  FILLER                    PIC X(54) VALUE LOW-VALUES.    ELTCOSMS
00203                                                                   ELTCOSMS
00204    05  WS-NO-TABULAR1.                                            ELTCOSMS
00205      10  FILLER                    PIC X(51)  VALUE               ELTCOSMS
00206         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTCOSMS
00207      10  FILLER                    PIC X(22)  VALUE               ELTCOSMS
00208         'GOING FROM BENEFIT ***'.                                 ELTCOSMS
00209                                                                   ELTCOSMS
00210    05  WS-NO-TABULAR2.                                            ELTCOSMS
00211      10  FILLER                    PIC X(15)  VALUE               ELTCOSMS
00212         '*** PROVISION: '.                                        ELTCOSMS
00213      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTCOSMS
00214      10  FILLER                    PIC X VALUE SPACE.             ELTCOSMS
00215      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTCOSMS
00216      10  FILLER                    PIC X(13)  VALUE               ELTCOSMS
00217         ' TO TABULAR: '.                                          ELTCOSMS
00218      10  WS-NO-TAB-ID              PIC X(6).                      ELTCOSMS
00219      10  FILLER                    PIC X VALUE SPACE.             ELTCOSMS
00220      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTCOSMS
00221      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTCOSMS
00222                                                                   ELTCOSMS
00223    05  WS-PGM-ERROR.                                              ELTCOSMS
00224      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTCOSMS
00225      10  FILLER                    PIC X(35)  VALUE               ELTCOSMS
00226         '***  P R O G R A M   E R R O R  ***'.                    ELTCOSMS
00227      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTCOSMS
00228                                                                   ELTCOSMS
00229    05  WS-BAD-INST-PROF-SEL.                                      ELTCOSMS
00230      10  FILLER                    PIC XX VALUE SPACE.            ELTCOSMS
00231      10  FILLER                    PIC X(47) VALUE                ELTCOSMS
00232         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTCOSMS
00233      10  FILLER                    PIC X(48) VALUE                ELTCOSMS
00234         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTCOSMS
00235      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTCOSMS
00236                                                                   ELTCOSMS
00237    05  WS-BAD-IN-OUT-SEL.                                         ELTCOSMS
00238      10  FILLER                    PIC X(08) VALUE SPACE.         ELTCOSMS
00239      10  FILLER                    PIC X(51) VALUE                ELTCOSMS
00240         '*** I N V A L I D   I N P U T   /   O U T P U T ***'.    ELTCOSMS
00241      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTCOSMS
00242                                                                   ELTCOSMS
00243    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTCOSMS
00244       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTCOSMS
00245                                                                   ELTCOSMS
00246    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTCOSMS
00247    05  WS-TEMP-BASIC-SUPP-LIT      PIC X(15).                     ELTCOSMS
00248                                                                   ELTCOSMS
00249  01  WS-END                            PIC X(16)  VALUE           ELTCOSMS
00250      '*** W/S ENDS ***'.                                          ELTCOSMS
00251 /             L I N K A G E   S E C T I O N                       ELTCOSMS
00252  LINKAGE SECTION.                                                 ELTCOSMS
00253  01  DFHCOMMAREA.                                                 ELTCOSMS
00254      COPY ELSCOMMC.                                               ELTCOSMS
00255 /  *** CIA  AREA ***                                              ELTCOSMS
00256      COPY ELSCIA2C.                                               ELTCOSMS
00257 /  *** IO PARM AREA ***                                           ELTCOSMS
00258      COPY ELSIOPMC.                                               ELTCOSMS
00259 /  *** KEY AREA ***                                               ELTCOSMS
00260      COPY ELSKEYSC.                                               ELTCOSMS
00261 /  *** OUTPUT TEXT AREA ***                                       ELTCOSMS
00262      COPY ELSOUTPC.                                               ELTCOSMS
00263 /  *** TOPIC SELECTION AREA ***                                   ELTCOSMS
00264      COPY ELSSSCBC.                                               ELTCOSMS
00265 /  *** CODE MANUAL INTERFACE ***                                  ELTCOSMS
00266      COPY ELSCMIFC.                                               ELTCOSMS
00267 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTCOSMS
00268      COPY ELSCMDSC.                                               ELTCOSMS
00269 /  *** BENEFIT PROVISION TABLE ***                                ELTCOSMS
00270      COPY ELSPRVNC.                                               ELTCOSMS
00271 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTCOSMS
00272      COPY ELSTCWAC.                                               ELTCOSMS
00273 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTCOSMS
00274      COPY ELSPLGSW.                                               ELTCOSMS
00275                                                                   ELTCOSMS
00276 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTCOSMS
00277      COPY ELSPLGTB.                                               ELTCOSMS
00278 /        C O N T R A C T   R E C O R D                            ELTCOSMS
00279  01  CONTRACT-RECORD.                                             ELTCOSMS
00280      COPY GCCONTRC.                                               ELTCOSMS
00281 /                  M A I N L I N E                                ELTCOSMS
00282  PROCEDURE DIVISION.                                              ELTCOSMS
00283                                                                   ELTCOSMS
00284 ******************************************************************ELTCOSMS
00285 *                                                                 ELTCOSMS
00286 *   PERFORM THE MAINLINE OPERATIONS.                              ELTCOSMS
00287 *                                                                 ELTCOSMS
00288 ******************************************************************ELTCOSMS
00289  0000-MAINLINE.                                                   ELTCOSMS
00290                                                                   ELTCOSMS
00291      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTCOSMS
00292         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTCOSMS
00293         EXEC CICS  ABEND ABCODE(CIA-ABCODE)  END-EXEC.            ELTCOSMS
00294                                                                   ELTCOSMS
00295      PERFORM 0999-SET-ADDRESSES.                                  ELTCOSMS
00296                                                                   ELTCOSMS
00297      IF SSB-PROV-CLASS-INST  OR  SSB-PROV-CLASS-BOTH              ELTCOSMS
00298         PERFORM 1000-INSTITUTIONAL-RTNE THRU 1999-EXIT.           ELTCOSMS
00299                                                                   ELTCOSMS
00300      IF SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH              ELTCOSMS
00301         PERFORM 2000-PROFESSIONAL-IP-RTNE THRU 2999-EXIT          ELTCOSMS
00302         PERFORM 4000-PROFESSIONAL-OP-RTNE THRU 4999-EXIT.         ELTCOSMS
00303                                                                   ELTCOSMS
00304                                                                   ELTCOSMS
00305      IF NOT SSB-PROV-CLASS-INST AND  NOT SSB-PROV-CLASS-PROF      ELTCOSMS
00306                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTCOSMS
00307         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTCOSMS
00308         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTCOSMS
00309         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTCOSMS
00310         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTCOSMS
00311         MOVE SPACE  TO  COF-FUNCTION                              ELTCOSMS
00312         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00313               COMMAREA(DFHCOMMAREA)                               ELTCOSMS
00314         END-EXEC.                                                 ELTCOSMS
00315                                                                   ELTCOSMS
00316      MOVE 'E'  TO  COF-FUNCTION.                                  ELTCOSMS
00317      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOSMS
00318                     COF-NBR-DTL-LINES.                            ELTCOSMS
00319      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00320      END-EXEC.                                                    ELTCOSMS
00321                                                                   ELTCOSMS
00322                                                                   ELTCOSMS
00323  0099-RETURN.                                                     ELTCOSMS
00324      EXEC CICS RETURN   END-EXEC.                                 ELTCOSMS
00325                                                                   ELTCOSMS
00326      GOBACK.                                                      ELTCOSMS
00327                                                                   ELTCOSMS
00328  0999-SET-ADDRESSES.                                              ELTCOSMS
00329                                                                   ELTCOSMS
00330      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCOSMS
00331          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCOSMS
00332                                                                   ELTCOSMS
00333      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCOSMS
00334      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00335          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCOSMS
00336                                                                   ELTCOSMS
00337      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCOSMS
00338      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00339          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCOSMS
00340                                                                   ELTCOSMS
00341      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCOSMS
00342      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00343          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTCOSMS
00344                                                                   ELTCOSMS
00345      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCOSMS
00346      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00347          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCOSMS
00348                                                                   ELTCOSMS
00349      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCOSMS
00350      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00351          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTCOSMS
00352                                                                   ELTCOSMS
00353      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTCOSMS
00354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00355          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTCOSMS
00356                                                                   ELTCOSMS
00357      MOVE '0'  TO  WS-CHAR-0.                                     ELTCOSMS
00358                                                                   ELTCOSMS
00359      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTCOSMS
00360                                                                   ELTCOSMS
00361      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTCOSMS
00362              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTCOSMS
00363                                                                   ELTCOSMS
00364      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTCOSMS
00365                                                                   ELTCOSMS
00366      SET CIA-STG-GETMAIN  TO TRUE.                                ELTCOSMS
00367                                                                   ELTCOSMS
00368      EXEC CICS LINK                                               ELTCOSMS
00369                PROGRAM('ELUSTGMG')                                ELTCOSMS
00370                COMMAREA(DFHCOMMAREA)                              ELTCOSMS
00371      END-EXEC.                                                    ELTCOSMS
00372                                                                   ELTCOSMS
00373      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTCOSMS
00374      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00375          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTCOSMS
00376                                                                   ELTCOSMS
00377                                                                   ELTCOSMS
00378 /        I N S T I T U T I O N A L     R T N E                    ELTCOSMS
00379 ***************************************************************** ELTCOSMS
00380 *        I N S T I T U T I O N A L     R T N E                    ELTCOSMS
00381 *                                                                 ELTCOSMS
00382 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCOSMS
00383 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCOSMS
00384 *  2. ACCESS CONTRACT RECORDS AND TRANSLATE THE DESIRED CODE VALUEELTCOSMS
00385 *  TO ENGLISH AND BUILD DISPLAY LINES.                            ELTCOSMS
00386 *                                                                 ELTCOSMS
00387 ***************************************************************** ELTCOSMS
00388  1000-INSTITUTIONAL-RTNE.                                         ELTCOSMS
00389      MOVE '1000'  TO  WS-PARA-ID1.                                ELTCOSMS
00390                                                                   ELTCOSMS
00391      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOSMS
00392      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCOSMS
00393      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOSMS
00394                     COF-NBR-DTL-LINES.                            ELTCOSMS
00395      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00396      END-EXEC.                                                    ELTCOSMS
00397      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCOSMS
00398      MOVE WS-HDR-2-INST  TO  COF-HDR-LINE(2).                     ELTCOSMS
00399                                                                   ELTCOSMS
00400      EXEC CICS LINK PROGRAM  ('ELUOUTPT')                         ELTCOSMS
00401                     COMMAREA (DFHCOMMAREA)                        ELTCOSMS
00402      END-EXEC.                                                    ELTCOSMS
00403                                                                   ELTCOSMS
00404      PERFORM 5100-COSMETIC-SURGERY-PAYMENT THRU 5199-EXIT.        ELTCOSMS
00405                                                                   ELTCOSMS
00406      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
00407      MOVE 'N' TO WS-PRINT-COMMON-LINE                             ELTCOSMS
00408                  WS-BASIC-SUPP.                                   ELTCOSMS
00409      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
00410                                                                   ELTCOSMS
00411 ***  SEE ROOM AND BOARD FOR . . .      ***                        ELTCOSMS
00412                                                                   ELTCOSMS
00413      ADD +2 TO WS-CIA.                                            ELTCOSMS
00414      MOVE WS-SEE-ROOM-AND-BOARD TO  COF-DTL-LINE(WS-CIA).         ELTCOSMS
00415                                                                   ELTCOSMS
00416 ***  SEE OUTPATIENT SURGERY TOPIC . . .***                        ELTCOSMS
00417                                                                   ELTCOSMS
00418      ADD +2 TO WS-CIA.                                            ELTCOSMS
00419      MOVE WS-SEE-OUTPATIENT-SURGERY TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
00420                                                                   ELTCOSMS
00421      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTCOSMS
00422      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00423      END-EXEC.                                                    ELTCOSMS
00424 **---------------------------------------------------------------+ELTCOSMS
00425 **                                                               |ELTCOSMS
00426 **     P A Y M E N T  C O N S I D E R A T I O N  T E X T         |ELTCOSMS
00427      INITIALIZE TCAR-FROM-AREA.                                   ELTCOSMS
00428      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCOSMS
00429             WS-PAY-CONSDR-TEXT2                                   ELTCOSMS
00430                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCOSMS
00431      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOSMS
00432      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCOSMS
00433      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCOSMS
00434                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCOSMS
00435      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOSMS
00436      IF WS-CIA > 17                                               ELTCOSMS
00437         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00438         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00439                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00440         END-EXEC                                                  ELTCOSMS
00441         MOVE +1            TO WS-CIA.                             ELTCOSMS
00442      ADD +1                TO  WS-CIA.                            ELTCOSMS
00443      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
00444      ADD +1                TO  WS-CIA.                            ELTCOSMS
00445      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
00446      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTCOSMS
00447      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCOSMS
00448                       COMMAREA(DFHCOMMAREA)                       ELTCOSMS
00449      END-EXEC.                                                    ELTCOSMS
00450      MOVE +1            TO WS-CIA.                                ELTCOSMS
00451 **                                                               |ELTCOSMS
00452 **---------------------------------------------------------------+ELTCOSMS
00453  1999-EXIT.  EXIT.                                                ELTCOSMS
00454 /        P R O F E S S I O N A L   I P   R T N E                  ELTCOSMS
00455 ***************************************************************** ELTCOSMS
00456 *        P R O F E S S I O N A L   I P   R T N E                  ELTCOSMS
00457 *                                                                 ELTCOSMS
00458 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCOSMS
00459 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCOSMS
00460 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCOSMS
00461 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCOSMS
00462 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCOSMS
00463 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCOSMS
00464 *  MODULE.                                                        ELTCOSMS
00465 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCOSMS
00466 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCOSMS
00467 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCOSMS
00468 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCOSMS
00469 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCOSMS
00470 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCOSMS
00471 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCOSMS
00472 *                                                                 ELTCOSMS
00473 ***************************************************************** ELTCOSMS
00474  2000-PROFESSIONAL-IP-RTNE.                                       ELTCOSMS
00475      MOVE '2000'  TO  WS-PARA-ID1.                                ELTCOSMS
00476                                                                   ELTCOSMS
00477      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOSMS
00478      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCOSMS
00479      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOSMS
00480                     COF-NBR-DTL-LINES.                            ELTCOSMS
00481      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00482      END-EXEC.                                                    ELTCOSMS
00483      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCOSMS
00484      MOVE WS-HDR-2-PROF-IP  TO  COF-HDR-LINE(2).                  ELTCOSMS
00485                                                                   ELTCOSMS
00486      EXEC CICS LINK PROGRAM  ('ELUOUTPT')                         ELTCOSMS
00487                     COMMAREA (DFHCOMMAREA)                        ELTCOSMS
00488      END-EXEC.                                                    ELTCOSMS
00489                                                                   ELTCOSMS
00490      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
00491                                                                   ELTCOSMS
00492      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
00493                                                                   ELTCOSMS
00494      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTCOSMS
00495      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTCOSMS
00496         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCOSMS
00497         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCOSMS
00498                                                                   ELTCOSMS
00499      GO TO 2020-CALL-COVERAGE.                                    ELTCOSMS
00500  2010-MOVE-IN-PROF-IP.                                            ELTCOSMS
00501      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCOSMS
00502      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTCOSMS
00503                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCOSMS
00504      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCOSMS
00505                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCOSMS
00506                                                                   ELTCOSMS
00507  2020-CALL-COVERAGE.                                              ELTCOSMS
00508      MOVE '2020'  TO  WS-PARA-ID1.                                ELTCOSMS
00509      MOVE 'COSMETIC SURGERY IS '  TO  SSB-TOPIC-PHRASE.           ELTCOSMS
00510                                                                   ELTCOSMS
00511      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCOSMS
00512      END-EXEC.                                                    ELTCOSMS
00513                                                                   ELTCOSMS
00514      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTCOSMS
00515      MOVE ' ' TO COF-FUNCTION.                                    ELTCOSMS
00516      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00517      END-EXEC.                                                    ELTCOSMS
00518                                                                   ELTCOSMS
00519      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
00520                                                                   ELTCOSMS
00521      IF PVN-COVG-NONE                                             ELTCOSMS
00522         GO TO 2999-EXIT.                                          ELTCOSMS
00523                                                                   ELTCOSMS
00524      MOVE +1  TO  WS-CIA.                                         ELTCOSMS
00525                                                                   ELTCOSMS
00526      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCOSMS
00527                                                                   ELTCOSMS
00528      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCOSMS
00529            PSP-PROVN-PRICING-METHD,                               ELTCOSMS
00530            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCOSMS
00531            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCOSMS
00532            PSP-TRANSF-OTHER-RESP-IND,                             ELTCOSMS
00533            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCOSMS
00534            PSP-COSM-SURG-PAYMT-IND                                ELTCOSMS
00535            PSP-SPILL-OVER-DED-APL-IND,                            ELTCOSMS
00536            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCOSMS
00537            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCOSMS
00538            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCOSMS
00539            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCOSMS
00540            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCOSMS
00541            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCOSMS
00542            PSC-BEN-SCOPE-ID.                                      ELTCOSMS
00543                                                                   ELTCOSMS
00544      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCOSMS
00545      END-EXEC.                                                    ELTCOSMS
00546                                                                   ELTCOSMS
00547      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCOSMS
00548      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
00549          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCOSMS
00550                                                                   ELTCOSMS
00551      PERFORM 2030-FIND-FIRST-NONZERO                              ELTCOSMS
00552         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCOSMS
00553         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCOSMS
00554                                                                   ELTCOSMS
00555      GO TO 2999-EXIT.                                             ELTCOSMS
00556  2030-FIND-FIRST-NONZERO.                                         ELTCOSMS
00557      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOSMS
00558      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCOSMS
00559         NEXT SENTENCE                                             ELTCOSMS
00560      ELSE                                                         ELTCOSMS
00561         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTCOSMS
00562                                                                   ELTCOSMS
00563  2040-BUILD-SCREEN-LINES.                                         ELTCOSMS
00564      MOVE '2040'  TO  WS-PARA-ID1.                                ELTCOSMS
00565                                                                   ELTCOSMS
00566      SET PLT-INDEX1   TO                                          ELTCOSMS
00567                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTCOSMS
00568      IF WS-NOT-FIRST-TIME                                         ELTCOSMS
00569         MOVE 'P'  TO  COF-FUNCTION                                ELTCOSMS
00570         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00571             COMMAREA(DFHCOMMAREA)                                 ELTCOSMS
00572         END-EXEC                                                  ELTCOSMS
00573      ELSE                                                         ELTCOSMS
00574         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCOSMS
00575                                                                   ELTCOSMS
00576      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
00577      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
00578                                                                   ELTCOSMS
00579      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
00580         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTCOSMS
00581            SET PLT-INDEX2  TO  2                                  ELTCOSMS
00582         ELSE                                                      ELTCOSMS
00583            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTCOSMS
00584            GO TO 2999-EXIT                                        ELTCOSMS
00585      ELSE                                                         ELTCOSMS
00586         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
00587                                                                   ELTCOSMS
00588 **---------------------------------------------------------------+ELTCOSMS
00589 **                                                               |ELTCOSMS
00590 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCOSMS
00591      ADD  +2  TO  WS-CIA.                                         ELTCOSMS
00592                                                                   ELTCOSMS
00593      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCOSMS
00594                                                                   ELTCOSMS
00595      MOVE ZERO  TO  WS-SUB2.                                      ELTCOSMS
00596      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCOSMS
00597      MOVE '2050'  TO  WS-PARA-ID1.                                ELTCOSMS
00598      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTCOSMS
00599         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTCOSMS
00600         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTCOSMS
00601      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOSMS
00602                                                                   ELTCOSMS
00603      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCOSMS
00604      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
00605      END-EXEC.                                                    ELTCOSMS
00606      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
00607      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
00608                                                                   ELTCOSMS
00609 **                                                               |ELTCOSMS
00610 **---------------------------------------------------------------+ELTCOSMS
00611                                                                   ELTCOSMS
00612 **---------------------------------------------------------------+ELTCOSMS
00613 **                                                               |ELTCOSMS
00614 **        P L A C E   O F   T R E A T M E N T                    |ELTCOSMS
00615 **                                                               |ELTCOSMS
00616                                                                   ELTCOSMS
00617      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00618      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00619            IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00620                                           NOT =  ZERO             ELTCOSMS
00621               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00622               MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)   ELTCOSMS
00623               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
00624                                                                   ELTCOSMS
00625      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00626      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00627            IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00628                                           NOT =  ZERO             ELTCOSMS
00629             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
00630               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00631               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
00632                                                                   ELTCOSMS
00633      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00634      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00635         IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
00636                                           NOT =  ZERO             ELTCOSMS
00637         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00638         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
00639         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
00640                                               TO  CMF-CODE-VALUE  ELTCOSMS
00641         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
00642         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00643         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00644         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00645         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00646                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00647         END-EXEC                                                  ELTCOSMS
00648         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00649         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00650                     WS-BASIC-SUPP                                 ELTCOSMS
00651         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00652                                                                   ELTCOSMS
00653                                                                   ELTCOSMS
00654      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00655         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00656         IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
00657                                           NOT =  ZERO             ELTCOSMS
00658         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00659         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
00660         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
00661                                               TO  CMF-CODE-VALUE  ELTCOSMS
00662         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
00663         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00664         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00665         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00666         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00667                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00668         END-EXEC                                                  ELTCOSMS
00669         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00670         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00671                     WS-BASIC-SUPP                                 ELTCOSMS
00672         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00673                                                                   ELTCOSMS
00674                                                                   ELTCOSMS
00675 **                                                               |ELTCOSMS
00676 **---------------------------------------------------------------+ELTCOSMS
00677                                                                   ELTCOSMS
00678 **---------------------------------------------------------------+ELTCOSMS
00679 **                                                               |ELTCOSMS
00680 **            B E N E F I T   S C O P E   I D                    |ELTCOSMS
00681 **                                                               |ELTCOSMS
00682      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00683      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00684            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
00685                                         '0000' AND  NOT =  '00  ' ELTCOSMS
00686               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00687               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTCOSMS
00688               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
00689                                                                   ELTCOSMS
00690      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00691      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00692            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
00693                                         '0000' AND  NOT =  '00  ' ELTCOSMS
00694             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
00695               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00696               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
00697                                                                   ELTCOSMS
00698      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00699      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00700            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
00701                                         '0000' AND  NOT =  '00  ' ELTCOSMS
00702               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTCOSMS
00703               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOSMS
00704               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOSMS
00705                                                    CMF-CODE-VALUE ELTCOSMS
00706               MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT       ELTCOSMS
00707               MOVE 'Y' TO WS-BASIC-SUPP                           ELTCOSMS
00708               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTCOSMS
00709               MOVE WS-CIA TO  COF-NBR-DTL-LINES                   ELTCOSMS
00710               EXEC CICS  LINK  PROGRAM('ELUOUTPT')                ELTCOSMS
00711                                COMMAREA(DFHCOMMAREA)              ELTCOSMS
00712               END-EXEC                                            ELTCOSMS
00713               MOVE ZERO TO WS-CIA                                 ELTCOSMS
00714               MOVE 'N' TO WS-PRINT-COMMON-LINE                    ELTCOSMS
00715                     WS-BASIC-SUPP                                 ELTCOSMS
00716               MOVE SPACE      TO COF-FUNCTION.                    ELTCOSMS
00717                                                                   ELTCOSMS
00718                                                                   ELTCOSMS
00719      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00720         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00721            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
00722                                         '0000' AND  NOT =  '00  ' ELTCOSMS
00723               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTCOSMS
00724               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOSMS
00725               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOSMS
00726                                                    CMF-CODE-VALUE ELTCOSMS
00727               MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT        ELTCOSMS
00728               MOVE 'Y' TO WS-BASIC-SUPP                           ELTCOSMS
00729               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTCOSMS
00730               MOVE WS-CIA TO  COF-NBR-DTL-LINES                   ELTCOSMS
00731               EXEC CICS  LINK  PROGRAM('ELUOUTPT')                ELTCOSMS
00732                                COMMAREA(DFHCOMMAREA)              ELTCOSMS
00733               END-EXEC                                            ELTCOSMS
00734               MOVE ZERO TO WS-CIA                                 ELTCOSMS
00735               MOVE 'N' TO WS-PRINT-COMMON-LINE                    ELTCOSMS
00736                     WS-BASIC-SUPP                                 ELTCOSMS
00737               MOVE SPACE      TO COF-FUNCTION.                    ELTCOSMS
00738                                                                   ELTCOSMS
00739 **                                                               |ELTCOSMS
00740 **---------------------------------------------------------------+ELTCOSMS
00741                                                                   ELTCOSMS
00742      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
00743         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00744      ELSE                                                         ELTCOSMS
00745         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
00746                                                                   ELTCOSMS
00747 **---------------------------------------------------------------+ELTCOSMS
00748 **                                                               |ELTCOSMS
00749 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCOSMS
00750 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCOSMS
00751 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTCOSMS
00752      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00753      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
00754         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
00755                                                              '19' ELTCOSMS
00756         ADD +2 TO WS-CIA                                          ELTCOSMS
00757         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOSMS
00758         MOVE 'Y'  TO  WS-PRINT-COMMON-LINE.                       ELTCOSMS
00759                                                                   ELTCOSMS
00760      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00761      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
00762         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
00763                                                        '19'       ELTCOSMS
00764       IF NOT PRINT-COMMON-LINE                                    ELTCOSMS
00765         ADD +2 TO WS-CIA                                          ELTCOSMS
00766         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTCOSMS
00767                                                                   ELTCOSMS
00768      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00769      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
00770         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOSMS
00771                            AND                                    ELTCOSMS
00772         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00773         SET  PLT-INDEX2  TO  2                                    ELTCOSMS
00774         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOSMS
00775                                                             ZERO  ELTCOSMS
00776            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP          ELTCOSMS
00777            ADD +1 TO WS-CIA                                       ELTCOSMS
00778            MOVE WS-BASIC-SUPP-LINE   TO  COF-DTL-LINE(WS-CIA).    ELTCOSMS
00779                                                                   ELTCOSMS
00780      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00781      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
00782         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOSMS
00783                            AND                                    ELTCOSMS
00784         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTCOSMS
00785         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP             ELTCOSMS
00786         ADD +1 TO WS-CIA                                          ELTCOSMS
00787         MOVE WS-BASIC-SUPP-LINE  TO  COF-DTL-LINE(WS-CIA).        ELTCOSMS
00788                                                                   ELTCOSMS
00789      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTCOSMS
00790         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00791         SET  PLT-INDEX2  TO  2                                    ELTCOSMS
00792         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOSMS
00793                                                             ZERO  ELTCOSMS
00794            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP          ELTCOSMS
00795            ADD +1  TO  WS-CIA                                     ELTCOSMS
00796            MOVE WS-BASIC-SUPP-LINE  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
00797                                                                   ELTCOSMS
00798      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00799      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00800         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00801                                                            =  ZEROELTCOSMS
00802            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00803                                                            =  ZEROELTCOSMS
00804               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCOSMS
00805            ELSE                                                   ELTCOSMS
00806               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOSMS
00807          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00808                                                  TO  WS-PERCENTAGEELTCOSMS
00809         ELSE                                                      ELTCOSMS
00810            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOSMS
00811          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00812                                                 TO  WS-PERCENTAGE.ELTCOSMS
00813                                                                   ELTCOSMS
00814      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
00815         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
00816                                             ZERO AND  NOT =  '19' ELTCOSMS
00817         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00818         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
00819         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOSMS
00820                                                    CMF-CODE-VALUE ELTCOSMS
00821         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
00822         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00823         PERFORM 2200-CODES-MANUAL-WITH-PERCENT                    ELTCOSMS
00824         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00825         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00826                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00827         END-EXEC                                                  ELTCOSMS
00828         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00829         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00830                     WS-BASIC-SUPP                                 ELTCOSMS
00831         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00832                                                                   ELTCOSMS
00833                                                                   ELTCOSMS
00834      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00835      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00836         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCOSMS
00837                                                               ZEROELTCOSMS
00838            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00839                                                            =  ZEROELTCOSMS
00840               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCOSMS
00841            ELSE                                                   ELTCOSMS
00842               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOSMS
00843          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00844                                                  TO  WS-PERCENTAGEELTCOSMS
00845         ELSE                                                      ELTCOSMS
00846            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOSMS
00847          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
00848                                                 TO  WS-PERCENTAGE.ELTCOSMS
00849                                                                   ELTCOSMS
00850      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
00851         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
00852                                             ZERO AND  NOT =  '19' ELTCOSMS
00853         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00854         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
00855         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOSMS
00856                                                    CMF-CODE-VALUE ELTCOSMS
00857         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
00858         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00859         PERFORM 2200-CODES-MANUAL-WITH-PERCENT                    ELTCOSMS
00860         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00861         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00862                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00863         END-EXEC                                                  ELTCOSMS
00864         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00865         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00866                     WS-BASIC-SUPP                                 ELTCOSMS
00867         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00868                                                                   ELTCOSMS
00869 **                                                               |ELTCOSMS
00870 **---------------------------------------------------------------+ELTCOSMS
00871                                                                   ELTCOSMS
00872      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
00873         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00874      ELSE                                                         ELTCOSMS
00875         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
00876                                                                   ELTCOSMS
00877                                                                   ELTCOSMS
00878 **---------------------------------------------------------------+ELTCOSMS
00879 **                                                               |ELTCOSMS
00880 **        P A Y M E N T   I N D I C A T O R                      |ELTCOSMS
00881 **                                                               |ELTCOSMS
00882                                                                   ELTCOSMS
00883      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00884      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00885            IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00886                                           NOT =  ZERO             ELTCOSMS
00887               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00888               MOVE WS-CRITERIA-FOR      TO COF-DTL-LINE(WS-CIA)   ELTCOSMS
00889               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
00890                                                                   ELTCOSMS
00891      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00892      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00893            IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00894                                           NOT =  ZERO             ELTCOSMS
00895             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
00896               ADD  +2  TO  WS-CIA                                 ELTCOSMS
00897               MOVE WS-CRITERIA-FOR  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
00898                                                                   ELTCOSMS
00899      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
00900      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
00901         IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
00902                                           NOT =  ZERO             ELTCOSMS
00903         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00904         MOVE 'COSM-SURG-PAYMT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
00905         MOVE PLP-COSM-SURG-PAYMT-IND (PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
00906                                               TO  CMF-CODE-VALUE  ELTCOSMS
00907         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
00908         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00909         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00910         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00911         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00912                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00913         END-EXEC                                                  ELTCOSMS
00914         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00915         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00916                     WS-BASIC-SUPP                                 ELTCOSMS
00917         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00918                                                                   ELTCOSMS
00919                                                                   ELTCOSMS
00920      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
00921         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00922         IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
00923                                           NOT =  ZERO             ELTCOSMS
00924         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00925         MOVE 'COSM-SURG-PAYMT-IND'   TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
00926         MOVE PLP-COSM-SURG-PAYMT-IND (PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
00927                                               TO  CMF-CODE-VALUE  ELTCOSMS
00928         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
00929         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
00930         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00931         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00932         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00933                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00934         END-EXEC                                                  ELTCOSMS
00935         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00936         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00937                     WS-BASIC-SUPP                                 ELTCOSMS
00938         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00939                                                                   ELTCOSMS
00940                                                                   ELTCOSMS
00941 **                                                               |ELTCOSMS
00942 **---------------------------------------------------------------+ELTCOSMS
00943                                                                   ELTCOSMS
00944                                                                   ELTCOSMS
00945      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
00946         SET PLT-INDEX2  TO  2                                     ELTCOSMS
00947      ELSE                                                         ELTCOSMS
00948         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
00949                                                                   ELTCOSMS
00950 **---------------------------------------------------------------+ELTCOSMS
00951 **                                                               |ELTCOSMS
00952 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTCOSMS
00953      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00954      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
00955         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
00956                                                         NOT =  '0'ELTCOSMS
00957         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00958         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTCOSMS
00959                                           CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
00960         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTCOSMS
00961                                                TO  CMF-CODE-VALUE ELTCOSMS
00962         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
00963         ADD +1 TO WS-CIA                                          ELTCOSMS
00964         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00965         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00966         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00967                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00968         END-EXEC                                                  ELTCOSMS
00969         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00970         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00971                     WS-BASIC-SUPP                                 ELTCOSMS
00972         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00973 **                                                               |ELTCOSMS
00974 **---------------------------------------------------------------+ELTCOSMS
00975                                                                   ELTCOSMS
00976 **---------------------------------------------------------------+ELTCOSMS
00977 **                                                               |ELTCOSMS
00978 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTCOSMS
00979      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
00980      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
00981         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCOSMS
00982                                                         NOT =  '0'ELTCOSMS
00983         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
00984         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTCOSMS
00985                                           CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
00986         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
00987                                                 TO  CMF-CODE-VALUEELTCOSMS
00988         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
00989         ADD +1 TO WS-CIA                                          ELTCOSMS
00990         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
00991         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
00992         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
00993                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
00994         END-EXEC                                                  ELTCOSMS
00995         MOVE ZERO TO WS-CIA                                       ELTCOSMS
00996         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
00997                     WS-BASIC-SUPP                                 ELTCOSMS
00998         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
00999 **                                                               |ELTCOSMS
01000 **---------------------------------------------------------------+ELTCOSMS
01001                                                                   ELTCOSMS
01002 **---------------------------------------------------------------+ELTCOSMS
01003 **                                                               |ELTCOSMS
01004 **         TRANSFER TO OTHER RESPONSIBILITY INDICATOR            |ELTCOSMS
01005 **                                                               |ELTCOSMS
01006                                                                   ELTCOSMS
01007      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
01008         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01009      ELSE                                                         ELTCOSMS
01010         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
01011                                                                   ELTCOSMS
01012      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTCOSMS
01013              NOT EQUAL ZEROS                                      ELTCOSMS
01014         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01015         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
01016         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
01017                                                TO  CMF-CODE-VALUE ELTCOSMS
01018         MOVE SPACES        TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
01019         ADD +1 TO WS-CIA                                          ELTCOSMS
01020         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
01021         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01022         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01023                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01024         END-EXEC                                                  ELTCOSMS
01025         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01026         MOVE 'Y' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01027         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01028 **                                                               |ELTCOSMS
01029 **---------------------------------------------------------------+ELTCOSMS
01030                                                                   ELTCOSMS
01031      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
01032         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01033      ELSE                                                         ELTCOSMS
01034         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
01035                                                                   ELTCOSMS
01036 **---------------------------------------------------------------+ELTCOSMS
01037 **                                                               |ELTCOSMS
01038 **                  # A A R   T A B U L A R   F O U N D          |ELTCOSMS
01039      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01040      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01041         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01042                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01043         MOVE 'Y' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01044         MOVE +2  TO  WS-CIA                                       ELTCOSMS
01045         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(WS-CIA)        ELTCOSMS
01046      ELSE                                                         ELTCOSMS
01047         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01048         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01049            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01050                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01051            MOVE 'Y'  TO  WS-PRINT-COMMON-LINE                     ELTCOSMS
01052            MOVE +2  TO  WS-CIA                                    ELTCOSMS
01053            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(WS-CIA).    ELTCOSMS
01054                                                                   ELTCOSMS
01055      IF PRINT-COMMON-LINE                                         ELTCOSMS
01056         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01057         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01058                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01059         END-EXEC                                                  ELTCOSMS
01060         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01061         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01062         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01063 **                                                               |ELTCOSMS
01064 **---------------------------------------------------------------+ELTCOSMS
01065                                                                   ELTCOSMS
01066 **---------------------------------------------------------------+ELTCOSMS
01067 **                                                               |ELTCOSMS
01068 **                  # P P F   T A B U L A R                      |ELTCOSMS
01069      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01070      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01071         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01072                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01073         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
01074                                                 KWA-GCTABULR-KEY  ELTCOSMS
01075         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
01076         IF IOP-RC-OK                                              ELTCOSMS
01077            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTCOSMS
01078                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
01079            END-EXEC                                               ELTCOSMS
01080         ELSE                                                      ELTCOSMS
01081            NEXT SENTENCE                                          ELTCOSMS
01082      ELSE                                                         ELTCOSMS
01083         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01084         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01085            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01086                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01087          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
01088                                                  KWA-GCTABULR-KEY ELTCOSMS
01089            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
01090            IF IOP-RC-OK                                           ELTCOSMS
01091               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTCOSMS
01092                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
01093               END-EXEC.                                           ELTCOSMS
01094 **                                                               |ELTCOSMS
01095 **---------------------------------------------------------------+ELTCOSMS
01096                                                                   ELTCOSMS
01097 **---------------------------------------------------------------+ELTCOSMS
01098 **                                                               |ELTCOSMS
01099 **                  # P V E   T A B U L A R                      |ELTCOSMS
01100      MOVE 'Y'  TO  WS-PRINT-COMMON-LINE.                          ELTCOSMS
01101      MOVE +2  TO  WS-CIA.                                         ELTCOSMS
01102      MOVE WS-PVE               TO  COF-DTL-LINE(WS-CIA).          ELTCOSMS
01103      ADD +1   TO  WS-CIA.                                         ELTCOSMS
01104                                                                   ELTCOSMS
01105      IF PRINT-COMMON-LINE                                         ELTCOSMS
01106         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01107         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01108                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01109         END-EXEC                                                  ELTCOSMS
01110         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01111         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01112         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01113 **---------------------------------------------------------------+ELTCOSMS
01114                                                                   ELTCOSMS
01115 **---------------------------------------------------------------+ELTCOSMS
01116 **                                                               |ELTCOSMS
01117 **                  # A B M   T A B U L A R                      |ELTCOSMS
01118      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01119      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01120         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01121                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01122         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
01123                                                  KWA-GCTABULR-KEY ELTCOSMS
01124         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
01125         IF IOP-RC-OK                                              ELTCOSMS
01126            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTCOSMS
01127                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
01128            END-EXEC                                               ELTCOSMS
01129         ELSE                                                      ELTCOSMS
01130            NEXT SENTENCE                                          ELTCOSMS
01131      ELSE                                                         ELTCOSMS
01132         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01133         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01134            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01135                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01136          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
01137                                                  KWA-GCTABULR-KEY ELTCOSMS
01138            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
01139            IF IOP-RC-OK                                           ELTCOSMS
01140               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTCOSMS
01141                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
01142               END-EXEC.                                           ELTCOSMS
01143 **                                                               |ELTCOSMS
01144 **---------------------------------------------------------------+ELTCOSMS
01145                                                                   ELTCOSMS
01146 **---------------------------------------------------------------+ELTCOSMS
01147 **                                                               |ELTCOSMS
01148 **                  # A C L   T A B U L A R                      |ELTCOSMS
01149      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01150      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01151         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01152                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01153         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
01154                                                 KWA-GCTABULR-KEY  ELTCOSMS
01155         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
01156         IF IOP-RC-OK                                              ELTCOSMS
01157            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTCOSMS
01158                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
01159            END-EXEC                                               ELTCOSMS
01160         ELSE                                                      ELTCOSMS
01161            NEXT SENTENCE                                          ELTCOSMS
01162      ELSE                                                         ELTCOSMS
01163         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01164         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01165            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01166                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01167          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
01168                                                KWA-GCTABULR-KEY   ELTCOSMS
01169            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
01170            IF IOP-RC-OK                                           ELTCOSMS
01171               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTCOSMS
01172                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
01173               END-EXEC.                                           ELTCOSMS
01174 **                                                               |ELTCOSMS
01175 **---------------------------------------------------------------+ELTCOSMS
01176                                                                   ELTCOSMS
01177 **---------------------------------------------------------------+ELTCOSMS
01178 **                                                               |ELTCOSMS
01179 **                  # A D L   T A B U L A R                      |ELTCOSMS
01180      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01181      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01182         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01183                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01184         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
01185                                                 KWA-GCTABULR-KEY  ELTCOSMS
01186         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
01187         IF IOP-RC-OK                                              ELTCOSMS
01188            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTCOSMS
01189                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
01190            END-EXEC                                               ELTCOSMS
01191         ELSE                                                      ELTCOSMS
01192            NEXT SENTENCE                                          ELTCOSMS
01193      ELSE                                                         ELTCOSMS
01194         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01195         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01196            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01197                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01198          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
01199                                               KWA-GCTABULR-KEY    ELTCOSMS
01200            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
01201            IF IOP-RC-OK                                           ELTCOSMS
01202               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTCOSMS
01203                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
01204               END-EXEC.                                           ELTCOSMS
01205 **                                                               |ELTCOSMS
01206 **---------------------------------------------------------------+ELTCOSMS
01207                                                                   ELTCOSMS
01208 **---------------------------------------------------------------+ELTCOSMS
01209 **                                                               |ELTCOSMS
01210 **                  # A O L   T A B U L A R                      |ELTCOSMS
01211      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
01212      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01213         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
01214                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01215         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
01216                                                   KWA-GCTABULR-KEYELTCOSMS
01217         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
01218         IF IOP-RC-OK                                              ELTCOSMS
01219            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTCOSMS
01220                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
01221            END-EXEC                                               ELTCOSMS
01222         ELSE                                                      ELTCOSMS
01223            NEXT SENTENCE                                          ELTCOSMS
01224      ELSE                                                         ELTCOSMS
01225         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01226         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
01227            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
01228                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
01229          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
01230                                                 KWA-GCTABULR-KEY  ELTCOSMS
01231            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
01232            IF IOP-RC-OK                                           ELTCOSMS
01233               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTCOSMS
01234                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
01235               END-EXEC.                                           ELTCOSMS
01236 **                                                               |ELTCOSMS
01237 **---------------------------------------------------------------+ELTCOSMS
01238                                                                   ELTCOSMS
01239 **---------------------------------------------------------------+ELTCOSMS
01240 **                                                               |ELTCOSMS
01241 **     P A Y M E N T  C O N S I D E R A T I O N  T E X T         |ELTCOSMS
01242      INITIALIZE TCAR-FROM-AREA.                                   ELTCOSMS
01243      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCOSMS
01244             WS-PAY-CONSDR-TEXT2                                   ELTCOSMS
01245                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCOSMS
01246      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOSMS
01247      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCOSMS
01248      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCOSMS
01249                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCOSMS
01250      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOSMS
01251      IF WS-CIA > 17                                               ELTCOSMS
01252         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01253         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01254                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01255         END-EXEC                                                  ELTCOSMS
01256         MOVE +1            TO WS-CIA.                             ELTCOSMS
01257      ADD +1                TO  WS-CIA.                            ELTCOSMS
01258      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
01259      ADD +1                TO  WS-CIA.                            ELTCOSMS
01260      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
01261      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTCOSMS
01262      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCOSMS
01263                       COMMAREA(DFHCOMMAREA)                       ELTCOSMS
01264      END-EXEC.                                                    ELTCOSMS
01265      MOVE +1            TO WS-CIA.                                ELTCOSMS
01266 **                                                               |ELTCOSMS
01267 **---------------------------------------------------------------+ELTCOSMS
01268                                                                   ELTCOSMS
01269  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTCOSMS
01270                                                                   ELTCOSMS
01271      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCOSMS
01272         ADD +1   TO WS-CIA                                        ELTCOSMS
01273         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01274         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01275         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCOSMS
01276         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCOSMS
01277                                                    CMF-CODE-VALUE ELTCOSMS
01278         PERFORM 2400-CODES-MANUAL-CALL                            ELTCOSMS
01279         STRING CMF-DESCR-LINE (1) ' '                             ELTCOSMS
01280                CMF-DESCR-LINE (2)                                 ELTCOSMS
01281                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCOSMS
01282         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCOSMS
01283         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCOSMS
01284         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCOSMS
01285         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCOSMS
01286         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCOSMS
01287         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTCOSMS
01288         MOVE WS-SERVICES-2ND  TO COF-DTL-LINE(WS-CIA)             ELTCOSMS
01289         IF WS-CIA < 20                                            ELTCOSMS
01290            ADD +1 TO WS-CIA                                       ELTCOSMS
01291            MOVE ZERO TO                                           ELTCOSMS
01292                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         ELTCOSMS
01293         ELSE                                                      ELTCOSMS
01294            EXEC CICS LINK PROGRAM('ELUOUTPT')                     ELTCOSMS
01295            END-EXEC                                               ELTCOSMS
01296            MOVE +1 TO WS-CIA                                      ELTCOSMS
01297            MOVE ZERO TO                                           ELTCOSMS
01298                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTCOSMS
01299                                                                   ELTCOSMS
01300  2090-PROBLEM-WITH-INDICES.                                       ELTCOSMS
01301                                                                   ELTCOSMS
01302      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCOSMS
01303      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOSMS
01304                                                                   ELTCOSMS
01305      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCOSMS
01306      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOSMS
01307                                                                   ELTCOSMS
01308      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
01309      END-EXEC.                                                    ELTCOSMS
01310                                                                   ELTCOSMS
01311  2999-EXIT.            EXIT.                                      ELTCOSMS
01312                                                                   ELTCOSMS
01313 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTCOSMS
01314  2100-CALL-CODES-MANUAL-LONG.                                     ELTCOSMS
01315      MOVE '2100'  TO  WS-PARA-ID2.                                ELTCOSMS
01316                                                                   ELTCOSMS
01317      INITIALIZE CMF-RETURN-CODE,                                  ELTCOSMS
01318                 TCAR-FROM-AREA.                                   ELTCOSMS
01319                                                                   ELTCOSMS
01320      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTCOSMS
01321      END-EXEC.                                                    ELTCOSMS
01322                                                                   ELTCOSMS
01323      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOSMS
01324      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
01325          ADDRESS OF CMF-DESCR.                                    ELTCOSMS
01326                                                                   ELTCOSMS
01327      IF BASIC-SUPP-LINE                                           ELTCOSMS
01328          MOVE SPACES TO TCAR-FROM-AREA                            ELTCOSMS
01329          STRING  CMF-DESCR-LINE (1) ' '                           ELTCOSMS
01330                  DELIMITED BY SIZE                                ELTCOSMS
01331          INTO TCAR-FROM-AREA                                      ELTCOSMS
01332          PERFORM 5020-MOVE-LINES-OUT THRU 5020-EXIT               ELTCOSMS
01333              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01334              UNTIL WS-SUB5 GREATER THAN CMF-NBR-DESCR-LINES       ELTCOSMS
01335                                                                   ELTCOSMS
01336          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCOSMS
01337          MOVE TCAR-TO-SUB  TO  TCAR-L                             ELTCOSMS
01338          MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT                      ELTCOSMS
01339          MOVE +50 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTCOSMS
01340                      TCAR-OUTPUT-FIELD-2-LEN                      ELTCOSMS
01341                      TCAR-OUTPUT-FIELD-3-LEN                      ELTCOSMS
01342                      TCAR-OUTPUT-FIELD-4-LEN                      ELTCOSMS
01343                      TCAR-OUTPUT-FIELD-5-LEN                      ELTCOSMS
01344                      TCAR-OUTPUT-FIELD-6-LEN                      ELTCOSMS
01345                      TCAR-OUTPUT-FIELD-7-LEN                      ELTCOSMS
01346                      TCAR-OUTPUT-FIELD-8-LEN                      ELTCOSMS
01347                      TCAR-OUTPUT-FIELD-9-LEN                      ELTCOSMS
01348                      TCAR-OUTPUT-FIELD-10-LEN                     ELTCOSMS
01349                      TCAR-OUTPUT-FIELD-11-LEN                     ELTCOSMS
01350                      TCAR-OUTPUT-FIELD-12-LEN                     ELTCOSMS
01351                      TCAR-OUTPUT-FIELD-13-LEN                     ELTCOSMS
01352                      TCAR-OUTPUT-FIELD-14-LEN                     ELTCOSMS
01353                      TCAR-OUTPUT-FIELD-15-LEN                     ELTCOSMS
01354                      TCAR-OUTPUT-FIELD-16-LEN                     ELTCOSMS
01355                      TCAR-OUTPUT-FIELD-17-LEN                     ELTCOSMS
01356                      TCAR-OUTPUT-FIELD-18-LEN                     ELTCOSMS
01357                      TCAR-OUTPUT-FIELD-19-LEN                     ELTCOSMS
01358                      TCAR-OUTPUT-FIELD-20-LEN                     ELTCOSMS
01359          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCOSMS
01360          ADD +1 TO WS-CIA                                         ELTCOSMS
01361          STRING WS-TEMP-BASIC-SUPP-LIT                            ELTCOSMS
01362                 TCAR-OPF-DATA (1)                                 ELTCOSMS
01363          DELIMITED BY SIZE  INTO  COF-DTL-LINE (WS-CIA)           ELTCOSMS
01364          PERFORM 5010-MOVE-LINES-OUT THRU 5010-EXIT               ELTCOSMS
01365              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01366              UNTIL WS-SUB5 GREATER THAN TCAR-OUTPUT-FIELDS-USED   ELTCOSMS
01367      ELSE                                                         ELTCOSMS
01368          MOVE SPACES TO TCAR-FROM-AREA                            ELTCOSMS
01369          STRING  CMF-DESCR-LINE (1) ' '                           ELTCOSMS
01370                  DELIMITED BY SIZE                                ELTCOSMS
01371          INTO TCAR-FROM-AREA                                      ELTCOSMS
01372          PERFORM 5020-MOVE-LINES-OUT THRU 5020-EXIT               ELTCOSMS
01373              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01374              UNTIL WS-SUB5 GREATER THAN CMF-NBR-DESCR-LINES       ELTCOSMS
01375                                                                   ELTCOSMS
01376          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCOSMS
01377          MOVE TCAR-TO-SUB  TO  TCAR-L                             ELTCOSMS
01378          MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT                      ELTCOSMS
01379          MOVE +50 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTCOSMS
01380                      TCAR-OUTPUT-FIELD-2-LEN                      ELTCOSMS
01381                      TCAR-OUTPUT-FIELD-3-LEN                      ELTCOSMS
01382                      TCAR-OUTPUT-FIELD-4-LEN                      ELTCOSMS
01383                      TCAR-OUTPUT-FIELD-5-LEN                      ELTCOSMS
01384                      TCAR-OUTPUT-FIELD-6-LEN                      ELTCOSMS
01385                      TCAR-OUTPUT-FIELD-7-LEN                      ELTCOSMS
01386                      TCAR-OUTPUT-FIELD-8-LEN                      ELTCOSMS
01387                      TCAR-OUTPUT-FIELD-9-LEN                      ELTCOSMS
01388                      TCAR-OUTPUT-FIELD-10-LEN                     ELTCOSMS
01389                      TCAR-OUTPUT-FIELD-11-LEN                     ELTCOSMS
01390                      TCAR-OUTPUT-FIELD-12-LEN                     ELTCOSMS
01391                      TCAR-OUTPUT-FIELD-13-LEN                     ELTCOSMS
01392                      TCAR-OUTPUT-FIELD-14-LEN                     ELTCOSMS
01393                      TCAR-OUTPUT-FIELD-15-LEN                     ELTCOSMS
01394                      TCAR-OUTPUT-FIELD-16-LEN                     ELTCOSMS
01395                      TCAR-OUTPUT-FIELD-17-LEN                     ELTCOSMS
01396                      TCAR-OUTPUT-FIELD-18-LEN                     ELTCOSMS
01397                      TCAR-OUTPUT-FIELD-19-LEN                     ELTCOSMS
01398                      TCAR-OUTPUT-FIELD-20-LEN                     ELTCOSMS
01399          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCOSMS
01400          PERFORM 5000-MOVE-LINES-OUT THRU 5000-EXIT               ELTCOSMS
01401              VARYING WS-SUB5 FROM 1 BY 1                          ELTCOSMS
01402              UNTIL WS-SUB5 GREATER THAN TCAR-OUTPUT-FIELDS-USED.  ELTCOSMS
01403                                                                   ELTCOSMS
01404                                                                   ELTCOSMS
01405  2199-EXIT.           EXIT.                                       ELTCOSMS
01406 /                                                                 ELTCOSMS
01407  2200-CODES-MANUAL-WITH-PERCENT.                                  ELTCOSMS
01408      MOVE '2200'  TO  WS-PARA-ID2.                                ELTCOSMS
01409                                                                   ELTCOSMS
01410      INITIALIZE CMF-RETURN-CODE,                                  ELTCOSMS
01411                 TCAR-FROM-AREA.                                   ELTCOSMS
01412                                                                   ELTCOSMS
01413      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTCOSMS
01414      END-EXEC.                                                    ELTCOSMS
01415                                                                   ELTCOSMS
01416      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOSMS
01417      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
01418          ADDRESS OF CMF-DESCR.                                    ELTCOSMS
01419                                                                   ELTCOSMS
01420      IF BASIC-SUPP-LINE                                           ELTCOSMS
01421          MOVE SPACES TO TCAR-FROM-AREA                            ELTCOSMS
01422          STRING  CMF-DESCR-LINE (1) ' '                           ELTCOSMS
01423                  DELIMITED BY SIZE                                ELTCOSMS
01424          INTO TCAR-FROM-AREA                                      ELTCOSMS
01425          PERFORM 5020-MOVE-LINES-OUT THRU 5020-EXIT               ELTCOSMS
01426              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01427              UNTIL WS-SUB5 GREATER THAN CMF-NBR-DESCR-LINES       ELTCOSMS
01428          STRING  TCAR-FROM-AREA DELIMITED BY '  '                 ELTCOSMS
01429                  WS-PERCENT-FLD     DELIMITED BY SIZE             ELTCOSMS
01430          INTO TCAR-FROM-AREA                                      ELTCOSMS
01431                                                                   ELTCOSMS
01432          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCOSMS
01433          MOVE TCAR-TO-SUB  TO  TCAR-L                             ELTCOSMS
01434          MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT                      ELTCOSMS
01435          MOVE +50 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTCOSMS
01436                      TCAR-OUTPUT-FIELD-2-LEN                      ELTCOSMS
01437                      TCAR-OUTPUT-FIELD-3-LEN                      ELTCOSMS
01438                      TCAR-OUTPUT-FIELD-4-LEN                      ELTCOSMS
01439                      TCAR-OUTPUT-FIELD-5-LEN                      ELTCOSMS
01440                      TCAR-OUTPUT-FIELD-6-LEN                      ELTCOSMS
01441                      TCAR-OUTPUT-FIELD-7-LEN                      ELTCOSMS
01442                      TCAR-OUTPUT-FIELD-8-LEN                      ELTCOSMS
01443                      TCAR-OUTPUT-FIELD-9-LEN                      ELTCOSMS
01444                      TCAR-OUTPUT-FIELD-10-LEN                     ELTCOSMS
01445                      TCAR-OUTPUT-FIELD-11-LEN                     ELTCOSMS
01446                      TCAR-OUTPUT-FIELD-12-LEN                     ELTCOSMS
01447                      TCAR-OUTPUT-FIELD-13-LEN                     ELTCOSMS
01448                      TCAR-OUTPUT-FIELD-14-LEN                     ELTCOSMS
01449                      TCAR-OUTPUT-FIELD-15-LEN                     ELTCOSMS
01450                      TCAR-OUTPUT-FIELD-16-LEN                     ELTCOSMS
01451                      TCAR-OUTPUT-FIELD-17-LEN                     ELTCOSMS
01452                      TCAR-OUTPUT-FIELD-18-LEN                     ELTCOSMS
01453                      TCAR-OUTPUT-FIELD-19-LEN                     ELTCOSMS
01454                      TCAR-OUTPUT-FIELD-20-LEN                     ELTCOSMS
01455          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCOSMS
01456          ADD +1 TO WS-CIA                                         ELTCOSMS
01457          STRING WS-TEMP-BASIC-SUPP-LIT                            ELTCOSMS
01458                 TCAR-OPF-DATA (1)                                 ELTCOSMS
01459          DELIMITED BY SIZE  INTO  COF-DTL-LINE (WS-CIA)           ELTCOSMS
01460          PERFORM 5010-MOVE-LINES-OUT THRU 5010-EXIT               ELTCOSMS
01461              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01462              UNTIL WS-SUB5 GREATER THAN TCAR-OUTPUT-FIELDS-USED   ELTCOSMS
01463      ELSE                                                         ELTCOSMS
01464          MOVE SPACES TO TCAR-FROM-AREA                            ELTCOSMS
01465          STRING  CMF-DESCR-LINE (1) ' '                           ELTCOSMS
01466                  DELIMITED BY SIZE                                ELTCOSMS
01467          INTO TCAR-FROM-AREA                                      ELTCOSMS
01468          PERFORM 5020-MOVE-LINES-OUT THRU 5020-EXIT               ELTCOSMS
01469              VARYING WS-SUB5 FROM 2 BY 1                          ELTCOSMS
01470              UNTIL WS-SUB5 GREATER THAN CMF-NBR-DESCR-LINES       ELTCOSMS
01471          STRING  TCAR-FROM-AREA DELIMITED BY '  '                 ELTCOSMS
01472                  WS-PERCENT-FLD     DELIMITED BY SIZE             ELTCOSMS
01473          INTO TCAR-FROM-AREA                                      ELTCOSMS
01474                                                                   ELTCOSMS
01475          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCOSMS
01476          MOVE TCAR-TO-SUB  TO  TCAR-L                             ELTCOSMS
01477          MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT                      ELTCOSMS
01478          MOVE +50 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTCOSMS
01479                      TCAR-OUTPUT-FIELD-2-LEN                      ELTCOSMS
01480                      TCAR-OUTPUT-FIELD-3-LEN                      ELTCOSMS
01481                      TCAR-OUTPUT-FIELD-4-LEN                      ELTCOSMS
01482                      TCAR-OUTPUT-FIELD-5-LEN                      ELTCOSMS
01483                      TCAR-OUTPUT-FIELD-6-LEN                      ELTCOSMS
01484                      TCAR-OUTPUT-FIELD-7-LEN                      ELTCOSMS
01485                      TCAR-OUTPUT-FIELD-8-LEN                      ELTCOSMS
01486                      TCAR-OUTPUT-FIELD-9-LEN                      ELTCOSMS
01487                      TCAR-OUTPUT-FIELD-10-LEN                     ELTCOSMS
01488                      TCAR-OUTPUT-FIELD-11-LEN                     ELTCOSMS
01489                      TCAR-OUTPUT-FIELD-12-LEN                     ELTCOSMS
01490                      TCAR-OUTPUT-FIELD-13-LEN                     ELTCOSMS
01491                      TCAR-OUTPUT-FIELD-14-LEN                     ELTCOSMS
01492                      TCAR-OUTPUT-FIELD-15-LEN                     ELTCOSMS
01493                      TCAR-OUTPUT-FIELD-16-LEN                     ELTCOSMS
01494                      TCAR-OUTPUT-FIELD-17-LEN                     ELTCOSMS
01495                      TCAR-OUTPUT-FIELD-18-LEN                     ELTCOSMS
01496                      TCAR-OUTPUT-FIELD-19-LEN                     ELTCOSMS
01497                      TCAR-OUTPUT-FIELD-20-LEN                     ELTCOSMS
01498          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCOSMS
01499          PERFORM 5000-MOVE-LINES-OUT THRU 5000-EXIT               ELTCOSMS
01500              VARYING WS-SUB5 FROM 1 BY 1                          ELTCOSMS
01501              UNTIL WS-SUB5 GREATER THAN TCAR-OUTPUT-FIELDS-USED.  ELTCOSMS
01502                                                                   ELTCOSMS
01503  2299-EXIT.           EXIT.                                       ELTCOSMS
01504                                                                   ELTCOSMS
01505 /            G E T   T A B U L A R   R E C O R D                  ELTCOSMS
01506 ***************************************************************** ELTCOSMS
01507 *            G E T   T A B U L A R   R E C O R D                  ELTCOSMS
01508 *                                                                 ELTCOSMS
01509 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF,           ELTCOSMS
01510 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTCOSMS
01511 *  TO DISPLAY.                                                    ELTCOSMS
01512 *                                                                 ELTCOSMS
01513 ***************************************************************** ELTCOSMS
01514  2300-GET-TABULAR-RECORD.                                         ELTCOSMS
01515      MOVE '2300'  TO  WS-PARA-ID2.                                ELTCOSMS
01516                                                                   ELTCOSMS
01517      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCOSMS
01518      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
01519          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTCOSMS
01520                                                                   ELTCOSMS
01521      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTCOSMS
01522      SET  CIA-GCTABULR-DDN          TO TRUE.                      ELTCOSMS
01523                                                                   ELTCOSMS
01524      SET IOP-RD                     TO TRUE.                      ELTCOSMS
01525      SET IOP-FCQ-NONE               TO TRUE.                      ELTCOSMS
01526      SET IOP-KVQ-NONE               TO TRUE.                      ELTCOSMS
01527                                                                   ELTCOSMS
01528      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTCOSMS
01529             COMMAREA(DFHCOMMAREA)                                 ELTCOSMS
01530      END-EXEC.                                                    ELTCOSMS
01531                                                                   ELTCOSMS
01532      IF IOP-RC-NOTFND                                             ELTCOSMS
01533         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTCOSMS
01534         EXEC CICS ABEND                                           ELTCOSMS
01535                   ABCODE(CIA-ABCODE)                              ELTCOSMS
01536         END-EXEC.                                                 ELTCOSMS
01537                                                                   ELTCOSMS
01538      IF NOT IOP-RC-OK                                             ELTCOSMS
01539         SET CIA-AB-CRITIO          TO TRUE                        ELTCOSMS
01540         EXEC CICS ABEND                                           ELTCOSMS
01541                   ABCODE(CIA-ABCODE)                              ELTCOSMS
01542         END-EXEC.                                                 ELTCOSMS
01543                                                                   ELTCOSMS
01544  2399-EXIT.           EXIT.                                       ELTCOSMS
01545                                                                   ELTCOSMS
01546 /            C O D E  M A N U A L  C A L L                        ELTCOSMS
01547  2400-CODES-MANUAL-CALL.                                          ELTCOSMS
01548      INITIALIZE CMF-RETURN-CODE,                                  ELTCOSMS
01549                 TCAR-FROM-AREA.                                   ELTCOSMS
01550                                                                   ELTCOSMS
01551      EXEC CICS LINK PROGRAM('ELUCMIF')                            ELTCOSMS
01552                     COMMAREA(DFHCOMMAREA)                         ELTCOSMS
01553      END-EXEC.                                                    ELTCOSMS
01554                                                                   ELTCOSMS
01555      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOSMS
01556      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
01557          ADDRESS OF CMF-DESCR.                                    ELTCOSMS
01558                                                                   ELTCOSMS
01559  2400-EXIT.       EXIT.                                           ELTCOSMS
01560 /            P R O F E S S I O N A L   O P   R T N E              ELTCOSMS
01561 ***************************************************************** ELTCOSMS
01562 *            P R O F E S S I O N A L   O P   R T N E              ELTCOSMS
01563 *                                                                 ELTCOSMS
01564 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCOSMS
01565 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCOSMS
01566 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCOSMS
01567 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCOSMS
01568 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCOSMS
01569 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCOSMS
01570 *  MODULE.                                                        ELTCOSMS
01571 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCOSMS
01572 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCOSMS
01573 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCOSMS
01574 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCOSMS
01575 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCOSMS
01576 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCOSMS
01577 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCOSMS
01578 *                                                                 ELTCOSMS
01579 ***************************************************************** ELTCOSMS
01580  4000-PROFESSIONAL-OP-RTNE.                                       ELTCOSMS
01581      MOVE '4000'  TO  WS-PARA-ID1.                                ELTCOSMS
01582                                                                   ELTCOSMS
01583      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOSMS
01584      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCOSMS
01585      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCOSMS
01586                     COF-NBR-DTL-LINES.                            ELTCOSMS
01587      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
01588      END-EXEC.                                                    ELTCOSMS
01589                                                                   ELTCOSMS
01590      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCOSMS
01591      MOVE WS-HDR-2-PROF-OP  TO  COF-HDR-LINE(2).                  ELTCOSMS
01592                                                                   ELTCOSMS
01593      EXEC CICS LINK PROGRAM  ('ELUOUTPT')                         ELTCOSMS
01594                     COMMAREA (DFHCOMMAREA)                        ELTCOSMS
01595      END-EXEC.                                                    ELTCOSMS
01596                                                                   ELTCOSMS
01597      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
01598      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
01599                                                                   ELTCOSMS
01600                                                                   ELTCOSMS
01601      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTCOSMS
01602      PERFORM 4010-MOVE-IN-PROF-OP                                 ELTCOSMS
01603         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCOSMS
01604         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTCOSMS
01605                                                                   ELTCOSMS
01606      GO TO 4020-CALL-COVERAGE.                                    ELTCOSMS
01607  4010-MOVE-IN-PROF-OP.                                            ELTCOSMS
01608      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCOSMS
01609      MOVE WS-PROF-OP-LIST(WS-SUB)  TO                             ELTCOSMS
01610                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCOSMS
01611      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCOSMS
01612                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCOSMS
01613                                                                   ELTCOSMS
01614  4020-CALL-COVERAGE.                                              ELTCOSMS
01615      MOVE '4020'  TO  WS-PARA-ID1.                                ELTCOSMS
01616      MOVE 'COSMETIC SURGERY IS '  TO  SSB-TOPIC-PHRASE.           ELTCOSMS
01617                                                                   ELTCOSMS
01618      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCOSMS
01619      END-EXEC.                                                    ELTCOSMS
01620                                                                   ELTCOSMS
01621      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTCOSMS
01622      MOVE ' ' TO COF-FUNCTION.                                    ELTCOSMS
01623      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
01624      END-EXEC.                                                    ELTCOSMS
01625                                                                   ELTCOSMS
01626      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
01627                                                                   ELTCOSMS
01628      IF PVN-COVG-NONE                                             ELTCOSMS
01629         GO TO 4999-EXIT.                                          ELTCOSMS
01630                                                                   ELTCOSMS
01631      MOVE +1  TO  WS-CIA.                                         ELTCOSMS
01632                                                                   ELTCOSMS
01633      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCOSMS
01634                                                                   ELTCOSMS
01635      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCOSMS
01636            PSP-PROVN-PRICING-METHD,                               ELTCOSMS
01637            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCOSMS
01638            PSP-TRANSF-OTHER-RESP-IND,                             ELTCOSMS
01639            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCOSMS
01640            PSP-COSM-SURG-PAYMT-IND                                ELTCOSMS
01641            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCOSMS
01642            PSP-SPILL-OVER-DED-APL-IND,                            ELTCOSMS
01643            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCOSMS
01644            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCOSMS
01645            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCOSMS
01646            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCOSMS
01647            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCOSMS
01648            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCOSMS
01649            PSC-BEN-SCOPE-ID.                                      ELTCOSMS
01650                                                                   ELTCOSMS
01651      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCOSMS
01652      END-EXEC.                                                    ELTCOSMS
01653                                                                   ELTCOSMS
01654      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCOSMS
01655      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
01656          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCOSMS
01657                                                                   ELTCOSMS
01658      PERFORM 4030-FIND-FIRST-NONZERO                              ELTCOSMS
01659         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCOSMS
01660         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTCOSMS
01661                                                                   ELTCOSMS
01662      GO TO 4999-EXIT.                                             ELTCOSMS
01663  4030-FIND-FIRST-NONZERO.                                         ELTCOSMS
01664      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOSMS
01665      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCOSMS
01666         NEXT SENTENCE                                             ELTCOSMS
01667      ELSE                                                         ELTCOSMS
01668         PERFORM 4040-BUILD-SCREEN-LINES.                          ELTCOSMS
01669                                                                   ELTCOSMS
01670  4040-BUILD-SCREEN-LINES.                                         ELTCOSMS
01671      MOVE '4040'  TO  WS-PARA-ID1.                                ELTCOSMS
01672                                                                   ELTCOSMS
01673      SET PLT-INDEX1   TO                                          ELTCOSMS
01674                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTCOSMS
01675      IF WS-NOT-FIRST-TIME                                         ELTCOSMS
01676         MOVE 'P'  TO  COF-FUNCTION                                ELTCOSMS
01677         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01678             COMMAREA(DFHCOMMAREA)                                 ELTCOSMS
01679         END-EXEC                                                  ELTCOSMS
01680      ELSE                                                         ELTCOSMS
01681         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCOSMS
01682                                                                   ELTCOSMS
01683      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
01684      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
01685                                                                   ELTCOSMS
01686      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
01687         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTCOSMS
01688            SET PLT-INDEX2  TO  2                                  ELTCOSMS
01689         ELSE                                                      ELTCOSMS
01690            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTCOSMS
01691            GO TO 4999-EXIT                                        ELTCOSMS
01692      ELSE                                                         ELTCOSMS
01693         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
01694                                                                   ELTCOSMS
01695 **---------------------------------------------------------------+ELTCOSMS
01696 **                                                               |ELTCOSMS
01697 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCOSMS
01698      ADD  +2  TO  WS-CIA.                                         ELTCOSMS
01699                                                                   ELTCOSMS
01700      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCOSMS
01701                                                                   ELTCOSMS
01702      MOVE ZERO  TO  WS-SUB2.                                      ELTCOSMS
01703      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCOSMS
01704      MOVE '4050'  TO  WS-PARA-ID1.                                ELTCOSMS
01705      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTCOSMS
01706         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTCOSMS
01707         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTCOSMS
01708      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCOSMS
01709                                                                   ELTCOSMS
01710      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCOSMS
01711      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
01712      END-EXEC.                                                    ELTCOSMS
01713      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
01714      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
01715                                                                   ELTCOSMS
01716 **                                                               |ELTCOSMS
01717 **---------------------------------------------------------------+ELTCOSMS
01718                                                                   ELTCOSMS
01719 **---------------------------------------------------------------+ELTCOSMS
01720 **                                                               |ELTCOSMS
01721 **        P L A C E   O F   T R E A T M E N T                    |ELTCOSMS
01722 **                                                               |ELTCOSMS
01723      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01724      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01725            IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
01726                                           NOT =  ZERO             ELTCOSMS
01727               ADD  +2  TO  WS-CIA                                 ELTCOSMS
01728               MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)   ELTCOSMS
01729               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
01730                                                                   ELTCOSMS
01731      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
01732      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01733            IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
01734                                           NOT =  ZERO             ELTCOSMS
01735             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
01736               ADD  +2  TO  WS-CIA                                 ELTCOSMS
01737               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
01738                                                                   ELTCOSMS
01739      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01740      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01741         IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
01742                                           NOT =  ZERO             ELTCOSMS
01743         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01744         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
01745         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
01746                                               TO  CMF-CODE-VALUE  ELTCOSMS
01747         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
01748         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
01749         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
01750         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01751         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01752                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01753         END-EXEC                                                  ELTCOSMS
01754         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01755         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01756                     WS-BASIC-SUPP                                 ELTCOSMS
01757         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01758                                                                   ELTCOSMS
01759                                                                   ELTCOSMS
01760      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01761         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01762         IF PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
01763                                           NOT =  ZERO             ELTCOSMS
01764         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01765         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
01766         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
01767                                               TO  CMF-CODE-VALUE  ELTCOSMS
01768         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
01769         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
01770         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
01771         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01772         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01773                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01774         END-EXEC                                                  ELTCOSMS
01775         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01776         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01777                     WS-BASIC-SUPP                                 ELTCOSMS
01778         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01779                                                                   ELTCOSMS
01780                                                                   ELTCOSMS
01781 **                                                               |ELTCOSMS
01782 **---------------------------------------------------------------+ELTCOSMS
01783                                                                   ELTCOSMS
01784 **---------------------------------------------------------------+ELTCOSMS
01785 **                                                               |ELTCOSMS
01786 **            B E N E F I T   S C O P E   I D                    |ELTCOSMS
01787 **                                                               |ELTCOSMS
01788      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01789      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01790            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
01791                                         '0000' AND  NOT =  '00  ' ELTCOSMS
01792               ADD  +2  TO  WS-CIA                                 ELTCOSMS
01793               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTCOSMS
01794               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
01795                                                                   ELTCOSMS
01796      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
01797      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01798            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
01799                                         '0000' AND  NOT =  '00  ' ELTCOSMS
01800             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
01801               ADD  +2  TO  WS-CIA                                 ELTCOSMS
01802               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
01803                                                                   ELTCOSMS
01804      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01805      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01806            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
01807                                         '0000' AND  NOT =  '00  ' ELTCOSMS
01808               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTCOSMS
01809               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOSMS
01810               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOSMS
01811                                                    CMF-CODE-VALUE ELTCOSMS
01812               MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT       ELTCOSMS
01813               MOVE 'Y' TO WS-BASIC-SUPP                           ELTCOSMS
01814               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTCOSMS
01815               MOVE WS-CIA TO  COF-NBR-DTL-LINES                   ELTCOSMS
01816               EXEC CICS  LINK  PROGRAM('ELUOUTPT')                ELTCOSMS
01817                                COMMAREA(DFHCOMMAREA)              ELTCOSMS
01818               END-EXEC                                            ELTCOSMS
01819               MOVE ZERO TO WS-CIA                                 ELTCOSMS
01820               MOVE 'N' TO WS-PRINT-COMMON-LINE                    ELTCOSMS
01821                     WS-BASIC-SUPP                                 ELTCOSMS
01822               MOVE SPACE      TO COF-FUNCTION.                    ELTCOSMS
01823                                                                   ELTCOSMS
01824                                                                   ELTCOSMS
01825      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01826         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01827            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTCOSMS
01828                                         '0000' AND  NOT =  '00  ' ELTCOSMS
01829               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTCOSMS
01830               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTCOSMS
01831               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTCOSMS
01832                                                    CMF-CODE-VALUE ELTCOSMS
01833               MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT        ELTCOSMS
01834               MOVE 'Y' TO WS-BASIC-SUPP                           ELTCOSMS
01835               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTCOSMS
01836               MOVE WS-CIA TO  COF-NBR-DTL-LINES                   ELTCOSMS
01837               EXEC CICS  LINK  PROGRAM('ELUOUTPT')                ELTCOSMS
01838                                COMMAREA(DFHCOMMAREA)              ELTCOSMS
01839               END-EXEC                                            ELTCOSMS
01840               MOVE ZERO TO WS-CIA                                 ELTCOSMS
01841               MOVE 'N' TO WS-PRINT-COMMON-LINE                    ELTCOSMS
01842                     WS-BASIC-SUPP                                 ELTCOSMS
01843               MOVE SPACE      TO COF-FUNCTION.                    ELTCOSMS
01844                                                                   ELTCOSMS
01845 **                                                               |ELTCOSMS
01846 **---------------------------------------------------------------+ELTCOSMS
01847                                                                   ELTCOSMS
01848      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
01849         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01850      ELSE                                                         ELTCOSMS
01851         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
01852                                                                   ELTCOSMS
01853 **---------------------------------------------------------------+ELTCOSMS
01854 **                                                               |ELTCOSMS
01855 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCOSMS
01856 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCOSMS
01857 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTCOSMS
01858      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01859      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01860         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
01861                                                              '19' ELTCOSMS
01862         ADD +2 TO WS-CIA                                          ELTCOSMS
01863         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCOSMS
01864         MOVE 'Y'  TO  WS-PRINT-COMMON-LINE.                       ELTCOSMS
01865                                                                   ELTCOSMS
01866      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
01867      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
01868         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
01869                                                        '19'       ELTCOSMS
01870       IF NOT PRINT-COMMON-LINE                                    ELTCOSMS
01871         ADD +2 TO WS-CIA                                          ELTCOSMS
01872         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTCOSMS
01873                                                                   ELTCOSMS
01874      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01875      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01876         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOSMS
01877                            AND                                    ELTCOSMS
01878         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01879         SET  PLT-INDEX2  TO  2                                    ELTCOSMS
01880         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOSMS
01881                                                             ZERO  ELTCOSMS
01882            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP          ELTCOSMS
01883            ADD +1 TO WS-CIA                                       ELTCOSMS
01884            MOVE WS-BASIC-SUPP-LINE TO  COF-DTL-LINE(WS-CIA).      ELTCOSMS
01885                                                                   ELTCOSMS
01886      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01887      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01888         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCOSMS
01889                            AND                                    ELTCOSMS
01890         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTCOSMS
01891         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP             ELTCOSMS
01892         ADD +1 TO WS-CIA                                          ELTCOSMS
01893         MOVE WS-BASIC-SUPP-LINE  TO  COF-DTL-LINE(WS-CIA).        ELTCOSMS
01894                                                                   ELTCOSMS
01895      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTCOSMS
01896         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01897         SET  PLT-INDEX2  TO  2                                    ELTCOSMS
01898         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCOSMS
01899                                                             ZERO  ELTCOSMS
01900            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC-SUPP          ELTCOSMS
01901            ADD +1  TO  WS-CIA                                     ELTCOSMS
01902            MOVE WS-BASIC-SUPP-LINE  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
01903                                                                   ELTCOSMS
01904      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01905      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01906         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
01907                                                            =  ZEROELTCOSMS
01908            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01909                                                            =  ZEROELTCOSMS
01910               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCOSMS
01911            ELSE                                                   ELTCOSMS
01912               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOSMS
01913          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01914                                                  TO  WS-PERCENTAGEELTCOSMS
01915         ELSE                                                      ELTCOSMS
01916            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOSMS
01917          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01918                                                 TO  WS-PERCENTAGE.ELTCOSMS
01919                                                                   ELTCOSMS
01920      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
01921         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
01922                                             ZERO AND  NOT =  '19' ELTCOSMS
01923         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01924         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
01925         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOSMS
01926                                                    CMF-CODE-VALUE ELTCOSMS
01927         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
01928         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
01929         PERFORM 2200-CODES-MANUAL-WITH-PERCENT                    ELTCOSMS
01930         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01931         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01932                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01933         END-EXEC                                                  ELTCOSMS
01934         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01935         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01936                     WS-BASIC-SUPP                                 ELTCOSMS
01937         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01938                                                                   ELTCOSMS
01939      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
01940      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
01941         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCOSMS
01942                                                               ZEROELTCOSMS
01943            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01944                                                            =  ZEROELTCOSMS
01945               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCOSMS
01946            ELSE                                                   ELTCOSMS
01947               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCOSMS
01948          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01949                                                  TO  WS-PERCENTAGEELTCOSMS
01950         ELSE                                                      ELTCOSMS
01951            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCOSMS
01952          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCOSMS
01953                                                 TO  WS-PERCENTAGE.ELTCOSMS
01954                                                                   ELTCOSMS
01955      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
01956         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCOSMS
01957                                             ZERO AND  NOT =  '19' ELTCOSMS
01958         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
01959         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
01960         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCOSMS
01961                                                    CMF-CODE-VALUE ELTCOSMS
01962         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
01963         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
01964         PERFORM 2200-CODES-MANUAL-WITH-PERCENT                    ELTCOSMS
01965         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
01966         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
01967                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
01968         END-EXEC                                                  ELTCOSMS
01969         MOVE ZERO TO WS-CIA                                       ELTCOSMS
01970         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
01971                     WS-BASIC-SUPP                                 ELTCOSMS
01972         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
01973                                                                   ELTCOSMS
01974 **                                                               |ELTCOSMS
01975 **---------------------------------------------------------------+ELTCOSMS
01976                                                                   ELTCOSMS
01977                                                                   ELTCOSMS
01978      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
01979         SET PLT-INDEX2  TO  2                                     ELTCOSMS
01980      ELSE                                                         ELTCOSMS
01981         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
01982                                                                   ELTCOSMS
01983                                                                   ELTCOSMS
01984                                                                   ELTCOSMS
01985 **---------------------------------------------------------------+ELTCOSMS
01986 **                                                               |ELTCOSMS
01987 **        P A Y M E N T   I N D I C A T O R                      |ELTCOSMS
01988 **                                                               |ELTCOSMS
01989                                                                   ELTCOSMS
01990      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
01991      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
01992            IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
01993                                           NOT =  ZERO             ELTCOSMS
01994               ADD  +2  TO  WS-CIA                                 ELTCOSMS
01995               MOVE WS-CRITERIA-FOR      TO COF-DTL-LINE(WS-CIA)   ELTCOSMS
01996               MOVE 'Y' TO WS-PRINT-COMMON-LINE.                   ELTCOSMS
01997                                                                   ELTCOSMS
01998      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
01999      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
02000            IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
02001                                           NOT =  ZERO             ELTCOSMS
02002             IF NOT PRINT-COMMON-LINE                              ELTCOSMS
02003               ADD  +2  TO  WS-CIA                                 ELTCOSMS
02004               MOVE WS-CRITERIA-FOR  TO  COF-DTL-LINE(WS-CIA).     ELTCOSMS
02005                                                                   ELTCOSMS
02006      SET  PLT-INDEX2  TO  1.                                      ELTCOSMS
02007      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCOSMS
02008         IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
02009                                           NOT =  ZERO             ELTCOSMS
02010         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02011         MOVE 'COSM-SURG-PAYMT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCOSMS
02012         MOVE PLP-COSM-SURG-PAYMT-IND (PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
02013                                               TO  CMF-CODE-VALUE  ELTCOSMS
02014         MOVE WS-BASIC-LIT  TO  WS-TEMP-BASIC-SUPP-LIT             ELTCOSMS
02015         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
02016         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
02017         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02018         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02019                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02020         END-EXEC                                                  ELTCOSMS
02021         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02022         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02023                     WS-BASIC-SUPP                                 ELTCOSMS
02024         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02025                                                                   ELTCOSMS
02026                                                                   ELTCOSMS
02027      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCOSMS
02028         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02029         IF PLP-COSM-SURG-PAYMT-IND  (PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
02030                                           NOT =  ZERO             ELTCOSMS
02031         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02032         MOVE 'COSM-SURG-PAYMT-IND'   TO  CMF-ELEMENT-SYSTEM-NAME  ELTCOSMS
02033         MOVE PLP-COSM-SURG-PAYMT-IND (PLT-INDEX1, PLT-INDEX2)     ELTCOSMS
02034                                               TO  CMF-CODE-VALUE  ELTCOSMS
02035         MOVE WS-SUPP-LIT  TO  WS-TEMP-BASIC-SUPP-LIT              ELTCOSMS
02036         MOVE 'Y' TO WS-BASIC-SUPP                                 ELTCOSMS
02037         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
02038         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02039         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02040                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02041         END-EXEC                                                  ELTCOSMS
02042         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02043         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02044                     WS-BASIC-SUPP                                 ELTCOSMS
02045         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02046                                                                   ELTCOSMS
02047                                                                   ELTCOSMS
02048 **                                                               |ELTCOSMS
02049 **---------------------------------------------------------------+ELTCOSMS
02050                                                                   ELTCOSMS
02051                                                                   ELTCOSMS
02052      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
02053         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02054      ELSE                                                         ELTCOSMS
02055         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
02056                                                                   ELTCOSMS
02057                                                                   ELTCOSMS
02058 **---------------------------------------------------------------+ELTCOSMS
02059 **                                                               |ELTCOSMS
02060 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTCOSMS
02061      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
02062      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
02063         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCOSMS
02064                                                         NOT =  '0'ELTCOSMS
02065         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02066         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTCOSMS
02067                                           CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
02068         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTCOSMS
02069                                                TO  CMF-CODE-VALUE ELTCOSMS
02070         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
02071         ADD +1 TO WS-CIA                                          ELTCOSMS
02072         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
02073         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02074         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02075                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02076         END-EXEC                                                  ELTCOSMS
02077         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02078         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02079         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02080                                                                   ELTCOSMS
02081 **                                                               |ELTCOSMS
02082 **---------------------------------------------------------------+ELTCOSMS
02083                                                                   ELTCOSMS
02084 **---------------------------------------------------------------+ELTCOSMS
02085 **                                                               |ELTCOSMS
02086 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTCOSMS
02087      SET  PLT-INDEX2  TO  2.                                      ELTCOSMS
02088      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTCOSMS
02089         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCOSMS
02090                                                         NOT =  '0'ELTCOSMS
02091         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02092         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTCOSMS
02093                                           CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
02094         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
02095                                                 TO  CMF-CODE-VALUEELTCOSMS
02096         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
02097         ADD +1 TO WS-CIA                                          ELTCOSMS
02098         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
02099         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02100         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02101                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02102         END-EXEC                                                  ELTCOSMS
02103         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02104         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02105         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02106                                                                   ELTCOSMS
02107 **                                                               |ELTCOSMS
02108 **---------------------------------------------------------------+ELTCOSMS
02109                                                                   ELTCOSMS
02110                                                                   ELTCOSMS
02111 **---------------------------------------------------------------+ELTCOSMS
02112 **                                                               |ELTCOSMS
02113 **         TRANSFER TO OTHER RESPONSIBILITY INDICATOR            |ELTCOSMS
02114 **                                                               |ELTCOSMS
02115                                                                   ELTCOSMS
02116      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
02117         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02118      ELSE                                                         ELTCOSMS
02119         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
02120                                                                   ELTCOSMS
02121      IF  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)       ELTCOSMS
02122              NOT =  ZEROS                                         ELTCOSMS
02123         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02124         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTCOSMS
02125         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTCOSMS
02126                                                TO  CMF-CODE-VALUE ELTCOSMS
02127         MOVE SPACES        TO  WS-TEMP-TEXT-AREA                  ELTCOSMS
02128         ADD +1 TO WS-CIA                                          ELTCOSMS
02129         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTCOSMS
02130         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02131         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02132                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02133         END-EXEC                                                  ELTCOSMS
02134         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02135         MOVE 'Y' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02136         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02137 **                                                               |ELTCOSMS
02138 **---------------------------------------------------------------+ELTCOSMS
02139                                                                   ELTCOSMS
02140      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCOSMS
02141         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02142      ELSE                                                         ELTCOSMS
02143         SET PLT-INDEX2  TO  1.                                    ELTCOSMS
02144                                                                   ELTCOSMS
02145                                                                   ELTCOSMS
02146                                                                   ELTCOSMS
02147 **---------------------------------------------------------------+ELTCOSMS
02148 **                                                               |ELTCOSMS
02149 **                  # A A R   T A B U L A R   F O U N D          |ELTCOSMS
02150      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02151      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02152         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02153                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02154         MOVE +2  TO  WS-CIA                                       ELTCOSMS
02155         MOVE 'Y'  TO  WS-PRINT-COMMON-LINE                        ELTCOSMS
02156         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(WS-CIA)        ELTCOSMS
02157      ELSE                                                         ELTCOSMS
02158         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02159         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02160            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02161                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02162            MOVE 'Y'  TO  WS-PRINT-COMMON-LINE                     ELTCOSMS
02163            MOVE +2  TO  WS-CIA                                    ELTCOSMS
02164            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(WS-CIA).    ELTCOSMS
02165                                                                   ELTCOSMS
02166      IF PRINT-COMMON-LINE                                         ELTCOSMS
02167         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02168         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02169                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02170         END-EXEC                                                  ELTCOSMS
02171         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02172         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02173         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02174 **                                                               |ELTCOSMS
02175 **---------------------------------------------------------------+ELTCOSMS
02176                                                                   ELTCOSMS
02177 **---------------------------------------------------------------+ELTCOSMS
02178 **                                                               |ELTCOSMS
02179 **                  # P P F   T A B U L A R                      |ELTCOSMS
02180      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02181      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02182         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02183                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02184         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
02185                                                  KWA-GCTABULR-KEY ELTCOSMS
02186         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
02187         IF IOP-RC-OK                                              ELTCOSMS
02188            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTCOSMS
02189                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
02190            END-EXEC                                               ELTCOSMS
02191         ELSE                                                      ELTCOSMS
02192            NEXT SENTENCE                                          ELTCOSMS
02193      ELSE                                                         ELTCOSMS
02194         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02195         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02196            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02197                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02198          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
02199                                                  KWA-GCTABULR-KEY ELTCOSMS
02200            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
02201            IF IOP-RC-OK                                           ELTCOSMS
02202               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTCOSMS
02203                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
02204               END-EXEC.                                           ELTCOSMS
02205 **                                                               |ELTCOSMS
02206 **---------------------------------------------------------------+ELTCOSMS
02207                                                                   ELTCOSMS
02208 **---------------------------------------------------------------+ELTCOSMS
02209 **                                                               |ELTCOSMS
02210 **                  # P V E   T A B U L A R                      |ELTCOSMS
02211      MOVE 'Y' TO WS-PRINT-COMMON-LINE.                            ELTCOSMS
02212      MOVE +2  TO  WS-CIA.                                         ELTCOSMS
02213      MOVE WS-PVE               TO  COF-DTL-LINE(WS-CIA).          ELTCOSMS
02214      ADD +1   TO  WS-CIA.                                         ELTCOSMS
02215                                                                   ELTCOSMS
02216      IF PRINT-COMMON-LINE                                         ELTCOSMS
02217         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02218         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02219                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02220         END-EXEC                                                  ELTCOSMS
02221         MOVE ZERO TO WS-CIA                                       ELTCOSMS
02222         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02223         MOVE SPACE      TO COF-FUNCTION.                          ELTCOSMS
02224 **---------------------------------------------------------------+ELTCOSMS
02225                                                                   ELTCOSMS
02226 **---------------------------------------------------------------+ELTCOSMS
02227 **                                                               |ELTCOSMS
02228 **                  # A B M   T A B U L A R                      |ELTCOSMS
02229      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02230      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02231         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02232                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02233         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
02234                                                  KWA-GCTABULR-KEY ELTCOSMS
02235         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
02236         IF IOP-RC-OK                                              ELTCOSMS
02237            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTCOSMS
02238                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
02239            END-EXEC                                               ELTCOSMS
02240         ELSE                                                      ELTCOSMS
02241            NEXT SENTENCE                                          ELTCOSMS
02242      ELSE                                                         ELTCOSMS
02243         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02244         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02245            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02246                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02247          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
02248                                                  KWA-GCTABULR-KEY ELTCOSMS
02249            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
02250            IF IOP-RC-OK                                           ELTCOSMS
02251               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTCOSMS
02252                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
02253               END-EXEC.                                           ELTCOSMS
02254                                                                   ELTCOSMS
02255 **---------------------------------------------------------------+ELTCOSMS
02256 **                                                               |ELTCOSMS
02257 **                  # A C L   T A B U L A R                      |ELTCOSMS
02258      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02259      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02260         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02261                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02262         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
02263                                                  KWA-GCTABULR-KEY ELTCOSMS
02264         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
02265         IF IOP-RC-OK                                              ELTCOSMS
02266            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTCOSMS
02267                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
02268            END-EXEC                                               ELTCOSMS
02269         ELSE                                                      ELTCOSMS
02270            NEXT SENTENCE                                          ELTCOSMS
02271      ELSE                                                         ELTCOSMS
02272         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02273         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02274            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02275                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02276          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
02277                                                  KWA-GCTABULR-KEY ELTCOSMS
02278            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
02279            IF IOP-RC-OK                                           ELTCOSMS
02280               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTCOSMS
02281                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
02282               END-EXEC.                                           ELTCOSMS
02283 **                                                               |ELTCOSMS
02284 **---------------------------------------------------------------+ELTCOSMS
02285                                                                   ELTCOSMS
02286 **---------------------------------------------------------------+ELTCOSMS
02287 **                                                               |ELTCOSMS
02288 **                  # A D L   T A B U L A R                      |ELTCOSMS
02289      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02290      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02291         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02292                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02293         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
02294                                                  KWA-GCTABULR-KEY ELTCOSMS
02295         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
02296         IF IOP-RC-OK                                              ELTCOSMS
02297            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTCOSMS
02298                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
02299            END-EXEC                                               ELTCOSMS
02300         ELSE                                                      ELTCOSMS
02301            NEXT SENTENCE                                          ELTCOSMS
02302      ELSE                                                         ELTCOSMS
02303         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02304         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02305            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02306                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02307          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
02308                                                  KWA-GCTABULR-KEY ELTCOSMS
02309            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
02310            IF IOP-RC-OK                                           ELTCOSMS
02311               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTCOSMS
02312                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
02313               END-EXEC.                                           ELTCOSMS
02314 **                                                               |ELTCOSMS
02315 **---------------------------------------------------------------+ELTCOSMS
02316                                                                   ELTCOSMS
02317 **---------------------------------------------------------------+ELTCOSMS
02318 **                                                               |ELTCOSMS
02319 **                  # A O L   T A B U L A R                      |ELTCOSMS
02320      SET PLT-INDEX2  TO  1.                                       ELTCOSMS
02321      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTCOSMS
02322         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCOSMS
02323                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02324         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTCOSMS
02325                                                  KWA-GCTABULR-KEY ELTCOSMS
02326         PERFORM 2300-GET-TABULAR-RECORD                           ELTCOSMS
02327         IF IOP-RC-OK                                              ELTCOSMS
02328            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTCOSMS
02329                 COMMAREA(DFHCOMMAREA)                             ELTCOSMS
02330            END-EXEC                                               ELTCOSMS
02331         ELSE                                                      ELTCOSMS
02332            NEXT SENTENCE                                          ELTCOSMS
02333      ELSE                                                         ELTCOSMS
02334         SET PLT-INDEX2  TO  2                                     ELTCOSMS
02335         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTCOSMS
02336            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCOSMS
02337                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCOSMS
02338          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTCOSMS
02339                                                  KWA-GCTABULR-KEY ELTCOSMS
02340            PERFORM 2300-GET-TABULAR-RECORD                        ELTCOSMS
02341            IF IOP-RC-OK                                           ELTCOSMS
02342               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTCOSMS
02343                    COMMAREA(DFHCOMMAREA)                          ELTCOSMS
02344               END-EXEC.                                           ELTCOSMS
02345 **                                                               |ELTCOSMS
02346 **---------------------------------------------------------------+ELTCOSMS
02347                                                                   ELTCOSMS
02348 **---------------------------------------------------------------+ELTCOSMS
02349 **                                                               |ELTCOSMS
02350 **     P A Y M E N T  C O N S I D E R A T I O N  T E X T         |ELTCOSMS
02351      INITIALIZE TCAR-FROM-AREA.                                   ELTCOSMS
02352      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCOSMS
02353             WS-PAY-CONSDR-TEXT2                                   ELTCOSMS
02354                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCOSMS
02355      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCOSMS
02356      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCOSMS
02357      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCOSMS
02358                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCOSMS
02359      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCOSMS
02360      IF WS-CIA > 17                                               ELTCOSMS
02361         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02362         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02363                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02364         END-EXEC                                                  ELTCOSMS
02365         MOVE +1            TO WS-CIA.                             ELTCOSMS
02366      ADD +1                TO  WS-CIA.                            ELTCOSMS
02367      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
02368      ADD +1                TO  WS-CIA.                            ELTCOSMS
02369      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCOSMS
02370      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTCOSMS
02371      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCOSMS
02372                       COMMAREA(DFHCOMMAREA)                       ELTCOSMS
02373      END-EXEC.                                                    ELTCOSMS
02374      MOVE +1            TO WS-CIA.                                ELTCOSMS
02375 **                                                               |ELTCOSMS
02376 **---------------------------------------------------------------+ELTCOSMS
02377                                                                   ELTCOSMS
02378                                                                   ELTCOSMS
02379  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTCOSMS
02380                                                                   ELTCOSMS
02381      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCOSMS
02382         ADD +1   TO WS-CIA                                        ELTCOSMS
02383         MOVE 'N' TO WS-PRINT-COMMON-LINE                          ELTCOSMS
02384         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCOSMS
02385         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCOSMS
02386         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCOSMS
02387                                                    CMF-CODE-VALUE ELTCOSMS
02388         PERFORM 2400-CODES-MANUAL-CALL                            ELTCOSMS
02389         STRING CMF-DESCR-LINE (1) ' '                             ELTCOSMS
02390                CMF-DESCR-LINE (2)                                 ELTCOSMS
02391                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCOSMS
02392         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCOSMS
02393         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCOSMS
02394         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCOSMS
02395         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCOSMS
02396         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCOSMS
02397         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTCOSMS
02398         MOVE WS-SERVICES-2ND  TO COF-DTL-LINE(WS-CIA)             ELTCOSMS
02399         IF WS-CIA < 20                                            ELTCOSMS
02400            ADD +1 TO WS-CIA                                       ELTCOSMS
02401            MOVE ZERO TO                                           ELTCOSMS
02402                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         ELTCOSMS
02403         ELSE                                                      ELTCOSMS
02404            EXEC CICS LINK PROGRAM('ELUOUTPT')                     ELTCOSMS
02405            END-EXEC                                               ELTCOSMS
02406            MOVE +1 TO WS-CIA                                      ELTCOSMS
02407            MOVE ZERO TO                                           ELTCOSMS
02408                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTCOSMS
02409  4090-PROBLEM-WITH-INDICES.                                       ELTCOSMS
02410                                                                   ELTCOSMS
02411      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCOSMS
02412      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCOSMS
02413                                                                   ELTCOSMS
02414      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCOSMS
02415      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCOSMS
02416                                                                   ELTCOSMS
02417      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCOSMS
02418      END-EXEC.                                                    ELTCOSMS
02419                                                                   ELTCOSMS
02420  4999-EXIT.           EXIT.                                       ELTCOSMS
02421                                                                   ELTCOSMS
02422 /                                                                 ELTCOSMS
02423  5000-MOVE-LINES-OUT.                                             ELTCOSMS
02424      ADD +1 TO WS-CIA.                                            ELTCOSMS
02425      MOVE TCAR-OPF-DATA (WS-SUB5) TO COF-DTL-LINE (WS-CIA).       ELTCOSMS
02426      IF WS-CIA EQUAL 20                                           ELTCOSMS
02427         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02428         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02429                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02430         END-EXEC                                                  ELTCOSMS
02431         MOVE ZERO TO WS-CIA.                                      ELTCOSMS
02432  5000-EXIT.   EXIT.                                               ELTCOSMS
02433  5010-MOVE-LINES-OUT.                                             ELTCOSMS
02434      ADD +1 TO WS-CIA.                                            ELTCOSMS
02435      MOVE TCAR-OPF-DATA (WS-SUB5) TO WS-DTL-BASIC-SUPP.           ELTCOSMS
02436      MOVE WS-BASIC-SUPP-LINE  TO COF-DTL-LINE (WS-CIA).           ELTCOSMS
02437      IF WS-CIA EQUAL 20                                           ELTCOSMS
02438         MOVE WS-CIA TO  COF-NBR-DTL-LINES                         ELTCOSMS
02439         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCOSMS
02440                          COMMAREA(DFHCOMMAREA)                    ELTCOSMS
02441         END-EXEC                                                  ELTCOSMS
02442         MOVE ZERO TO WS-CIA.                                      ELTCOSMS
02443  5010-EXIT.   EXIT.                                               ELTCOSMS
02444  5020-MOVE-LINES-OUT.                                             ELTCOSMS
02445      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTCOSMS
02446             ' ' DELIMITED BY SIZE                                 ELTCOSMS
02447             CMF-DESCR-LINE (WS-SUB5) DELIMITED BY SIZE            ELTCOSMS
02448      INTO TCAR-FROM-AREA.                                         ELTCOSMS
02449  5020-EXIT.  EXIT.                                                ELTCOSMS
02450 /                 COSMETIC SURGERY PAYMENT ELIGIBILITY            ELTCOSMS
02451  5100-COSMETIC-SURGERY-PAYMENT.                                   ELTCOSMS
02452 *************************************************************     ELTCOSMS
02453 * THE FOLLOWING IS OBTAINED FROM CONTRACT(S) WHICH WILL BE  *     ELTCOSMS
02454 * SUB-DIVIDED BY BASIC AND SUPPLEMENTAL.                    *     ELTCOSMS
02455 *************************************************************     ELTCOSMS
02456                                                                   ELTCOSMS
02457      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTCOSMS
02458      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
02459          ADDRESS OF CONTRACT-RECORD.                              ELTCOSMS
02460      IF CIA-RC-PTR-NULL                                           ELTCOSMS
02461         SET CIA-ELSCONIS-DDN TO TRUE                              ELTCOSMS
02462         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTCOSMS
02463             ADDRESS OF CONTRACT-RECORD                            ELTCOSMS
02464         IF CIA-RC-PTR-NULL                                        ELTCOSMS
02465            GO TO 5199-EXIT.                                       ELTCOSMS
02466                                                                   ELTCOSMS
02467      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
02468      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
02469      MOVE 'N' TO WS-PRINT-COMMON-LINE                             ELTCOSMS
02470                  WS-BASIC-SUPP.                                   ELTCOSMS
02471                                                                   ELTCOSMS
02472      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTCOSMS
02473      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
02474          ADDRESS OF CONTRACT-RECORD.                              ELTCOSMS
02475      IF CIA-RC-OK                                                 ELTCOSMS
02476         PERFORM 5200-COSM-SURG-PAYMT-IND THRU 5299-EXIT.          ELTCOSMS
02477                                                                   ELTCOSMS
02478      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTCOSMS
02479      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOSMS
02480          ADDRESS OF CONTRACT-RECORD.                              ELTCOSMS
02481      IF CIA-RC-OK                                                 ELTCOSMS
02482         PERFORM 5300-COSM-SURG-PAYMT-IND THRU 5399-EXIT.          ELTCOSMS
02483                                                                   ELTCOSMS
02484      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
02485      MOVE 'N' TO WS-PRINT-COMMON-LINE                             ELTCOSMS
02486                  WS-BASIC-SUPP.                                   ELTCOSMS
02487      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
02488                                                                   ELTCOSMS
02489  5199-EXIT.  EXIT.                                                ELTCOSMS
02490                                                                   ELTCOSMS
02491  5200-COSM-SURG-PAYMT-IND.                                        ELTCOSMS
02492      IF GCT-COSM-SURG-PAYMT-IND  =  ZERO                          ELTCOSMS
02493          GO TO 5299-EXIT.                                         ELTCOSMS
02494      MOVE 'Y' TO WS-PRINT-COMMON-LINE.                            ELTCOSMS
02495      ADD +2 TO WS-CIA.                                            ELTCOSMS
02496      MOVE WS-CRITERIA-FOR TO COF-DTL-LINE (WS-CIA).               ELTCOSMS
02497                                                                   ELTCOSMS
02498      MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX.           ELTCOSMS
02499      MOVE 'COSM-SURG-PAYMT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELTCOSMS
02500      MOVE GCT-COSM-SURG-PAYMT-IND                                 ELTCOSMS
02501                                  TO  CMF-CODE-VALUE.              ELTCOSMS
02502      MOVE WS-BASIC-LIT           TO WS-TEMP-BASIC-SUPP-LIT.       ELTCOSMS
02503      MOVE 'Y' TO WS-BASIC-SUPP.                                   ELTCOSMS
02504      PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.          ELTCOSMS
02505      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTCOSMS
02506      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCOSMS
02507          COMMAREA(DFHCOMMAREA)                                    ELTCOSMS
02508      END-EXEC.                                                    ELTCOSMS
02509      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
02510      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
02511                                                                   ELTCOSMS
02512  5299-EXIT.  EXIT.                                                ELTCOSMS
02513                                                                   ELTCOSMS
02514  5300-COSM-SURG-PAYMT-IND.                                        ELTCOSMS
02515      IF GCT-COSM-SURG-PAYMT-IND  =  ZERO                          ELTCOSMS
02516          GO TO 5399-EXIT.                                         ELTCOSMS
02517      IF NOT PRINT-COMMON-LINE                                     ELTCOSMS
02518          ADD +2 TO WS-CIA                                         ELTCOSMS
02519          MOVE WS-CRITERIA-FOR TO COF-DTL-LINE (WS-CIA).           ELTCOSMS
02520                                                                   ELTCOSMS
02521      MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX.           ELTCOSMS
02522      MOVE 'COSM-SURG-PAYMT-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELTCOSMS
02523      MOVE GCT-COSM-SURG-PAYMT-IND                                 ELTCOSMS
02524                                  TO  CMF-CODE-VALUE.              ELTCOSMS
02525      MOVE WS-SUPP-LIT            TO WS-TEMP-BASIC-SUPP-LIT.       ELTCOSMS
02526      MOVE 'Y' TO WS-BASIC-SUPP.                                   ELTCOSMS
02527      PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.          ELTCOSMS
02528      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTCOSMS
02529      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCOSMS
02530          COMMAREA(DFHCOMMAREA)                                    ELTCOSMS
02531      END-EXEC.                                                    ELTCOSMS
02532      MOVE ZERO TO WS-CIA.                                         ELTCOSMS
02533      MOVE SPACE      TO COF-FUNCTION.                             ELTCOSMS
02534                                                                   ELTCOSMS
02535  5399-EXIT.  EXIT.                                                ELTCOSMS
02536                                                                   ELTCOSMS
02537                                                                   ELTCOSMS
02538      COPY ELSTCOMP.                                               ELTCOSMS
02539                                                                   ELTCOSMS
