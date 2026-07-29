00001 *      LAST MAINTENANCE TIME:  9.56.37  DATE: 04/19/86            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTCONGS
00003  PROGRAM-ID. ELTCONGS.                                               LV002
00004  AUTHOR. D SECOR  -  A C I.                                       ELTCONGS
00005  DATE-WRITTEN.   5/16/86.                                         ELTCONGS
00006  DATE-COMPILED.                                                   ELTCONGS
00007      SKIP3                                                        ELTCONGS
00008 ******************************************************************ELTCONGS
00009 *@>ELTCONGS                                                       ELTCONGS
00010 *@¬                                                               ELTCONGS
00011 *                        PROGRAM ABSTRACT                         ELTCONGS
00012 *                                                                 ELTCONGS
00013 *@¬ PROGRAM NAME:   E.L.S. CONGENITAL SURGERY BENEFITS            ELTCONGS
00014 *@¬                                                               ELTCONGS
00015 *@¬ PROGRAM I.D.:   ELTCONGS                                      ELTCONGS
00016 *@¬                                                               ELTCONGS
00017 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTCONGS
00018 *@¬            SURGICAL COVERAGE GIVEN A MEMBER WITH KNOWN        ELTCONGS
00019 *@¬            CONGENITAL DEFECTS.                                ELTCONGS
00020 *@¬                                                               ELTCONGS
00021 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF SURGICAL COVERAGE ELTCONGS
00022 *@¬            AFFORDED A MEMBER BY HIS GROUP, WHEN HE HAS        ELTCONGS
00023 *@¬            CONGENITAL DEFECTS.  THIS INFORMATION IS GOTTEN BY ELTCONGS
00024 *@¬            INTEROGATING THE BENEFIT PROVISIONS FOR THE GROUP  ELTCONGS
00025 *@¬            WITHIN THE CONTRACT FOR A PARTICULAR RANGE OF      ELTCONGS
00026 *@¬            DATES.                                             ELTCONGS
00027 *@¬                                                               ELTCONGS
00028 *@¬ RECORDS                                                       ELTCONGS
00029 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT, BENEFIT PROVISION FORMAT ELTCONGS
00030 *@¬            C.                                                 ELTCONGS
00031 *@¬                                                               ELTCONGS
00032 *@¬ PROCESSING                                                    ELTCONGS
00033 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTCONGS
00034 *@¬                                                               ELTCONGS
00035 *@¬                                                               ELTCONGS
00036 ***************************************************************** ELTCONGS
00037      SKIP3                                                        ELTCONGS
00038 ***************************************************************** ELTCONGS
00039 *               U D A T E   H I S T O R Y                       * ELTCONGS
00040 *                                                               * ELTCONGS
00041 * CHG NUM    DATE    PGM  DESCRIPTION                           * ELTCONGS
00042 * -------  --------  ---  ------------------------------------- * ELTCONGS
00043 *   ----   08/14/86  LET  USING THE 1ST HEADER LINE FROM THE    * ELTCONGS
00044 *                         PROLOG FOR THE TOPIC SCREENS.         * ELTCONGS
00045 *                                                               * ELTCONGS
00046 *   ----   10/01/86  JTC  COBOL VS II CONVERSION                * ELTCONGS
00047 *                                                               * ELTCONGS
00048 *   ----   10/19/87  NAC  REWORD PHRASE FOR COVERED BENEFITS.   * ELTCONGS
00049 *                                                               * ELTCONGS
00050 *   ----   04/07/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.      * ELTCONGS
00051 *                                                               * ELTCONGS
00052 *   ----   10/24/89  RKH  ADDED TRANSFER TO OTHER RESPONSIBILITY* ELTCONGS
00053 *                                                               * ELTCONGS
00054 *   ----   11/13/90  AKK  CHANGE '0' TEST ON TRANSF OF OTHER    * ELTCONGS
00055 *                         RESPONSIBILITY IND TO 'ZERO' DUE TO   * ELTCONGS
00056 *                         EXPANSION OF GCBENPVC COPYBOOK        * ELTCONGS
00057 *                         CAUSING RESULTING CHANGE IN ELSPLGTB  * ELTCONGS
00058 *                         COPYBOOK.                             * ELTCONGS
00059 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTCONGS
00060 ***************************************************************** ELTCONGS
00061 /                                                                 ELTCONGS
00062  ENVIRONMENT DIVISION.                                            ELTCONGS
00063      SKIP3                                                        ELTCONGS
00064  DATA DIVISION.                                                   ELTCONGS
00065  WORKING-STORAGE SECTION.                                         ELTCONGS
00066  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTCONGS
00067      '***ELTCONGS WS BEGINS***'.                                  ELTCONGS
00068  01  WS-PARA-COMMENTS.                                            ELTCONGS
00069    05  WS-PARA-ID1               PIC X(4) VALUE 'XXXX'.           ELTCONGS
00070    05  WS-PARA-ID2               PIC X(4) VALUE 'XXXX'.           ELTCONGS
00071    05  WS-PARA-ID3               PIC X(4) VALUE 'XXXX'.           ELTCONGS
00072                                                                   ELTCONGS
00073  01  WS-ABEND-CODE               PIC X(4) VALUE 'EMRG'.           ELTCONGS
00074                                                                   ELTCONGS
00075 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTCONGS
00076  01  WS-WORK-FIELDS.                                              ELTCONGS
00077      05  WS-CHAR-0                     PIC X.                     ELTCONGS
00078      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTCONGS
00079      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTCONGS
00080      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTCONGS
00081      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTCONGS
00082      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTCONGS
00083      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTCONGS
00084      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTCONGS
00085      05  WS-FIRSTTIME-IND              PIC X.                     ELTCONGS
00086        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTCONGS
00087      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTCONGS
00088        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTCONGS
00089      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTCONGS
00090        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTCONGS
00091      05  WS-EXPLANATION-IND            PIC S9 COMP-3.             ELTCONGS
00092        88  WS-EXPLANATION-PRODUCED         VALUE +1 THRU +3.      ELTCONGS
00093        88  WS-BASIC-EXPLANATION            VALUE +1, +3.          ELTCONGS
00094        88  WS-BASIC-ONLY-EXPLAIN           VALUE +1.              ELTCONGS
00095        88  WS-SUPP-EXPLANATION             VALUE +2 THRU +3.      ELTCONGS
00096        88  WS-SUPP-ONLY-EXPLAIN            VALUE +2.              ELTCONGS
00097        88  WS-NO-EXPLANATION               VALUE +0.              ELTCONGS
00098      05  WS-BASIC-EXPLAIN-CNT          PIC S9 COMP-3.             ELTCONGS
00099      05  WS-SUPP-EXPLAIN-CNT           PIC S9 COMP-3.             ELTCONGS
00100      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTCONGS
00101      05  WS-PERCENT-FLD.                                          ELTCONGS
00102        10  WS-PERCENTAGE               PIC ZZ9.                   ELTCONGS
00103        10  WS-PERCENT-SIGN             PIC X.                     ELTCONGS
00104                                                                   ELTCONGS
00105 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTCONGS
00106  01  WS-BEN-PROV-ID.                                              ELTCONGS
00107      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +1. ELTCONGS
00108      05  WS-PROF-INPAT-CNT             PIC S9(4) COMP   VALUE +1. ELTCONGS
00109      05  WS-PROF-INPAT-TAB.                                       ELTCONGS
00110        10  FILLER                      PIC X(6)  VALUE 'CDSI C'.  ELTCONGS
00111      05  WS-PROF-INPAT-LIST  REDEFINES    WS-PROF-INPAT-TAB       ELTCONGS
00112                                        PIC X(6)  OCCURS 1 TIMES.  ELTCONGS
00113                                                                   ELTCONGS
00114      05  WS-PROF-OUTPAT-CNT            PIC S9(4)  COMP  VALUE +1. ELTCONGS
00115      05  WS-PROF-OUTPAT-TAB.                                      ELTCONGS
00116        10  FILLER                      PIC X(6)  VALUE 'CDSO C'.  ELTCONGS
00117      05  WS-PROF-OUTPAT-LIST    REDEFINES    WS-PROF-OUTPAT-TAB   ELTCONGS
00118                                        PIC X(6)  OCCURS 1 TIMES.  ELTCONGS
00119                                                                   ELTCONGS
00120 /            D I S P L A Y   L I N E S                            ELTCONGS
00121  01  WS-ELS-DISPLAY-LINES.                                        ELTCONGS
00122    05  WS-HDR-2-PROF-INPAT.                                       ELTCONGS
00123      10  FILLER                    PIC X(19) VALUE SPACES.        ELTCONGS
00124      10  FILLER                    PIC X(41)                      ELTCONGS
00125          VALUE 'CONGENITAL SURGERY INPATIENT PROFESSIONAL'.       ELTCONGS
00126      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTCONGS
00127                                                                   ELTCONGS
00128    05  WS-HDR-2-PROF-OUTPAT.                                      ELTCONGS
00129      10  FILLER                    PIC X(18) VALUE SPACES.        ELTCONGS
00130      10  FILLER                    PIC X(42)                      ELTCONGS
00131          VALUE 'CONGENITAL SURGERY OUTPATIENT PROFESSIONAL'.      ELTCONGS
00132      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTCONGS
00133                                                                   ELTCONGS
00134    05  WS-HDR-2-INST.                                             ELTCONGS
00135      10  FILLER                    PIC X(23) VALUE SPACES.        ELTCONGS
00136      10  FILLER                    PIC X(32)                      ELTCONGS
00137          VALUE 'CONGENITAL SURGERY INSTITUTIONAL'.                ELTCONGS
00138      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTCONGS
00139                                                                   ELTCONGS
00140    05  WS-FOLLOWING-BEN.                                          ELTCONGS
00141      10  FILLER                    PIC X(21) VALUE                ELTCONGS
00142          'COVERED SERVICES ARE:'.                                 ELTCONGS
00143                                                                   ELTCONGS
00144    05  WS-PAY-CONSDR-TEXT1.                                       ELTCONGS
00145      10  FILLER                    PIC X(45)                      ELTCONGS
00146        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTCONGS
00147                                                                   ELTCONGS
00148    05  WS-PAY-CONSDR-TEXT2.                                       ELTCONGS
00149      10  FILLER                    PIC X(44)                      ELTCONGS
00150        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTCONGS
00151                                                                   ELTCONGS
00152    05  WS-STANCES-FOR-ELIGIBILITY  PIC X(37)                      ELTCONGS
00153          VALUE 'THE CIRCUMSTANCES FOR ELIGIBILITY IS:'.           ELTCONGS
00154                                                                   ELTCONGS
00155    05  WS-CONGENITAL-SURGERY-IS    PIC X(22)                      ELTCONGS
00156          VALUE 'CONGENITAL SURGERY IS '.                          ELTCONGS
00157                                                                   ELTCONGS
00158    05  WS-CONGENITAL-COVERED       PIC X(29)                      ELTCONGS
00159          VALUE 'CONGENITAL SURGERY IS COVERED'.                   ELTCONGS
00160                                                                   ELTCONGS
00161    05  WS-CONGENITAL-NOT-COVERED   PIC X(33)                      ELTCONGS
00162          VALUE 'CONGENITAL SURGERY IS NOT COVERED'.               ELTCONGS
00163                                                                   ELTCONGS
00164    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTCONGS
00165          VALUE 'SERVICES MAY BE RENDERED '.                       ELTCONGS
00166                                                                   ELTCONGS
00167    05  WS-PAYMNT-BASED.                                           ELTCONGS
00168      10  FILLER                    PIC X(20)                      ELTCONGS
00169          VALUE 'PAYMENT IS BASED ON '.                            ELTCONGS
00170      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTCONGS
00171                                                                   ELTCONGS
00172    05  WS-BASIC.                                                  ELTCONGS
00173      10  WS-BASIC-LIT              PIC X(16)                      ELTCONGS
00174         VALUE '         BASIC: '.                                 ELTCONGS
00175      10  WS-DTL-BASIC-LONG.                                       ELTCONGS
00176        15  WS-DTL-BASIC            PIC X(50) VALUE SPACES.        ELTCONGS
00177        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTCONGS
00178                                                                   ELTCONGS
00179    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTCONGS
00180        VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTCONGS
00181                                                                   ELTCONGS
00182    05  WS-SUPPLEMENTAL.                                           ELTCONGS
00183      10  WS-SUPP-LIT               PIC X(16)                      ELTCONGS
00184          VALUE '  SUPPLEMENTAL: '.                                ELTCONGS
00185      10  WS-DTL-SUPP-LONG.                                        ELTCONGS
00186        15  WS-DTL-SUPPLEMENTAL     PIC X(50) VALUE SPACES.        ELTCONGS
00187        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTCONGS
00188                                                                   ELTCONGS
00189    05  WS-PAYABLE-AS.                                             ELTCONGS
00190      10  FILLER                    PIC X(40) VALUE                ELTCONGS
00191          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTCONGS
00192      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTCONGS
00193                                                                   ELTCONGS
00194    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER '. ELTCONGS
00195                                                                   ELTCONGS
00196    05  WS-FOR-SUPP-ACCIDENT.                                      ELTCONGS
00197      10  FILLER                    PIC X(26)                      ELTCONGS
00198        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTCONGS
00199      10  WS-NO-DAYS                PIC ZZ9.                       ELTCONGS
00200      10  WS-FOR-SUPP-ACC-DESC      PIC X(50).                     ELTCONGS
00201                                                                   ELTCONGS
00202    05  WS-SEE-RM-AND-BOARD.                                       ELTCONGS
00203      10  FILLER                    PIC X(44)                      ELTCONGS
00204        VALUE 'SEE ROOM AND BOARD FOR ADDITIONAL INPATIENT '.      ELTCONGS
00205      10  FILLER                    PIC X(8)  VALUE 'BENEFITS'.    ELTCONGS
00206      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTCONGS
00207                                                                   ELTCONGS
00208    05  WS-SEE-OP-SURGERY.                                         ELTCONGS
00209      10  FILLER                    PIC X(44)                      ELTCONGS
00210        VALUE 'SEE OUTPATIENT SURGERY TOPIC FOR OUTPATIENT '.      ELTCONGS
00211      10  FILLER                    PIC X(8)  VALUE 'BENEFITS'.    ELTCONGS
00212      10  FILLER                    PIC X(27) VALUE LOW-VALUES.    ELTCONGS
00213                                                                   ELTCONGS
00214    05  WS-CONTRACT-RELATED.                                       ELTCONGS
00215      10  FILLER                    PIC X(48)                      ELTCONGS
00216        VALUE 'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTCONGS
00217      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTCONGS
00218                                                                   ELTCONGS
00219    05  WS-PROVIDER-ELIGIBILITY.                                   ELTCONGS
00220      10  FILLER                    PIC X(43)                      ELTCONGS
00221        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY'.       ELTCONGS
00222      10  FILLER                    PIC X(36) VALUE LOW-VALUES.    ELTCONGS
00223                                                                   ELTCONGS
00224    05  WS-NO-TABULAR1.                                            ELTCONGS
00225      10  FILLER                    PIC X(51)  VALUE               ELTCONGS
00226         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTCONGS
00227      10  FILLER                    PIC X(22)  VALUE               ELTCONGS
00228         'GOING FROM BENEFIT ***'.                                 ELTCONGS
00229                                                                   ELTCONGS
00230    05  WS-NO-TABULAR2.                                            ELTCONGS
00231      10  FILLER                    PIC X(15)  VALUE               ELTCONGS
00232         '*** PROVISION: '.                                        ELTCONGS
00233      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTCONGS
00234      10  FILLER                    PIC X VALUE SPACE.             ELTCONGS
00235      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTCONGS
00236      10  FILLER                    PIC X(13)  VALUE               ELTCONGS
00237         ' TO TABULAR: '.                                          ELTCONGS
00238      10  WS-NO-TAB-ID              PIC X(6).                      ELTCONGS
00239      10  FILLER                    PIC X VALUE SPACE.             ELTCONGS
00240      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTCONGS
00241      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTCONGS
00242                                                                   ELTCONGS
00243    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTCONGS
00244    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTCONGS
00245      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTCONGS
00246                                                                   ELTCONGS
00247  01  WS-END                            PIC X(16)  VALUE           ELTCONGS
00248      '*** W/S ENDS ***'.                                          ELTCONGS
00249 /             L I N K A G E   S E C T I O N                       ELTCONGS
00250  LINKAGE SECTION.                                                 ELTCONGS
00251  01  DFHCOMMAREA.                                                 ELTCONGS
00252      COPY ELSCOMMC.                                               ELTCONGS
00253 /                                                                 ELTCONGS
00254      COPY ELSCIA2C.                                               ELTCONGS
00255 /                                                                 ELTCONGS
00256 ***  IO PARM AREA ***                                             ELTCONGS
00257      COPY ELSIOPMC.                                               ELTCONGS
00258 /                                                                 ELTCONGS
00259      COPY ELSKEYSC.                                               ELTCONGS
00260 /                                                                 ELTCONGS
00261      COPY ELSOUTPC.                                               ELTCONGS
00262 /                                                                 ELTCONGS
00263      COPY ELSSSCBC.                                               ELTCONGS
00264 /                                                                 ELTCONGS
00265      COPY ELSCMIFC.                                               ELTCONGS
00266 /                                                                 ELTCONGS
00267      COPY ELSCMDSC.                                               ELTCONGS
00268 /                                                                 ELTCONGS
00269      COPY ELSPRVNC.                                               ELTCONGS
00270 /                                                                 ELTCONGS
00271 ****  PAYMENT LEVEL FLD REQUEST INDS ***                          ELTCONGS
00272      COPY ELSPLGSW.                                               ELTCONGS
00273 /                                                                 ELTCONGS
00274 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTCONGS
00275      COPY ELSPLGTB.                                               ELTCONGS
00276 /                                                                 ELTCONGS
00277      COPY ELSTCWAC.                                               ELTCONGS
00278 /        C O N T R A C T   R E C O R D                            ELTCONGS
00279  01  CONTRACT-RECORD.                                             ELTCONGS
00280      COPY GCCONTRC.                                               ELTCONGS
00281 /                  M A I N L I N E                                ELTCONGS
00282  PROCEDURE DIVISION.                                              ELTCONGS
00283                                                                   ELTCONGS
00284 ******************************************************************ELTCONGS
00285 *                                                                 ELTCONGS
00286 *   PERFORM THE MAINLINE OPERATIONS.                              ELTCONGS
00287 *                                                                 ELTCONGS
00288 ******************************************************************ELTCONGS
00289  0000-MAINLINE   SECTION.                                         ELTCONGS
00290                                                                   ELTCONGS
00291      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTCONGS
00292         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTCONGS
00293         EXEC CICS  ABEND  ABCODE(CIA-ABCODE)                      ELTCONGS
00294         END-EXEC.                                                 ELTCONGS
00295                                                                   ELTCONGS
00296      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCONGS
00297          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCONGS
00298                                                                   ELTCONGS
00299      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCONGS
00300      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00301          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCONGS
00302                                                                   ELTCONGS
00303      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCONGS
00304      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00305          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCONGS
00306                                                                   ELTCONGS
00307      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCONGS
00308      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00309          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTCONGS
00310                                                                   ELTCONGS
00311      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCONGS
00312      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00313          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCONGS
00314                                                                   ELTCONGS
00315      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCONGS
00316      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00317          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTCONGS
00318                                                                   ELTCONGS
00319      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTCONGS
00320      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00321          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTCONGS
00322                                                                   ELTCONGS
00323      MOVE '0'  TO  WS-CHAR-0.                                     ELTCONGS
00324                                                                   ELTCONGS
00325      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTCONGS
00326                                                                   ELTCONGS
00327      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTCONGS
00328               (WS-TABLE-MAX-CNT  *   LENGTH OF PVN-BEN-PROVN-TBL).ELTCONGS
00329                                                                   ELTCONGS
00330      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTCONGS
00331                                                                   ELTCONGS
00332      SET CIA-STG-GETMAIN TO TRUE.                                 ELTCONGS
00333                                                                   ELTCONGS
00334      EXEC CICS LINK                                               ELTCONGS
00335                PROGRAM('ELUSTGMG')                                ELTCONGS
00336                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00337      END-EXEC.                                                    ELTCONGS
00338                                                                   ELTCONGS
00339      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTCONGS
00340      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00341          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTCONGS
00342                                                                   ELTCONGS
00343      IF SSB-PROV-CLASS-INST OR  SSB-PROV-CLASS-BOTH               ELTCONGS
00344         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTCONGS
00345                                                                   ELTCONGS
00346      IF SSB-PROV-CLASS-PROF OR  SSB-PROV-CLASS-BOTH               ELTCONGS
00347         PERFORM 2000-PROFESSIONAL-IP-RTNE                         ELTCONGS
00348         PERFORM 3000-PROFESSIONAL-OP-RTNE.                        ELTCONGS
00349                                                                   ELTCONGS
00350                                                                   ELTCONGS
00351      IF NOT SSB-PROV-CLASS-PROF AND  NOT SSB-PROV-CLASS-BOTH AND  ELTCONGS
00352         NOT SSB-PROV-CLASS-INST                                   ELTCONGS
00353         MOVE 'P'  TO  COF-FUNCTION                                ELTCONGS
00354         MOVE ZERO  TO  COF-NBR-HDR-LINES,                         ELTCONGS
00355                        COF-NBR-DTL-LINES                          ELTCONGS
00356         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00357               COMMAREA(DFHCOMMAREA)                               ELTCONGS
00358         END-EXEC                                                  ELTCONGS
00359         MOVE SPACE  TO  COF-FUNCTION                              ELTCONGS
00360         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTCONGS
00361         MOVE 2  TO  COF-NBR-DTL-LINES                             ELTCONGS
00362         MOVE '*** I N V A L I D   R E Q U E S T ***'  TO          ELTCONGS
00363                                                COF-DTL-LINE(2)    ELTCONGS
00364         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00365               COMMAREA(DFHCOMMAREA)                               ELTCONGS
00366         END-EXEC.                                                 ELTCONGS
00367                                                                   ELTCONGS
00368      MOVE 'E'  TO  COF-FUNCTION.                                  ELTCONGS
00369      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCONGS
00370                     COF-NBR-DTL-LINES.                            ELTCONGS
00371      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00372      END-EXEC.                                                    ELTCONGS
00373                                                                   ELTCONGS
00374                                                                   ELTCONGS
00375  0099-RETURN.                                                     ELTCONGS
00376      EXEC CICS RETURN   END-EXEC.                                 ELTCONGS
00377                                                                   ELTCONGS
00378      GOBACK.                                                      ELTCONGS
00379 /        I N S T I T U T I O N A L   I P   R T N E                ELTCONGS
00380 ***************************************************************** ELTCONGS
00381 *        I N S T I T U T I O N A L   I P   R T N E                ELTCONGS
00382 *                                                                 ELTCONGS
00383 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCONGS
00384 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCONGS
00385 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCONGS
00386 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCONGS
00387 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCONGS
00388 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCONGS
00389 *  MODULE.                                                        ELTCONGS
00390 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCONGS
00391 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCONGS
00392 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCONGS
00393 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCONGS
00394 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCONGS
00395 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCONGS
00396 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCONGS
00397 *                                                                 ELTCONGS
00398 ***************************************************************** ELTCONGS
00399  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTCONGS
00400      MOVE '1000'  TO  WS-PARA-ID1.                                ELTCONGS
00401                                                                   ELTCONGS
00402      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONGS
00403      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCONGS
00404      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCONGS
00405                     COF-NBR-DTL-LINES.                            ELTCONGS
00406      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00407      END-EXEC.                                                    ELTCONGS
00408                                                                   ELTCONGS
00409      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONGS
00410      MOVE WS-HDR-2-INST  TO  COF-HDR-LINE(2).                     ELTCONGS
00411                                                                   ELTCONGS
00412      MOVE LOW-VALUES  TO  COF-DTL-LINE(1).                        ELTCONGS
00413      MOVE +1  TO  COF-NBR-DTL-LINES,     WS-CIA.                  ELTCONGS
00414      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00415      END-EXEC.                                                    ELTCONGS
00416                                                                   ELTCONGS
00417 **---------------------------------------------------------------+ELTCONGS
00418 **                                                               |ELTCONGS
00419 **                C I R C U M S T A N C E S                      |ELTCONGS
00420 **                         F O R                                 |ELTCONGS
00421 **                  E L I G I B I L I T Y                        |ELTCONGS
00422      PERFORM 2200-INST-ELIGIBILITY.                               ELTCONGS
00423 **                                                               |ELTCONGS
00424 **---------------------------------------------------------------+ELTCONGS
00425                                                                   ELTCONGS
00426 **---------------------------------------------------------------+ELTCONGS
00427 **                                                               |ELTCONGS
00428 **   S E E   R O O M   &   B O A R D   F O R   A D D I T I O N S |ELTCONGS
00429 **                           A N D                               |ELTCONGS
00430 **   S E E   O / P   S U R G E R Y   T O P I C   F O R   B E N . |ELTCONGS
00431      MOVE WS-SEE-RM-AND-BOARD  TO  COF-DTL-LINE(WS-CIA).          ELTCONGS
00432                                                                   ELTCONGS
00433      ADD +2  TO  WS-CIA.                                          ELTCONGS
00434                                                                   ELTCONGS
00435      MOVE WS-SEE-OP-SURGERY  TO  COF-DTL-LINE(WS-CIA).            ELTCONGS
00436                                                                   ELTCONGS
00437                                                                   ELTCONGS
00438      ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTCONGS
00439      MOVE +1  TO  WS-CIA.                                         ELTCONGS
00440      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONGS
00441                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00442      END-EXEC.                                                    ELTCONGS
00443 **                                                               |ELTCONGS
00444 **---------------------------------------------------------------+ELTCONGS
00445                                                                   ELTCONGS
00446                                                                   ELTCONGS
00447                                                                   ELTCONGS
00448  1099-EXIT.            EXIT.                                      ELTCONGS
00449 /        P R O F E S S I O N A L   I P   R T N E                  ELTCONGS
00450 ***************************************************************** ELTCONGS
00451 *        P R O F E S S I O N A L   I P   R T N E                  ELTCONGS
00452 *                                                                 ELTCONGS
00453 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCONGS
00454 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCONGS
00455 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCONGS
00456 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCONGS
00457 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCONGS
00458 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCONGS
00459 *  MODULE.                                                        ELTCONGS
00460 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCONGS
00461 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCONGS
00462 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCONGS
00463 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCONGS
00464 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCONGS
00465 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCONGS
00466 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCONGS
00467 *                                                                 ELTCONGS
00468 ***************************************************************** ELTCONGS
00469  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTCONGS
00470      MOVE '2000'  TO  WS-PARA-ID1.                                ELTCONGS
00471                                                                   ELTCONGS
00472      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONGS
00473      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCONGS
00474      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCONGS
00475                     COF-NBR-DTL-LINES.                            ELTCONGS
00476      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00477      END-EXEC.                                                    ELTCONGS
00478                                                                   ELTCONGS
00479      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONGS
00480      MOVE WS-HDR-2-PROF-INPAT  TO  COF-HDR-LINE(2).               ELTCONGS
00481                                                                   ELTCONGS
00482      MOVE WS-PROF-INPAT-CNT  TO  PVN-NBR-BEN-PROVN.               ELTCONGS
00483      PERFORM 2010-MOVE-IN-PROF-ACC                                ELTCONGS
00484         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCONGS
00485         UNTIL WS-SUB  >  WS-PROF-INPAT-CNT.                       ELTCONGS
00486                                                                   ELTCONGS
00487      GO TO 2020-CALL-COVERAGE.                                    ELTCONGS
00488  2010-MOVE-IN-PROF-ACC.                                           ELTCONGS
00489      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCONGS
00490      MOVE WS-PROF-INPAT-LIST(WS-SUB)  TO                          ELTCONGS
00491                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCONGS
00492      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCONGS
00493                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCONGS
00494                                                                   ELTCONGS
00495  2020-CALL-COVERAGE.                                              ELTCONGS
00496      MOVE '2020'  TO  WS-PARA-ID1.                                ELTCONGS
00497      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONGS
00498      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00499      END-EXEC.                                                    ELTCONGS
00500                                                                   ELTCONGS
00501      MOVE WS-CONGENITAL-SURGERY-IS  TO  SSB-TOPIC-PHRASE.         ELTCONGS
00502                                                                   ELTCONGS
00503      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCONGS
00504      END-EXEC.                                                    ELTCONGS
00505                                                                   ELTCONGS
00506      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTCONGS
00507      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00508      END-EXEC.                                                    ELTCONGS
00509                                                                   ELTCONGS
00510      IF PVN-COVG-NONE                                             ELTCONGS
00511         GO TO 2099-EXIT.                                          ELTCONGS
00512                                                                   ELTCONGS
00513      MOVE +1  TO  WS-CIA.                                         ELTCONGS
00514                                                                   ELTCONGS
00515      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCONGS
00516                                                                   ELTCONGS
00517      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCONGS
00518            PSP-PROVN-PRICING-METHD,                               ELTCONGS
00519            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCONGS
00520            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONGS
00521            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONGS
00522            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCONGS
00523            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCONGS
00524            PSP-SPILL-OVER-DED-APL-IND,                            ELTCONGS
00525            PSP-CONG-DFCT-SURG-PMT-ELG,                            ELTCONGS
00526            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCONGS
00527            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCONGS
00528            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCONGS
00529            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCONGS
00530            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCONGS
00531            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCONGS
00532            PSC-BEN-SCOPE-ID.                                      ELTCONGS
00533                                                                   ELTCONGS
00534      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCONGS
00535      END-EXEC.                                                    ELTCONGS
00536                                                                   ELTCONGS
00537      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCONGS
00538      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
00539          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCONGS
00540                                                                   ELTCONGS
00541      PERFORM 2030-FIND-FIRST-NONZERO                              ELTCONGS
00542         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCONGS
00543         UNTIL WS-SUB  >  WS-PROF-INPAT-CNT.                       ELTCONGS
00544                                                                   ELTCONGS
00545      GO TO 2099-EXIT.                                             ELTCONGS
00546                                                                   ELTCONGS
00547  2030-FIND-FIRST-NONZERO.                                         ELTCONGS
00548      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONGS
00549      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCONGS
00550         NEXT SENTENCE                                             ELTCONGS
00551      ELSE                                                         ELTCONGS
00552         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTCONGS
00553                                                                   ELTCONGS
00554  2040-BUILD-SCREEN-LINES.                                         ELTCONGS
00555      MOVE '2040'  TO  WS-PARA-ID1.                                ELTCONGS
00556                                                                   ELTCONGS
00557      SET PLT-INDEX1   TO                                          ELTCONGS
00558                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTCONGS
00559      IF WS-NOT-FIRST-TIME                                         ELTCONGS
00560         MOVE 'P'  TO  COF-FUNCTION                                ELTCONGS
00561         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00562             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
00563         END-EXEC                                                  ELTCONGS
00564      ELSE                                                         ELTCONGS
00565         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCONGS
00566                                                                   ELTCONGS
00567      MOVE +1  TO  WS-CIA.                                         ELTCONGS
00568      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
00569         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTCONGS
00570            SET PLT-INDEX2  TO  2                                  ELTCONGS
00571         ELSE                                                      ELTCONGS
00572            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTCONGS
00573            GO TO 2099-EXIT                                        ELTCONGS
00574      ELSE                                                         ELTCONGS
00575         SET PLT-INDEX2  TO  1.                                    ELTCONGS
00576                                                                   ELTCONGS
00577 **---------------------------------------------------------------+ELTCONGS
00578 **                                                               |ELTCONGS
00579 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCONGS
00580      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCONGS
00581      ADD  +1  TO  WS-CIA.                                         ELTCONGS
00582      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCONGS
00583      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTCONGS
00584         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTCONGS
00585         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-INPAT-CNT.              ELTCONGS
00586      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONGS
00587                                                                   ELTCONGS
00588      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCONGS
00589      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00590      END-EXEC.                                                    ELTCONGS
00591      MOVE +1  TO  WS-CIA.                                         ELTCONGS
00592 **                                                               |ELTCONGS
00593 **---------------------------------------------------------------+ELTCONGS
00594                                                                   ELTCONGS
00595                                                                   ELTCONGS
00596 **---------------------------------------------------------------+ELTCONGS
00597 **                                                               |ELTCONGS
00598 **        P L A C E   O F   T R E A T M E N T                    |ELTCONGS
00599      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
00600                                                              ZERO ELTCONGS
00601         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00602         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCONGS
00603         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCONGS
00604                                               TO  CMF-CODE-VALUE  ELTCONGS
00605         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00606         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTCONGS
00607         PERFORM 2100-CODES-MANUAL-LONG                            ELTCONGS
00608         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
00609         MOVE +1  TO  WS-CIA                                       ELTCONGS
00610         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00611               COMMAREA(DFHCOMMAREA)                               ELTCONGS
00612         END-EXEC.                                                 ELTCONGS
00613 **                                                               |ELTCONGS
00614 **---------------------------------------------------------------+ELTCONGS
00615                                                                   ELTCONGS
00616 **---------------------------------------------------------------+ELTCONGS
00617 **                                                               |ELTCONGS
00618 **            B E N E F I T   S C O P E   I D                    |ELTCONGS
00619      SET PLT-INDEX2  TO  1.                                       ELTCONGS
00620      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00621         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
00622                                       '0000' AND  NOT =  '00  '   ELTCONGS
00623         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00624         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTCONGS
00625         ADD  +1  TO  WS-CIA.                                      ELTCONGS
00626                                                                   ELTCONGS
00627      SET PLT-INDEX2  TO  2.                                       ELTCONGS
00628      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00629         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
00630                                  '0000' AND  NOT =  '00  ' AND    ELTCONGS
00631         NOT WS-ADD-A-BLANK-LINE                                   ELTCONGS
00632         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00633         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTCONGS
00634         ADD  +1  TO  WS-CIA.                                      ELTCONGS
00635                                                                   ELTCONGS
00636      SET PLT-INDEX2  TO  1.                                       ELTCONGS
00637      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00638         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
00639                                       '0000' AND  NOT =  '00  '   ELTCONGS
00640         MOVE 'BPC'  TO  CMF-RECORD-PREFIX                         ELTCONGS
00641         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTCONGS
00642         MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTCONGS
00643                                                    CMF-CODE-VALUE ELTCONGS
00644         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00645         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00646         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00647                                                                   ELTCONGS
00648      SET PLT-INDEX2  TO  2.                                       ELTCONGS
00649      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00650         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
00651                                       '0000' AND  NOT =  '00  '   ELTCONGS
00652         MOVE 'BPC'  TO  CMF-RECORD-PREFIX                         ELTCONGS
00653         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTCONGS
00654         MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTCONGS
00655                                                    CMF-CODE-VALUE ELTCONGS
00656         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
00657         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00658         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00659                                                                   ELTCONGS
00660      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
00661         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00662         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
00663         MOVE 1  TO  WS-CIA                                        ELTCONGS
00664         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00665             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
00666         END-EXEC.                                                 ELTCONGS
00667 **                                                               |ELTCONGS
00668 **---------------------------------------------------------------+ELTCONGS
00669                                                                   ELTCONGS
00670      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
00671         SET PLT-INDEX2  TO  2                                     ELTCONGS
00672      ELSE                                                         ELTCONGS
00673         SET PLT-INDEX2  TO  1.                                    ELTCONGS
00674                                                                   ELTCONGS
00675 **---------------------------------------------------------------+ELTCONGS
00676 **                                                               |ELTCONGS
00677 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCONGS
00678 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCONGS
00679 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTCONGS
00680      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00681      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00682         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
00683                                            ZERO AND  NOT =  '19'  ELTCONGS
00684         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCONGS
00685         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00686         ADD +1  TO  WS-CIA.                                       ELTCONGS
00687                                                                   ELTCONGS
00688      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
00689      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00690         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
00691                                        ZERO AND  NOT =  '19' AND  ELTCONGS
00692         NOT WS-ADD-A-BLANK-LINE                                   ELTCONGS
00693         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCONGS
00694         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00695         ADD +1  TO  WS-CIA.                                       ELTCONGS
00696                                                                   ELTCONGS
00697      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00698      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00699         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCONGS
00700                              AND                                  ELTCONGS
00701         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
00702         SET  PLT-INDEX2  TO  2                                    ELTCONGS
00703         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCONGS
00704                                                              ZERO ELTCONGS
00705            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCONGS
00706            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCONGS
00707            ADD +1  TO  WS-CIA.                                    ELTCONGS
00708                                                                   ELTCONGS
00709      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00710      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00711         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCONGS
00712                               AND                                 ELTCONGS
00713         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTCONGS
00714         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTCONGS
00715         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTCONGS
00716         ADD +1  TO  WS-CIA.                                       ELTCONGS
00717                                                                   ELTCONGS
00718      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTCONGS
00719         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
00720         SET  PLT-INDEX2  TO  2                                    ELTCONGS
00721         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCONGS
00722                                                              ZERO ELTCONGS
00723            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCONGS
00724            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCONGS
00725            ADD +1  TO  WS-CIA.                                    ELTCONGS
00726                                                                   ELTCONGS
00727      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00728      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
00729         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
00730                                                            =  ZEROELTCONGS
00731            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00732                                                            =  ZEROELTCONGS
00733               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCONGS
00734            ELSE                                                   ELTCONGS
00735               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCONGS
00736          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00737                                                  TO  WS-PERCENTAGEELTCONGS
00738         ELSE                                                      ELTCONGS
00739            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCONGS
00740          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00741                                                 TO  WS-PERCENTAGE.ELTCONGS
00742      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00743         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
00744                                                              ZERO ELTCONGS
00745         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00746         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONGS
00747         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCONGS
00748                                                    CMF-CODE-VALUE ELTCONGS
00749         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00750         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00751         PERFORM 2600-CODE-MANUAL-WITH-PERCENT.                    ELTCONGS
00752                                                                   ELTCONGS
00753      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
00754      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
00755         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCONGS
00756                                                               ZEROELTCONGS
00757            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00758                                                            =  ZEROELTCONGS
00759               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTCONGS
00760            ELSE                                                   ELTCONGS
00761               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCONGS
00762          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00763                                                  TO  WS-PERCENTAGEELTCONGS
00764         ELSE                                                      ELTCONGS
00765            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCONGS
00766          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
00767                                                 TO  WS-PERCENTAGE.ELTCONGS
00768      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00769         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
00770                                                              ZERO ELTCONGS
00771         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00772         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONGS
00773         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCONGS
00774                                                    CMF-CODE-VALUE ELTCONGS
00775         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
00776         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00777         PERFORM 2600-CODE-MANUAL-WITH-PERCENT.                    ELTCONGS
00778                                                                   ELTCONGS
00779      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
00780         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00781         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
00782         MOVE 1  TO  WS-CIA                                        ELTCONGS
00783         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00784             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
00785         END-EXEC.                                                 ELTCONGS
00786 **                                                               |ELTCONGS
00787 **---------------------------------------------------------------+ELTCONGS
00788                                                                   ELTCONGS
00789 **---------------------------------------------------------------+ELTCONGS
00790 **                                                               |ELTCONGS
00791 **                C I R C U M S T A N C E S                      |ELTCONGS
00792 **                         F O R                                 |ELTCONGS
00793 **                  E L I G I B I L I T Y                        |ELTCONGS
00794      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00795      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00796         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
00797                                                              '00' ELTCONGS
00798         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00799         MOVE WS-STANCES-FOR-ELIGIBILITY  TO                       ELTCONGS
00800                                           COF-DTL-LINE(WS-CIA)    ELTCONGS
00801         ADD +1  TO  WS-CIA.                                       ELTCONGS
00802                                                                   ELTCONGS
00803      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
00804      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00805         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
00806                                                          '00' AND ELTCONGS
00807         NOT WS-ADD-A-BLANK-IND                                    ELTCONGS
00808         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00809         MOVE WS-STANCES-FOR-ELIGIBILITY  TO                       ELTCONGS
00810                                           COF-DTL-LINE(WS-CIA)    ELTCONGS
00811         ADD +1  TO  WS-CIA.                                       ELTCONGS
00812                                                                   ELTCONGS
00813      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
00814      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00815         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
00816                                                              '00' ELTCONGS
00817         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00818         MOVE 'CONG-DFCT-SURG-PMT-ELG'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
00819         MOVE PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
00820                                                TO  CMF-CODE-VALUE ELTCONGS
00821         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00822         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00823         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00824                                                                   ELTCONGS
00825      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
00826      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00827         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
00828                                                              '00' ELTCONGS
00829         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00830         MOVE 'CONG-DFCT-SURG-PMT-ELG'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
00831         MOVE PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
00832                                                TO  CMF-CODE-VALUE ELTCONGS
00833         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
00834         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00835         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00836                                                                   ELTCONGS
00837      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
00838         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00839         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
00840         MOVE 1  TO  WS-CIA                                        ELTCONGS
00841         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00842             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
00843         END-EXEC.                                                 ELTCONGS
00844 **                                                               |ELTCONGS
00845 **---------------------------------------------------------------+ELTCONGS
00846                                                                   ELTCONGS
00847      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
00848         SET PLT-INDEX2  TO  2                                     ELTCONGS
00849      ELSE                                                         ELTCONGS
00850         SET PLT-INDEX2  TO  1.                                    ELTCONGS
00851                                                                   ELTCONGS
00852 **---------------------------------------------------------------+ELTCONGS
00853 **                                                               |ELTCONGS
00854 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTCONGS
00855      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
00856      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00857         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCONGS
00858                                                         NOT =  '0'ELTCONGS
00859         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00860         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00861         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTCONGS
00862                                           CMF-ELEMENT-SYSTEM-NAME ELTCONGS
00863         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTCONGS
00864                                                TO  CMF-CODE-VALUE ELTCONGS
00865         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00866         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00867 **                                                               |ELTCONGS
00868 **---------------------------------------------------------------+ELTCONGS
00869                                                                   ELTCONGS
00870 **---------------------------------------------------------------+ELTCONGS
00871 **                                                               |ELTCONGS
00872 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTCONGS
00873      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
00874         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCONGS
00875                                                         NOT =  '0'ELTCONGS
00876         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00877         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00878         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
00879         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
00880                                                 TO  CMF-CODE-VALUEELTCONGS
00881         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00882         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00883                                                                   ELTCONGS
00884      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
00885         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00886         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
00887         MOVE 1  TO  WS-CIA                                        ELTCONGS
00888         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00889                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00890         END-EXEC.                                                 ELTCONGS
00891 **                                                               |ELTCONGS
00892 **---------------------------------------------------------------+ELTCONGS
00893                                                                   ELTCONGS
00894      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
00895         SET PLT-INDEX2  TO  2                                     ELTCONGS
00896      ELSE                                                         ELTCONGS
00897         SET PLT-INDEX2  TO  1.                                    ELTCONGS
00898                                                                   ELTCONGS
00899 **---------------------------------------------------------------+ELTCONGS
00900 **                                                               |ELTCONGS
00901 **         TRANSFER TO OTHER RESPONSIBILITY INDICATOR            |ELTCONGS
00902                                                                   ELTCONGS
00903      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTCONGS
00904         ZERO                                                      ELTCONGS
00905         NEXT SENTENCE                                             ELTCONGS
00906      ELSE                                                         ELTCONGS
00907         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
00908         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00909         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTCONGS
00910         MOVE PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)    ELTCONGS
00911              TO  CMF-CODE-VALUE                                   ELTCONGS
00912         MOVE SPACES        TO  WS-TEMP-TEXT-AREA                  ELTCONGS
00913         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
00914 **                                                               |ELTCONGS
00915 **---------------------------------------------------------------+ELTCONGS
00916                                                                   ELTCONGS
00917      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
00918         SET PLT-INDEX2  TO  2                                     ELTCONGS
00919      ELSE                                                         ELTCONGS
00920         SET PLT-INDEX2  TO  1.                                    ELTCONGS
00921                                                                   ELTCONGS
00922                                                                   ELTCONGS
00923 **---------------------------------------------------------------+ELTCONGS
00924 **                                                               |ELTCONGS
00925 **         G E N E R A L   T A B U L A R   R T N E               |ELTCONGS
00926      PERFORM 2400-GENERAL-TABULAR-RTNE.                           ELTCONGS
00927 **                                                               |ELTCONGS
00928 **---------------------------------------------------------------+ELTCONGS
00929                                                                   ELTCONGS
00930 **---------------------------------------------------------------+ELTCONGS
00931 **                                                               |ELTCONGS
00932 **      P A Y M E N T  C O N S I D E R A T I O N  T E X T        |ELTCONGS
00933      INITIALIZE TCAR-FROM-AREA.                                   ELTCONGS
00934      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCONGS
00935             WS-PAY-CONSDR-TEXT2                                   ELTCONGS
00936                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCONGS
00937      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONGS
00938      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONGS
00939      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCONGS
00940                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCONGS
00941      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONGS
00942      IF WS-CIA > 17                                               ELTCONGS
00943         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTCONGS
00944         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
00945                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00946         END-EXEC                                                  ELTCONGS
00947         MOVE +1            TO WS-CIA.                             ELTCONGS
00948      ADD +1                TO  WS-CIA.                            ELTCONGS
00949      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONGS
00950      ADD +1                TO  WS-CIA.                            ELTCONGS
00951      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCONGS
00952      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTCONGS
00953      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONGS
00954                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00955      END-EXEC.                                                    ELTCONGS
00956      MOVE +1            TO WS-CIA.                                ELTCONGS
00957 **                                                               |ELTCONGS
00958 **---------------------------------------------------------------+ELTCONGS
00959  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTCONGS
00960                                                                   ELTCONGS
00961      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCONGS
00962         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
00963         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCONGS
00964         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCONGS
00965                                                    CMF-CODE-VALUE ELTCONGS
00966         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTCONGS
00967         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
00968         PERFORM 2100-CODES-MANUAL-LONG                            ELTCONGS
00969         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCONGS
00970         IF WS-CIA  >  20 OR  =  20                                ELTCONGS
00971            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTCONGS
00972            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCONGS
00973                COMMAREA(DFHCOMMAREA)                              ELTCONGS
00974            END-EXEC                                               ELTCONGS
00975            MOVE +1  TO  WS-CIA.                                   ELTCONGS
00976                                                                   ELTCONGS
00977  2090-PROBLEM-WITH-INDICES.                                       ELTCONGS
00978                                                                   ELTCONGS
00979      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCONGS
00980      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONGS
00981                                                                   ELTCONGS
00982      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONGS
00983      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONGS
00984                                                                   ELTCONGS
00985      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
00986      END-EXEC.                                                    ELTCONGS
00987                                                                   ELTCONGS
00988  2099-EXIT.            EXIT.                                      ELTCONGS
00989                                                                   ELTCONGS
00990 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTCONGS
00991  2100-CODES-MANUAL-LONG SECTION.                                  ELTCONGS
00992      MOVE '2100'  TO  WS-PARA-ID3.                                ELTCONGS
00993                                                                   ELTCONGS
00994      INITIALIZE CMF-RETURN-CODE,                                  ELTCONGS
00995                 TCAR-FROM-AREA.                                   ELTCONGS
00996                                                                   ELTCONGS
00997      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCONGS
00998      END-EXEC.                                                    ELTCONGS
00999                                                                   ELTCONGS
01000      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCONGS
01001      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01002          ADDRESS OF CMF-DESCR.                                    ELTCONGS
01003                                                                   ELTCONGS
01004      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTCONGS
01005         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTCONGS
01006         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTCONGS
01007            CMF-DESCR-LINE(1),        ' ',                         ELTCONGS
01008            CMF-DESCR-LINE(2),        ' ',                         ELTCONGS
01009            CMF-DESCR-LINE(3)                                      ELTCONGS
01010            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTCONGS
01011      ELSE                                                         ELTCONGS
01012         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTCONGS
01013         STRING CMF-DESCR-LINE(1),        ' ',                     ELTCONGS
01014            CMF-DESCR-LINE(2),        ' ',                         ELTCONGS
01015            CMF-DESCR-LINE(3)                                      ELTCONGS
01016            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTCONGS
01017      MOVE 'TCPR'  TO  WS-PARA-ID3.                                ELTCONGS
01018      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONGS
01019                                                                   ELTCONGS
01020      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTCONGS
01021      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTCONGS
01022      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTCONGS
01023      MOVE 'TCUN'  TO  WS-PARA-ID3.                                ELTCONGS
01024      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONGS
01025      MOVE '2100'  TO  WS-PARA-ID3.                                ELTCONGS
01026                                                                   ELTCONGS
01027      IF WS-MOVE-LINES-TO-CIA                                      ELTCONGS
01028         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTCONGS
01029            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTCONGS
01030                                             WS-TEMP-NOT-USED-CNT  ELTCONGS
01031            MOVE '2150'  TO  WS-PARA-ID3                           ELTCONGS
01032            PERFORM 2150-CONCATENATE-TO-TEMP-TEXT                  ELTCONGS
01033              VARYING WS-SUB1  FROM  1  BY  1                      ELTCONGS
01034              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTCONGS
01035            MOVE '2100'  TO  WS-PARA-ID3                           ELTCONGS
01036            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTCONGS
01037            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTCONGS
01038            ADD +1  TO  WS-CIA                                     ELTCONGS
01039         ELSE                                                      ELTCONGS
01040            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTCONGS
01041            ADD +1  TO  WS-CIA.                                    ELTCONGS
01042                                                                   ELTCONGS
01043      IF WS-MOVE-LINES-TO-CIA                                      ELTCONGS
01044         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTCONGS
01045            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTCONGS
01046            ADD +1  TO  WS-CIA                                     ELTCONGS
01047         ELSE                                                      ELTCONGS
01048            NEXT SENTENCE                                          ELTCONGS
01049      ELSE                                                         ELTCONGS
01050         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTCONGS
01051                                                                   ELTCONGS
01052      GO TO 2199-EXIT.                                             ELTCONGS
01053                                                                   ELTCONGS
01054  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTCONGS
01055      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTCONGS
01056      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTCONGS
01057                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTCONGS
01058                                                                   ELTCONGS
01059  2199-EXIT.           EXIT.                                       ELTCONGS
01060                                                                   ELTCONGS
01061 /        I N S T I T U T I O N A L   E L I G I B I L I T Y        ELTCONGS
01062 ***************************************************************** ELTCONGS
01063 *        I N S T I T U T I O N A L   E L I G I B I L I T Y        ELTCONGS
01064 *                                                                 ELTCONGS
01065 *                                                                 ELTCONGS
01066 ***************************************************************** ELTCONGS
01067  2200-INST-ELIGIBILITY SECTION.                                   ELTCONGS
01068      MOVE '2200'  TO  WS-PARA-ID2.                                ELTCONGS
01069                                                                   ELTCONGS
01070      PERFORM 2300-SET-CON-REC-ELSCONIB-PTR.                       ELTCONGS
01071      IF CIA-RC-PTR-NULL                                           ELTCONGS
01072         PERFORM 2310-SET-CON-REC-ELSCONIS-PTR                     ELTCONGS
01073         IF CIA-RC-PTR-NULL                                        ELTCONGS
01074            MOVE WS-CONGENITAL-NOT-COVERED                         ELTCONGS
01075            TO COF-DTL-LINE(WS-CIA)                                ELTCONGS
01076            ADD +1  TO  WS-CIA                                     ELTCONGS
01077            GO TO 2299-EXIT.                                       ELTCONGS
01078                                                                   ELTCONGS
01079      PERFORM 2300-SET-CON-REC-ELSCONIB-PTR.                       ELTCONGS
01080      IF NOT CIA-RC-PTR-NULL                                       ELTCONGS
01081         IF GCT-CONG-DFCT-SURG-PMT-EL-IND  =  ZERO                 ELTCONGS
01082            PERFORM 2310-SET-CON-REC-ELSCONIS-PTR                  ELTCONGS
01083            IF NOT CIA-RC-PTR-NULL                                 ELTCONGS
01084               IF GCT-CONG-DFCT-SURG-PMT-EL-IND  =  ZERO           ELTCONGS
01085                  MOVE WS-CONGENITAL-NOT-COVERED  TO               ELTCONGS
01086                                         COF-DTL-LINE(WS-CIA)      ELTCONGS
01087                  ADD +1  TO  WS-CIA                               ELTCONGS
01088                  GO TO 2299-EXIT                                  ELTCONGS
01089               ELSE                                                ELTCONGS
01090                  NEXT SENTENCE                                    ELTCONGS
01091            ELSE                                                   ELTCONGS
01092               MOVE WS-CONGENITAL-NOT-COVERED  TO                  ELTCONGS
01093                                         COF-DTL-LINE(WS-CIA)      ELTCONGS
01094               ADD +1  TO  WS-CIA                                  ELTCONGS
01095               GO TO 2299-EXIT.                                    ELTCONGS
01096                                                                   ELTCONGS
01097      PERFORM 2300-SET-CON-REC-ELSCONIB-PTR.                       ELTCONGS
01098      IF CIA-RC-PTR-NULL                                           ELTCONGS
01099         PERFORM 2310-SET-CON-REC-ELSCONIS-PTR                     ELTCONGS
01100         IF NOT CIA-RC-PTR-NULL                                    ELTCONGS
01101            IF GCT-CONG-DFCT-SURG-PMT-EL-IND  =  ZERO              ELTCONGS
01102               MOVE WS-CONGENITAL-NOT-COVERED                      ELTCONGS
01103               TO COF-DTL-LINE(WS-CIA)                             ELTCONGS
01104               ADD +1  TO  WS-CIA                                  ELTCONGS
01105               GO TO 2299-EXIT.                                    ELTCONGS
01106                                                                   ELTCONGS
01107                                                                   ELTCONGS
01108      MOVE WS-STANCES-FOR-ELIGIBILITY  TO  COF-DTL-LINE(WS-CIA).   ELTCONGS
01109      ADD +1  TO  WS-CIA.                                          ELTCONGS
01110      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTCONGS
01111      MOVE +63  TO  WS-TEMP-NOT-USED-CNT.                          ELTCONGS
01112      MOVE 'CONG-DFCT-SURG-PMT-EL-IND'  TO                         ELTCONGS
01113                                        CMF-ELEMENT-SYSTEM-NAME.   ELTCONGS
01114                                                                   ELTCONGS
01115      PERFORM 2300-SET-CON-REC-ELSCONIB-PTR.                       ELTCONGS
01116      IF NOT CIA-RC-PTR-NULL                                       ELTCONGS
01117         IF GCT-CONG-DFCT-SURG-PMT-EL-IND  NOT =  ZERO             ELTCONGS
01118            MOVE GCT-CONG-DFCT-SURG-PMT-EL-IND  TO  CMF-CODE-VALUE ELTCONGS
01119            MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA               ELTCONGS
01120            PERFORM 2100-CODES-MANUAL-LONG.                        ELTCONGS
01121                                                                   ELTCONGS
01122      PERFORM 2310-SET-CON-REC-ELSCONIS-PTR.                       ELTCONGS
01123      IF NOT CIA-RC-PTR-NULL                                       ELTCONGS
01124         IF GCT-CONG-DFCT-SURG-PMT-EL-IND  NOT =  ZERO             ELTCONGS
01125            MOVE GCT-CONG-DFCT-SURG-PMT-EL-IND  TO  CMF-CODE-VALUE ELTCONGS
01126            MOVE +63  TO  WS-TEMP-NOT-USED-CNT                     ELTCONGS
01127            MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                ELTCONGS
01128            PERFORM 2100-CODES-MANUAL-LONG.                        ELTCONGS
01129                                                                   ELTCONGS
01130  2280-ADD-A-LINE-OUTPUT.                                          ELTCONGS
01131      MOVE '2280'  TO  WS-PARA-ID2.                                ELTCONGS
01132                                                                   ELTCONGS
01133      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTCONGS
01134      MOVE 1  TO  WS-CIA.                                          ELTCONGS
01135      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONGS
01136             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01137      END-EXEC.                                                    ELTCONGS
01138                                                                   ELTCONGS
01139  2299-EXIT.           EXIT.                                       ELTCONGS
01140                                                                   ELTCONGS
01141  2300-SET-CON-REC-ELSCONIB-PTR.                                   ELTCONGS
01142      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTCONGS
01143      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01144          ADDRESS OF CONTRACT-RECORD.                              ELTCONGS
01145                                                                   ELTCONGS
01146  2310-SET-CON-REC-ELSCONIS-PTR.                                   ELTCONGS
01147      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTCONGS
01148      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01149          ADDRESS OF CONTRACT-RECORD.                              ELTCONGS
01150                                                                   ELTCONGS
01151 /     G E N E R A L   G E T   T A B U L A R   R T N E             ELTCONGS
01152  2400-GENERAL-TABULAR-RTNE SECTION.                               ELTCONGS
01153      MOVE '2400'  TO  WS-PARA-ID2.                                ELTCONGS
01154                                                                   ELTCONGS
01155 **---------------------------------------------------------------+ELTCONGS
01156 **                                                               |ELTCONGS
01157 **                  # A A R   T A B U L A R   F O U N D          |ELTCONGS
01158      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01159      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01160         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01161                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01162         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01163         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTCONGS
01164         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTCONGS
01165      ELSE                                                         ELTCONGS
01166         SET PLT-INDEX2  TO  2                                     ELTCONGS
01167         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTCONGS
01168            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTCONGS
01169                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01170            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTCONGS
01171            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTCONGS
01172            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTCONGS
01173                                                                   ELTCONGS
01174      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01175         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01176         MOVE 1  TO  WS-CIA                                        ELTCONGS
01177         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01178             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01179         END-EXEC.                                                 ELTCONGS
01180 **                                                               |ELTCONGS
01181 **---------------------------------------------------------------+ELTCONGS
01182                                                                   ELTCONGS
01183 **---------------------------------------------------------------+ELTCONGS
01184 **                                                               |ELTCONGS
01185 **                  # P P F   T A B U L A R                      |ELTCONGS
01186      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01187      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01188         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01189                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01190         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01191                                                   KWA-GCTABULR-KEYELTCONGS
01192         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01193         IF IOP-RC-OK                                              ELTCONGS
01194            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTCONGS
01195                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01196            END-EXEC.                                              ELTCONGS
01197                                                                   ELTCONGS
01198      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01199      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01200         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01201                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTCONGS
01202         KWA-PROVISION-ID = '#PPF  ' AND                           ELTCONGS
01203         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01204                                                  KWA-GCTABULR-KEY ELTCONGS
01205         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01206                                                   KWA-GCTABULR-KEYELTCONGS
01207         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01208         IF IOP-RC-OK                                              ELTCONGS
01209            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTCONGS
01210                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01211            END-EXEC.                                              ELTCONGS
01212      MOVE SPACES  TO  KWA-PROVISION-ID.                           ELTCONGS
01213 **                                                               |ELTCONGS
01214 **---------------------------------------------------------------+ELTCONGS
01215                                                                   ELTCONGS
01216 **---------------------------------------------------------------+ELTCONGS
01217 **                                                               |ELTCONGS
01218 **                  # P V E   T A B U L A R                      |ELTCONGS
01219      MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                            ELTCONGS
01220      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONGS
01221      MOVE WS-PROVIDER-ELIGIBILITY  TO  COF-DTL-LINE(1).           ELTCONGS
01222                                                                   ELTCONGS
01223      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01224         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01225         MOVE 1  TO  WS-CIA                                        ELTCONGS
01226         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01227             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01228         END-EXEC.                                                 ELTCONGS
01229 **                                                               |ELTCONGS
01230 **---------------------------------------------------------------+ELTCONGS
01231                                                                   ELTCONGS
01232 **---------------------------------------------------------------+ELTCONGS
01233 **                                                               |ELTCONGS
01234 **                  # A B M   T A B U L A R                      |ELTCONGS
01235      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01236      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01237         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01238                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01239         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01240                                                 KWA-GCTABULR-KEY  ELTCONGS
01241         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01242         IF IOP-RC-OK                                              ELTCONGS
01243            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTCONGS
01244                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01245            END-EXEC.                                              ELTCONGS
01246                                                                   ELTCONGS
01247      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01248      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01249         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01250                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTCONGS
01251         KWA-PROVISION-ID = '#ABM  ' AND                           ELTCONGS
01252         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01253                                                 KWA-GCTABULR-KEY  ELTCONGS
01254         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01255                                                 KWA-GCTABULR-KEY  ELTCONGS
01256         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01257         IF IOP-RC-OK                                              ELTCONGS
01258            EXEC  CICS  LINK  PROGRAM('ELFMABM')                   ELTCONGS
01259                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01260            END-EXEC.                                              ELTCONGS
01261      MOVE SPACES  TO  KWA-PROVISION-ID.                           ELTCONGS
01262 **                                                               |ELTCONGS
01263 **---------------------------------------------------------------+ELTCONGS
01264                                                                   ELTCONGS
01265 **---------------------------------------------------------------+ELTCONGS
01266 **                                                               |ELTCONGS
01267 **                  # A C L   T A B U L A R                      |ELTCONGS
01268      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01269      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01270         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01271                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01272         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01273                                                   KWA-GCTABULR-KEYELTCONGS
01274         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01275         IF IOP-RC-OK                                              ELTCONGS
01276            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTCONGS
01277                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01278            END-EXEC.                                              ELTCONGS
01279                                                                   ELTCONGS
01280      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01281      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01282         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01283                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTCONGS
01284         KWA-PROVISION-ID = '#ACL  ' AND                           ELTCONGS
01285         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01286                                                   KWA-GCTABULR-KEYELTCONGS
01287         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01288                                                   KWA-GCTABULR-KEYELTCONGS
01289         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01290         IF IOP-RC-OK                                              ELTCONGS
01291            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTCONGS
01292                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01293            END-EXEC.                                              ELTCONGS
01294      MOVE SPACES  TO  KWA-PROVISION-ID.                           ELTCONGS
01295 **                                                               |ELTCONGS
01296 **---------------------------------------------------------------+ELTCONGS
01297                                                                   ELTCONGS
01298 **---------------------------------------------------------------+ELTCONGS
01299 **                                                               |ELTCONGS
01300 **                  # A D L   T A B U L A R                      |ELTCONGS
01301      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01302      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01303         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01304                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01305         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01306                                                   KWA-GCTABULR-KEYELTCONGS
01307         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01308         IF IOP-RC-OK                                              ELTCONGS
01309            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTCONGS
01310                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01311            END-EXEC.                                              ELTCONGS
01312                                                                   ELTCONGS
01313      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01314      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01315         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01316                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTCONGS
01317         KWA-PROVISION-ID = '#ADL  ' AND                           ELTCONGS
01318         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01319                                                   KWA-GCTABULR-KEYELTCONGS
01320         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01321                                                   KWA-GCTABULR-KEYELTCONGS
01322         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01323         IF IOP-RC-OK                                              ELTCONGS
01324            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTCONGS
01325                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01326            END-EXEC.                                              ELTCONGS
01327      MOVE SPACES  TO  KWA-PROVISION-ID.                           ELTCONGS
01328 **                                                               |ELTCONGS
01329 **---------------------------------------------------------------+ELTCONGS
01330                                                                   ELTCONGS
01331 **---------------------------------------------------------------+ELTCONGS
01332 **                                                               |ELTCONGS
01333 **                  # A O L   T A B U L A R                      |ELTCONGS
01334      SET PLT-INDEX2  TO  1.                                       ELTCONGS
01335      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01336         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01337                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTCONGS
01338         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01339                                                   KWA-GCTABULR-KEYELTCONGS
01340         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01341         IF IOP-RC-OK                                              ELTCONGS
01342            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTCONGS
01343                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01344            END-EXEC.                                              ELTCONGS
01345                                                                   ELTCONGS
01346      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01347      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01348         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01349                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTCONGS
01350         KWA-PROVISION-ID = '#AOL  ' AND                           ELTCONGS
01351         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01352                                                   KWA-GCTABULR-KEYELTCONGS
01353         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTCONGS
01354                                                   KWA-GCTABULR-KEYELTCONGS
01355         PERFORM 2500-GET-TABULAR-RECORD                           ELTCONGS
01356         IF IOP-RC-OK                                              ELTCONGS
01357            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTCONGS
01358                 COMMAREA(DFHCOMMAREA)                             ELTCONGS
01359            END-EXEC.                                              ELTCONGS
01360      MOVE SPACES  TO  KWA-PROVISION-ID.                           ELTCONGS
01361 **                                                               |ELTCONGS
01362 **---------------------------------------------------------------+ELTCONGS
01363                                                                   ELTCONGS
01364  2499-EXIT.          EXIT.                                        ELTCONGS
01365                                                                   ELTCONGS
01366 /            G E T   T A B U L A R   R E C O R D                  ELTCONGS
01367 ***************************************************************** ELTCONGS
01368 *            G E T   T A B U L A R   R E C O R D                  ELTCONGS
01369 *                                                                 ELTCONGS
01370 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTCONGS
01371 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTCONGS
01372 *  TO DISPLAY.                                                    ELTCONGS
01373 *                                                                 ELTCONGS
01374 ***************************************************************** ELTCONGS
01375  2500-GET-TABULAR-RECORD SECTION.                                 ELTCONGS
01376      MOVE '2500'  TO  WS-PARA-ID3.                                ELTCONGS
01377                                                                   ELTCONGS
01378      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCONGS
01379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01380          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTCONGS
01381      SET  CIA-GCTABULR-DDN TO TRUE.                               ELTCONGS
01382      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTCONGS
01383      SET IOP-RD            TO TRUE.                               ELTCONGS
01384      SET IOP-FCQ-NONE      TO TRUE.                               ELTCONGS
01385      SET IOP-KVQ-NONE      TO TRUE.                               ELTCONGS
01386      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTCONGS
01387             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01388      END-EXEC.                                                    ELTCONGS
01389                                                                   ELTCONGS
01390      IF IOP-RC-NOTFND                                             ELTCONGS
01391         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTCONGS
01392         EXEC CICS ABEND                                           ELTCONGS
01393                   ABCODE(CIA-ABCODE)                              ELTCONGS
01394         END-EXEC.                                                 ELTCONGS
01395                                                                   ELTCONGS
01396      IF NOT IOP-RC-OK                                             ELTCONGS
01397         SET CIA-AB-CRITIO          TO TRUE                        ELTCONGS
01398         EXEC CICS ABEND                                           ELTCONGS
01399                   ABCODE(CIA-ABCODE)                              ELTCONGS
01400         END-EXEC.                                                 ELTCONGS
01401                                                                   ELTCONGS
01402  2599-EXIT.           EXIT.                                       ELTCONGS
01403                                                                   ELTCONGS
01404 /    C O D E S   M A N U A L   W I T H   P E R C E N T A G E      ELTCONGS
01405  2600-CODE-MANUAL-WITH-PERCENT SECTION.                           ELTCONGS
01406      MOVE '2600'  TO  WS-PARA-ID2.                                ELTCONGS
01407                                                                   ELTCONGS
01408      INITIALIZE CMF-RETURN-CODE,                                  ELTCONGS
01409                 TCAR-FROM-AREA.                                   ELTCONGS
01410                                                                   ELTCONGS
01411      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTCONGS
01412      END-EXEC.                                                    ELTCONGS
01413                                                                   ELTCONGS
01414      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCONGS
01415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01416          ADDRESS OF CMF-DESCR.                                    ELTCONGS
01417                                                                   ELTCONGS
01418      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTCONGS
01419         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTCONGS
01420         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTCONGS
01421            CMF-DESCR-LINE(1),        ' ',                         ELTCONGS
01422            CMF-DESCR-LINE(2),        ' ',                         ELTCONGS
01423            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTCONGS
01424            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTCONGS
01425      ELSE                                                         ELTCONGS
01426         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTCONGS
01427         STRING CMF-DESCR-LINE(1),        ' ',                     ELTCONGS
01428            CMF-DESCR-LINE(2),        ' ',                         ELTCONGS
01429            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTCONGS
01430            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTCONGS
01431      MOVE 'TCPR'  TO  WS-PARA-ID3.                                ELTCONGS
01432      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONGS
01433                                                                   ELTCONGS
01434      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTCONGS
01435      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTCONGS
01436      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTCONGS
01437                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTCONGS
01438                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTCONGS
01439      MOVE 'TCUN'  TO  WS-PARA-ID3.                                ELTCONGS
01440      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONGS
01441                                                                   ELTCONGS
01442      IF WS-MOVE-LINES-TO-CIA                                      ELTCONGS
01443         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTCONGS
01444            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTCONGS
01445                                             WS-TEMP-NOT-USED-CNT  ELTCONGS
01446            MOVE '2650'  TO  WS-PARA-ID2                           ELTCONGS
01447            PERFORM 2650-CONCATENATE-TO-TEMP-TEXT                  ELTCONGS
01448              VARYING WS-SUB1  FROM  1  BY  1                      ELTCONGS
01449              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTCONGS
01450            MOVE '2600'  TO  WS-PARA-ID2                           ELTCONGS
01451            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTCONGS
01452            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTCONGS
01453            ADD +1  TO  WS-CIA                                     ELTCONGS
01454         ELSE                                                      ELTCONGS
01455            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTCONGS
01456            ADD +1  TO  WS-CIA.                                    ELTCONGS
01457                                                                   ELTCONGS
01458      IF WS-MOVE-LINES-TO-CIA                                      ELTCONGS
01459         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTCONGS
01460            MOVE '2660'  TO  WS-PARA-ID2                           ELTCONGS
01461            PERFORM 2660-MOVE-LINES-TO-CIA                         ELTCONGS
01462               VARYING WS-SUB1  FROM 2  BY  1                      ELTCONGS
01463               UNTIL WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED           ELTCONGS
01464         ELSE                                                      ELTCONGS
01465            NEXT SENTENCE                                          ELTCONGS
01466      ELSE                                                         ELTCONGS
01467         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTCONGS
01468                                                                   ELTCONGS
01469      GO TO 2699-EXIT.                                             ELTCONGS
01470                                                                   ELTCONGS
01471  2650-CONCATENATE-TO-TEMP-TEXT.                                   ELTCONGS
01472      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTCONGS
01473      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTCONGS
01474                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTCONGS
01475                                                                   ELTCONGS
01476  2660-MOVE-LINES-TO-CIA.                                          ELTCONGS
01477      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTCONGS
01478      ADD +1  TO  WS-CIA.                                          ELTCONGS
01479                                                                   ELTCONGS
01480  2699-EXIT.           EXIT.                                       ELTCONGS
01481                                                                   ELTCONGS
01482                                                                   ELTCONGS
01483 /            P R O F E S S I O N A L   O P   R T N E              ELTCONGS
01484 ***************************************************************** ELTCONGS
01485 *            P R O F E S S I O N A L   O P   R T N E              ELTCONGS
01486 *                                                                 ELTCONGS
01487 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTCONGS
01488 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTCONGS
01489 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTCONGS
01490 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTCONGS
01491 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTCONGS
01492 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTCONGS
01493 *  MODULE.                                                        ELTCONGS
01494 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTCONGS
01495 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTCONGS
01496 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTCONGS
01497 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTCONGS
01498 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTCONGS
01499 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTCONGS
01500 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTCONGS
01501 *                                                                 ELTCONGS
01502 ***************************************************************** ELTCONGS
01503  3000-PROFESSIONAL-OP-RTNE SECTION.                               ELTCONGS
01504      MOVE '3000'  TO  WS-PARA-ID1.                                ELTCONGS
01505                                                                   ELTCONGS
01506      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTCONGS
01507      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONGS
01508      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTCONGS
01509                     COF-NBR-DTL-LINES.                            ELTCONGS
01510      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
01511      END-EXEC.                                                    ELTCONGS
01512                                                                   ELTCONGS
01513      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONGS
01514      MOVE WS-HDR-2-PROF-OUTPAT  TO  COF-HDR-LINE(2).              ELTCONGS
01515                                                                   ELTCONGS
01516      MOVE WS-PROF-OUTPAT-CNT  TO  PVN-NBR-BEN-PROVN.              ELTCONGS
01517      PERFORM 3010-MOVE-IN-PROF-OP                                 ELTCONGS
01518         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCONGS
01519         UNTIL WS-SUB  >  WS-PROF-OUTPAT-CNT.                      ELTCONGS
01520                                                                   ELTCONGS
01521      GO TO 3020-CALL-COVERAGE.                                    ELTCONGS
01522  3010-MOVE-IN-PROF-OP.                                            ELTCONGS
01523      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCONGS
01524      MOVE WS-PROF-OUTPAT-LIST(WS-SUB)  TO                         ELTCONGS
01525                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCONGS
01526      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTCONGS
01527                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTCONGS
01528                                                                   ELTCONGS
01529  3020-CALL-COVERAGE.                                              ELTCONGS
01530      MOVE '3020'  TO  WS-PARA-ID1.                                ELTCONGS
01531                                                                   ELTCONGS
01532      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONGS
01533      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
01534      END-EXEC.                                                    ELTCONGS
01535                                                                   ELTCONGS
01536      MOVE WS-CONGENITAL-SURGERY-IS  TO  SSB-TOPIC-PHRASE.         ELTCONGS
01537                                                                   ELTCONGS
01538      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCONGS
01539      END-EXEC.                                                    ELTCONGS
01540                                                                   ELTCONGS
01541      ADD +1  TO   COF-NBR-DTL-LINES.                              ELTCONGS
01542      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
01543      END-EXEC.                                                    ELTCONGS
01544                                                                   ELTCONGS
01545      IF PVN-COVG-NONE                                             ELTCONGS
01546         GO TO 3099-EXIT.                                          ELTCONGS
01547                                                                   ELTCONGS
01548      MOVE +1  TO  WS-CIA.                                         ELTCONGS
01549                                                                   ELTCONGS
01550      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCONGS
01551                                                                   ELTCONGS
01552      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTCONGS
01553            PSP-PROVN-PRICING-METHD,                               ELTCONGS
01554            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCONGS
01555            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONGS
01556            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONGS
01557            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCONGS
01558            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCONGS
01559            PSP-SPILL-OVER-DED-APL-IND,                            ELTCONGS
01560            PSP-CONG-DFCT-SURG-PMT-ELG,                            ELTCONGS
01561            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCONGS
01562            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCONGS
01563            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCONGS
01564            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCONGS
01565            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCONGS
01566            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCONGS
01567            PSC-BEN-SCOPE-ID.                                      ELTCONGS
01568                                                                   ELTCONGS
01569      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCONGS
01570      END-EXEC.                                                    ELTCONGS
01571                                                                   ELTCONGS
01572      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCONGS
01573      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONGS
01574          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCONGS
01575                                                                   ELTCONGS
01576      MOVE '3040'  TO  WS-PARA-ID1.                                ELTCONGS
01577      PERFORM 3030-FIND-FIRST-NONZERO                              ELTCONGS
01578         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCONGS
01579         UNTIL WS-SUB  >  WS-PROF-OUTPAT-CNT.                      ELTCONGS
01580                                                                   ELTCONGS
01581      GO TO 3099-EXIT.                                             ELTCONGS
01582  3030-FIND-FIRST-NONZERO.                                         ELTCONGS
01583      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONGS
01584      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCONGS
01585         NEXT SENTENCE                                             ELTCONGS
01586      ELSE                                                         ELTCONGS
01587         PERFORM 3040-BUILD-SCREEN-LINES.                          ELTCONGS
01588                                                                   ELTCONGS
01589  3040-BUILD-SCREEN-LINES.                                         ELTCONGS
01590                                                                   ELTCONGS
01591      SET PLT-INDEX1   TO                                          ELTCONGS
01592                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTCONGS
01593      IF WS-NOT-FIRST-TIME                                         ELTCONGS
01594         MOVE 'P'  TO  COF-FUNCTION                                ELTCONGS
01595         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01596             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01597         END-EXEC                                                  ELTCONGS
01598      ELSE                                                         ELTCONGS
01599         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTCONGS
01600                                                                   ELTCONGS
01601      MOVE +1  TO  WS-CIA.                                         ELTCONGS
01602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01603         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTCONGS
01604            SET PLT-INDEX2  TO  2                                  ELTCONGS
01605         ELSE                                                      ELTCONGS
01606            PERFORM 3090-PROBLEM-WITH-INDICES                      ELTCONGS
01607            GO TO 3099-EXIT                                        ELTCONGS
01608      ELSE                                                         ELTCONGS
01609         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01610                                                                   ELTCONGS
01611 **---------------------------------------------------------------+ELTCONGS
01612 **                                                               |ELTCONGS
01613 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTCONGS
01614      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTCONGS
01615      ADD  +1  TO  WS-CIA.                                         ELTCONGS
01616      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCONGS
01617      PERFORM 3050-ZERO-ALL-WITH-SAME-NO                           ELTCONGS
01618         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTCONGS
01619         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-OUTPAT-CNT.            ELTCONGS
01620      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONGS
01621                                                                   ELTCONGS
01622      ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTCONGS
01623      MOVE 1  TO  WS-CIA.                                          ELTCONGS
01624      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONGS
01625             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01626      END-EXEC.                                                    ELTCONGS
01627 **                                                               |ELTCONGS
01628 **---------------------------------------------------------------+ELTCONGS
01629                                                                   ELTCONGS
01630                                                                   ELTCONGS
01631                                                                   ELTCONGS
01632      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01633         SET PLT-INDEX2  TO  2                                     ELTCONGS
01634      ELSE                                                         ELTCONGS
01635         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01636                                                                   ELTCONGS
01637 **---------------------------------------------------------------+ELTCONGS
01638 **                                                               |ELTCONGS
01639 **        P L A C E   O F   T R E A T M E N T                    |ELTCONGS
01640      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTCONGS
01641                                                              ZERO ELTCONGS
01642         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01643         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTCONGS
01644         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTCONGS
01645                                               TO  CMF-CODE-VALUE  ELTCONGS
01646         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01647         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTCONGS
01648         PERFORM 2100-CODES-MANUAL-LONG                            ELTCONGS
01649         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
01650         MOVE +1  TO  WS-CIA                                       ELTCONGS
01651         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01652               COMMAREA(DFHCOMMAREA)                               ELTCONGS
01653         END-EXEC.                                                 ELTCONGS
01654 **                                                               |ELTCONGS
01655 **---------------------------------------------------------------+ELTCONGS
01656                                                                   ELTCONGS
01657      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01658         SET PLT-INDEX2  TO  2                                     ELTCONGS
01659      ELSE                                                         ELTCONGS
01660         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01661                                                                   ELTCONGS
01662 **---------------------------------------------------------------+ELTCONGS
01663 **                                                               |ELTCONGS
01664 **            B E N E F I T   S C O P E   I D                    |ELTCONGS
01665      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01666      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01667         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
01668                                       '0000' AND  NOT =  '00  '   ELTCONGS
01669         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTCONGS
01670         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01671         ADD  +1  TO  WS-CIA.                                      ELTCONGS
01672                                                                   ELTCONGS
01673      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01674      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01675         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
01676                                   '0000' AND  NOT =  '00  ' AND   ELTCONGS
01677         NOT WS-ADD-A-BLANK-LINE                                   ELTCONGS
01678         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTCONGS
01679         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01680         ADD  +1  TO  WS-CIA.                                      ELTCONGS
01681                                                                   ELTCONGS
01682      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01683      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01684         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
01685                                       '0000' AND  NOT =  '00  '   ELTCONGS
01686         MOVE 'BPC'  TO  CMF-RECORD-PREFIX                         ELTCONGS
01687         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTCONGS
01688         MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTCONGS
01689                                                    CMF-CODE-VALUE ELTCONGS
01690         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01691         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01692         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01693                                                                   ELTCONGS
01694      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01695      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01696         PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTCONGS
01697                                       '0000' AND  NOT =  '00  '   ELTCONGS
01698         MOVE 'BPC'  TO  CMF-RECORD-PREFIX                         ELTCONGS
01699         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTCONGS
01700         MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTCONGS
01701                                                    CMF-CODE-VALUE ELTCONGS
01702         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
01703         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01704         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01705                                                                   ELTCONGS
01706      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01707         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01708         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
01709         MOVE 1  TO  WS-CIA                                        ELTCONGS
01710         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01711                COMMAREA(DFHCOMMAREA)                              ELTCONGS
01712         END-EXEC.                                                 ELTCONGS
01713 **                                                               |ELTCONGS
01714 **---------------------------------------------------------------+ELTCONGS
01715                                                                   ELTCONGS
01716      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01717         SET PLT-INDEX2  TO  2                                     ELTCONGS
01718      ELSE                                                         ELTCONGS
01719         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01720                                                                   ELTCONGS
01721 **---------------------------------------------------------------+ELTCONGS
01722 **                                                               |ELTCONGS
01723 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTCONGS
01724 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTCONGS
01725 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTCONGS
01726      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01727      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01728         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
01729                                           ZERO AND  NOT =  '19'   ELTCONGS
01730         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCONGS
01731         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01732         ADD +1  TO  WS-CIA.                                       ELTCONGS
01733                                                                   ELTCONGS
01734      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01735      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01736         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
01737                                        ZERO AND  NOT =  '19' AND  ELTCONGS
01738         NOT WS-ADD-A-BLANK-LINE                                   ELTCONGS
01739         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTCONGS
01740         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01741         ADD +1  TO  WS-CIA.                                       ELTCONGS
01742                                                                   ELTCONGS
01743      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01744      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01745         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCONGS
01746                             AND                                   ELTCONGS
01747         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
01748         SET  PLT-INDEX2  TO  2                                    ELTCONGS
01749         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCONGS
01750                                                              ZERO ELTCONGS
01751            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCONGS
01752            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCONGS
01753            ADD +1  TO  WS-CIA.                                    ELTCONGS
01754                                                                   ELTCONGS
01755      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01756      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01757         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTCONGS
01758                               AND                                 ELTCONGS
01759         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTCONGS
01760         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTCONGS
01761         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTCONGS
01762         ADD +1  TO  WS-CIA.                                       ELTCONGS
01763                                                                   ELTCONGS
01764      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTCONGS
01765         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
01766         SET  PLT-INDEX2  TO  2                                    ELTCONGS
01767         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTCONGS
01768                                                              ZERO ELTCONGS
01769            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTCONGS
01770            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTCONGS
01771            ADD +1  TO  WS-CIA.                                    ELTCONGS
01772                                                                   ELTCONGS
01773      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01774      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
01775         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
01776                                                            =  ZEROELTCONGS
01777            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01778                                                            =  ZEROELTCONGS
01779               MOVE SPACE  TO  WS-PERCENT-FLD                      ELTCONGS
01780            ELSE                                                   ELTCONGS
01781               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCONGS
01782          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01783                                                  TO  WS-PERCENTAGEELTCONGS
01784         ELSE                                                      ELTCONGS
01785            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCONGS
01786          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01787                                                 TO  WS-PERCENTAGE.ELTCONGS
01788      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01789         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
01790                                                              ZERO ELTCONGS
01791         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01792         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONGS
01793         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCONGS
01794                                                    CMF-CODE-VALUE ELTCONGS
01795         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01796         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01797         PERFORM 2600-CODE-MANUAL-WITH-PERCENT.                    ELTCONGS
01798                                                                   ELTCONGS
01799      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01800      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONGS
01801         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTCONGS
01802                                                               ZEROELTCONGS
01803            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01804                                                            =  ZEROELTCONGS
01805               MOVE SPACE  TO  WS-PERCENT-FLD                      ELTCONGS
01806            ELSE                                                   ELTCONGS
01807               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTCONGS
01808          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01809                                                  TO  WS-PERCENTAGEELTCONGS
01810         ELSE                                                      ELTCONGS
01811            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTCONGS
01812          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONGS
01813                                                 TO  WS-PERCENTAGE.ELTCONGS
01814      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01815         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTCONGS
01816                                                              ZERO ELTCONGS
01817         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01818         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONGS
01819         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTCONGS
01820                                                    CMF-CODE-VALUE ELTCONGS
01821         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
01822         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01823         PERFORM 2600-CODE-MANUAL-WITH-PERCENT.                    ELTCONGS
01824                                                                   ELTCONGS
01825      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01826         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01827         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
01828         MOVE 1  TO  WS-CIA                                        ELTCONGS
01829         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01830                COMMAREA(DFHCOMMAREA)                              ELTCONGS
01831         END-EXEC.                                                 ELTCONGS
01832 **                                                               |ELTCONGS
01833 **---------------------------------------------------------------+ELTCONGS
01834                                                                   ELTCONGS
01835 **---------------------------------------------------------------+ELTCONGS
01836 **                                                               |ELTCONGS
01837 **                C I R C U M S T A N C E S                      |ELTCONGS
01838 **                         F O R                                 |ELTCONGS
01839 **                  E L I G I B I L I T Y                        |ELTCONGS
01840      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01841      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01842         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
01843                                                              '00' ELTCONGS
01844         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01845         MOVE WS-STANCES-FOR-ELIGIBILITY  TO                       ELTCONGS
01846                                           COF-DTL-LINE(WS-CIA)    ELTCONGS
01847         ADD +1  TO  WS-CIA.                                       ELTCONGS
01848                                                                   ELTCONGS
01849      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01850      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01851         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
01852                                                          '00' AND ELTCONGS
01853         NOT WS-ADD-A-BLANK-IND                                    ELTCONGS
01854         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01855         MOVE WS-STANCES-FOR-ELIGIBILITY  TO                       ELTCONGS
01856                                           COF-DTL-LINE(WS-CIA)    ELTCONGS
01857         ADD +1  TO  WS-CIA.                                       ELTCONGS
01858                                                                   ELTCONGS
01859      SET  PLT-INDEX2  TO  1.                                      ELTCONGS
01860      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01861         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
01862                                                              '00' ELTCONGS
01863         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01864         MOVE 'CONG-DFCT-SURG-PMT-ELG'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
01865         MOVE PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
01866                                                TO  CMF-CODE-VALUE ELTCONGS
01867         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01868         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01869         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01870                                                                   ELTCONGS
01871      SET  PLT-INDEX2  TO  2.                                      ELTCONGS
01872      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTCONGS
01873         PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)  NOT = ELTCONGS
01874                                                              '00' ELTCONGS
01875         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01876         MOVE 'CONG-DFCT-SURG-PMT-ELG'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
01877         MOVE PLP-CONG-DFCT-SURG-PMT-ELG(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
01878                                                TO  CMF-CODE-VALUE ELTCONGS
01879         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTCONGS
01880         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
01881         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01882                                                                   ELTCONGS
01883      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01884         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01885         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
01886         MOVE 1  TO  WS-CIA                                        ELTCONGS
01887         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01888             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01889         END-EXEC.                                                 ELTCONGS
01890 **                                                               |ELTCONGS
01891 **---------------------------------------------------------------+ELTCONGS
01892                                                                   ELTCONGS
01893                                                                   ELTCONGS
01894 **---------------------------------------------------------------+ELTCONGS
01895 **                                                               |ELTCONGS
01896 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTCONGS
01897      SET PLT-INDEX2  TO  2.                                       ELTCONGS
01898      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES AND      ELTCONGS
01899         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCONGS
01900                                                         NOT =  '0'ELTCONGS
01901         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01902         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01903         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTCONGS
01904                                           CMF-ELEMENT-SYSTEM-NAME ELTCONGS
01905         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTCONGS
01906                                                TO  CMF-CODE-VALUE ELTCONGS
01907         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01908         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01909 **                                                               |ELTCONGS
01910 **---------------------------------------------------------------+ELTCONGS
01911                                                                   ELTCONGS
01912 **---------------------------------------------------------------+ELTCONGS
01913 **                                                               |ELTCONGS
01914 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTCONGS
01915      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES AND      ELTCONGS
01916         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCONGS
01917                                                         NOT =  '0'ELTCONGS
01918         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01919         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01920         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTCONGS
01921         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTCONGS
01922                                                 TO  CMF-CODE-VALUEELTCONGS
01923         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01924         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01925                                                                   ELTCONGS
01926      IF WS-ADD-A-BLANK-LINE                                       ELTCONGS
01927         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01928         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTCONGS
01929         MOVE 1  TO  WS-CIA                                        ELTCONGS
01930         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01931             COMMAREA(DFHCOMMAREA)                                 ELTCONGS
01932         END-EXEC.                                                 ELTCONGS
01933 **                                                               |ELTCONGS
01934 **---------------------------------------------------------------+ELTCONGS
01935                                                                   ELTCONGS
01936      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01937         SET PLT-INDEX2  TO  2                                     ELTCONGS
01938      ELSE                                                         ELTCONGS
01939         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01940                                                                   ELTCONGS
01941 **---------------------------------------------------------------+ELTCONGS
01942 **                                                               |ELTCONGS
01943 **         TRANSFER TO OTHER RESPONSIBILITY INDICATOR            |ELTCONGS
01944                                                                   ELTCONGS
01945      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTCONGS
01946         ZERO                                                      ELTCONGS
01947         NEXT SENTENCE                                             ELTCONGS
01948      ELSE                                                         ELTCONGS
01949         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTCONGS
01950         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
01951         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTCONGS
01952         MOVE PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)    ELTCONGS
01953              TO  CMF-CODE-VALUE                                   ELTCONGS
01954         MOVE SPACES        TO  WS-TEMP-TEXT-AREA                  ELTCONGS
01955         PERFORM 2100-CODES-MANUAL-LONG.                           ELTCONGS
01956 **                                                               |ELTCONGS
01957 **---------------------------------------------------------------+ELTCONGS
01958                                                                   ELTCONGS
01959                                                                   ELTCONGS
01960      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONGS
01961         SET PLT-INDEX2  TO  2                                     ELTCONGS
01962      ELSE                                                         ELTCONGS
01963         SET PLT-INDEX2  TO  1.                                    ELTCONGS
01964                                                                   ELTCONGS
01965                                                                   ELTCONGS
01966                                                                   ELTCONGS
01967 **---------------------------------------------------------------+ELTCONGS
01968 **                                                               |ELTCONGS
01969 **         G E N E R A L   T A B U L A R   R T N E               |ELTCONGS
01970      PERFORM 2400-GENERAL-TABULAR-RTNE.                           ELTCONGS
01971 **                                                               |ELTCONGS
01972 **---------------------------------------------------------------+ELTCONGS
01973                                                                   ELTCONGS
01974 **---------------------------------------------------------------+ELTCONGS
01975 **                                                               |ELTCONGS
01976 **      P A Y M E N T  C O N S I D E R A T I O N  T E X T        |ELTCONGS
01977      INITIALIZE TCAR-FROM-AREA.                                   ELTCONGS
01978      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCONGS
01979             WS-PAY-CONSDR-TEXT2                                   ELTCONGS
01980                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCONGS
01981      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONGS
01982      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONGS
01983      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCONGS
01984                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCONGS
01985      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONGS
01986      IF WS-CIA > 17                                               ELTCONGS
01987         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTCONGS
01988         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONGS
01989                COMMAREA(DFHCOMMAREA)                              ELTCONGS
01990         END-EXEC                                                  ELTCONGS
01991         MOVE +1            TO WS-CIA.                             ELTCONGS
01992      ADD +1                TO  WS-CIA.                            ELTCONGS
01993      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONGS
01994      ADD +1                TO  WS-CIA.                            ELTCONGS
01995      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCONGS
01996      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTCONGS
01997      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONGS
01998                COMMAREA(DFHCOMMAREA)                              ELTCONGS
01999      END-EXEC.                                                    ELTCONGS
02000      MOVE +1            TO WS-CIA.                                ELTCONGS
02001 **                                                               |ELTCONGS
02002 **---------------------------------------------------------------+ELTCONGS
02003                                                                   ELTCONGS
02004  3050-ZERO-ALL-WITH-SAME-NO.                                      ELTCONGS
02005                                                                   ELTCONGS
02006      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCONGS
02007         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONGS
02008         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCONGS
02009         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCONGS
02010                                                   CMF-CODE-VALUE  ELTCONGS
02011         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTCONGS
02012         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTCONGS
02013         PERFORM 2100-CODES-MANUAL-LONG                            ELTCONGS
02014         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCONGS
02015         IF WS-CIA  >  20 OR  =  20                                ELTCONGS
02016            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTCONGS
02017            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCONGS
02018                COMMAREA(DFHCOMMAREA)                              ELTCONGS
02019            END-EXEC                                               ELTCONGS
02020            MOVE +1  TO  WS-CIA.                                   ELTCONGS
02021                                                                   ELTCONGS
02022  3090-PROBLEM-WITH-INDICES.                                       ELTCONGS
02023                                                                   ELTCONGS
02024      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCONGS
02025      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONGS
02026                                                                   ELTCONGS
02027      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONGS
02028      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONGS
02029                                                                   ELTCONGS
02030      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONGS
02031      END-EXEC.                                                    ELTCONGS
02032                                                                   ELTCONGS
02033  3099-EXIT.           EXIT.                                       ELTCONGS
02034                                                                   ELTCONGS
02035      COPY ELSTCOMP.                                               ELTCONGS
02036                                                                   ELTCONGS
02037 /              A B E N D                                          ELTCONGS
02038 ******************************************************************ELTCONGS
02039 *                        A B E N D                                ELTCONGS
02040 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTCONGS
02041 *                                                                 ELTCONGS
02042 ******************************************************************ELTCONGS
02043  9999-ABEND SECTION.                                              ELTCONGS
02044                                                                   ELTCONGS
02045      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTCONGS
02046                                                                   ELTCONGS
02047  9999-EXIT.     EXIT.                                             ELTCONGS
