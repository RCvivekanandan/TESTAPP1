00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTASSTS.                                            ELTASSTS
00003  AUTHOR. ALIDA JATICH OF T. M. FLOYD, INC.                           LV002
00004  DATE-WRITTEN. MAY 13, 1986.                                      ELTASSTS
00005  DATE-COMPILED.                                                   ELTASSTS
00006 ******************************************************************ELTASSTS
00007 **                                                              **ELTASSTS
00008 **                                                              **ELTASSTS
00009 **                       PROGRAM ABSTRACT                       **ELTASSTS
00010 **                                                              **ELTASSTS
00011 **  PROGRAM NAME: E.L.S. ASSISTANT SURGEON TOPIC                **ELTASSTS
00012 **                                                              **ELTASSTS
00013 **  PROGRAM I.D.: ELTASSTS                                      **ELTASSTS
00014 **                                                              **ELTASSTS
00015 **  PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING           **ELTASSTS
00016 **             BENEFIT COVERAGE FOR ASSISTANT SURGEON.          **ELTASSTS
00017 **                                                              **ELTASSTS
00018 **  OVERVIEW:  THE PROGRAM DISPLAYS THE ASSISTANT SURGEON       **ELTASSTS
00019 **             COVERAGE AFFORDED A PATIENT BY HIS GROUP.        **ELTASSTS
00020 **             THE REQUIRED INFORMATION IS OBTAINED BY          **ELTASSTS
00021 **             INTERROGATING THE BENEFIT PROVISIONS FOR         **ELTASSTS
00022 **             THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR   **ELTASSTS
00023 **             RANGE OF DATES.                                  **ELTASSTS
00024 **                                                              **ELTASSTS
00025 **  RECORDS                                                     **ELTASSTS
00026 **  ACCESSED:  GROUP SPECIFIC, VARIOUS BENEFIT PROVISION, AND   **ELTASSTS
00027 **             MANY DATA ELEMENT AND CODE VALUE RECORDS.        **ELTASSTS
00028 **                                                              **ELTASSTS
00029 **  PROCESSING                                                  **ELTASSTS
00030 **  FUNCTIONS:                                                  **ELTASSTS
00031 **                                                              **ELTASSTS
00032 **                                                              **ELTASSTS
00033 ******************************************************************ELTASSTS
00034 *                                                                *ELTASSTS
00035 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTASSTS
00036 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELTASSTS
00037 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTASSTS
00038 *                                                                *ELTASSTS
00039 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELTASSTS
00040 *                                                                *ELTASSTS
00041 *    0000    05/13/86  AMJ  ORIGINAL MODULE WRITTEN.             *ELTASSTS
00042 *    0000    05/27/86  AMJ  ADDED SECOND BLL CELL FOR            *ELTASSTS
00043 *                           GCCONTRC > 4096 BYTES.               *ELTASSTS
00044 *    0150    06/04/86  AMJ  CHANGED TO DISPLAY CONTRACT FIELD    *ELTASSTS
00045 *                           ONLY WHEN BEN PROV EXISTS FOR THAT   *ELTASSTS
00046 *                           LINE OF BUSINESS.                    *ELTASSTS
00047 *    0144    06/10/86  AMJ  FIXED ERROR IN DISPLAY OF CONTRACT   *ELTASSTS
00048 *                           FIELD.                               *ELTASSTS
00049 *                                                                *ELTASSTS
00050 *    ----    08/14/86  LET  USING THE 1ST HEADER LINE FROM THE   *ELTASSTS
00051 *                           PROLOG ON THE TOPIC SCREENS.         *ELTASSTS
00052 *                                                                *ELTASSTS
00053 *    ----    09/26/86  JTC  VS II CONVERISON                     *ELTASSTS
00054 *                                                                *ELTASSTS
00055 *    ----    10/19/87  NAC  REWORK PHRASE FOR COVERED BENEFITS.  *ELTASSTS
00056 *                                                                *ELTASSTS
00057 *    ----    03/27/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS      *ELTASSTS
00058 *                                                                *ELTASSTS
00059 *    ----    10/14/89  RKH  ADDED TRANSFER TO OTHER RESP IND     *ELTASSTS
00060 *                                                                *ELTASSTS
00061 *         20-MAR-1990  RJL  CORRECTED ABEND CODE 'ELO1' TO       *ELTASSTS
00062 *                           'EL01'.                              *ELTASSTS
00063 *    ----    11/12/90  AKK  CHANGED '0' COMPARE ON PLP TRANSFER  *ELTASSTS
00064 *                           OTHER RESP IND TO 'ZERO' DUE TO      *ELTASSTS
00065 *                           CHANGE IN GCBENPVC CAUSING A CHANGE  *ELTASSTS
00066 *                           IN ELSPLGTB.                         *ELTASSTS
00067 *                                                                *ELTASSTS
00068 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELTASSTS
00069 ******************************************************************ELTASSTS
00070 /                                                                 ELTASSTS
00071  ENVIRONMENT DIVISION.                                            ELTASSTS
00072      SKIP3                                                        ELTASSTS
00073  DATA DIVISION.                                                   ELTASSTS
00074  WORKING-STORAGE SECTION.                                         ELTASSTS
00075  01  WS-BEGIN                    PIC X(24) VALUE                  ELTASSTS
00076      '***ELTASSTS WS BEGINS***'.                                  ELTASSTS
00077  01  WS-DIAGNOSTICS.                                              ELTASSTS
00078      05  WS-PARA-ID1             PIC X(4)  VALUE 'XXXX'.          ELTASSTS
00079      05  WS-PARA-ID2             PIC X(4)  VALUE 'XXXX'.          ELTASSTS
00080                                                                   ELTASSTS
00081  01  WS-ABEND-CODE               PIC X(4)  VALUE 'XXXX'.          ELTASSTS
00082 /                                                                 ELTASSTS
00083 ******************************************************************ELTASSTS
00084 ** WORKFIELDS AND SWITCHES                                      **ELTASSTS
00085 ******************************************************************ELTASSTS
00086  01  WS-WORK-FIELDS.                                              ELTASSTS
00087      05  WS-CHAR-0                     PIC X.                     ELTASSTS
00088      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTASSTS
00089      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTASSTS
00090      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTASSTS
00091      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTASSTS
00092      05  WS-SUB4                       PIC S999  COMP-3 VALUE +0. ELTASSTS
00093      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTASSTS
00094      05  WS-FIRSTTIME-IND              PIC X.                     ELTASSTS
00095          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTASSTS
00096      05  WS-BASIC-OR-CMM-IND           PIC X     VALUE 'N'.       ELTASSTS
00097          88  WS-BASIC-EXISTS               VALUE 'B'.             ELTASSTS
00098          88  WS-CMM-EXISTS                 VALUE 'C'.             ELTASSTS
00099      05  WS-SMM-IND                    PIC X     VALUE 'N'.       ELTASSTS
00100          88  WS-SMM-EXISTS                 VALUE 'S'.             ELTASSTS
00101      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTASSTS
00102          88  WS-ADD-A-BLANK-LINE           VALUE 'Y'.             ELTASSTS
00103      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTASSTS
00104          88  WS-MOVE-LINES-TO-CIA          VALUE 'Y'.             ELTASSTS
00105      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTASSTS
00106      05  WS-PERCENT-FLD.                                          ELTASSTS
00107        10  WS-PERCENTAGE               PIC ZZ9.                   ELTASSTS
00108        10  WS-PERCENT-SIGN             PIC X.                     ELTASSTS
00109 /                                                                 ELTASSTS
00110 ******************************************************************ELTASSTS
00111 ** BENEFIT PROVISION ID'S BY TYPE                               **ELTASSTS
00112 ******************************************************************ELTASSTS
00113  01  WS-BEN-PROV-ID.                                              ELTASSTS
00114      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +1. ELTASSTS
00115      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +1. ELTASSTS
00116      05  WS-PROF-IP-TAB.                                          ELTASSTS
00117        10  FILLER                      PIC X(6)  VALUE 'ASSI C'.  ELTASSTS
00118      05  WS-PROF-IP-LIST REDEFINES WS-PROF-IP-TAB                 ELTASSTS
00119                                        PIC X(6)  OCCURS 1 TIMES.  ELTASSTS
00120                                                                   ELTASSTS
00121      05  WS-PROF-OP-CNT                PIC S9(4) COMP   VALUE +1. ELTASSTS
00122      05  WS-PROF-OP-TAB.                                          ELTASSTS
00123        10  FILLER                      PIC X(6)  VALUE 'ASSO C'.  ELTASSTS
00124      05  WS-PROF-OP-LIST REDEFINES WS-PROF-OP-TAB                 ELTASSTS
00125                                        PIC X(6)  OCCURS 1 TIMES.  ELTASSTS
00126 /                                                                 ELTASSTS
00127 ******************************************************************ELTASSTS
00128 ** DISPLAY LINES                                                **ELTASSTS
00129 ******************************************************************ELTASSTS
00130  01  WS-ELS-DISPLAY-LINES.                                        ELTASSTS
00131    05  WS-HDR-2-PROF-IP.                                          ELTASSTS
00132      10  FILLER                    PIC X(19) VALUE SPACES.        ELTASSTS
00133      10  FILLER                    PIC X(40)                      ELTASSTS
00134          VALUE 'ASSISTANT SURGEON INPATIENT PROFESSIONAL'.        ELTASSTS
00135      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTASSTS
00136                                                                   ELTASSTS
00137    05  WS-HDR-2-PROF-OP.                                          ELTASSTS
00138      10  FILLER                    PIC X(19) VALUE SPACES.        ELTASSTS
00139      10  FILLER                    PIC X(41)                      ELTASSTS
00140          VALUE 'ASSISTANT SURGEON OUTPATIENT PROFESSIONAL'.       ELTASSTS
00141      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTASSTS
00142                                                                   ELTASSTS
00143    05  WS-SEE-CCP.                                                ELTASSTS
00144      10  FILLER                    PIC X(31)                      ELTASSTS
00145          VALUE 'SEE COST CONTAINMENT TOPIC FOR '.                 ELTASSTS
00146      10  FILLER                    PIC X(22)                      ELTASSTS
00147          VALUE 'ADDITIONAL LIMITATIONS'.                          ELTASSTS
00148      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTASSTS
00149                                                                   ELTASSTS
00150    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTASSTS
00151          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTASSTS
00152                                                                   ELTASSTS
00153    05  WS-FOLLOWING-BEN.                                          ELTASSTS
00154      10  FILLER                    PIC X(21) VALUE                ELTASSTS
00155          'COVERED SERVICES ARE:'.                                 ELTASSTS
00156                                                                   ELTASSTS
00157    05  WS-PAY-CONSDR-TEXT1.                                       ELTASSTS
00158      10  FILLER                    PIC X(45)                      ELTASSTS
00159        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTASSTS
00160    05  WS-PAY-CONSDR-TEXT2.                                       ELTASSTS
00161      10  FILLER                    PIC X(44)                      ELTASSTS
00162        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTASSTS
00163                                                                   ELTASSTS
00164    05  WS-PAYMNT-BASED.                                           ELTASSTS
00165      10  FILLER                    PIC X(21)                      ELTASSTS
00166          VALUE 'PAYMENT IS BASED ON: '.                           ELTASSTS
00167      10  FILLER                    PIC X(58) VALUE LOW-VALUES.    ELTASSTS
00168                                                                   ELTASSTS
00169    05  WS-BASIC.                                                  ELTASSTS
00170      10  WS-BASIC-LIT              PIC X(16)                      ELTASSTS
00171          VALUE '         BASIC: '.                                ELTASSTS
00172      10  WS-DTL-BASIC              PIC X(50) VALUE SPACES.        ELTASSTS
00173      10  FILLER                    PIC X(13) VALUE LOW-VALUES.    ELTASSTS
00174                                                                   ELTASSTS
00175    05  WS-SUPPLEMENTAL.                                           ELTASSTS
00176      10  WS-SUPP-LIT               PIC X(16)                      ELTASSTS
00177          VALUE '  SUPPLEMENTAL: '.                                ELTASSTS
00178      10  WS-DTL-SUPPLEMENTAL       PIC X(50) VALUE SPACES.        ELTASSTS
00179      10  FILLER                    PIC X(13) VALUE LOW-VALUES.    ELTASSTS
00180                                                                   ELTASSTS
00181    05  WS-PAYABLE-AS.                                             ELTASSTS
00182      10  FILLER                    PIC X(45) VALUE                ELTASSTS
00183          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTASSTS
00184      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTASSTS
00185                                                                   ELTASSTS
00186    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER'.  ELTASSTS
00187                                                                   ELTASSTS
00188    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTASSTS
00189        'SPILLOVER COINSURANCE:'.                                  ELTASSTS
00190                                                                   ELTASSTS
00191    05  WS-SPILLOVER-DED            PIC X(23)  VALUE               ELTASSTS
00192        ' SPILLOVER DEDUCTIBLE:'.                                  ELTASSTS
00193                                                                   ELTASSTS
00194    05  WS-BASIC-INDEMNITY.                                        ELTASSTS
00195      10  FILLER                    PIC X(49)                      ELTASSTS
00196        VALUE 'BASIC INDEMNITY EXCESS SPILLS TO SUPPLEMENTAL MM '. ELTASSTS
00197      10  WS-DTL-BASIC-INDEMNITY    PIC X(27) VALUE SPACES.        ELTASSTS
00198      10  FILLER                    PIC X(3) VALUE LOW-VALUES.     ELTASSTS
00199                                                                   ELTASSTS
00200    05  WS-CONTRACT-RELATED.                                       ELTASSTS
00201      10  FILLER                    PIC X(50) VALUE                ELTASSTS
00202        'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.      ELTASSTS
00203      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTASSTS
00204                                                                   ELTASSTS
00205    05  WS-TOPIC-PHRASE.                                           ELTASSTS
00206      10  FILLER                    PIC X(17)                      ELTASSTS
00207        VALUE 'ASSISTANT SURGEON'.                                 ELTASSTS
00208      10  FILLER                    PIC X      VALUE QUOTE.        ELTASSTS
00209      10  FILLER                    PIC X(15)                      ELTASSTS
00210        VALUE 'S SERVICES ARE '.                                   ELTASSTS
00211                                                                   ELTASSTS
00212    05  WS-NO-TABULAR1.                                            ELTASSTS
00213      10  FILLER                    PIC X(51)  VALUE               ELTASSTS
00214         '*** FOUND A GENERIC CONTRACT FILE INCONSISTENCY IN '.    ELTASSTS
00215      10  FILLER                    PIC X(22)  VALUE               ELTASSTS
00216         'GOING FROM BENEFIT ***'.                                 ELTASSTS
00217                                                                   ELTASSTS
00218    05  WS-NO-TABULAR2.                                            ELTASSTS
00219      10  FILLER                    PIC X(15)  VALUE               ELTASSTS
00220         '*** PROVISION: '.                                        ELTASSTS
00221      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTASSTS
00222      10  FILLER                    PIC X VALUE SPACE.             ELTASSTS
00223      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTASSTS
00224      10  FILLER                    PIC X(13)  VALUE               ELTASSTS
00225         ' TO TABULAR: '.                                          ELTASSTS
00226      10  WS-NO-TAB-ID              PIC X(6).                      ELTASSTS
00227      10  FILLER                    PIC X VALUE SPACE.             ELTASSTS
00228      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTASSTS
00229      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTASSTS
00230                                                                   ELTASSTS
00231    05  WS-PERCENT-OF-ALLOWANCE.                                   ELTASSTS
00232      10  FILLER                    PIC X(44)                      ELTASSTS
00233       VALUE '                THE PERCENT OF ALLOWANCE IS '.       ELTASSTS
00234      10  WS-PCT-ALLOW              PIC ZZ9.                       ELTASSTS
00235      10  FILLER                    PIC X VALUE '%'.               ELTASSTS
00236                                                                   ELTASSTS
00237    05  WS-IF-SAME-PVDR-BILLING.                                   ELTASSTS
00238      10  FILLER                    PIC X(32)                      ELTASSTS
00239       VALUE 'IF THE SAME PROVIDER IS BILLING '.                   ELTASSTS
00240      10  FILLER                    PIC X(37)                      ELTASSTS
00241       VALUE 'SURGERY/ANESTHESIA/ASSISTANT SURGEON:'.              ELTASSTS
00242                                                                   ELTASSTS
00243    05  WS-PRIM-SURG-DEPEND-IND.                                   ELTASSTS
00244      10  FILLER                    PIC X(44)                      ELTASSTS
00245       VALUE 'THE PRIMARY SURGEON DEPENDENCY INDICATOR IS:'.       ELTASSTS
00246 *                                                                 ELTASSTS
00247 *  05  WS-ELIG-METHOD-OF-TREAT.                                   ELTASSTS
00248 *    10  FILLER                    PIC X(36)                      ELTASSTS
00249 *     VALUE 'THE ELIGIBLE METHOD OF TREATMENT IS:'.               ELTASSTS
00250                                                                   ELTASSTS
00251    05  WS-CHECK-CONTRACT-FOR-PVE.                                 ELTASSTS
00252      10  FILLER                    PIC X(44)                      ELTASSTS
00253       VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.       ELTASSTS
00254                                                                   ELTASSTS
00255    05  WS-STAFF-PROV-NOT-AVAIL.                                   ELTASSTS
00256      10  FILLER                    PIC X(51)                      ELTASSTS
00257      VALUE 'PAY ASSISTANT SURGEON ONLY IF HOSPITAL STAFF PROVID'. ELTASSTS
00258      10  FILLER                    PIC X(20)                      ELTASSTS
00259      VALUE 'ER IS NOT AVAILABLE.'.                                ELTASSTS
00260                                                                   ELTASSTS
00261    05  WS-ASST-SRG-NOT-COV-INST.                                  ELTASSTS
00262      10  FILLER                    PIC X(51)                      ELTASSTS
00263      VALUE 'ASSISTANT SURGEON SERVICES ARE NOT COVERED AS AN IN'. ELTASSTS
00264      10  FILLER                    PIC X(20)                      ELTASSTS
00265      VALUE 'STITUTIONAL BENEFIT.'.                                ELTASSTS
00266                                                                   ELTASSTS
00267    05  WS-POSSIBLE-ERROR.                                         ELTASSTS
00268      10  FILLER                    PIC X(42) VALUE                ELTASSTS
00269         'POSSIBLE ERROR CONDITION - PLEASE CALL SSD'.             ELTASSTS
00270      10  FILLER                    PIC X(37) VALUE LOW-VALUES.    ELTASSTS
00271                                                                   ELTASSTS
00272    05  WS-BLANK-PREFIX-DET-LINE.                                  ELTASSTS
00273      10  FILLER                    PIC X(16) VALUE SPACES.        ELTASSTS
00274      10  WS-TRUNC-TEXT             PIC X(63) VALUE LOW-VALUES.    ELTASSTS
00275                                                                   ELTASSTS
00276    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTASSTS
00277    05  FILLER REDEFINES WS-TEMP-TEXT-AREA.                        ELTASSTS
00278      10  WS-TEMP-TEXT-CHAR         PIC X OCCURS 79 TIMES.         ELTASSTS
00279                                                                   ELTASSTS
00280  01  WS-END                            PIC X(16)  VALUE           ELTASSTS
00281      '*** W/S ENDS ***'.                                          ELTASSTS
00282 /                                                                 ELTASSTS
00283  LINKAGE SECTION.                                                 ELTASSTS
00284  01  DFHCOMMAREA.                                                 ELTASSTS
00285      COPY ELSCOMMC.                                               ELTASSTS
00286 /                                                                 ELTASSTS
00287      COPY ELSCIA2C.                                               ELTASSTS
00288 /                                                                 ELTASSTS
00289 *** PARM AREA ***                                                 ELTASSTS
00290      COPY ELSIOPMC.                                               ELTASSTS
00291 /                                                                 ELTASSTS
00292      COPY ELSKEYSC.                                               ELTASSTS
00293 /                                                                 ELTASSTS
00294      COPY ELSOUTPC.                                               ELTASSTS
00295 /                                                                 ELTASSTS
00296      COPY ELSSSCBC.                                               ELTASSTS
00297 /                                                                 ELTASSTS
00298      COPY ELSCMIFC.                                               ELTASSTS
00299 /                                                                 ELTASSTS
00300      COPY ELSCMDSC.                                               ELTASSTS
00301 /                                                                 ELTASSTS
00302      COPY ELSPRVNC.                                               ELTASSTS
00303 /                                                                 ELTASSTS
00304 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTASSTS
00305      COPY ELSPLGSW.                                               ELTASSTS
00306 *** BENEFIT PROVISION TABLE OF FLDS ***                           ELTASSTS
00307      COPY ELSPLGTB.                                               ELTASSTS
00308 /                                                                 ELTASSTS
00309      COPY ELSTCWAC.                                               ELTASSTS
00310 /                                                                 ELTASSTS
00311  01  BASIC-CONTRACT-REC.                                          ELTASSTS
00312      COPY GCCONTRC.                                               ELTASSTS
00313 /                                                                 ELTASSTS
00314  01  SUPP-CONTRACT-REC.                                           ELTASSTS
00315      COPY GCCONTR3.                                               ELTASSTS
00316 /                                                                 ELTASSTS
00317  PROCEDURE DIVISION.                                              ELTASSTS
00318                                                                   ELTASSTS
00319  0000-MAINLINE.                                                   ELTASSTS
00320                                                                   ELTASSTS
00321      MOVE '0' TO WS-CHAR-0.                                       ELTASSTS
00322                                                                   ELTASSTS
00323 ******************************************************************ELTASSTS
00324 ** BE SURE THAT THE MAIN ONLINE DRIVER PASSED US THE RIGHT      **ELTASSTS
00325 ** DATA AREAS.                                                  **ELTASSTS
00326 ******************************************************************ELTASSTS
00327      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELTASSTS
00328         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTASSTS
00329         EXEC CICS ABEND  ABCODE('EL01')  END-EXEC                 ELTASSTS
00330      END-IF.                                                      ELTASSTS
00331 ***  ADDRESS DATA AREAS USING PASSED POINTERS ***                 ELTASSTS
00332                                                                   ELTASSTS
00333      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTASSTS
00334          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTASSTS
00335                                                                   ELTASSTS
00336      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTASSTS
00337      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00338          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTASSTS
00339                                                                   ELTASSTS
00340      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTASSTS
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00342          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTASSTS
00343                                                                   ELTASSTS
00344      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTASSTS
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00346          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTASSTS
00347                                                                   ELTASSTS
00348      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTASSTS
00349      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00350          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTASSTS
00351                                                                   ELTASSTS
00352      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTASSTS
00353      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00354          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTASSTS
00355                                                                   ELTASSTS
00356      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTASSTS
00357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00358          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTASSTS
00359                                                                   ELTASSTS
00360      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTASSTS
00361                                                                   ELTASSTS
00362      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTASSTS
00363      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00364          ADDRESS OF BASIC-CONTRACT-REC                            ELTASSTS
00365      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
00366          MOVE 'B' TO WS-BASIC-OR-CMM-IND.                         ELTASSTS
00367                                                                   ELTASSTS
00368      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTASSTS
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00370          ADDRESS OF SUPP-CONTRACT-REC.                            ELTASSTS
00371      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
00372          MOVE 'S' TO WS-SMM-IND.                                  ELTASSTS
00373                                                                   ELTASSTS
00374 ******************************************************************ELTASSTS
00375 ** ASSISTANT SURGEON COVERAGE IS SHOWN UNDER THE PROFESSIONAL   **ELTASSTS
00376 ** CATEGORY ONLY.  WE SHOW BOTH INPATIENT AND OUTPATIENT        **ELTASSTS
00377 ** COVERAGE SINCE THERE IS NO SUBTOPIC MENU FOR THIS TOPIC.     **ELTASSTS
00378 ******************************************************************ELTASSTS
00379                                                                   ELTASSTS
00380      IF NOT SSB-PROV-CLASS-PROF                                   ELTASSTS
00381         MOVE WS-ASST-SRG-NOT-COV-INST TO COF-DTL-LINE(3)          ELTASSTS
00382         MOVE ZERO TO COF-NBR-HDR-LINES                            ELTASSTS
00383         MOVE +3 TO COF-NBR-DTL-LINES                              ELTASSTS
00384         SET COF-NEW-PAGE TO TRUE                                  ELTASSTS
00385         EXEC CICS LINK                                            ELTASSTS
00386              PROGRAM('ELUOUTPT')                                  ELTASSTS
00387              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00388              END-EXEC.                                            ELTASSTS
00389                                                                   ELTASSTS
00390      IF NOT SSB-PROV-CLASS-INST                                   ELTASSTS
00391                                                                   ELTASSTS
00392         SET CIA-ELSPRVN-DDN TO TRUE.                              ELTASSTS
00393                                                                   ELTASSTS
00394         COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +         ELTASSTS
00395                (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL)   ELTASSTS
00396                                                                   ELTASSTS
00397         SET CIA-STG-GETMAIN TO TRUE                               ELTASSTS
00398                                                                   ELTASSTS
00399         EXEC CICS LINK                                            ELTASSTS
00400               PROGRAM('ELUSTGMG')                                 ELTASSTS
00401               COMMAREA(DFHCOMMAREA)                               ELTASSTS
00402         END-EXEC                                                  ELTASSTS
00403                                                                   ELTASSTS
00404         SET CIA-ELSPRVN-DDN TO TRUE.                              ELTASSTS
00405         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTASSTS
00406             ADDRESS OF PVN-BENEFIT-PROVISION-LIST                 ELTASSTS
00407                                                                   ELTASSTS
00408         PERFORM 2000-PROFESSIONAL-IP-RTNE                         ELTASSTS
00409         PERFORM 4000-PROFESSIONAL-OP-RTNE.                        ELTASSTS
00410                                                                   ELTASSTS
00411 ******************************************************************ELTASSTS
00412 ** TERMINATE OUTPUT PROCESSING.                                 **ELTASSTS
00413 ******************************************************************ELTASSTS
00414      MOVE 'E' TO COF-FUNCTION.                                    ELTASSTS
00415      MOVE ZERO TO COF-NBR-HDR-LINES,                              ELTASSTS
00416                   COF-NBR-DTL-LINES.                              ELTASSTS
00417      EXEC CICS LINK                                               ELTASSTS
00418           PROGRAM('ELUOUTPT')                                     ELTASSTS
00419           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00420           END-EXEC.                                               ELTASSTS
00421                                                                   ELTASSTS
00422                                                                   ELTASSTS
00423  0099-RETURN.                                                     ELTASSTS
00424      EXEC CICS RETURN                                             ELTASSTS
00425           END-EXEC.                                               ELTASSTS
00426 /                                                                 ELTASSTS
00427 ******************************************************************ELTASSTS
00428 *        P R O F E S S I O N A L   I P   R T N E                 *ELTASSTS
00429 *                                                                *ELTASSTS
00430 *          THIS ROUTINE HAS A NUMBER OF STEPS.                   *ELTASSTS
00431 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.          *ELTASSTS
00432 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE  *ELTASSTS
00433 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL*ELTASSTS
00434 *  END OF PAGE AND EXIT THIS ROUTINE.                            *ELTASSTS
00435 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING    *ELTASSTS
00436 *  MODULE.                                                       *ELTASSTS
00437 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A  *ELTASSTS
00438 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                 *ELTASSTS
00439 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.         *ELTASSTS
00440 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE    *ELTASSTS
00441 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                    *ELTASSTS
00442 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR        *ELTASSTS
00443 *  BENEFITS HAVE BEEN DISPLAYED.                                 *ELTASSTS
00444 *                                                                *ELTASSTS
00445 ******************************************************************ELTASSTS
00446  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTASSTS
00447      MOVE '2000' TO WS-PARA-ID1.                                  ELTASSTS
00448                                                                   ELTASSTS
00449      MOVE 'P' TO COF-FUNCTION.                                    ELTASSTS
00450      MOVE 'Y' TO WS-FIRSTTIME-IND.                                ELTASSTS
00451      MOVE ZERO TO COF-NBR-HDR-LINES,                              ELTASSTS
00452                   COF-NBR-DTL-LINES.                              ELTASSTS
00453      EXEC CICS LINK                                               ELTASSTS
00454           PROGRAM('ELUOUTPT')                                     ELTASSTS
00455           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00456           END-EXEC.                                               ELTASSTS
00457                                                                   ELTASSTS
00458      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTASSTS
00459      MOVE WS-HDR-2-PROF-IP TO COF-HDR-LINE(2).                    ELTASSTS
00460                                                                   ELTASSTS
00461      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTASSTS
00462                                                                   ELTASSTS
00463      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTASSTS
00464         VARYING  WS-SUB FROM +1 BY +1                             ELTASSTS
00465         UNTIL WS-SUB > WS-PROF-IP-CNT.                            ELTASSTS
00466      GO TO 2020-CALL-COVERAGE.                                    ELTASSTS
00467                                                                   ELTASSTS
00468  2010-MOVE-IN-PROF-IP.                                            ELTASSTS
00469      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
00470      MOVE WS-PROF-IP-LIST(WS-SUB) TO                              ELTASSTS
00471          PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).                     ELTASSTS
00472      MOVE ZERO TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),            ELTASSTS
00473                   PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).            ELTASSTS
00474                                                                   ELTASSTS
00475  2020-CALL-COVERAGE.                                              ELTASSTS
00476      MOVE '2020' TO WS-PARA-ID1.                                  ELTASSTS
00477      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
00478      MOVE WS-SEE-CCP TO COF-DTL-LINE(2).                          ELTASSTS
00479      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
00480      EXEC CICS LINK                                               ELTASSTS
00481           PROGRAM('ELUOUTPT')                                     ELTASSTS
00482           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00483           END-EXEC.                                               ELTASSTS
00484                                                                   ELTASSTS
00485      MOVE WS-TOPIC-PHRASE TO SSB-TOPIC-PHRASE.                    ELTASSTS
00486                                                                   ELTASSTS
00487      EXEC CICS LINK                                               ELTASSTS
00488         PROGRAM('ELGCOVER')                                       ELTASSTS
00489           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00490         END-EXEC.                                                 ELTASSTS
00491                                                                   ELTASSTS
00492      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTASSTS
00493      MOVE LOW-VALUES TO COF-DTL-LINE(COF-NBR-DTL-LINES).          ELTASSTS
00494      EXEC CICS LINK                                               ELTASSTS
00495           PROGRAM('ELUOUTPT')                                     ELTASSTS
00496           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00497           END-EXEC.                                               ELTASSTS
00498                                                                   ELTASSTS
00499      IF PVN-COVG-NONE                                             ELTASSTS
00500         GO TO 2099-EXIT.                                          ELTASSTS
00501                                                                   ELTASSTS
00502      MOVE +1 TO WS-CIA.                                           ELTASSTS
00503                                                                   ELTASSTS
00504      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTASSTS
00505                                                                   ELTASSTS
00506      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTASSTS
00507                  PSP-PROVN-PRICING-METHD,                         ELTASSTS
00508                  PSP-ADDITIONAL-PRICING-PRCNT,                    ELTASSTS
00509                  PSP-VARIABLE-INDEMNITY-PRCNT,                    ELTASSTS
00510                  PSP-TRANSF-OTHER-RESP-IND,                       ELTASSTS
00511                  PSP-SPILL-OVER-COINS-APL-IND,                    ELTASSTS
00512                  PSP-SPILL-OVER-DED-APL-IND,                      ELTASSTS
00513                  PSP-BEN-TAB-PROVN-ID-AAR,                        ELTASSTS
00514                  PSP-BEN-TAB-PROVN-ID-ABM,                        ELTASSTS
00515                  PSP-BEN-TAB-PROVN-ID-ACL,                        ELTASSTS
00516                  PSP-BEN-TAB-PROVN-ID-ADL,                        ELTASSTS
00517                  PSP-BEN-TAB-PROVN-ID-AOL,                        ELTASSTS
00518                  PSP-BEN-TAB-PROVN-ID-PPF,                        ELTASSTS
00519                  PSC-ELIG-METHD-OF-TREAT-IND,                     ELTASSTS
00520                  PSC-PRIM-SURG-DEPEND-IND,                        ELTASSTS
00521                  PSC-PRIM-SURG-DPD-PAY-PCT,                       ELTASSTS
00522                  PSC-BEN-SCOPE-ID.                                ELTASSTS
00523                                                                   ELTASSTS
00524      EXEC CICS LINK                                               ELTASSTS
00525           PROGRAM('ELUPLGRP')                                     ELTASSTS
00526           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
00527           END-EXEC.                                               ELTASSTS
00528                                                                   ELTASSTS
00529                                                                   ELTASSTS
00530      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTASSTS
00531      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00532          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTASSTS
00533                                                                   ELTASSTS
00534      PERFORM 2030-FIND-FIRST-NONZERO                              ELTASSTS
00535         VARYING WS-SUB FROM +1 BY +1                              ELTASSTS
00536         UNTIL WS-SUB > WS-PROF-IP-CNT.                            ELTASSTS
00537                                                                   ELTASSTS
00538      GO TO 2099-EXIT.                                             ELTASSTS
00539                                                                   ELTASSTS
00540  2030-FIND-FIRST-NONZERO.                                         ELTASSTS
00541      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
00542      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTASSTS
00543         NEXT SENTENCE                                             ELTASSTS
00544      ELSE                                                         ELTASSTS
00545         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTASSTS
00546                                                                   ELTASSTS
00547  2040-BUILD-SCREEN-LINES.                                         ELTASSTS
00548      MOVE '2040' TO WS-PARA-ID1.                                  ELTASSTS
00549                                                                   ELTASSTS
00550      SET PLT-INDEX1  TO                                           ELTASSTS
00551                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTASSTS
00552      IF WS-NOT-FIRST-TIME                                         ELTASSTS
00553         MOVE 'P' TO COF-FUNCTION                                  ELTASSTS
00554         EXEC CICS LINK   PROGRAM('ELUOUTPT')                      ELTASSTS
00555              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00556              END-EXEC                                             ELTASSTS
00557      ELSE                                                         ELTASSTS
00558         MOVE 'N' TO WS-FIRSTTIME-IND.                             ELTASSTS
00559                                                                   ELTASSTS
00560      MOVE +1 TO WS-CIA.                                           ELTASSTS
00561                                                                   ELTASSTS
00562      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
00563         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
00564            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTASSTS
00565            GO TO 2099-EXIT                                        ELTASSTS
00566         ELSE                                                      ELTASSTS
00567            SET PLT-INDEX2 TO 2                                    ELTASSTS
00568      ELSE                                                         ELTASSTS
00569         SET PLT-INDEX2 TO 1.                                      ELTASSTS
00570                                                                   ELTASSTS
00571 **---------------------------------------------------------------+ELTASSTS
00572 **                                                               |ELTASSTS
00573 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTASSTS
00574      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE(WS-CIA).               ELTASSTS
00575      ADD  +1 TO WS-CIA.                                           ELTASSTS
00576      MOVE ZERO TO WS-SUB2.                                        ELTASSTS
00577      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTASSTS
00578      MOVE '2050' TO WS-PARA-ID1.                                  ELTASSTS
00579      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTASSTS
00580         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTASSTS
00581         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTASSTS
00582      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
00583                                                                   ELTASSTS
00584      ADD  +1, WS-CIA GIVING COF-NBR-DTL-LINES.                    ELTASSTS
00585      EXEC CICS LINK                                               ELTASSTS
00586           PROGRAM('ELUOUTPT')                                     ELTASSTS
00587              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00588           END-EXEC.                                               ELTASSTS
00589      MOVE +1 TO WS-CIA.                                           ELTASSTS
00590 **                                                               |ELTASSTS
00591 **---------------------------------------------------------------+ELTASSTS
00592                                                                   ELTASSTS
00593 ******************************************************************ELTASSTS
00594 ** SUPPRESS USE OF CONTRACT FIELDS IF THE ASSISTANT SURGEON     **ELTASSTS
00595 ** BENEFIT PROVISION DOES NOT EXIST FOR THAT LINE OF BUSINESS.  **ELTASSTS
00596 ******************************************************************ELTASSTS
00597      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTASSTS
00598      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00599          ADDRESS OF BASIC-CONTRACT-REC.                           ELTASSTS
00600      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
00601          MOVE 'B' TO WS-BASIC-OR-CMM-IND                          ELTASSTS
00602      ELSE                                                         ELTASSTS
00603          MOVE 'N' TO WS-BASIC-OR-CMM-IND.                         ELTASSTS
00604                                                                   ELTASSTS
00605      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTASSTS
00606      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
00607          ADDRESS OF SUPP-CONTRACT-REC.                            ELTASSTS
00608      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
00609          MOVE 'S' TO WS-SMM-IND                                   ELTASSTS
00610      ELSE                                                         ELTASSTS
00611          MOVE 'N' TO WS-SMM-IND.                                  ELTASSTS
00612                                                                   ELTASSTS
00613      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
00614         MOVE 'N' TO WS-BASIC-OR-CMM-IND                           ELTASSTS
00615         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
00616            MOVE 'N' TO WS-SMM-IND                                 ELTASSTS
00617         ELSE                                                      ELTASSTS
00618            NEXT SENTENCE                                          ELTASSTS
00619      ELSE                                                         ELTASSTS
00620         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
00621            MOVE 'N' TO WS-SMM-IND.                                ELTASSTS
00622                                                                   ELTASSTS
00623                                                                   ELTASSTS
00624 **---------------------------------------------------------------+ELTASSTS
00625 **                                                               |ELTASSTS
00626 **        P L A C E   O F   T R E A T M E N T                    |ELTASSTS
00627 **                                                               |ELTASSTS
00628                                                                   ELTASSTS
00629      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00630      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00631          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
00632                  NOT = ZERO                                       ELTASSTS
00633              MOVE WS-SERVICES-RENDERED                            ELTASSTS
00634                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
00635              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
00636              ADD +1 TO WS-CIA.                                    ELTASSTS
00637                                                                   ELTASSTS
00638      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00639      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00640          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
00641                  NOT = ZERO                                       ELTASSTS
00642              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTASSTS
00643                  MOVE WS-SERVICES-RENDERED                        ELTASSTS
00644                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
00645                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
00646                  ADD +1 TO WS-CIA.                                ELTASSTS
00647                                                                   ELTASSTS
00648      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00649      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00650          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
00651                  NOT = ZERO                                       ELTASSTS
00652              MOVE 'BP' TO CMF-RECORD-PREFIX                       ELTASSTS
00653              MOVE 'PLACE-TREAT-ELIG-IND'                          ELTASSTS
00654                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
00655              MOVE PLP-PLACE-TREAT-ELIG-IND                        ELTASSTS
00656                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
00657                  TO CMF-CODE-VALUE                                ELTASSTS
00658               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
00659               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00660               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
00661                                                                   ELTASSTS
00662      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00663      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00664          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
00665                  NOT = ZERO                                       ELTASSTS
00666              MOVE 'BP' TO CMF-RECORD-PREFIX                       ELTASSTS
00667              MOVE 'PLACE-TREAT-ELIG-IND'                          ELTASSTS
00668                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
00669              MOVE PLP-PLACE-TREAT-ELIG-IND                        ELTASSTS
00670                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
00671                  TO CMF-CODE-VALUE                                ELTASSTS
00672               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
00673               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00674               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
00675                                                                   ELTASSTS
00676      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
00677         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00678         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
00679         MOVE 1 TO WS-CIA                                          ELTASSTS
00680         EXEC CICS LINK                                            ELTASSTS
00681              PROGRAM('ELUOUTPT')                                  ELTASSTS
00682              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00683              END-EXEC.                                            ELTASSTS
00684 **                                                               |ELTASSTS
00685 **---------------------------------------------------------------+ELTASSTS
00686                                                                   ELTASSTS
00687 **---------------------------------------------------------------+ELTASSTS
00688 **                                                               |ELTASSTS
00689 **            B E N E F I T   S C O P E   I D                    |ELTASSTS
00690      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00692         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
00693            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00694                                         '0000' AND NOT = '00  '   ELTASSTS
00695               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
00696               ADD  +1 TO WS-CIA                                   ELTASSTS
00697               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
00698            ELSE                                                   ELTASSTS
00699               NEXT SENTENCE                                       ELTASSTS
00700         ELSE                                                      ELTASSTS
00701            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00702                                         '0000' AND NOT = '00  '   ELTASSTS
00703               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
00704               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
00705               ADD  +1 TO WS-CIA.                                  ELTASSTS
00706                                                                   ELTASSTS
00707      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00708      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00709         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
00710            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00711                                         '0000' AND NOT = '00  '   ELTASSTS
00712               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
00713               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
00714               ADD  +1 TO WS-CIA                                   ELTASSTS
00715            ELSE                                                   ELTASSTS
00716               NEXT SENTENCE                                       ELTASSTS
00717         ELSE                                                      ELTASSTS
00718            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00719                                         '0000' AND NOT = '00  '   ELTASSTS
00720               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
00721               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
00722               ADD  +1 TO WS-CIA.                                  ELTASSTS
00723                                                                   ELTASSTS
00724      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00725      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00726         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
00727            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00728                                         '0000' AND NOT = '00  '   ELTASSTS
00729               MOVE 'BPC' TO CMF-RECORD-PREFIX                     ELTASSTS
00730               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
00731               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
00732                                                    CMF-CODE-VALUE ELTASSTS
00733               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
00734               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00735               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTASSTS
00736            ELSE                                                   ELTASSTS
00737               NEXT SENTENCE                                       ELTASSTS
00738         ELSE                                                      ELTASSTS
00739            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00740                                         '0000' AND NOT = '00  '   ELTASSTS
00741               MOVE 'BPE' TO CMF-RECORD-PREFIX                     ELTASSTS
00742               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
00743               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
00744                                                    CMF-CODE-VALUE ELTASSTS
00745               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
00746               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00747               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
00748                                                                   ELTASSTS
00749      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00750         SET PLT-INDEX2 TO 2                                       ELTASSTS
00751         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
00752            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00753                                         '0000' AND NOT = '00  '   ELTASSTS
00754               MOVE 'BPC' TO CMF-RECORD-PREFIX                     ELTASSTS
00755               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
00756               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
00757                                                    CMF-CODE-VALUE ELTASSTS
00758               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
00759               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00760               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTASSTS
00761            ELSE                                                   ELTASSTS
00762               NEXT SENTENCE                                       ELTASSTS
00763         ELSE                                                      ELTASSTS
00764            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
00765                                         '0000' AND NOT = '00  '   ELTASSTS
00766               MOVE 'BPE' TO CMF-RECORD-PREFIX                     ELTASSTS
00767               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
00768               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
00769                                                    CMF-CODE-VALUE ELTASSTS
00770               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
00771               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
00772               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
00773                                                                   ELTASSTS
00774      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
00775         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00776         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
00777         MOVE 1 TO WS-CIA                                          ELTASSTS
00778         EXEC CICS LINK                                            ELTASSTS
00779              PROGRAM('ELUOUTPT')                                  ELTASSTS
00780              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00781              END-EXEC.                                            ELTASSTS
00782 **                                                               |ELTASSTS
00783 **---------------------------------------------------------------+ELTASSTS
00784                                                                   ELTASSTS
00785      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
00786         SET PLT-INDEX2 TO 2                                       ELTASSTS
00787      ELSE                                                         ELTASSTS
00788         SET PLT-INDEX2 TO 1.                                      ELTASSTS
00789                                                                   ELTASSTS
00790 **---------------------------------------------------------------+ELTASSTS
00791 **                                                               |ELTASSTS
00792 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTASSTS
00793 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTASSTS
00794 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTASSTS
00795      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00797         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
00798                                                              '19' ELTASSTS
00799         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA)                ELTASSTS
00800         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00801         ADD +1 TO WS-CIA.                                         ELTASSTS
00802                                                                   ELTASSTS
00803      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00804      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00805         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
00806                                                        '19' AND   ELTASSTS
00807         NOT WS-ADD-A-BLANK-LINE                                   ELTASSTS
00808         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA)                ELTASSTS
00809         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00810         ADD +1 TO WS-CIA.                                         ELTASSTS
00811                                                                   ELTASSTS
00812      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00813      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00814         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTASSTS
00815                            AND                                    ELTASSTS
00816         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00817         SET PLT-INDEX2 TO 2                                       ELTASSTS
00818         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
00819                                                             ZERO  ELTASSTS
00820            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTASSTS
00821            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTASSTS
00822            ADD +1 TO WS-CIA.                                      ELTASSTS
00823                                                                   ELTASSTS
00824      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00825      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00826         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTASSTS
00827                            AND                                    ELTASSTS
00828         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTASSTS
00829         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTASSTS
00830         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                     ELTASSTS
00831         ADD +1 TO WS-CIA.                                         ELTASSTS
00832                                                                   ELTASSTS
00833      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTASSTS
00834         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00835         SET PLT-INDEX2 TO 2                                       ELTASSTS
00836         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
00837                                                             ZERO  ELTASSTS
00838            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTASSTS
00839            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTASSTS
00840            ADD +1 TO WS-CIA.                                      ELTASSTS
00841                                                                   ELTASSTS
00842      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00843      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00844         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTASSTS
00845                                                           = ZERO  ELTASSTS
00846            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00847                                                           = ZERO  ELTASSTS
00848               MOVE SPACES TO WS-PERCENT-FLD                       ELTASSTS
00849            ELSE                                                   ELTASSTS
00850               MOVE '%' TO WS-PERCENT-SIGN                         ELTASSTS
00851          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00852                                                 TO WS-PERCENTAGE  ELTASSTS
00853         ELSE                                                      ELTASSTS
00854            MOVE '%' TO WS-PERCENT-SIGN                            ELTASSTS
00855          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00856                                                TO WS-PERCENTAGE.  ELTASSTS
00857      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00858         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
00859                                             ZERO AND NOT = '19'   ELTASSTS
00860         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
00861         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTASSTS
00862         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTASSTS
00863                                                    CMF-CODE-VALUE ELTASSTS
00864         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTASSTS
00865         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
00866         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTASSTS
00867                                                                   ELTASSTS
00868      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00869      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00870         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTASSTS
00871                                                               ZEROELTASSTS
00872            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00873                                                           = ZERO  ELTASSTS
00874               MOVE SPACES TO WS-PERCENT-FLD                       ELTASSTS
00875            ELSE                                                   ELTASSTS
00876               MOVE '%' TO WS-PERCENT-SIGN                         ELTASSTS
00877          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00878                                                 TO WS-PERCENTAGE  ELTASSTS
00879         ELSE                                                      ELTASSTS
00880            MOVE '%' TO WS-PERCENT-SIGN                            ELTASSTS
00881          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
00882                                                TO WS-PERCENTAGE.  ELTASSTS
00883      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
00884         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
00885                                             ZERO AND NOT = '19'   ELTASSTS
00886         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
00887         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTASSTS
00888         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTASSTS
00889                                                    CMF-CODE-VALUE ELTASSTS
00890         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTASSTS
00891         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
00892         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTASSTS
00893                                                                   ELTASSTS
00894      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
00895         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00896         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
00897         MOVE 1 TO WS-CIA                                          ELTASSTS
00898         EXEC CICS LINK                                            ELTASSTS
00899              PROGRAM('ELUOUTPT')                                  ELTASSTS
00900              COMMAREA(DFHCOMMAREA)                                ELTASSTS
00901              END-EXEC.                                            ELTASSTS
00902 **                                                               |ELTASSTS
00903 **---------------------------------------------------------------+ELTASSTS
00904                                                                   ELTASSTS
00905      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
00906         SET PLT-INDEX2 TO 2                                       ELTASSTS
00907      ELSE                                                         ELTASSTS
00908         SET PLT-INDEX2 TO 1.                                      ELTASSTS
00909                                                                   ELTASSTS
00910 **---------------------------------------------------------------+ELTASSTS
00911 **                                                               |ELTASSTS
00912 **     P R I M A R Y   S U R G E O N   D E P E N D E N C Y       |ELTASSTS
00913 **                     I N D I C A T O R                         |ELTASSTS
00914 **                                                               |ELTASSTS
00915 **     P R I M A R Y   S U R G E O N   D E P E N D E N C Y       |ELTASSTS
00916 **               P A Y M E N T   P E R C E N T                   |ELTASSTS
00917 **         ( P E R C E N T   O F   A L L O W A N C E )           |ELTASSTS
00918 **                                                               |ELTASSTS
00919 **                                                               |ELTASSTS
00920      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00921      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00922          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
00923                  NOT = ZERO                                       ELTASSTS
00924              MOVE WS-PRIM-SURG-DEPEND-IND                         ELTASSTS
00925                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
00926              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
00927              ADD +1 TO WS-CIA.                                    ELTASSTS
00928                                                                   ELTASSTS
00929      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00930      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00931          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
00932                  NOT = ZERO                                       ELTASSTS
00933              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTASSTS
00934                  MOVE WS-PRIM-SURG-DEPEND-IND                     ELTASSTS
00935                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
00936                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
00937                  ADD +1 TO WS-CIA.                                ELTASSTS
00938                                                                   ELTASSTS
00939      SET PLT-INDEX2 TO 1.                                         ELTASSTS
00940                                                                   ELTASSTS
00941      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00942          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
00943                  NOT = ZERO                                       ELTASSTS
00944              MOVE 'BPC' TO CMF-RECORD-PREFIX                      ELTASSTS
00945              MOVE 'PRIM-SURG-DEPEND-IND'                          ELTASSTS
00946                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
00947              MOVE PLC-PRIM-SURG-DEPEND-IND                        ELTASSTS
00948                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
00949                  TO CMF-CODE-VALUE                                ELTASSTS
00950              MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
00951              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
00952              PERFORM 2100-CALL-CODES-MANUAL-LONG                  ELTASSTS
00953              IF  PLC-PRIM-SURG-DPD-PAY-PCT                        ELTASSTS
00954                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
00955                      IS NUMERIC                                   ELTASSTS
00956                    AND PLC-PRIM-SURG-DPD-PAY-PCT                  ELTASSTS
00957                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
00958                      NOT = ZEROES                                 ELTASSTS
00959                MOVE PLC-PRIM-SURG-DPD-PAY-PCT                     ELTASSTS
00960                    (PLT-INDEX1, PLT-INDEX2)                       ELTASSTS
00961                    TO WS-PCT-ALLOW                                ELTASSTS
00962                MOVE WS-PERCENT-OF-ALLOWANCE                       ELTASSTS
00963                    TO COF-DTL-LINE (WS-CIA)                       ELTASSTS
00964                ADD +1 TO WS-CIA.                                  ELTASSTS
00965                                                                   ELTASSTS
00966      SET PLT-INDEX2 TO 2.                                         ELTASSTS
00967                                                                   ELTASSTS
00968      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
00969          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
00970                  NOT = ZERO                                       ELTASSTS
00971              MOVE 'BPC' TO CMF-RECORD-PREFIX                      ELTASSTS
00972              MOVE 'PRIM-SURG-DEPEND-IND'                          ELTASSTS
00973                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
00974              MOVE PLC-PRIM-SURG-DEPEND-IND                        ELTASSTS
00975                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
00976                  TO CMF-CODE-VALUE                                ELTASSTS
00977              MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                ELTASSTS
00978              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
00979              PERFORM 2100-CALL-CODES-MANUAL-LONG                  ELTASSTS
00980              IF  PLC-PRIM-SURG-DPD-PAY-PCT                        ELTASSTS
00981                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
00982                      IS NUMERIC                                   ELTASSTS
00983                    AND PLC-PRIM-SURG-DPD-PAY-PCT                  ELTASSTS
00984                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
00985                      NOT = ZEROES                                 ELTASSTS
00986                MOVE PLC-PRIM-SURG-DPD-PAY-PCT                     ELTASSTS
00987                    (PLT-INDEX1, PLT-INDEX2)                       ELTASSTS
00988                    TO WS-PCT-ALLOW                                ELTASSTS
00989                MOVE WS-PERCENT-OF-ALLOWANCE                       ELTASSTS
00990                    TO COF-DTL-LINE (WS-CIA)                       ELTASSTS
00991                ADD +1 TO WS-CIA.                                  ELTASSTS
00992                                                                   ELTASSTS
00993      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
00994         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
00995         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
00996         MOVE 1 TO WS-CIA                                          ELTASSTS
00997         EXEC CICS LINK                                            ELTASSTS
00998             PROGRAM('ELUOUTPT')                                   ELTASSTS
00999             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
01000             END-EXEC.                                             ELTASSTS
01001 **                                                               |ELTASSTS
01002 **                                                               |ELTASSTS
01003 **---------------------------------------------------------------+ELTASSTS
01004                                                                   ELTASSTS
01005                                                                   ELTASSTS
01006 **---------------------------------------------------------------+ELTASSTS
01007 **                                                               |ELTASSTS
01008 ** I F   T H E   S A M E   P R O V I D E R   I S   B I L L I N G |ELTASSTS
01009 ** P R I M   S U R G / A N E S T H E S I A / A S S T   S U R G   |ELTASSTS
01010 **                                                               |ELTASSTS
01011      IF (WS-BASIC-EXISTS OR WS-CMM-EXISTS)                        ELTASSTS
01012          IF GCT-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO              ELTASSTS
01013                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
01014              MOVE WS-IF-SAME-PVDR-BILLING                         ELTASSTS
01015                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
01016              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
01017              ADD +1 TO WS-CIA.                                    ELTASSTS
01018                                                                   ELTASSTS
01019      IF NOT WS-ADD-A-BLANK-LINE                                   ELTASSTS
01020          IF WS-SMM-EXISTS                                         ELTASSTS
01021              IF GCT3-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO         ELTASSTS
01022                      AND NOT = SPACES AND NOT = LOW-VALUES        ELTASSTS
01023                  MOVE WS-IF-SAME-PVDR-BILLING                     ELTASSTS
01024                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
01025                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
01026                  ADD +1 TO WS-CIA.                                ELTASSTS
01027                                                                   ELTASSTS
01028      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01029      IF (WS-BASIC-EXISTS OR WS-CMM-EXISTS)                        ELTASSTS
01030          IF GCT-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO              ELTASSTS
01031                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
01032              MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                 ELTASSTS
01033              MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                     ELTASSTS
01034                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
01035              MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                   ELTASSTS
01036                  TO CMF-CODE-VALUE                                ELTASSTS
01037              MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
01038              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
01039              PERFORM 2100-CALL-CODES-MANUAL-LONG.                 ELTASSTS
01040                                                                   ELTASSTS
01041      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01042      IF WS-SMM-EXISTS                                             ELTASSTS
01043          IF GCT3-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO             ELTASSTS
01044                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
01045              MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                 ELTASSTS
01046              MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                     ELTASSTS
01047                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
01048              MOVE GCT3-SAME-PROV-SRG-ANS-SRG-AST                  ELTASSTS
01049                  TO CMF-CODE-VALUE                                ELTASSTS
01050              MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                ELTASSTS
01051              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
01052              PERFORM 2100-CALL-CODES-MANUAL-LONG.                 ELTASSTS
01053                                                                   ELTASSTS
01054      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
01055         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
01056         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01057         MOVE 1 TO WS-CIA                                          ELTASSTS
01058         EXEC CICS LINK                                            ELTASSTS
01059             PROGRAM('ELUOUTPT')                                   ELTASSTS
01060             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
01061             END-EXEC.                                             ELTASSTS
01062 **                                                               |ELTASSTS
01063 **                                                               |ELTASSTS
01064 **---------------------------------------------------------------+ELTASSTS
01065                                                                   ELTASSTS
01066 **---------------------------------------------------------------+ELTASSTS
01067 **                                                               |ELTASSTS
01068 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTASSTS
01069      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01070      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01071         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
01072                                                        NOT = '0'  ELTASSTS
01073         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
01074         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTASSTS
01075                                           CMF-ELEMENT-SYSTEM-NAME ELTASSTS
01076         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTASSTS
01077                                               TO CMF-CODE-VALUE   ELTASSTS
01078         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTASSTS
01079         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
01080         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01081         MOVE 1 TO WS-CIA                                          ELTASSTS
01082         EXEC CICS LINK                                            ELTASSTS
01083             PROGRAM('ELUOUTPT')                                   ELTASSTS
01084             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
01085             END-EXEC.                                             ELTASSTS
01086 **                                                               |ELTASSTS
01087 **---------------------------------------------------------------+ELTASSTS
01088                                                                   ELTASSTS
01089 **---------------------------------------------------------------+ELTASSTS
01090 **                                                               |ELTASSTS
01091 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTASSTS
01092      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01093      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01094         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTASSTS
01095                                                        NOT = '0'  ELTASSTS
01096         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
01097         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTASSTS
01098                                           CMF-ELEMENT-SYSTEM-NAME ELTASSTS
01099         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTASSTS
01100                                                TO CMF-CODE-VALUE  ELTASSTS
01101         MOVE WS-SPILLOVER-DED TO WS-TEMP-TEXT-AREA                ELTASSTS
01102         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
01103         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01104         MOVE 1 TO WS-CIA                                          ELTASSTS
01105         EXEC CICS LINK                                            ELTASSTS
01106             PROGRAM('ELUOUTPT')                                   ELTASSTS
01107             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
01108             END-EXEC.                                             ELTASSTS
01109 **                                                               |ELTASSTS
01110 **---------------------------------------------------------------+ELTASSTS
01111                                                                   ELTASSTS
01112      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
01113         SET PLT-INDEX2 TO 2                                       ELTASSTS
01114      ELSE                                                         ELTASSTS
01115         SET PLT-INDEX2 TO 1.                                      ELTASSTS
01116                                                                   ELTASSTS
01117 **---------------------------------------------------------------+ELTASSTS
01118 **                                                               |ELTASSTS
01119 **           TRANSFER TO OTHER RESPONSIBILITY IND                |ELTASSTS
01120 **                                                               |ELTASSTS
01121                                                                   ELTASSTS
01122      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
01123          ZERO                                                     ELTASSTS
01124         NEXT SENTENCE                                             ELTASSTS
01125      ELSE                                                         ELTASSTS
01126         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
01127         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTASSTS
01128         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTASSTS
01129                                                TO CMF-CODE-VALUE  ELTASSTS
01130         MOVE SPACES           TO WS-TEMP-TEXT-AREA                ELTASSTS
01131         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
01132         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01133         MOVE 1 TO WS-CIA                                          ELTASSTS
01134         EXEC CICS LINK                                            ELTASSTS
01135             PROGRAM('ELUOUTPT')                                   ELTASSTS
01136             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
01137             END-EXEC.                                             ELTASSTS
01138 **                                                               |ELTASSTS
01139 **---------------------------------------------------------------+ELTASSTS
01140                                                                   ELTASSTS
01141      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
01142         SET PLT-INDEX2 TO 2                                       ELTASSTS
01143      ELSE                                                         ELTASSTS
01144         SET PLT-INDEX2 TO 1.                                      ELTASSTS
01145                                                                   ELTASSTS
01146 **---------------------------------------------------------------+ELTASSTS
01147 **                                                               |ELTASSTS
01148 **                  # A A R   T A B U L A R   F O U N D          |ELTASSTS
01149      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01150      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01151         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01152                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01153         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
01154         MOVE +1 TO COF-NBR-DTL-LINES                              ELTASSTS
01155         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTASSTS
01156      ELSE                                                         ELTASSTS
01157         SET PLT-INDEX2 TO 2                                       ELTASSTS
01158         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01159            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01160                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01161            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTASSTS
01162            MOVE +1 TO COF-NBR-DTL-LINES                           ELTASSTS
01163            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTASSTS
01164                                                                   ELTASSTS
01165      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
01166         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
01167         MOVE 1 TO WS-CIA                                          ELTASSTS
01168         EXEC CICS LINK                                            ELTASSTS
01169              PROGRAM('ELUOUTPT')                                  ELTASSTS
01170              COMMAREA(DFHCOMMAREA)                                ELTASSTS
01171              END-EXEC.                                            ELTASSTS
01172 **                                                               |ELTASSTS
01173 **---------------------------------------------------------------+ELTASSTS
01174                                                                   ELTASSTS
01175 **---------------------------------------------------------------+ELTASSTS
01176 **                                                               |ELTASSTS
01177 **                  # P P F   T A B U L A R                      |ELTASSTS
01178      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01179      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01180         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01181                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01182         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
01183                                                  KWA-GCTABULR-KEY ELTASSTS
01184         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
01185         IF IOP-RC-OK                                              ELTASSTS
01186            EXEC CICS LINK                                         ELTASSTS
01187                 PROGRAM('ELGPPF')                                 ELTASSTS
01188                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01189                 END-EXEC                                          ELTASSTS
01190         ELSE                                                      ELTASSTS
01191            NEXT SENTENCE                                          ELTASSTS
01192      ELSE                                                         ELTASSTS
01193         SET PLT-INDEX2 TO 2                                       ELTASSTS
01194         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01195            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01196                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01197          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
01198                                                 KWA-GCTABULR-KEY  ELTASSTS
01199            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
01200            IF IOP-RC-OK                                           ELTASSTS
01201               EXEC CICS LINK                                      ELTASSTS
01202                    PROGRAM('ELGPPF')                              ELTASSTS
01203                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
01204                    END-EXEC.                                      ELTASSTS
01205 **                                                               |ELTASSTS
01206 **---------------------------------------------------------------+ELTASSTS
01207                                                                   ELTASSTS
01208 **---------------------------------------------------------------+ELTASSTS
01209 **                                                               |ELTASSTS
01210 **                  # P V E   T A B U L A R                      |ELTASSTS
01211      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
01212      MOVE WS-CHECK-CONTRACT-FOR-PVE                               ELTASSTS
01213          TO COF-DTL-LINE(2).                                      ELTASSTS
01214      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
01215      EXEC CICS LINK                                               ELTASSTS
01216           PROGRAM('ELUOUTPT')                                     ELTASSTS
01217           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01218           END-EXEC.                                               ELTASSTS
01219 **                                                               |ELTASSTS
01220 **---------------------------------------------------------------+ELTASSTS
01221                                                                   ELTASSTS
01222 **---------------------------------------------------------------+ELTASSTS
01223 **                                                               |ELTASSTS
01224 **                  # A B M   T A B U L A R                      |ELTASSTS
01225      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01226      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01227         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01228                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01229         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
01230                                              KWA-GCTABULR-KEY     ELTASSTS
01231         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
01232         IF IOP-RC-OK                                              ELTASSTS
01233            EXEC CICS LINK                                         ELTASSTS
01234                 PROGRAM('ELGMAXIM')                               ELTASSTS
01235                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01236                 END-EXEC                                          ELTASSTS
01237         ELSE                                                      ELTASSTS
01238            NEXT SENTENCE                                          ELTASSTS
01239      ELSE                                                         ELTASSTS
01240         SET PLT-INDEX2 TO 2                                       ELTASSTS
01241         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01242            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01243                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01244          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
01245                                              KWA-GCTABULR-KEY     ELTASSTS
01246            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
01247            IF IOP-RC-OK                                           ELTASSTS
01248               EXEC CICS LINK                                      ELTASSTS
01249                    PROGRAM('ELGMAXIM')                            ELTASSTS
01250                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
01251                    END-EXEC.                                      ELTASSTS
01252 **                                                               |ELTASSTS
01253 **---------------------------------------------------------------+ELTASSTS
01254                                                                   ELTASSTS
01255 **---------------------------------------------------------------+ELTASSTS
01256 **                                                               |ELTASSTS
01257 **                  # A C L   T A B U L A R                      |ELTASSTS
01258      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01259      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01260         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01261                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01262         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
01263                                              KWA-GCTABULR-KEY     ELTASSTS
01264         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
01265         IF IOP-RC-OK                                              ELTASSTS
01266            EXEC CICS LINK                                         ELTASSTS
01267                 PROGRAM('ELGCOINS')                               ELTASSTS
01268                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01269                 END-EXEC                                          ELTASSTS
01270         ELSE                                                      ELTASSTS
01271            NEXT SENTENCE                                          ELTASSTS
01272      ELSE                                                         ELTASSTS
01273         SET PLT-INDEX2 TO 2                                       ELTASSTS
01274         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01275            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01276                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01277          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
01278                                              KWA-GCTABULR-KEY     ELTASSTS
01279            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
01280            IF IOP-RC-OK                                           ELTASSTS
01281               EXEC CICS LINK                                      ELTASSTS
01282                    PROGRAM('ELGCOINS')                            ELTASSTS
01283                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
01284                    END-EXEC.                                      ELTASSTS
01285 **                                                               |ELTASSTS
01286 **---------------------------------------------------------------+ELTASSTS
01287                                                                   ELTASSTS
01288 **---------------------------------------------------------------+ELTASSTS
01289 **                                                               |ELTASSTS
01290 **                  # A D L   T A B U L A R                      |ELTASSTS
01291      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01292      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01293         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01294                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01295         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
01296                                              KWA-GCTABULR-KEY     ELTASSTS
01297         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
01298         IF IOP-RC-OK                                              ELTASSTS
01299            EXEC CICS LINK                                         ELTASSTS
01300                 PROGRAM('ELGDEDBL')                               ELTASSTS
01301                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01302                 END-EXEC                                          ELTASSTS
01303         ELSE                                                      ELTASSTS
01304            NEXT SENTENCE                                          ELTASSTS
01305      ELSE                                                         ELTASSTS
01306         SET PLT-INDEX2 TO 2                                       ELTASSTS
01307         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01308            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01309                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01310          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
01311                                              KWA-GCTABULR-KEY     ELTASSTS
01312            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
01313            IF IOP-RC-OK                                           ELTASSTS
01314               EXEC CICS LINK                                      ELTASSTS
01315                    PROGRAM('ELGDEDBL')                            ELTASSTS
01316                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
01317                    END-EXEC.                                      ELTASSTS
01318 **                                                               |ELTASSTS
01319 **---------------------------------------------------------------+ELTASSTS
01320                                                                   ELTASSTS
01321 **---------------------------------------------------------------+ELTASSTS
01322 **                                                               |ELTASSTS
01323 **                  # A O L   T A B U L A R                      |ELTASSTS
01324      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01325      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
01326         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
01327                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01328         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
01329                                              KWA-GCTABULR-KEY     ELTASSTS
01330         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
01331         IF IOP-RC-OK                                              ELTASSTS
01332            EXEC CICS LINK                                         ELTASSTS
01333                 PROGRAM('ELGOUTPX')                               ELTASSTS
01334                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01335                 END-EXEC                                          ELTASSTS
01336         ELSE                                                      ELTASSTS
01337            NEXT SENTENCE                                          ELTASSTS
01338      ELSE                                                         ELTASSTS
01339         SET PLT-INDEX2 TO 2                                       ELTASSTS
01340         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
01341            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
01342                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
01343          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
01344                                              KWA-GCTABULR-KEY     ELTASSTS
01345            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
01346            IF IOP-RC-OK                                           ELTASSTS
01347               EXEC CICS LINK                                      ELTASSTS
01348                    PROGRAM('ELGOUTPX')                            ELTASSTS
01349                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
01350                    END-EXEC.                                      ELTASSTS
01351 **                                                               |ELTASSTS
01352 **---------------------------------------------------------------+ELTASSTS
01353                                                                   ELTASSTS
01354      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
01355      MOVE WS-STAFF-PROV-NOT-AVAIL TO COF-DTL-LINE(2).             ELTASSTS
01356      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
01357      EXEC CICS LINK                                               ELTASSTS
01358           PROGRAM('ELUOUTPT')                                     ELTASSTS
01359           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01360           END-EXEC.                                               ELTASSTS
01361                                                                   ELTASSTS
01362 **---------------------------------------------------------------+ELTASSTS
01363 **                                                               |ELTASSTS
01364 **         P A Y M E N T  C O N S I D E R A T I O N  T E X T     |ELTASSTS
01365      INITIALIZE TCAR-FROM-AREA.                                   ELTASSTS
01366      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTASSTS
01367             WS-PAY-CONSDR-TEXT2                                   ELTASSTS
01368                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTASSTS
01369      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTASSTS
01370      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTASSTS
01371      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTASSTS
01372                                TCAR-OUTPUT-FIELD-2-LEN.           ELTASSTS
01373      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTASSTS
01374      IF WS-CIA > 17                                               ELTASSTS
01375            MOVE WS-CIA TO COF-NBR-DTL-LINES                       ELTASSTS
01376            EXEC CICS LINK                                         ELTASSTS
01377                PROGRAM('ELUOUTPT')                                ELTASSTS
01378                COMMAREA(DFHCOMMAREA)                              ELTASSTS
01379            END-EXEC                                               ELTASSTS
01380            MOVE +1            TO WS-CIA.                          ELTASSTS
01381      ADD +1                TO  WS-CIA.                            ELTASSTS
01382      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTASSTS
01383      ADD +1                TO  WS-CIA.                            ELTASSTS
01384      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTASSTS
01385      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTASSTS
01386      EXEC CICS LINK                                               ELTASSTS
01387           PROGRAM('ELUOUTPT')                                     ELTASSTS
01388           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01389           END-EXEC.                                               ELTASSTS
01390      MOVE +1            TO WS-CIA.                                ELTASSTS
01391 **                                                               |ELTASSTS
01392 **---------------------------------------------------------------+ELTASSTS
01393                                                                   ELTASSTS
01394  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTASSTS
01395                                                                   ELTASSTS
01396      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB              ELTASSTS
01397         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
01398         MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME               ELTASSTS
01399         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTASSTS
01400                                                    CMF-CODE-VALUE ELTASSTS
01401         MOVE SPACES TO WS-TEMP-TEXT-AREA                          ELTASSTS
01402         MOVE +58 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
01403         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
01404         MOVE ZERO TO PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)          ELTASSTS
01405         ADD  1 TO WS-SUB2                                         ELTASSTS
01406         IF WS-CIA > 20 OR = 20                                    ELTASSTS
01407            MOVE WS-CIA TO COF-NBR-DTL-LINES                       ELTASSTS
01408            EXEC CICS LINK                                         ELTASSTS
01409                 PROGRAM('ELUOUTPT')                               ELTASSTS
01410                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
01411                 END-EXEC                                          ELTASSTS
01412            MOVE +1 TO WS-CIA.                                     ELTASSTS
01413                                                                   ELTASSTS
01414  2090-PROBLEM-WITH-INDICES.                                       ELTASSTS
01415                                                                   ELTASSTS
01416      MOVE 'PROBLEM W/ INDICES' TO COF-DTL-LINE(1).                ELTASSTS
01417      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTASSTS
01418                                                                   ELTASSTS
01419      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTASSTS
01420      MOVE 'P' TO COF-FUNCTION.                                    ELTASSTS
01421                                                                   ELTASSTS
01422      EXEC CICS LINK                                               ELTASSTS
01423           PROGRAM('ELUOUTPT')                                     ELTASSTS
01424           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01425           END-EXEC.                                               ELTASSTS
01426                                                                   ELTASSTS
01427  2099-EXIT.            EXIT.                                      ELTASSTS
01428                                                                   ELTASSTS
01429 /                                                                 ELTASSTS
01430 ******************************************************************ELTASSTS
01431 ** INTERFACE TO CODES MANUAL DATABASE FOR LONG DESCRIPTION      **ELTASSTS
01432 ******************************************************************ELTASSTS
01433  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTASSTS
01434      MOVE '2100' TO WS-PARA-ID2.                                  ELTASSTS
01435                                                                   ELTASSTS
01436      INITIALIZE CMF-RETURN-CODE,                                  ELTASSTS
01437                 TCAR-FROM-AREA.                                   ELTASSTS
01438                                                                   ELTASSTS
01439      EXEC CICS LINK                                               ELTASSTS
01440           PROGRAM('ELUCMIF')                                      ELTASSTS
01441           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01442           END-EXEC.                                               ELTASSTS
01443                                                                   ELTASSTS
01444      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTASSTS
01445      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01446          ADDRESS OF CMF-DESCR.                                    ELTASSTS
01447                                                                   ELTASSTS
01448      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTASSTS
01449         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTASSTS
01450         STRING WS-TEMP-TEXT-AREA, ' ',                            ELTASSTS
01451            CMF-DESCR-LINE(1), ' ',                                ELTASSTS
01452            CMF-DESCR-LINE(2), ' ',                                ELTASSTS
01453            CMF-DESCR-LINE(3)                                      ELTASSTS
01454            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTASSTS
01455      ELSE                                                         ELTASSTS
01456         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTASSTS
01457         STRING CMF-DESCR-LINE(1), ' ',                            ELTASSTS
01458            CMF-DESCR-LINE(2), ' ',                                ELTASSTS
01459            CMF-DESCR-LINE(3)                                      ELTASSTS
01460            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTASSTS
01461                                                                   ELTASSTS
01462      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTASSTS
01463                                                                   ELTASSTS
01464      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTASSTS
01465      MOVE +2 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTASSTS
01466 ******************************************************************ELTASSTS
01467 ** TEMPORARY FIX BY ALIDA TO IMPROVE APPEARANCE OF OUTPUT       **ELTASSTS
01468 ******************************************************************ELTASSTS
01469 *    MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTASSTS
01470      MOVE +63 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTASSTS
01471      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTASSTS
01472                                                                   ELTASSTS
01473      IF WS-MOVE-LINES-TO-CIA                                      ELTASSTS
01474         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTASSTS
01475            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTASSTS
01476                                             WS-TEMP-NOT-USED-CNT  ELTASSTS
01477            MOVE '2150' TO WS-PARA-ID2                             ELTASSTS
01478            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTASSTS
01479               VARYING  WS-SUB1 FROM 1 BY 1                        ELTASSTS
01480               UNTIL  WS-TEMP-NOT-USED-CNT > +78                   ELTASSTS
01481            MOVE '2100' TO WS-PARA-ID2                             ELTASSTS
01482            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTASSTS
01483            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTASSTS
01484            ADD +1 TO WS-CIA                                       ELTASSTS
01485         ELSE                                                      ELTASSTS
01486            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)          ELTASSTS
01487            ADD +1 TO WS-CIA.                                      ELTASSTS
01488                                                                   ELTASSTS
01489      IF WS-MOVE-LINES-TO-CIA                                      ELTASSTS
01490         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTASSTS
01491 ******************************************************************ELTASSTS
01492 ** TEMPORARY FIX BY ALIDA TO IMPROVE APPEARANCE OF OUTPUT       **ELTASSTS
01493 ******************************************************************ELTASSTS
01494 *          MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTASSTS
01495            MOVE TCAR-OPF-DATA(2) TO WS-TRUNC-TEXT                 ELTASSTS
01496            MOVE WS-BLANK-PREFIX-DET-LINE                          ELTASSTS
01497                                  TO COF-DTL-LINE(WS-CIA)          ELTASSTS
01498            ADD +1 TO WS-CIA                                       ELTASSTS
01499         ELSE                                                      ELTASSTS
01500            NEXT SENTENCE                                          ELTASSTS
01501      ELSE                                                         ELTASSTS
01502         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTASSTS
01503                                                                   ELTASSTS
01504      GO TO 2199-EXIT.                                             ELTASSTS
01505                                                                   ELTASSTS
01506  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTASSTS
01507      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTASSTS
01508      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTASSTS
01509          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT).                 ELTASSTS
01510                                                                   ELTASSTS
01511  2199-EXIT.                                                       ELTASSTS
01512      EXIT.                                                        ELTASSTS
01513 /                                                                 ELTASSTS
01514 ******************************************************************ELTASSTS
01515 ** INTERFACE TO CODES MANUAL DATABASE FOR LONG DESCRIPTION      **ELTASSTS
01516 ** (PERCENTAGE FIELD)                                           **ELTASSTS
01517 ******************************************************************ELTASSTS
01518  2200-CODES-MANUAL-WITH-PERCENT SECTION.                          ELTASSTS
01519      MOVE '2200' TO WS-PARA-ID2.                                  ELTASSTS
01520                                                                   ELTASSTS
01521      INITIALIZE CMF-RETURN-CODE,                                  ELTASSTS
01522                 TCAR-FROM-AREA.                                   ELTASSTS
01523                                                                   ELTASSTS
01524      EXEC CICS LINK                                               ELTASSTS
01525           PROGRAM('ELUCMIF')                                      ELTASSTS
01526           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01527           END-EXEC.                                               ELTASSTS
01528                                                                   ELTASSTS
01529      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTASSTS
01530      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01531          ADDRESS OF CMF-DESCR.                                    ELTASSTS
01532                                                                   ELTASSTS
01533      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTASSTS
01534         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTASSTS
01535         STRING WS-TEMP-TEXT-AREA, ' ',                            ELTASSTS
01536            CMF-DESCR-LINE(1), ' ',                                ELTASSTS
01537            CMF-DESCR-LINE(2), ' ',                                ELTASSTS
01538            CMF-DESCR-LINE(3), ' ', WS-PERCENT-FLD                 ELTASSTS
01539            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTASSTS
01540      ELSE                                                         ELTASSTS
01541         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTASSTS
01542         STRING CMF-DESCR-LINE(1), ' ',                            ELTASSTS
01543            CMF-DESCR-LINE(2), ' ',                                ELTASSTS
01544            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTASSTS
01545            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTASSTS
01546                                                                   ELTASSTS
01547      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTASSTS
01548                                                                   ELTASSTS
01549      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTASSTS
01550      MOVE +4 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTASSTS
01551      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN,                         ELTASSTS
01552                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTASSTS
01553                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTASSTS
01554      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTASSTS
01555                                                                   ELTASSTS
01556      IF WS-MOVE-LINES-TO-CIA                                      ELTASSTS
01557         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTASSTS
01558            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTASSTS
01559                                             WS-TEMP-NOT-USED-CNT  ELTASSTS
01560            MOVE '2250' TO WS-PARA-ID2                             ELTASSTS
01561            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTASSTS
01562               VARYING  WS-SUB1 FROM 1 BY 1                        ELTASSTS
01563               UNTIL  WS-TEMP-NOT-USED-CNT > +78                   ELTASSTS
01564            MOVE '2200' TO WS-PARA-ID2                             ELTASSTS
01565            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTASSTS
01566            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTASSTS
01567            ADD +1 TO WS-CIA                                       ELTASSTS
01568         ELSE                                                      ELTASSTS
01569            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)          ELTASSTS
01570            ADD +1 TO WS-CIA.                                      ELTASSTS
01571                                                                   ELTASSTS
01572      IF WS-MOVE-LINES-TO-CIA                                      ELTASSTS
01573         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTASSTS
01574            MOVE '2260' TO WS-PARA-ID2                             ELTASSTS
01575            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTASSTS
01576               VARYING  WS-SUB1 FROM 2 BY 1                        ELTASSTS
01577               UNTIL  WS-SUB1 > TCAR-OUTPUT-FIELDS-USED            ELTASSTS
01578         ELSE                                                      ELTASSTS
01579            NEXT SENTENCE                                          ELTASSTS
01580      ELSE                                                         ELTASSTS
01581         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTASSTS
01582                                                                   ELTASSTS
01583      GO TO 2299-EXIT.                                             ELTASSTS
01584                                                                   ELTASSTS
01585  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTASSTS
01586      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTASSTS
01587      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTASSTS
01588                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTASSTS
01589                                                                   ELTASSTS
01590  2260-MOVE-LINES-TO-CIA.                                          ELTASSTS
01591      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTASSTS
01592      ADD +1 TO WS-CIA.                                            ELTASSTS
01593                                                                   ELTASSTS
01594  2299-EXIT.           EXIT.                                       ELTASSTS
01595                                                                   ELTASSTS
01596 /                                                                 ELTASSTS
01597 ***************************************************************** ELTASSTS
01598 *            G E T   T A B U L A R   R E C O R D                * ELTASSTS
01599 *                                                               * ELTASSTS
01600 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,    * ELTASSTS
01601 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED      * ELTASSTS
01602 *  TO DISPLAY.                                                  * ELTASSTS
01603 *                                                               * ELTASSTS
01604 ***************************************************************** ELTASSTS
01605  2300-GET-TABULAR-RECORD SECTION.                                 ELTASSTS
01606      MOVE '2300' TO WS-PARA-ID2.                                  ELTASSTS
01607                                                                   ELTASSTS
01608      SET CIA-GCTABULR-DDN TO TRUE.                                ELTASSTS
01609      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01610          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTASSTS
01611      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTASSTS
01612      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTASSTS
01613      SET IOP-RD                          TO TRUE.                 ELTASSTS
01614      SET IOP-FCQ-NONE                    TO TRUE.                 ELTASSTS
01615      SET IOP-KVQ-NONE                    TO TRUE.                 ELTASSTS
01616      EXEC CICS LINK                                               ELTASSTS
01617           PROGRAM('ELUIOPGM')                                     ELTASSTS
01618           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01619           END-EXEC.                                               ELTASSTS
01620                                                                   ELTASSTS
01621      IF IOP-RC-NOTFND                                             ELTASSTS
01622         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTASSTS
01623         EXEC CICS ABEND                                           ELTASSTS
01624                   ABCODE(CIA-ABCODE)                              ELTASSTS
01625         END-EXEC.                                                 ELTASSTS
01626                                                                   ELTASSTS
01627      IF NOT IOP-RC-OK                                             ELTASSTS
01628         SET CIA-AB-CRITIO          TO TRUE                        ELTASSTS
01629         EXEC CICS ABEND                                           ELTASSTS
01630                   ABCODE(CIA-ABCODE)                              ELTASSTS
01631         END-EXEC.                                                 ELTASSTS
01632                                                                   ELTASSTS
01633  2399-EXIT.                                                       ELTASSTS
01634      EXIT.                                                        ELTASSTS
01635 /                                                                 ELTASSTS
01636 ******************************************************************ELTASSTS
01637 *            P R O F E S S I O N A L   O P   R T N E             *ELTASSTS
01638 *                                                                *ELTASSTS
01639 *          THIS ROUTINE HAS A NUMBER OF STEPS.                   *ELTASSTS
01640 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.          *ELTASSTS
01641 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE  *ELTASSTS
01642 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL*ELTASSTS
01643 *  END OF PAGE AND EXIT THIS ROUTINE.                            *ELTASSTS
01644 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING    *ELTASSTS
01645 *  MODULE.                                                       *ELTASSTS
01646 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A  *ELTASSTS
01647 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                 *ELTASSTS
01648 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.         *ELTASSTS
01649 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE    *ELTASSTS
01650 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                    *ELTASSTS
01651 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR        *ELTASSTS
01652 *  BENEFITS HAVE BEEN DISPLAYED.                                 *ELTASSTS
01653 *                                                                *ELTASSTS
01654 ******************************************************************ELTASSTS
01655  4000-PROFESSIONAL-OP-RTNE SECTION.                               ELTASSTS
01656      MOVE '4000' TO WS-PARA-ID1.                                  ELTASSTS
01657                                                                   ELTASSTS
01658      MOVE 'Y' TO WS-FIRSTTIME-IND.                                ELTASSTS
01659      MOVE 'P' TO COF-FUNCTION.                                    ELTASSTS
01660      MOVE ZERO TO COF-NBR-HDR-LINES,                              ELTASSTS
01661                     COF-NBR-DTL-LINES.                            ELTASSTS
01662      EXEC CICS LINK                                               ELTASSTS
01663           PROGRAM('ELUOUTPT')                                     ELTASSTS
01664           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01665           END-EXEC.                                               ELTASSTS
01666                                                                   ELTASSTS
01667      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTASSTS
01668      MOVE WS-HDR-2-PROF-OP TO COF-HDR-LINE(2).                    ELTASSTS
01669                                                                   ELTASSTS
01670      MOVE WS-PROF-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTASSTS
01671                                                                   ELTASSTS
01672      PERFORM 4010-MOVE-IN-PROF-OP                                 ELTASSTS
01673         VARYING  WS-SUB FROM +1 BY +1                             ELTASSTS
01674         UNTIL WS-SUB > WS-PROF-OP-CNT.                            ELTASSTS
01675                                                                   ELTASSTS
01676      GO TO 4020-CALL-COVERAGE.                                    ELTASSTS
01677  4010-MOVE-IN-PROF-OP.                                            ELTASSTS
01678      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
01679      MOVE WS-PROF-OP-LIST(WS-SUB) TO                              ELTASSTS
01680                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTASSTS
01681      MOVE ZERO TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),            ELTASSTS
01682                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTASSTS
01683                                                                   ELTASSTS
01684  4020-CALL-COVERAGE.                                              ELTASSTS
01685      MOVE '4020' TO WS-PARA-ID1.                                  ELTASSTS
01686      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
01687      MOVE WS-SEE-CCP TO COF-DTL-LINE(2).                          ELTASSTS
01688      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
01689      EXEC CICS LINK                                               ELTASSTS
01690           PROGRAM('ELUOUTPT')                                     ELTASSTS
01691           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01692           END-EXEC.                                               ELTASSTS
01693                                                                   ELTASSTS
01694      MOVE WS-TOPIC-PHRASE TO SSB-TOPIC-PHRASE.                    ELTASSTS
01695                                                                   ELTASSTS
01696      EXEC CICS LINK                                               ELTASSTS
01697           PROGRAM('ELGCOVER')                                     ELTASSTS
01698           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01699           END-EXEC.                                               ELTASSTS
01700                                                                   ELTASSTS
01701      ADD +1 TO  COF-NBR-DTL-LINES.                                ELTASSTS
01702      MOVE LOW-VALUES TO COF-DTL-LINE(COF-NBR-DTL-LINES).          ELTASSTS
01703      EXEC CICS LINK                                               ELTASSTS
01704           PROGRAM('ELUOUTPT')                                     ELTASSTS
01705           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01706           END-EXEC.                                               ELTASSTS
01707                                                                   ELTASSTS
01708      IF PVN-COVG-NONE                                             ELTASSTS
01709         GO TO 4099-EXIT.                                          ELTASSTS
01710                                                                   ELTASSTS
01711      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTASSTS
01712                                                                   ELTASSTS
01713      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTASSTS
01714                  PSP-PROVN-PRICING-METHD,                         ELTASSTS
01715                  PSP-ADDITIONAL-PRICING-PRCNT,                    ELTASSTS
01716                  PSP-TRANSF-OTHER-RESP-IND,                       ELTASSTS
01717                  PSP-VARIABLE-INDEMNITY-PRCNT,                    ELTASSTS
01718                  PSP-SPILL-OVER-COINS-APL-IND,                    ELTASSTS
01719                  PSP-SPILL-OVER-DED-APL-IND,                      ELTASSTS
01720                  PSP-BEN-TAB-PROVN-ID-AAR,                        ELTASSTS
01721                  PSP-BEN-TAB-PROVN-ID-ABM,                        ELTASSTS
01722                  PSP-BEN-TAB-PROVN-ID-ACL,                        ELTASSTS
01723                  PSP-BEN-TAB-PROVN-ID-ADL,                        ELTASSTS
01724                  PSP-BEN-TAB-PROVN-ID-AOL,                        ELTASSTS
01725                  PSP-BEN-TAB-PROVN-ID-PPF,                        ELTASSTS
01726                  PSC-ELIG-METHD-OF-TREAT-IND,                     ELTASSTS
01727                  PSC-PRIM-SURG-DEPEND-IND,                        ELTASSTS
01728                  PSC-PRIM-SURG-DPD-PAY-PCT,                       ELTASSTS
01729                  PSC-BEN-SCOPE-ID.                                ELTASSTS
01730                                                                   ELTASSTS
01731      EXEC CICS LINK                                               ELTASSTS
01732           PROGRAM('ELUPLGRP')                                     ELTASSTS
01733           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01734           END-EXEC.                                               ELTASSTS
01735                                                                   ELTASSTS
01736      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTASSTS
01737      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01738          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTASSTS
01739                                                                   ELTASSTS
01740      PERFORM 4030-FIND-FIRST-NONZERO                              ELTASSTS
01741         VARYING WS-SUB FROM +1 BY +1                              ELTASSTS
01742         UNTIL WS-SUB > WS-PROF-OP-CNT.                            ELTASSTS
01743                                                                   ELTASSTS
01744      GO TO 4099-EXIT.                                             ELTASSTS
01745  4030-FIND-FIRST-NONZERO.                                         ELTASSTS
01746      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
01747      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTASSTS
01748         NEXT SENTENCE                                             ELTASSTS
01749      ELSE                                                         ELTASSTS
01750         PERFORM 4040-BUILD-SCREEN-LINES.                          ELTASSTS
01751                                                                   ELTASSTS
01752  4040-BUILD-SCREEN-LINES.                                         ELTASSTS
01753      MOVE '4040' TO WS-PARA-ID1.                                  ELTASSTS
01754                                                                   ELTASSTS
01755      SET PLT-INDEX1  TO                                           ELTASSTS
01756                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTASSTS
01757      IF WS-NOT-FIRST-TIME                                         ELTASSTS
01758         MOVE 'P' TO COF-FUNCTION                                  ELTASSTS
01759         EXEC CICS LINK                                            ELTASSTS
01760              PROGRAM('ELUOUTPT')                                  ELTASSTS
01761              COMMAREA(DFHCOMMAREA)                                ELTASSTS
01762              END-EXEC                                             ELTASSTS
01763      ELSE                                                         ELTASSTS
01764         MOVE 'N' TO WS-FIRSTTIME-IND.                             ELTASSTS
01765                                                                   ELTASSTS
01766      MOVE +1 TO WS-CIA.                                           ELTASSTS
01767                                                                   ELTASSTS
01768      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
01769         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
01770            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTASSTS
01771            GO TO 4099-EXIT                                        ELTASSTS
01772         ELSE                                                      ELTASSTS
01773            SET PLT-INDEX2 TO 2                                    ELTASSTS
01774      ELSE                                                         ELTASSTS
01775         SET PLT-INDEX2 TO 1.                                      ELTASSTS
01776                                                                   ELTASSTS
01777 **---------------------------------------------------------------+ELTASSTS
01778 **                                                               |ELTASSTS
01779 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTASSTS
01780      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE(WS-CIA).               ELTASSTS
01781      ADD  +1 TO WS-CIA.                                           ELTASSTS
01782      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTASSTS
01783      MOVE ZERO TO WS-SUB2.                                        ELTASSTS
01784      MOVE '4050' TO WS-PARA-ID1.                                  ELTASSTS
01785      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTASSTS
01786         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTASSTS
01787         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.                ELTASSTS
01788      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTASSTS
01789                                                                   ELTASSTS
01790      ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES.                     ELTASSTS
01791      MOVE 1 TO WS-CIA.                                            ELTASSTS
01792      EXEC CICS LINK                                               ELTASSTS
01793           PROGRAM('ELUOUTPT')                                     ELTASSTS
01794           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
01795           END-EXEC.                                               ELTASSTS
01796 **                                                               |ELTASSTS
01797 **---------------------------------------------------------------+ELTASSTS
01798                                                                   ELTASSTS
01799 ******************************************************************ELTASSTS
01800 ** SUPPRESS USE OF CONTRACT FIELDS IF THE ASSISTANT SURGEON     **ELTASSTS
01801 ** BENEFIT PROVISION DOES NOT EXIST FOR THAT LINE OF BUSINESS.  **ELTASSTS
01802 ******************************************************************ELTASSTS
01803      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTASSTS
01804      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01805          ADDRESS OF BASIC-CONTRACT-REC.                           ELTASSTS
01806      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
01807         MOVE 'B' TO WS-BASIC-OR-CMM-IND                           ELTASSTS
01808      ELSE                                                         ELTASSTS
01809         MOVE 'N' TO WS-BASIC-OR-CMM-IND.                          ELTASSTS
01810                                                                   ELTASSTS
01811      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTASSTS
01812      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTASSTS
01813          ADDRESS OF SUPP-CONTRACT-REC.                            ELTASSTS
01814      IF NOT CIA-RC-PTR-NULL                                       ELTASSTS
01815         MOVE 'S' TO WS-SMM-IND                                    ELTASSTS
01816      ELSE                                                         ELTASSTS
01817         MOVE 'N' TO WS-SMM-IND.                                   ELTASSTS
01818                                                                   ELTASSTS
01819      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
01820         MOVE 'N' TO WS-BASIC-OR-CMM-IND                           ELTASSTS
01821         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
01822            MOVE 'N' TO WS-SMM-IND                                 ELTASSTS
01823         ELSE                                                      ELTASSTS
01824            NEXT SENTENCE                                          ELTASSTS
01825      ELSE                                                         ELTASSTS
01826         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZEROES           ELTASSTS
01827            MOVE 'N' TO WS-SMM-IND.                                ELTASSTS
01828                                                                   ELTASSTS
01829 **---------------------------------------------------------------+ELTASSTS
01830 **                                                               |ELTASSTS
01831 **        P L A C E   O F   T R E A T M E N T                    |ELTASSTS
01832 **                                                               |ELTASSTS
01833                                                                   ELTASSTS
01834      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01835      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01836          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
01837                  NOT = ZERO                                       ELTASSTS
01838              MOVE WS-SERVICES-RENDERED                            ELTASSTS
01839                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
01840              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
01841              ADD +1 TO WS-CIA.                                    ELTASSTS
01842                                                                   ELTASSTS
01843      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01844      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01845          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
01846                  NOT = ZERO                                       ELTASSTS
01847              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTASSTS
01848                  MOVE WS-SERVICES-RENDERED                        ELTASSTS
01849                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
01850                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
01851                  ADD +1 TO WS-CIA.                                ELTASSTS
01852                                                                   ELTASSTS
01853      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01854      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01855          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
01856                  NOT = ZERO                                       ELTASSTS
01857              MOVE 'BP' TO CMF-RECORD-PREFIX                       ELTASSTS
01858              MOVE 'PLACE-TREAT-ELIG-IND'                          ELTASSTS
01859                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
01860              MOVE PLP-PLACE-TREAT-ELIG-IND                        ELTASSTS
01861                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
01862                  TO CMF-CODE-VALUE                                ELTASSTS
01863               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
01864               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01865               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
01866                                                                   ELTASSTS
01867      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01868      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01869          IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
01870                  NOT = ZERO                                       ELTASSTS
01871              MOVE 'BP' TO CMF-RECORD-PREFIX                       ELTASSTS
01872              MOVE 'PLACE-TREAT-ELIG-IND'                          ELTASSTS
01873                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
01874              MOVE PLP-PLACE-TREAT-ELIG-IND                        ELTASSTS
01875                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
01876                  TO CMF-CODE-VALUE                                ELTASSTS
01877               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
01878               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01879               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
01880                                                                   ELTASSTS
01881      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
01882         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
01883         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01884         MOVE 1 TO WS-CIA                                          ELTASSTS
01885         EXEC CICS LINK                                            ELTASSTS
01886              PROGRAM('ELUOUTPT')                                  ELTASSTS
01887              COMMAREA(DFHCOMMAREA)                                ELTASSTS
01888              END-EXEC.                                            ELTASSTS
01889 **                                                               |ELTASSTS
01890 **---------------------------------------------------------------+ELTASSTS
01891                                                                   ELTASSTS
01892 **---------------------------------------------------------------+ELTASSTS
01893 **                                                               |ELTASSTS
01894 **            B E N E F I T   S C O P E   I D                    |ELTASSTS
01895      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01896      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01897         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
01898            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01899                                         '0000' AND NOT = '00  '   ELTASSTS
01900               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
01901               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
01902               ADD  +1 TO WS-CIA                                   ELTASSTS
01903            ELSE                                                   ELTASSTS
01904               NEXT SENTENCE                                       ELTASSTS
01905         ELSE                                                      ELTASSTS
01906            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01907                                         '0000' AND NOT = '00  '   ELTASSTS
01908               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
01909               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
01910               ADD  +1 TO WS-CIA.                                  ELTASSTS
01911                                                                   ELTASSTS
01912      SET PLT-INDEX2 TO 2.                                         ELTASSTS
01913      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01914         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
01915            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01916                                         '0000' AND NOT = '00  '   ELTASSTS
01917               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
01918               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
01919               ADD  +1 TO WS-CIA                                   ELTASSTS
01920            ELSE                                                   ELTASSTS
01921               NEXT SENTENCE                                       ELTASSTS
01922         ELSE                                                      ELTASSTS
01923            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01924                                         '0000' AND NOT = '00  '   ELTASSTS
01925               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTASSTS
01926               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTASSTS
01927               ADD  +1 TO WS-CIA.                                  ELTASSTS
01928                                                                   ELTASSTS
01929      SET PLT-INDEX2 TO 1.                                         ELTASSTS
01930      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01931         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
01932            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01933                                         '0000' AND NOT = '00  '   ELTASSTS
01934               MOVE 'BPC' TO CMF-RECORD-PREFIX                     ELTASSTS
01935               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
01936               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
01937                                                    CMF-CODE-VALUE ELTASSTS
01938               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
01939               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01940               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTASSTS
01941            ELSE                                                   ELTASSTS
01942               NEXT SENTENCE                                       ELTASSTS
01943         ELSE                                                      ELTASSTS
01944            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01945                                         '0000' AND NOT = '00  '   ELTASSTS
01946               MOVE 'BPE' TO CMF-RECORD-PREFIX                     ELTASSTS
01947               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
01948               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
01949                                                    CMF-CODE-VALUE ELTASSTS
01950               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTASSTS
01951               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01952               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
01953                                                                   ELTASSTS
01954      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
01955         SET PLT-INDEX2 TO 2                                       ELTASSTS
01956         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTASSTS
01957            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01958                                         '0000' AND NOT = '00  '   ELTASSTS
01959               MOVE 'BPC' TO CMF-RECORD-PREFIX                     ELTASSTS
01960               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
01961               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
01962                                                    CMF-CODE-VALUE ELTASSTS
01963               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
01964               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01965               PERFORM 2100-CALL-CODES-MANUAL-LONG                 ELTASSTS
01966            ELSE                                                   ELTASSTS
01967               NEXT SENTENCE                                       ELTASSTS
01968         ELSE                                                      ELTASSTS
01969            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTASSTS
01970                                         '0000' AND NOT = '00  '   ELTASSTS
01971               MOVE 'BPE' TO CMF-RECORD-PREFIX                     ELTASSTS
01972               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTASSTS
01973               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTASSTS
01974                                                    CMF-CODE-VALUE ELTASSTS
01975               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
01976               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTASSTS
01977               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTASSTS
01978                                                                   ELTASSTS
01979      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
01980         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
01981         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
01982         MOVE 1 TO WS-CIA                                          ELTASSTS
01983         EXEC CICS LINK                                            ELTASSTS
01984              PROGRAM('ELUOUTPT')                                  ELTASSTS
01985              COMMAREA(DFHCOMMAREA)                                ELTASSTS
01986              END-EXEC.                                            ELTASSTS
01987 **                                                               |ELTASSTS
01988 **---------------------------------------------------------------+ELTASSTS
01989                                                                   ELTASSTS
01990      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
01991         SET PLT-INDEX2 TO 2                                       ELTASSTS
01992      ELSE                                                         ELTASSTS
01993         SET PLT-INDEX2 TO 1.                                      ELTASSTS
01994                                                                   ELTASSTS
01995 **---------------------------------------------------------------+ELTASSTS
01996 **                                                               |ELTASSTS
01997 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTASSTS
01998 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTASSTS
01999 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTASSTS
02000      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02001      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02002         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
02003                                                              '19' ELTASSTS
02004         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02005         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA)                ELTASSTS
02006         ADD +1 TO WS-CIA.                                         ELTASSTS
02007                                                                   ELTASSTS
02008      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02009      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02010         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
02011                                                        '19' AND   ELTASSTS
02012         NOT WS-ADD-A-BLANK-LINE                                   ELTASSTS
02013         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02014         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA)                ELTASSTS
02015         ADD +1 TO WS-CIA.                                         ELTASSTS
02016                                                                   ELTASSTS
02017      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02018      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02019         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTASSTS
02020                            AND                                    ELTASSTS
02021         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02022         SET PLT-INDEX2 TO 2                                       ELTASSTS
02023         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
02024                                                              ZERO ELTASSTS
02025            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTASSTS
02026            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTASSTS
02027            ADD +1 TO WS-CIA.                                      ELTASSTS
02028                                                                   ELTASSTS
02029      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02030      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02031         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTASSTS
02032                            AND                                    ELTASSTS
02033         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTASSTS
02034         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTASSTS
02035         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                     ELTASSTS
02036         ADD +1 TO WS-CIA.                                         ELTASSTS
02037                                                                   ELTASSTS
02038      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTASSTS
02039         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02040         SET PLT-INDEX2 TO 2                                       ELTASSTS
02041         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
02042                                                              ZERO ELTASSTS
02043            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTASSTS
02044            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTASSTS
02045            ADD +1 TO WS-CIA.                                      ELTASSTS
02046                                                                   ELTASSTS
02047      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02048      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02049         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTASSTS
02050                                                           = ZERO  ELTASSTS
02051            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02052                                                           = ZERO  ELTASSTS
02053               MOVE SPACES TO WS-PERCENT-FLD                       ELTASSTS
02054            ELSE                                                   ELTASSTS
02055               MOVE '%' TO WS-PERCENT-SIGN                         ELTASSTS
02056          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02057                                                 TO WS-PERCENTAGE  ELTASSTS
02058         ELSE                                                      ELTASSTS
02059            MOVE '%' TO WS-PERCENT-SIGN                            ELTASSTS
02060          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02061                                                TO WS-PERCENTAGE.  ELTASSTS
02062      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02063         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
02064                                             ZERO AND NOT = '19'   ELTASSTS
02065         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02066         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTASSTS
02067         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTASSTS
02068                                                    CMF-CODE-VALUE ELTASSTS
02069         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTASSTS
02070         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
02071         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTASSTS
02072                                                                   ELTASSTS
02073      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02074      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02075         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTASSTS
02076                                                               ZEROELTASSTS
02077            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02078                                                           = ZERO  ELTASSTS
02079               MOVE SPACES TO WS-PERCENT-FLD                       ELTASSTS
02080            ELSE                                                   ELTASSTS
02081               MOVE '%' TO WS-PERCENT-SIGN                         ELTASSTS
02082          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02083                                                 TO WS-PERCENTAGE  ELTASSTS
02084         ELSE                                                      ELTASSTS
02085            MOVE '%' TO WS-PERCENT-SIGN                            ELTASSTS
02086          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTASSTS
02087                                                TO WS-PERCENTAGE.  ELTASSTS
02088      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02089         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTASSTS
02090                                             ZERO AND NOT = '19'   ELTASSTS
02091         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02092         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTASSTS
02093         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTASSTS
02094                                                    CMF-CODE-VALUE ELTASSTS
02095         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTASSTS
02096         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
02097         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTASSTS
02098                                                                   ELTASSTS
02099      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
02100         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02101         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02102         MOVE 1 TO WS-CIA                                          ELTASSTS
02103         EXEC CICS LINK                                            ELTASSTS
02104              PROGRAM('ELUOUTPT')                                  ELTASSTS
02105              COMMAREA(DFHCOMMAREA)                                ELTASSTS
02106              END-EXEC.                                            ELTASSTS
02107 **                                                               |ELTASSTS
02108 **---------------------------------------------------------------+ELTASSTS
02109                                                                   ELTASSTS
02110      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
02111         SET PLT-INDEX2 TO 2                                       ELTASSTS
02112      ELSE                                                         ELTASSTS
02113         SET PLT-INDEX2 TO 1.                                      ELTASSTS
02114                                                                   ELTASSTS
02115                                                                   ELTASSTS
02116 **---------------------------------------------------------------+ELTASSTS
02117 **                                                               |ELTASSTS
02118 **     P R I M A R Y   S U R G E O N   D E P E N D E N C Y       |ELTASSTS
02119 **                     I N D I C A T O R                         |ELTASSTS
02120 **                                                               |ELTASSTS
02121 **     P R I M A R Y   S U R G E O N   D E P E N D E N C Y       |ELTASSTS
02122 **               P A Y M E N T   P E R C E N T                   |ELTASSTS
02123 **         ( P E R C E N T   O F   A L L O W A N C E )           |ELTASSTS
02124 **                                                               |ELTASSTS
02125 **                                                               |ELTASSTS
02126      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02127      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02128          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
02129                  NOT = ZERO                                       ELTASSTS
02130              MOVE WS-PRIM-SURG-DEPEND-IND                         ELTASSTS
02131                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
02132              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
02133              ADD +1 TO WS-CIA.                                    ELTASSTS
02134                                                                   ELTASSTS
02135      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02136      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02137          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
02138                  NOT = ZERO                                       ELTASSTS
02139              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTASSTS
02140                  MOVE WS-PRIM-SURG-DEPEND-IND                     ELTASSTS
02141                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
02142                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
02143                  ADD +1 TO WS-CIA.                                ELTASSTS
02144                                                                   ELTASSTS
02145      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02146                                                                   ELTASSTS
02147      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02148          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
02149                  NOT = ZERO                                       ELTASSTS
02150              MOVE 'BPC' TO CMF-RECORD-PREFIX                      ELTASSTS
02151              MOVE 'PRIM-SURG-DEPEND-IND'                          ELTASSTS
02152                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
02153              MOVE PLC-PRIM-SURG-DEPEND-IND                        ELTASSTS
02154                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
02155                  TO CMF-CODE-VALUE                                ELTASSTS
02156              MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
02157              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
02158              PERFORM 2100-CALL-CODES-MANUAL-LONG                  ELTASSTS
02159              IF  PLC-PRIM-SURG-DPD-PAY-PCT                        ELTASSTS
02160                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
02161                      IS NUMERIC                                   ELTASSTS
02162                    AND PLC-PRIM-SURG-DPD-PAY-PCT                  ELTASSTS
02163                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
02164                      NOT = ZEROES                                 ELTASSTS
02165                MOVE PLC-PRIM-SURG-DPD-PAY-PCT                     ELTASSTS
02166                    (PLT-INDEX1, PLT-INDEX2)                       ELTASSTS
02167                    TO WS-PCT-ALLOW                                ELTASSTS
02168                MOVE WS-PERCENT-OF-ALLOWANCE                       ELTASSTS
02169                    TO COF-DTL-LINE (WS-CIA)                       ELTASSTS
02170                ADD +1 TO WS-CIA.                                  ELTASSTS
02171                                                                   ELTASSTS
02172      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02173                                                                   ELTASSTS
02174      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTASSTS
02175          IF PLC-PRIM-SURG-DEPEND-IND (PLT-INDEX1, PLT-INDEX2)     ELTASSTS
02176                  NOT = ZERO                                       ELTASSTS
02177              MOVE 'BPC' TO CMF-RECORD-PREFIX                      ELTASSTS
02178              MOVE 'PRIM-SURG-DEPEND-IND'                          ELTASSTS
02179                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
02180              MOVE PLC-PRIM-SURG-DEPEND-IND                        ELTASSTS
02181                  (PLT-INDEX1, PLT-INDEX2)                         ELTASSTS
02182                  TO CMF-CODE-VALUE                                ELTASSTS
02183              MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                ELTASSTS
02184              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
02185              PERFORM 2100-CALL-CODES-MANUAL-LONG                  ELTASSTS
02186              IF  PLC-PRIM-SURG-DPD-PAY-PCT                        ELTASSTS
02187                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
02188                      IS NUMERIC                                   ELTASSTS
02189                    AND PLC-PRIM-SURG-DPD-PAY-PCT                  ELTASSTS
02190                      (PLT-INDEX1, PLT-INDEX2)                     ELTASSTS
02191                      NOT = ZEROES                                 ELTASSTS
02192                MOVE PLC-PRIM-SURG-DPD-PAY-PCT                     ELTASSTS
02193                    (PLT-INDEX1, PLT-INDEX2)                       ELTASSTS
02194                    TO WS-PCT-ALLOW                                ELTASSTS
02195                MOVE WS-PERCENT-OF-ALLOWANCE                       ELTASSTS
02196                    TO COF-DTL-LINE (WS-CIA)                       ELTASSTS
02197                ADD +1 TO WS-CIA.                                  ELTASSTS
02198                                                                   ELTASSTS
02199      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
02200         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02201         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02202         MOVE 1 TO WS-CIA                                          ELTASSTS
02203         EXEC CICS LINK                                            ELTASSTS
02204             PROGRAM('ELUOUTPT')                                   ELTASSTS
02205             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
02206             END-EXEC.                                             ELTASSTS
02207 **                                                               |ELTASSTS
02208 **                                                               |ELTASSTS
02209 **---------------------------------------------------------------+ELTASSTS
02210                                                                   ELTASSTS
02211                                                                   ELTASSTS
02212 **---------------------------------------------------------------+ELTASSTS
02213 **                                                               |ELTASSTS
02214 ** I F   T H E   S A M E   P R O V I D E R   I S   B I L L I N G |ELTASSTS
02215 ** P R I M   S U R G / A N E S T H E S I A / A S S T   S U R G   |ELTASSTS
02216 **                                                               |ELTASSTS
02217      IF (WS-BASIC-EXISTS OR WS-CMM-EXISTS)                        ELTASSTS
02218          IF GCT-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO              ELTASSTS
02219                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
02220              MOVE WS-IF-SAME-PVDR-BILLING                         ELTASSTS
02221                  TO COF-DTL-LINE (WS-CIA)                         ELTASSTS
02222              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTASSTS
02223              ADD +1 TO WS-CIA.                                    ELTASSTS
02224                                                                   ELTASSTS
02225      IF NOT WS-ADD-A-BLANK-LINE                                   ELTASSTS
02226          IF WS-SMM-EXISTS                                         ELTASSTS
02227              IF GCT3-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO         ELTASSTS
02228                      AND NOT = SPACES AND NOT = LOW-VALUES        ELTASSTS
02229                  MOVE WS-IF-SAME-PVDR-BILLING                     ELTASSTS
02230                      TO COF-DTL-LINE (WS-CIA)                     ELTASSTS
02231                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTASSTS
02232                  ADD +1 TO WS-CIA.                                ELTASSTS
02233                                                                   ELTASSTS
02234      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02235      IF (WS-BASIC-EXISTS OR WS-CMM-EXISTS)                        ELTASSTS
02236          IF GCT-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO              ELTASSTS
02237                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
02238              MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                 ELTASSTS
02239              MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                     ELTASSTS
02240                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
02241              MOVE GCT-SAME-PROV-SRG-ANS-SRG-AST                   ELTASSTS
02242                  TO CMF-CODE-VALUE                                ELTASSTS
02243              MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA               ELTASSTS
02244              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
02245              PERFORM 2100-CALL-CODES-MANUAL-LONG.                 ELTASSTS
02246                                                                   ELTASSTS
02247      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02248      IF WS-SMM-EXISTS                                             ELTASSTS
02249          IF GCT3-SAME-PROV-SRG-ANS-SRG-AST NOT = ZERO             ELTASSTS
02250                  AND NOT = SPACES AND NOT = LOW-VALUES            ELTASSTS
02251              MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                 ELTASSTS
02252              MOVE 'SAME-PROV-SRG-ANS-SRG-AST'                     ELTASSTS
02253                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTASSTS
02254              MOVE GCT3-SAME-PROV-SRG-ANS-SRG-AST                  ELTASSTS
02255                  TO CMF-CODE-VALUE                                ELTASSTS
02256              MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                ELTASSTS
02257              MOVE +63 TO WS-TEMP-NOT-USED-CNT                     ELTASSTS
02258              PERFORM 2100-CALL-CODES-MANUAL-LONG.                 ELTASSTS
02259                                                                   ELTASSTS
02260      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
02261         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02262         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02263         MOVE 1 TO WS-CIA                                          ELTASSTS
02264         EXEC CICS LINK                                            ELTASSTS
02265             PROGRAM('ELUOUTPT')                                   ELTASSTS
02266             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
02267             END-EXEC.                                             ELTASSTS
02268 **                                                               |ELTASSTS
02269 **                                                               |ELTASSTS
02270 **---------------------------------------------------------------+ELTASSTS
02271                                                                   ELTASSTS
02272                                                                   ELTASSTS
02273                                                                   ELTASSTS
02274 **---------------------------------------------------------------+ELTASSTS
02275 **                                                               |ELTASSTS
02276 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTASSTS
02277      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02278      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02279         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTASSTS
02280                                                        NOT = '0'  ELTASSTS
02281         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02282         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTASSTS
02283                                           CMF-ELEMENT-SYSTEM-NAME ELTASSTS
02284         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTASSTS
02285                                               TO CMF-CODE-VALUE   ELTASSTS
02286         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTASSTS
02287         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
02288         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02289         MOVE 1 TO WS-CIA                                          ELTASSTS
02290         EXEC CICS LINK                                            ELTASSTS
02291             PROGRAM('ELUOUTPT')                                   ELTASSTS
02292             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
02293             END-EXEC.                                             ELTASSTS
02294 **                                                               |ELTASSTS
02295 **---------------------------------------------------------------+ELTASSTS
02296                                                                   ELTASSTS
02297 **---------------------------------------------------------------+ELTASSTS
02298 **                                                               |ELTASSTS
02299 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTASSTS
02300      SET PLT-INDEX2 TO 2.                                         ELTASSTS
02301      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02302         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTASSTS
02303                                                        NOT = '0'  ELTASSTS
02304         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02305         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTASSTS
02306                                           CMF-ELEMENT-SYSTEM-NAME ELTASSTS
02307         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTASSTS
02308                                                TO CMF-CODE-VALUE  ELTASSTS
02309         MOVE WS-SPILLOVER-DED TO WS-TEMP-TEXT-AREA                ELTASSTS
02310         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
02311         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02312         MOVE 1 TO WS-CIA                                          ELTASSTS
02313         EXEC CICS LINK                                            ELTASSTS
02314             PROGRAM('ELUOUTPT')                                   ELTASSTS
02315             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
02316             END-EXEC.                                             ELTASSTS
02317 **                                                               |ELTASSTS
02318 **---------------------------------------------------------------+ELTASSTS
02319                                                                   ELTASSTS
02320                                                                   ELTASSTS
02321      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
02322         SET PLT-INDEX2 TO 2                                       ELTASSTS
02323      ELSE                                                         ELTASSTS
02324         SET PLT-INDEX2 TO 1.                                      ELTASSTS
02325                                                                   ELTASSTS
02326 **---------------------------------------------------------------+ELTASSTS
02327 **                                                               |ELTASSTS
02328 **           TRANSFER TO OTHER RESPONSIBILITY IND                |ELTASSTS
02329 **                                                               |ELTASSTS
02330                                                                   ELTASSTS
02331      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTASSTS
02332           ZERO                                                    ELTASSTS
02333         NEXT SENTENCE                                             ELTASSTS
02334      ELSE                                                         ELTASSTS
02335         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02336         MOVE 'TRANSF-OTHER-RESP-IND' TO   CMF-ELEMENT-SYSTEM-NAME ELTASSTS
02337         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTASSTS
02338                                                TO CMF-CODE-VALUE  ELTASSTS
02339         MOVE SPACES           TO WS-TEMP-TEXT-AREA                ELTASSTS
02340         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
02341         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTASSTS
02342         MOVE 1 TO WS-CIA                                          ELTASSTS
02343         EXEC CICS LINK                                            ELTASSTS
02344             PROGRAM('ELUOUTPT')                                   ELTASSTS
02345             COMMAREA(DFHCOMMAREA)                                 ELTASSTS
02346             END-EXEC.                                             ELTASSTS
02347 **                                                               |ELTASSTS
02348 **---------------------------------------------------------------+ELTASSTS
02349                                                                   ELTASSTS
02350      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTASSTS
02351         SET PLT-INDEX2 TO 2                                       ELTASSTS
02352      ELSE                                                         ELTASSTS
02353         SET PLT-INDEX2 TO 1.                                      ELTASSTS
02354                                                                   ELTASSTS
02355 **---------------------------------------------------------------+ELTASSTS
02356 **                                                               |ELTASSTS
02357 **                  # A A R   T A B U L A R   F O U N D          |ELTASSTS
02358      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02359      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02360         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02361                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02362         MOVE +1 TO COF-NBR-DTL-LINES                              ELTASSTS
02363         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02364         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTASSTS
02365      ELSE                                                         ELTASSTS
02366         SET PLT-INDEX2 TO 2                                       ELTASSTS
02367         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02368            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02369                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02370            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTASSTS
02371            MOVE +1 TO COF-NBR-DTL-LINES                           ELTASSTS
02372            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTASSTS
02373                                                                   ELTASSTS
02374      IF WS-ADD-A-BLANK-LINE                                       ELTASSTS
02375         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTASSTS
02376         MOVE 1 TO WS-CIA                                          ELTASSTS
02377         EXEC CICS LINK                                            ELTASSTS
02378              PROGRAM('ELUOUTPT')                                  ELTASSTS
02379              COMMAREA(DFHCOMMAREA)                                ELTASSTS
02380              END-EXEC.                                            ELTASSTS
02381 **                                                               |ELTASSTS
02382 **---------------------------------------------------------------+ELTASSTS
02383                                                                   ELTASSTS
02384 **---------------------------------------------------------------+ELTASSTS
02385 **                                                               |ELTASSTS
02386 **                  # P P F   T A B U L A R                      |ELTASSTS
02387      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02388      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02389         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02390                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02391         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
02392                                              KWA-GCTABULR-KEY     ELTASSTS
02393         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
02394         IF IOP-RC-OK                                              ELTASSTS
02395            EXEC CICS LINK                                         ELTASSTS
02396                 PROGRAM('ELGPPF')                                 ELTASSTS
02397                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02398                 END-EXEC                                          ELTASSTS
02399         ELSE                                                      ELTASSTS
02400            NEXT SENTENCE                                          ELTASSTS
02401      ELSE                                                         ELTASSTS
02402         SET PLT-INDEX2 TO 2                                       ELTASSTS
02403         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02404            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02405                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02406          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
02407                                              KWA-GCTABULR-KEY     ELTASSTS
02408            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
02409            IF IOP-RC-OK                                           ELTASSTS
02410               EXEC CICS LINK                                      ELTASSTS
02411                    PROGRAM('ELGPPF')                              ELTASSTS
02412                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
02413                    END-EXEC.                                      ELTASSTS
02414 **                                                               |ELTASSTS
02415 **---------------------------------------------------------------+ELTASSTS
02416                                                                   ELTASSTS
02417 **---------------------------------------------------------------+ELTASSTS
02418 **                                                               |ELTASSTS
02419 **                  # P V E   T A B U L A R                      |ELTASSTS
02420 **                                                               |ELTASSTS
02421      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
02422      MOVE WS-CHECK-CONTRACT-FOR-PVE                               ELTASSTS
02423          TO COF-DTL-LINE(2).                                      ELTASSTS
02424      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
02425      EXEC CICS LINK                                               ELTASSTS
02426           PROGRAM('ELUOUTPT')                                     ELTASSTS
02427           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
02428           END-EXEC.                                               ELTASSTS
02429 **                                                               |ELTASSTS
02430 **---------------------------------------------------------------+ELTASSTS
02431                                                                   ELTASSTS
02432 **---------------------------------------------------------------+ELTASSTS
02433 **                                                               |ELTASSTS
02434 **                  # A B M   T A B U L A R                      |ELTASSTS
02435      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02436      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02437         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02438                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02439         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
02440                                              KWA-GCTABULR-KEY     ELTASSTS
02441         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
02442         IF IOP-RC-OK                                              ELTASSTS
02443            EXEC CICS LINK                                         ELTASSTS
02444                 PROGRAM('ELGMAXIM')                               ELTASSTS
02445                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02446                 END-EXEC                                          ELTASSTS
02447         ELSE                                                      ELTASSTS
02448            NEXT SENTENCE                                          ELTASSTS
02449      ELSE                                                         ELTASSTS
02450         SET PLT-INDEX2 TO 2                                       ELTASSTS
02451         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02452            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02453                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02454          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
02455                                              KWA-GCTABULR-KEY     ELTASSTS
02456            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
02457            IF IOP-RC-OK                                           ELTASSTS
02458               EXEC CICS LINK                                      ELTASSTS
02459                    PROGRAM('ELGMAXIM')                            ELTASSTS
02460                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
02461                    END-EXEC.                                      ELTASSTS
02462                                                                   ELTASSTS
02463 **---------------------------------------------------------------+ELTASSTS
02464 **                                                               |ELTASSTS
02465 **                  # A C L   T A B U L A R                      |ELTASSTS
02466      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02467      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02468         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02469                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02470         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
02471                                              KWA-GCTABULR-KEY     ELTASSTS
02472         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
02473         IF IOP-RC-OK                                              ELTASSTS
02474            EXEC CICS LINK                                         ELTASSTS
02475                 PROGRAM('ELGCOINS')                               ELTASSTS
02476                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02477                 END-EXEC                                          ELTASSTS
02478         ELSE                                                      ELTASSTS
02479            NEXT SENTENCE                                          ELTASSTS
02480      ELSE                                                         ELTASSTS
02481         SET PLT-INDEX2 TO 2                                       ELTASSTS
02482         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02483            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02484                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02485          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
02486                                              KWA-GCTABULR-KEY     ELTASSTS
02487            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
02488            IF IOP-RC-OK                                           ELTASSTS
02489               EXEC CICS LINK                                      ELTASSTS
02490                    PROGRAM('ELGCOINS')                            ELTASSTS
02491                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
02492                    END-EXEC.                                      ELTASSTS
02493 **                                                               |ELTASSTS
02494 **---------------------------------------------------------------+ELTASSTS
02495                                                                   ELTASSTS
02496 **---------------------------------------------------------------+ELTASSTS
02497 **                                                               |ELTASSTS
02498 **                  # A D L   T A B U L A R                      |ELTASSTS
02499      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02500      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02501         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02502                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02503         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
02504                                              KWA-GCTABULR-KEY     ELTASSTS
02505         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
02506         IF IOP-RC-OK                                              ELTASSTS
02507            EXEC CICS LINK                                         ELTASSTS
02508                 PROGRAM('ELGDEDBL')                               ELTASSTS
02509                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02510                 END-EXEC                                          ELTASSTS
02511         ELSE                                                      ELTASSTS
02512            NEXT SENTENCE                                          ELTASSTS
02513      ELSE                                                         ELTASSTS
02514         SET PLT-INDEX2 TO 2                                       ELTASSTS
02515         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02516            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02517                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02518          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTASSTS
02519                                              KWA-GCTABULR-KEY     ELTASSTS
02520            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
02521            IF IOP-RC-OK                                           ELTASSTS
02522               EXEC CICS LINK                                      ELTASSTS
02523                    PROGRAM('ELGDEDBL')                            ELTASSTS
02524                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
02525                    END-EXEC.                                      ELTASSTS
02526 **                                                               |ELTASSTS
02527 **---------------------------------------------------------------+ELTASSTS
02528                                                                   ELTASSTS
02529 **---------------------------------------------------------------+ELTASSTS
02530 **                                                               |ELTASSTS
02531 **                  # A O L   T A B U L A R                      |ELTASSTS
02532      SET PLT-INDEX2 TO 1.                                         ELTASSTS
02533      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTASSTS
02534         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTASSTS
02535                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02536         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTASSTS
02537                                              KWA-GCTABULR-KEY     ELTASSTS
02538         PERFORM 2300-GET-TABULAR-RECORD                           ELTASSTS
02539         IF IOP-RC-OK                                              ELTASSTS
02540            EXEC CICS LINK                                         ELTASSTS
02541                 PROGRAM('ELGOUTPX')                               ELTASSTS
02542                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02543                 END-EXEC                                          ELTASSTS
02544         ELSE                                                      ELTASSTS
02545            NEXT SENTENCE                                          ELTASSTS
02546      ELSE                                                         ELTASSTS
02547         SET PLT-INDEX2 TO 2                                       ELTASSTS
02548         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTASSTS
02549            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTASSTS
02550                    ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTASSTS
02551            MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  ELTASSTS
02552                                           TO KWA-GCTABULR-KEY     ELTASSTS
02553            PERFORM 2300-GET-TABULAR-RECORD                        ELTASSTS
02554            IF IOP-RC-OK                                           ELTASSTS
02555               EXEC CICS LINK                                      ELTASSTS
02556                    PROGRAM('ELGOUTPX')                            ELTASSTS
02557                    COMMAREA(DFHCOMMAREA)                          ELTASSTS
02558                    END-EXEC.                                      ELTASSTS
02559 **                                                               |ELTASSTS
02560 **---------------------------------------------------------------+ELTASSTS
02561                                                                   ELTASSTS
02562      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTASSTS
02563      MOVE WS-STAFF-PROV-NOT-AVAIL TO COF-DTL-LINE(2).             ELTASSTS
02564      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTASSTS
02565      EXEC CICS LINK                                               ELTASSTS
02566           PROGRAM('ELUOUTPT')                                     ELTASSTS
02567           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
02568           END-EXEC.                                               ELTASSTS
02569                                                                   ELTASSTS
02570 **---------------------------------------------------------------+ELTASSTS
02571 **                                                               |ELTASSTS
02572 **         P A Y M E N T  C O N S I D E R A T I O N  T E X T     |ELTASSTS
02573      INITIALIZE TCAR-FROM-AREA.                                   ELTASSTS
02574      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTASSTS
02575             WS-PAY-CONSDR-TEXT2                                   ELTASSTS
02576                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTASSTS
02577      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTASSTS
02578      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTASSTS
02579      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTASSTS
02580                                TCAR-OUTPUT-FIELD-2-LEN.           ELTASSTS
02581      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTASSTS
02582      IF WS-CIA > 17                                               ELTASSTS
02583            MOVE WS-CIA TO COF-NBR-DTL-LINES                       ELTASSTS
02584            EXEC CICS LINK                                         ELTASSTS
02585                PROGRAM('ELUOUTPT')                                ELTASSTS
02586                COMMAREA(DFHCOMMAREA)                              ELTASSTS
02587            END-EXEC                                               ELTASSTS
02588            MOVE +1            TO WS-CIA.                          ELTASSTS
02589      ADD +1                TO  WS-CIA.                            ELTASSTS
02590      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTASSTS
02591      ADD +1                TO  WS-CIA.                            ELTASSTS
02592      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTASSTS
02593      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTASSTS
02594      EXEC CICS LINK                                               ELTASSTS
02595           PROGRAM('ELUOUTPT')                                     ELTASSTS
02596           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
02597           END-EXEC.                                               ELTASSTS
02598      MOVE +1            TO WS-CIA.                                ELTASSTS
02599 **                                                               |ELTASSTS
02600 **---------------------------------------------------------------+ELTASSTS
02601                                                                   ELTASSTS
02602  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTASSTS
02603                                                                   ELTASSTS
02604      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB              ELTASSTS
02605         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTASSTS
02606         MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME               ELTASSTS
02607         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTASSTS
02608                                                   CMF-CODE-VALUE  ELTASSTS
02609         MOVE SPACES TO WS-TEMP-TEXT-AREA                          ELTASSTS
02610         MOVE +58 TO WS-TEMP-NOT-USED-CNT                          ELTASSTS
02611         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTASSTS
02612         MOVE ZERO TO PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)          ELTASSTS
02613         ADD  1 TO WS-SUB2                                         ELTASSTS
02614         IF WS-CIA > 20 OR = 20                                    ELTASSTS
02615            MOVE WS-CIA TO COF-NBR-DTL-LINES                       ELTASSTS
02616            EXEC CICS LINK                                         ELTASSTS
02617                 PROGRAM('ELUOUTPT')                               ELTASSTS
02618                 COMMAREA(DFHCOMMAREA)                             ELTASSTS
02619                 END-EXEC                                          ELTASSTS
02620            MOVE +1 TO WS-CIA.                                     ELTASSTS
02621                                                                   ELTASSTS
02622  4090-PROBLEM-WITH-INDICES.                                       ELTASSTS
02623                                                                   ELTASSTS
02624      MOVE 'PROBLEM W/ INDICES' TO COF-DTL-LINE(1).                ELTASSTS
02625      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTASSTS
02626                                                                   ELTASSTS
02627      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTASSTS
02628      MOVE 'P' TO COF-FUNCTION.                                    ELTASSTS
02629                                                                   ELTASSTS
02630      EXEC CICS LINK                                               ELTASSTS
02631           PROGRAM('ELUOUTPT')                                     ELTASSTS
02632           COMMAREA(DFHCOMMAREA)                                   ELTASSTS
02633           END-EXEC.                                               ELTASSTS
02634                                                                   ELTASSTS
02635  4099-EXIT.                                                       ELTASSTS
02636      EXIT.                                                        ELTASSTS
02637                                                                   ELTASSTS
02638 /                                                                 ELTASSTS
02639      COPY ELSTCOMP.                                               ELTASSTS
02640 /                                                                 ELTASSTS
02641 ******************************************************************ELTASSTS
02642 ** IF PROGRAM EXECUTION SEQUENCE IMPROPERLY FALLS OUT OF THE    **ELTASSTS
02643 ** BOTTOM OF THE PROGRAM, THIS ROUTINE WILL SET A DIAGNOSTIC    **ELTASSTS
02644 ** CODE TO LET US KNOW WHAT WENT WRONG.                         **ELTASSTS
02645 ******************************************************************ELTASSTS
02646  9900-FALLTHRU-CATCHER SECTION.                                   ELTASSTS
02647                                                                   ELTASSTS
02648      SET CIA-AB-UNDEF TO TRUE.                                    ELTASSTS
02649      PERFORM 9999-ABEND.                                          ELTASSTS
02650                                                                   ELTASSTS
02651  9900-EXIT.                                                       ELTASSTS
02652      EXIT.                                                        ELTASSTS
02653                                                                   ELTASSTS
02654 ******************************************************************ELTASSTS
02655 ** INVOKE THE CATASTROPHIC ERROR HANDLER PASSING ALONG THE      **ELTASSTS
02656 ** ERROR CODE IN THE DFHCOMMAREA.  NO ELS ECI PROGRAM SHOULD    **ELTASSTS
02657 ** BE ISSUING ITS OWN ABENDS.                                   **ELTASSTS
02658 ******************************************************************ELTASSTS
02659  9999-ABEND SECTION.                                              ELTASSTS
02660                                                                   ELTASSTS
02661      EXEC CICS ABEND                                              ELTASSTS
02662           ABCODE (WS-ABEND-CODE)                                  ELTASSTS
02663           END-EXEC.                                               ELTASSTS
02664                                                                   ELTASSTS
02665  9999-EXIT.                                                       ELTASSTS
02666      GOBACK.                                                      ELTASSTS
