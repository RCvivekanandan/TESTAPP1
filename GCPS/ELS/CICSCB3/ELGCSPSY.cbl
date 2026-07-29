00001 *      LAST MAINTENANCE TIME: 15.18.18  DATE: 08/21/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELGCSPSYPL - LEVEL 010 AS OF 10/14/88       ELGCSPSY
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELGCSPSY
00005  IDENTIFICATION DIVISION.                                         ELGCSPSY
00006                                                                   ELGCSPSY
00007  PROGRAM-ID.         ELGCSPSY.                                    ELGCSPSY
00008                                                                   ELGCSPSY
00009  AUTHOR.             RICK BARILEAU.                               ELGCSPSY
00010                                                                   ELGCSPSY
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSPSY
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSPSY
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSPSY
00014                      233 N. MICHIGAN AVE                          ELGCSPSY
00015                      CHICAGO, ILLINOIS 60601                      ELGCSPSY
00016                                                                   ELGCSPSY
00017  DATE-WRITTEN.       22-FEB-1988.                                 ELGCSPSY
00018                                                                   ELGCSPSY
00019  DATE-COMPILED.                                                   ELGCSPSY
00020                                                                   ELGCSPSY
00021  SECURITY.           COPYRIGHT 1986,                              ELGCSPSY
00022                      HEALTH CARE SERVICE CORPORATION              ELGCSPSY
00023      SKIP3                                                        ELGCSPSY
00024  ENVIRONMENT DIVISION.                                            ELGCSPSY
00025                                                                   ELGCSPSY
00026  CONFIGURATION SECTION.                                           ELGCSPSY
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSPSY
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSPSY
00029      EJECT                                                        ELGCSPSY
00030 ******************************************************************ELGCSPSY
00031 *                                                                *ELGCSPSY
00032 *  ELGCSPSY - ELS:  THIS MODULE GATHERS ALL INFORMATION THAT     *ELGCSPSY
00033 *                   PERTAINS TO PSYCHIATRIC SERVICES             *ELGCSPSY
00034 *                   WITH THE CONTRACT AND WILL DISPLAY IT.       *ELGCSPSY
00035 *                                                                *ELGCSPSY
00036 *  NOTES :  THE MODULES ELUCSCOV AND ELUCSPLG WILL ALREADY HAVE  *ELGCSPSY
00037 *           DETERMINED THE COVERAGE AND PAYMENT LEVEL FOR EACH   *ELGCSPSY
00038 *           BENEFIT PROVISION ASSOCIATED WITH THIS SUBTOPIC      *ELGCSPSY
00039 *           PRIOR TO REACHING THIS PROGRAM.                      *ELGCSPSY
00040 *                                                                *ELGCSPSY
00041 *  THE FOLLOWING ARE THOSE BENEFIT PROVISIONS INVOLVED :         *ELGCSPSY
00042 *           GPI  B  =  GROUP PSYCHOTHERAPY INPATIENT             *ELGCSPSY
00043 *           GPI  D  =  GROUP PSYCHOTHERAPY INPATIENT             *ELGCSPSY
00044 *           GPO  B  =  GROUP PSYCHOTHERAPY OUTPATIENT            *ELGCSPSY
00045 *           GPO  E  =  GROUP PSYCHOTHERAPY OUTPATIENT            *ELGCSPSY
00046 *           IPI  B  =  INDIVIDUAL PSYCHOTHERAPY INPATIENT        *ELGCSPSY
00047 *           IPI  D  =  INDIVIDUAL PSYCHOTHERAPY INPATIENT        *ELGCSPSY
00048 *           IPO  B  =  INDIVIDUAL PSYCHOTHERAPY OUTPATIENT       *ELGCSPSY
00049 *           IPO  E  =  INDIVIDUAL PSYCHOTHERAPY OUTPATIENT       *ELGCSPSY
00050 *           PSI  B  =  PSYCHOLOGICAL TESTING INPATIENT           *ELGCSPSY
00051 *           PSI  D  =  PSYCHOLOGICAL TESTING INPATIENT           *ELGCSPSY
00052 *           PSO  B  =  PSYCHOLOGICAL TESTING OUTPATIENT          *ELGCSPSY
00053 *           PSO  E  =  PSYCHOLOGICAL TESTING OUTPATIENT          *ELGCSPSY
00054 *                                                                *ELGCSPSY
00055 ******************************************************************ELGCSPSY
00056 *                                                                *ELGCSPSY
00057 *                      MAINTENANCE HISTORY                       *ELGCSPSY
00058 *                                                                *ELGCSPSY
00059 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSPSY
00060 * ----- ----------- --- ----- ---------------------------------- *ELGCSPSY
00061 * 01.00 22-FEB-1988 REB       CREATED                            *ELGCSPSY
00062 * 01.01 04-MAR-1988 REB       REMOVED CODE FOR DISCLAIMER MESSAGE*ELGCSPSY
00063 * 01.02 07-APR-1988 REB       DISPLAY A MESSAGE CONCERNING       *ELGCSPSY
00064 *                             COINSURANCE SINCE THE INFO IS      *ELGCSPSY
00065 *                             BEING SUPPRESSED AT THE MOMENT.    *ELGCSPSY
00066 * 01.03 12-APR-1988 REB       DISPLAY A MESSAGE ABOUT MENTAL     *ELGCSPSY
00067 *                             CONDITIONS.                        *ELGCSPSY
00068 * 01.04 14-OCT-1988 EGL       REMOVED SPECIAL MESSAGE ABOUT      *ELGCSPSY
00069 *                             ADDITIONAL BENEFITS.  ALSO CHANGED *ELGCSPSY
00070 *                             PROGRAM TO USE NEW STORAGE         *ELGCSPSY
00071 *                             MANAGEMENT ROUTINES.               *ELGCSPSY
00072 * 01.05 21-AUG-1989 AKK       DESTRUCT PROGRAM.                  *ELGCSPSY
00073 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSPSY
00074 ******************************************************************ELGCSPSY
00075                                                                   ELGCSPSY
00076  DATA DIVISION.                                                   ELGCSPSY
00077  WORKING-STORAGE SECTION.                                         ELGCSPSY
00078  01  WS-MISC.                                                     ELGCSPSY
00079      05  FILLER                  PIC  X(24) VALUE                 ELGCSPSY
00080          '** ELGCSPSY WS BEGINS **'.                              ELGCSPSY
00081                                                                   ELGCSPSY
00082  01  WS-FIXED-TEXT-DESCRIPTION.                                   ELGCSPSY
00083      05  WS-PSY-HEADER-1.                                         ELGCSPSY
00084          10  FILLER              PIC X(79) VALUE 'THE FOLLOWING DIELGCSPSY
00085 -        'SPLAYED BENEFITS ARE FOR: PSYCHIATRIC SERVICES'.        ELGCSPSY
00086      05  WS-DASH-LINE.                                            ELGCSPSY
00087          10  FILLER              PIC X(70) VALUE '----------------ELGCSPSY
00088 -        '------------------------------------------------------'.ELGCSPSY
00089          10  FILLER              PIC X(09) VALUE '---------'.     ELGCSPSY
00090      05  WS-BAR-LINE.                                             ELGCSPSY
00091          10  FILLER              PIC X(30) VALUE SPACES.          ELGCSPSY
00092          10  FILLER              PIC X(01) VALUE '|'.             ELGCSPSY
00093          10  FILLER              PIC X(46) VALUE SPACES.          ELGCSPSY
00094                                                                   ELGCSPSY
00095  LINKAGE SECTION.                                                 ELGCSPSY
00096  01  DFHCOMMAREA.                                                 ELGCSPSY
00097      COPY ELSCOMMC.                                               ELGCSPSY
00098 /                                                                 ELGCSPSY
00099      COPY ELSCIA2C.                                               ELGCSPSY
00100 /                                                                 ELGCSPSY
00101      COPY ELSIOPMC.                                               ELGCSPSY
00102 /                                                                 ELGCSPSY
00103      COPY ELSKEYSC.                                               ELGCSPSY
00104 /                                                                 ELGCSPSY
00105      COPY ELSSRTPC.                                               ELGCSPSY
00106 /                                                                 ELGCSPSY
00107      COPY ELSSSCBC.                                               ELGCSPSY
00108 /                                                                 ELGCSPSY
00109      COPY ELSTCWAC.                                               ELGCSPSY
00110 /                                                                 ELGCSPSY
00111      COPY ELSOUTPC.                                               ELGCSPSY
00112 /    COPYBOOK FOR CONTRACT SUMMARY POINTER TABLE                  ELGCSPSY
00113      COPY ELSCSPTC.                                               ELGCSPSY
00114 /    COPYBOOK FOR CONTRACT SUMMARY BENEFIT PROVISION TABLE        ELGCSPSY
00115      COPY ELSCSBPC.                                               ELGCSPSY
00116      EJECT                                                        ELGCSPSY
00117  PROCEDURE DIVISION.                                              ELGCSPSY
00118 ************************************************************      ELGCSPSY
00119 *                                                          *      ELGCSPSY
00120 *                    PROCEDURE DIVISION                    *      ELGCSPSY
00121 *                                                          *      ELGCSPSY
00122 ************************************************************      ELGCSPSY
00123                                                                   ELGCSPSY
00124                                                                   ELGCSPSY
00125 ************************************************************      ELGCSPSY
00126 *                                                          *      ELGCSPSY
00127 *        DO ELGCSPSY                                       *      ELGCSPSY
00128 *                                                          *      ELGCSPSY
00129 ************************************************************      ELGCSPSY
00130  DO-ELSGSPY.                                                      ELGCSPSY
00131      PERFORM INITIALIZATION.                                      ELGCSPSY
00132      PERFORM PROCESS.                                             ELGCSPSY
00133      GOBACK.                                                      ELGCSPSY
00134                                                                   ELGCSPSY
00135                                                                   ELGCSPSY
00136 ************************************************************      ELGCSPSY
00137 *                                                          *      ELGCSPSY
00138 *        INITIALIZATION.                                   *      ELGCSPSY
00139 *                                                          *      ELGCSPSY
00140 ************************************************************      ELGCSPSY
00141  INITIALIZATION.                                                  ELGCSPSY
00142      PERFORM ESTABLISH-ADDRESS-OF-CO.                             ELGCSPSY
00143      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELGCSPSY
00144                                                                   ELGCSPSY
00145                                                                   ELGCSPSY
00146 ************************************************************      ELGCSPSY
00147 *                                                          *      ELGCSPSY
00148 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCSPSY
00149 *                                                          *      ELGCSPSY
00150 ************************************************************      ELGCSPSY
00151  ESTABLISH-ADDRESS-OF-CO.                                         ELGCSPSY
00152      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCSPSY
00153      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELGCSPSY
00154      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELGCSPSY
00155                                                                   ELGCSPSY
00156 ************************************************************      ELGCSPSY
00157 *                                                          *      ELGCSPSY
00158 *        CHECK FOR VALID COMMAREA                          *      ELGCSPSY
00159 *                                                          *      ELGCSPSY
00160 ************************************************************      ELGCSPSY
00161  CHECK-FOR-VALID-COMMAREA.                                        ELGCSPSY
00162      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSPSY
00163         EXEC CICS ABEND                                           ELGCSPSY
00164                   ABCODE('EL01')                                  ELGCSPSY
00165            END-EXEC.                                              ELGCSPSY
00166                                                                   ELGCSPSY
00167 ************************************************************      ELGCSPSY
00168 *                                                          *      ELGCSPSY
00169 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELGCSPSY
00170 *                                                          *      ELGCSPSY
00171 ************************************************************      ELGCSPSY
00172  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELGCSPSY
00173      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSPSY
00174                 ADDRESS OF                                        ELGCSPSY
00175          CIA-ELS-COMMON-INTERFACE-AREA.                           ELGCSPSY
00176      IF CIA-RC-PTR-NULL                                           ELGCSPSY
00177         EXEC CICS ABEND                                           ELGCSPSY
00178                   ABCODE('EL02')                                  ELGCSPSY
00179            END-EXEC.                                              ELGCSPSY
00180                                                                   ELGCSPSY
00181 ************************************************************      ELGCSPSY
00182 *                                                          *      ELGCSPSY
00183 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELGCSPSY
00184 *                                                          *      ELGCSPSY
00185 ************************************************************      ELGCSPSY
00186  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELGCSPSY
00187      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGCSPSY
00188      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSPSY
00189                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELGCSPSY
00190      IF CIA-RC-PTR-NULL                                           ELGCSPSY
00191          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSPSY
00192                                                                   ELGCSPSY
00193 ************************************************************      ELGCSPSY
00194 *                                                          *      ELGCSPSY
00195 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSPSY
00196 *                                                          *      ELGCSPSY
00197 ************************************************************      ELGCSPSY
00198  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSPSY
00199      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSPSY
00200      PERFORM SIGNAL-ABEND.                                        ELGCSPSY
00201                                                                   ELGCSPSY
00202 ************************************************************      ELGCSPSY
00203 *                                                          *      ELGCSPSY
00204 *        SIGNAL ABEND                                      *      ELGCSPSY
00205 *                                                          *      ELGCSPSY
00206 ************************************************************      ELGCSPSY
00207  SIGNAL-ABEND.                                                    ELGCSPSY
00208      EXEC CICS ABEND                                              ELGCSPSY
00209                ABCODE(CIA-ABCODE)                                 ELGCSPSY
00210         END-EXEC.                                                 ELGCSPSY
00211                                                                   ELGCSPSY
00212 ************************************************************      ELGCSPSY
00213 *                                                          *      ELGCSPSY
00214 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGCSPSY
00215 *                                                          *      ELGCSPSY
00216 ************************************************************      ELGCSPSY
00217  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELGCSPSY
00218      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCSPSY
00219      PERFORM ESTABLISH-ADDRESSABILITY-CSPT.                       ELGCSPSY
00220      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELGCSPSY
00221                                                                   ELGCSPSY
00222 ************************************************************      ELGCSPSY
00223 *                                                          *      ELGCSPSY
00224 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCSPSY
00225 *                                                          *      ELGCSPSY
00226 ************************************************************      ELGCSPSY
00227  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCSPSY
00228      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSPSY
00229      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSPSY
00230                 ADDRESS OF COF-OUTPUT-INTERFACE.                  ELGCSPSY
00231      IF CIA-RC-PTR-NULL                                           ELGCSPSY
00232          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSPSY
00233                                                                   ELGCSPSY
00234                                                                   ELGCSPSY
00235 ************************************************************      ELGCSPSY
00236 *                                                          *      ELGCSPSY
00237 *        ESTABLISH ADDRESSABILITY OF CS POINTER TABLE      *      ELGCSPSY
00238 *                                                          *      ELGCSPSY
00239 ************************************************************      ELGCSPSY
00240  ESTABLISH-ADDRESSABILITY-CSPT.                                   ELGCSPSY
00241      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELGCSPSY
00242      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSPSY
00243                 ADDRESS OF CSPT-POINTER-LIST.                     ELGCSPSY
00244      IF CIA-RC-PTR-NULL                                           ELGCSPSY
00245          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSPSY
00246                                                                   ELGCSPSY
00247 ************************************************************      ELGCSPSY
00248 *                                                          *      ELGCSPSY
00249 *        ESTABLISH ADDRESSABILITY OF CS BENEFIT PROVISION T*      ELGCSPSY
00250 *                                                          *      ELGCSPSY
00251 ************************************************************      ELGCSPSY
00252  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELGCSPSY
00253      IF CSPT-PSY-BP-TBL-PTR = NULL                                ELGCSPSY
00254          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELGCSPSY
00255      ELSE                                                         ELGCSPSY
00256          PERFORM ESTABLISH-ADDRESS-OF-CSBPC.                      ELGCSPSY
00257                                                                   ELGCSPSY
00258 ************************************************************      ELGCSPSY
00259 *                                                          *      ELGCSPSY
00260 *        ESTABLISH ADDRESS OF CSBPC                        *      ELGCSPSY
00261 *                                                          *      ELGCSPSY
00262 ************************************************************      ELGCSPSY
00263  ESTABLISH-ADDRESS-OF-CSBPC.                                      ELGCSPSY
00264      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELGCSPSY
00265          CSPT-PSY-BP-TBL-PTR.                                     ELGCSPSY
00266                                                                   ELGCSPSY
00267 ************************************************************      ELGCSPSY
00268 *                                                          *      ELGCSPSY
00269 *        PROCESS                                           *      ELGCSPSY
00270 *                                                          *      ELGCSPSY
00271 ************************************************************      ELGCSPSY
00272  PROCESS.                                                         ELGCSPSY
00273      PERFORM DISPLAY-HEADINGS-FOR-SUBTOPIC.                       ELGCSPSY
00274      SET PROCESS-PSY   TO TRUE.                                   ELGCSPSY
00275      PERFORM CALL-SENTENCE-INTERFACE.                             ELGCSPSY
00276      PERFORM CALL-CONTRACT-SUMMARY-OUTPUT-I.                      ELGCSPSY
00277      SET COMPLETED-PSY TO TRUE.                                   ELGCSPSY
00278                                                                   ELGCSPSY
00279                                                                   ELGCSPSY
00280 ************************************************************      ELGCSPSY
00281 *                                                          *      ELGCSPSY
00282 *        DISPLAY HEADINGS FOR SUBTOPIC                     *      ELGCSPSY
00283 *                                                          *      ELGCSPSY
00284 ************************************************************      ELGCSPSY
00285  DISPLAY-HEADINGS-FOR-SUBTOPIC.                                   ELGCSPSY
00286      SET COF-NEW-PAGE     TO TRUE.                                ELGCSPSY
00287      MOVE +3              TO COF-NBR-HDR-LINES.                   ELGCSPSY
00288      MOVE +0              TO COF-NBR-DTL-LINES.                   ELGCSPSY
00289      MOVE WS-PSY-HEADER-1 TO COF-HDR-LINE (2).                    ELGCSPSY
00290      MOVE WS-DASH-LINE    TO COF-HDR-LINE (3).                    ELGCSPSY
00291      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSPSY
00292                                                                   ELGCSPSY
00293                                                                   ELGCSPSY
00294 ************************************************************      ELGCSPSY
00295 *                                                          *      ELGCSPSY
00296 *        CALL OUTPUT INTERFACE                             *      ELGCSPSY
00297 *                                                          *      ELGCSPSY
00298 ************************************************************      ELGCSPSY
00299  CALL-OUTPUT-INTERFACE.                                           ELGCSPSY
00300      EXEC CICS LINK                                               ELGCSPSY
00301                PROGRAM ('ELUOUTPT')                               ELGCSPSY
00302                COMMAREA (DFHCOMMAREA)                             ELGCSPSY
00303         END-EXEC.                                                 ELGCSPSY
00304                                                                   ELGCSPSY
00305 ************************************************************      ELGCSPSY
00306 *                                                          *      ELGCSPSY
00307 *        CALL SENTENCE INTERFACE                           *      ELGCSPSY
00308 *                                                          *      ELGCSPSY
00309 ************************************************************      ELGCSPSY
00310  CALL-SENTENCE-INTERFACE.                                         ELGCSPSY
00311      EXEC CICS LINK                                               ELGCSPSY
00312                PROGRAM ('ELUCSENT')                               ELGCSPSY
00313                COMMAREA (DFHCOMMAREA)                             ELGCSPSY
00314         END-EXEC.                                                 ELGCSPSY
00315                                                                   ELGCSPSY
00316 ************************************************************      ELGCSPSY
00317 *                                                          *      ELGCSPSY
00318 *        CALL CONTRACT SUMMARY OUTPUT INTERFACE            *      ELGCSPSY
00319 *                                                          *      ELGCSPSY
00320 ************************************************************      ELGCSPSY
00321  CALL-CONTRACT-SUMMARY-OUTPUT-I.                                  ELGCSPSY
00322      EXEC CICS LINK                                               ELGCSPSY
00323                PROGRAM ('ELUCSOUT')                               ELGCSPSY
00324                COMMAREA (DFHCOMMAREA)                             ELGCSPSY
00325         END-EXEC.                                                 ELGCSPSY
