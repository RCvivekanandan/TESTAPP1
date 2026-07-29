00001 *      LAST MAINTENANCE TIME:  9.45.19  DATE: 08/22/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELGCSGCIPL - LEVEL 057 AS OF 11/23/88       ELGCSGCI
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELGCSGCI
00005  IDENTIFICATION DIVISION.                                         ELGCSGCI
00006                                                                   ELGCSGCI
00007  PROGRAM-ID.         ELGCSGCI.                                    ELGCSGCI
00008                                                                   ELGCSGCI
00009  AUTHOR.             NINA CERVANTES.                              ELGCSGCI
00010                                                                   ELGCSGCI
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSGCI
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSGCI
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSGCI
00014                      233 N. MICHIGAN AVE                          ELGCSGCI
00015                      CHICAGO, ILLINOIS 60601                      ELGCSGCI
00016                                                                   ELGCSGCI
00017  DATE-WRITTEN.       03-MAR-1988.                                 ELGCSGCI
00018                                                                   ELGCSGCI
00019  DATE-COMPILED.                                                   ELGCSGCI
00020                                                                   ELGCSGCI
00021  SECURITY.           COPYRIGHT 1986,                              ELGCSGCI
00022                      HEALTH CARE SERVICE CORPORATION              ELGCSGCI
00023      SKIP3                                                        ELGCSGCI
00024  ENVIRONMENT DIVISION.                                            ELGCSGCI
00025                                                                   ELGCSGCI
00026  CONFIGURATION SECTION.                                           ELGCSGCI
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSGCI
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSGCI
00029      EJECT                                                        ELGCSGCI
00030 ******************************************************************ELGCSGCI
00031 *              MAINTAINANCE HISTORY                              *ELGCSGCI
00032 *                                                                *ELGCSGCI
00033 *   MOD     DATE     BY     ACTION                               *ELGCSGCI
00034 *  01.00  88-MAR-03  NAC    CREATED                              *ELGCSGCI
00035 *  02.00  88-MAR-23  NAC    INSERT COMMAS IN EDIT PATTERNS.      *ELGCSGCI
00036 *  03.00  88-MAR-30  NAC    INTERROGATE #IPGT TABULAR TO IDENTIFY*ELGCSGCI
00037 *                           PROVIDER TYPE                        *ELGCSGCI
00038 *  04.00  88-MAR-31  NAC    LOOK AT UNQUALIFIED TABULARS ONLY.   *ELGCSGCI
00039 *                                                                *ELGCSGCI
00040 *  05.00  88-APR-01  NAC    CHECK FOR UNLIMITED AGE FOR DEPENDENT*ELGCSGCI
00041 *                           AND STUDENT; ALL ACCUMS MUST HAVE    *ELGCSGCI
00042 *                           SERVICE GROUP = ZEROES;SHOW MESSAGE  *ELGCSGCI
00043 *                           FOR FAMILY DEDUCTIBLE; SHOW MESSAGE  *ELGCSGCI
00044 *                           IF MORE THAN ONE DEDUCTIBLE EXIST.   *ELGCSGCI
00045 *  06.00  88-APR-05  NAC    CORRECT LOGIC FOR THE COMPARISON OF  *ELGCSGCI
00046 *                           IBGRS FOR ASCEND/DESCEND.            *ELGCSGCI
00047 *  07.00  88-APR-20  NAC    SUPPRESS COINSURANCE INFORMATION.    *ELGCSGCI
00048 *  08.00  88-APR-27  NAC    REMOVE COMPARISON OF DEFINITION FOR  *ELGCSGCI
00049 *                           ASCEND/DESCEND.                      *ELGCSGCI
00050 *  09.00  88-APR-29  NAC    ADD COMPARISON OF INTERNAL DESCRIPTOR*ELGCSGCI
00051 *                           FOR ASEND/DESCEND.                   *ELGCSGCI
00052 *  10.00  88-MAY-11  NAC    CORRECT LOGIC TO DISPLAY PROVIDER    *ELGCSGCI
00053 *                           TYPE FOR MAXIMUM.                    *ELGCSGCI
00054 *  11.00  88-SEP-19  NAC    REVISE PROGRAM FOR OVERALL CONFIDENCE*ELGCSGCI
00055 *                           FACTORS;  THE PROGRAM WILL PROCESS   *ELGCSGCI
00056 *                           TABLES INSTEAD OF ACTUAL RECORDS.    *ELGCSGCI
00057 *  11.01  88-NOV-04  NAC    TRANSLATE BENEFIT SCOPE FOR COINSURAN*ELGCSGCI
00058 *                           CE AND DISPLAY ALONG WITH PERCENT;   *ELGCSGCI
00059 *                           FOR OPX, BYPASS TABULARS WHICH INDICA*ELGCSGCI
00060 *                           TE FAMILY STATUS AND IS UNLIMITED UN *ELGCSGCI
00061 *                           TIL ASCEND/DESCEND LOGIC IS IN PLACE *ELGCSGCI
00062 *                           DURING NEXT PHASE.                   *ELGCSGCI
00063 *  11.02  88-NOV-21  NAC    SUPPRESS BS ACL HEADER IF NO BS CON- *ELGCSGCI
00064 *                           TRACT EXISTS.                        *ELGCSGCI
00065 *  11.03  88-NOV-22  NAC    DISPLAY POT AND CONDITION BIT TRANS- *ELGCSGCI
00066 *                           LATION FOR COINSURANCE.              *ELGCSGCI
00067 *  12/00  89-AUG-22  AKK    DESTRUCT PROGRAM.                    *ELGCSGCI
00068 *  12.01  94-FEB-23  JPB    ADDED TRANSLATION AND DISPLAY OF     *ELGCSGCI
00069 *                           NETWORK UTILIZATION REVIEW INDICATOR.*ELGCSGCI
00070 *  12.02  03-JAN-08  AKK    REGEN'D DUE TO ADDITION OF SMI AND   *ELGCSGCI
00071 *                           NSMI FOR ACCUMS                      *ELGCSGCI
00072 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          DELGCSGCI
00073 *                           NSMI FOR ACCUMS                      *ELGCSGCI
00074 *                                                                *ELGCSGCI
00075 ******************************************************************ELGCSGCI
00076 *                                                                 ELGCSGCI
00077  DATA DIVISION.                                                   ELGCSGCI
00078  WORKING-STORAGE SECTION.                                         ELGCSGCI
00079  01  WS-HOLD-AREA.                                                ELGCSGCI
00080      03  WS-L-O-B-CONTRACT         PIC X(01).                     ELGCSGCI
00081          88  INSTITUTIONAL-LOB-CONTRACT     VALUE  '1' '5'        ELGCSGCI
00082                                                      '6' '8'.     ELGCSGCI
00083          88  PROFESSIONAL-LOB-CONTRACT      VALUE  '2' '5'        ELGCSGCI
00084                                                     '7' '8'.      ELGCSGCI
00085          88  SUPPLEMENTAL-LOB-CONTRACT       VALUE '3' '6'        ELGCSGCI
00086                                                     '7' '8'.      ELGCSGCI
00087          88  COMPREHENSIVE-LOB-CONTRACT      VALUE '4'.           ELGCSGCI
00088      03  WS-SUBA                 PIC S9(04) COMP  VALUE ZEROES.   ELGCSGCI
00089      03  WS-SUBB                 PIC S9(04) COMP  VALUE ZEROES.   ELGCSGCI
00090      03  WS-DEDUCT-CNT           PIC S9(04) COMP  VALUE ZEROES.   ELGCSGCI
00091      03  WS-BENEFIT-SCOPE        PIC X(04)        VALUE SPACES.   ELGCSGCI
00092  01  WS-SWITCHES.                                                 ELGCSGCI
00093      03  PROCESSING-LOB           PIC X      VALUE SPACES.        ELGCSGCI
00094          88  PROCESSING-MAJOR-MEDICAL   VALUE 'M'.                ELGCSGCI
00095          88  PROCESSING-INSTITUTIONAL   VALUE 'I'.                ELGCSGCI
00096          88  PROCESSING-PROFESSIONAL    VALUE 'P'.                ELGCSGCI
00097          88  PROCESSING-COMPREHENSIVE   VALUE 'C'.                ELGCSGCI
00098      03  PROCESSING-TABULAR       PIC X(06)  VALUE SPACES.        ELGCSGCI
00099          88  PROCESSING-ABM                  VALUE '#ABM  '.      ELGCSGCI
00100          88  PROCESSING-ACL                  VALUE '#ACL  '.      ELGCSGCI
00101          88  PROCESSING-ADL                  VALUE '#ADL  '.      ELGCSGCI
00102          88  PROCESSING-AOL                  VALUE '#AOL  '.      ELGCSGCI
00103      03  DEDUCT-MESSAGE-SW        PIC X(01)  VALUE SPACES.        ELGCSGCI
00104          88  DEDUCT-MSG-NOT-DISPLAYED        VALUE 'N'.           ELGCSGCI
00105          88  DEDUCT-MSG-DISPLAYED            VALUE 'Y'.           ELGCSGCI
00106      03  BS-ACL-MESSAGE-SW        PIC X(01)  VALUE 'N'.           ELGCSGCI
00107          88  BS-ACL-MSG-NOT-DISPLAYED        VALUE 'N'.           ELGCSGCI
00108          88  BS-ACL-MSG-DISPLAYED            VALUE 'Y'.           ELGCSGCI
00109      03  PROVISION-SW             PIC X(01)  VALUE SPACES.        ELGCSGCI
00110          88  PROVISION-NOT-FOUND             VALUE 'N'.           ELGCSGCI
00111          88  PROVISION-FOUND                 VALUE 'Y'.           ELGCSGCI
00112      03  PROCESS-TABLE.                                           ELGCSGCI
00113          05  PROCESS-IND  OCCURS  150 TIMES INDEXED BY WS-IDX     ELGCSGCI
00114                                   PIC X.                          ELGCSGCI
00115              88  ACCUM-DISPLAYED  VALUE 'Y'.                      ELGCSGCI
00116                                                                   ELGCSGCI
00117  01  PROGRAM-CONSTANTS.                                           ELGCSGCI
00118      03  PC-ONE                   PIC S9(01) VALUE 1.             ELGCSGCI
00119      03  PC-FIVE                  PIC S9(01) VALUE 5.             ELGCSGCI
00120      03  PC-SEVEN                 PIC S9(01) VALUE 7.             ELGCSGCI
00121      03  PC-PROVIDER              PIC X(09) VALUE 'PROVIDER'.     ELGCSGCI
00122      03  PC-INDIVIDUAL            PIC X(01) VALUE 'I'.            ELGCSGCI
00123      03  PC-ABM                   PIC X(06) VALUE '#ABM  '.       ELGCSGCI
00124      03  PC-ACL                   PIC X(06) VALUE '#ACL  '.       ELGCSGCI
00125      03  PC-ADL                   PIC X(06) VALUE '#ADL  '.       ELGCSGCI
00126      03  PC-AOL                   PIC X(06) VALUE '#AOL  '.       ELGCSGCI
00127      03  PC-IBGR                  PIC X(06) VALUE '#IBGR '.       ELGCSGCI
00128      03  WS-CF-THRESHOLD   COMP-1 VALUE +0.200000E+00.            ELGCSGCI
00129 /                                                                 ELGCSGCI
00130  01  HEADER-LINE.                                                 ELGCSGCI
00131      03  FILLER         PIC X(22)      VALUE SPACES.              ELGCSGCI
00132      03  FILLER         PIC X(57)      VALUE                      ELGCSGCI
00133          'GENERAL   CONTRACT   INFORMATION'.                      ELGCSGCI
00134                                                                   ELGCSGCI
00135  01  MASK-LINE.                                                   ELGCSGCI
00136      03  FILLER         PIC X(30)      VALUE SPACES.              ELGCSGCI
00137      03  FILLER         PIC X(49)      VALUE '|'.                 ELGCSGCI
00138                                                                   ELGCSGCI
00139  01  DETAIL-LINE.                                                 ELGCSGCI
00140      03  LEFT-SIDE    PIC X(31).                                  ELGCSGCI
00141      03  FILLER       PIC XX             VALUE SPACES.            ELGCSGCI
00142      03  RIGHT-SIDE   PIC X(46).                                  ELGCSGCI
00143      03  FILLER  REDEFINES  RIGHT-SIDE.                           ELGCSGCI
00144          05  RIGHT-SIDE-A    PIC X(23).                           ELGCSGCI
00145          05  RIGHT-SIDE-B    PIC X(23).                           ELGCSGCI
00146                                                                   ELGCSGCI
00147  01  PRICING-HEADER-SW        PIC X(01)  VALUE SPACES.            ELGCSGCI
00148          88  PRICING-HEADER-NOT-DISPLAYED    VALUE 'N'.           ELGCSGCI
00149          88  PRICING-HEADER-DISPLAYED        VALUE 'Y'.           ELGCSGCI
00150                                                                   ELGCSGCI
00151  01  LEFT-FILLER.                                                 ELGCSGCI
00152      03  FILLER           PIC X(30)      VALUE SPACES.            ELGCSGCI
00153      03  FILLER           PIC X(01)      VALUE '|'.               ELGCSGCI
00154                                                                   ELGCSGCI
00155  01  WS-DOLLAR-EDIT       PIC $$,$$$,$$9.99.                      ELGCSGCI
00156  01  WS-NINE-EDIT         PIC 9(07)V99.                           ELGCSGCI
00157  01  WS-WHOLE-NUM  REDEFINES WS-NINE-EDIT   PIC X(09).            ELGCSGCI
00158  01  FILLER  REDEFINES  WS-NINE-EDIT.                             ELGCSGCI
00159      03  FILLER           PIC X(07).                              ELGCSGCI
00160          88  ALL-NINES       VALUE '9999999'.                     ELGCSGCI
00161      03  FILLER           PIC X(02).                              ELGCSGCI
00162                                                                   ELGCSGCI
00163  01  EDIT-LITERALS.                                               ELGCSGCI
00164      03  OVERALL-DEDUCT-LIT.                                      ELGCSGCI
00165          05 FILLER            PIC X(09)      VALUE SPACES.        ELGCSGCI
00166          05 FILLER            PIC X(22)      VALUE                ELGCSGCI
00167              'OVERALL DEDUCTIBLES  |'.                            ELGCSGCI
00168      03  OVERALL-PRICING-LIT.                                     ELGCSGCI
00169          05 FILLER            PIC X(09)      VALUE SPACES.        ELGCSGCI
00170          05 FILLER            PIC X(22)      VALUE                ELGCSGCI
00171              'OVERALL COINSURANCE  |'.                            ELGCSGCI
00172      03  OVERALL-OPX-LIT.                                         ELGCSGCI
00173          05 FILLER            PIC X(07)      VALUE SPACES.        ELGCSGCI
00174          05 FILLER            PIC X(24)      VALUE                ELGCSGCI
00175              'OVERALL OUT-OF-POCKET  |'.                          ELGCSGCI
00176      03  OVERALL-STOP-LOSS.                                       ELGCSGCI
00177          05 FILLER            PIC X(11)      VALUE SPACES.        ELGCSGCI
00178          05 FILLER            PIC X(20)      VALUE                ELGCSGCI
00179          'OVERALL STOP LOSS  |'.                                  ELGCSGCI
00180      03  OVERALL-MAXIMUM-LIT.                                     ELGCSGCI
00181          05 FILLER            PIC X(13)      VALUE SPACES.        ELGCSGCI
00182          05 FILLER            PIC X(18)      VALUE                ELGCSGCI
00183          'OVERALL MAXIMUM  |'.                                    ELGCSGCI
00184      03  AGE-LIMIT-LIT.                                           ELGCSGCI
00185          05 FILLER            PIC X(19)      VALUE SPACES.        ELGCSGCI
00186          05 FILLER            PIC X(12)      VALUE                ELGCSGCI
00187          'AGE LIMIT  |'.                                          ELGCSGCI
00188      03  BENEFIT-PERIOD-LIT.                                      ELGCSGCI
00189          05 FILLER            PIC X(14)      VALUE SPACES.        ELGCSGCI
00190          05 FILLER            PIC X(17)      VALUE                ELGCSGCI
00191          'BENEFIT PERIOD  |'.                                     ELGCSGCI
00192      03  LOB-LIT.                                                 ELGCSGCI
00193          05 FILLER            PIC X(12)      VALUE SPACES.        ELGCSGCI
00194          05 FILLER            PIC X(19)      VALUE                ELGCSGCI
00195          'LINE OF BUSINESS  |'.                                   ELGCSGCI
00196      03  POT-LIT.                                                 ELGCSGCI
00197          05 FILLER            PIC X(10)      VALUE SPACES.        ELGCSGCI
00198          05 FILLER            PIC X(21)      VALUE                ELGCSGCI
00199          'PLACE OF TREATMENT  |'.                                 ELGCSGCI
00200      03  CONDITION-LIT.                                           ELGCSGCI
00201          05 FILLER            PIC X(18)      VALUE SPACES.        ELGCSGCI
00202          05 FILLER            PIC X(13)      VALUE                ELGCSGCI
00203          'CONDITIONS  |'.                                         ELGCSGCI
00204      03  HOSP-LIT.                                                ELGCSGCI
00205          05 FILLER            PIC X(20)      VALUE SPACES.        ELGCSGCI
00206          05 FILLER            PIC X(11)      VALUE                ELGCSGCI
00207          'HOSPITAL  |'.                                           ELGCSGCI
00208      03  PHYS-LIT.                                                ELGCSGCI
00209          05 FILLER            PIC X(18)      VALUE SPACES.        ELGCSGCI
00210          05 FILLER            PIC X(13)      VALUE                ELGCSGCI
00211          'PHYSICIANS  |'.                                         ELGCSGCI
00212      03  SUPPLEMENTAL-LIT.                                        ELGCSGCI
00213          05 FILLER            PIC X(16)      VALUE SPACES.        ELGCSGCI
00214          05 FILLER            PIC X(15)      VALUE                ELGCSGCI
00215          'SUPPLEMENTAL  |'.                                       ELGCSGCI
00216      03  COMP-LIT.                                                ELGCSGCI
00217          05 FILLER            PIC X(31)      VALUE                ELGCSGCI
00218          ' COMPREHENSIVE MAJOR MEDICAL  |'.                       ELGCSGCI
00219      03  TIMELY-FILING-LIT.                                       ELGCSGCI
00220          05 FILLER            PIC X(15)      VALUE SPACES.        ELGCSGCI
00221          05 FILLER            PIC X(16)      VALUE                ELGCSGCI
00222          'TIMELY FILING  |'.                                      ELGCSGCI
00223      03  TYPE-OF-CONTRACT-LIT.                                    ELGCSGCI
00224          05 FILLER            PIC X(12)      VALUE SPACES.        ELGCSGCI
00225          05 FILLER            PIC X(19)      VALUE                ELGCSGCI
00226          'TYPE OF CONTRACT  |'.                                   ELGCSGCI
00227      03  NETWORK-UTIL-REV-LIT.                                    ELGCSGCI
00228          05 FILLER            PIC X(31)      VALUE                ELGCSGCI
00229          ' NETWORK/UTILIZATION REVIEW   |'.                       ELGCSGCI
00230      03  INDIVIDUAL-LIT.                                          ELGCSGCI
00231          05 FILLER           PIC X(15)  VALUE ' PER INDIVIDUAL'.  ELGCSGCI
00232      03  FAMILY-LIT.                                              ELGCSGCI
00233          05 FILLER           PIC X(11)  VALUE ' PER FAMILY'.      ELGCSGCI
00234      03  DETERMINE-LIT.                                           ELGCSGCI
00235          05 FILLER           PIC X(14)  VALUE ' DETERMINED BY'.   ELGCSGCI
00236      03  U-AND-C-LIT.                                             ELGCSGCI
00237          05 FILLER           PIC X(19)  VALUE                     ELGCSGCI
00238             'USUAL AND CUSTOMARY'.                                ELGCSGCI
00239      03  PEOPLE-LIT.                                              ELGCSGCI
00240          05 FILLER           PIC X(43)  VALUE                     ELGCSGCI
00241             ' TIMES THE INDIVIDUAL DEDUCTIBLE PER FAMILY'.        ELGCSGCI
00242      03  DEPENDENT-LIT.                                           ELGCSGCI
00243          05 FILLER            PIC X(10)      VALUE 'DEPENDENT'.   ELGCSGCI
00244          05  DEPENDENT-AGE    PIC ZZ9.                            ELGCSGCI
00245      03  STUDENT-LIT.                                             ELGCSGCI
00246          05 FILLER            PIC X(08)      VALUE 'STUDENT'.     ELGCSGCI
00247          05 STUDENT-AGE      PIC ZZ9.                             ELGCSGCI
00248      03  FIRST-PERCENTAGE.                                        ELGCSGCI
00249          05 FIRST-PERC-EDIT      PIC ZZ9.                         ELGCSGCI
00250          05 FILLER               PIC X(01)    VALUE '%'.          ELGCSGCI
00251 /                                                                 ELGCSGCI
00252  LINKAGE SECTION.                                                 ELGCSGCI
00253  01  DFHCOMMAREA.                                                 ELGCSGCI
00254      COPY ELSCOMMC.                                               ELGCSGCI
00255 /                                                                 ELGCSGCI
00256      COPY ELSCIA2C.                                               ELGCSGCI
00257 /                                                                 ELGCSGCI
00258      COPY ELSSSCBC.                                               ELGCSGCI
00259 /                                                                 ELGCSGCI
00260      COPY ELSCSACC.                                               ELGCSGCI
00261 /                                                                 ELGCSGCI
00262      COPY ELSRRBLC.                                               ELGCSGCI
00263 /                                                                 ELGCSGCI
00264      COPY ELSATBLC.                                               ELGCSGCI
00265 /                                                                 ELGCSGCI
00266      COPY ELSIOPMC.                                               ELGCSGCI
00267 /                                                                 ELGCSGCI
00268      COPY ELSKEYSC.                                               ELGCSGCI
00269 /                                                                 ELGCSGCI
00270      COPY ELSOUTPC.                                               ELGCSGCI
00271 /                                                                 ELGCSGCI
00272      COPY ELSTCWAC.                                               ELGCSGCI
00273 /                                                                 ELGCSGCI
00274      COPY ELSCMIFC.                                               ELGCSGCI
00275 /                                                                 ELGCSGCI
00276      COPY ELSCMDSC.                                               ELGCSGCI
00277 /                                                                 ELGCSGCI
00278  01  GROUP-SPECIFIC-RECORD.                                       ELGCSGCI
00279      COPY GCGROUPC.                                               ELGCSGCI
00280 /                                                                 ELGCSGCI
00281  01  CONTRACT-RECORD.                                             ELGCSGCI
00282      COPY GCCONTRC.                                               ELGCSGCI
00283 /                                                                 ELGCSGCI
00284  01  BENEFIT-PROVISION-RECORD.                                    ELGCSGCI
00285      COPY GCBENPVC.                                               ELGCSGCI
00286 /                                                                 ELGCSGCI
00287                                                                   ELGCSGCI
00288      EJECT                                                        ELGCSGCI
00289  PROCEDURE DIVISION.                                              ELGCSGCI
00290 ************************************************************      ELGCSGCI
00291 *                                                          *      ELGCSGCI
00292 *                    PROCEDURE DIVISION                    *      ELGCSGCI
00293 *                                                          *      ELGCSGCI
00294 ************************************************************      ELGCSGCI
00295                                                                   ELGCSGCI
00296                                                                   ELGCSGCI
00297 ************************************************************      ELGCSGCI
00298 *                                                          *      ELGCSGCI
00299 *        GENERAL CONTRACT INQUIRY                          *      ELGCSGCI
00300 *                                                          *      ELGCSGCI
00301 ************************************************************      ELGCSGCI
00302  GENERAL-CONTRACT-INQUIRY.                                        ELGCSGCI
00303      PERFORM INITIALIZATION.                                      ELGCSGCI
00304      PERFORM PROCESS-GENERAL-CONTRACT-INFOR.                      ELGCSGCI
00305      GOBACK.                                                      ELGCSGCI
00306                                                                   ELGCSGCI
00307 ************************************************************      ELGCSGCI
00308 *                                                          *      ELGCSGCI
00309 *        INITIALIZATION.                                   *      ELGCSGCI
00310 *                                                          *      ELGCSGCI
00311 ************************************************************      ELGCSGCI
00312  INITIALIZATION.                                                  ELGCSGCI
00313      PERFORM ESTABLISH-ADDRESS-OF-CO.                             ELGCSGCI
00314      PERFORM ESTABLISH-ADDRESSABILITY-OF-EL.                      ELGCSGCI
00315      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELGCSGCI
00316      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELGCSGCI
00317      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELGCSGCI
00318      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCSGCI
00319      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELGCSGCI
00320                                                                   ELGCSGCI
00321                                                                   ELGCSGCI
00322 ************************************************************      ELGCSGCI
00323 *                                                          *      ELGCSGCI
00324 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCSGCI
00325 *                                                          *      ELGCSGCI
00326 ************************************************************      ELGCSGCI
00327  ESTABLISH-ADDRESS-OF-CO.                                         ELGCSGCI
00328      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCSGCI
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-CI.                      ELGCSGCI
00330      PERFORM ESTABLISH-ADDRESSABILITY-CSAC.                       ELGCSGCI
00331                                                                   ELGCSGCI
00332 ************************************************************      ELGCSGCI
00333 *                                                          *      ELGCSGCI
00334 *        CHECK FOR VALID COMMAREA                          *      ELGCSGCI
00335 *                                                          *      ELGCSGCI
00336 ************************************************************      ELGCSGCI
00337  CHECK-FOR-VALID-COMMAREA.                                        ELGCSGCI
00338      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSGCI
00339         EXEC CICS ABEND                                           ELGCSGCI
00340                   ABCODE ('EL01')                                 ELGCSGCI
00341                   END-EXEC.                                       ELGCSGCI
00342                                                                   ELGCSGCI
00343 ************************************************************      ELGCSGCI
00344 *                                                          *      ELGCSGCI
00345 *        ESTABLISH ADDRESSABILITY OF CIA                   *      ELGCSGCI
00346 *                                                          *      ELGCSGCI
00347 ************************************************************      ELGCSGCI
00348  ESTABLISH-ADDRESSABILITY-OF-CI.                                  ELGCSGCI
00349      IF ECA-CIA-PTR = NULL                                        ELGCSGCI
00350         EXEC CICS ABEND                                           ELGCSGCI
00351                   ABCODE ('EL02')                                 ELGCSGCI
00352                   END-EXEC.                                       ELGCSGCI
00353      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSGCI
00354                      ADDRESS OF                                   ELGCSGCI
00355          CIA-ELS-COMMON-INTERFACE-AREA.                           ELGCSGCI
00356                                                                   ELGCSGCI
00357                                                                   ELGCSGCI
00358 ************************************************************      ELGCSGCI
00359 *                                                          *      ELGCSGCI
00360 *        ESTABLISH ADDRESSABILITY OF ELSCSAC               *      ELGCSGCI
00361 *                                                          *      ELGCSGCI
00362 ************************************************************      ELGCSGCI
00363  ESTABLISH-ADDRESSABILITY-CSAC.                                   ELGCSGCI
00364      SET  CIA-ELSCSAC-DDN   TO TRUE.                              ELGCSGCI
00365      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00366                      ADDRESS OF CSAC-ACCUMULATOR-TABLE.           ELGCSGCI
00367      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00368          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00369                                                                   ELGCSGCI
00370 ************************************************************      ELGCSGCI
00371 *                                                          *      ELGCSGCI
00372 *        ESTABLISH ADDRESSABILITY OF ELSSSCB               *      ELGCSGCI
00373 *                                                          *      ELGCSGCI
00374 ************************************************************      ELGCSGCI
00375  ESTABLISH-ADDRESSABILITY-OF-EL.                                  ELGCSGCI
00376      SET  CIA-ELSSSCB-DDN   TO TRUE.                              ELGCSGCI
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00378                      ADDRESS OF                                   ELGCSGCI
00379          SSB-SELECTOR-STATUS-CTL-BLK.                             ELGCSGCI
00380      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00381          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00382                                                                   ELGCSGCI
00383                                                                   ELGCSGCI
00384 ************************************************************      ELGCSGCI
00385 *                                                          *      ELGCSGCI
00386 *        SIGNAL UNALLOCATED AREA ERROR                     *      ELGCSGCI
00387 *                                                          *      ELGCSGCI
00388 ************************************************************      ELGCSGCI
00389  SIGNAL-UNALLOCATED-AREA-ERROR.                                   ELGCSGCI
00390      SET  CIA-AB-UNALLOC-AREA  TO TRUE.                           ELGCSGCI
00391      PERFORM SIGNAL-ABEND.                                        ELGCSGCI
00392                                                                   ELGCSGCI
00393                                                                   ELGCSGCI
00394 ************************************************************      ELGCSGCI
00395 *                                                          *      ELGCSGCI
00396 *        SIGNAL ABEND                                      *      ELGCSGCI
00397 *                                                          *      ELGCSGCI
00398 ************************************************************      ELGCSGCI
00399  SIGNAL-ABEND.                                                    ELGCSGCI
00400      EXEC CICS ABEND                                              ELGCSGCI
00401                ABCODE (CIA-ABCODE)                                ELGCSGCI
00402                END-EXEC.                                          ELGCSGCI
00403                                                                   ELGCSGCI
00404 ************************************************************      ELGCSGCI
00405 *                                                          *      ELGCSGCI
00406 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELGCSGCI
00407 *                                                          *      ELGCSGCI
00408 ************************************************************      ELGCSGCI
00409  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELGCSGCI
00410      SET  CIA-ELSGRPSP-DDN  TO TRUE.                              ELGCSGCI
00411      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00412                      ADDRESS OF GROUP-SPECIFIC-RECORD.            ELGCSGCI
00413      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00414          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00415                                                                   ELGCSGCI
00416 ************************************************************      ELGCSGCI
00417 *                                                          *      ELGCSGCI
00418 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGCSGCI
00419 *                                                          *      ELGCSGCI
00420 ************************************************************      ELGCSGCI
00421  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELGCSGCI
00422      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGCSGCI
00423      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00424                      ADDRESS OF                                   ELGCSGCI
00425          CMF-CODES-MANUAL-INTERFACE.                              ELGCSGCI
00426      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00427          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00428                                                                   ELGCSGCI
00429 ************************************************************      ELGCSGCI
00430 *                                                          *      ELGCSGCI
00431 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION      *      ELGCSGCI
00432 *                                                          *      ELGCSGCI
00433 ************************************************************      ELGCSGCI
00434  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELGCSGCI
00435      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGCSGCI
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00437                      ADDRESS OF                                   ELGCSGCI
00438          TCAR-COMPRESSION-WORK-AREA.                              ELGCSGCI
00439      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00440          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00441                                                                   ELGCSGCI
00442 ************************************************************      ELGCSGCI
00443 *                                                          *      ELGCSGCI
00444 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCSGCI
00445 *                                                          *      ELGCSGCI
00446 ************************************************************      ELGCSGCI
00447  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCSGCI
00448      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSGCI
00449      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00450                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELGCSGCI
00451      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00452          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00453                                                                   ELGCSGCI
00454 ************************************************************      ELGCSGCI
00455 *                                                          *      ELGCSGCI
00456 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELGCSGCI
00457 *                                                          *      ELGCSGCI
00458 ************************************************************      ELGCSGCI
00459  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELGCSGCI
00460      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGCSGCI
00461      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00462                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELGCSGCI
00463      IF CIA-RC-PTR-NULL                                           ELGCSGCI
00464          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELGCSGCI
00465                                                                   ELGCSGCI
00466 ************************************************************      ELGCSGCI
00467 *                                                          *      ELGCSGCI
00468 *        PROCESS GENERAL CONTRACT INFORMATION              *      ELGCSGCI
00469 *                                                          *      ELGCSGCI
00470 ************************************************************      ELGCSGCI
00471  PROCESS-GENERAL-CONTRACT-INFOR.                                  ELGCSGCI
00472      PERFORM CREATE-HEADER.                                       ELGCSGCI
00473      IF CSAC-ADL-GC-TBL-PTR NOT = NULLS                           ELGCSGCI
00474          PERFORM FIND-CONFIDENCE-FACTORS-FOR-AD.                  ELGCSGCI
00475      IF SSB-PROF-BAS-L-O-B = '2'  OR  '4'                         ELGCSGCI
00476          PERFORM DETERMINE-SCOPE.                                 ELGCSGCI
00477      IF CSAC-ACL-GC-TBL-PTR NOT = NULLS                           ELGCSGCI
00478          PERFORM FIND-CONFIDENCE-FACTORS-FOR-AC.                  ELGCSGCI
00479      IF BS-ACL-MSG-NOT-DISPLAYED  AND                             ELGCSGCI
00480                  SSB-PROF-BAS-L-O-B = '2'  OR  '4'                ELGCSGCI
00481          PERFORM DISPLAY-BS-COINSURANCE.                          ELGCSGCI
00482      IF CSAC-AOL-GC-TBL-PTR NOT = NULLS                           ELGCSGCI
00483          PERFORM FIND-CONFIDENCE-FACTORS-FOR-AO.                  ELGCSGCI
00484      IF CSAC-ABM-GC-TBL-PTR NOT = NULLS                           ELGCSGCI
00485          PERFORM FIND-CONFIDENCE-FACTORS-FOR-AB.                  ELGCSGCI
00486      IF GCG-DEP-MAX-AGE > ZEROES  OR                              ELGCSGCI
00487                   GCG-STU-MAX-AGE > ZEROES                        ELGCSGCI
00488          PERFORM CONSTRUCT-AGE-LIMIT-VERBIAGE.                    ELGCSGCI
00489      IF GCG-TIMELY-FILG-IND                                       ELGCSGCI
00490                       NOT = ZEROES AND LOW-VALUES AND             ELGCSGCI
00491          SPACES                                                   ELGCSGCI
00492          PERFORM CONSTRUCT-TIMELY-FILING-VERBIA.                  ELGCSGCI
00493      IF GCG-L-O-B-CONTRACT-LEVEL-IND                              ELGCSGCI
00494                       NOT = ZEROES AND LOW-VALUES AND             ELGCSGCI
00495          SPACES                                                   ELGCSGCI
00496          PERFORM CONSTRUCT-TYPE-OF-CONTRACT-VER.                  ELGCSGCI
00497                                                                   ELGCSGCI
00498      IF GCG-NETWORK-UTIL-REVIEW-IND                               ELGCSGCI
00499                       NOT = ZEROES AND LOW-VALUES AND             ELGCSGCI
00500          SPACES                                                   ELGCSGCI
00501          PERFORM CONSTRUCT-NETWORK-UTIL-REV-PHR.                  ELGCSGCI
00502                                                                   ELGCSGCI
00503 ************************************************************      ELGCSGCI
00504 *                                                          *      ELGCSGCI
00505 *        CREATE HEADER                                     *      ELGCSGCI
00506 *                                                          *      ELGCSGCI
00507 ************************************************************      ELGCSGCI
00508  CREATE-HEADER.                                                   ELGCSGCI
00509      SET COF-NEW-PAGE           TO TRUE.                          ELGCSGCI
00510      MOVE MASK-LINE             TO COF-MASK-LINE.                 ELGCSGCI
00511      MOVE +3                    TO COF-NBR-HDR-LINES.             ELGCSGCI
00512      MOVE ZERO                  TO COF-NBR-DTL-LINES.             ELGCSGCI
00513      MOVE HEADER-LINE           TO COF-HDR-LINE (2).              ELGCSGCI
00514      MOVE ALL '-'               TO COF-HDR-LINE (3).              ELGCSGCI
00515      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
00516      SET COF-CONTINUE           TO TRUE.                          ELGCSGCI
00517      MOVE ZERO                  TO COF-NBR-HDR-LINES.             ELGCSGCI
00518                                                                   ELGCSGCI
00519 ************************************************************      ELGCSGCI
00520 *                                                          *      ELGCSGCI
00521 *        FIND CONFIDENCE FACTORS FOR ABM                   *      ELGCSGCI
00522 *                                                          *      ELGCSGCI
00523 ************************************************************      ELGCSGCI
00524  FIND-CONFIDENCE-FACTORS-FOR-AB.                                  ELGCSGCI
00525      SET PROCESSING-ABM TO TRUE.                                  ELGCSGCI
00526      INITIALIZE PROCESS-TABLE.                                    ELGCSGCI
00527      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE   TO                   ELGCSGCI
00528          CSAC-ABM-GC-TBL-PTR.                                     ELGCSGCI
00529      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ABM-RR-PTR.  ELGCSGCI
00530      SET RRBL-X-IDX TO 1.                                         ELGCSGCI
00531      PERFORM LOOP-THRU-ELSRRBL                                    ELGCSGCI
00532          VARYING RRBL-X-IDX FROM 1 BY 1                           ELGCSGCI
00533                  UNTIL RRBL-X-IDX GREATER THAN                    ELGCSGCI
00534              RRBL-TBL-CNT.                                        ELGCSGCI
00535                                                                   ELGCSGCI
00536 ************************************************************      ELGCSGCI
00537 *                                                          *      ELGCSGCI
00538 *        FIND CONFIDENCE FACTORS FOR ACL                   *      ELGCSGCI
00539 *                                                          *      ELGCSGCI
00540 ************************************************************      ELGCSGCI
00541  FIND-CONFIDENCE-FACTORS-FOR-AC.                                  ELGCSGCI
00542      SET PROCESSING-ACL TO TRUE.                                  ELGCSGCI
00543      INITIALIZE PROCESS-TABLE.                                    ELGCSGCI
00544      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE   TO                   ELGCSGCI
00545          CSAC-ACL-GC-TBL-PTR.                                     ELGCSGCI
00546      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ACL-RR-PTR.  ELGCSGCI
00547      SET PRICING-HEADER-NOT-DISPLAYED TO TRUE.                    ELGCSGCI
00548      SET RRBL-X-IDX TO 1.                                         ELGCSGCI
00549      PERFORM LOOP-THRU-ELSRRBL                                    ELGCSGCI
00550          VARYING RRBL-X-IDX FROM 1 BY 1                           ELGCSGCI
00551                  UNTIL RRBL-X-IDX GREATER THAN                    ELGCSGCI
00552              RRBL-TBL-CNT.                                        ELGCSGCI
00553                                                                   ELGCSGCI
00554 ************************************************************      ELGCSGCI
00555 *                                                          *      ELGCSGCI
00556 *        DETERMINE SCOPE                                   *      ELGCSGCI
00557 *                                                          *      ELGCSGCI
00558 ************************************************************      ELGCSGCI
00559  DETERMINE-SCOPE.                                                 ELGCSGCI
00560      SET CIA-ELSCONPB-DDN TO TRUE.                                ELGCSGCI
00561      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00562                      ADDRESS OF CONTRACT-RECORD.                  ELGCSGCI
00563      IF CIA-RC-OK                                                 ELGCSGCI
00564          PERFORM SEARCH-BENEFIT-PROVISIONS.                       ELGCSGCI
00565                                                                   ELGCSGCI
00566 ************************************************************      ELGCSGCI
00567 *                                                          *      ELGCSGCI
00568 *        SEARCH BENEFIT PROVISIONS                         *      ELGCSGCI
00569 *                                                          *      ELGCSGCI
00570 ************************************************************      ELGCSGCI
00571  SEARCH-BENEFIT-PROVISIONS.                                       ELGCSGCI
00572      SET PROVISION-NOT-FOUND TO TRUE.                             ELGCSGCI
00573      PERFORM SEARCH-BENEFIT-PROVISION-ENTRI                       ELGCSGCI
00574          VARYING GCT-INDEX FROM 1 BY 1                            ELGCSGCI
00575                      UNTIL GCT-INDEX >                            ELGCSGCI
00576              GCT-COUNT-BEN-PROVN-POINTERS  OR                     ELGCSGCI
00577                      PROVISION-FOUND.                             ELGCSGCI
00578                                                                   ELGCSGCI
00579                                                                   ELGCSGCI
00580 ************************************************************      ELGCSGCI
00581 *                                                          *      ELGCSGCI
00582 *        SEARCH BENEFIT PROVISION ENTRIES                  *      ELGCSGCI
00583 *                                                          *      ELGCSGCI
00584 ************************************************************      ELGCSGCI
00585  SEARCH-BENEFIT-PROVISION-ENTRI.                                  ELGCSGCI
00586      IF GCT-BP-ID (GCT-INDEX) = 'SRGI '                           ELGCSGCI
00587          PERFORM SAVE-BENEFIT-SCOPE.                              ELGCSGCI
00588                                                                   ELGCSGCI
00589 ************************************************************      ELGCSGCI
00590 *                                                          *      ELGCSGCI
00591 *        SAVE BENEFIT SCOPE                                *      ELGCSGCI
00592 *                                                          *      ELGCSGCI
00593 ************************************************************      ELGCSGCI
00594  SAVE-BENEFIT-SCOPE.                                              ELGCSGCI
00595      SET PROVISION-FOUND TO TRUE.                                 ELGCSGCI
00596      SET CIA-GCBENPRV-DDN TO TRUE.                                ELGCSGCI
00597      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
00598                            ADDRESS OF                             ELGCSGCI
00599          IOP-INPUT-OUTPUT-PARAMETERS.                             ELGCSGCI
00600      SET CIA-GCBENPRV-DDN TO TRUE.                                ELGCSGCI
00601      MOVE GCT-BEN-PROVN-ID (GCT-INDEX)      TO KWA-PROVISION-ID.  ELGCSGCI
00602      MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) TO                    ELGCSGCI
00603          KWA-PROVISION-SLOT-NO.                                   ELGCSGCI
00604      MOVE KWA-GCBENPRV-KEY TO IOP-FILE-KEY.                       ELGCSGCI
00605      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGCSGCI
00606      SET IOP-RD TO TRUE.                                          ELGCSGCI
00607      SET IOP-FCQ-NONE TO TRUE.                                    ELGCSGCI
00608      SET IOP-KVQ-EQ TO TRUE.                                      ELGCSGCI
00609      PERFORM READ-BENEFIT-PROVISION.                              ELGCSGCI
00610      SET ADDRESS OF BENEFIT-PROVISION-RECORD TO IOP-REC-PTR.      ELGCSGCI
00611      MOVE GPC-BEN-SCOPE-ID TO WS-BENEFIT-SCOPE.                   ELGCSGCI
00612                                                                   ELGCSGCI
00613 ************************************************************      ELGCSGCI
00614 *                                                          *      ELGCSGCI
00615 *        READ BENEFIT PROVISION                            *      ELGCSGCI
00616 *                                                          *      ELGCSGCI
00617 ************************************************************      ELGCSGCI
00618  READ-BENEFIT-PROVISION.                                          ELGCSGCI
00619      PERFORM LINK-TO-I-O-PGM.                                     ELGCSGCI
00620      IF NOT IOP-RC-OK                                             ELGCSGCI
00621         SET CIA-AB-NOTFND-GCBENPRV TO TRUE                        ELGCSGCI
00622         EXEC CICS ABEND                                           ELGCSGCI
00623                   ABCODE (CIA-ABCODE)                             ELGCSGCI
00624              END-EXEC.                                            ELGCSGCI
00625                                                                   ELGCSGCI
00626 ************************************************************      ELGCSGCI
00627 *                                                          *      ELGCSGCI
00628 *        LINK TO I-O PGM                                   *      ELGCSGCI
00629 *                                                          *      ELGCSGCI
00630 ************************************************************      ELGCSGCI
00631  LINK-TO-I-O-PGM.                                                 ELGCSGCI
00632      EXEC CICS LINK                                               ELGCSGCI
00633                PROGRAM ('ELUIOPGM')                               ELGCSGCI
00634                COMMAREA (DFHCOMMAREA)                             ELGCSGCI
00635           END-EXEC.                                               ELGCSGCI
00636                                                                   ELGCSGCI
00637 ************************************************************      ELGCSGCI
00638 *                                                          *      ELGCSGCI
00639 *        FIND CONFIDENCE FACTORS FOR ADL                   *      ELGCSGCI
00640 *                                                          *      ELGCSGCI
00641 ************************************************************      ELGCSGCI
00642  FIND-CONFIDENCE-FACTORS-FOR-AD.                                  ELGCSGCI
00643      SET PROCESSING-ADL TO TRUE.                                  ELGCSGCI
00644      INITIALIZE PROCESS-TABLE.                                    ELGCSGCI
00645      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE   TO                   ELGCSGCI
00646          CSAC-ADL-GC-TBL-PTR.                                     ELGCSGCI
00647      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-ADL-RR-PTR.  ELGCSGCI
00648      SET RRBL-X-IDX TO 1.                                         ELGCSGCI
00649      PERFORM LOOP-THRU-ELSRRBL                                    ELGCSGCI
00650          VARYING RRBL-X-IDX FROM 1 BY 1                           ELGCSGCI
00651                  UNTIL RRBL-X-IDX GREATER THAN                    ELGCSGCI
00652              RRBL-TBL-CNT.                                        ELGCSGCI
00653                                                                   ELGCSGCI
00654 ************************************************************      ELGCSGCI
00655 *                                                          *      ELGCSGCI
00656 *        FIND CONFIDENCE FACTORS FOR AOL                   *      ELGCSGCI
00657 *                                                          *      ELGCSGCI
00658 ************************************************************      ELGCSGCI
00659  FIND-CONFIDENCE-FACTORS-FOR-AO.                                  ELGCSGCI
00660      SET PROCESSING-AOL TO TRUE.                                  ELGCSGCI
00661      INITIALIZE PROCESS-TABLE.                                    ELGCSGCI
00662      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE   TO                   ELGCSGCI
00663          CSAC-AOL-GC-TBL-PTR.                                     ELGCSGCI
00664      SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST TO CSAC-AOL-RR-PTR.  ELGCSGCI
00665      SET RRBL-X-IDX TO 1.                                         ELGCSGCI
00666      PERFORM LOOP-THRU-ELSRRBL                                    ELGCSGCI
00667          VARYING RRBL-X-IDX FROM 1 BY 1                           ELGCSGCI
00668                  UNTIL RRBL-X-IDX GREATER THAN                    ELGCSGCI
00669              RRBL-TBL-CNT.                                        ELGCSGCI
00670                                                                   ELGCSGCI
00671                                                                   ELGCSGCI
00672 ************************************************************      ELGCSGCI
00673 *                                                          *      ELGCSGCI
00674 *        LOOP THRU ELSRRBL                                 *      ELGCSGCI
00675 *                                                          *      ELGCSGCI
00676 ************************************************************      ELGCSGCI
00677  LOOP-THRU-ELSRRBL.                                               ELGCSGCI
00678      SET RRBL-Y-IDX TO 1.                                         ELGCSGCI
00679      PERFORM LOOP-THRU-CF-ENTRIES                                 ELGCSGCI
00680          VARYING RRBL-Y-IDX FROM 1 BY 1                           ELGCSGCI
00681                   UNTIL RRBL-Y-IDX GREATER THAN                   ELGCSGCI
00682              ATBL-TBL-CNT.                                        ELGCSGCI
00683                                                                   ELGCSGCI
00684                                                                   ELGCSGCI
00685 ************************************************************      ELGCSGCI
00686 *                                                          *      ELGCSGCI
00687 *        LOOP THRU CF ENTRIES                              *      ELGCSGCI
00688 *                                                          *      ELGCSGCI
00689 ************************************************************      ELGCSGCI
00690  LOOP-THRU-CF-ENTRIES.                                            ELGCSGCI
00691      IF RRBL-CF-ENTRIES (RRBL-X-IDX, RRBL-Y-IDX) >                ELGCSGCI
00692          WS-CF-THRESHOLD                                          ELGCSGCI
00693                                       OR   =                      ELGCSGCI
00694          WS-CF-THRESHOLD                                          ELGCSGCI
00695          PERFORM CHECK-TO-PROCESS.                                ELGCSGCI
00696                                                                   ELGCSGCI
00697                                                                   ELGCSGCI
00698 ************************************************************      ELGCSGCI
00699 *                                                          *      ELGCSGCI
00700 *        CHECK TO PROCESS                                  *      ELGCSGCI
00701 *                                                          *      ELGCSGCI
00702 ************************************************************      ELGCSGCI
00703  CHECK-TO-PROCESS.                                                ELGCSGCI
00704      SET WS-IDX TO RRBL-Y-IDX.                                    ELGCSGCI
00705      IF ACCUM-DISPLAYED (WS-IDX)                                  ELGCSGCI
00706          CONTINUE                                                 ELGCSGCI
00707      ELSE                                                         ELGCSGCI
00708          PERFORM PROCESS-TABULAR.                                 ELGCSGCI
00709                                                                   ELGCSGCI
00710                                                                   ELGCSGCI
00711 ************************************************************      ELGCSGCI
00712 *                                                          *      ELGCSGCI
00713 *        PROCESS TABULAR                                   *      ELGCSGCI
00714 *                                                          *      ELGCSGCI
00715 ************************************************************      ELGCSGCI
00716  PROCESS-TABULAR.                                                 ELGCSGCI
00717      SET ATBL-X-IDX TO RRBL-Y-IDX.                                ELGCSGCI
00718      PERFORM CHECK-LINE-OF-BUSINESS.                              ELGCSGCI
00719      IF PROCESSING-ABM                                            ELGCSGCI
00720          PERFORM CONSTRUCT-VERBIAGE-FOR-ABM.                      ELGCSGCI
00721      IF PROCESSING-ACL                                            ELGCSGCI
00722          PERFORM CONSTRUCT-VERBIAGE-FOR-ACL.                      ELGCSGCI
00723      IF PROCESSING-ADL                                            ELGCSGCI
00724          PERFORM CONSTRUCT-VERBIAGE-FOR-ADL.                      ELGCSGCI
00725      IF PROCESSING-AOL                                            ELGCSGCI
00726          PERFORM CONSTRUCT-VERBIAGE-FOR-AOL.                      ELGCSGCI
00727      SET ACCUM-DISPLAYED (WS-IDX) TO TRUE.                        ELGCSGCI
00728                                                                   ELGCSGCI
00729 ************************************************************      ELGCSGCI
00730 *                                                          *      ELGCSGCI
00731 *        CHECK LINE OF BUSINESS                            *      ELGCSGCI
00732 *                                                          *      ELGCSGCI
00733 ************************************************************      ELGCSGCI
00734  CHECK-LINE-OF-BUSINESS.                                          ELGCSGCI
00735      MOVE ATBL-L-O-B (ATBL-X-IDX) TO WS-L-O-B-CONTRACT.           ELGCSGCI
00736      IF SUPPLEMENTAL-LOB-CONTRACT                                 ELGCSGCI
00737         SET PROCESSING-MAJOR-MEDICAL TO TRUE                      ELGCSGCI
00738      ELSE IF INSTITUTIONAL-LOB-CONTRACT                           ELGCSGCI
00739         SET PROCESSING-INSTITUTIONAL TO TRUE                      ELGCSGCI
00740      ELSE IF PROFESSIONAL-LOB-CONTRACT                            ELGCSGCI
00741         SET PROCESSING-PROFESSIONAL TO TRUE                       ELGCSGCI
00742      ELSE                                                         ELGCSGCI
00743         SET PROCESSING-COMPREHENSIVE TO TRUE.                     ELGCSGCI
00744                                                                   ELGCSGCI
00745 ************************************************************      ELGCSGCI
00746 *                                                          *      ELGCSGCI
00747 *        CONSTRUCT VERBIAGE FOR ABM                        *      ELGCSGCI
00748 *                                                          *      ELGCSGCI
00749 ************************************************************      ELGCSGCI
00750  CONSTRUCT-VERBIAGE-FOR-ABM.                                      ELGCSGCI
00751      INITIALIZE COF-DTL.                                          ELGCSGCI
00752      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
00753      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
00754      PERFORM CONSTRUCT-OVERALL-ABM-PHRASE.                        ELGCSGCI
00755      PERFORM CONSTRUCT-ABM-BENEFIT-PERIOD-P.                      ELGCSGCI
00756      PERFORM CONSTRUCT-ABM-LOB-PHRASE.                            ELGCSGCI
00757      PERFORM CONSTRUCT-ABM-POT-PHRASE.                            ELGCSGCI
00758      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
00759                                                                   ELGCSGCI
00760 ************************************************************      ELGCSGCI
00761 *                                                          *      ELGCSGCI
00762 *        CONSTRUCT OVERALL ABM PHRASE                      *      ELGCSGCI
00763 *                                                          *      ELGCSGCI
00764 ************************************************************      ELGCSGCI
00765  CONSTRUCT-OVERALL-ABM-PHRASE.                                    ELGCSGCI
00766      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
00767      MOVE OVERALL-MAXIMUM-LIT     TO LEFT-SIDE.                   ELGCSGCI
00768      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
00769                 WS-SUBA.                                          ELGCSGCI
00770      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = PC-FIVE               ELGCSGCI
00771          PERFORM USE-ABM-DOLLAR-EDIT                              ELGCSGCI
00772      ELSE                                                         ELGCSGCI
00773          PERFORM USE-ABM-NUMERIC-EDIT.                            ELGCSGCI
00774      IF ATBL-FAM-OR-INDIV (ATBL-X-IDX) = PC-INDIVIDUAL            ELGCSGCI
00775          PERFORM DISPLAY-INDIVIDUAL                               ELGCSGCI
00776      ELSE                                                         ELGCSGCI
00777          PERFORM DISPLAY-FAMILY.                                  ELGCSGCI
00778      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
00779      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
00780      MOVE +1          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
00781      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN.                 ELGCSGCI
00782      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
00783      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
00784      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
00785          (COF-NBR-DTL-LINES).                                     ELGCSGCI
00786                                                                   ELGCSGCI
00787 ************************************************************      ELGCSGCI
00788 *                                                          *      ELGCSGCI
00789 *        USE ABM DOLLAR EDIT                               *      ELGCSGCI
00790 *                                                          *      ELGCSGCI
00791 ************************************************************      ELGCSGCI
00792  USE-ABM-DOLLAR-EDIT.                                             ELGCSGCI
00793      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-DOLLAR-EDIT.  ELGCSGCI
00794      ADD 1 TO WS-SUBA.                                            ELGCSGCI
00795      MOVE WS-DOLLAR-EDIT TO TCAR-FROM-LINE (WS-SUBA).             ELGCSGCI
00796                                                                   ELGCSGCI
00797 ************************************************************      ELGCSGCI
00798 *                                                          *      ELGCSGCI
00799 *        USE ABM NUMERIC EDIT                              *      ELGCSGCI
00800 *                                                          *      ELGCSGCI
00801 ************************************************************      ELGCSGCI
00802  USE-ABM-NUMERIC-EDIT.                                            ELGCSGCI
00803      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-NINE-EDIT.    ELGCSGCI
00804      MOVE PC-ABM                        TO CMF-RECORD-PREFIX.     ELGCSGCI
00805      MOVE 'BAMA-VALUE-QUALIFIER'        TO                        ELGCSGCI
00806          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
00807      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)      TO               ELGCSGCI
00808          CMF-CODE-VALUE.                                          ELGCSGCI
00809      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
00810      ADD 1 TO WS-SUBA.                                            ELGCSGCI
00811      INSPECT WS-WHOLE-NUM REPLACING LEADING ZEROES BY SPACES.     ELGCSGCI
00812      STRING WS-WHOLE-NUM  ' ' CMF-DESCR-LINE (1) DELIMITED BY     ELGCSGCI
00813          SIZE                                                     ELGCSGCI
00814             INTO TCAR-FROM-LINE (WS-SUBA).                        ELGCSGCI
00815                                                                   ELGCSGCI
00816 ************************************************************      ELGCSGCI
00817 *                                                          *      ELGCSGCI
00818 *        DISPLAY INDIVIDUAL                                *      ELGCSGCI
00819 *                                                          *      ELGCSGCI
00820 ************************************************************      ELGCSGCI
00821  DISPLAY-INDIVIDUAL.                                              ELGCSGCI
00822      ADD 1 TO WS-SUBA.                                            ELGCSGCI
00823      MOVE INDIVIDUAL-LIT TO TCAR-FROM-LINE (WS-SUBA).             ELGCSGCI
00824                                                                   ELGCSGCI
00825 ************************************************************      ELGCSGCI
00826 *                                                          *      ELGCSGCI
00827 *        DISPLAY FAMILY                                    *      ELGCSGCI
00828 *                                                          *      ELGCSGCI
00829 ************************************************************      ELGCSGCI
00830  DISPLAY-FAMILY.                                                  ELGCSGCI
00831      ADD 1 TO WS-SUBA.                                            ELGCSGCI
00832      MOVE FAMILY-LIT TO TCAR-FROM-LINE (WS-SUBA).                 ELGCSGCI
00833                                                                   ELGCSGCI
00834 ************************************************************      ELGCSGCI
00835 *                                                          *      ELGCSGCI
00836 *        CONSTRUCT ABM BENEFIT PERIOD PHRASE               *      ELGCSGCI
00837 *                                                          *      ELGCSGCI
00838 ************************************************************      ELGCSGCI
00839  CONSTRUCT-ABM-BENEFIT-PERIOD-P.                                  ELGCSGCI
00840      MOVE PC-ABM                        TO                        ELGCSGCI
00841          CMF-RECORD-PREFIX.                                       ELGCSGCI
00842      MOVE 'BAMA-BENEFIT-PERIOD'         TO                        ELGCSGCI
00843          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
00844      MOVE ATBL-BENEFIT-PERIOD (ATBL-X-IDX)      TO                ELGCSGCI
00845          CMF-CODE-VALUE.                                          ELGCSGCI
00846      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
00847      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
00848      MOVE BENEFIT-PERIOD-LIT     TO LEFT-SIDE.                    ELGCSGCI
00849      MOVE CMF-DESCR-LINE (1) TO RIGHT-SIDE.                       ELGCSGCI
00850      MOVE DETAIL-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).        ELGCSGCI
00851                                                                   ELGCSGCI
00852 ************************************************************      ELGCSGCI
00853 *                                                          *      ELGCSGCI
00854 *        CONSTRUCT ABM LOB PHRASE                          *      ELGCSGCI
00855 *                                                          *      ELGCSGCI
00856 ************************************************************      ELGCSGCI
00857  CONSTRUCT-ABM-LOB-PHRASE.                                        ELGCSGCI
00858      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
00859                 WS-SUBA.                                          ELGCSGCI
00860      MOVE PC-ABM                        TO                        ELGCSGCI
00861          CMF-RECORD-PREFIX.                                       ELGCSGCI
00862      MOVE 'BAMA-L-O-B'                  TO                        ELGCSGCI
00863          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
00864      MOVE ATBL-L-O-B (ATBL-X-IDX)       TO CMF-CODE-VALUE.        ELGCSGCI
00865      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
00866      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
00867          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
00868                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
00869      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
00870      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
00871      MOVE +2          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
00872      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
00873                          TCAR-OUTPUT-FIELD-2-LEN.                 ELGCSGCI
00874      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
00875      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
00876      MOVE LOB-LIT                TO LEFT-SIDE.                    ELGCSGCI
00877      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
00878      MOVE DETAIL-LINE            TO COF-DTL-LINE                  ELGCSGCI
00879          (COF-NBR-DTL-LINES).                                     ELGCSGCI
00880      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
00881          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
00882                                                                   ELGCSGCI
00883 ************************************************************      ELGCSGCI
00884 *                                                          *      ELGCSGCI
00885 *        CONSTRUCT ABM POT PHRASE                          *      ELGCSGCI
00886 *                                                          *      ELGCSGCI
00887 ************************************************************      ELGCSGCI
00888  CONSTRUCT-ABM-POT-PHRASE.                                        ELGCSGCI
00889      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
00890                 WS-SUBA.                                          ELGCSGCI
00891      MOVE PC-ABM                        TO                        ELGCSGCI
00892          CMF-RECORD-PREFIX.                                       ELGCSGCI
00893      MOVE 'BAMA-PLACE-OF-TREATMENT'     TO                        ELGCSGCI
00894          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
00895      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)     TO             ELGCSGCI
00896          CMF-CODE-VALUE.                                          ELGCSGCI
00897      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
00898      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
00899          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
00900                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
00901      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
00902      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
00903      MOVE +3          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
00904      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
00905                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
00906                          TCAR-OUTPUT-FIELD-3-LEN.                 ELGCSGCI
00907      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
00908      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
00909      MOVE POT-LIT                TO LEFT-SIDE.                    ELGCSGCI
00910      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
00911      MOVE DETAIL-LINE            TO COF-DTL-LINE                  ELGCSGCI
00912          (COF-NBR-DTL-LINES).                                     ELGCSGCI
00913      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
00914          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
00915                                                                   ELGCSGCI
00916 ************************************************************      ELGCSGCI
00917 *                                                          *      ELGCSGCI
00918 *        CONSTRUCT VERBIAGE FOR ACL                        *      ELGCSGCI
00919 *                                                          *      ELGCSGCI
00920 ************************************************************      ELGCSGCI
00921  CONSTRUCT-VERBIAGE-FOR-ACL.                                      ELGCSGCI
00922      INITIALIZE COF-DTL                                           ELGCSGCI
00923                 TCAR-FROM-AREA                                    ELGCSGCI
00924                 WS-SUBA.                                          ELGCSGCI
00925      IF PRICING-HEADER-NOT-DISPLAYED                              ELGCSGCI
00926          PERFORM DISPLAY-PRICING-HEADER.                          ELGCSGCI
00927      PERFORM CONSTRUCT-ACL-PRICING.                               ELGCSGCI
00928      PERFORM CONSTRUCT-ACL-POT-PHRASE.                            ELGCSGCI
00929      PERFORM CONSTRUCT-ACL-CONDITION-BITS-P.                      ELGCSGCI
00930      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
00931                                                                   ELGCSGCI
00932 ************************************************************      ELGCSGCI
00933 *                                                          *      ELGCSGCI
00934 *        DISPLAY PRICING HEADER                            *      ELGCSGCI
00935 *                                                          *      ELGCSGCI
00936 ************************************************************      ELGCSGCI
00937  DISPLAY-PRICING-HEADER.                                          ELGCSGCI
00938      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
00939      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
00940      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
00941      MOVE OVERALL-PRICING-LIT     TO COF-DTL-LINE                 ELGCSGCI
00942          (COF-NBR-DTL-LINES).                                     ELGCSGCI
00943      SET PRICING-HEADER-DISPLAYED TO TRUE.                        ELGCSGCI
00944                                                                   ELGCSGCI
00945 ***********************************************************       ELGCSGCI
00946 *                                                          *      ELGCSGCI
00947 *        DISPLAY HOSPITAL LIT                              *      ELGCSGCI
00948 *                                                          *      ELGCSGCI
00949 ************************************************************      ELGCSGCI
00950  DISPLAY-HOSPITAL-LIT.                                            ELGCSGCI
00951      MOVE HOSP-LIT TO LEFT-SIDE.                                  ELGCSGCI
00952                                                                   ELGCSGCI
00953 ***********************************************************       ELGCSGCI
00954 *                                                          *      ELGCSGCI
00955 *        DISPLAY PROFESSIONAL LIT                          *      ELGCSGCI
00956 *                                                          *      ELGCSGCI
00957 ************************************************************      ELGCSGCI
00958  DISPLAY-PROFESSIONAL-LIT.                                        ELGCSGCI
00959      MOVE PHYS-LIT TO LEFT-SIDE.                                  ELGCSGCI
00960                                                                   ELGCSGCI
00961 ************************************************************      ELGCSGCI
00962 *                                                          *      ELGCSGCI
00963 *        DISPLAY SUPPLEMENTAL LIT                          *      ELGCSGCI
00964 *                                                          *      ELGCSGCI
00965 ************************************************************      ELGCSGCI
00966  DISPLAY-SUPPLEMENTAL-LIT.                                        ELGCSGCI
00967      MOVE SUPPLEMENTAL-LIT TO LEFT-SIDE.                          ELGCSGCI
00968                                                                   ELGCSGCI
00969 ************************************************************      ELGCSGCI
00970 *                                                          *      ELGCSGCI
00971 *        DISPLAY COMPREHENSIVE LIT                         *      ELGCSGCI
00972 *                                                          *      ELGCSGCI
00973 ************************************************************      ELGCSGCI
00974  DISPLAY-COMPREHENSIVE-LIT.                                       ELGCSGCI
00975      MOVE COMP-LIT TO LEFT-SIDE.                                  ELGCSGCI
00976                                                                   ELGCSGCI
00977 ************************************************************      ELGCSGCI
00978 *                                                          *      ELGCSGCI
00979 *        MOVE ACTUAL ACL VALUES                            *      ELGCSGCI
00980 *                                                          *      ELGCSGCI
00981 ************************************************************      ELGCSGCI
00982  MOVE-ACTUAL-ACL-VALUES.                                          ELGCSGCI
00983      MOVE ATBL-PERCENT-LEVEL (ATBL-X-IDX) TO FIRST-PERC-EDIT.     ELGCSGCI
00984      ADD 1 TO WS-SUBA.                                            ELGCSGCI
00985      MOVE FIRST-PERCENTAGE TO TCAR-FROM-LINE (WS-SUBA).           ELGCSGCI
00986      IF PROCESSING-PROFESSIONAL                                   ELGCSGCI
00987          PERFORM DISPLAY-BENEFIT-SCOPE.                           ELGCSGCI
00988                                                                   ELGCSGCI
00989 ************************************************************      ELGCSGCI
00990 *                                                          *      ELGCSGCI
00991 *        DISPLAY BS COINSURANCE                            *      ELGCSGCI
00992 *                                                          *      ELGCSGCI
00993 ************************************************************      ELGCSGCI
00994  DISPLAY-BS-COINSURANCE.                                          ELGCSGCI
00995      INITIALIZE COF-DTL                                           ELGCSGCI
00996                 TCAR-FROM-AREA                                    ELGCSGCI
00997                 WS-SUBA.                                          ELGCSGCI
00998      IF PRICING-HEADER-NOT-DISPLAYED                              ELGCSGCI
00999          PERFORM DISPLAY-PRICING-HEADER.                          ELGCSGCI
01000      PERFORM DISPLAY-PROFESSIONAL-LIT.                            ELGCSGCI
01001      IF WS-BENEFIT-SCOPE = ZERO OR SPACE                          ELGCSGCI
01002          PERFORM DISPLAY-BS-U-AND-C                               ELGCSGCI
01003      ELSE                                                         ELGCSGCI
01004          PERFORM DISPLAY-BS-BENEFIT-SCOPE-TRANS.                  ELGCSGCI
01005      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01006                                                                   ELGCSGCI
01007                                                                   ELGCSGCI
01008 ************************************************************      ELGCSGCI
01009 *                                                          *      ELGCSGCI
01010 *        DISPLAY BENEFIT SCOPE                             *      ELGCSGCI
01011 *                                                          *      ELGCSGCI
01012 ************************************************************      ELGCSGCI
01013  DISPLAY-BENEFIT-SCOPE.                                           ELGCSGCI
01014      SET BS-ACL-MSG-DISPLAYED TO TRUE.                            ELGCSGCI
01015      IF WS-BENEFIT-SCOPE = ZERO OR SPACE                          ELGCSGCI
01016          PERFORM DISPLAY-U-AND-C                                  ELGCSGCI
01017      ELSE                                                         ELGCSGCI
01018          PERFORM DISPLAY-BENEFIT-SCOPE-TRANSLAT.                  ELGCSGCI
01019                                                                   ELGCSGCI
01020                                                                   ELGCSGCI
01021 ************************************************************      ELGCSGCI
01022 *                                                          *      ELGCSGCI
01023 *        DISPLAY BS U AND C                                *      ELGCSGCI
01024 *                                                          *      ELGCSGCI
01025 ************************************************************      ELGCSGCI
01026  DISPLAY-BS-U-AND-C.                                              ELGCSGCI
01027      MOVE U-AND-C-LIT TO RIGHT-SIDE.                              ELGCSGCI
01028      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01029      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01030          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01031                                                                   ELGCSGCI
01032                                                                   ELGCSGCI
01033 ************************************************************      ELGCSGCI
01034 *                                                          *      ELGCSGCI
01035 *        DISPLAY U AND C                                   *      ELGCSGCI
01036 *                                                          *      ELGCSGCI
01037 ************************************************************      ELGCSGCI
01038  DISPLAY-U-AND-C.                                                 ELGCSGCI
01039      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01040      MOVE U-AND-C-LIT TO TCAR-FROM-LINE (WS-SUBA).                ELGCSGCI
01041                                                                   ELGCSGCI
01042 ************************************************************      ELGCSGCI
01043 *                                                          *      ELGCSGCI
01044 *        DISPLAY BENEFIT SCOPE TRANSLATION                 *      ELGCSGCI
01045 *                                                          *      ELGCSGCI
01046 ************************************************************      ELGCSGCI
01047  DISPLAY-BENEFIT-SCOPE-TRANSLAT.                                  ELGCSGCI
01048      MOVE 'BPC'                         TO CMF-RECORD-PREFIX.     ELGCSGCI
01049      MOVE 'BEN-SCOPE-ID'                TO                        ELGCSGCI
01050          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01051      MOVE WS-BENEFIT-SCOPE              TO CMF-CODE-VALUE.        ELGCSGCI
01052      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01053      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01054          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01055                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01056                                                                   ELGCSGCI
01057 ************************************************************      ELGCSGCI
01058 *                                                          *      ELGCSGCI
01059 *        DISPLAY BS BENEFIT SCOPE TRANSLATION              *      ELGCSGCI
01060 *                                                          *      ELGCSGCI
01061 ************************************************************      ELGCSGCI
01062  DISPLAY-BS-BENEFIT-SCOPE-TRANS.                                  ELGCSGCI
01063      MOVE 'BPC'                         TO                        ELGCSGCI
01064          CMF-RECORD-PREFIX.                                       ELGCSGCI
01065      MOVE 'BEN-SCOPE-ID'                TO                        ELGCSGCI
01066          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01067      MOVE WS-BENEFIT-SCOPE              TO CMF-CODE-VALUE.        ELGCSGCI
01068      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01069      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01070          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01071                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01072      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01073      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01074      MOVE +02         TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01075      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01076                          TCAR-OUTPUT-FIELD-2-LEN.                 ELGCSGCI
01077      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01078      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01079      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01080      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01081          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01082      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01083          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01084                                                                   ELGCSGCI
01085 ************************************************************      ELGCSGCI
01086 *                                                          *      ELGCSGCI
01087 *        CONSTRUCT ACL PRICING                             *      ELGCSGCI
01088 *                                                          *      ELGCSGCI
01089 ************************************************************      ELGCSGCI
01090  CONSTRUCT-ACL-PRICING.                                           ELGCSGCI
01091      IF PROCESSING-INSTITUTIONAL                                  ELGCSGCI
01092          PERFORM DISPLAY-HOSPITAL-LIT                             ELGCSGCI
01093      ELSE IF PROCESSING-PROFESSIONAL                              ELGCSGCI
01094          PERFORM DISPLAY-PROFESSIONAL-LIT                         ELGCSGCI
01095      ELSE IF PROCESSING-MAJOR-MEDICAL                             ELGCSGCI
01096          PERFORM DISPLAY-SUPPLEMENTAL-LIT                         ELGCSGCI
01097      ELSE IF PROCESSING-COMPREHENSIVE                             ELGCSGCI
01098          PERFORM DISPLAY-COMPREHENSIVE-LIT.                       ELGCSGCI
01099      PERFORM MOVE-ACTUAL-ACL-VALUES.                              ELGCSGCI
01100      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01101      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01102      MOVE +10         TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01103      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01104                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01105                          TCAR-OUTPUT-FIELD-3-LEN                  ELGCSGCI
01106                          TCAR-OUTPUT-FIELD-4-LEN                  ELGCSGCI
01107                          TCAR-OUTPUT-FIELD-5-LEN                  ELGCSGCI
01108                          TCAR-OUTPUT-FIELD-6-LEN                  ELGCSGCI
01109                          TCAR-OUTPUT-FIELD-7-LEN                  ELGCSGCI
01110                          TCAR-OUTPUT-FIELD-8-LEN                  ELGCSGCI
01111                          TCAR-OUTPUT-FIELD-9-LEN                  ELGCSGCI
01112                          TCAR-OUTPUT-FIELD-10-LEN.                ELGCSGCI
01113      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01114      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01115      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01116      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01117          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01118      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01119          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01120                                                                   ELGCSGCI
01121 ************************************************************      ELGCSGCI
01122 *                                                          *      ELGCSGCI
01123 *        CONSTRUCT ACL POT PHRASE                          *      ELGCSGCI
01124 *                                                          *      ELGCSGCI
01125 ************************************************************      ELGCSGCI
01126  CONSTRUCT-ACL-POT-PHRASE.                                        ELGCSGCI
01127      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
01128                 WS-SUBA.                                          ELGCSGCI
01129      MOVE PC-ACL                        TO                        ELGCSGCI
01130          CMF-RECORD-PREFIX.                                       ELGCSGCI
01131      MOVE 'COINS-PLACE-OF-TREATMENT'     TO                       ELGCSGCI
01132          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01133      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)     TO             ELGCSGCI
01134          CMF-CODE-VALUE.                                          ELGCSGCI
01135      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01136      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01137          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01138                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01139      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01140      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01141      MOVE +3          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01142      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01143                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01144                          TCAR-OUTPUT-FIELD-3-LEN.                 ELGCSGCI
01145      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01146      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01147      MOVE POT-LIT                TO LEFT-SIDE.                    ELGCSGCI
01148      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
01149      MOVE DETAIL-LINE            TO COF-DTL-LINE                  ELGCSGCI
01150          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01151      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01152          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01153                                                                   ELGCSGCI
01154 ************************************************************      ELGCSGCI
01155 *                                                          *      ELGCSGCI
01156 *        CONSTRUCT ACL CONDITION BITS PHRASE               *      ELGCSGCI
01157 *                                                          *      ELGCSGCI
01158 ************************************************************      ELGCSGCI
01159  CONSTRUCT-ACL-CONDITION-BITS-P.                                  ELGCSGCI
01160      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
01161                 WS-SUBA.                                          ELGCSGCI
01162      MOVE ATBL-CONDITION (ATBL-X-IDX) TO                          ELGCSGCI
01163          CMF-CONDITION-BITS.                                      ELGCSGCI
01164      EXEC CICS LINK PROGRAM ('ELUCONDB')                          ELGCSGCI
01165                     COMMAREA (DFHCOMMAREA)                        ELGCSGCI
01166          END-EXEC.                                                ELGCSGCI
01167      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCSGCI
01168      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
01169                            ADDRESS OF CMF-DESCR.                  ELGCSGCI
01170      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01171          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01172                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01173      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01174      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01175      MOVE +8          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01176      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01177                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01178                          TCAR-OUTPUT-FIELD-3-LEN                  ELGCSGCI
01179                          TCAR-OUTPUT-FIELD-4-LEN                  ELGCSGCI
01180                          TCAR-OUTPUT-FIELD-5-LEN                  ELGCSGCI
01181                          TCAR-OUTPUT-FIELD-6-LEN                  ELGCSGCI
01182                          TCAR-OUTPUT-FIELD-7-LEN                  ELGCSGCI
01183                          TCAR-OUTPUT-FIELD-8-LEN.                 ELGCSGCI
01184      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01185      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01186      MOVE CONDITION-LIT          TO LEFT-SIDE.                    ELGCSGCI
01187      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
01188      MOVE DETAIL-LINE            TO COF-DTL-LINE                  ELGCSGCI
01189          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01190      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01191          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01192                                                                   ELGCSGCI
01193 ************************************************************      ELGCSGCI
01194 *                                                          *      ELGCSGCI
01195 *        CONSTRUCT VERBIAGE FOR ADL                        *      ELGCSGCI
01196 *                                                          *      ELGCSGCI
01197 ************************************************************      ELGCSGCI
01198  CONSTRUCT-VERBIAGE-FOR-ADL.                                      ELGCSGCI
01199      INITIALIZE COF-DTL.                                          ELGCSGCI
01200      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01201      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01202      PERFORM CONSTRUCT-OVERALL-ADL-PHRASE.                        ELGCSGCI
01203      PERFORM CONSTRUCT-ADL-BENEFIT-PERIOD-P.                      ELGCSGCI
01204      PERFORM CONSTRUCT-ADL-LOB-PHRASE.                            ELGCSGCI
01205      PERFORM CONSTRUCT-ADL-POT-PHRASE.                            ELGCSGCI
01206      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01207                                                                   ELGCSGCI
01208 ************************************************************      ELGCSGCI
01209 *                                                          *      ELGCSGCI
01210 *        CONSTRUCT OVERALL ADL PHRASE                      *      ELGCSGCI
01211 *                                                          *      ELGCSGCI
01212 ************************************************************      ELGCSGCI
01213  CONSTRUCT-OVERALL-ADL-PHRASE.                                    ELGCSGCI
01214      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01215      MOVE OVERALL-DEDUCT-LIT      TO LEFT-SIDE.                   ELGCSGCI
01216      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
01217                 WS-SUBA.                                          ELGCSGCI
01218      IF ATBL-VALUE-LIMIT (ATBL-X-IDX) NEGATIVE                    ELGCSGCI
01219          PERFORM CHECK-DEDUCTIBLE-SOURCE-INDICA                   ELGCSGCI
01220      ELSE                                                         ELGCSGCI
01221          PERFORM DISPLAY-DEDUCTIBLE-VALUE-LIMIT.                  ELGCSGCI
01222                                                                   ELGCSGCI
01223                                                                   ELGCSGCI
01224 ************************************************************      ELGCSGCI
01225 *                                                          *      ELGCSGCI
01226 *        CHECK DEDUCTIBLE SOURCE INDICATOR                 *      ELGCSGCI
01227 *                                                          *      ELGCSGCI
01228 ************************************************************      ELGCSGCI
01229  CHECK-DEDUCTIBLE-SOURCE-INDICA.                                  ELGCSGCI
01230      IF GCG-DED-BASE-AMT-SOURCE-IND NOT = ZERO                    ELGCSGCI
01231          PERFORM DISPLAY-DEDUCTIBLE-SOURCE-INDI                   ELGCSGCI
01232      ELSE                                                         ELGCSGCI
01233          PERFORM JUST-DISPLAY-ADL-PHRASE.                         ELGCSGCI
01234                                                                   ELGCSGCI
01235                                                                   ELGCSGCI
01236 ************************************************************      ELGCSGCI
01237 *                                                          *      ELGCSGCI
01238 *        JUST DISPLAY ADL PHRASE                           *      ELGCSGCI
01239 *                                                          *      ELGCSGCI
01240 ************************************************************      ELGCSGCI
01241  JUST-DISPLAY-ADL-PHRASE.                                         ELGCSGCI
01242      MOVE SPACES TO RIGHT-SIDE.                                   ELGCSGCI
01243      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01244          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01245                                                                   ELGCSGCI
01246 ************************************************************      ELGCSGCI
01247 *                                                          *      ELGCSGCI
01248 *        DISPLAY DEDUCTIBLE SOURCE INDICATOR               *      ELGCSGCI
01249 *                                                          *      ELGCSGCI
01250 ************************************************************      ELGCSGCI
01251  DISPLAY-DEDUCTIBLE-SOURCE-INDI.                                  ELGCSGCI
01252      MOVE 'GROUP'                       TO CMF-RECORD-PREFIX.     ELGCSGCI
01253      MOVE 'DED-BASE-AMT-SOURCE-IND'     TO                        ELGCSGCI
01254          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01255      MOVE GCG-DED-BASE-AMT-SOURCE-IND   TO CMF-CODE-VALUE.        ELGCSGCI
01256      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01257      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01258      MOVE DETERMINE-LIT TO TCAR-FROM-LINE (WS-SUBA).              ELGCSGCI
01259      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01260          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01261                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01262      IF ATBL-FAM-OR-INDIV (ATBL-X-IDX) = PC-INDIVIDUAL            ELGCSGCI
01263          PERFORM DISPLAY-INDIVIDUAL                               ELGCSGCI
01264      ELSE                                                         ELGCSGCI
01265          PERFORM DISPLAY-FAMILY.                                  ELGCSGCI
01266      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01267      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01268      MOVE +3          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01269      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01270                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01271                          TCAR-OUTPUT-FIELD-3-LEN.                 ELGCSGCI
01272      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01273      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01274      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01275          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01276      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01277          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01278                                                                   ELGCSGCI
01279 ************************************************************      ELGCSGCI
01280 *                                                          *      ELGCSGCI
01281 *        DISPLAY DEDUCTIBLE VALUE LIMIT                    *      ELGCSGCI
01282 *                                                          *      ELGCSGCI
01283 ************************************************************      ELGCSGCI
01284  DISPLAY-DEDUCTIBLE-VALUE-LIMIT.                                  ELGCSGCI
01285      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = PC-FIVE               ELGCSGCI
01286          PERFORM DISPLAY-ADL-DOLLAR-EDIT                          ELGCSGCI
01287      ELSE IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) =                  ELGCSGCI
01288          PC-SEVEN                                                 ELGCSGCI
01289          PERFORM DISPLAY-ADL-PEOPLE-PHRASE                        ELGCSGCI
01290      ELSE                                                         ELGCSGCI
01291          PERFORM DISPLAY-ADL-NUMERIC-EDIT.                        ELGCSGCI
01292      IF ATBL-FAM-OR-INDIV (ATBL-X-IDX) = PC-INDIVIDUAL            ELGCSGCI
01293          PERFORM DISPLAY-INDIVIDUAL                               ELGCSGCI
01294      ELSE                                                         ELGCSGCI
01295          PERFORM DISPLAY-FAMILY.                                  ELGCSGCI
01296      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01297      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01298      MOVE +1          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01299      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN.                 ELGCSGCI
01300      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01301      MOVE TCAR-OPF-DATA (1)      TO RIGHT-SIDE.                   ELGCSGCI
01302      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01303          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01304                                                                   ELGCSGCI
01305 ************************************************************      ELGCSGCI
01306 *                                                          *      ELGCSGCI
01307 *        DISPLAY ADL DOLLAR EDIT                           *      ELGCSGCI
01308 *                                                          *      ELGCSGCI
01309 ************************************************************      ELGCSGCI
01310  DISPLAY-ADL-DOLLAR-EDIT.                                         ELGCSGCI
01311      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-DOLLAR-EDIT.  ELGCSGCI
01312      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01313      MOVE WS-DOLLAR-EDIT TO TCAR-FROM-LINE (WS-SUBA).             ELGCSGCI
01314                                                                   ELGCSGCI
01315 ************************************************************      ELGCSGCI
01316 *                                                          *      ELGCSGCI
01317 *        DISPLAY ADL NUMERIC EDIT                          *      ELGCSGCI
01318 *                                                          *      ELGCSGCI
01319 ************************************************************      ELGCSGCI
01320  DISPLAY-ADL-NUMERIC-EDIT.                                        ELGCSGCI
01321      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-NINE-EDIT.    ELGCSGCI
01322      MOVE PC-ADL                        TO CMF-RECORD-PREFIX.     ELGCSGCI
01323      MOVE 'DEDL-VALUE-QUALIFIER'        TO                        ELGCSGCI
01324          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01325      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)      TO               ELGCSGCI
01326          CMF-CODE-VALUE.                                          ELGCSGCI
01327      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01328      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01329      INSPECT WS-WHOLE-NUM REPLACING LEADING ZEROES BY SPACES.     ELGCSGCI
01330      STRING WS-WHOLE-NUM  ' ' CMF-DESCR-LINE (1)                  ELGCSGCI
01331             DELIMITED BY SIZE INTO TCAR-FROM-LINE                 ELGCSGCI
01332          (WS-SUBA).                                               ELGCSGCI
01333                                                                   ELGCSGCI
01334 ************************************************************      ELGCSGCI
01335 *                                                          *      ELGCSGCI
01336 *        DISPLAY ADL PEOPLE PHRASE                         *      ELGCSGCI
01337 *                                                          *      ELGCSGCI
01338 ************************************************************      ELGCSGCI
01339  DISPLAY-ADL-PEOPLE-PHRASE.                                       ELGCSGCI
01340      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-NINE-EDIT.    ELGCSGCI
01341      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01342      INSPECT WS-WHOLE-NUM REPLACING LEADING ZEROES BY SPACES.     ELGCSGCI
01343      STRING WS-WHOLE-NUM  ' ' PEOPLE-LIT DELIMITED BY SIZE        ELGCSGCI
01344             INTO TCAR-FROM-LINE (WS-SUBA).                        ELGCSGCI
01345                                                                   ELGCSGCI
01346 ************************************************************      ELGCSGCI
01347 *                                                          *      ELGCSGCI
01348 *        CONSTRUCT ADL BENEFIT PERIOD PHRASE               *      ELGCSGCI
01349 *                                                          *      ELGCSGCI
01350 ************************************************************      ELGCSGCI
01351  CONSTRUCT-ADL-BENEFIT-PERIOD-P.                                  ELGCSGCI
01352      MOVE PC-ADL                        TO                        ELGCSGCI
01353          CMF-RECORD-PREFIX.                                       ELGCSGCI
01354      MOVE 'DEDL-BENEFIT-PERIOD'         TO                        ELGCSGCI
01355          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01356      MOVE ATBL-BENEFIT-PERIOD (ATBL-X-IDX)       TO               ELGCSGCI
01357          CMF-CODE-VALUE.                                          ELGCSGCI
01358      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01359      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01360      MOVE BENEFIT-PERIOD-LIT     TO LEFT-SIDE.                    ELGCSGCI
01361      MOVE CMF-DESCR-LINE (1)     TO RIGHT-SIDE.                   ELGCSGCI
01362      MOVE DETAIL-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).        ELGCSGCI
01363                                                                   ELGCSGCI
01364 ************************************************************      ELGCSGCI
01365 *                                                          *      ELGCSGCI
01366 *        CONSTRUCT ADL LOB PHRASE                          *      ELGCSGCI
01367 *                                                          *      ELGCSGCI
01368 ************************************************************      ELGCSGCI
01369  CONSTRUCT-ADL-LOB-PHRASE.                                        ELGCSGCI
01370      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
01371                 WS-SUBA.                                          ELGCSGCI
01372      MOVE PC-ADL                        TO                        ELGCSGCI
01373          CMF-RECORD-PREFIX.                                       ELGCSGCI
01374      MOVE 'DEDL-L-O-B'                  TO                        ELGCSGCI
01375          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01376      MOVE ATBL-L-O-B (ATBL-X-IDX)       TO CMF-CODE-VALUE.        ELGCSGCI
01377      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01378      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01379          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01380                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01381      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01382      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01383      MOVE +2          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01384      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01385                          TCAR-OUTPUT-FIELD-2-LEN.                 ELGCSGCI
01386      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01387      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01388      MOVE LOB-LIT                 TO LEFT-SIDE.                   ELGCSGCI
01389      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01390      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01391          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01392      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01393          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01394                                                                   ELGCSGCI
01395 ************************************************************      ELGCSGCI
01396 *                                                          *      ELGCSGCI
01397 *        CONSTRUCT ADL POT PHRASE                          *      ELGCSGCI
01398 *                                                          *      ELGCSGCI
01399 ************************************************************      ELGCSGCI
01400  CONSTRUCT-ADL-POT-PHRASE.                                        ELGCSGCI
01401      INITIALIZE TCAR-FROM-AREA                                    ELGCSGCI
01402                 WS-SUBA.                                          ELGCSGCI
01403      MOVE PC-ADL                        TO                        ELGCSGCI
01404          CMF-RECORD-PREFIX.                                       ELGCSGCI
01405      MOVE 'DEDL-PLACE-OF-TREATMENT'     TO                        ELGCSGCI
01406          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01407      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX) TO CMF-CODE-VALUE. ELGCSGCI
01408      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01409      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01410          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01411                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01412      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01413      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01414      MOVE +3          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01415      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01416                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01417                          TCAR-OUTPUT-FIELD-3-LEN.                 ELGCSGCI
01418      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01419      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01420      MOVE POT-LIT                 TO LEFT-SIDE.                   ELGCSGCI
01421      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01422      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01423          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01424      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01425          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01426                                                                   ELGCSGCI
01427                                                                   ELGCSGCI
01428 ************************************************************      ELGCSGCI
01429 *                                                          *      ELGCSGCI
01430 *        CONSTRUCT VERBIAGE FOR AOL                        *      ELGCSGCI
01431 *                                                          *      ELGCSGCI
01432 ************************************************************      ELGCSGCI
01433  CONSTRUCT-VERBIAGE-FOR-AOL.                                      ELGCSGCI
01434      IF ATBL-FAM-OR-INDIV (ATBL-X-IDX) = 'I'                      ELGCSGCI
01435          PERFORM CHECK-VALUE-QUALIFIER-FOR-AOL.                   ELGCSGCI
01436                                                                   ELGCSGCI
01437 ************************************************************      ELGCSGCI
01438 *                                                          *      ELGCSGCI
01439 *        CHECK VALUE QUALIFIER FOR AOL                     *      ELGCSGCI
01440 *                                                          *      ELGCSGCI
01441 ************************************************************      ELGCSGCI
01442  CHECK-VALUE-QUALIFIER-FOR-AOL.                                   ELGCSGCI
01443      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = PC-FIVE               ELGCSGCI
01444          PERFORM CHECK-VALUE-LIMIT-FOR-AOL                        ELGCSGCI
01445      ELSE                                                         ELGCSGCI
01446          PERFORM DISPLAY-VERBIAGE-FOR-AOL.                        ELGCSGCI
01447                                                                   ELGCSGCI
01448 ************************************************************      ELGCSGCI
01449 *                                                          *      ELGCSGCI
01450 *        CHECK VALUE LIMIT FOR AOL                         *      ELGCSGCI
01451 *                                                          *      ELGCSGCI
01452 ************************************************************      ELGCSGCI
01453  CHECK-VALUE-LIMIT-FOR-AOL.                                       ELGCSGCI
01454      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO                        ELGCSGCI
01455          WS-NINE-EDIT.                                            ELGCSGCI
01456      IF ALL-NINES                                                 ELGCSGCI
01457          CONTINUE                                                 ELGCSGCI
01458      ELSE                                                         ELGCSGCI
01459          PERFORM DISPLAY-VERBIAGE-FOR-AOL.                        ELGCSGCI
01460                                                                   ELGCSGCI
01461 ************************************************************      ELGCSGCI
01462 *                                                          *      ELGCSGCI
01463 *        DISPLAY VERBIAGE FOR AOL                          *      ELGCSGCI
01464 *                                                          *      ELGCSGCI
01465 ************************************************************      ELGCSGCI
01466  DISPLAY-VERBIAGE-FOR-AOL.                                        ELGCSGCI
01467      INITIALIZE COF-DTL                                           ELGCSGCI
01468                 TCAR-FROM-AREA                                    ELGCSGCI
01469                 WS-SUBA.                                          ELGCSGCI
01470      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01471      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01472      IF ATBL-DEFINITION (ATBL-X-IDX) = '1' OR '2' OR              ELGCSGCI
01473                                                      '5' OR       ELGCSGCI
01474          '6'                                                      ELGCSGCI
01475          PERFORM DISPLAY-STOP-LOSS-LIT                            ELGCSGCI
01476      ELSE                                                         ELGCSGCI
01477          PERFORM DISPLAY-OPX-LIT.                                 ELGCSGCI
01478      PERFORM MOVE-ACTUAL-AOL-VALUES.                              ELGCSGCI
01479      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01480      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01481      MOVE +10         TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01482      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01483                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01484                          TCAR-OUTPUT-FIELD-3-LEN                  ELGCSGCI
01485                          TCAR-OUTPUT-FIELD-4-LEN                  ELGCSGCI
01486                          TCAR-OUTPUT-FIELD-5-LEN                  ELGCSGCI
01487                          TCAR-OUTPUT-FIELD-6-LEN                  ELGCSGCI
01488                          TCAR-OUTPUT-FIELD-7-LEN                  ELGCSGCI
01489                          TCAR-OUTPUT-FIELD-8-LEN                  ELGCSGCI
01490                          TCAR-OUTPUT-FIELD-9-LEN                  ELGCSGCI
01491                          TCAR-OUTPUT-FIELD-10-LEN.                ELGCSGCI
01492      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01493      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01494      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01495      MOVE DETAIL-LINE             TO COF-DTL-LINE                 ELGCSGCI
01496          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01497      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01498          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01499      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01500                                                                   ELGCSGCI
01501 ************************************************************      ELGCSGCI
01502 *                                                          *      ELGCSGCI
01503 *        DISPLAY STOP-LOSS LIT                             *      ELGCSGCI
01504 *                                                          *      ELGCSGCI
01505 ************************************************************      ELGCSGCI
01506  DISPLAY-STOP-LOSS-LIT.                                           ELGCSGCI
01507      MOVE OVERALL-STOP-LOSS TO LEFT-SIDE.                         ELGCSGCI
01508                                                                   ELGCSGCI
01509 ************************************************************      ELGCSGCI
01510 *                                                          *      ELGCSGCI
01511 *        DISPLAY OPX LIT                                   *      ELGCSGCI
01512 *                                                          *      ELGCSGCI
01513 ************************************************************      ELGCSGCI
01514  DISPLAY-OPX-LIT.                                                 ELGCSGCI
01515      MOVE OVERALL-OPX-LIT   TO LEFT-SIDE.                         ELGCSGCI
01516                                                                   ELGCSGCI
01517 ************************************************************      ELGCSGCI
01518 *                                                          *      ELGCSGCI
01519 *        MOVE ACTUAL AOL VALUES                            *      ELGCSGCI
01520 *                                                          *      ELGCSGCI
01521 ************************************************************      ELGCSGCI
01522  MOVE-ACTUAL-AOL-VALUES.                                          ELGCSGCI
01523      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = PC-FIVE               ELGCSGCI
01524          PERFORM DISPLAY-AOL-DOLLAR-EDIT                          ELGCSGCI
01525      ELSE IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) =                  ELGCSGCI
01526          PC-SEVEN                                                 ELGCSGCI
01527          PERFORM DISPLAY-AOL-PEOPLE-PHRASE                        ELGCSGCI
01528      ELSE                                                         ELGCSGCI
01529          PERFORM DISPLAY-AOL-NUMERIC-EDIT.                        ELGCSGCI
01530                                                                   ELGCSGCI
01531                                                                   ELGCSGCI
01532 ************************************************************      ELGCSGCI
01533 *                                                          *      ELGCSGCI
01534 *        DISPLAY AOL DOLLAR EDIT                           *      ELGCSGCI
01535 *                                                          *      ELGCSGCI
01536 ************************************************************      ELGCSGCI
01537  DISPLAY-AOL-DOLLAR-EDIT.                                         ELGCSGCI
01538      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO                  ELGCSGCI
01539          WS-DOLLAR-EDIT.                                          ELGCSGCI
01540      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01541      MOVE WS-DOLLAR-EDIT TO TCAR-FROM-LINE (WS-SUBA).             ELGCSGCI
01542                                                                   ELGCSGCI
01543 ************************************************************      ELGCSGCI
01544 *                                                          *      ELGCSGCI
01545 *        DISPLAY AOL NUMERIC EDIT                          *      ELGCSGCI
01546 *                                                          *      ELGCSGCI
01547 ************************************************************      ELGCSGCI
01548  DISPLAY-AOL-NUMERIC-EDIT.                                        ELGCSGCI
01549      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-NINE-EDIT.    ELGCSGCI
01550      MOVE PC-AOL                        TO CMF-RECORD-PREFIX.     ELGCSGCI
01551      MOVE 'O-P-X-VALUE-QUALIFIER'        TO                       ELGCSGCI
01552          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01553      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)      TO               ELGCSGCI
01554          CMF-CODE-VALUE.                                          ELGCSGCI
01555      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01556      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01557      INSPECT WS-WHOLE-NUM REPLACING LEADING ZEROES BY SPACES.     ELGCSGCI
01558      STRING WS-WHOLE-NUM  ' ' CMF-DESCR-LINE (1)                  ELGCSGCI
01559             DELIMITED BY SIZE INTO TCAR-FROM-LINE                 ELGCSGCI
01560          (WS-SUBA).                                               ELGCSGCI
01561                                                                   ELGCSGCI
01562                                                                   ELGCSGCI
01563 ************************************************************      ELGCSGCI
01564 *                                                          *      ELGCSGCI
01565 *        DISPLAY AOL PEOPLE PHRASE                         *      ELGCSGCI
01566 *                                                          *      ELGCSGCI
01567 ************************************************************      ELGCSGCI
01568  DISPLAY-AOL-PEOPLE-PHRASE.                                       ELGCSGCI
01569      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO WS-NINE-EDIT.    ELGCSGCI
01570      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01571      INSPECT WS-WHOLE-NUM REPLACING LEADING ZEROES BY SPACES.     ELGCSGCI
01572      STRING WS-WHOLE-NUM  ' ' PEOPLE-LIT DELIMITED BY SIZE        ELGCSGCI
01573             INTO TCAR-FROM-LINE (WS-SUBA).                        ELGCSGCI
01574                                                                   ELGCSGCI
01575                                                                   ELGCSGCI
01576 ************************************************************      ELGCSGCI
01577 *                                                          *      ELGCSGCI
01578 *        LINK TO ELUOUTPT                                  *      ELGCSGCI
01579 *                                                          *      ELGCSGCI
01580 ************************************************************      ELGCSGCI
01581  LINK-TO-ELUOUTPT.                                                ELGCSGCI
01582      EXEC CICS LINK                                               ELGCSGCI
01583                PROGRAM ('ELUOUTPT')                               ELGCSGCI
01584                COMMAREA (DFHCOMMAREA)                             ELGCSGCI
01585           END-EXEC.                                               ELGCSGCI
01586                                                                   ELGCSGCI
01587 ************************************************************      ELGCSGCI
01588 *                                                          *      ELGCSGCI
01589 *        LINK TO ELUCMIF                                   *      ELGCSGCI
01590 *                                                          *      ELGCSGCI
01591 ************************************************************      ELGCSGCI
01592  LINK-TO-ELUCMIF.                                                 ELGCSGCI
01593      EXEC CICS LINK                                               ELGCSGCI
01594                PROGRAM ('ELUCMIF')                                ELGCSGCI
01595                COMMAREA (DFHCOMMAREA)                             ELGCSGCI
01596           END-EXEC.                                               ELGCSGCI
01597      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCSGCI
01598      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSGCI
01599                      ADDRESS OF CMF-DESCR.                        ELGCSGCI
01600                                                                   ELGCSGCI
01601 ************************************************************      ELGCSGCI
01602 *                                                          *      ELGCSGCI
01603 *        CONSTRUCT AGE LIMIT VERBIAGE                      *      ELGCSGCI
01604 *                                                          *      ELGCSGCI
01605 ************************************************************      ELGCSGCI
01606  CONSTRUCT-AGE-LIMIT-VERBIAGE.                                    ELGCSGCI
01607      INITIALIZE COF-DTL                                           ELGCSGCI
01608                 DETAIL-LINE.                                      ELGCSGCI
01609      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01610      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01611      MOVE AGE-LIMIT-LIT TO LEFT-SIDE.                             ELGCSGCI
01612      IF GCG-DEP-MAX-AGE = 999                                     ELGCSGCI
01613          PERFORM DISPLAY-UNLIMITED-DEPENDENT-AG                   ELGCSGCI
01614      ELSE IF GCG-DEP-MAX-AGE > ZEROES                             ELGCSGCI
01615          PERFORM DISPLAY-DEPENDENT-AGE.                           ELGCSGCI
01616      IF GCG-STU-MAX-AGE = 999                                     ELGCSGCI
01617          PERFORM DISPLAY-UNLIMITED-STUDENT-AGE                    ELGCSGCI
01618      ELSE IF GCG-STU-MAX-AGE > ZEROES                             ELGCSGCI
01619          PERFORM DISPLAY-STUDENT-AGE.                             ELGCSGCI
01620      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01621      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01622          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01623      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01624                                                                   ELGCSGCI
01625 ************************************************************      ELGCSGCI
01626 *                                                          *      ELGCSGCI
01627 *        DISPLAY DEPENDENT AGE                             *      ELGCSGCI
01628 *                                                          *      ELGCSGCI
01629 ************************************************************      ELGCSGCI
01630  DISPLAY-DEPENDENT-AGE.                                           ELGCSGCI
01631      MOVE GCG-DEP-MAX-AGE TO DEPENDENT-AGE.                       ELGCSGCI
01632      MOVE DEPENDENT-LIT TO RIGHT-SIDE-A.                          ELGCSGCI
01633                                                                   ELGCSGCI
01634 ************************************************************      ELGCSGCI
01635 *                                                          *      ELGCSGCI
01636 *        DISPLAY UNLIMITED DEPENDENT AGE                   *      ELGCSGCI
01637 *                                                          *      ELGCSGCI
01638 ************************************************************      ELGCSGCI
01639  DISPLAY-UNLIMITED-DEPENDENT-AG.                                  ELGCSGCI
01640      MOVE 'DEPENDENT UNLIMITED' TO RIGHT-SIDE-A.                  ELGCSGCI
01641                                                                   ELGCSGCI
01642 ************************************************************      ELGCSGCI
01643 *                                                          *      ELGCSGCI
01644 *        DISPLAY STUDENT AGE                               *      ELGCSGCI
01645 *                                                          *      ELGCSGCI
01646 ************************************************************      ELGCSGCI
01647  DISPLAY-STUDENT-AGE.                                             ELGCSGCI
01648      MOVE GCG-STU-MAX-AGE TO STUDENT-AGE.                         ELGCSGCI
01649      MOVE STUDENT-LIT TO RIGHT-SIDE-B.                            ELGCSGCI
01650                                                                   ELGCSGCI
01651 ************************************************************      ELGCSGCI
01652 *                                                          *      ELGCSGCI
01653 *        DISPLAY UNLIMITED STUDENT AGE                     *      ELGCSGCI
01654 *                                                          *      ELGCSGCI
01655 ************************************************************      ELGCSGCI
01656  DISPLAY-UNLIMITED-STUDENT-AGE.                                   ELGCSGCI
01657      MOVE 'STUDENT UNLIMITED' TO RIGHT-SIDE-B.                    ELGCSGCI
01658                                                                   ELGCSGCI
01659 ************************************************************      ELGCSGCI
01660 *                                                          *      ELGCSGCI
01661 *        CONSTRUCT TIMELY FILING VERBIAGE                  *      ELGCSGCI
01662 *                                                          *      ELGCSGCI
01663 ************************************************************      ELGCSGCI
01664  CONSTRUCT-TIMELY-FILING-VERBIA.                                  ELGCSGCI
01665      INITIALIZE COF-DTL                                           ELGCSGCI
01666                 TCAR-FROM-AREA                                    ELGCSGCI
01667                 WS-SUBA.                                          ELGCSGCI
01668      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01669      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01670      MOVE 'GROUP'                       TO                        ELGCSGCI
01671          CMF-RECORD-PREFIX.                                       ELGCSGCI
01672      MOVE 'TIMELY-FILG-IND'             TO                        ELGCSGCI
01673          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01674      MOVE GCG-TIMELY-FILG-IND           TO CMF-CODE-VALUE.        ELGCSGCI
01675      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01676      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01677          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01678                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01679      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01680      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01681      MOVE +8          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01682      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01683                          TCAR-OUTPUT-FIELD-2-LEN                  ELGCSGCI
01684                          TCAR-OUTPUT-FIELD-3-LEN                  ELGCSGCI
01685                          TCAR-OUTPUT-FIELD-4-LEN                  ELGCSGCI
01686                          TCAR-OUTPUT-FIELD-5-LEN                  ELGCSGCI
01687                          TCAR-OUTPUT-FIELD-6-LEN                  ELGCSGCI
01688                          TCAR-OUTPUT-FIELD-7-LEN                  ELGCSGCI
01689                          TCAR-OUTPUT-FIELD-8-LEN.                 ELGCSGCI
01690      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01691      MOVE 2 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01692      MOVE TIMELY-FILING-LIT TO LEFT-SIDE.                         ELGCSGCI
01693      MOVE TCAR-OPF-DATA (1) TO RIGHT-SIDE.                        ELGCSGCI
01694      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01695          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01696      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01697          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01698      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01699                                                                   ELGCSGCI
01700 ************************************************************      ELGCSGCI
01701 *                                                          *      ELGCSGCI
01702 *        MOVE TRANSLATION LINES OUT                        *      ELGCSGCI
01703 *                                                          *      ELGCSGCI
01704 ************************************************************      ELGCSGCI
01705  MOVE-TRANSLATION-LINES-OUT.                                      ELGCSGCI
01706      ADD 1 TO WS-SUBA.                                            ELGCSGCI
01707      MOVE CMF-DESCR-LINE (WS-SUBB) TO TCAR-FROM-LINE              ELGCSGCI
01708          (WS-SUBA).                                               ELGCSGCI
01709                                                                   ELGCSGCI
01710 ************************************************************      ELGCSGCI
01711 *                                                          *      ELGCSGCI
01712 *        MOVE COMPRESSED LINES                             *      ELGCSGCI
01713 *                                                          *      ELGCSGCI
01714 ************************************************************      ELGCSGCI
01715  MOVE-COMPRESSED-LINES.                                           ELGCSGCI
01716      MOVE LEFT-FILLER          TO LEFT-SIDE.                      ELGCSGCI
01717      PERFORM MOVE-COMPRESSED-LINES-OUT                            ELGCSGCI
01718          VARYING WS-SUBA FROM 2 BY 1                              ELGCSGCI
01719                    UNTIL WS-SUBA > TCAR-OUTPUT-FIELDS-USED.       ELGCSGCI
01720                                                                   ELGCSGCI
01721                                                                   ELGCSGCI
01722 ************************************************************      ELGCSGCI
01723 *                                                          *      ELGCSGCI
01724 *        MOVE COMPRESSED LINES OUT                         *      ELGCSGCI
01725 *                                                          *      ELGCSGCI
01726 ************************************************************      ELGCSGCI
01727  MOVE-COMPRESSED-LINES-OUT.                                       ELGCSGCI
01728      MOVE TCAR-OPF-DATA (WS-SUBA) TO RIGHT-SIDE.                  ELGCSGCI
01729      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01730      MOVE DETAIL-LINE          TO COF-DTL-LINE                    ELGCSGCI
01731          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01732                                                                   ELGCSGCI
01733 ************************************************************      ELGCSGCI
01734 *                                                          *      ELGCSGCI
01735 *        CONSTRUCT TYPE OF CONTRACT VERBIAGE               *      ELGCSGCI
01736 *                                                          *      ELGCSGCI
01737 ************************************************************      ELGCSGCI
01738  CONSTRUCT-TYPE-OF-CONTRACT-VER.                                  ELGCSGCI
01739      INITIALIZE COF-DTL                                           ELGCSGCI
01740                 TCAR-FROM-AREA                                    ELGCSGCI
01741                 WS-SUBA.                                          ELGCSGCI
01742      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01743      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01744      MOVE 'GROUP'                       TO                        ELGCSGCI
01745          CMF-RECORD-PREFIX.                                       ELGCSGCI
01746      MOVE 'L-O-B-CONTRACT-LEVEL-IND'    TO                        ELGCSGCI
01747          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01748      MOVE GCG-L-O-B-CONTRACT-LEVEL-IND  TO CMF-CODE-VALUE.        ELGCSGCI
01749      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01750      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01751          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01752                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01753      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01754      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01755      MOVE +2          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01756      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01757                          TCAR-OUTPUT-FIELD-2-LEN.                 ELGCSGCI
01758      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01759      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01760      MOVE TYPE-OF-CONTRACT-LIT    TO LEFT-SIDE.                   ELGCSGCI
01761      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01762      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01763          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01764      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01765          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01766      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01767                                                                   ELGCSGCI
01768 ************************************************************      ELGCSGCI
01769 *                                                          *      ELGCSGCI
01770 *        CONSTRUCT NETWORK UTILIZATION REVIEW VERBIAGE     *      ELGCSGCI
01771 *                                                          *      ELGCSGCI
01772 ************************************************************      ELGCSGCI
01773  CONSTRUCT-NETWORK-UTIL-REV-PHR.                                  ELGCSGCI
01774      INITIALIZE COF-DTL                                           ELGCSGCI
01775                 TCAR-FROM-AREA                                    ELGCSGCI
01776                 WS-SUBA.                                          ELGCSGCI
01777      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELGCSGCI
01778      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELGCSGCI
01779      MOVE 'GROUP'                       TO                        ELGCSGCI
01780          CMF-RECORD-PREFIX.                                       ELGCSGCI
01781      MOVE 'NETWORK-UTIL-REVIEW-IND'    TO                         ELGCSGCI
01782          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSGCI
01783      MOVE GCG-NETWORK-UTIL-REVIEW-IND  TO CMF-CODE-VALUE.         ELGCSGCI
01784      PERFORM LINK-TO-ELUCMIF.                                     ELGCSGCI
01785      PERFORM MOVE-TRANSLATION-LINES-OUT                           ELGCSGCI
01786          VARYING WS-SUBB FROM 1 BY 1                              ELGCSGCI
01787                    UNTIL WS-SUBB > CMF-NBR-DESCR-LINES.           ELGCSGCI
01788      PERFORM TEXT-COMPRESSION.                                    ELGCSGCI
01789      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELGCSGCI
01790      MOVE +2          TO TCAR-OUTPUT-FIELD-COUNT.                 ELGCSGCI
01791      MOVE +46         TO TCAR-OUTPUT-FIELD-1-LEN                  ELGCSGCI
01792                          TCAR-OUTPUT-FIELD-2-LEN.                 ELGCSGCI
01793      PERFORM TEXT-UNSTRING.                                       ELGCSGCI
01794      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGCSGCI
01795      MOVE NETWORK-UTIL-REV-LIT    TO LEFT-SIDE.                   ELGCSGCI
01796      MOVE TCAR-OPF-DATA (1)       TO RIGHT-SIDE.                  ELGCSGCI
01797      MOVE DETAIL-LINE TO COF-DTL-LINE                             ELGCSGCI
01798          (COF-NBR-DTL-LINES).                                     ELGCSGCI
01799      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELGCSGCI
01800          PERFORM MOVE-COMPRESSED-LINES.                           ELGCSGCI
01801      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSGCI
01802                                                                   ELGCSGCI
01803 ************************************************************      ELGCSGCI
01804 *                                                          *      ELGCSGCI
01805 *        TEXT COMPRESSION                                  *      ELGCSGCI
01806 *                                                          *      ELGCSGCI
01807 ************************************************************      ELGCSGCI
01808  TEXT-COMPRESSION.                                                ELGCSGCI
01809      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELGCSGCI
01810                                                                   ELGCSGCI
01811 ************************************************************      ELGCSGCI
01812 *                                                          *      ELGCSGCI
01813 *        TEXT UNSTRING                                     *      ELGCSGCI
01814 *                                                          *      ELGCSGCI
01815 ************************************************************      ELGCSGCI
01816  TEXT-UNSTRING.                                                   ELGCSGCI
01817      PERFORM TCPR-000-TEXT-UNSTRING.                              ELGCSGCI
01818      COPY ELSTCOMP.                                               ELGCSGCI
