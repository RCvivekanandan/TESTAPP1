00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUCSCOV
00003  PROGRAM-ID.         ELUCSCOV.                                       LV001
00004                                                                   ELUCSCOV
00005  AUTHOR.             ANNE KEFFER-KING.                            ELUCSCOV
00006                                                                   ELUCSCOV
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSCOV
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSCOV
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSCOV
00010                      233 N. MICHIGAN AVE                          ELUCSCOV
00011                      CHICAGO, ILLINOIS 60601                      ELUCSCOV
00012                                                                   ELUCSCOV
00013  DATE-WRITTEN.       05-JAN-1988.                                 ELUCSCOV
00014                                                                   ELUCSCOV
00015  DATE-COMPILED.                                                   ELUCSCOV
00016                                                                   ELUCSCOV
00017  SECURITY.           COPYRIGHT 1986,                              ELUCSCOV
00018                      HEALTH CARE SERVICE CORPORATION              ELUCSCOV
00019      SKIP3                                                        ELUCSCOV
00020  ENVIRONMENT DIVISION.                                            ELUCSCOV
00021                                                                   ELUCSCOV
00022  CONFIGURATION SECTION.                                           ELUCSCOV
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSCOV
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSCOV
00025      EJECT                                                        ELUCSCOV
00026 ******************************************************************ELUCSCOV
00027 *                                                                *ELUCSCOV
00028 *                      MAINTENANCE HISTORY                       *ELUCSCOV
00029 *                                                                *ELUCSCOV
00030 *  MOD     DATE     BY  DRPT                ACTION               *ELUCSCOV
00031 * ----- ----------- --- ----- ---------------------------------- *ELUCSCOV
00032 * 01.00 05-JAN-1988 AKK       CREATED                            *ELUCSCOV
00033 *                                                                *ELUCSCOV
00034 * 01.01 22-APR-1988 AKK       ADDED CERTFN-REQRM UNDER COMMON    *ELUCSCOV
00035 *                             MOVES FROM GCBENPRV RECORD         *ELUCSCOV
00036 *                                                                *ELUCSCOV
00037 * 01.02 18-AUG-1988 REB       CHANGES MADE TO SUPPORT THE OVERALL*ELUCSCOV
00038 *                             ACCUM PROJECT AND USING THE        *ELUCSCOV
00039 *                             ENHANCEMENTS FOR STORAGE MANAGEMENT*ELUCSCOV
00040 *                                                                *ELUCSCOV
00041 * 01.03 25-AUG-1988 REB       CHANGES TO REFLECT NEW VERSION OF  *ELUCSCOV
00042 *                             COPYBOOK 'ELSCSBPC'.               *ELUCSCOV
00043 *                                                                *ELUCSCOV
00044 * 01.04 18-NOV-1988 NAC       CORRECT LOGIC WHEN TESTING THE     *ELUCSCOV
00045 *                             PRESENSE OF BENEFIT SCOPE.         *ELUCSCOV
00046 *                                                                *ELUCSCOV
00047 * 01.05 23-NOV-1988 NAC       TEST PRESENSE IF ACCUM POINTER     *ELUCSCOV
00048 *                             PRIOR TO LINKING TO ACCUM PROGRAMS.*ELUCSCOV
00049 *                                                                *ELUCSCOV
00050 * 01/06 21-DEC-1989 AKK       CHANGED COINS-AD-PTR TO COINS-AD-  *ELUCSCOV
00051 *                             SUB AND DESTRUCTED.                 ELUCSCOV
00052 ******************************************************************ELUCSCOV
00053 *                     PROGRAM OVERVIEW                           *ELUCSCOV
00054 *                                                                *ELUCSCOV
00055 *  *******PLEASE NOTE-- THIS PROGRAM MUST BE COMPILED USING      *ELUCSCOV
00056 *         COBOL COMPILER VERSION 2.1 CHECK FOR OVERRRIDE *****   *ELUCSCOV
00057 *                                                                *ELUCSCOV
00058 *  ELCSCOV - ELS  THIS MODULE DETERMINES WHETHER A GIVEN         *ELUCSCOV
00059 *                 GROUP OF BENEFITS IS COVERED.  THIS IS         *ELUCSCOV
00060 *                 ACCOMPLISHED BY EXAMINING THE CONTRACT         *ELUCSCOV
00061 *                 RECORD AND THE ASSOCIATED BENEFIT PROVISION    *ELUCSCOV
00062 *                 RECORD.  BOTH BASIC AND SUPPLEMENTAL ARE       *ELUCSCOV
00063 *                 EXAMINED.  ELUCSCOV ALSO GATHERS ACCUMULATOR   *ELUCSCOV
00064 *                 INFORMATION AS IT LINKS TO ELUCSACL, ELUCSABM  *ELUCSCOV
00065 *                 AND ELUCSADL.  PAYMENT LEVEL GROUPING INFO-    *ELUCSCOV
00066 *                 RMATION IS ADDED LAST WHEN ELUCSCOV CALLS      *ELUCSCOV
00067 *                 ELUCSPLG.                                      *ELUCSCOV
00068 ****************************************************************  ELUCSCOV
00069 * THE SECTION FOR SETTING 'ANCILLARY CHARGES' TO 'COVERED' IS   * ELUCSCOV
00070 * FOR CASES WHERE ANCILLARY SHOW 'NOT COVERED INITIALLY BUT IS  * ELUCSCOV
00071 * REALLY A COVERED ITEM.                                        * ELUCSCOV
00072 ***************************************************************** ELUCSCOV
00073  DATA DIVISION.                                                   ELUCSCOV
00074  WORKING-STORAGE SECTION.                                         ELUCSCOV
00075  01  PROGRAM-CONSTANTS.                                           ELUCSCOV
00076      03  PC-ABM             PIC X(06)  VALUE '#ABM  '.            ELUCSCOV
00077      03  PC-ACL             PIC X(06)  VALUE '#ACL  '.            ELUCSCOV
00078      03  PC-ADL             PIC X(06)  VALUE '#ADL  '.            ELUCSCOV
00079      03  PC-PPF             PIC X(06)  VALUE '#PPF  '.            ELUCSCOV
00080      03  PC-ANCILLARY       PIC X(05)  VALUE '@ANC '.             ELUCSCOV
00081  01  WORK-AREAS.                                                  ELUCSCOV
00082      03  HOLD-SUBTOPIC      PIC X(03)  VALUE SPACES.              ELUCSCOV
00083      03  HOLD-IDX           PIC S9(04) COMP.                      ELUCSCOV
00084      03  HOLD-PROVISION-GRP PIC S9(04) COMP.                      ELUCSCOV
00085      03  WS-EMERGENCY-PROVISION                                   ELUCSCOV
00086                             PIC  X(06) VALUE SPACES.              ELUCSCOV
00087          88  EMERGENCY-ACCIDENT-PROVISION                         ELUCSCOV
00088                                        VALUE 'EAER B' 'EAC  E'.   ELUCSCOV
00089          88  EMERGENCY-MEDICAL-PROVISION                          ELUCSCOV
00090                                        VALUE 'EMER B' 'EMC  E'.   ELUCSCOV
00091  01  WS-SWITCHES.                                                 ELUCSCOV
00092      03  ANCILLARY-SWITCH   PIC X(01)  VALUE 'Y'.                 ELUCSCOV
00093          88  ANCILLARY-COVERED         VALUE 'Y'.                 ELUCSCOV
00094          88  ANCILLARY-NOT-COVERED     VALUE 'N'.                 ELUCSCOV
00095      03  BENEFIT-PROVSION-SW PIC X(01) VALUE SPACES.              ELUCSCOV
00096          88  BENEFIT-PROVISION-FOUND   VALUE 'Y'.                 ELUCSCOV
00097          88  BENEFIT-PROVISION-NOT-FOUND                          ELUCSCOV
00098                                        VALUE 'N'.                 ELUCSCOV
00099  LINKAGE SECTION.                                                 ELUCSCOV
00100  01  DFHCOMMAREA.                                                 ELUCSCOV
00101     COPY ELSCOMMC.                                                ELUCSCOV
00102 *                                                                 ELUCSCOV
00103     COPY ELSCIA2C.                                                ELUCSCOV
00104 /                                                                 ELUCSCOV
00105     COPY ELSIOPMC.                                                ELUCSCOV
00106 /                                                                 ELUCSCOV
00107     COPY ELSKEYSC.                                                ELUCSCOV
00108 /                                                                 ELUCSCOV
00109     COPY ELSCSBPC.                                                ELUCSCOV
00110 /                                                                 ELUCSCOV
00111     COPY ELSCSPTC.                                                ELUCSCOV
00112 /                                                                 ELUCSCOV
00113     COPY ELSCSACC.                                                ELUCSCOV
00114 /                                                                 ELUCSCOV
00115  01  BENEFIT-PROVISION-RECORD.                                    ELUCSCOV
00116     COPY GCBENPVC.                                                ELUCSCOV
00117 /                                                                 ELUCSCOV
00118  01  CONTRACT-RECORD.                                             ELUCSCOV
00119     COPY GCCONTRC.                                                ELUCSCOV
00120 /                                                                 ELUCSCOV
00121  01  CSCO-PARM-REQ-TABLE.                                         ELUCSCOV
00122      05  CSCO-GROUP-CNT           PIC S9(04)   COMP.              ELUCSCOV
00123      05  CSCO-TBL-CNT             PIC S9(04)   COMP.              ELUCSCOV
00124      05  CSCO-TBL-ENTRY  OCCURS 1 TO 50 TIMES                     ELUCSCOV
00125                         DEPENDING ON CSCO-TBL-CNT                 ELUCSCOV
00126                         INDEXED BY CSCO-IDX.                      ELUCSCOV
00127          07  CSCO-SUBTOPIC        PIC X(03).                      ELUCSCOV
00128          07  CSCO-PROVISION-GRP   PIC S99.                        ELUCSCOV
00129          07  CSCO-BP-ID           PIC X(06).                      ELUCSCOV
00130          07  CSCO-PROVIDER-CLASS  PIC X(01).                      ELUCSCOV
00131          07  CSCO-SERVICE-CLASS   PIC X(01).                      ELUCSCOV
00132          07  CSCO-PAYMENT-REQ     PIC X(01).                      ELUCSCOV
00133      EJECT                                                        ELUCSCOV
00134  PROCEDURE DIVISION.                                              ELUCSCOV
00135 ************************************************************      ELUCSCOV
00136 *                                                          *      ELUCSCOV
00137 *                    PROCEDURE DIVISION                    *      ELUCSCOV
00138 *                                                          *      ELUCSCOV
00139 ************************************************************      ELUCSCOV
00140                                                                   ELUCSCOV
00141 ************************************************************      ELUCSCOV
00142 *                                                          *      ELUCSCOV
00143 *    COVERAGE MODULE                                       *      ELUCSCOV
00144 *                                                          *      ELUCSCOV
00145 ************************************************************      ELUCSCOV
00146  COVERAGE-MODULE.                                                 ELUCSCOV
00147      PERFORM INITIALIZATION.                                      ELUCSCOV
00148      PERFORM PROCESS-COVERAGE-DETERMINATION.                      ELUCSCOV
00149      GOBACK.                                                      ELUCSCOV
00150                                                                   ELUCSCOV
00151                                                                   ELUCSCOV
00152 ************************************************************      ELUCSCOV
00153 *                                                          *      ELUCSCOV
00154 *        INITIALIZATION                                    *      ELUCSCOV
00155 *                                                          *      ELUCSCOV
00156 ************************************************************      ELUCSCOV
00157  INITIALIZATION.                                                  ELUCSCOV
00158      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELUCSCOV
00159      PERFORM CALL-STORAGE-INITIALIZATION-UT.                      ELUCSCOV
00160      PERFORM ESTABLISH-ADDRESSABILITY-OF-PO.                      ELUCSCOV
00161      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELUCSCOV
00162      PERFORM ESTABLISH-ADDRESSABILITY-OF-BE.                      ELUCSCOV
00163                                                                   ELUCSCOV
00164                                                                   ELUCSCOV
00165 ************************************************************      ELUCSCOV
00166 *                                                          *      ELUCSCOV
00167 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELUCSCOV
00168 *                                                          *      ELUCSCOV
00169 ************************************************************      ELUCSCOV
00170  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELUCSCOV
00171      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELUCSCOV
00172      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELUCSCOV
00173      EJECT                                                        ELUCSCOV
00174                                                                   ELUCSCOV
00175                                                                   ELUCSCOV
00176 ************************************************************      ELUCSCOV
00177 *                                                          *      ELUCSCOV
00178 *        CHECK FOR VALID COMMAREA                          *      ELUCSCOV
00179 *                                                          *      ELUCSCOV
00180 ************************************************************      ELUCSCOV
00181  CHECK-FOR-VALID-COMMAREA.                                        ELUCSCOV
00182      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUCSCOV
00183          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSCOV
00184                                                                   ELUCSCOV
00185                                                                   ELUCSCOV
00186 ************************************************************      ELUCSCOV
00187 *                                                          *      ELUCSCOV
00188 *        SIGNAL INVALID COMMAREA                           *      ELUCSCOV
00189 *                                                          *      ELUCSCOV
00190 ************************************************************      ELUCSCOV
00191  SIGNAL-INVALID-COMMAREA.                                         ELUCSCOV
00192      EXEC CICS ABEND                                              ELUCSCOV
00193                ABCODE('EL01')                                     ELUCSCOV
00194         END-EXEC.                                                 ELUCSCOV
00195      EJECT                                                        ELUCSCOV
00196                                                                   ELUCSCOV
00197                                                                   ELUCSCOV
00198 ************************************************************      ELUCSCOV
00199 *                                                          *      ELUCSCOV
00200 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELUCSCOV
00201 *                                                          *      ELUCSCOV
00202 ************************************************************      ELUCSCOV
00203  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELUCSCOV
00204      IF ECA-CIA-PTR = NULL                                        ELUCSCOV
00205          PERFORM SIGNAL-INVALID-CIA                               ELUCSCOV
00206      ELSE                                                         ELUCSCOV
00207          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELUCSCOV
00208                                                                   ELUCSCOV
00209                                                                   ELUCSCOV
00210 ************************************************************      ELUCSCOV
00211 *                                                          *      ELUCSCOV
00212 *        SIGNAL INVALID CIA                                *      ELUCSCOV
00213 *                                                          *      ELUCSCOV
00214 ************************************************************      ELUCSCOV
00215  SIGNAL-INVALID-CIA.                                              ELUCSCOV
00216      EXEC CICS ABEND                                              ELUCSCOV
00217                ABCODE('EL02')                                     ELUCSCOV
00218         END-EXEC.                                                 ELUCSCOV
00219                                                                   ELUCSCOV
00220                                                                   ELUCSCOV
00221 ************************************************************      ELUCSCOV
00222 *                                                          *      ELUCSCOV
00223 *        ESTABLISH ADDRESS OF CIA                          *      ELUCSCOV
00224 *                                                          *      ELUCSCOV
00225 ************************************************************      ELUCSCOV
00226  ESTABLISH-ADDRESS-OF-CIA.                                        ELUCSCOV
00227      SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA TO              ELUCSCOV
00228          ECA-CIA-PTR.                                             ELUCSCOV
00229                                                                   ELUCSCOV
00230                                                                   ELUCSCOV
00231 ************************************************************      ELUCSCOV
00232 *                                                          *      ELUCSCOV
00233 *        CALL STORAGE INITIALIZATION UTILITY               *      ELUCSCOV
00234 *                                                          *      ELUCSCOV
00235 ************************************************************      ELUCSCOV
00236  CALL-STORAGE-INITIALIZATION-UT.                                  ELUCSCOV
00237      CALL 'ELUINISM'  USING DFHCOMMAREA                           ELUCSCOV
00238                       ADDRESS OF                                  ELUCSCOV
00239          CIA-ELS-COMMON-INTERFACE-AREA.                           ELUCSCOV
00240      EJECT                                                        ELUCSCOV
00241                                                                   ELUCSCOV
00242                                                                   ELUCSCOV
00243 ************************************************************      ELUCSCOV
00244 *                                                          *      ELUCSCOV
00245 *        ESTABLISH ADDRESSABILITY OF POINTER LIST          *      ELUCSCOV
00246 *                                                          *      ELUCSCOV
00247 ************************************************************      ELUCSCOV
00248  ESTABLISH-ADDRESSABILITY-OF-PO.                                  ELUCSCOV
00249      SET  CIA-ELSCSPTC-DDN TO TRUE.                               ELUCSCOV
00250      CALL 'ELUSETAD'  USING  DFHCOMMAREA                          ELUCSCOV
00251                       ADDRESS OF CSPT-POINTER-LIST.               ELUCSCOV
00252      IF CIA-RC-PTR-NULL                                           ELUCSCOV
00253          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
00254                                                                   ELUCSCOV
00255                                                                   ELUCSCOV
00256 ************************************************************      ELUCSCOV
00257 *                                                          *      ELUCSCOV
00258 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSCOV
00259 *                                                          *      ELUCSCOV
00260 ************************************************************      ELUCSCOV
00261  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSCOV
00262      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSCOV
00263      PERFORM SIGNAL-ABEND.                                        ELUCSCOV
00264                                                                   ELUCSCOV
00265                                                                   ELUCSCOV
00266 ************************************************************      ELUCSCOV
00267 *                                                          *      ELUCSCOV
00268 *        SIGNAL ABEND                                      *      ELUCSCOV
00269 *                                                          *      ELUCSCOV
00270 ************************************************************      ELUCSCOV
00271  SIGNAL-ABEND.                                                    ELUCSCOV
00272      EXEC CICS ABEND                                              ELUCSCOV
00273                ABCODE(CIA-ABCODE)                                 ELUCSCOV
00274         END-EXEC.                                                 ELUCSCOV
00275      EJECT                                                        ELUCSCOV
00276                                                                   ELUCSCOV
00277                                                                   ELUCSCOV
00278 ************************************************************      ELUCSCOV
00279 *                                                          *      ELUCSCOV
00280 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELUCSCOV
00281 *                                                          *      ELUCSCOV
00282 ************************************************************      ELUCSCOV
00283  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELUCSCOV
00284      SET  CIA-ELSKEYS-DDN TO TRUE.                                ELUCSCOV
00285      CALL 'ELUSETAD'  USING  DFHCOMMAREA                          ELUCSCOV
00286                       ADDRESS OF                                  ELUCSCOV
00287          KWA-FILE-KEY-WORK-AREA.                                  ELUCSCOV
00288      IF CIA-RC-PTR-NULL                                           ELUCSCOV
00289          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
00290      EJECT                                                        ELUCSCOV
00291                                                                   ELUCSCOV
00292                                                                   ELUCSCOV
00293 ************************************************************      ELUCSCOV
00294 *                                                          *      ELUCSCOV
00295 *        ESTABLISH ADDRESSABILITY OF BEN PROV RECORD       *      ELUCSCOV
00296 *                                                          *      ELUCSCOV
00297 ************************************************************      ELUCSCOV
00298  ESTABLISH-ADDRESSABILITY-OF-BE.                                  ELUCSCOV
00299      SET  CIA-GCBENPRV-DDN TO TRUE.                               ELUCSCOV
00300      CALL 'ELUSETAD'  USING  DFHCOMMAREA                          ELUCSCOV
00301                       ADDRESS OF                                  ELUCSCOV
00302          BENEFIT-PROVISION-RECORD.                                ELUCSCOV
00303      IF CIA-RC-PTR-NULL                                           ELUCSCOV
00304          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
00305      EJECT                                                        ELUCSCOV
00306                                                                   ELUCSCOV
00307                                                                   ELUCSCOV
00308 ************************************************************      ELUCSCOV
00309 *                                                          *      ELUCSCOV
00310 *        PROCESS COVERAGE DETERMINATION                    *      ELUCSCOV
00311 *                                                          *      ELUCSCOV
00312 ************************************************************      ELUCSCOV
00313  PROCESS-COVERAGE-DETERMINATION.                                  ELUCSCOV
00314      PERFORM SCAN-REQUEST-LIST                                    ELUCSCOV
00315          VARYING CSPT-IDX FROM 1 BY 1                             ELUCSCOV
00316                  UNTIL CSPT-IDX > CSPT-TBL-CNT.                   ELUCSCOV
00317      PERFORM CALL-OVERALL-ACCUM-DETERMINATI.                      ELUCSCOV
00318      PERFORM CALL-BP-ACCUM-LOADER-UTILITY.                        ELUCSCOV
00319      SET  CIA-ELSCSAC-DDN TO TRUE.                                ELUCSCOV
00320      CALL 'ELUSETAD'  USING  DFHCOMMAREA                          ELUCSCOV
00321                       ADDRESS OF                                  ELUCSCOV
00322          CSAC-ACCUMULATOR-TABLE.                                  ELUCSCOV
00323      IF CIA-RC-OK                                                 ELUCSCOV
00324          PERFORM LINK-TO-ACCUM-PROGRAMS.                          ELUCSCOV
00325      PERFORM LINK-TO-PAYMENT-LEVEL-GROUPING.                      ELUCSCOV
00326      EJECT                                                        ELUCSCOV
00327                                                                   ELUCSCOV
00328                                                                   ELUCSCOV
00329 ************************************************************      ELUCSCOV
00330 *                                                          *      ELUCSCOV
00331 *        LINK TO ACCUM PROGRAMS                            *      ELUCSCOV
00332 *                                                          *      ELUCSCOV
00333 ************************************************************      ELUCSCOV
00334  LINK-TO-ACCUM-PROGRAMS.                                          ELUCSCOV
00335      IF CSAC-ABM-GC-TBL-PTR  NOT = NULLS  OR                      ELUCSCOV
00336                  CSAC-ABM-BP-TBL-PTR  NOT = NULLS                 ELUCSCOV
00337          PERFORM LINK-TO-MAXIMUM.                                 ELUCSCOV
00338      IF CSAC-ACL-GC-TBL-PTR  NOT = NULLS  OR                      ELUCSCOV
00339                  CSAC-ACL-BP-TBL-PTR  NOT = NULLS                 ELUCSCOV
00340          PERFORM LINK-TO-COINSURANCE.                             ELUCSCOV
00341      IF CSAC-ADL-GC-TBL-PTR  NOT = NULLS  OR                      ELUCSCOV
00342                  CSAC-ADL-BP-TBL-PTR  NOT = NULLS                 ELUCSCOV
00343          PERFORM LINK-TO-DEDUCTIBLE.                              ELUCSCOV
00344                                                                   ELUCSCOV
00345                                                                   ELUCSCOV
00346 ************************************************************      ELUCSCOV
00347 *                                                          *      ELUCSCOV
00348 *        LINK TO MAXIMUM                                   *      ELUCSCOV
00349 *                                                          *      ELUCSCOV
00350 ************************************************************      ELUCSCOV
00351  LINK-TO-MAXIMUM.                                                 ELUCSCOV
00352      EXEC CICS LINK                                               ELUCSCOV
00353                PROGRAM ('ELUCSABM')                               ELUCSCOV
00354                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00355         END-EXEC.                                                 ELUCSCOV
00356                                                                   ELUCSCOV
00357                                                                   ELUCSCOV
00358 ************************************************************      ELUCSCOV
00359 *                                                          *      ELUCSCOV
00360 *        LINK TO COINSURANCE                               *      ELUCSCOV
00361 *                                                          *      ELUCSCOV
00362 ************************************************************      ELUCSCOV
00363  LINK-TO-COINSURANCE.                                             ELUCSCOV
00364      EXEC CICS LINK                                               ELUCSCOV
00365                PROGRAM ('ELUCSACL')                               ELUCSCOV
00366                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00367         END-EXEC.                                                 ELUCSCOV
00368                                                                   ELUCSCOV
00369                                                                   ELUCSCOV
00370 ************************************************************      ELUCSCOV
00371 *                                                          *      ELUCSCOV
00372 *        LINK TO DEDUCTIBLE                                *      ELUCSCOV
00373 *                                                          *      ELUCSCOV
00374 ************************************************************      ELUCSCOV
00375  LINK-TO-DEDUCTIBLE.                                              ELUCSCOV
00376      EXEC CICS LINK                                               ELUCSCOV
00377                PROGRAM ('ELUCSADL')                               ELUCSCOV
00378                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00379         END-EXEC.                                                 ELUCSCOV
00380      EJECT                                                        ELUCSCOV
00381                                                                   ELUCSCOV
00382                                                                   ELUCSCOV
00383 ************************************************************      ELUCSCOV
00384 *                                                          *      ELUCSCOV
00385 *        CALL OVERALL ACCUM DETERMINATION UTILITY          *      ELUCSCOV
00386 *                                                          *      ELUCSCOV
00387 ************************************************************      ELUCSCOV
00388  CALL-OVERALL-ACCUM-DETERMINATI.                                  ELUCSCOV
00389      EXEC CICS LINK                                               ELUCSCOV
00390                PROGRAM ('ELUOVACM')                               ELUCSCOV
00391                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00392         END-EXEC.                                                 ELUCSCOV
00393      EJECT                                                        ELUCSCOV
00394                                                                   ELUCSCOV
00395                                                                   ELUCSCOV
00396 ************************************************************      ELUCSCOV
00397 *                                                          *      ELUCSCOV
00398 *        CALL BP ACCUM LOADER UTILITY                      *      ELUCSCOV
00399 *                                                          *      ELUCSCOV
00400 ************************************************************      ELUCSCOV
00401  CALL-BP-ACCUM-LOADER-UTILITY.                                    ELUCSCOV
00402      EXEC CICS LINK                                               ELUCSCOV
00403                PROGRAM ('ELUBPLDA')                               ELUCSCOV
00404                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00405         END-EXEC.                                                 ELUCSCOV
00406      EJECT                                                        ELUCSCOV
00407                                                                   ELUCSCOV
00408                                                                   ELUCSCOV
00409 ************************************************************      ELUCSCOV
00410 *                                                          *      ELUCSCOV
00411 *        LINK TO PAYMENT LEVEL GROUPING                    *      ELUCSCOV
00412 *                                                          *      ELUCSCOV
00413 ************************************************************      ELUCSCOV
00414  LINK-TO-PAYMENT-LEVEL-GROUPING.                                  ELUCSCOV
00415      EXEC CICS LINK                                               ELUCSCOV
00416                PROGRAM ('ELUCSPLG')                               ELUCSCOV
00417                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00418         END-EXEC.                                                 ELUCSCOV
00419                                                                   ELUCSCOV
00420                                                                   ELUCSCOV
00421 ************************************************************      ELUCSCOV
00422 *                                                          *      ELUCSCOV
00423 *        SCAN REQUEST LIST                                 *      ELUCSCOV
00424 *                                                          *      ELUCSCOV
00425 ************************************************************      ELUCSCOV
00426  SCAN-REQUEST-LIST.                                               ELUCSCOV
00427      IF CSPT-REQ-LIST-PTR (CSPT-IDX) NOT = NULLS                  ELUCSCOV
00428          PERFORM SET-UP-BENEFIT-PROVISION-TABLE.                  ELUCSCOV
00429      EJECT                                                        ELUCSCOV
00430                                                                   ELUCSCOV
00431                                                                   ELUCSCOV
00432 ************************************************************      ELUCSCOV
00433 *                                                          *      ELUCSCOV
00434 *        SET UP BENEFIT PROVISION TABLE                    *      ELUCSCOV
00435 *                                                          *      ELUCSCOV
00436 ************************************************************      ELUCSCOV
00437  SET-UP-BENEFIT-PROVISION-TABLE.                                  ELUCSCOV
00438      PERFORM ESTABLISH-ADDRESSABILITY-OF-CU.                      ELUCSCOV
00439      PERFORM ALLOCATE-BENEFIT-PROVISION-TAB.                      ELUCSCOV
00440      PERFORM ESTABLISH-ADDRESS-OF-CURRENT-P.                      ELUCSCOV
00441      PERFORM INITALIZE-AND-MOVE-COMMON-AREA.                      ELUCSCOV
00442      PERFORM INITIALIZE-BENEFIT-PROVISION-T                       ELUCSCOV
00443          VARYING CSBP-X-IDX FROM 1 BY 1 UNTIL                     ELUCSCOV
00444                       CSBP-X-IDX > CSBP-TBL-CNT.                  ELUCSCOV
00445      SET CSBP-X-IDX TO 1.                                         ELUCSCOV
00446      PERFORM CREATE-BENEFIT-PROVISION-TABLE                       ELUCSCOV
00447          VARYING CSCO-IDX FROM 1 BY 1                             ELUCSCOV
00448                       UNTIL CSCO-IDX > CSCO-TBL-CNT.              ELUCSCOV
00449      IF CSPT-IHS-REQ-LIST-PTR NOT = NULLS                         ELUCSCOV
00450          PERFORM RESCAN-BENEFIT-PROVISION-TABLE.                  ELUCSCOV
00451                                                                   ELUCSCOV
00452                                                                   ELUCSCOV
00453 ************************************************************      ELUCSCOV
00454 *                                                          *      ELUCSCOV
00455 *        ESTABLISH ADDRESSABILITY OF CURRENT REQUEST LIST  *      ELUCSCOV
00456 *                                                          *      ELUCSCOV
00457 ************************************************************      ELUCSCOV
00458  ESTABLISH-ADDRESSABILITY-OF-CU.                                  ELUCSCOV
00459      SET ADDRESS OF CSCO-PARM-REQ-TABLE                           ELUCSCOV
00460                  TO CSPT-REQ-LIST-PTR (CSPT-IDX).                 ELUCSCOV
00461      EJECT                                                        ELUCSCOV
00462                                                                   ELUCSCOV
00463                                                                   ELUCSCOV
00464 ************************************************************      ELUCSCOV
00465 *                                                          *      ELUCSCOV
00466 *        ALLOCATE BENEFIT PROVISION TABLE                  *      ELUCSCOV
00467 *                                                          *      ELUCSCOV
00468 ************************************************************      ELUCSCOV
00469  ALLOCATE-BENEFIT-PROVISION-TAB.                                  ELUCSCOV
00470      SET  CIA-ELSCSBPC-DDN  TO TRUE.                              ELUCSCOV
00471      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00472                        ADDRESS OF                                 ELUCSCOV
00473          CSBP-BENEFIT-PROVISION-TABLE.                            ELUCSCOV
00474      COMPUTE CIA-AREA-LEN = LENGTH OF CSBP-FIXED-PORTION +        ELUCSCOV
00475             (CIA-MVO      * LENGTH OF                             ELUCSCOV
00476          CSBP-BENEFIT-PROVISION-ENTRY).                           ELUCSCOV
00477      PERFORM CALL-STORAGE-MANAGER.                                ELUCSCOV
00478      SET  CIA-ELSCSBPC-DDN  TO TRUE.                              ELUCSCOV
00479      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00480                        ADDRESS OF                                 ELUCSCOV
00481          CSBP-BENEFIT-PROVISION-TABLE.                            ELUCSCOV
00482      IF CIA-RC-PTR-NULL                                           ELUCSCOV
00483          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
00484                                                                   ELUCSCOV
00485                                                                   ELUCSCOV
00486 ************************************************************      ELUCSCOV
00487 *                                                          *      ELUCSCOV
00488 *        CALL STORAGE MANAGER                              *      ELUCSCOV
00489 *                                                          *      ELUCSCOV
00490 ************************************************************      ELUCSCOV
00491  CALL-STORAGE-MANAGER.                                            ELUCSCOV
00492      SET  CIA-STG-GETMAIN   TO TRUE.                              ELUCSCOV
00493      EXEC CICS LINK                                               ELUCSCOV
00494                PROGRAM ('ELUSTGMG')                               ELUCSCOV
00495                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
00496         END-EXEC.                                                 ELUCSCOV
00497      EJECT                                                        ELUCSCOV
00498                                                                   ELUCSCOV
00499                                                                   ELUCSCOV
00500 ************************************************************      ELUCSCOV
00501 *                                                          *      ELUCSCOV
00502 *        ESTABLISH ADDRESS OF CURRENT POINTER LIST TABLE   *      ELUCSCOV
00503 *                                                          *      ELUCSCOV
00504 ************************************************************      ELUCSCOV
00505  ESTABLISH-ADDRESS-OF-CURRENT-P.                                  ELUCSCOV
00506      SET  CIA-ELSCSBPC-DDN    TO TRUE.                            ELUCSCOV
00507      CALL 'ELUSETAD'   USING  DFHCOMMAREA                         ELUCSCOV
00508                        CSPT-BP-TBL-PTR (CSPT-IDX).                ELUCSCOV
00509      IF CIA-RC-PTR-NULL                                           ELUCSCOV
00510          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
00511      EJECT                                                        ELUCSCOV
00512                                                                   ELUCSCOV
00513                                                                   ELUCSCOV
00514 ************************************************************      ELUCSCOV
00515 *                                                          *      ELUCSCOV
00516 *        INITALIZE AND MOVE COMMON AREA                    *      ELUCSCOV
00517 *                                                          *      ELUCSCOV
00518 ************************************************************      ELUCSCOV
00519  INITALIZE-AND-MOVE-COMMON-AREA.                                  ELUCSCOV
00520      INITIALIZE CSBP-FIXED-PORTION.                               ELUCSCOV
00521      MOVE CSCO-GROUP-CNT TO CSBP-GROUP-CNT.                       ELUCSCOV
00522      MOVE CSCO-TBL-CNT TO CSBP-TBL-CNT.                           ELUCSCOV
00523                                                                   ELUCSCOV
00524                                                                   ELUCSCOV
00525 ************************************************************      ELUCSCOV
00526 *                                                          *      ELUCSCOV
00527 *        DO SUBTOPIC MOVE                                  *      ELUCSCOV
00528 *                                                          *      ELUCSCOV
00529 ************************************************************      ELUCSCOV
00530  DO-SUBTOPIC-MOVE.                                                ELUCSCOV
00531      MOVE CSCO-SUBTOPIC (CSCO-IDX) TO HOLD-SUBTOPIC.              ELUCSCOV
00532      MOVE CSCO-SUBTOPIC (CSCO-IDX) TO                             ELUCSCOV
00533          CSBP-SUBTOPIC.                                           ELUCSCOV
00534      EJECT                                                        ELUCSCOV
00535                                                                   ELUCSCOV
00536                                                                   ELUCSCOV
00537 ************************************************************      ELUCSCOV
00538 *                                                          *      ELUCSCOV
00539 *        CREATE BENEFIT PROVISION TABLE                    *      ELUCSCOV
00540 *                                                          *      ELUCSCOV
00541 ************************************************************      ELUCSCOV
00542  CREATE-BENEFIT-PROVISION-TABLE.                                  ELUCSCOV
00543      IF CSCO-SUBTOPIC (CSCO-IDX) NOT = HOLD-SUBTOPIC              ELUCSCOV
00544          PERFORM DO-SUBTOPIC-MOVE.                                ELUCSCOV
00545      MOVE CSCO-PROVISION-GRP (CSCO-IDX) TO                        ELUCSCOV
00546          CSBP-PROVISION-GRP (CSBP-X-IDX).                         ELUCSCOV
00547      MOVE CSCO-BP-ID (CSCO-IDX) TO                                ELUCSCOV
00548          CSBP-BP-KEY (CSBP-X-IDX).                                ELUCSCOV
00549      MOVE CSCO-PROVIDER-CLASS (CSCO-IDX) TO                       ELUCSCOV
00550          CSBP-PROVIDER-CLASS (CSBP-X-IDX).                        ELUCSCOV
00551      MOVE CSCO-SERVICE-CLASS (CSCO-IDX) TO                        ELUCSCOV
00552          CSBP-SERVICE-CLASS (CSBP-X-IDX).                         ELUCSCOV
00553      MOVE CSCO-PAYMENT-REQ (CSCO-IDX) TO                          ELUCSCOV
00554          CSBP-PAYMENT-REQ (CSBP-X-IDX).                           ELUCSCOV
00555      PERFORM EXAMINE-PAYMENT-REQUEST.                             ELUCSCOV
00556      SET CSBP-X-IDX UP BY 1.                                      ELUCSCOV
00557      EJECT                                                        ELUCSCOV
00558                                                                   ELUCSCOV
00559                                                                   ELUCSCOV
00560 ************************************************************      ELUCSCOV
00561 *                                                          *      ELUCSCOV
00562 *        INITIALIZE BENEFIT PROVISION TABLE                *      ELUCSCOV
00563 *                                                          *      ELUCSCOV
00564 ************************************************************      ELUCSCOV
00565  INITIALIZE-BENEFIT-PROVISION-T.                                  ELUCSCOV
00566      INITIALIZE CSBP-REQUEST-PARAMETERS (CSBP-X-IDX)              ELUCSCOV
00567                 CSBP-PAYMENT-LEVEL-DATA (CSBP-X-IDX).             ELUCSCOV
00568      PERFORM INITIALIZE-POINTERS.                                 ELUCSCOV
00569      PERFORM INITIALIZE-MAXIMUMS                                  ELUCSCOV
00570          VARYING CSBP-Y-IDX FROM 1 BY 1                           ELUCSCOV
00571                      UNTIL CSBP-Y-IDX  GREATER THAN 2.            ELUCSCOV
00572                                                                   ELUCSCOV
00573                                                                   ELUCSCOV
00574 ************************************************************      ELUCSCOV
00575 *                                                          *      ELUCSCOV
00576 *        INITIALIZE POINTERS                               *      ELUCSCOV
00577 *                                                          *      ELUCSCOV
00578 ************************************************************      ELUCSCOV
00579  INITIALIZE-POINTERS.                                             ELUCSCOV
00580      SET CSBP-SENTENCE-PTR (CSBP-X-IDX) TO NULLS.                 ELUCSCOV
00581      MOVE ZERO TO CSBP-COINS-AD-SUB (CSBP-X-IDX).                 ELUCSCOV
00582                                                                   ELUCSCOV
00583                                                                   ELUCSCOV
00584 ************************************************************      ELUCSCOV
00585 *                                                          *      ELUCSCOV
00586 *        INITIALIZE MAXIMUMS                               *      ELUCSCOV
00587 *                                                          *      ELUCSCOV
00588 ************************************************************      ELUCSCOV
00589  INITIALIZE-MAXIMUMS.                                             ELUCSCOV
00590      INITIALIZE CSBP-BAMA-INFO (CSBP-X-IDX,                       ELUCSCOV
00591          CSBP-Y-IDX).                                             ELUCSCOV
00592      EJECT                                                        ELUCSCOV
00593                                                                   ELUCSCOV
00594                                                                   ELUCSCOV
00595 ************************************************************      ELUCSCOV
00596 *                                                          *      ELUCSCOV
00597 *        EXAMINE PAYMENT REQUEST                           *      ELUCSCOV
00598 *                                                          *      ELUCSCOV
00599 ************************************************************      ELUCSCOV
00600  EXAMINE-PAYMENT-REQUEST.                                         ELUCSCOV
00601 ******************************************                        ELUCSCOV
00602 ** PULLED OUT THE \
00603 ** \
00604 ** PUT CONDITIONS TOGETHER.             **                        ELUCSCOV
00605 ** REB ==> 08/25/88.                    **                        ELUCSCOV
00606 ******************************************                        ELUCSCOV
00607      IF CSBP-NO-PAYMENT-REQUESTED (CSBP-X-IDX) OR                 ELUCSCOV
00608                   CSBP-PAYMENT-REQUESTED (CSBP-X-IDX)             ELUCSCOV
00609          PERFORM CHECK-FOR-BASIC-BENEFITS                         ELUCSCOV
00610      ELSE IF CSBP-USE-SUPP-INFO (CSBP-X-IDX)                      ELUCSCOV
00611          PERFORM CHECK-FOR-SUPPLEMENTAL-BENEFIT.                  ELUCSCOV
00612      IF   CSBP-COVERED (CSBP-X-IDX)                               ELUCSCOV
00613                   OR CSBP-COVERED-ON-SUPP (CSBP-X-IDX)            ELUCSCOV
00614          PERFORM DO-LOB-MOVE.                                     ELUCSCOV
00615      EJECT                                                        ELUCSCOV
00616                                                                   ELUCSCOV
00617                                                                   ELUCSCOV
00618 ************************************************************      ELUCSCOV
00619 *                                                          *      ELUCSCOV
00620 *        CHECK FOR BASIC BENEFITS                          *      ELUCSCOV
00621 *                                                          *      ELUCSCOV
00622 ************************************************************      ELUCSCOV
00623  CHECK-FOR-BASIC-BENEFITS.                                        ELUCSCOV
00624      SET BENEFIT-PROVISION-NOT-FOUND TO TRUE.                     ELUCSCOV
00625      PERFORM PROCESS-BASIC-BENEFITS.                              ELUCSCOV
00626      IF BENEFIT-PROVISION-NOT-FOUND                               ELUCSCOV
00627          PERFORM EXAMINE-SUPPLEMENTAL-BENEFITS                    ELUCSCOV
00628      ELSE                                                         ELUCSCOV
00629          PERFORM SET-BENEFIT-PROVISION-COVERED.                   ELUCSCOV
00630                                                                   ELUCSCOV
00631                                                                   ELUCSCOV
00632 ************************************************************      ELUCSCOV
00633 *                                                          *      ELUCSCOV
00634 *        SET BENEFIT PROVISION COVERED                     *      ELUCSCOV
00635 *                                                          *      ELUCSCOV
00636 ************************************************************      ELUCSCOV
00637  SET-BENEFIT-PROVISION-COVERED.                                   ELUCSCOV
00638      SET CSBP-COVERED (CSBP-X-IDX) TO TRUE.                       ELUCSCOV
00639                                                                   ELUCSCOV
00640                                                                   ELUCSCOV
00641 ************************************************************      ELUCSCOV
00642 *                                                          *      ELUCSCOV
00643 *        PROCESS BASIC BENEFITS                            *      ELUCSCOV
00644 *                                                          *      ELUCSCOV
00645 ************************************************************      ELUCSCOV
00646  PROCESS-BASIC-BENEFITS.                                          ELUCSCOV
00647      PERFORM CHECK-FOR-BASIC-POINTERS.                            ELUCSCOV
00648      IF NOT CIA-RC-PTR-NULL              AND                      ELUCSCOV
00649                    (CSBP-INSTITUTIONAL (CSBP-X-IDX)  OR           ELUCSCOV
00650                     CSBP-PROFESSIONAL (CSBP-X-IDX))               ELUCSCOV
00651          PERFORM PROCESS-INDIVIDUAL-BENEFIT-PRO.                  ELUCSCOV
00652      IF BENEFIT-PROVISION-FOUND AND NOT                           ELUCSCOV
00653                    CSBP-NO-PAYMENT-REQUESTED                      ELUCSCOV
00654          (CSBP-X-IDX)                                             ELUCSCOV
00655          PERFORM CONTINUE-PROCESSING-COVERAGE-D.                  ELUCSCOV
00656      EJECT                                                        ELUCSCOV
00657                                                                   ELUCSCOV
00658                                                                   ELUCSCOV
00659 ************************************************************      ELUCSCOV
00660 *                                                          *      ELUCSCOV
00661 *        EXAMINE SUPPLEMENTAL BENEFITS                     *      ELUCSCOV
00662 *                                                          *      ELUCSCOV
00663 ************************************************************      ELUCSCOV
00664  EXAMINE-SUPPLEMENTAL-BENEFITS.                                   ELUCSCOV
00665      PERFORM PROCESS-SUPPLEMENTAL-BENEFITS.                       ELUCSCOV
00666      IF BENEFIT-PROVISION-NOT-FOUND                               ELUCSCOV
00667          PERFORM SET-BENEFIT-PROVISION-NOT-COVE                   ELUCSCOV
00668      ELSE                                                         ELUCSCOV
00669          PERFORM SET-BENEFIT-PROVISION-COVEREDX.                  ELUCSCOV
00670                                                                   ELUCSCOV
00671                                                                   ELUCSCOV
00672 ************************************************************      ELUCSCOV
00673 *                                                          *      ELUCSCOV
00674 *        SET BENEFIT PROVISION COVERED ON SUPP             *      ELUCSCOV
00675 *                                                          *      ELUCSCOV
00676 ************************************************************      ELUCSCOV
00677  SET-BENEFIT-PROVISION-COVEREDX.                                  ELUCSCOV
00678      SET CSBP-COVERED-ON-SUPP (CSBP-X-IDX) TO TRUE.               ELUCSCOV
00679                                                                   ELUCSCOV
00680                                                                   ELUCSCOV
00681 ************************************************************      ELUCSCOV
00682 *                                                          *      ELUCSCOV
00683 *        SET BENEFIT PROVISION NOT COVERED                 *      ELUCSCOV
00684 *                                                          *      ELUCSCOV
00685 ************************************************************      ELUCSCOV
00686  SET-BENEFIT-PROVISION-NOT-COVE.                                  ELUCSCOV
00687      SET CSBP-NOT-COVERED (CSBP-X-IDX) TO TRUE.                   ELUCSCOV
00688                                                                   ELUCSCOV
00689                                                                   ELUCSCOV
00690 ************************************************************      ELUCSCOV
00691 *                                                          *      ELUCSCOV
00692 *        PROCESS SUPPLEMENTAL BENEFITS                     *      ELUCSCOV
00693 *                                                          *      ELUCSCOV
00694 ************************************************************      ELUCSCOV
00695  PROCESS-SUPPLEMENTAL-BENEFITS.                                   ELUCSCOV
00696      PERFORM CHECK-FOR-POINTERS-ON-SUPPLEME.                      ELUCSCOV
00697      IF NOT CIA-RC-PTR-NULL              AND                      ELUCSCOV
00698                    (CSBP-INSTITUTIONAL (CSBP-X-IDX)  OR           ELUCSCOV
00699                     CSBP-PROFESSIONAL (CSBP-X-IDX))               ELUCSCOV
00700          PERFORM PROCESS-INDIVIDUAL-BENEFIT-PRO.                  ELUCSCOV
00701      IF BENEFIT-PROVISION-FOUND AND                               ELUCSCOV
00702                    CSBP-USE-SUPP-INFO (CSBP-X-IDX)                ELUCSCOV
00703          PERFORM CONTINUE-PROCESSING-COVERAGE-D.                  ELUCSCOV
00704      EJECT                                                        ELUCSCOV
00705                                                                   ELUCSCOV
00706                                                                   ELUCSCOV
00707 ************************************************************      ELUCSCOV
00708 *                                                          *      ELUCSCOV
00709 *        CHECK FOR POINTERS ON SUPPLEMENTAL                *      ELUCSCOV
00710 *                                                          *      ELUCSCOV
00711 ************************************************************      ELUCSCOV
00712  CHECK-FOR-POINTERS-ON-SUPPLEME.                                  ELUCSCOV
00713      IF CSBP-INSTITUTIONAL (CSBP-X-IDX)                           ELUCSCOV
00714          PERFORM ESTABLISH-ADDRESSABILITY-OF-IN                   ELUCSCOV
00715      ELSE                                                         ELUCSCOV
00716          PERFORM ESTABLISH-ADDRESSABILITY-OF-PS.                  ELUCSCOV
00717      EJECT                                                        ELUCSCOV
00718                                                                   ELUCSCOV
00719                                                                   ELUCSCOV
00720 ************************************************************      ELUCSCOV
00721 *                                                          *      ELUCSCOV
00722 *        CHECK FOR BASIC POINTERS                          *      ELUCSCOV
00723 *                                                          *      ELUCSCOV
00724 ************************************************************      ELUCSCOV
00725  CHECK-FOR-BASIC-POINTERS.                                        ELUCSCOV
00726      IF CSBP-INSTITUTIONAL (CSBP-X-IDX)                           ELUCSCOV
00727          PERFORM ESTABLISH-ADDRESSABILITY-OF-IB                   ELUCSCOV
00728      ELSE                                                         ELUCSCOV
00729          PERFORM ESTABLISH-ADDRESSABILITY-OF-PR.                  ELUCSCOV
00730 *WHEN SUPPLEMENTAL PAYMENT INFORMATION IS REQUESTED, SUPPLEMENTAL*ELUCSCOV
00731 *CONTRACT INFO IS CHECKED FIRST, THE BASIC CONTRACTS ARE THEN    *ELUCSCOV
00732 *CHECKED IF NO SUPPLEMENTAL COVERAGE IS FOUND.  THIS IS THE      *ELUCSCOV
00733 *REVERSE OF WHAT IS DONE IF BASIC PAYMENT INFO IS REQUESTED.  THE*ELUCSCOV
00734 *DIFFERENCE IS THAT IF THE PAYMENT REQUEST SHOWED 'USE-SUPP-INFO'*ELUCSCOV
00735 *AND THE BENEFIT IS ACTUALLY COVERED UNDER BASIC THEN BASIC      *ELUCSCOV
00736 *PAYMENT INFORMATION MUST BE SHOWN  AND THE COVERAGE INDICATOR   *ELUCSCOV
00737 *CHANGED TO SHOW COVERED ON BASIC.  IF HOWEVER, THE PAYMENT RE-  *ELUCSCOV
00738 * QUEST ORIGINALLY SHOWED 'PAYMENT-REQUESTED' BUT IS REALLY COV- *ELUCSCOV
00739 *ERED UNDER SUPPLEMENTAL, THEN THE COVERAGE INDICATOR IS CHANGED *ELUCSCOV
00740 *TO SHOW COVERED ON SUPPLEMENTAL, BUT NO PAYMENT INFORMATION IS  *ELUCSCOV
00741 *TO BE SHOWN.                                                    *ELUCSCOV
00742      EJECT                                                        ELUCSCOV
00743                                                                   ELUCSCOV
00744                                                                   ELUCSCOV
00745 ************************************************************      ELUCSCOV
00746 *                                                          *      ELUCSCOV
00747 *        CHECK FOR SUPPLEMENTAL BENEFITS                   *      ELUCSCOV
00748 *                                                          *      ELUCSCOV
00749 ************************************************************      ELUCSCOV
00750  CHECK-FOR-SUPPLEMENTAL-BENEFIT.                                  ELUCSCOV
00751      SET BENEFIT-PROVISION-NOT-FOUND TO TRUE.                     ELUCSCOV
00752      PERFORM PROCESS-SUPPLEMENTAL-BENEFITS.                       ELUCSCOV
00753      IF BENEFIT-PROVISION-NOT-FOUND                               ELUCSCOV
00754          PERFORM EXAMINE-BASIC-BENEFITS                           ELUCSCOV
00755      ELSE                                                         ELUCSCOV
00756          PERFORM SET-BENEFIT-PROVISION-COVEREDX.                  ELUCSCOV
00757      EJECT                                                        ELUCSCOV
00758                                                                   ELUCSCOV
00759                                                                   ELUCSCOV
00760 ************************************************************      ELUCSCOV
00761 *                                                          *      ELUCSCOV
00762 *        EXAMINE BASIC BENEFITS                            *      ELUCSCOV
00763 *                                                          *      ELUCSCOV
00764 ************************************************************      ELUCSCOV
00765  EXAMINE-BASIC-BENEFITS.                                          ELUCSCOV
00766      PERFORM PROCESS-BASIC-BENEFITS.                              ELUCSCOV
00767      IF BENEFIT-PROVISION-NOT-FOUND                               ELUCSCOV
00768          PERFORM SET-BENEFIT-PROVISION-NOT-COVE                   ELUCSCOV
00769      ELSE                                                         ELUCSCOV
00770          PERFORM BASIC-BENEFIT-PROVISION-IS-COV.                  ELUCSCOV
00771                                                                   ELUCSCOV
00772                                                                   ELUCSCOV
00773 ************************************************************      ELUCSCOV
00774 *                                                          *      ELUCSCOV
00775 *        BASIC BENEFIT PROVISION IS COVERED                *      ELUCSCOV
00776 *                                                          *      ELUCSCOV
00777 ************************************************************      ELUCSCOV
00778  BASIC-BENEFIT-PROVISION-IS-COV.                                  ELUCSCOV
00779      PERFORM SET-BENEFIT-PROVISION-COVERED.                       ELUCSCOV
00780      SET CSBP-PAYMENT-REQUESTED (CSBP-X-IDX) TO TRUE.             ELUCSCOV
00781                                                                   ELUCSCOV
00782                                                                   ELUCSCOV
00783 ************************************************************      ELUCSCOV
00784 *                                                          *      ELUCSCOV
00785 *        PROCESS INDIVIDUAL BENEFIT PROVISION              *      ELUCSCOV
00786 *                                                          *      ELUCSCOV
00787 ************************************************************      ELUCSCOV
00788  PROCESS-INDIVIDUAL-BENEFIT-PRO.                                  ELUCSCOV
00789      PERFORM INVESTIGATE-CONTRACT-RECORD-FO                       ELUCSCOV
00790          VARYING GCT-INDEX FROM 1 BY 1                            ELUCSCOV
00791                       UNTIL GCT-INDEX =                           ELUCSCOV
00792              GCT-COUNT-BEN-PROVN-POINTERS                         ELUCSCOV
00793                          OR (GCT-BEN-PROVN-ID (GCT-INDEX) >       ELUCSCOV
00794                             CSBP-BP-KEY (CSBP-X-IDX)).            ELUCSCOV
00795      EJECT                                                        ELUCSCOV
00796                                                                   ELUCSCOV
00797                                                                   ELUCSCOV
00798 ************************************************************      ELUCSCOV
00799 *                                                          *      ELUCSCOV
00800 *        ESTABLISH ADDRESSABILITY OF PROF                  *      ELUCSCOV
00801 *                                                          *      ELUCSCOV
00802 ************************************************************      ELUCSCOV
00803  ESTABLISH-ADDRESSABILITY-OF-PR.                                  ELUCSCOV
00804      SET  CIA-ELSCONPB-DDN TO TRUE.                               ELUCSCOV
00805      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00806                        ADDRESS OF CONTRACT-RECORD.                ELUCSCOV
00807      EJECT                                                        ELUCSCOV
00808                                                                   ELUCSCOV
00809                                                                   ELUCSCOV
00810 ************************************************************      ELUCSCOV
00811 *                                                          *      ELUCSCOV
00812 *        ESTABLISH ADDRESSABILITY OF INST                  *      ELUCSCOV
00813 *                                                          *      ELUCSCOV
00814 ************************************************************      ELUCSCOV
00815  ESTABLISH-ADDRESSABILITY-OF-IB.                                  ELUCSCOV
00816      SET  CIA-ELSCONIB-DDN TO TRUE.                               ELUCSCOV
00817      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00818                        ADDRESS OF CONTRACT-RECORD.                ELUCSCOV
00819                                                                   ELUCSCOV
00820                                                                   ELUCSCOV
00821 ************************************************************      ELUCSCOV
00822 *                                                          *      ELUCSCOV
00823 *        DO LOB MOVE                                       *      ELUCSCOV
00824 *                                                          *      ELUCSCOV
00825 ************************************************************      ELUCSCOV
00826  DO-LOB-MOVE.                                                     ELUCSCOV
00827      MOVE GCT-L-O-B TO CSBP-L-O-B (CSBP-X-IDX).                   ELUCSCOV
00828                                                                   ELUCSCOV
00829                                                                   ELUCSCOV
00830 ************************************************************      ELUCSCOV
00831 *                                                          *      ELUCSCOV
00832 *        ESTABLISH ADDRESSABILITY OF INST SUPP             *      ELUCSCOV
00833 *                                                          *      ELUCSCOV
00834 ************************************************************      ELUCSCOV
00835  ESTABLISH-ADDRESSABILITY-OF-IN.                                  ELUCSCOV
00836      SET  CIA-ELSCONIS-DDN TO TRUE.                               ELUCSCOV
00837      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00838                        ADDRESS OF CONTRACT-RECORD.                ELUCSCOV
00839                                                                   ELUCSCOV
00840                                                                   ELUCSCOV
00841 ************************************************************      ELUCSCOV
00842 *                                                          *      ELUCSCOV
00843 *        ESTABLISH ADDRESSABILITY OF PROF SUPP             *      ELUCSCOV
00844 *                                                          *      ELUCSCOV
00845 ************************************************************      ELUCSCOV
00846  ESTABLISH-ADDRESSABILITY-OF-PS.                                  ELUCSCOV
00847      SET  CIA-ELSCONPS-DDN TO TRUE.                               ELUCSCOV
00848      CALL 'ELUSETAD'   USING DFHCOMMAREA                          ELUCSCOV
00849                        ADDRESS OF CONTRACT-RECORD.                ELUCSCOV
00850      EJECT                                                        ELUCSCOV
00851                                                                   ELUCSCOV
00852                                                                   ELUCSCOV
00853 ************************************************************      ELUCSCOV
00854 *                                                          *      ELUCSCOV
00855 *        INVESTIGATE CONTRACT RECORD FOR MATCH             *      ELUCSCOV
00856 *                                                          *      ELUCSCOV
00857 ************************************************************      ELUCSCOV
00858  INVESTIGATE-CONTRACT-RECORD-FO.                                  ELUCSCOV
00859      IF  GCT-BEN-PROVN-ID (GCT-INDEX) = CSBP-BP-KEY               ELUCSCOV
00860          (CSBP-X-IDX)                                             ELUCSCOV
00861                  AND GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) >          ELUCSCOV
00862          ZERO                                                     ELUCSCOV
00863          PERFORM SETUP-AND-DO-BEN-PROV-READ.                      ELUCSCOV
00864                                                                   ELUCSCOV
00865                                                                   ELUCSCOV
00866 ************************************************************      ELUCSCOV
00867 *                                                          *      ELUCSCOV
00868 *        SETUP AND DO BEN PROV READ                        *      ELUCSCOV
00869 *                                                          *      ELUCSCOV
00870 ************************************************************      ELUCSCOV
00871  SETUP-AND-DO-BEN-PROV-READ.                                      ELUCSCOV
00872      MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) TO                    ELUCSCOV
00873          KWA-GCP-PROVN-SLOT-NO.                                   ELUCSCOV
00874      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO KWA-GCP-PROVN-ID.           ELUCSCOV
00875      PERFORM READ-BENPV-RECORD.                                   ELUCSCOV
00876      IF IOP-RC-OK                                                 ELUCSCOV
00877          PERFORM SET-ADDRESS-OF-BENEFIT-PROV-RE                   ELUCSCOV
00878      ELSE IF IOP-RC-NOTFND                                        ELUCSCOV
00879          PERFORM SIGNAL-CONTRACT-CODING-ERROR.                    ELUCSCOV
00880      SET BENEFIT-PROVISION-FOUND TO TRUE.                         ELUCSCOV
00881                                                                   ELUCSCOV
00882                                                                   ELUCSCOV
00883 ************************************************************      ELUCSCOV
00884 *                                                          *      ELUCSCOV
00885 *        SET ADDRESS OF BENEFIT PROV RECORD                *      ELUCSCOV
00886 *                                                          *      ELUCSCOV
00887 ************************************************************      ELUCSCOV
00888  SET-ADDRESS-OF-BENEFIT-PROV-RE.                                  ELUCSCOV
00889      SET ADDRESS OF BENEFIT-PROVISION-RECORD TO                   ELUCSCOV
00890          IOP-REC-PTR.                                             ELUCSCOV
00891      SET IOP-REC-PTR TO NULLS.                                    ELUCSCOV
00892      EJECT                                                        ELUCSCOV
00893                                                                   ELUCSCOV
00894                                                                   ELUCSCOV
00895 ************************************************************      ELUCSCOV
00896 *                                                          *      ELUCSCOV
00897 *        CONTINUE PROCESSING COVERAGE DETERMINATION        *      ELUCSCOV
00898 *                                                          *      ELUCSCOV
00899 ************************************************************      ELUCSCOV
00900  CONTINUE-PROCESSING-COVERAGE-D.                                  ELUCSCOV
00901      PERFORM DO-COMMON-MOVES.                                     ELUCSCOV
00902      IF CSBP-FORMAT-A (CSBP-X-IDX)                                ELUCSCOV
00903          PERFORM DO-TYPE-A-REC-MOVES                              ELUCSCOV
00904      ELSE IF CSBP-FORMAT-B (CSBP-X-IDX)                           ELUCSCOV
00905          PERFORM DO-TYPE-B-REC-MOVES                              ELUCSCOV
00906      ELSE IF CSBP-FORMAT-C (CSBP-X-IDX)                           ELUCSCOV
00907          PERFORM DO-TYPE-C-REC-MOVES                              ELUCSCOV
00908      ELSE IF CSBP-FORMAT-D (CSBP-X-IDX)                           ELUCSCOV
00909          PERFORM DO-TYPE-D-REC-MOVES                              ELUCSCOV
00910      ELSE IF CSBP-FORMAT-E (CSBP-X-IDX)                           ELUCSCOV
00911          PERFORM DO-TYPE-E-REC-MOVES                              ELUCSCOV
00912      ELSE IF CSBP-FORMAT-W (CSBP-X-IDX)                           ELUCSCOV
00913          PERFORM DO-TYPE-W-REC-MOVES.                             ELUCSCOV
00914 *********************************************************         ELUCSCOV
00915 **  I REMOVED CODE THAT HAD A SEPARATE LOOP TO SEARCH  **         ELUCSCOV
00916 **  FOR THE #PPF TABULAR ATTACHED AT BENEFIT PROVISION **         ELUCSCOV
00917 **  LEVEL. THE LOGIC WILL BE PUT TOGETHER WHEN         **         ELUCSCOV
00918 **  SEARCHING FOR ALL ACCUMULATORS ATTACHED.           **         ELUCSCOV
00919 **            REB ===> 08/18/88.                       **         ELUCSCOV
00920 *********************************************************         ELUCSCOV
00921      PERFORM INVESTIGATE-BENEFIT-PROVISIONX                       ELUCSCOV
00922          VARYING GCP-INDEX FROM 1 BY 1 UNTIL                      ELUCSCOV
00923                     GCP-INDEX =                                   ELUCSCOV
00924              GCP-COUNT-TAB-PROVN-POINTERS.                        ELUCSCOV
00925      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO                             ELUCSCOV
00926          WS-EMERGENCY-PROVISION.                                  ELUCSCOV
00927      IF CSBP-OUTPATIENT-ST AND (EMERGENCY-ACCIDENT-PROVISION      ELUCSCOV
00928          OR                                                       ELUCSCOV
00929                   EMERGENCY-MEDICAL-PROVISION)                    ELUCSCOV
00930          PERFORM EMERGENCY-DAYS-MOVE.                             ELUCSCOV
00931      EJECT                                                        ELUCSCOV
00932                                                                   ELUCSCOV
00933                                                                   ELUCSCOV
00934 ************************************************************      ELUCSCOV
00935 *                                                          *      ELUCSCOV
00936 *        EMERGENCY DAYS MOVE                               *      ELUCSCOV
00937 *                                                          *      ELUCSCOV
00938 ************************************************************      ELUCSCOV
00939  EMERGENCY-DAYS-MOVE.                                             ELUCSCOV
00940      IF EMERGENCY-ACCIDENT-PROVISION                              ELUCSCOV
00941          PERFORM DO-ACCIDENT-DAYS-MOVE                            ELUCSCOV
00942      ELSE IF EMERGENCY-MEDICAL-PROVISION                          ELUCSCOV
00943          PERFORM DO-MEDICAL-DAYS-MOVE.                            ELUCSCOV
00944 *******************************************                       ELUCSCOV
00945 ** I REMOVED CODE THAT MOVED SPACES TO   **                       ELUCSCOV
00946 ** WS-EMERGENCY-PROVISION. IT IS         **                       ELUCSCOV
00947 ** UNNECESSARY BECAUSE WE ARE OVERLAYING **                       ELUCSCOV
00948 ** THAT FIELD EACH TIME AROUND.          **                       ELUCSCOV
00949 **      REB ===> 08/18/88.               **                       ELUCSCOV
00950 *******************************************                       ELUCSCOV
00951                                                                   ELUCSCOV
00952                                                                   ELUCSCOV
00953 ************************************************************      ELUCSCOV
00954 *                                                          *      ELUCSCOV
00955 *        DO ACCIDENT DAYS MOVE                             *      ELUCSCOV
00956 *                                                          *      ELUCSCOV
00957 ************************************************************      ELUCSCOV
00958  DO-ACCIDENT-DAYS-MOVE.                                           ELUCSCOV
00959      MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT TO                        ELUCSCOV
00960           CSBP-DAYS-BTWN-ACCD-EMRG-TREAT (CSBP-X-IDX).            ELUCSCOV
00961                                                                   ELUCSCOV
00962                                                                   ELUCSCOV
00963 ************************************************************      ELUCSCOV
00964 *                                                          *      ELUCSCOV
00965 *        DO MEDICAL DAYS MOVE                              *      ELUCSCOV
00966 *                                                          *      ELUCSCOV
00967 ************************************************************      ELUCSCOV
00968  DO-MEDICAL-DAYS-MOVE.                                            ELUCSCOV
00969      MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT TO                         ELUCSCOV
00970           CSBP-DAYS-BTWN-MED-EMRG-TREAT (CSBP-X-IDX).             ELUCSCOV
00971      EJECT                                                        ELUCSCOV
00972                                                                   ELUCSCOV
00973                                                                   ELUCSCOV
00974 ************************************************************      ELUCSCOV
00975 *                                                          *      ELUCSCOV
00976 *        DO COMMON MOVES                                   *      ELUCSCOV
00977 *                                                          *      ELUCSCOV
00978 ************************************************************      ELUCSCOV
00979  DO-COMMON-MOVES.                                                 ELUCSCOV
00980      MOVE GCP-PROVN-PRICING-METHD TO                              ELUCSCOV
00981                    CSBP-PROVN-PRICING-METHD                       ELUCSCOV
00982          (CSBP-X-IDX).                                            ELUCSCOV
00983      IF GCP-ADDITIONAL-PRICING-PRCNT > ZERO                       ELUCSCOV
00984          PERFORM DO-ADD-PRICING-PRCNT-MOVE.                       ELUCSCOV
00985      IF GCP-VARIABLE-INDEMNITY-PRCNT > ZERO                       ELUCSCOV
00986          PERFORM DO-VARIABLE-INDEMNITY-MOVE.                      ELUCSCOV
00987      IF GCP-CERTFN-REQRM-IND > SPACES                             ELUCSCOV
00988          PERFORM DO-CERTIFICATION-REQUIRED-MOVE.                  ELUCSCOV
00989                                                                   ELUCSCOV
00990                                                                   ELUCSCOV
00991 ************************************************************      ELUCSCOV
00992 *                                                          *      ELUCSCOV
00993 *        DO ADD PRICING PRCNT MOVE                         *      ELUCSCOV
00994 *                                                          *      ELUCSCOV
00995 ************************************************************      ELUCSCOV
00996  DO-ADD-PRICING-PRCNT-MOVE.                                       ELUCSCOV
00997      MOVE GCP-ADDITIONAL-PRICING-PRCNT TO                         ELUCSCOV
00998                    CSBP-ADDITIONAL-PRICING-PRCNT                  ELUCSCOV
00999          (CSBP-X-IDX).                                            ELUCSCOV
01000                                                                   ELUCSCOV
01001                                                                   ELUCSCOV
01002 ************************************************************      ELUCSCOV
01003 *                                                          *      ELUCSCOV
01004 *        DO VARIABLE INDEMNITY MOVE                        *      ELUCSCOV
01005 *                                                          *      ELUCSCOV
01006 ************************************************************      ELUCSCOV
01007  DO-VARIABLE-INDEMNITY-MOVE.                                      ELUCSCOV
01008      MOVE GCP-VARIABLE-INDEMNITY-PRCNT TO                         ELUCSCOV
01009                    CSBP-VARIABLE-INDEMNITY-PRCNT                  ELUCSCOV
01010          (CSBP-X-IDX).                                            ELUCSCOV
01011                                                                   ELUCSCOV
01012                                                                   ELUCSCOV
01013 ************************************************************      ELUCSCOV
01014 *                                                          *      ELUCSCOV
01015 *        DO CERTIFICATION REQUIRED MOVE                    *      ELUCSCOV
01016 *                                                          *      ELUCSCOV
01017 ************************************************************      ELUCSCOV
01018  DO-CERTIFICATION-REQUIRED-MOVE.                                  ELUCSCOV
01019      MOVE GCP-CERTFN-REQRM-IND TO                                 ELUCSCOV
01020                    CSBP-CERTFN-REQRM-IND (CSBP-X-IDX).            ELUCSCOV
01021      EJECT                                                        ELUCSCOV
01022                                                                   ELUCSCOV
01023                                                                   ELUCSCOV
01024 ************************************************************      ELUCSCOV
01025 *                                                          *      ELUCSCOV
01026 *        DO TYPE-A-REC MOVES                               *      ELUCSCOV
01027 *                                                          *      ELUCSCOV
01028 ************************************************************      ELUCSCOV
01029  DO-TYPE-A-REC-MOVES.                                             ELUCSCOV
01030      MOVE GPA-FLAT-RATE-PDM-AMT  TO CSBP-FLAT-RATE-PDM-AMT        ELUCSCOV
01031          (CSBP-X-IDX).                                            ELUCSCOV
01032      MOVE GPA-ADDN-ALLOW-AMT-PER-DAY TO                           ELUCSCOV
01033                      CSBP-ADDN-ALLOW-AMT-PER-DAY                  ELUCSCOV
01034          (CSBP-X-IDX).                                            ELUCSCOV
01035      EJECT                                                        ELUCSCOV
01036                                                                   ELUCSCOV
01037                                                                   ELUCSCOV
01038 ************************************************************      ELUCSCOV
01039 *                                                          *      ELUCSCOV
01040 *        DO TYPE-B-REC MOVES                               *      ELUCSCOV
01041 *                                                          *      ELUCSCOV
01042 ************************************************************      ELUCSCOV
01043  DO-TYPE-B-REC-MOVES.                                             ELUCSCOV
01044      MOVE GPB-MAX-AMT-PER-VISIT TO CSBP-MAX-AMT-PER-VISIT         ELUCSCOV
01045          (CSBP-X-IDX).                                            ELUCSCOV
01046      EJECT                                                        ELUCSCOV
01047                                                                   ELUCSCOV
01048                                                                   ELUCSCOV
01049 ************************************************************      ELUCSCOV
01050 *                                                          *      ELUCSCOV
01051 *        DO TYPE-C-REC MOVES                               *      ELUCSCOV
01052 *                                                          *      ELUCSCOV
01053 ************************************************************      ELUCSCOV
01054  DO-TYPE-C-REC-MOVES.                                             ELUCSCOV
01055      IF GPC-BEN-SCOPE-ID = SPACES                                 ELUCSCOV
01056          PERFORM FORCE-ZEROES-FOR-BENEFIT-SCOPE                   ELUCSCOV
01057      ELSE                                                         ELUCSCOV
01058          PERFORM MOVE-GPC-BENEFIT-SCOPE.                          ELUCSCOV
01059                                                                   ELUCSCOV
01060                                                                   ELUCSCOV
01061 ************************************************************      ELUCSCOV
01062 *                                                          *      ELUCSCOV
01063 *        FORCE ZEROES FOR BENEFIT SCOPE                    *      ELUCSCOV
01064 *                                                          *      ELUCSCOV
01065 ************************************************************      ELUCSCOV
01066  FORCE-ZEROES-FOR-BENEFIT-SCOPE.                                  ELUCSCOV
01067      MOVE ZEROES            TO CSBP-BEN-SCOPE-ID                  ELUCSCOV
01068          (CSBP-X-IDX).                                            ELUCSCOV
01069                                                                   ELUCSCOV
01070                                                                   ELUCSCOV
01071 ************************************************************      ELUCSCOV
01072 *                                                          *      ELUCSCOV
01073 *        MOVE GPC BENEFIT SCOPE                            *      ELUCSCOV
01074 *                                                          *      ELUCSCOV
01075 ************************************************************      ELUCSCOV
01076  MOVE-GPC-BENEFIT-SCOPE.                                          ELUCSCOV
01077      MOVE GPC-BEN-SCOPE-ID  TO CSBP-BEN-SCOPE-ID (CSBP-X-IDX).    ELUCSCOV
01078                                                                   ELUCSCOV
01079                                                                   ELUCSCOV
01080 ************************************************************      ELUCSCOV
01081 *                                                          *      ELUCSCOV
01082 *        MOVE GPD BENEFIT SCOPE                            *      ELUCSCOV
01083 *                                                          *      ELUCSCOV
01084 ************************************************************      ELUCSCOV
01085  MOVE-GPD-BENEFIT-SCOPE.                                          ELUCSCOV
01086      MOVE GPD-BEN-SCOPE-ID  TO CSBP-BEN-SCOPE-ID (CSBP-X-IDX).    ELUCSCOV
01087                                                                   ELUCSCOV
01088                                                                   ELUCSCOV
01089 ************************************************************      ELUCSCOV
01090 *                                                          *      ELUCSCOV
01091 *        MOVE GPE BENEFIT SCOPE                            *      ELUCSCOV
01092 *                                                          *      ELUCSCOV
01093 ************************************************************      ELUCSCOV
01094  MOVE-GPE-BENEFIT-SCOPE.                                          ELUCSCOV
01095      MOVE GPE-BEN-SCOPE-ID  TO CSBP-BEN-SCOPE-ID (CSBP-X-IDX).    ELUCSCOV
01096      EJECT                                                        ELUCSCOV
01097                                                                   ELUCSCOV
01098                                                                   ELUCSCOV
01099 ************************************************************      ELUCSCOV
01100 *                                                          *      ELUCSCOV
01101 *        DO TYPE-D-REC MOVES                               *      ELUCSCOV
01102 *                                                          *      ELUCSCOV
01103 ************************************************************      ELUCSCOV
01104  DO-TYPE-D-REC-MOVES.                                             ELUCSCOV
01105      IF GPD-BEN-SCOPE-ID = SPACES                                 ELUCSCOV
01106          PERFORM FORCE-ZEROES-FOR-BENEFIT-SCOPE                   ELUCSCOV
01107      ELSE                                                         ELUCSCOV
01108          PERFORM MOVE-GPD-BENEFIT-SCOPE.                          ELUCSCOV
01109      MOVE GPD-MAX-AMT-PER-VISIT TO CSBP-MAX-AMT-PER-VISIT         ELUCSCOV
01110          (CSBP-X-IDX).                                            ELUCSCOV
01111      MOVE GPD-FLAT-RATE-PDM-AMT TO CSBP-FLAT-RATE-PDM-AMT         ELUCSCOV
01112          (CSBP-X-IDX).                                            ELUCSCOV
01113      EJECT                                                        ELUCSCOV
01114                                                                   ELUCSCOV
01115                                                                   ELUCSCOV
01116 ************************************************************      ELUCSCOV
01117 *                                                          *      ELUCSCOV
01118 *        DO TYPE-E-REC MOVES                               *      ELUCSCOV
01119 *                                                          *      ELUCSCOV
01120 ************************************************************      ELUCSCOV
01121  DO-TYPE-E-REC-MOVES.                                             ELUCSCOV
01122      IF GPE-BEN-SCOPE-ID = SPACES                                 ELUCSCOV
01123          PERFORM FORCE-ZEROES-FOR-BENEFIT-SCOPE                   ELUCSCOV
01124      ELSE                                                         ELUCSCOV
01125          PERFORM MOVE-GPE-BENEFIT-SCOPE.                          ELUCSCOV
01126      MOVE GPE-MAX-AMT-PER-VISIT TO CSBP-MAX-AMT-PER-VISIT         ELUCSCOV
01127          (CSBP-X-IDX).                                            ELUCSCOV
01128      EJECT                                                        ELUCSCOV
01129                                                                   ELUCSCOV
01130                                                                   ELUCSCOV
01131 ************************************************************      ELUCSCOV
01132 *                                                          *      ELUCSCOV
01133 *        DO TYPE-W-REC MOVES                               *      ELUCSCOV
01134 *                                                          *      ELUCSCOV
01135 ************************************************************      ELUCSCOV
01136  DO-TYPE-W-REC-MOVES.                                             ELUCSCOV
01137      MOVE GPW-FLAT-RATE-PDM-AMT TO CSBP-FLAT-RATE-PDM-AMT         ELUCSCOV
01138          (CSBP-X-IDX).                                            ELUCSCOV
01139      MOVE GPW-ADDN-ALLOW-AMT-PER-DAY TO                           ELUCSCOV
01140                      CSBP-ADDN-ALLOW-AMT-PER-DAY                  ELUCSCOV
01141          (CSBP-X-IDX).                                            ELUCSCOV
01142      EJECT                                                        ELUCSCOV
01143                                                                   ELUCSCOV
01144                                                                   ELUCSCOV
01145 ************************************************************      ELUCSCOV
01146 *                                                          *      ELUCSCOV
01147 *        INVESTIGATE BENEFIT PROVISION FOR ACCUM AND PPF SL*      ELUCSCOV
01148 *                                                          *      ELUCSCOV
01149 ************************************************************      ELUCSCOV
01150  INVESTIGATE-BENEFIT-PROVISIONX.                                  ELUCSCOV
01151 ***********************************************                   ELUCSCOV
01152 **  I CHANGE THE CODE BELOW TO \
01153 **  BECAUSE IT CAN ONLY BE ONE OF THE ACCUMS **                   ELUCSCOV
01154 **  AT ONE TIME.        REB ===> 08/18/88.   **                   ELUCSCOV
01155 ***********************************************                   ELUCSCOV
01156      IF GCP-BP-ID (GCP-INDEX) = PC-ABM AND                        ELUCSCOV
01157                 GCP-BP-SLOT-NO (GCP-INDEX) > ZERO                 ELUCSCOV
01158          PERFORM DO-ABM-SLOT-NUMBER-MOVE                          ELUCSCOV
01159      ELSE IF GCP-BP-ID (GCP-INDEX) = PC-ACL AND                   ELUCSCOV
01160                 GCP-BP-SLOT-NO (GCP-INDEX) > ZERO                 ELUCSCOV
01161          PERFORM DO-ACL-SLOT-NUMBER-MOVE                          ELUCSCOV
01162      ELSE IF GCP-BP-ID (GCP-INDEX) = PC-ADL AND                   ELUCSCOV
01163                 GCP-BP-SLOT-NO (GCP-INDEX) > ZERO                 ELUCSCOV
01164          PERFORM DO-ADL-SLOT-NUMBER-MOVE                          ELUCSCOV
01165      ELSE IF GCP-BP-ID (GCP-INDEX)      = PC-PPF     AND          ELUCSCOV
01166                 GCP-BP-SLOT-NO (GCP-INDEX) > ZERO       AND       ELUCSCOV
01167                 CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) =           ELUCSCOV
01168          '09'                                                     ELUCSCOV
01169          PERFORM DO-PPF-SLOT-NUMBER-MOVE.                         ELUCSCOV
01170      EJECT                                                        ELUCSCOV
01171                                                                   ELUCSCOV
01172                                                                   ELUCSCOV
01173 ************************************************************      ELUCSCOV
01174 *                                                          *      ELUCSCOV
01175 *        DO ABM SLOT NUMBER MOVE                           *      ELUCSCOV
01176 *                                                          *      ELUCSCOV
01177 ************************************************************      ELUCSCOV
01178  DO-ABM-SLOT-NUMBER-MOVE.                                         ELUCSCOV
01179      MOVE GCP-BP-SLOT-NO (GCP-INDEX) TO CSBP-BP-ABM-SLOT          ELUCSCOV
01180          (CSBP-X-IDX).                                            ELUCSCOV
01181      EJECT                                                        ELUCSCOV
01182                                                                   ELUCSCOV
01183                                                                   ELUCSCOV
01184 ************************************************************      ELUCSCOV
01185 *                                                          *      ELUCSCOV
01186 *        DO ACL SLOT NUMBER MOVE                           *      ELUCSCOV
01187 *                                                          *      ELUCSCOV
01188 ************************************************************      ELUCSCOV
01189  DO-ACL-SLOT-NUMBER-MOVE.                                         ELUCSCOV
01190      MOVE GCP-BP-SLOT-NO (GCP-INDEX) TO CSBP-BP-ACL-SLOT          ELUCSCOV
01191          (CSBP-X-IDX).                                            ELUCSCOV
01192      EJECT                                                        ELUCSCOV
01193                                                                   ELUCSCOV
01194                                                                   ELUCSCOV
01195 ************************************************************      ELUCSCOV
01196 *                                                          *      ELUCSCOV
01197 *        DO ADL SLOT NUMBER MOVE                           *      ELUCSCOV
01198 *                                                          *      ELUCSCOV
01199 ************************************************************      ELUCSCOV
01200  DO-ADL-SLOT-NUMBER-MOVE.                                         ELUCSCOV
01201      MOVE GCP-BP-SLOT-NO (GCP-INDEX) TO CSBP-BP-ADL-SLOT          ELUCSCOV
01202          (CSBP-X-IDX).                                            ELUCSCOV
01203      EJECT                                                        ELUCSCOV
01204                                                                   ELUCSCOV
01205                                                                   ELUCSCOV
01206 ************************************************************      ELUCSCOV
01207 *                                                          *      ELUCSCOV
01208 *        DO PPF SLOT NUMBER MOVE                           *      ELUCSCOV
01209 *                                                          *      ELUCSCOV
01210 ************************************************************      ELUCSCOV
01211  DO-PPF-SLOT-NUMBER-MOVE.                                         ELUCSCOV
01212      MOVE GCP-BP-SLOT-NO (GCP-INDEX) TO CSBP-BP-PPF-SLOT          ELUCSCOV
01213          (CSBP-X-IDX).                                            ELUCSCOV
01214                                                                   ELUCSCOV
01215                                                                   ELUCSCOV
01216 ************************************************************      ELUCSCOV
01217 *                                                          *      ELUCSCOV
01218 *        SIGNAL CONTRACT CODING ERROR                      *      ELUCSCOV
01219 *                                                          *      ELUCSCOV
01220 ************************************************************      ELUCSCOV
01221  SIGNAL-CONTRACT-CODING-ERROR.                                    ELUCSCOV
01222      SET CIA-AB-NOTFND-GCBENPRV TO TRUE.                          ELUCSCOV
01223      EXEC CICS ABEND                                              ELUCSCOV
01224          ABCODE(CIA-ABCODE)                                       ELUCSCOV
01225         END-EXEC.                                                 ELUCSCOV
01226      EJECT                                                        ELUCSCOV
01227                                                                   ELUCSCOV
01228                                                                   ELUCSCOV
01229 ************************************************************      ELUCSCOV
01230 *                                                          *      ELUCSCOV
01231 *        READ BENPV RECORD                                 *      ELUCSCOV
01232 *                                                          *      ELUCSCOV
01233 ************************************************************      ELUCSCOV
01234  READ-BENPV-RECORD.                                               ELUCSCOV
01235      SET CIA-GCBENPRV-DDN TO TRUE.                                ELUCSCOV
01236      CALL 'ELUSETAD'  USING DFHCOMMAREA                           ELUCSCOV
01237                       ADDRESS OF                                  ELUCSCOV
01238          IOP-INPUT-OUTPUT-PARAMETERS.                             ELUCSCOV
01239      IF CIA-RC-PTR-NULL                                           ELUCSCOV
01240          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSCOV
01241      SET IOP-RD            TO TRUE.                               ELUCSCOV
01242      SET IOP-FCQ-NONE      TO TRUE.                               ELUCSCOV
01243      SET IOP-KVQ-EQ        TO TRUE.                               ELUCSCOV
01244      SET IOP-STG-MODE-MOVE TO TRUE.                               ELUCSCOV
01245      MOVE KWA-GCBENPRV-KEY TO IOP-FILE-KEY.                       ELUCSCOV
01246      PERFORM LINK-TO-I-O-PGM.                                     ELUCSCOV
01247                                                                   ELUCSCOV
01248                                                                   ELUCSCOV
01249 ************************************************************      ELUCSCOV
01250 *                                                          *      ELUCSCOV
01251 *        LINK TO I O PGM                                   *      ELUCSCOV
01252 *                                                          *      ELUCSCOV
01253 ************************************************************      ELUCSCOV
01254  LINK-TO-I-O-PGM.                                                 ELUCSCOV
01255      EXEC CICS LINK                                               ELUCSCOV
01256                PROGRAM ('ELUIOPGM')                               ELUCSCOV
01257                COMMAREA (DFHCOMMAREA)                             ELUCSCOV
01258         END-EXEC.                                                 ELUCSCOV
01259      EJECT                                                        ELUCSCOV
01260                                                                   ELUCSCOV
01261                                                                   ELUCSCOV
01262 ************************************************************      ELUCSCOV
01263 *                                                          *      ELUCSCOV
01264 *        RESCAN BENEFIT PROVISION TABLE                    *      ELUCSCOV
01265 *                                                          *      ELUCSCOV
01266 ************************************************************      ELUCSCOV
01267  RESCAN-BENEFIT-PROVISION-TABLE.                                  ELUCSCOV
01268      PERFORM DO-SCAN-OF-PROVISION-GROUPS                          ELUCSCOV
01269          VARYING CSBP-X-IDX FROM 1 BY 1 UNTIL CSBP-X-IDX          ELUCSCOV
01270                    > CSBP-TBL-CNT OR ANCILLARY-NOT-COVERED.       ELUCSCOV
01271      IF ANCILLARY-NOT-COVERED                                     ELUCSCOV
01272          PERFORM CONTINUE-RESCAN-OF-PROVISION-G.                  ELUCSCOV
01273                                                                   ELUCSCOV
01274                                                                   ELUCSCOV
01275 ************************************************************      ELUCSCOV
01276 *                                                          *      ELUCSCOV
01277 *        DO SCAN OF PROVISION GROUPS                       *      ELUCSCOV
01278 *                                                          *      ELUCSCOV
01279 ************************************************************      ELUCSCOV
01280  DO-SCAN-OF-PROVISION-GROUPS.                                     ELUCSCOV
01281      IF    CSBP-BP-ID (CSBP-X-IDX) = PC-ANCILLARY                 ELUCSCOV
01282                 AND CSBP-NOT-COVERED (CSBP-X-IDX)                 ELUCSCOV
01283          PERFORM CHECK-FOR-BENEFIT-PROVISION-AN.                  ELUCSCOV
01284                                                                   ELUCSCOV
01285                                                                   ELUCSCOV
01286 ************************************************************      ELUCSCOV
01287 *                                                          *      ELUCSCOV
01288 *        CHECK FOR BENEFIT PROVISION ANCILLARY             *      ELUCSCOV
01289 *                                                          *      ELUCSCOV
01290 ************************************************************      ELUCSCOV
01291  CHECK-FOR-BENEFIT-PROVISION-AN.                                  ELUCSCOV
01292      SET ANCILLARY-NOT-COVERED TO TRUE.                           ELUCSCOV
01293      SET HOLD-IDX TO CSBP-X-IDX.                                  ELUCSCOV
01294      MOVE CSBP-PROVISION-GRP (CSBP-X-IDX) TO                      ELUCSCOV
01295          HOLD-PROVISION-GRP.                                      ELUCSCOV
01296                                                                   ELUCSCOV
01297                                                                   ELUCSCOV
01298 ************************************************************      ELUCSCOV
01299 *                                                          *      ELUCSCOV
01300 *        CONTINUE RESCAN OF PROVISION GROUPS               *      ELUCSCOV
01301 *                                                          *      ELUCSCOV
01302 ************************************************************      ELUCSCOV
01303  CONTINUE-RESCAN-OF-PROVISION-G.                                  ELUCSCOV
01304      PERFORM COMPLETE-RESCAN-OF-BENEFIT-PRO                       ELUCSCOV
01305          VARYING CSBP-X-IDX FROM HOLD-IDX BY 1                    ELUCSCOV
01306                     UNTIL CSBP-X-IDX > CSBP-TBL-CNT OR            ELUCSCOV
01307              ANCILLARY-COVERED.                                   ELUCSCOV
01308                                                                   ELUCSCOV
01309                                                                   ELUCSCOV
01310 ************************************************************      ELUCSCOV
01311 *                                                          *      ELUCSCOV
01312 *        COMPLETE RESCAN OF BENEFIT PROVISION TABLE        *      ELUCSCOV
01313 *                                                          *      ELUCSCOV
01314 ************************************************************      ELUCSCOV
01315  COMPLETE-RESCAN-OF-BENEFIT-PRO.                                  ELUCSCOV
01316      IF CSBP-PROVISION-GRP (CSBP-X-IDX) =                         ELUCSCOV
01317                  HOLD-PROVISION-GRP AND CSBP-COVERED              ELUCSCOV
01318          (CSBP-X-IDX)                                             ELUCSCOV
01319          PERFORM SET-ANCILLARY-TO-COVERED.                        ELUCSCOV
01320                                                                   ELUCSCOV
01321                                                                   ELUCSCOV
01322 ************************************************************      ELUCSCOV
01323 *                                                          *      ELUCSCOV
01324 *        SET ANCILLARY TO COVERED                          *      ELUCSCOV
01325 *                                                          *      ELUCSCOV
01326 ************************************************************      ELUCSCOV
01327  SET-ANCILLARY-TO-COVERED.                                        ELUCSCOV
01328      SET ANCILLARY-COVERED TO TRUE.                               ELUCSCOV
01329      SET CSBP-COVERED (HOLD-IDX) TO TRUE.                         ELUCSCOV
01330                                                                   ELUCSCOV
