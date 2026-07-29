00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUBPLDA
00003  PROGRAM-ID.           ELUBPLDA.                                     LV001
00004                                                                   ELUBPLDA
00005  AUTHOR.               R. BARILEAU.                               ELUBPLDA
00006                        R. LUKETICH 12-OCT-1989.                   ELUBPLDA
00007                                                                   ELUBPLDA
00008  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUBPLDA
00009                        A MUTUAL LEGAL RESERVE COMPANY             ELUBPLDA
00010                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUBPLDA
00011                        233 N. MICHIGAN AVE                        ELUBPLDA
00012                        CHICAGO, ILLINOIS 60601                    ELUBPLDA
00013                                                                   ELUBPLDA
00014  DATE-WRITTEN.         25-AUG-1988.                               ELUBPLDA
00015                        12-OCT-1989 MAJOR RESTRUCTURING AND        ELUBPLDA
00016                                    REVISIONS.                     ELUBPLDA
00017  DATE-COMPILED.                                                   ELUBPLDA
00018                                                                   ELUBPLDA
00019  ENVIRONMENT DIVISION.                                            ELUBPLDA
00020                                                                   ELUBPLDA
00021  CONFIGURATION SECTION.                                           ELUBPLDA
00022                                                                   ELUBPLDA
00023  SOURCE-COMPUTER. IBM-3033.                                       ELUBPLDA
00024  OBJECT-COMPUTER. IBM-3033.                                       ELUBPLDA
00025                                                                   ELUBPLDA
00026 ****************************************************************  ELUBPLDA
00027 *                                                              *  ELUBPLDA
00028 *  ELUBPLDA :  THIS MODULE IS CALLLED BY 'ELUCSCOV'.           *  ELUBPLDA
00029 *              THE PURPOSE IS TO CONSTRUCT A TABLE (ELSATBLC)  *  ELUBPLDA
00030 *              FOR EACH ACCUM TYPE (MAXIMUM OF 4). EACH        *  ELUBPLDA
00031 *              CONTRACT SUMMARY BENEFIT PROVISION TABLE        *  ELUBPLDA
00032 *              (ELSCSBPC) IS SCANNED FOR BP LEVEL ACCUMS SLOTS *  ELUBPLDA
00033 *              SUPPLIED BY 'ELUCSCOV'. IF SLOT IS UNIQUE, IT   *  ELUBPLDA
00034 *              WILL STORE FIELDS ASSOCIATED WITH THAT ACCUM    *  ELUBPLDA
00035 *              TYPE INTO APPROPIATE TABLE (ELSATBLC).          *  ELUBPLDA
00036 *                                                              *  ELUBPLDA
00037 ****************************************************************  ELUBPLDA
00038 *                      MAINTENANCE HISTORY                     *  ELUBPLDA
00039 *                                                              *  ELUBPLDA
00040 *  MOD     DATE      BY              DESCRIPTION               *  ELUBPLDA
00041 * ----- ----------- --- -------------------------------------- *  ELUBPLDA
00042 * 01.00 25-AUG-1988 REB CREATED                                *  ELUBPLDA
00043 * 02.00 12-OCT-1989 RJL CONVERTED FROM STRUCTURE(S) CODE       *  ELUBPLDA
00044 *                       GENERATOR, ADD BISCENDING INDICATOR    *  ELUBPLDA
00045 *                       PROCESSING, MAJOR STRUCTURAL           *  ELUBPLDA
00046 *                       REVISIONS.                             *  ELUBPLDA
00047 * 02.01 08-FEB-1990 EGL CORRECTED INCORRECT ACCESS TO CIA-MVO  *  ELUBPLDA
00048 *                       WHICH CALCULATED AN INCORRECT AMOUNT   *  ELUBPLDA
00049 *                       OF STORAGE AND A STORAGE VIOLATION.    *  ELUBPLDA
00050 * 02.02 15-FEB-1990 RJL CHANGED ACCUMULATOR READS TO LOCATE    *  ELUBPLDA
00051 *                       MODE TO ELIMINATE A STORAGE VIOLATION. *  ELUBPLDA
00052 * 02.03 19-OCT-1992 BAK CHANGED LINKS TO CALLS AND ADDED IDGD  *  ELUBPLDA
00053 *                       AND IPGP SLOT NUMBER SUPPORT.  ALSO    *  ELUBPLDA
00054 *                       ADDED TABULAR PROCESSING FOR IDGD, IPGN*  ELUBPLDA
00055 *                       IPGP AND IPGT MODELED ON IBGR PROCESS. *  ELUBPLDA
00056 * 02.04 27-JUL-1993 RGO ADDED 'DFHEIBLK' TO PARM IN CALLS TO   *  ELUBPLDA
00057 *                       ELUSTGMG AND ELUIOPGM TO PREVENT       *  ELUBPLDA
00058 *                       ASRA'S.                                *  ELUBPLDA
00059 *                                                              *  ELUBPLDA
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00060 ****************************************************************  ELUBPLDA
00061 /                                                                 ELUBPLDA
00062  DATA DIVISION.                                                   ELUBPLDA
00063                                                                   ELUBPLDA
00064  WORKING-STORAGE SECTION.                                         ELUBPLDA
00065                                                                   ELUBPLDA
00066  01  WS-SWITCHES.                                                 ELUBPLDA
00067      05  WS-ACCUM-SLOT-SW           PIC X(01)   VALUE SPACE.      ELUBPLDA
00068          88  DUPLICATE-SLOT-NBR                 VALUE '1'.        ELUBPLDA
00069          88  UNIQUE-SLOT-NBR                    VALUE '2'.        ELUBPLDA
00070      05  WS-PROCESS-ACCUM-SW        PIC X(01)   VALUE SPACE.      ELUBPLDA
00071          88  PROCESS-ABM                        VALUE 'X'.        ELUBPLDA
00072          88  PROCESS-ACL                        VALUE 'Y'.        ELUBPLDA
00073          88  PROCESS-ADL                        VALUE 'Z'.        ELUBPLDA
00074                                                                   ELUBPLDA
00075  01  PROGRAM-CONSTANTS.                                           ELUBPLDA
00076      05  PC-ABM                     PIC X(06)   VALUE '#ABM  '.   ELUBPLDA
00077      05  PC-ACL                     PIC X(06)   VALUE '#ACL  '.   ELUBPLDA
00078      05  PC-ADL                     PIC X(06)   VALUE '#ADL  '.   ELUBPLDA
00079      05  PC-IBGR                    PIC X(06)   VALUE '#IBGR '.   ELUBPLDA
00080      05  PC-IDGD                    PIC X(06)   VALUE '#IDGD '.   ELUBPLDA
00081      05  PC-IPGN                    PIC X(06)   VALUE '#IPGN '.   ELUBPLDA
00082      05  PC-IPGP                    PIC X(06)   VALUE '#IPGP '.   ELUBPLDA
00083      05  PC-IPGT                    PIC X(06)   VALUE '#IPGT '.   ELUBPLDA
00084                                                                   ELUBPLDA
00085  01  WS-IBGR-TBL-MAX                PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00086  01  WS-IDGD-TBL-MAX                PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00087  01  WS-IPGN-TBL-MAX                PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00088  01  WS-IPGP-TBL-MAX                PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00089  01  WS-IPGT-TBL-MAX                PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00090  01  WS-SUB1                        PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00091  01  WS-SUB2                        PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00092  01  WS-SUB3                        PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00093  01  WS-SUB4                        PIC S9(04)  VALUE +0  COMP.   ELUBPLDA
00094  01  WS-IBGR-SLOT-NBR               PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00095  01  WS-IDGD-SLOT-NBR               PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00096  01  WS-IPGN-SLOT-NBR               PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00097  01  WS-IPGP-SLOT-NBR               PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00098  01  WS-IPGT-SLOT-NBR               PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00099  01  WS-ACCUM-SLOT-NBR              PIC S9(07)  VALUE +0  COMP-3. ELUBPLDA
00100 /                                                                 ELUBPLDA
00101  LINKAGE SECTION.                                                 ELUBPLDA
00102 /                                                                 ELUBPLDA
00103  01  DFHCOMMAREA.                                                 ELUBPLDA
00104      COPY ELSCOMMC.                                               ELUBPLDA
00105 /                                                                 ELUBPLDA
00106      COPY ELSCIA2C.                                               ELUBPLDA
00107 /                                                                 ELUBPLDA
00108      COPY ELSIOPMC.                                               ELUBPLDA
00109 /                                                                 ELUBPLDA
00110      COPY ELSKEYSC.                                               ELUBPLDA
00111 /                                                                 ELUBPLDA
00112      COPY ELSCSBPC.                                               ELUBPLDA
00113 /                                                                 ELUBPLDA
00114      COPY ELSCSPTC.                                               ELUBPLDA
00115 /    COPYBOOK USED FOR OVERALL ACCUM ATBL ACCUMULATOR TABLE       ELUBPLDA
00116      COPY ELSATBLC.                                               ELUBPLDA
00117 /    COPYBOOK USED FOR OVERALL ACCUM IBGR TABLE                   ELUBPLDA
00118      COPY ELSIBGRC.                                               ELUBPLDA
00119 /    COPYBOOK USED FOR OVERALL ACCUM IDGD TABLE                   ELUBPLDA
00120      COPY ELSIDGDC.                                               ELUBPLDA
00121 /    COPYBOOK USED FOR OVERALL ACCUM IPGN TABLE                   ELUBPLDA
00122      COPY ELSIPGNC.                                               ELUBPLDA
00123 /    COPYBOOK USED FOR OVERALL ACCUM IPGP TABLE                   ELUBPLDA
00124      COPY ELSIPGPC.                                               ELUBPLDA
00125 /    COPYBOOK USED FOR OVERALL ACCUM IPGT TABLE                   ELUBPLDA
00126      COPY ELSIPGTC.                                               ELUBPLDA
00127 /    COPYBOOK USED FOR OVERALL ACCUM ACCUMULATOR TABLE            ELUBPLDA
00128      COPY ELSCSACC.                                               ELUBPLDA
00129 /    GCPS COPYBOOK USED FOR MAXIMUM RECORD LAYOUT                 ELUBPLDA
00130  01  MAXIMUM-RECORD.                                              ELUBPLDA
00131      COPY GCTABMC.                                                ELUBPLDA
00132 /    GCPS COPYBOOK USED FOR COINSURANCE RECORD LAYOUT             ELUBPLDA
00133  01  COINSURANCE-RECORD.                                          ELUBPLDA
00134      COPY GCTACLC.                                                ELUBPLDA
00135 /    GCPS COPYBOOK USED FOR DEDUCTIBLE RECORD LAYOUT              ELUBPLDA
00136  01  DEDUCTIBLE-RECORD.                                           ELUBPLDA
00137      COPY GCTADLC.                                                ELUBPLDA
00138 /***********************************************************      ELUBPLDA
00139 *                                                          *      ELUBPLDA
00140 *    BENEFIT PROVISION ACCUMULATOR LOADER                  *      ELUBPLDA
00141 *    PROCEDURE DIVISION                                    *      ELUBPLDA
00142 *                                                          *      ELUBPLDA
00143 ************************************************************      ELUBPLDA
00144                                                                   ELUBPLDA
00145  PROCEDURE DIVISION.                                              ELUBPLDA
00146                                                                   ELUBPLDA
00147  000-BP-ACCUM-LOADER-MAIN.                                        ELUBPLDA
00148      PERFORM 001-INITIALIZE.                                      ELUBPLDA
00149      PERFORM 100-PROCESS.                                         ELUBPLDA
00150      GOBACK.                                                      ELUBPLDA
00151                                                                   ELUBPLDA
00152 ************************************************************      ELUBPLDA
00153 *                                                          *      ELUBPLDA
00154 *        INITIALIZE                                        *      ELUBPLDA
00155 *                                                          *      ELUBPLDA
00156 ************************************************************      ELUBPLDA
00157                                                                   ELUBPLDA
00158  001-INITIALIZE.                                                  ELUBPLDA
00159      PERFORM 990-ESTAB-ECI-STG-ENVIRON.                           ELUBPLDA
00160      PERFORM 901-ESTAB-ADDR-ELSCSPTC.                             ELUBPLDA
00161      PERFORM 902-ESTAB-ADDR-ELSKEYS.                              ELUBPLDA
00162      PERFORM 903-ESTAB-ADDR-ELSCSAC.                              ELUBPLDA
00163                                                                   ELUBPLDA
00164 ************************************************************      ELUBPLDA
00165 *                                                          *      ELUBPLDA
00166 *        PROCESS                                           *      ELUBPLDA
00167 *                                                          *      ELUBPLDA
00168 ************************************************************      ELUBPLDA
00169                                                                   ELUBPLDA
00170  100-PROCESS.                                                     ELUBPLDA
00171      PERFORM 110-PROC-ALL-AVAIL-CSBP                              ELUBPLDA
00172         VARYING WS-SUB1 FROM 1 BY 1                               ELUBPLDA
00173           UNTIL WS-SUB1 > CSPT-TBL-CNT.                           ELUBPLDA
00174                                                                   ELUBPLDA
00175 /***********************************************************      ELUBPLDA
00176 *                                                          *      ELUBPLDA
00177 *    PROCESS ALL AVAILABLE CONTRACT SUMMARY BENEFIT        *      ELUBPLDA
00178 *    PROVISION TABLES                                      *      ELUBPLDA
00179 *                                                          *      ELUBPLDA
00180 ************************************************************      ELUBPLDA
00181                                                                   ELUBPLDA
00182  110-PROC-ALL-AVAIL-CSBP.                                         ELUBPLDA
00183      IF CSPT-BP-TBL-PTR (WS-SUB1) NOT = NULLS                     ELUBPLDA
00184      THEN                                                         ELUBPLDA
00185         SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE               ELUBPLDA
00186          TO CSPT-BP-TBL-PTR (WS-SUB1)                             ELUBPLDA
00187         PERFORM 120-PROC-QUAL-CSBP-ENTRIES                        ELUBPLDA
00188             VARYING WS-SUB2 FROM 1 BY 1                           ELUBPLDA
00189               UNTIL WS-SUB2 > CSBP-TBL-CNT.                       ELUBPLDA
00190                                                                   ELUBPLDA
00191 /***********************************************************      ELUBPLDA
00192 *                                                          *      ELUBPLDA
00193 *    PROCESS QUALIFIED CONTRACT SUMMARY BENEFIT PROVISION  *      ELUBPLDA
00194 *    TABLE ENTRIES                                         *      ELUBPLDA
00195 *                                                          *      ELUBPLDA
00196 *  THE TERM \
00197 *  THAT IT MEETS ALL THE FOLLOWING REQUIREMENTS:           *      ELUBPLDA
00198 *                                                          *      ELUBPLDA
00199 *  1.) PROVN-PRICING-METHD > ZERO                          *      ELUBPLDA
00200 *  2.) THE BENEFIT PROVISION IS NOT A FORMAT \
00201 *  3.) BENEFIT PROVISION COVERED AND PAYMENT REQUESTED     *      ELUBPLDA
00202 *              -------  OR  -------                        *      ELUBPLDA
00203 *      BENEFIT PROVISION COVERED ON SUPPLEMENTAL AND       *      ELUBPLDA
00204 *      INDICATED TO USE SUPPLEMENTAL INFORMATION           *      ELUBPLDA
00205 *                                                          *      ELUBPLDA
00206 *    THE FOLLOWING TRUTH TABLE SHOWS ALL THE RULES.        *      ELUBPLDA
00207 *                                                          *      ELUBPLDA
00208 *                ? = DON'T CARE                            *      ELUBPLDA
00209 *                                                          *      ELUBPLDA
00210 *       PPM  = PROVISION PRICING METHOD VALUE              *      ELUBPLDA
00211 *                0 = PPM IS ZERO                           *      ELUBPLDA
00212 *               -0 = PPM IS NON-ZERO                       *      ELUBPLDA
00213 *       FMT  = PROVISION FORMAT CODE                       *      ELUBPLDA
00214 *                W = FORMAT IS 'W'                         *      ELUBPLDA
00215 *               -W = FORMAT IS NOT 'W'                     *      ELUBPLDA
00216 *       PRQ  = WAS PAYMENT INFORMATION REQUESTED           *      ELUBPLDA
00217 *                N = NO                                    *      ELUBPLDA
00218 *                S = USE SUP, IF NOT ON BAS                *      ELUBPLDA
00219 *                Y = YES, BAS INFORMATION ONLY             *      ELUBPLDA
00220 *       COV  = IS BENEFIT COVERED                          *      ELUBPLDA
00221 *                N = NO                                    *      ELUBPLDA
00222 *                S = COVERED ON SUP (BUT NOT BAS)          *      ELUBPLDA
00223 *                Y = COVERED (ON BAS)                      *      ELUBPLDA
00224 *       QUAL = DOES PROVISION QUALIFY (RESULT)             *      ELUBPLDA
00225 *                N = NO                                    *      ELUBPLDA
00226 *                Y = YES                                   *      ELUBPLDA
00227 *                                                          *      ELUBPLDA
00228 *             PPM | FMT | PRQ | COV || QUAL                *      ELUBPLDA
00229 *             ----+-----+-----+-----++------               *      ELUBPLDA
00230 *              0  |  ?  |  ?  |  ?  ||  N                  *      ELUBPLDA
00231 *              ?  |  W  |  ?  |  ?  ||  N                  *      ELUBPLDA
00232 *              ?  |  ?  |  N  |  ?  ||  N                  *      ELUBPLDA
00233 *              ?  |  ?  |  ?  |  N  ||  N                  *      ELUBPLDA
00234 *             -0  | -W  |  P  |  C  ||  Y                  *      ELUBPLDA
00235 *             -0  | -W  |  P  |  S  ||  N                  *      ELUBPLDA
00236 *             -0  | -W  |  S  |  C  ||  Y                  *      ELUBPLDA
00237 *             -0  | -W  |  S  |  S  ||  Y                  *      ELUBPLDA
00238 *                                                          *      ELUBPLDA
00239 ************************************************************      ELUBPLDA
00240                                                                   ELUBPLDA
00241  120-PROC-QUAL-CSBP-ENTRIES.                                      ELUBPLDA
00242      IF    CSBP-PROVN-PRICING-METHD (WS-SUB2) = ZERO              ELUBPLDA
00243         OR CSBP-FORMAT-W (WS-SUB2)                                ELUBPLDA
00244         OR CSBP-NO-PAYMENT-REQUESTED (WS-SUB2)                    ELUBPLDA
00245         OR CSBP-NOT-COVERED (WS-SUB2)                             ELUBPLDA
00246      THEN                                                         ELUBPLDA
00247         CONTINUE                                                  ELUBPLDA
00248      ELSE                                                         ELUBPLDA
00249         IF CSBP-PAYMENT-REQUESTED (WS-SUB2)                       ELUBPLDA
00250         THEN                                                      ELUBPLDA
00251            IF CSBP-COVERED (WS-SUB2)                              ELUBPLDA
00252            THEN                                                   ELUBPLDA
00253               PERFORM 130-PROC-BP-LVL-ACCUM                       ELUBPLDA
00254            ELSE                                                   ELUBPLDA
00255               IF CSBP-USE-SUPP-INFO (WS-SUB2)                     ELUBPLDA
00256               THEN                                                ELUBPLDA
00257                  IF    CSBP-COVERED (WS-SUB2)                     ELUBPLDA
00258                     OR CSBP-COVERED-ON-SUPP (WS-SUB2)             ELUBPLDA
00259                  THEN                                             ELUBPLDA
00260                     PERFORM 130-PROC-BP-LVL-ACCUM                 ELUBPLDA
00261                  ELSE                                             ELUBPLDA
00262                     CONTINUE.                                     ELUBPLDA
00263                                                                   ELUBPLDA
00264 /***********************************************************      ELUBPLDA
00265 *                                                          *      ELUBPLDA
00266 *    PROCESS ALL BENEFIT PROVISION LEVEL ACCUMULATORS      *      ELUBPLDA
00267 *    FOR THE CURRENT QUALIFIED PROVISION                   *      ELUBPLDA
00268 *                                                          *      ELUBPLDA
00269 ************************************************************      ELUBPLDA
00270                                                                   ELUBPLDA
00271  130-PROC-BP-LVL-ACCUM.                                           ELUBPLDA
00272                                                                   ELUBPLDA
00273      IF CSBP-BP-ABM-SLOT (WS-SUB2) > ZERO                         ELUBPLDA
00274      THEN                                                         ELUBPLDA
00275         SET PROCESS-ABM TO TRUE                                   ELUBPLDA
00276         PERFORM 200-PROC-MAXIMUM.                                 ELUBPLDA
00277                                                                   ELUBPLDA
00278      IF CSBP-BP-ACL-SLOT (WS-SUB2) > ZERO                         ELUBPLDA
00279      THEN                                                         ELUBPLDA
00280         SET PROCESS-ACL TO TRUE                                   ELUBPLDA
00281         PERFORM 300-PROC-COINSURANCE.                             ELUBPLDA
00282                                                                   ELUBPLDA
00283      IF CSBP-BP-ADL-SLOT (WS-SUB2) > ZERO                         ELUBPLDA
00284      THEN                                                         ELUBPLDA
00285         SET PROCESS-ADL TO TRUE                                   ELUBPLDA
00286         PERFORM 400-PROC-DEDUCTIBLE.                              ELUBPLDA
00287                                                                   ELUBPLDA
00288 /***********************************************************      ELUBPLDA
00289 *                                                          *      ELUBPLDA
00290 *    PROCESS MAXIMUM ACCUMULATORS                          *      ELUBPLDA
00291 *                                                          *      ELUBPLDA
00292 ************************************************************      ELUBPLDA
00293                                                                   ELUBPLDA
00294  200-PROC-MAXIMUM.                                                ELUBPLDA
00295                                                                   ELUBPLDA
00296      IF CSAC-ABM-BP-TBL-PTR = NULL                                ELUBPLDA
00297      THEN                                                         ELUBPLDA
00298         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00299         PERFORM 500-ACQ-STG-ELSATBL                               ELUBPLDA
00300         CALL 'ELUSETAD'                                           ELUBPLDA
00301            USING DFHCOMMAREA                                      ELUBPLDA
00302                  ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUBPLDA
00303         IF CIA-RC-PTR-NULL                                        ELUBPLDA
00304         THEN                                                      ELUBPLDA
00305            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUBPLDA
00306         ELSE                                                      ELUBPLDA
00307            SET CSAC-ABM-BP-TBL-PTR                                ELUBPLDA
00308             TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                  ELUBPLDA
00309      ELSE                                                         ELUBPLDA
00310         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00311         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUBPLDA
00312          TO CSAC-ABM-BP-TBL-PTR                                   ELUBPLDA
00313         CALL 'ELUSAVAD'                                           ELUBPLDA
00314            USING DFHCOMMAREA                                      ELUBPLDA
00315                  ADDRESS OF ATBL-ACCUMULATOR-TABLE.               ELUBPLDA
00316                                                                   ELUBPLDA
00317      MOVE CSBP-BP-ABM-SLOT (WS-SUB2) TO WS-ACCUM-SLOT-NBR.        ELUBPLDA
00318      PERFORM 510-CHK-SLOT-ALREADY-LOADED.                         ELUBPLDA
00319      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
00320         PERFORM 210-OBTAIN-ABM-RECORD                             ELUBPLDA
00321         PERFORM 220-BUILD-ABM-ELSATBLC                            ELUBPLDA
00322            VARYING GAA-INDEX FROM 1 BY 1                          ELUBPLDA
00323              UNTIL    GAA-INDEX = GAA-ENTRY-COUNT                 ELUBPLDA
00324                    OR GAA-ENTRY (GAA-INDEX) = HIGH-VALUES         ELUBPLDA
00325      ELSE                                                         ELUBPLDA
00326         CONTINUE.                                                 ELUBPLDA
00327                                                                   ELUBPLDA
00328 /***********************************************************      ELUBPLDA
00329 *                                                          *      ELUBPLDA
00330 *    OBTAIN ABM TABULAR RECORD AT BENEFIT PROVISION LEVEL  *      ELUBPLDA
00331 *                                                          *      ELUBPLDA
00332 ************************************************************      ELUBPLDA
00333                                                                   ELUBPLDA
00334  210-OBTAIN-ABM-RECORD.                                           ELUBPLDA
00335      MOVE PC-ABM TO KWA-PROVISION-ID.                             ELUBPLDA
00336      MOVE CSBP-BP-ABM-SLOT (WS-SUB2) TO KWA-PROVISION-SLOT-NO.    ELUBPLDA
00337      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
00338      SET ADDRESS OF MAXIMUM-RECORD TO IOP-REC-PTR.                ELUBPLDA
00339                                                                   ELUBPLDA
00340 ************************************************************      ELUBPLDA
00341 *                                                          *      ELUBPLDA
00342 *    BUILD MAXIMUMS ACCUMULATOR TABLE                      *      ELUBPLDA
00343 *                                                          *      ELUBPLDA
00344 ************************************************************      ELUBPLDA
00345                                                                   ELUBPLDA
00346  220-BUILD-ABM-ELSATBLC.                                          ELUBPLDA
00347      IF GAA-ENTRY (GAA-INDEX) NOT = HIGH-VALUES                   ELUBPLDA
00348      THEN                                                         ELUBPLDA
00349         PERFORM 520-INIT-NEXT-ATBL-ENTRY                          ELUBPLDA
00350         PERFORM 221-LOAD-ABM-DATA-ELEMENTS                        ELUBPLDA
00351         PERFORM 222-LOAD-ABM-INTERNAL-TABS                        ELUBPLDA
00352            VARYING GAA-INT-INDEX FROM 1 BY 1                      ELUBPLDA
00353              UNTIL GAA-INT-INDEX >                                ELUBPLDA
00354                 GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX).           ELUBPLDA
00355                                                                   ELUBPLDA
00356 /***********************************************************      ELUBPLDA
00357 *                                                          *      ELUBPLDA
00358 *    LOAD ABM DATA ELEMENTS INTO ATBL TABLE ENTRY          *      ELUBPLDA
00359 *                                                          *      ELUBPLDA
00360 ************************************************************      ELUBPLDA
00361                                                                   ELUBPLDA
00362  221-LOAD-ABM-DATA-ELEMENTS.                                      ELUBPLDA
00363      MOVE GAA-PROVISION-SLOT-NO                                   ELUBPLDA
00364        TO ATBL-SLOT-NUMBER (ATBL-TBL-CNT).                        ELUBPLDA
00365      MOVE GAA-BAMA-BEN-PER-MAX-OVRD-IND (GAA-INDEX)               ELUBPLDA
00366        TO ATBL-BEN-PER-MAX-OVRD-IND (ATBL-TBL-CNT).               ELUBPLDA
00367      MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                  ELUBPLDA
00368        TO ATBL-BEN-PER-TIME-FCTR (ATBL-TBL-CNT).                  ELUBPLDA
00369      MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                  ELUBPLDA
00370        TO ATBL-BEN-PER-TIME-QUAL (ATBL-TBL-CNT).                  ELUBPLDA
00371      MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     ELUBPLDA
00372        TO ATBL-BENEFIT-PERIOD (ATBL-TBL-CNT).                     ELUBPLDA
00373      MOVE GAA-BAMA-BISCENDING-IND-RSV (GAA-INDEX)                 ELUBPLDA
00374        TO ATBL-BISCEND-IND (ATBL-TBL-CNT).                        ELUBPLDA
00375      MOVE GAA-BAMA-CO-PAY-IND (GAA-INDEX)                         ELUBPLDA
00376        TO ATBL-CO-PAY-IND (ATBL-TBL-CNT).                         ELUBPLDA
00377      MOVE GAA-BAMA-CONDITION (GAA-INDEX)                          ELUBPLDA
00378        TO ATBL-CONDITION (ATBL-TBL-CNT).                          ELUBPLDA
00379      MOVE GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                   ELUBPLDA
00380        TO ATBL-COST-CONTAIN-IND (ATBL-TBL-CNT).                   ELUBPLDA
00381      MOVE GAA-BAMA-DAY-FACTOR-IND (GAA-INDEX)                     ELUBPLDA
00382        TO ATBL-DAY-FACTOR-IND (ATBL-TBL-CNT).                     ELUBPLDA
00383      MOVE GAA-BAMA-DEFINITION (GAA-INDEX)                         ELUBPLDA
00384        TO ATBL-DEFINITION (ATBL-TBL-CNT).                         ELUBPLDA
00385      MOVE GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                       ELUBPLDA
00386        TO ATBL-FAM-OR-INDIV (ATBL-TBL-CNT).                       ELUBPLDA
00387      MOVE GAA-BAMA-FYI-VALUE (GAA-INDEX)                          ELUBPLDA
00388        TO ATBL-FYI-VALUE (ATBL-TBL-CNT).                          ELUBPLDA
00389      MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                ELUBPLDA
00390        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-TBL-CNT).                ELUBPLDA
00391      MOVE GAA-BAMA-INTERVAL-OVRD-IND (GAA-INDEX)                  ELUBPLDA
00392        TO ATBL-INTERVAL-OVRD-IND (ATBL-TBL-CNT).                  ELUBPLDA
00393      MOVE GAA-BAMA-INTERVAL-OVRD-VALUE (GAA-INDEX)                ELUBPLDA
00394        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-TBL-CNT).                ELUBPLDA
00395      MOVE GAA-BAMA-INTERVAL-TIME-FCTR (GAA-INDEX)                 ELUBPLDA
00396        TO ATBL-INTERVAL-TIME-FCTR (ATBL-TBL-CNT).                 ELUBPLDA
00397      MOVE GAA-BAMA-INTERVAL-TYPE (GAA-INDEX)                      ELUBPLDA
00398        TO ATBL-INTERVAL-TYPE (ATBL-TBL-CNT).                      ELUBPLDA
00399      MOVE GAA-BAMA-L-O-B (GAA-INDEX)                              ELUBPLDA
00400        TO ATBL-L-O-B (ATBL-TBL-CNT).                              ELUBPLDA
00401      MOVE GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                 ELUBPLDA
00402        TO ATBL-PLACE-OF-TREATMENT (ATBL-TBL-CNT).                 ELUBPLDA
00403      MOVE GAA-BAMA-REINSTATEMENT-IND (GAA-INDEX)                  ELUBPLDA
00404        TO ATBL-REINSTATEMENT-IND  (ATBL-TBL-CNT).                 ELUBPLDA
00405      MOVE GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                      ELUBPLDA
00406        TO ATBL-SERVICE-GROUP (ATBL-TBL-CNT).                      ELUBPLDA
00407      MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                        ELUBPLDA
00408        TO ATBL-VALUE-LIMIT (ATBL-TBL-CNT).                        ELUBPLDA
00409      MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                    ELUBPLDA
00410        TO ATBL-VALUE-QUALIFIER (ATBL-TBL-CNT).                    ELUBPLDA
00411                                                                   ELUBPLDA
00412 /***********************************************************      ELUBPLDA
00413 *                                                          *      ELUBPLDA
00414 *    LOAD ABM INTERNAL TABULARS DATA                       *      ELUBPLDA
00415 *                                                          *      ELUBPLDA
00416 ************************************************************      ELUBPLDA
00417                                                                   ELUBPLDA
00418  222-LOAD-ABM-INTERNAL-TABS.                                      ELUBPLDA
00419      IF     GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX) = PC-IBGR       ELUBPLDA
00420         AND GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > ZERO        ELUBPLDA
00421      THEN                                                         ELUBPLDA
00422         MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)              ELUBPLDA
00423           TO ATBL-IBGR-SLOT-NUMBER (ATBL-TBL-CNT)                 ELUBPLDA
00424              WS-IBGR-SLOT-NBR                                     ELUBPLDA
00425         PERFORM 530-LOAD-IBGR-TABULAR                             ELUBPLDA
00426      ELSE                                                         ELUBPLDA
00427         IF     GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX) = PC-IDGD    ELUBPLDA
00428            AND GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > ZERO     ELUBPLDA
00429         THEN                                                      ELUBPLDA
00430            MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)           ELUBPLDA
00431              TO ATBL-IDGD-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00432                 WS-IDGD-SLOT-NBR                                  ELUBPLDA
00433            PERFORM 540-LOAD-IDGD-TABULAR                          ELUBPLDA
00434      ELSE                                                         ELUBPLDA
00435         IF     GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX) = PC-IPGN    ELUBPLDA
00436            AND GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > ZERO     ELUBPLDA
00437         THEN                                                      ELUBPLDA
00438            MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)           ELUBPLDA
00439              TO ATBL-IPGN-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00440                 WS-IPGN-SLOT-NBR                                  ELUBPLDA
00441            PERFORM 550-LOAD-IPGN-TABULAR                          ELUBPLDA
00442      ELSE                                                         ELUBPLDA
00443         IF     GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX) = PC-IPGP    ELUBPLDA
00444            AND GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > ZERO     ELUBPLDA
00445         THEN                                                      ELUBPLDA
00446            MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)           ELUBPLDA
00447              TO ATBL-IPGP-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00448                 WS-IPGP-SLOT-NBR                                  ELUBPLDA
00449            PERFORM 560-LOAD-IPGP-TABULAR                          ELUBPLDA
00450      ELSE                                                         ELUBPLDA
00451         IF     GAA-INT-ID (GAA-INDEX, GAA-INT-INDEX) = PC-IPGT    ELUBPLDA
00452            AND GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX) > ZERO     ELUBPLDA
00453         THEN                                                      ELUBPLDA
00454            MOVE GAA-INT-SLOT (GAA-INDEX, GAA-INT-INDEX)           ELUBPLDA
00455              TO ATBL-IPGT-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00456                 WS-IPGT-SLOT-NBR                                  ELUBPLDA
00457            PERFORM 570-LOAD-IPGT-TABULAR                          ELUBPLDA
00458      ELSE                                                         ELUBPLDA
00459            CONTINUE.                                              ELUBPLDA
00460                                                                   ELUBPLDA
00461 /***********************************************************      ELUBPLDA
00462 *                                                          *      ELUBPLDA
00463 *    PROCESS COINSURANCE ACCUMULATORS                      *      ELUBPLDA
00464 *                                                          *      ELUBPLDA
00465 ************************************************************      ELUBPLDA
00466                                                                   ELUBPLDA
00467  300-PROC-COINSURANCE.                                            ELUBPLDA
00468                                                                   ELUBPLDA
00469      IF CSAC-ACL-BP-TBL-PTR = NULL                                ELUBPLDA
00470      THEN                                                         ELUBPLDA
00471         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00472         PERFORM 500-ACQ-STG-ELSATBL                               ELUBPLDA
00473         CALL 'ELUSETAD'                                           ELUBPLDA
00474            USING DFHCOMMAREA                                      ELUBPLDA
00475                  ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUBPLDA
00476         IF CIA-RC-PTR-NULL                                        ELUBPLDA
00477         THEN                                                      ELUBPLDA
00478            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUBPLDA
00479         ELSE                                                      ELUBPLDA
00480            SET CSAC-ACL-BP-TBL-PTR                                ELUBPLDA
00481             TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                  ELUBPLDA
00482      ELSE                                                         ELUBPLDA
00483         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00484         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUBPLDA
00485          TO CSAC-ACL-BP-TBL-PTR                                   ELUBPLDA
00486         CALL 'ELUSAVAD'                                           ELUBPLDA
00487            USING DFHCOMMAREA                                      ELUBPLDA
00488                  ADDRESS OF ATBL-ACCUMULATOR-TABLE.               ELUBPLDA
00489                                                                   ELUBPLDA
00490      MOVE CSBP-BP-ACL-SLOT (WS-SUB2) TO WS-ACCUM-SLOT-NBR.        ELUBPLDA
00491      PERFORM 510-CHK-SLOT-ALREADY-LOADED.                         ELUBPLDA
00492      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
00493         PERFORM 310-OBTAIN-ACL-RECORD                             ELUBPLDA
00494         PERFORM 320-BUILD-ACL-ELSATBLC                            ELUBPLDA
00495            VARYING GAB-INDEX FROM 1 BY 1                          ELUBPLDA
00496              UNTIL    GAB-INDEX = GAB-ENTRY-COUNT                 ELUBPLDA
00497                    OR GAB-ENTRY (GAB-INDEX) = HIGH-VALUES         ELUBPLDA
00498      ELSE                                                         ELUBPLDA
00499         CONTINUE.                                                 ELUBPLDA
00500                                                                   ELUBPLDA
00501 /***********************************************************      ELUBPLDA
00502 *                                                          *      ELUBPLDA
00503 *    OBTAIN ACL TABULAR RECORD AT BENEFIT PROVISION LEVEL  *      ELUBPLDA
00504 *                                                          *      ELUBPLDA
00505 ************************************************************      ELUBPLDA
00506                                                                   ELUBPLDA
00507  310-OBTAIN-ACL-RECORD.                                           ELUBPLDA
00508      MOVE PC-ACL TO KWA-PROVISION-ID.                             ELUBPLDA
00509      MOVE CSBP-BP-ACL-SLOT (WS-SUB2) TO KWA-PROVISION-SLOT-NO.    ELUBPLDA
00510      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
00511      SET ADDRESS OF COINSURANCE-RECORD TO IOP-REC-PTR.            ELUBPLDA
00512                                                                   ELUBPLDA
00513 ************************************************************      ELUBPLDA
00514 *                                                          *      ELUBPLDA
00515 *    BUILD COINSURANCE ACCUMULATOR TABLE                   *      ELUBPLDA
00516 *                                                          *      ELUBPLDA
00517 ************************************************************      ELUBPLDA
00518                                                                   ELUBPLDA
00519  320-BUILD-ACL-ELSATBLC.                                          ELUBPLDA
00520      IF GAB-ENTRY (GAB-INDEX) NOT = HIGH-VALUES                   ELUBPLDA
00521      THEN                                                         ELUBPLDA
00522         PERFORM 520-INIT-NEXT-ATBL-ENTRY                          ELUBPLDA
00523         PERFORM 321-LOAD-ACL-DATA-ELEMENTS                        ELUBPLDA
00524         PERFORM 322-LOAD-ACL-INTERNAL-TABS                        ELUBPLDA
00525            VARYING GAB-INT-INDEX FROM 1 BY 1                      ELUBPLDA
00526              UNTIL GAB-INT-INDEX >                                ELUBPLDA
00527                 GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).           ELUBPLDA
00528                                                                   ELUBPLDA
00529 /***********************************************************      ELUBPLDA
00530 *                                                          *      ELUBPLDA
00531 *    LOAD ACL DATA ELEMENTS INTO ATBL TABLE ENTRY          *      ELUBPLDA
00532 *                                                          *      ELUBPLDA
00533 ************************************************************      ELUBPLDA
00534                                                                   ELUBPLDA
00535  321-LOAD-ACL-DATA-ELEMENTS.                                      ELUBPLDA
00536      MOVE GAB-PROVISION-SLOT-NO                                   ELUBPLDA
00537        TO ATBL-SLOT-NUMBER (ATBL-TBL-CNT).                        ELUBPLDA
00538      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)               ELUBPLDA
00539        TO ATBL-1ST-DOLR-COVRGE-LMT (ATBL-TBL-CNT).                ELUBPLDA
00540      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                ELUBPLDA
00541        TO ATBL-ASCEND-DESCEND-IND (ATBL-TBL-CNT).                 ELUBPLDA
00542      MOVE GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX)                 ELUBPLDA
00543        TO ATBL-BEN-PER-TIME-FCTR (ATBL-TBL-CNT).                  ELUBPLDA
00544      MOVE GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX)                 ELUBPLDA
00545        TO ATBL-BEN-PER-TIME-QUAL (ATBL-TBL-CNT).                  ELUBPLDA
00546      MOVE GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    ELUBPLDA
00547        TO ATBL-BENEFIT-PERIOD (ATBL-TBL-CNT).                     ELUBPLDA
00548      MOVE GAB-COINS-BISCENDING-IND (GAB-INDEX)                    ELUBPLDA
00549        TO ATBL-BISCEND-IND (ATBL-TBL-CNT).                        ELUBPLDA
00550      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)               ELUBPLDA
00551        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-TBL-CNT).                ELUBPLDA
00552      MOVE GAB-COINS-CO-PAY-IND (GAB-INDEX)                        ELUBPLDA
00553        TO ATBL-CO-PAY-IND (ATBL-TBL-CNT).                         ELUBPLDA
00554      MOVE GAB-COINS-CONDITION (GAB-INDEX)                         ELUBPLDA
00555        TO ATBL-CONDITION (ATBL-TBL-CNT).                          ELUBPLDA
00556      MOVE GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                  ELUBPLDA
00557        TO ATBL-COST-CONTAIN-IND (ATBL-TBL-CNT).                   ELUBPLDA
00558      MOVE GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)                    ELUBPLDA
00559        TO ATBL-DAY-FACTOR-IND (ATBL-TBL-CNT).                     ELUBPLDA
00560      MOVE GAB-COINS-DEFINITION (GAB-INDEX)                        ELUBPLDA
00561        TO ATBL-DEFINITION (ATBL-TBL-CNT).                         ELUBPLDA
00562      MOVE GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                      ELUBPLDA
00563        TO ATBL-FAM-OR-INDIV (ATBL-TBL-CNT).                       ELUBPLDA
00564      MOVE GAB-COINS-FYI-VALUE (GAB-INDEX)                         ELUBPLDA
00565        TO ATBL-FYI-VALUE (ATBL-TBL-CNT).                          ELUBPLDA
00566      MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)               ELUBPLDA
00567        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-TBL-CNT).                ELUBPLDA
00568      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)                 ELUBPLDA
00569        TO ATBL-INTERVAL-OVRD-IND (ATBL-TBL-CNT).                  ELUBPLDA
00570      MOVE GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)               ELUBPLDA
00571        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-TBL-CNT).                ELUBPLDA
00572      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)                ELUBPLDA
00573        TO ATBL-INTERVAL-TIME-FCTR (ATBL-TBL-CNT).                 ELUBPLDA
00574      MOVE GAB-COINS-INTERVAL-TYPE (GAB-INDEX)                     ELUBPLDA
00575        TO ATBL-INTERVAL-TYPE (ATBL-TBL-CNT).                      ELUBPLDA
00576      MOVE GAB-COINS-L-O-B (GAB-INDEX)                             ELUBPLDA
00577        TO ATBL-L-O-B (ATBL-TBL-CNT).                              ELUBPLDA
00578      MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                     ELUBPLDA
00579        TO ATBL-PERCENT-LEVEL (ATBL-TBL-CNT).                      ELUBPLDA
00580      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                ELUBPLDA
00581        TO ATBL-PLACE-OF-TREATMENT (ATBL-TBL-CNT).                 ELUBPLDA
00582      MOVE GAB-COINS-REINSTATEMENT-IND (GAB-INDEX)                 ELUBPLDA
00583        TO ATBL-REINSTATEMENT-IND  (ATBL-TBL-CNT).                 ELUBPLDA
00584      MOVE GAB-COINS-SERVICE-GROUP (GAB-INDEX)                     ELUBPLDA
00585        TO ATBL-SERVICE-GROUP (ATBL-TBL-CNT).                      ELUBPLDA
00586      MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                       ELUBPLDA
00587        TO ATBL-VALUE-LIMIT (ATBL-TBL-CNT).                        ELUBPLDA
00588      MOVE GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                   ELUBPLDA
00589        TO ATBL-VALUE-QUALIFIER (ATBL-TBL-CNT).                    ELUBPLDA
00590                                                                   ELUBPLDA
00591 /***********************************************************      ELUBPLDA
00592 *                                                          *      ELUBPLDA
00593 *    LOAD ACL INTERNAL TABULARS DATA                       *      ELUBPLDA
00594 *                                                          *      ELUBPLDA
00595 ************************************************************      ELUBPLDA
00596                                                                   ELUBPLDA
00597  322-LOAD-ACL-INTERNAL-TABS.                                      ELUBPLDA
00598      IF     GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX) = PC-IBGR       ELUBPLDA
00599         AND GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > ZERO        ELUBPLDA
00600      THEN                                                         ELUBPLDA
00601         MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)              ELUBPLDA
00602           TO ATBL-IBGR-SLOT-NUMBER (ATBL-TBL-CNT)                 ELUBPLDA
00603              WS-IBGR-SLOT-NBR                                     ELUBPLDA
00604         PERFORM 530-LOAD-IBGR-TABULAR                             ELUBPLDA
00605      ELSE                                                         ELUBPLDA
00606         IF     GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX) = PC-IDGD    ELUBPLDA
00607            AND GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > ZERO     ELUBPLDA
00608         THEN                                                      ELUBPLDA
00609            MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)           ELUBPLDA
00610              TO ATBL-IDGD-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00611                 WS-IDGD-SLOT-NBR                                  ELUBPLDA
00612            PERFORM 540-LOAD-IDGD-TABULAR                          ELUBPLDA
00613      ELSE                                                         ELUBPLDA
00614         IF     GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX) = PC-IPGN    ELUBPLDA
00615            AND GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > ZERO     ELUBPLDA
00616         THEN                                                      ELUBPLDA
00617            MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)           ELUBPLDA
00618              TO ATBL-IPGN-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00619                 WS-IPGN-SLOT-NBR                                  ELUBPLDA
00620            PERFORM 550-LOAD-IPGN-TABULAR                          ELUBPLDA
00621      ELSE                                                         ELUBPLDA
00622         IF     GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX) = PC-IPGP    ELUBPLDA
00623            AND GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > ZERO     ELUBPLDA
00624         THEN                                                      ELUBPLDA
00625            MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)           ELUBPLDA
00626              TO ATBL-IPGP-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00627                 WS-IPGP-SLOT-NBR                                  ELUBPLDA
00628            PERFORM 560-LOAD-IPGP-TABULAR                          ELUBPLDA
00629      ELSE                                                         ELUBPLDA
00630         IF     GAB-INT-ID (GAB-INDEX, GAB-INT-INDEX) = PC-IPGT    ELUBPLDA
00631             AND GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX) > ZERO    ELUBPLDA
00632         THEN                                                      ELUBPLDA
00633            MOVE GAB-INT-SLOT (GAB-INDEX, GAB-INT-INDEX)           ELUBPLDA
00634              TO ATBL-IPGT-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00635                 WS-IPGT-SLOT-NBR                                  ELUBPLDA
00636            PERFORM 570-LOAD-IPGT-TABULAR                          ELUBPLDA
00637      ELSE                                                         ELUBPLDA
00638         CONTINUE.                                                 ELUBPLDA
00639                                                                   ELUBPLDA
00640 /***********************************************************      ELUBPLDA
00641 *                                                          *      ELUBPLDA
00642 *    PROCESS DEDUCTIBLE ACCUMULATORS                       *      ELUBPLDA
00643 *                                                          *      ELUBPLDA
00644 ************************************************************      ELUBPLDA
00645                                                                   ELUBPLDA
00646  400-PROC-DEDUCTIBLE.                                             ELUBPLDA
00647                                                                   ELUBPLDA
00648      IF CSAC-ADL-BP-TBL-PTR = NULL                                ELUBPLDA
00649      THEN                                                         ELUBPLDA
00650         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00651         PERFORM 500-ACQ-STG-ELSATBL                               ELUBPLDA
00652         CALL 'ELUSETAD'                                           ELUBPLDA
00653            USING DFHCOMMAREA                                      ELUBPLDA
00654                  ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUBPLDA
00655         IF CIA-RC-PTR-NULL                                        ELUBPLDA
00656         THEN                                                      ELUBPLDA
00657            PERFORM 991-SIGNAL-UNALLOC-AREA                        ELUBPLDA
00658         ELSE                                                      ELUBPLDA
00659            SET CSAC-ADL-BP-TBL-PTR                                ELUBPLDA
00660             TO ADDRESS OF ATBL-ACCUMULATOR-TABLE                  ELUBPLDA
00661      ELSE                                                         ELUBPLDA
00662         SET CIA-ELSATBL-DDN TO TRUE                               ELUBPLDA
00663         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELUBPLDA
00664          TO CSAC-ADL-BP-TBL-PTR                                   ELUBPLDA
00665         CALL 'ELUSAVAD'                                           ELUBPLDA
00666            USING DFHCOMMAREA                                      ELUBPLDA
00667                  ADDRESS OF ATBL-ACCUMULATOR-TABLE.               ELUBPLDA
00668                                                                   ELUBPLDA
00669      MOVE CSBP-BP-ADL-SLOT (WS-SUB2) TO WS-ACCUM-SLOT-NBR.        ELUBPLDA
00670      PERFORM 510-CHK-SLOT-ALREADY-LOADED.                         ELUBPLDA
00671      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
00672         PERFORM 410-OBTAIN-ADL-RECORD                             ELUBPLDA
00673         PERFORM 420-BUILD-ADL-ELSATBLC                            ELUBPLDA
00674            VARYING GAC-INDEX FROM 1 BY 1                          ELUBPLDA
00675              UNTIL    GAC-INDEX = GAC-ENTRY-COUNT                 ELUBPLDA
00676                    OR GAC-ENTRY (GAC-INDEX) = HIGH-VALUES         ELUBPLDA
00677      ELSE                                                         ELUBPLDA
00678         CONTINUE.                                                 ELUBPLDA
00679                                                                   ELUBPLDA
00680 /***********************************************************      ELUBPLDA
00681 *                                                          *      ELUBPLDA
00682 *    OBTAIN ADL TABULAR RECORD AT BENEFIT PROVISION LEVEL  *      ELUBPLDA
00683 *                                                          *      ELUBPLDA
00684 ************************************************************      ELUBPLDA
00685                                                                   ELUBPLDA
00686  410-OBTAIN-ADL-RECORD.                                           ELUBPLDA
00687      MOVE PC-ADL TO KWA-PROVISION-ID.                             ELUBPLDA
00688      MOVE CSBP-BP-ADL-SLOT (WS-SUB2) TO KWA-PROVISION-SLOT-NO.    ELUBPLDA
00689      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
00690      SET ADDRESS OF DEDUCTIBLE-RECORD TO IOP-REC-PTR.             ELUBPLDA
00691                                                                   ELUBPLDA
00692 ************************************************************      ELUBPLDA
00693 *                                                          *      ELUBPLDA
00694 *    BUILD DEDUCTIBLE ACCUMULATOR TABLE                    *      ELUBPLDA
00695 *                                                          *      ELUBPLDA
00696 ************************************************************      ELUBPLDA
00697                                                                   ELUBPLDA
00698  420-BUILD-ADL-ELSATBLC.                                          ELUBPLDA
00699      IF GAC-ENTRY (GAC-INDEX) NOT = HIGH-VALUES                   ELUBPLDA
00700      THEN                                                         ELUBPLDA
00701         PERFORM 520-INIT-NEXT-ATBL-ENTRY                          ELUBPLDA
00702         PERFORM 421-LOAD-ADL-DATA-ELEMENTS                        ELUBPLDA
00703         PERFORM 422-LOAD-ADL-INTERNAL-TABS                        ELUBPLDA
00704            VARYING GAC-INT-INDEX FROM 1 BY 1                      ELUBPLDA
00705              UNTIL GAC-INT-INDEX >                                ELUBPLDA
00706                 GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX).           ELUBPLDA
00707                                                                   ELUBPLDA
00708 /***********************************************************      ELUBPLDA
00709 *                                                          *      ELUBPLDA
00710 *    LOAD ADL DATA ELEMENTS INTO ATBL TABLE ENTRY          *      ELUBPLDA
00711 *                                                          *      ELUBPLDA
00712 ************************************************************      ELUBPLDA
00713                                                                   ELUBPLDA
00714  421-LOAD-ADL-DATA-ELEMENTS.                                      ELUBPLDA
00715      MOVE GAC-PROVISION-SLOT-NO                                   ELUBPLDA
00716        TO ATBL-SLOT-NUMBER (ATBL-TBL-CNT).                        ELUBPLDA
00717      MOVE GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)                  ELUBPLDA
00718        TO ATBL-BEN-PER-TIME-FCTR (ATBL-TBL-CNT).                  ELUBPLDA
00719      MOVE GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)                  ELUBPLDA
00720        TO ATBL-BEN-PER-TIME-QUAL (ATBL-TBL-CNT).                  ELUBPLDA
00721      MOVE GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     ELUBPLDA
00722        TO ATBL-BENEFIT-PERIOD (ATBL-TBL-CNT).                     ELUBPLDA
00723      MOVE GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX)                   ELUBPLDA
00724        TO ATBL-CARRY-OVER-CREDIT-IND (ATBL-TBL-CNT).              ELUBPLDA
00725      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)                ELUBPLDA
00726        TO ATBL-CLAIM-LVL-ACCUM-IND (ATBL-TBL-CNT).                ELUBPLDA
00727      MOVE GAC-DEDL-CO-PAY-IND (GAC-INDEX)                         ELUBPLDA
00728        TO ATBL-CO-PAY-IND (ATBL-TBL-CNT).                         ELUBPLDA
00729      MOVE GAC-DEDL-CONDITION (GAC-INDEX)                          ELUBPLDA
00730        TO ATBL-CONDITION (ATBL-TBL-CNT).                          ELUBPLDA
00731      MOVE GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                   ELUBPLDA
00732        TO ATBL-COST-CONTAIN-IND (ATBL-TBL-CNT).                   ELUBPLDA
00733      MOVE GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)                     ELUBPLDA
00734        TO ATBL-DAY-FACTOR-IND (ATBL-TBL-CNT).                     ELUBPLDA
00735      MOVE GAC-DEDL-DEFINITION (GAC-INDEX)                         ELUBPLDA
00736        TO ATBL-DEFINITION (ATBL-TBL-CNT).                         ELUBPLDA
00737      MOVE GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                       ELUBPLDA
00738        TO ATBL-FAM-OR-INDIV (ATBL-TBL-CNT).                       ELUBPLDA
00739      MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX)                          ELUBPLDA
00740        TO ATBL-FYI-VALUE (ATBL-TBL-CNT).                          ELUBPLDA
00741      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                ELUBPLDA
00742        TO ATBL-INTERNAL-DESCRIPTOR (ATBL-TBL-CNT).                ELUBPLDA
00743      MOVE GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)                  ELUBPLDA
00744        TO ATBL-INTERVAL-OVRD-IND (ATBL-TBL-CNT).                  ELUBPLDA
00745      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)                ELUBPLDA
00746        TO ATBL-INTERVAL-OVRD-VALUE (ATBL-TBL-CNT).                ELUBPLDA
00747      MOVE GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)                 ELUBPLDA
00748        TO ATBL-INTERVAL-TIME-FCTR (ATBL-TBL-CNT).                 ELUBPLDA
00749      MOVE GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)                      ELUBPLDA
00750        TO ATBL-INTERVAL-TYPE (ATBL-TBL-CNT).                      ELUBPLDA
00751      MOVE GAC-DEDL-L-O-B (GAC-INDEX)                              ELUBPLDA
00752        TO ATBL-L-O-B (ATBL-TBL-CNT).                              ELUBPLDA
00753      MOVE GAC-DEDL-MANDATORY-IND (GAC-INDEX)                      ELUBPLDA
00754        TO ATBL-MANDATORY-IND (ATBL-TBL-CNT).                      ELUBPLDA
00755      MOVE GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                 ELUBPLDA
00756        TO ATBL-PLACE-OF-TREATMENT (ATBL-TBL-CNT).                 ELUBPLDA
00757      MOVE GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                      ELUBPLDA
00758        TO ATBL-SERVICE-GROUP (ATBL-TBL-CNT).                      ELUBPLDA
00759      MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                        ELUBPLDA
00760        TO ATBL-VALUE-LIMIT (ATBL-TBL-CNT).                        ELUBPLDA
00761      MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                    ELUBPLDA
00762        TO ATBL-VALUE-QUALIFIER (ATBL-TBL-CNT).                    ELUBPLDA
00763                                                                   ELUBPLDA
00764 /***********************************************************      ELUBPLDA
00765 *                                                          *      ELUBPLDA
00766 *    LOAD ADL INTERNAL TABULARS DATA                       *      ELUBPLDA
00767 *                                                          *      ELUBPLDA
00768 ************************************************************      ELUBPLDA
00769                                                                   ELUBPLDA
00770  422-LOAD-ADL-INTERNAL-TABS.                                      ELUBPLDA
00771      IF     GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX) = PC-IBGR       ELUBPLDA
00772         AND GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > ZERO        ELUBPLDA
00773      THEN                                                         ELUBPLDA
00774         MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)              ELUBPLDA
00775           TO ATBL-IBGR-SLOT-NUMBER (ATBL-TBL-CNT)                 ELUBPLDA
00776              WS-IBGR-SLOT-NBR                                     ELUBPLDA
00777         PERFORM 530-LOAD-IBGR-TABULAR                             ELUBPLDA
00778      ELSE                                                         ELUBPLDA
00779         IF     GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX) = PC-IDGD    ELUBPLDA
00780            AND GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > ZERO     ELUBPLDA
00781         THEN                                                      ELUBPLDA
00782            MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)           ELUBPLDA
00783              TO ATBL-IDGD-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00784                 WS-IDGD-SLOT-NBR                                  ELUBPLDA
00785            PERFORM 540-LOAD-IDGD-TABULAR                          ELUBPLDA
00786      ELSE                                                         ELUBPLDA
00787         IF     GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX) = PC-IPGN    ELUBPLDA
00788            AND GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > ZERO     ELUBPLDA
00789         THEN                                                      ELUBPLDA
00790            MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)           ELUBPLDA
00791              TO ATBL-IPGN-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00792                 WS-IPGN-SLOT-NBR                                  ELUBPLDA
00793            PERFORM 550-LOAD-IPGN-TABULAR                          ELUBPLDA
00794      ELSE                                                         ELUBPLDA
00795         IF     GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX) = PC-IPGP    ELUBPLDA
00796            AND GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > ZERO     ELUBPLDA
00797         THEN                                                      ELUBPLDA
00798            MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)           ELUBPLDA
00799              TO ATBL-IPGP-SLOT-NUMBER (ATBL-TBL-CNT)              ELUBPLDA
00800                 WS-IPGP-SLOT-NBR                                  ELUBPLDA
00801            PERFORM 560-LOAD-IPGP-TABULAR                          ELUBPLDA
00802      ELSE                                                         ELUBPLDA
00803         IF     GAC-INT-ID (GAC-INDEX, GAC-INT-INDEX) = PC-IPGT    ELUBPLDA
00804            AND GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX) > ZERO     ELUBPLDA
00805         THEN                                                      ELUBPLDA
00806               MOVE GAC-INT-SLOT (GAC-INDEX, GAC-INT-INDEX)        ELUBPLDA
00807                 TO ATBL-IPGT-SLOT-NUMBER (ATBL-TBL-CNT)           ELUBPLDA
00808                    WS-IPGT-SLOT-NBR                               ELUBPLDA
00809                PERFORM 570-LOAD-IPGT-TABULAR                      ELUBPLDA
00810         ELSE                                                      ELUBPLDA
00811              CONTINUE.                                            ELUBPLDA
00812                                                                   ELUBPLDA
00813 /***********************************************************      ELUBPLDA
00814 *                                                          *      ELUBPLDA
00815 *    ACQUIRE STORAGE FOR ELSATBL                           *      ELUBPLDA
00816 *                                                          *      ELUBPLDA
00817 ************************************************************      ELUBPLDA
00818                                                                   ELUBPLDA
00819  500-ACQ-STG-ELSATBL.                                             ELUBPLDA
00820      SET  CIA-ELSATBL-DDN TO TRUE.                                ELUBPLDA
00821 *    CALL 'ELUSAVAD'                                              ELUBPLDA
00822 *       USING DFHCOMMAREA                                         ELUBPLDA
00823 *             ADDRESS OF ATBL-ACCUMULATOR-TABLE.                  ELUBPLDA
00824      CALL 'ELUSETAD'                                              ELUBPLDA
00825         USING DFHCOMMAREA                                         ELUBPLDA
00826               ADDRESS OF ATBL-ACCUMULATOR-TABLE.                  ELUBPLDA
00827      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
00828           LENGTH OF ATBL-TBL-CNT                                  ELUBPLDA
00829         + (CIA-MVO * LENGTH OF ATBL-ACCUMULATOR).                 ELUBPLDA
00830      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
00831      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
00832      SET  CIA-ELSATBL-DDN TO TRUE.                                ELUBPLDA
00833      CALL 'ELUSETAD'                                              ELUBPLDA
00834         USING DFHCOMMAREA                                         ELUBPLDA
00835               ADDRESS OF ATBL-ACCUMULATOR-TABLE.                  ELUBPLDA
00836      IF CIA-RC-PTR-NULL                                           ELUBPLDA
00837      THEN                                                         ELUBPLDA
00838         PERFORM 991-SIGNAL-UNALLOC-AREA                           ELUBPLDA
00839      ELSE                                                         ELUBPLDA
00840         INITIALIZE ATBL-TBL-CNT.                                  ELUBPLDA
00841                                                                   ELUBPLDA
00842 ************************************************************      ELUBPLDA
00843 *                                                          *      ELUBPLDA
00844 *    CHECK FOR SLOT NUMBER ALREADY LOADED                  *      ELUBPLDA
00845 *                                                          *      ELUBPLDA
00846 ************************************************************      ELUBPLDA
00847                                                                   ELUBPLDA
00848  510-CHK-SLOT-ALREADY-LOADED.                                     ELUBPLDA
00849      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
00850      PERFORM WITH TEST BEFORE                                     ELUBPLDA
00851         VARYING WS-SUB4 FROM 1 BY 1                               ELUBPLDA
00852           UNTIL    WS-SUB4 > ATBL-TBL-CNT                         ELUBPLDA
00853                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
00854         IF WS-ACCUM-SLOT-NBR = ATBL-SLOT-NUMBER (WS-SUB4)         ELUBPLDA
00855         THEN                                                      ELUBPLDA
00856            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
00857         ELSE                                                      ELUBPLDA
00858            CONTINUE                                               ELUBPLDA
00859         END-IF                                                    ELUBPLDA
00860         END-PERFORM.                                              ELUBPLDA
00861                                                                   ELUBPLDA
00862 ************************************************************      ELUBPLDA
00863 *                                                          *      ELUBPLDA
00864 *    INITIALIZE THE NEXT ATBL TABLE ENTRY                  *      ELUBPLDA
00865 *                                                          *      ELUBPLDA
00866 ************************************************************      ELUBPLDA
00867                                                                   ELUBPLDA
00868  520-INIT-NEXT-ATBL-ENTRY.                                        ELUBPLDA
00869      ADD +1 TO ATBL-TBL-CNT.                                      ELUBPLDA
00870      INITIALIZE ATBL-SLOT-NUMBER (ATBL-TBL-CNT)                   ELUBPLDA
00871                 ATBL-INTERNAL-TABULARS (ATBL-TBL-CNT)             ELUBPLDA
00872                 ATBL-DATA-ELEMENTS (ATBL-TBL-CNT)                 ELUBPLDA
00873                 ATBL-ATTR-CONFIDENCE-FACTORS (ATBL-TBL-CNT)       ELUBPLDA
00874                 ATBL-CF-WORK-ENTRY (ATBL-TBL-CNT)                 ELUBPLDA
00875                 ATBL-KEY-LINKAGE (ATBL-TBL-CNT).                  ELUBPLDA
00876                                                                   ELUBPLDA
00877 /***********************************************************      ELUBPLDA
00878 *                                                          *      ELUBPLDA
00879 *    LOAD IBGR INTERNAL TABULAR INTO STORAGE               *      ELUBPLDA
00880 *                                                          *      ELUBPLDA
00881 ************************************************************      ELUBPLDA
00882                                                                   ELUBPLDA
00883  530-LOAD-IBGR-TABULAR.                                           ELUBPLDA
00884      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUBPLDA
00885      CALL 'ELUSETAD'                                              ELUBPLDA
00886          USING DFHCOMMAREA                                        ELUBPLDA
00887                ADDRESS OF IBGR-INTERNAL-TABS-TABLE.               ELUBPLDA
00888      IF CIA-RC-PTR-NULL                                           ELUBPLDA
00889      THEN                                                         ELUBPLDA
00890         PERFORM 531-ACQ-STG-IBGR-PTR-TBL                          ELUBPLDA
00891      ELSE                                                         ELUBPLDA
00892         MOVE CIA-MVO TO WS-IBGR-TBL-MAX                           ELUBPLDA
00893                                                                   ELUBPLDA
00894      PERFORM 532-CHK-IBGR-ALREADY-LOADED.                         ELUBPLDA
00895      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
00896      THEN                                                         ELUBPLDA
00897         IF IBGR-TBL-CNT < WS-IBGR-TBL-MAX                         ELUBPLDA
00898         THEN                                                      ELUBPLDA
00899            PERFORM 533-LOAD-IBGR                                  ELUBPLDA
00900         ELSE                                                      ELUBPLDA
00901            SET CIA-AB-INCR-TBL-SIZE TO TRUE                       ELUBPLDA
00902            PERFORM 999-SIGNAL-ABEND                               ELUBPLDA
00903      ELSE                                                         ELUBPLDA
00904         CONTINUE.                                                 ELUBPLDA
00905                                                                   ELUBPLDA
00906 ************************************************************      ELUBPLDA
00907 *                                                          *      ELUBPLDA
00908 *    ACQUIRE STORAGE FOR THE IBGR POINTER TABLE            *      ELUBPLDA
00909 *                                                          *      ELUBPLDA
00910 ************************************************************      ELUBPLDA
00911                                                                   ELUBPLDA
00912  531-ACQ-STG-IBGR-PTR-TBL.                                        ELUBPLDA
00913      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELUBPLDA
00914      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
00915           LENGTH OF IBGR-TBL-CNT                                  ELUBPLDA
00916         + (CIA-MVO * LENGTH OF IBGR-INTERNAL-TABS).               ELUBPLDA
00917      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
00918      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
00919                                                                   ELUBPLDA
00920      CALL 'ELUSETAD'                                              ELUBPLDA
00921         USING DFHCOMMAREA                                         ELUBPLDA
00922               ADDRESS OF IBGR-INTERNAL-TABS-TABLE.                ELUBPLDA
00923      IF CIA-RC-PTR-NULL                                           ELUBPLDA
00924      THEN                                                         ELUBPLDA
00925          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUBPLDA
00926      ELSE                                                         ELUBPLDA
00927         MOVE CIA-MVO TO WS-IBGR-TBL-MAX                           ELUBPLDA
00928         INITIALIZE IBGR-TBL-CNT.                                  ELUBPLDA
00929                                                                   ELUBPLDA
00930 /***********************************************************      ELUBPLDA
00931 *                                                          *      ELUBPLDA
00932 *    CHECK TO SEE OF IBGR SLOT NUMBER IS ALREADY LODED     *      ELUBPLDA
00933 *                                                          *      ELUBPLDA
00934 ************************************************************      ELUBPLDA
00935                                                                   ELUBPLDA
00936  532-CHK-IBGR-ALREADY-LOADED.                                     ELUBPLDA
00937      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
00938      PERFORM WITH TEST BEFORE                                     ELUBPLDA
00939         VARYING WS-SUB3 FROM 1 BY 1                               ELUBPLDA
00940           UNTIL    WS-SUB3 > IBGR-TBL-CNT                         ELUBPLDA
00941                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
00942         IF WS-IBGR-SLOT-NBR = IBGR-SLOT-NUMBER (WS-SUB3)          ELUBPLDA
00943         THEN                                                      ELUBPLDA
00944            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
00945         ELSE                                                      ELUBPLDA
00946            CONTINUE                                               ELUBPLDA
00947         END-IF                                                    ELUBPLDA
00948         END-PERFORM.                                              ELUBPLDA
00949                                                                   ELUBPLDA
00950 ************************************************************      ELUBPLDA
00951 *                                                          *      ELUBPLDA
00952 *    LOAD IBGR RECORD INTO MEMORY AND ADD TO POINTER TABLE *      ELUBPLDA
00953 *                                                          *      ELUBPLDA
00954 ************************************************************      ELUBPLDA
00955                                                                   ELUBPLDA
00956  533-LOAD-IBGR.                                                   ELUBPLDA
00957      MOVE PC-IBGR TO KWA-PROVISION-ID.                            ELUBPLDA
00958      MOVE WS-IBGR-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELUBPLDA
00959      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
00960      ADD +1 TO IBGR-TBL-CNT.                                      ELUBPLDA
00961      SET IBGR-TABULAR-PTR (IBGR-TBL-CNT) TO IOP-REC-PTR.          ELUBPLDA
00962      MOVE WS-IBGR-SLOT-NBR TO IBGR-SLOT-NUMBER (IBGR-TBL-CNT).    ELUBPLDA
00963      SET IOP-REC-PTR TO NULLS.                                    ELUBPLDA
00964                                                                   ELUBPLDA
00965 /***********************************************************      ELUBPLDA
00966 *                                                          *      ELUBPLDA
00967 *    LOAD IDGD INTERNAL TABULAR INTO STORAGE               *      ELUBPLDA
00968 *                                                          *      ELUBPLDA
00969 ************************************************************      ELUBPLDA
00970                                                                   ELUBPLDA
00971  540-LOAD-IDGD-TABULAR.                                           ELUBPLDA
00972      SET CIA-ELSIDGD-DDN TO TRUE.                                 ELUBPLDA
00973      CALL 'ELUSETAD'                                              ELUBPLDA
00974          USING DFHCOMMAREA                                        ELUBPLDA
00975                ADDRESS OF IDGD-INTERNAL-TABS-TABLE.               ELUBPLDA
00976      IF CIA-RC-PTR-NULL                                           ELUBPLDA
00977      THEN                                                         ELUBPLDA
00978         PERFORM 541-ACQ-STG-IDGD-PTR-TBL                          ELUBPLDA
00979      ELSE                                                         ELUBPLDA
00980         MOVE CIA-MVO TO WS-IDGD-TBL-MAX                           ELUBPLDA
00981                                                                   ELUBPLDA
00982      PERFORM 542-CHK-IDGD-ALREADY-LOADED.                         ELUBPLDA
00983      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
00984      THEN                                                         ELUBPLDA
00985         IF IDGD-TBL-CNT < WS-IDGD-TBL-MAX                         ELUBPLDA
00986         THEN                                                      ELUBPLDA
00987            PERFORM 543-LOAD-IDGD                                  ELUBPLDA
00988         ELSE                                                      ELUBPLDA
00989            SET CIA-AB-INCR-TBL-SIZE TO TRUE                       ELUBPLDA
00990            PERFORM 999-SIGNAL-ABEND                               ELUBPLDA
00991      ELSE                                                         ELUBPLDA
00992         CONTINUE.                                                 ELUBPLDA
00993                                                                   ELUBPLDA
00994 ************************************************************      ELUBPLDA
00995 *                                                          *      ELUBPLDA
00996 *    ACQUIRE STORAGE FOR THE IDGD POINTER TABLE            *      ELUBPLDA
00997 *                                                          *      ELUBPLDA
00998 ************************************************************      ELUBPLDA
00999                                                                   ELUBPLDA
01000  541-ACQ-STG-IDGD-PTR-TBL.                                        ELUBPLDA
01001      SET CIA-ELSIDGD-DDN TO TRUE.                                 ELUBPLDA
01002      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
01003           LENGTH OF IDGD-TBL-CNT                                  ELUBPLDA
01004         + (CIA-MVO * LENGTH OF IDGD-INTERNAL-TABS).               ELUBPLDA
01005      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
01006      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
01007                                                                   ELUBPLDA
01008      CALL 'ELUSETAD'                                              ELUBPLDA
01009         USING DFHCOMMAREA                                         ELUBPLDA
01010               ADDRESS OF IDGD-INTERNAL-TABS-TABLE.                ELUBPLDA
01011      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01012      THEN                                                         ELUBPLDA
01013          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUBPLDA
01014      ELSE                                                         ELUBPLDA
01015         MOVE CIA-MVO TO WS-IDGD-TBL-MAX                           ELUBPLDA
01016         INITIALIZE IDGD-TBL-CNT.                                  ELUBPLDA
01017                                                                   ELUBPLDA
01018 /***********************************************************      ELUBPLDA
01019 *                                                          *      ELUBPLDA
01020 *    CHECK TO SEE OF IDGD SLOT NUMBER IS ALREADY LODED     *      ELUBPLDA
01021 *                                                          *      ELUBPLDA
01022 ************************************************************      ELUBPLDA
01023                                                                   ELUBPLDA
01024  542-CHK-IDGD-ALREADY-LOADED.                                     ELUBPLDA
01025      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
01026      PERFORM WITH TEST BEFORE                                     ELUBPLDA
01027         VARYING WS-SUB3 FROM 1 BY 1                               ELUBPLDA
01028           UNTIL    WS-SUB3 > IDGD-TBL-CNT                         ELUBPLDA
01029                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
01030         IF WS-IDGD-SLOT-NBR = IDGD-SLOT-NUMBER (WS-SUB3)          ELUBPLDA
01031         THEN                                                      ELUBPLDA
01032            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
01033         ELSE                                                      ELUBPLDA
01034            CONTINUE                                               ELUBPLDA
01035         END-IF                                                    ELUBPLDA
01036         END-PERFORM.                                              ELUBPLDA
01037                                                                   ELUBPLDA
01038 ************************************************************      ELUBPLDA
01039 *                                                          *      ELUBPLDA
01040 *    LOAD IDGD RECORD INTO MEMORY AND ADD TO POINTER TABLE *      ELUBPLDA
01041 *                                                          *      ELUBPLDA
01042 ************************************************************      ELUBPLDA
01043                                                                   ELUBPLDA
01044  543-LOAD-IDGD.                                                   ELUBPLDA
01045      MOVE PC-IDGD TO KWA-PROVISION-ID.                            ELUBPLDA
01046      MOVE WS-IDGD-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELUBPLDA
01047      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
01048      ADD +1 TO IDGD-TBL-CNT.                                      ELUBPLDA
01049      SET IDGD-TABULAR-PTR (IDGD-TBL-CNT) TO IOP-REC-PTR.          ELUBPLDA
01050      MOVE WS-IDGD-SLOT-NBR TO IDGD-SLOT-NUMBER (IDGD-TBL-CNT).    ELUBPLDA
01051      SET IOP-REC-PTR TO NULLS.                                    ELUBPLDA
01052                                                                   ELUBPLDA
01053 /***********************************************************      ELUBPLDA
01054 *                                                          *      ELUBPLDA
01055 *    LOAD IPGN INTERNAL TABULAR INTO STORAGE               *      ELUBPLDA
01056 *                                                          *      ELUBPLDA
01057 ************************************************************      ELUBPLDA
01058                                                                   ELUBPLDA
01059  550-LOAD-IPGN-TABULAR.                                           ELUBPLDA
01060      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELUBPLDA
01061      CALL 'ELUSETAD'                                              ELUBPLDA
01062          USING DFHCOMMAREA                                        ELUBPLDA
01063                ADDRESS OF IPGN-INTERNAL-TABS-TABLE.               ELUBPLDA
01064      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01065      THEN                                                         ELUBPLDA
01066         PERFORM 551-ACQ-STG-IPGN-PTR-TBL                          ELUBPLDA
01067      ELSE                                                         ELUBPLDA
01068         MOVE CIA-MVO TO WS-IPGN-TBL-MAX                           ELUBPLDA
01069                                                                   ELUBPLDA
01070      PERFORM 552-CHK-IPGN-ALREADY-LOADED.                         ELUBPLDA
01071      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
01072      THEN                                                         ELUBPLDA
01073         IF IPGN-TBL-CNT < WS-IPGN-TBL-MAX                         ELUBPLDA
01074         THEN                                                      ELUBPLDA
01075            PERFORM 553-LOAD-IPGN                                  ELUBPLDA
01076         ELSE                                                      ELUBPLDA
01077            SET CIA-AB-INCR-TBL-SIZE TO TRUE                       ELUBPLDA
01078            PERFORM 999-SIGNAL-ABEND                               ELUBPLDA
01079      ELSE                                                         ELUBPLDA
01080         CONTINUE.                                                 ELUBPLDA
01081                                                                   ELUBPLDA
01082 ************************************************************      ELUBPLDA
01083 *                                                          *      ELUBPLDA
01084 *    ACQUIRE STORAGE FOR THE IPGN POINTER TABLE            *      ELUBPLDA
01085 *                                                          *      ELUBPLDA
01086 ************************************************************      ELUBPLDA
01087                                                                   ELUBPLDA
01088  551-ACQ-STG-IPGN-PTR-TBL.                                        ELUBPLDA
01089      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELUBPLDA
01090      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
01091           LENGTH OF IPGN-TBL-CNT                                  ELUBPLDA
01092         + (CIA-MVO * LENGTH OF IPGN-INTERNAL-TABS).               ELUBPLDA
01093      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
01094      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
01095                                                                   ELUBPLDA
01096      CALL 'ELUSETAD'                                              ELUBPLDA
01097         USING DFHCOMMAREA                                         ELUBPLDA
01098               ADDRESS OF IPGN-INTERNAL-TABS-TABLE.                ELUBPLDA
01099      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01100      THEN                                                         ELUBPLDA
01101          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUBPLDA
01102      ELSE                                                         ELUBPLDA
01103         MOVE CIA-MVO TO WS-IPGN-TBL-MAX                           ELUBPLDA
01104         INITIALIZE IPGN-TBL-CNT.                                  ELUBPLDA
01105                                                                   ELUBPLDA
01106 /***********************************************************      ELUBPLDA
01107 *                                                          *      ELUBPLDA
01108 *    CHECK TO SEE OF IPGN SLOT NUMBER IS ALREADY LODED     *      ELUBPLDA
01109 *                                                          *      ELUBPLDA
01110 ************************************************************      ELUBPLDA
01111                                                                   ELUBPLDA
01112  552-CHK-IPGN-ALREADY-LOADED.                                     ELUBPLDA
01113      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
01114      PERFORM WITH TEST BEFORE                                     ELUBPLDA
01115         VARYING WS-SUB3 FROM 1 BY 1                               ELUBPLDA
01116           UNTIL    WS-SUB3 > IPGN-TBL-CNT                         ELUBPLDA
01117                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
01118         IF WS-IPGN-SLOT-NBR = IPGN-SLOT-NUMBER (WS-SUB3)          ELUBPLDA
01119         THEN                                                      ELUBPLDA
01120            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
01121         ELSE                                                      ELUBPLDA
01122            CONTINUE                                               ELUBPLDA
01123         END-IF                                                    ELUBPLDA
01124         END-PERFORM.                                              ELUBPLDA
01125                                                                   ELUBPLDA
01126 ************************************************************      ELUBPLDA
01127 *                                                          *      ELUBPLDA
01128 *    LOAD IPGN RECORD INTO MEMORY AND ADD TO POINTER TABLE *      ELUBPLDA
01129 *                                                          *      ELUBPLDA
01130 ************************************************************      ELUBPLDA
01131                                                                   ELUBPLDA
01132  553-LOAD-IPGN.                                                   ELUBPLDA
01133      MOVE PC-IPGN TO KWA-PROVISION-ID.                            ELUBPLDA
01134      MOVE WS-IPGN-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELUBPLDA
01135      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
01136      ADD +1 TO IPGN-TBL-CNT.                                      ELUBPLDA
01137      SET IPGN-TABULAR-PTR (IPGN-TBL-CNT) TO IOP-REC-PTR.          ELUBPLDA
01138      MOVE WS-IPGN-SLOT-NBR TO IPGN-SLOT-NUMBER (IPGN-TBL-CNT).    ELUBPLDA
01139      SET IOP-REC-PTR TO NULLS.                                    ELUBPLDA
01140                                                                   ELUBPLDA
01141 /***********************************************************      ELUBPLDA
01142 *                                                          *      ELUBPLDA
01143 *    LOAD IPGP INTERNAL TABULAR INTO STORAGE               *      ELUBPLDA
01144 *                                                          *      ELUBPLDA
01145 ************************************************************      ELUBPLDA
01146                                                                   ELUBPLDA
01147  560-LOAD-IPGP-TABULAR.                                           ELUBPLDA
01148      SET CIA-ELSIPGP-DDN TO TRUE.                                 ELUBPLDA
01149      CALL 'ELUSETAD'                                              ELUBPLDA
01150          USING DFHCOMMAREA                                        ELUBPLDA
01151                ADDRESS OF IPGP-INTERNAL-TABS-TABLE.               ELUBPLDA
01152      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01153      THEN                                                         ELUBPLDA
01154         PERFORM 561-ACQ-STG-IPGP-PTR-TBL                          ELUBPLDA
01155      ELSE                                                         ELUBPLDA
01156         MOVE CIA-MVO TO WS-IPGP-TBL-MAX                           ELUBPLDA
01157                                                                   ELUBPLDA
01158      PERFORM 562-CHK-IPGP-ALREADY-LOADED.                         ELUBPLDA
01159      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
01160      THEN                                                         ELUBPLDA
01161         IF IPGP-TBL-CNT < WS-IPGP-TBL-MAX                         ELUBPLDA
01162         THEN                                                      ELUBPLDA
01163            PERFORM 563-LOAD-IPGP                                  ELUBPLDA
01164         ELSE                                                      ELUBPLDA
01165            SET CIA-AB-INCR-TBL-SIZE TO TRUE                       ELUBPLDA
01166            PERFORM 999-SIGNAL-ABEND                               ELUBPLDA
01167      ELSE                                                         ELUBPLDA
01168         CONTINUE.                                                 ELUBPLDA
01169                                                                   ELUBPLDA
01170 ************************************************************      ELUBPLDA
01171 *                                                          *      ELUBPLDA
01172 *    ACQUIRE STORAGE FOR THE IPGP POINTER TABLE            *      ELUBPLDA
01173 *                                                          *      ELUBPLDA
01174 ************************************************************      ELUBPLDA
01175                                                                   ELUBPLDA
01176  561-ACQ-STG-IPGP-PTR-TBL.                                        ELUBPLDA
01177      SET CIA-ELSIPGP-DDN TO TRUE.                                 ELUBPLDA
01178      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
01179           LENGTH OF IPGP-TBL-CNT                                  ELUBPLDA
01180         + (CIA-MVO * LENGTH OF IPGP-INTERNAL-TABS).               ELUBPLDA
01181      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
01182      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
01183                                                                   ELUBPLDA
01184      CALL 'ELUSETAD'                                              ELUBPLDA
01185         USING DFHCOMMAREA                                         ELUBPLDA
01186               ADDRESS OF IPGP-INTERNAL-TABS-TABLE.                ELUBPLDA
01187      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01188      THEN                                                         ELUBPLDA
01189          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUBPLDA
01190      ELSE                                                         ELUBPLDA
01191         MOVE CIA-MVO TO WS-IPGP-TBL-MAX                           ELUBPLDA
01192         INITIALIZE IPGP-TBL-CNT.                                  ELUBPLDA
01193                                                                   ELUBPLDA
01194 /***********************************************************      ELUBPLDA
01195 *                                                          *      ELUBPLDA
01196 *    CHECK TO SEE OF IPGP SLOT NUMBER IS ALREADY LODED     *      ELUBPLDA
01197 *                                                          *      ELUBPLDA
01198 ************************************************************      ELUBPLDA
01199                                                                   ELUBPLDA
01200  562-CHK-IPGP-ALREADY-LOADED.                                     ELUBPLDA
01201      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
01202      PERFORM WITH TEST BEFORE                                     ELUBPLDA
01203         VARYING WS-SUB3 FROM 1 BY 1                               ELUBPLDA
01204           UNTIL    WS-SUB3 > IPGP-TBL-CNT                         ELUBPLDA
01205                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
01206         IF WS-IPGP-SLOT-NBR = IPGP-SLOT-NUMBER (WS-SUB3)          ELUBPLDA
01207         THEN                                                      ELUBPLDA
01208            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
01209         ELSE                                                      ELUBPLDA
01210            CONTINUE                                               ELUBPLDA
01211         END-IF                                                    ELUBPLDA
01212         END-PERFORM.                                              ELUBPLDA
01213                                                                   ELUBPLDA
01214 ************************************************************      ELUBPLDA
01215 *                                                          *      ELUBPLDA
01216 *    LOAD IPGP RECORD INTO MEMORY AND ADD TO POINTER TABLE *      ELUBPLDA
01217 *                                                          *      ELUBPLDA
01218 ************************************************************      ELUBPLDA
01219                                                                   ELUBPLDA
01220  563-LOAD-IPGP.                                                   ELUBPLDA
01221      MOVE PC-IPGP TO KWA-PROVISION-ID.                            ELUBPLDA
01222      MOVE WS-IPGP-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELUBPLDA
01223      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
01224      ADD +1 TO IPGP-TBL-CNT.                                      ELUBPLDA
01225      SET IPGP-TABULAR-PTR (IPGP-TBL-CNT) TO IOP-REC-PTR.          ELUBPLDA
01226      MOVE WS-IPGP-SLOT-NBR TO IPGP-SLOT-NUMBER (IPGP-TBL-CNT).    ELUBPLDA
01227      SET IOP-REC-PTR TO NULLS.                                    ELUBPLDA
01228                                                                   ELUBPLDA
01229 /***********************************************************      ELUBPLDA
01230 *                                                          *      ELUBPLDA
01231 *    LOAD IPGT INTERNAL TABULAR INTO STORAGE               *      ELUBPLDA
01232 *                                                          *      ELUBPLDA
01233 ************************************************************      ELUBPLDA
01234                                                                   ELUBPLDA
01235  570-LOAD-IPGT-TABULAR.                                           ELUBPLDA
01236      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELUBPLDA
01237      CALL 'ELUSETAD'                                              ELUBPLDA
01238          USING DFHCOMMAREA                                        ELUBPLDA
01239                ADDRESS OF IPGT-INTERNAL-TABS-TABLE.               ELUBPLDA
01240      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01241      THEN                                                         ELUBPLDA
01242         PERFORM 571-ACQ-STG-IPGT-PTR-TBL                          ELUBPLDA
01243      ELSE                                                         ELUBPLDA
01244         MOVE CIA-MVO TO WS-IPGT-TBL-MAX                           ELUBPLDA
01245                                                                   ELUBPLDA
01246      PERFORM 572-CHK-IPGT-ALREADY-LOADED.                         ELUBPLDA
01247      IF UNIQUE-SLOT-NBR                                           ELUBPLDA
01248      THEN                                                         ELUBPLDA
01249         IF IPGT-TBL-CNT < WS-IPGT-TBL-MAX                         ELUBPLDA
01250         THEN                                                      ELUBPLDA
01251            PERFORM 573-LOAD-IPGT                                  ELUBPLDA
01252         ELSE                                                      ELUBPLDA
01253            SET CIA-AB-INCR-TBL-SIZE TO TRUE                       ELUBPLDA
01254            PERFORM 999-SIGNAL-ABEND                               ELUBPLDA
01255      ELSE                                                         ELUBPLDA
01256         CONTINUE.                                                 ELUBPLDA
01257                                                                   ELUBPLDA
01258 ************************************************************      ELUBPLDA
01259 *                                                          *      ELUBPLDA
01260 *    ACQUIRE STORAGE FOR THE IPGT POINTER TABLE            *      ELUBPLDA
01261 *                                                          *      ELUBPLDA
01262 ************************************************************      ELUBPLDA
01263                                                                   ELUBPLDA
01264  571-ACQ-STG-IPGT-PTR-TBL.                                        ELUBPLDA
01265      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELUBPLDA
01266      COMPUTE CIA-AREA-LEN =                                       ELUBPLDA
01267           LENGTH OF IPGT-TBL-CNT                                  ELUBPLDA
01268         + (CIA-MVO * LENGTH OF IPGT-INTERNAL-TABS).               ELUBPLDA
01269      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
01270      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
01271                                                                   ELUBPLDA
01272      CALL 'ELUSETAD'                                              ELUBPLDA
01273         USING DFHCOMMAREA                                         ELUBPLDA
01274               ADDRESS OF IPGT-INTERNAL-TABS-TABLE.                ELUBPLDA
01275      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01276      THEN                                                         ELUBPLDA
01277          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUBPLDA
01278      ELSE                                                         ELUBPLDA
01279         MOVE CIA-MVO TO WS-IPGT-TBL-MAX                           ELUBPLDA
01280         INITIALIZE IPGT-TBL-CNT.                                  ELUBPLDA
01281                                                                   ELUBPLDA
01282 /***********************************************************      ELUBPLDA
01283 *                                                          *      ELUBPLDA
01284 *    CHECK TO SEE OF IPGT SLOT NUMBER IS ALREADY LODED     *      ELUBPLDA
01285 *                                                          *      ELUBPLDA
01286 ************************************************************      ELUBPLDA
01287                                                                   ELUBPLDA
01288  572-CHK-IPGT-ALREADY-LOADED.                                     ELUBPLDA
01289      SET UNIQUE-SLOT-NBR TO TRUE.                                 ELUBPLDA
01290      PERFORM WITH TEST BEFORE                                     ELUBPLDA
01291         VARYING WS-SUB3 FROM 1 BY 1                               ELUBPLDA
01292           UNTIL    WS-SUB3 > IPGT-TBL-CNT                         ELUBPLDA
01293                 OR DUPLICATE-SLOT-NBR                             ELUBPLDA
01294         IF WS-IPGT-SLOT-NBR = IPGT-SLOT-NUMBER (WS-SUB3)          ELUBPLDA
01295         THEN                                                      ELUBPLDA
01296            SET DUPLICATE-SLOT-NBR TO TRUE                         ELUBPLDA
01297         ELSE                                                      ELUBPLDA
01298            CONTINUE                                               ELUBPLDA
01299         END-IF                                                    ELUBPLDA
01300         END-PERFORM.                                              ELUBPLDA
01301                                                                   ELUBPLDA
01302 ************************************************************      ELUBPLDA
01303 *                                                          *      ELUBPLDA
01304 *    LOAD IPGT RECORD INTO MEMORY AND ADD TO POINTER TABLE *      ELUBPLDA
01305 *                                                          *      ELUBPLDA
01306 ************************************************************      ELUBPLDA
01307                                                                   ELUBPLDA
01308  573-LOAD-IPGT.                                                   ELUBPLDA
01309      MOVE PC-IPGT TO KWA-PROVISION-ID.                            ELUBPLDA
01310      MOVE WS-IPGT-SLOT-NBR TO KWA-PROVISION-SLOT-NO.              ELUBPLDA
01311      PERFORM 800-READ-TABULAR-RECORD.                             ELUBPLDA
01312      ADD +1 TO IPGT-TBL-CNT.                                      ELUBPLDA
01313      SET IPGT-TABULAR-PTR (IPGT-TBL-CNT) TO IOP-REC-PTR.          ELUBPLDA
01314      MOVE WS-IPGT-SLOT-NBR TO IPGT-SLOT-NUMBER (IPGT-TBL-CNT).    ELUBPLDA
01315      SET IOP-REC-PTR TO NULLS.                                    ELUBPLDA
01316                                                                   ELUBPLDA
01317 /***********************************************************      ELUBPLDA
01318 *                                                          *      ELUBPLDA
01319 *    READ TABULAR RECORD                                   *      ELUBPLDA
01320 *                                                          *      ELUBPLDA
01321 ************************************************************      ELUBPLDA
01322                                                                   ELUBPLDA
01323  800-READ-TABULAR-RECORD.                                         ELUBPLDA
01324      SET CIA-GCTABULR-DDN TO TRUE.                                ELUBPLDA
01325      CALL 'ELUSETAD'                                              ELUBPLDA
01326         USING DFHCOMMAREA                                         ELUBPLDA
01327               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELUBPLDA
01328      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01329      THEN                                                         ELUBPLDA
01330          PERFORM 801-ACQ-STG-GCTABULR.                            ELUBPLDA
01331      SET IOP-RD TO TRUE.                                          ELUBPLDA
01332      SET IOP-FCQ-NONE TO TRUE.                                    ELUBPLDA
01333      SET IOP-KVQ-EQ TO TRUE.                                      ELUBPLDA
01334      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUBPLDA
01335      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELUBPLDA
01336      PERFORM 952-CALL-I-O-MODULE.                                 ELUBPLDA
01337      IF IOP-RC-NOTFND                                             ELUBPLDA
01338      THEN                                                         ELUBPLDA
01339         PERFORM 992-SIGNAL-TABULAR-NOT-FOUND                      ELUBPLDA
01340      ELSE                                                         ELUBPLDA
01341         IF NOT IOP-RC-OK                                          ELUBPLDA
01342         THEN                                                      ELUBPLDA
01343            PERFORM 993-SIGNAL-CRITICAL-I-O-ERROR                  ELUBPLDA
01344         ELSE                                                      ELUBPLDA
01345            CONTINUE.                                              ELUBPLDA
01346                                                                   ELUBPLDA
01347 ************************************************************      ELUBPLDA
01348 *                                                          *      ELUBPLDA
01349 *    ACQUIRE STORAGE FOR GCTABULAR IOP BLOCK               *      ELUBPLDA
01350 *                                                          *      ELUBPLDA
01351 ************************************************************      ELUBPLDA
01352                                                                   ELUBPLDA
01353  801-ACQ-STG-GCTABULR.                                            ELUBPLDA
01354      SET CIA-STG-GETMAIN TO TRUE.                                 ELUBPLDA
01355      PERFORM 951-CALL-STORAGE-MANAGER.                            ELUBPLDA
01356      SET CIA-GCTABULR-DDN TO TRUE.                                ELUBPLDA
01357      CALL 'ELUSETAD'                                              ELUBPLDA
01358         USING DFHCOMMAREA                                         ELUBPLDA
01359         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELUBPLDA
01360      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01361      THEN                                                         ELUBPLDA
01362         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUBPLDA
01363                                                                   ELUBPLDA
01364 /***********************************************************      ELUBPLDA
01365 *                                                          *      ELUBPLDA
01366 *    ESTABLISH ADDRESSABILITY OF POINTER LIST              *      ELUBPLDA
01367 *                                                          *      ELUBPLDA
01368 ************************************************************      ELUBPLDA
01369  901-ESTAB-ADDR-ELSCSPTC.                                         ELUBPLDA
01370      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELUBPLDA
01371      CALL 'ELUSETAD'                                              ELUBPLDA
01372         USING DFHCOMMAREA                                         ELUBPLDA
01373               ADDRESS OF CSPT-POINTER-LIST.                       ELUBPLDA
01374      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01375      THEN                                                         ELUBPLDA
01376         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUBPLDA
01377                                                                   ELUBPLDA
01378 ************************************************************      ELUBPLDA
01379 *                                                          *      ELUBPLDA
01380 *    ESTABLISH ADDRESSABILITY OF KEY WORK AREA             *      ELUBPLDA
01381 *                                                          *      ELUBPLDA
01382 ************************************************************      ELUBPLDA
01383  902-ESTAB-ADDR-ELSKEYS.                                          ELUBPLDA
01384      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUBPLDA
01385      CALL 'ELUSETAD'                                              ELUBPLDA
01386         USING DFHCOMMAREA                                         ELUBPLDA
01387               ADDRESS OF KWA-FILE-KEY-WORK-AREA.                  ELUBPLDA
01388      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01389      THEN                                                         ELUBPLDA
01390         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUBPLDA
01391                                                                   ELUBPLDA
01392 ************************************************************      ELUBPLDA
01393 *                                                          *      ELUBPLDA
01394 *    ESTABLISH ADDRESSABILITY OF CS ACCUMULATOR TABLE      *      ELUBPLDA
01395 *                                                          *      ELUBPLDA
01396 ************************************************************      ELUBPLDA
01397                                                                   ELUBPLDA
01398  903-ESTAB-ADDR-ELSCSAC.                                          ELUBPLDA
01399      SET CIA-ELSCSAC-DDN TO TRUE.                                 ELUBPLDA
01400      CALL 'ELUSETAD'                                              ELUBPLDA
01401         USING DFHCOMMAREA                                         ELUBPLDA
01402               ADDRESS OF CSAC-ACCUMULATOR-TABLE.                  ELUBPLDA
01403      IF CIA-RC-PTR-NULL                                           ELUBPLDA
01404      THEN                                                         ELUBPLDA
01405         PERFORM 991-SIGNAL-UNALLOC-AREA.                          ELUBPLDA
01406                                                                   ELUBPLDA
01407 /***********************************************************      ELUBPLDA
01408 *                                                          *      ELUBPLDA
01409 *    CALL STORAGE MANAGER                                  *      ELUBPLDA
01410 *                                                          *      ELUBPLDA
01411 ************************************************************      ELUBPLDA
01412                                                                   ELUBPLDA
01413  951-CALL-STORAGE-MANAGER.                                        ELUBPLDA
01414      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA.                  ELUBPLDA
01415                                                                   ELUBPLDA
01416 ************************************************************      ELUBPLDA
01417 *                                                          *      ELUBPLDA
01418 *    CALL I-O MODULE                                       *      ELUBPLDA
01419 *                                                          *      ELUBPLDA
01420 ************************************************************      ELUBPLDA
01421                                                                   ELUBPLDA
01422  952-CALL-I-O-MODULE.                                             ELUBPLDA
01423      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA.                  ELUBPLDA
01424                                                                   ELUBPLDA
01425 /***********************************************************      ELUBPLDA
01426 *                                                          *      ELUBPLDA
01427 *    ESTABLISH THE ENGLISH CONTRACT INQUIRY STORAGE        *      ELUBPLDA
01428 *    MANAGEMENT ENVIRONMENT                                *      ELUBPLDA
01429 *                                                          *      ELUBPLDA
01430 *  - CHECK VALIDITY OF COMMAREA, ABEND IF NOT VALID        *      ELUBPLDA
01431 *  - ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA,    *      ELUBPLDA
01432 *    ABEND IF THE POINTER IS NULL                          *      ELUBPLDA
01433 *  - INITIALIZE THE STORAGE MANAGEMENT SYSTEM              *      ELUBPLDA
01434 *                                                          *      ELUBPLDA
01435 ************************************************************      ELUBPLDA
01436  990-ESTAB-ECI-STG-ENVIRON.                                       ELUBPLDA
01437      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUBPLDA
01438      THEN                                                         ELUBPLDA
01439         EXEC CICS ABEND ABCODE ('EL01') END-EXEC                  ELUBPLDA
01440      ELSE                                                         ELUBPLDA
01441         IF ECA-CIA-PTR = NULL                                     ELUBPLDA
01442         THEN                                                      ELUBPLDA
01443            EXEC CICS ABEND ABCODE ('EL02') END-EXEC               ELUBPLDA
01444         ELSE                                                      ELUBPLDA
01445            CALL 'ELUINISM'                                        ELUBPLDA
01446               USING DFHCOMMAREA                                   ELUBPLDA
01447                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELUBPLDA
01448                                                                   ELUBPLDA
01449 /***********************************************************      ELUBPLDA
01450 *                                                          *      ELUBPLDA
01451 *    SIGNAL UNALLOCATED AREA ERROR                         *      ELUBPLDA
01452 *                                                          *      ELUBPLDA
01453 ************************************************************      ELUBPLDA
01454  991-SIGNAL-UNALLOC-AREA.                                         ELUBPLDA
01455      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUBPLDA
01456      PERFORM 999-SIGNAL-ABEND.                                    ELUBPLDA
01457                                                                   ELUBPLDA
01458 ************************************************************      ELUBPLDA
01459 *                                                          *      ELUBPLDA
01460 *    SIGNAL TABULAR NOT FOUND                              *      ELUBPLDA
01461 *                                                          *      ELUBPLDA
01462 ************************************************************      ELUBPLDA
01463  992-SIGNAL-TABULAR-NOT-FOUND.                                    ELUBPLDA
01464      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELUBPLDA
01465      PERFORM 999-SIGNAL-ABEND.                                    ELUBPLDA
01466                                                                   ELUBPLDA
01467 ************************************************************      ELUBPLDA
01468 *                                                          *      ELUBPLDA
01469 *    SIGNAL CRITIACAL I-O ERROR                            *      ELUBPLDA
01470 *                                                          *      ELUBPLDA
01471 ************************************************************      ELUBPLDA
01472  993-SIGNAL-CRITICAL-I-O-ERROR.                                   ELUBPLDA
01473      SET CIA-AB-CRITIO TO TRUE.                                   ELUBPLDA
01474      PERFORM 999-SIGNAL-ABEND.                                    ELUBPLDA
01475                                                                   ELUBPLDA
01476 ************************************************************      ELUBPLDA
01477 *                                                          *      ELUBPLDA
01478 *    SIGNAL ABEND                                          *      ELUBPLDA
01479 *                                                          *      ELUBPLDA
01480 ************************************************************      ELUBPLDA
01481                                                                   ELUBPLDA
01482  999-SIGNAL-ABEND.                                                ELUBPLDA
01483      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUBPLDA
