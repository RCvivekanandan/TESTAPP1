00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCAC
00003  PROGRAM-ID.         ELXPMCAC.                                       LV004
00004                                                                   ELXPMCAC
00005  AUTHOR.             JOHN BEIRNE                                  ELXPMCAC
00006                                                                   ELXPMCAC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCAC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCAC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCAC
00010                      233 N. MICHIGAN AVE                          ELXPMCAC
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCAC
00012                                                                   ELXPMCAC
00013  DATE-WRITTEN.       18-NOV-1992.                                 ELXPMCAC
00014                                                                   ELXPMCAC
00015  DATE-COMPILED.                                                   ELXPMCAC
00016                                                                   ELXPMCAC
00017                                                                   ELXPMCAC
00018  SECURITY.           COPYRIGHT 1992,                              ELXPMCAC
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCAC
00020      SKIP3                                                        ELXPMCAC
00021  TITLE 'ELIGIBILITY SUMMARY ACCUMULATOR DISPLAY        '.         ELXPMCAC
00022  ENVIRONMENT DIVISION.                                            ELXPMCAC
00023                                                                   ELXPMCAC
00024  CONFIGURATION SECTION.                                           ELXPMCAC
00025  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCAC
00026  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCAC
00027      EJECT                                                        ELXPMCAC
00028 ******************************************************************ELXPMCAC
00029 *                                                                *ELXPMCAC
00030 *    PROGRAM:    ELXPMCAC                                        *ELXPMCAC
00031 *    DATE:       18-NOV-1992                                     *ELXPMCAC
00032 *    AUTHOR:     JOHN BEIRNE                                     *ELXPMCAC
00033 *    FUNCTION:   THIS PROGRAM PROCESSES CONFIDENCE FACTORS FOR   *ELXPMCAC
00034 *                THE ACCUMS PORTION OF ELIGIBILITY SUMMARY. IT   *ELXPMCAC
00035 *                CALLS THE PROGRAMS WHICH PROCESS THE CONFIDENCE *ELXPMCAC
00036 *                FACTORS FOR COST CONTAINMENT AND BENEFIT PRO-   *ELXPMCAC
00037 *                VISIONS.                                        *ELXPMCAC
00038 ******************************************************************ELXPMCAC
00039 *                    ERROR CODE LOG                              *ELXPMCAC
00040 ******************************************************************ELXPMCAC
00041 *  +4001 - UNABLE TO ADDRESS GROUP RECORD                        *ELXPMCAC
00042 *  +4002 - UNABLE TO ADDRESS PMCI COMMUNICATION AREA             *ELXPMCAC
00043 *  +4004 - NO BENEFIT PROVISION TABLE FOUND (ELSPMCID)           *ELXPMCAC
00044 *  +4005 - UNIDENTIFIED PARAMETERS TO ELKACCMF                   *ELXPMCAC
00045 *  +4006 - MISSING PARAMETERS TO ELKACCMF                        *ELXPMCAC
00046 *  +4007 - UNIDENTIFIED PARAMETERS TO ELXSPCFF                   *ELXPMCAC
00047 *  +4008 - MISSING PARAMETERS TO ELXSPCFF                        *ELXPMCAC
00048 *  +4009 - INTERNAL ERROR IN ELXSPCFF                            *ELXPMCAC
00049 *  +4010 - IPGN RECALCULATION FAILED                             *ELXPMCAC
00050 *  +4011 - IPGT RECALCULATION FAILED                             *ELXPMCAC
00051 *  +4012 - UNABLE TO ADDRESS CSAC ACCUMULATOR TABLE              *ELXPMCAC
00052 *  +4013 - IPGS RECALCULATION FAILED                             *ELXPMCAC
00053 ******************************************************************ELXPMCAC
00054 *                      MAINTENANCE HISTORY                       *ELXPMCAC
00055 *                                                                *ELXPMCAC
00056 * MOD      DATE     BY  DRPT                ACTION               *ELXPMCAC
00057 * ----- ----------- --- ----- ---------------------------------- *ELXPMCAC
00058 * 01.00 18-NOV-1992 JPB       CREATED                            *ELXPMCAC
00059 *                                                                *ELXPMCAC
00060 * 01.01 08-DEC-1992 AKK       UNCOMMENTED CALL TO ELXPMCAP AND   *ELXPMCAC
00061 *                             EXPANDED USING STATEMENT IN THE    *ELXPMCAC
00062 *                             CALL.  THEN RECOMMENTED IT.        *ELXPMCAC
00063 *                                                                *ELXPMCAC
00064 * 01.02 14-DEC-1992 AKK       UNCOMMENTED CALL TO ELXPMCAP TO    *ELXPMCAC
00065 *                             START TESTING.                     *ELXPMCAC
00066 *                                                                *ELXPMCAC
00067 * 01.03 22-DEC-1992 JPB       CHANGED NUMBERING SCHEME           *ELXPMCAC
00068 *                                                                *ELXPMCAC
00069 * 01.05 13-JAN-1993 JPB       CHANGED COINSURANCE LOGIC TO       *ELXPMCAC
00070 *                             DISPLAY THE AMOUNT THE SUBSCRIBER  *ELXPMCAC
00071 *                             PAYS.                              *ELXPMCAC
00072 *                                                                *ELXPMCAC
00073 * 01.06 19-FEB-1993 JPB       IF THERE IS NO COINSURANCE, THE    *ELXPMCAC
00074 *                             PMCI-COINSURANCE-NONE SWITCH IS    *ELXPMCAC
00075 *                             SET TO TRUE.                       *ELXPMCAC
00076 *                                                                *ELXPMCAC
00077 * 01.07 18-MAR 1993 JPB       TOOK PLAN OUT OF CALCULATION FOR   *ELXPMCAC
00078 *                             PROFESSIONAL SIDE.                 *ELXPMCAC
00079 * 01.08 25-MAY 1993 BAK       COMBINED LIKE CONFIDENCE FACTOR    *ELXPMCAC
00080 *                             CALCULATIONS CHANGE HARD CODE OF   *ELXPMCAC
00081 *                             CONFIDENCE FACTOR LITORALS TO      *ELXPMCAC
00082 *                             WORK FIELDS.                       *ELXPMCAC
00083 * 02.00 26-MAY 1993 BAK       ADD MAJOR MEDICAL SUPPORT          *ELXPMCAC
00084 *                                                                *ELXPMCAC
00085 * 02.01 16-JUN 1993 JPB       ADDED LOGIC TO MAKE SURE SOMETHING *ELXPMCAC
00086 *                             IS IN PMCI-MM-CONTRACT-KEY BEFORE  *ELXPMCAC
00087 *                             CALCULATING FOR MAJOR-MEDICAL.     *ELXPMCAC
00088 * 02.02 29-JUN 1993 BAK       ADDED LOGIC TO CHECK FOR MULTIPLE  *ELXPMCAC
00089 *                             ACCUMS FOR INDIVIDUAL AND FAMILLY  *ELXPMCAC
00090 *                             DEDUCTIBLES TO SET CALL TO TRUE.   *ELXPMCAC
00091 *                                                                *ELXPMCAC
00092 * 02.03 26-JUL 1993 AKK       ADDED ERROR CHECKS PRIOR TO CALLS  *ELXPMCAC
00093 *                             TO ELXPMCAB AND ELXPMCAP.  ADDED   *ELXPMCAC
00094 *                             +4004 ERROR CODE.                  *ELXPMCAC
00095 * 03.00 04-OCT 1993 BAK       ADDED SUPPORT FOR PHASE 2 FOR RPO  *ELXPMCAC
00096 *                             AND MCNP ENHANCEMENTS. ISSR 13071. *ELXPMCAC
00097 * 03.01 JAN 18, 1994 RGO     -FIX DEDUCTIBLES. IN 2150-, BENIFIT *ELXPMCAC
00098 *                             PERIODS '0E' SHOULD BE HANDLED LIKE*ELXPMCAC
00099 *                             '0D'. THIS IS ALSO THE CASE IN     *ELXPMCAC
00100 *                             2152-. ALSO FIXED A BUG IN 2152-:  *ELXPMCAC
00101 *                             IT WAS SETTING INDIVIDUAL INSTEAD  *ELXPMCAC
00102 *                             OF THE FAMILY INDICATOR.           *ELXPMCAC
00103 *                            -FOUND BUGS IN 3136M-,3137M-,3138M-,*ELXPMCAC
00104 *                             AND 2139M-.  WS-CF-3 WAS USING     *ELXPMCAC
00105 *                             BASIC INSTEAD OF SUPPLEMENTAL.     *ELXPMCAC
00106 *                                                                *ELXPMCAC
00107 * 03.02 JUNE 7,1994 RGO     -FIX ASRA OCCURRING DUE TO A NULL    *ELXPMCAC
00108 *                            POINTERS IN THE CSAC TABLE,         *ELXPMCAC
00109 *                            PARAGRAPH 1400-.                    *ELXPMCAC
00110 *                                                                *ELXPMCAC
00111 * MARCH 8,1995 RGO  CPO PROJECT.                                 *ELXPMCAC
00112 *              1) NO NEED TO UPDATE 3100-.                       *ELXPMCAC
00113 *              2) ADD CHECKS FOR CPO IN 3250.                    *ELXPMCAC
00114 *                                                                *ELXPMCAC
00115 * APRIL 3,1996 RGO  COMMUNITY BLUE PROJECT                       *ELXPMCAC
00116 *              1) ADD CHECKS FOR CBL IN 3250.                    *ELXPMCAC
00117 *                                                                *ELXPMCAC
00118 * APRIL 14 2000 AKK ADDED CODED TO TRY AND HANDLE 'COMBINED'     *ELXPMCAC
00119 *              DEDUCTIBLES.  ON 04/18/00 COMMENTED IT OUT,       *ELXPMCAC
00120 *              IT DID NOT WORK.                                  *ELXPMCAC
00121 *                                                                *ELXPMCAC
00122 * AUGUST 28 2000 AKK ADDED SUPPORT FOR #IPGS                     *ELXPMCAC
00123 *                                                                *ELXPMCAC
00124 * 08/28/2000   JP CHANGED PARA. 1250 TO PERFORM PARA. 1400 UNTIL  ELXPMCAC
00125 *              CSAC-X-IDX > 5 DUE TO ADDITION OF ACP POINTER      ELXPMCAC
00126 *              TO ELSCSACC.                                       ELXPMCAC
00127 *                                                                 ELXPMCAC
00128 * SEPT  06  2000 AKK ADDED SUPPORT FOR BAE PROGRAM.  THIS WILL   *ELXPMCAC
00129 *                    BE MOVED UP WITH THE CHANGES FOR ACP         ELXPMCAC
00130 *                    AND #IPGS                                    ELXPMCAC
00131 *                                                                 ELXPMCAC
00132 * 09/22/2000   JP    ADDED CODING AND COPY MEMBER ELSACCDE TO     ELXPMCAC
00133 *                    CREATE ACCUM CDE TABLE.                      ELXPMCAC
00134 *                                                                 ELXPMCAC
00135 * 03/12/03     AKK REGEN'D W/ UPDATE PMCCOMM WITH PROV UNIT USING ELXPMCAC
00136 *                  ELS VERSION.                                   ELXPMCAC
00137 * 04/01/03     AKK REGEN'D W/ UPDATE PMCCOMM WITH PROV UNIT USING ELXPMCAC
00138 *                  ELS VERSION. MORE CHANGES IN BA31              ELXPMCAC
00139 * 05/07/03     AKK REGEN'D DUE TO CHANGES IN PROC/DIAG CODE       ELXPMCAC
00140 *                                                                 ELXPMCAC
00141 * 05/23/03     AKK REGEN'D DUE TO CHANGES IN OTHER PROGRAMS       ELXPMCAC
00142 *                  FOR FILE EXPANSION                             ELXPMCAC
00143 * 06/24/03     AKK REGEN'D DUE TO CHANGES IN CALLED PGMS          ELXPMCAC
00144 *                  FOR FILE EXPANSION                             ELXPMCAC
00145 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCAC
00146 *                                                                *ELXPMCAC
00147 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCAC
00148 *                                                                *ELXPMCAC
00149 * 02.02 27-JAN-2004 AKK REGEN FOR COMPILER CHANGES               *ELXPMCAC
00150 *                                                                *ELXPMCAC
00151 * 03.00 27-APR-2005 AKK ADD FREEMAINS TO ALLEVIATE SHORT ON      *ELXPMCAC
00152 *                       STORAGE CONDITON IN M270 TRANSACTION.    *ELXPMCAC
00153 *                                                                *ELXPMCAC
00154 * 03.01 13-JUL-2005 AKK REGEN FOR INTERTEST                      *ELXPMCAC
00155 *                                                                *ELXPMCAC
00156 ******************************************************************ELXPMCAC
00157      EJECT                                                        ELXPMCAC
00158  DATA DIVISION.                                                   ELXPMCAC
00159  WORKING-STORAGE SECTION.                                         ELXPMCAC
00160  01  WS-HDG                      PIC X(32)                        ELXPMCAC
00161           VALUE '******* WS STARTS HERE ******'.                  ELXPMCAC
00162                                                                   ELXPMCAC
00163  01  WS-SWITCHES.                                                 ELXPMCAC
00164      05 WS-ACL-SWITCH            PIC      X(01).                  ELXPMCAC
00165         88 PROCESSING-ACL                       VALUE 'Y'.        ELXPMCAC
00166         88 NOT-PROCESSING-ACL                   VALUE 'N'.        ELXPMCAC
00167      05 WS-TERMINAL-SWITCH       PIC      X(01).                  ELXPMCAC
00168         88 SW-NO-TRMNL-ERR                       VALUE 'N'.       ELXPMCAC
00169         88 SW-TRMNL-ERR                          VALUE 'Y'.       ELXPMCAC
00170                                                                   ELXPMCAC
00171      05 WS-INTERNAL-ERROR-SWITCH PIC      X(01).                  ELXPMCAC
00172         88 SW-NO-INTRNL-ERR                      VALUE 'N'.       ELXPMCAC
00173         88 SW-INTRNL-ERR                         VALUE 'Y'.       ELXPMCAC
00174                                                                   ELXPMCAC
00175      05 WS-ATBL-SWITCH           PIC      X(01).                  ELXPMCAC
00176         88 WS-ATBL-NOT-FOUND                     VALUE 'N'.       ELXPMCAC
00177         88 WS-ATBL-OK                            VALUE 'Y'.       ELXPMCAC
00178                                                                   ELXPMCAC
00179      05 WS-CSAC-SWITCH           PIC      X(01).                  ELXPMCAC
00180         88 WS-CSAC-NOT-FOUND                     VALUE 'N'.       ELXPMCAC
00181         88 WS-CSAC-OK                            VALUE 'Y'.       ELXPMCAC
00182                                                                   ELXPMCAC
00183      05 WS-PRG-VARIANCE-SWITCH   PIC      X(01).                  ELXPMCAC
00184         88 WS-PRG-VAR-NOT-FOUND                  VALUE 'N'.       ELXPMCAC
00185         88 WS-PRG-VAR-FOUND                      VALUE 'Y'.       ELXPMCAC
00186                                                                   ELXPMCAC
00187      05  WS-IPGN-SLOT-SWITCHES   PIC      X(01).                  ELXPMCAC
00188          88 WS-IPGN-SLOT-NO-FOUND                VALUE 'Y'.       ELXPMCAC
00189          88 WS-IPGN-SLOT-NO-NOT-FOUND            VALUE 'N'.       ELXPMCAC
00190          88 WS-IPGN-SLOT-NO-NOT-IN-TABLE         VALUE 'T'.       ELXPMCAC
00191                                                                   ELXPMCAC
00192      05  WS-IPGT-SLOT-SWITCHES   PIC      X(01).                  ELXPMCAC
00193          88 WS-IPGT-SLOT-NO-FOUND                VALUE 'Y'.       ELXPMCAC
00194          88 WS-IPGT-SLOT-NO-NOT-FOUND            VALUE 'N'.       ELXPMCAC
00195          88 WS-IPGT-SLOT-NO-NOT-IN-TABLE         VALUE 'T'.       ELXPMCAC
00196                                                                   ELXPMCAC
00197      05  WS-IPGS-SLOT-SWITCHES   PIC      X(01).                  ELXPMCAC
00198          88 WS-IPGS-SLOT-NO-FOUND                VALUE 'Y'.       ELXPMCAC
00199          88 WS-IPGS-SLOT-NO-NOT-FOUND            VALUE 'N'.       ELXPMCAC
00200          88 WS-IPGS-SLOT-NO-NOT-IN-TABLE         VALUE 'T'.       ELXPMCAC
00201                                                                   ELXPMCAC
00202      05  WS-MCNP-PENALTY-IND       PIC X(01)  VALUE 'N'.          ELXPMCAC
00203        88  WS-MCNP-PEN-FOUND               VALUE 'Y'.             ELXPMCAC
00204        88  WS-MCNP-PEN-NOT-FOUND           VALUE 'N'.             ELXPMCAC
00205                                                                   ELXPMCAC
00206      05  WS-MCNP-INCENT-IND       PIC X(01)  VALUE 'N'.           ELXPMCAC
00207        88  WS-MCNP-INC-FOUND               VALUE 'Y'.             ELXPMCAC
00208        88  WS-MCNP-INC-NOT-FOUND           VALUE 'N'.             ELXPMCAC
00209                                                                   ELXPMCAC
00210      05  WS-PPO-PENALTY-IND       PIC X(01)  VALUE 'N'.           ELXPMCAC
00211        88  WS-PPO-PEN-FOUND               VALUE 'Y'.              ELXPMCAC
00212        88  WS-PPO-PEN-NOT-FOUND           VALUE 'N'.              ELXPMCAC
00213                                                                   ELXPMCAC
00214      05  WS-PPO-INCENT-IND       PIC X(01)  VALUE 'N'.            ELXPMCAC
00215        88  WS-PPO-INC-FOUND               VALUE 'Y'.              ELXPMCAC
00216        88  WS-PPO-INC-NOT-FOUND           VALUE 'N'.              ELXPMCAC
00217                                                                   ELXPMCAC
00218      05  WS-RPO-PENALTY-IND       PIC X(01)  VALUE 'N'.           ELXPMCAC
00219        88  WS-RPO-PEN-FOUND               VALUE 'Y'.              ELXPMCAC
00220        88  WS-RPO-PEN-NOT-FOUND           VALUE 'N'.              ELXPMCAC
00221                                                                   ELXPMCAC
00222      05  WS-RPO-INCENT-IND       PIC X(01)  VALUE 'N'.            ELXPMCAC
00223        88  WS-RPO-INC-FOUND               VALUE 'Y'.              ELXPMCAC
00224        88  WS-RPO-INC-NOT-FOUND           VALUE 'N'.              ELXPMCAC
00225                                                                   ELXPMCAC
00226      05  WS-BAE-PENALTY-IND       PIC X(01)  VALUE 'N'.           ELXPMCAC
00227        88  WS-BAE-PEN-FOUND               VALUE 'Y'.              ELXPMCAC
00228        88  WS-BAE-PEN-NOT-FOUND           VALUE 'N'.              ELXPMCAC
00229                                                                   ELXPMCAC
00230  01  WS-RETURN-CODE                     PIC S9(04) COMP.          ELXPMCAC
00231      88  WS-SUCCESSFUL-CALL             VALUE +0.                 ELXPMCAC
00232      88  WS-UNIDENTIFIED-PARAMETER      VALUE +8.                 ELXPMCAC
00233      88  WS-MISSING-PARAMETER           VALUE +12.                ELXPMCAC
00234      88  WS-INTERNAL-ERROR              VALUE +16.                ELXPMCAC
00235                                                                   ELXPMCAC
00236  01  WS-CONFIDENCE-FACTORS.                                       ELXPMCAC
00237      05 WS-CF-1                  COMP-1.                          ELXPMCAC
00238      05 WS-CF-2                  COMP-1.                          ELXPMCAC
00239      05 WS-CF-3                  COMP-1.                          ELXPMCAC
00240      05 WS-CF-4                  COMP-1.                          ELXPMCAC
00241      05 WS-CF-5                  COMP-1.                          ELXPMCAC
00242      05 WS-CF-6                  COMP-1.                          ELXPMCAC
00243                                                                   ELXPMCAC
00244  01  WS-WEIGHTS.                                                  ELXPMCAC
00245      05 WS-WT-LFTM               COMP-1          VALUE 0.75E+00.  ELXPMCAC
00246      05 WS-WT-ANL                COMP-1          VALUE 0.75E+00.  ELXPMCAC
00247      05 WS-WT-FAM                COMP-1          VALUE 0.50E+00.  ELXPMCAC
00248      05 WS-WT-INDVDL             COMP-1          VALUE 0.50E+00.  ELXPMCAC
00249      05 WS-WT-INST-BAS           COMP-1          VALUE 0.50E+00.  ELXPMCAC
00250      05 WS-WT-INST-SUP           COMP-1          VALUE 0.50E+00.  ELXPMCAC
00251      05 WS-WT-PROF-BAS           COMP-1          VALUE 0.50E+00.  ELXPMCAC
00252      05 WS-WT-PROF-SUP           COMP-1          VALUE 0.50E+00.  ELXPMCAC
00253      05 WS-WT-IP                 COMP-1          VALUE 0.50E+00.  ELXPMCAC
00254      05 WS-WT-OP                 COMP-1          VALUE 0.50E+00.  ELXPMCAC
00255      05 WS-WT-PLAN               COMP-1          VALUE 0.25E+00.  ELXPMCAC
00256      05 WS-WT-NON-PLAN           COMP-1          VALUE 0.25E+00.  ELXPMCAC
00257      05 WS-WT-OV                 COMP-1          VALUE 0.50E+00.  ELXPMCAC
00258      05 WS-WT-SP                 COMP-1          VALUE 0.50E+00.  ELXPMCAC
00259                                                                   ELXPMCAC
00260  01  WS-ABSOLUTE-CONFIDENCE-FACTORS.                              ELXPMCAC
00261      05 WS-CF-TRUE               COMP-1          VALUE +1.00E+00. ELXPMCAC
00262      05 WS-CF-FALSE              COMP-1          VALUE -1.00E+00. ELXPMCAC
00263      05 WS-CF-ZERO               COMP-1          VALUE +0.00E+00. ELXPMCAC
00264                                                                   ELXPMCAC
00265  01  WS-TEST-SAVE-ENTRIES.                                        ELXPMCAC
00266      05 WS-BS-TEST-CF               COMP-1           VALUE ZERO.  ELXPMCAC
00267      05 WS-BS-TEST-SUB              PIC S9(04)  COMP VALUE ZERO.  ELXPMCAC
00268      05 WS-MM-TEST-CF               COMP-1           VALUE ZERO.  ELXPMCAC
00269      05 WS-MM-TEST-SUB              PIC S9(04)  COMP VALUE ZERO.  ELXPMCAC
00270      05 WS-SAVE-COVER-TYPE          PIC X(01)        VALUE SPACE. ELXPMCAC
00271      05 WS-SAVE-THRESHOLD           COMP-1           VALUE ZERO.  ELXPMCAC
00272                                                                   ELXPMCAC
00273  01  WS-SUBSCRIPTS-AND-COUNTERS.                                  ELXPMCAC
00274      05 WS-SPECIAL-SUB           PIC S9(04)      COMP.            ELXPMCAC
00275      05 WS-OUT-OF-POCKET-FOUND   PIC S9(04)      COMP.            ELXPMCAC
00276      05 WS-BS-OPX-FOUND          PIC S9(04)      COMP.            ELXPMCAC
00277      05 WS-MM-OPX-FOUND          PIC S9(04)      COMP.            ELXPMCAC
00278      05 WS-INDVD-DED-FOUND       PIC S9(04)      COMP.            ELXPMCAC
00279      05 WS-BS-INDVD-FOUND        PIC S9(04)      COMP.            ELXPMCAC
00280      05 WS-MM-INDVD-FOUND        PIC S9(04)      COMP.            ELXPMCAC
00281      05 WS-FAMILY-DED-FOUND      PIC S9(04)      COMP.            ELXPMCAC
00282      05 WS-BS-FML-FOUND          PIC S9(04)      COMP.            ELXPMCAC
00283      05 WS-MM-FML-FOUND          PIC S9(04)      COMP.            ELXPMCAC
00284                                                                   ELXPMCAC
00285                                                                   ELXPMCAC
00286  COPY ELSCVG2C.                                                   ELXPMCAC
00287  COPY ELSPVTLC.                                                   ELXPMCAC
00288  COPY ELSPVSLC.                                                   ELXPMCAC
00289  COPY ELSPVNLC.                                                   ELXPMCAC
00290                                                                   ELXPMCAC
00291  LINKAGE SECTION.                                                 ELXPMCAC
00292  01  DFHCOMMAREA.                                                 ELXPMCAC
00293  COPY ELSCOMMC.                                                   ELXPMCAC
00294                                                                   ELXPMCAC
00295 *ELS COMMON INTERFACE AREA                                        ELXPMCAC
00296  COPY ELSCIA2C.                                                   ELXPMCAC
00297                                                                   ELXPMCAC
00298 /PMCI RESULTS COMMON AREA                                         ELXPMCAC
00299  01   PMCI-COMM-AREA.                                             ELXPMCAC
00300  COPY PMCCOMM.                                                    ELXPMCAC
00301                                                                   ELXPMCAC
00302  COPY ELSACCDE.                                                   ELXPMCAC
00303                                                                   ELXPMCAC
00304 /IBGR CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00305  COPY ELSIBGRC.                                                   ELXPMCAC
00306                                                                   ELXPMCAC
00307 *IDGD CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00308  COPY ELSIDGDC.                                                   ELXPMCAC
00309                                                                   ELXPMCAC
00310 *IPGN CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00311  COPY ELSIPGNC.                                                   ELXPMCAC
00312                                                                   ELXPMCAC
00313 *IPGP CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00314  COPY ELSIPGPC.                                                   ELXPMCAC
00315                                                                   ELXPMCAC
00316 *IPGT CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00317  COPY ELSIPGTC.                                                   ELXPMCAC
00318                                                                   ELXPMCAC
00319 *IPGS CONFIDENCE FACTOR TABLE                                     ELXPMCAC
00320  COPY ELSIPGSC.                                                   ELXPMCAC
00321                                                                   ELXPMCAC
00322 *ATBL ACCUMULATOR TABLE                                           ELXPMCAC
00323  COPY ELSATBLC.                                                   ELXPMCAC
00324                                                                   ELXPMCAC
00325 *ACCUMS POINTER AREA                                              ELXPMCAC
00326  COPY ELSCSACC.                                                   ELXPMCAC
00327                                                                   ELXPMCAC
00328                                                                   ELXPMCAC
00329  COPY ELSPMCID.                                                   ELXPMCAC
00330                                                                   ELXPMCAC
00331 *GROUP SPECIFIC RECORD                                            ELXPMCAC
00332  01 GROUP-SPECIFIC-RECORD.                                        ELXPMCAC
00333  COPY GCGROUPC.                                                   ELXPMCAC
00334                                                                   ELXPMCAC
00335 /***********************************************************      ELXPMCAC
00336 *                                                          *      ELXPMCAC
00337 *                    PROCEDURE DIVISION                    *      ELXPMCAC
00338 *                                                          *      ELXPMCAC
00339 ************************************************************      ELXPMCAC
00340                                                                   ELXPMCAC
00341  PROCEDURE DIVISION.                                              ELXPMCAC
00342                                                                   ELXPMCAC
00343 ************************************************************      ELXPMCAC
00344 *                                                          *      ELXPMCAC
00345 *    ELXPMCAC MAINLINE                                     *      ELXPMCAC
00346 *                                                          *      ELXPMCAC
00347 ************************************************************      ELXPMCAC
00348                                                                   ELXPMCAC
00349  0000-ELXPMCAC-MAINLINE.                                          ELXPMCAC
00350      SET SW-NO-TRMNL-ERR TO TRUE                                  ELXPMCAC
00351      IF ECA-CIA-PTR = NULL                                        ELXPMCAC
00352         CONTINUE                                                  ELXPMCAC
00353      ELSE                                                         ELXPMCAC
00354         PERFORM 0110-EST-ADDRS-OF-AREAS                           ELXPMCAC
00355         IF SW-NO-TRMNL-ERR                                        ELXPMCAC
00356            PERFORM 1000-PROCESS                                   ELXPMCAC
00357         ELSE                                                      ELXPMCAC
00358            CONTINUE                                               ELXPMCAC
00359         END-IF                                                    ELXPMCAC
00360      END-IF.                                                      ELXPMCAC
00361      PERFORM 9999-DO-FREEMAINS.                                   ELXPMCAC
00362      GOBACK.                                                      ELXPMCAC
00363                                                                   ELXPMCAC
00364                                                                   ELXPMCAC
00365 ***********************************************************       ELXPMCAC
00366 *                                                         *       ELXPMCAC
00367 *    ESTABLISH ADDRESSABILITY OF AREAS                    *       ELXPMCAC
00368 *                                                         *       ELXPMCAC
00369 ***********************************************************       ELXPMCAC
00370                                                                   ELXPMCAC
00371  0110-EST-ADDRS-OF-AREAS.                                         ELXPMCAC
00372      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCAC
00373                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELXPMCAC
00374                                                                   ELXPMCAC
00375      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCAC
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00377                   ADDRESS OF PMCI-COMM-AREA.                      ELXPMCAC
00378                                                                   ELXPMCAC
00379                                                                   ELXPMCAC
00380                                                                   ELXPMCAC
00381                                                                   ELXPMCAC
00382                                                                   ELXPMCAC
00383      IF CIA-RC-OK                                                 ELXPMCAC
00384         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCAC
00385         PERFORM 0120-EST-GROUP-SPCFC-ADDRS                        ELXPMCAC
00386      ELSE                                                         ELXPMCAC
00387         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCAC
00388         MOVE +4002 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
00389         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00390      END-IF.                                                      ELXPMCAC
00391                                                                   ELXPMCAC
00392 ***********************************************************       ELXPMCAC
00393 *                                                         *       ELXPMCAC
00394 *    ESTABLISH GROUP SPECIFIC ADDRESSABILITY              *       ELXPMCAC
00395 *                                                         *       ELXPMCAC
00396 ***********************************************************       ELXPMCAC
00397  0120-EST-GROUP-SPCFC-ADDRS.                                      ELXPMCAC
00398      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELXPMCAC
00399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00400                         ADDRESS OF GROUP-SPECIFIC-RECORD.         ELXPMCAC
00401      IF CIA-RC-OK                                                 ELXPMCAC
00402         CONTINUE                                                  ELXPMCAC
00403      ELSE                                                         ELXPMCAC
00404         SET SW-INTRNL-ERR TO TRUE                                 ELXPMCAC
00405         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00406         MOVE +4001 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAC
00407                                                                   ELXPMCAC
00408 /***********************************************************      ELXPMCAC
00409 *                                                          *      ELXPMCAC
00410 *    PROCESS                                               *      ELXPMCAC
00411 *                                                          *      ELXPMCAC
00412 ************************************************************      ELXPMCAC
00413                                                                   ELXPMCAC
00414  1000-PROCESS.                                                    ELXPMCAC
00415      MOVE +0000 TO PMCI-BLUE-CHIP-ERROR-CODE.                     ELXPMCAC
00416      CALL 'ELUGCLDA' USING DFHEIBLK                               ELXPMCAC
00417                            DFHCOMMAREA                            ELXPMCAC
00418      PERFORM 1001-EST-ADDRS-OF-CSAC.                              ELXPMCAC
00419      PERFORM 1101-EST-ADDRS-OF-IBGR.                              ELXPMCAC
00420      PERFORM 1102-EST-ADDRS-OF-IDGD.                              ELXPMCAC
00421      PERFORM 1103-EST-ADDRS-OF-IPGN.                              ELXPMCAC
00422      PERFORM 1104-EST-ADDRS-OF-IPGP.                              ELXPMCAC
00423      PERFORM 1105-EST-ADDRS-OF-IPGT.                              ELXPMCAC
00424      PERFORM 1106-EST-ADDRS-OF-IPGS.                              ELXPMCAC
00425                                                                   ELXPMCAC
00426      CALL 'ELKACCMF' USING CSAC-ACCUMULATOR-TABLE                 ELXPMCAC
00427                            IBGR-INTERNAL-TABS-TABLE               ELXPMCAC
00428                            IDGD-INTERNAL-TABS-TABLE               ELXPMCAC
00429                            IPGN-INTERNAL-TABS-TABLE               ELXPMCAC
00430                            IPGP-INTERNAL-TABS-TABLE               ELXPMCAC
00431                            IPGT-INTERNAL-TABS-TABLE               ELXPMCAC
00432                            IPGS-INTERNAL-TABS-TABLE.              ELXPMCAC
00433      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAC
00434      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00435         PERFORM 1110-RECALC-PROV-BASED-CFS                        ELXPMCAC
00436         IF  PMCI-BC-SUCCESSFUL                                    ELXPMCAC
00437             PERFORM 2000-PROCESS-ALL-ACCUMS                       ELXPMCAC
00438         ELSE                                                      ELXPMCAC
00439            CONTINUE                                               ELXPMCAC
00440         END-IF                                                    ELXPMCAC
00441      ELSE                                                         ELXPMCAC
00442         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00443         EVALUATE TRUE                                             ELXPMCAC
00444            WHEN WS-UNIDENTIFIED-PARAMETER                         ELXPMCAC
00445               MOVE +4005 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00446            WHEN WS-MISSING-PARAMETER                              ELXPMCAC
00447               MOVE +4006 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00448            WHEN WS-INTERNAL-ERROR                                 ELXPMCAC
00449               MOVE +4012 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00450         END-EVALUATE                                              ELXPMCAC
00451      END-IF.                                                      ELXPMCAC
00452                                                                   ELXPMCAC
00453 ***********************************************************       ELXPMCAC
00454 *                                                         *       ELXPMCAC
00455 *    ESTABLISH ADDRESSABILITY OF CSAC TABLE               *       ELXPMCAC
00456 *                                                         *       ELXPMCAC
00457 ***********************************************************       ELXPMCAC
00458  1001-EST-ADDRS-OF-CSAC.                                          ELXPMCAC
00459      SET CIA-ELSCSAC-DDN TO TRUE                                  ELXPMCAC
00460      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00461            ADDRESS OF CSAC-ACCUMULATOR-TABLE                      ELXPMCAC
00462      IF CIA-RC-OK                                                 ELXPMCAC
00463         SET WS-CSAC-OK TO TRUE                                    ELXPMCAC
00464      ELSE                                                         ELXPMCAC
00465         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCAC
00466         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00467         MOVE +4012 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
00468         SET WS-CSAC-NOT-FOUND TO TRUE.                            ELXPMCAC
00469                                                                   ELXPMCAC
00470 ************************************************************      ELXPMCAC
00471 *                                                          *      ELXPMCAC
00472 *    ESTABLISH ADDRESSABLITY OF BENEFIT PROVISIONS TABULAR *      ELXPMCAC
00473 *                                                          *      ELXPMCAC
00474 ************************************************************      ELXPMCAC
00475                                                                   ELXPMCAC
00476  1101-EST-ADDRS-OF-IBGR.                                          ELXPMCAC
00477      SET CIA-ELSIBGR-DDN TO TRUE.                                 ELXPMCAC
00478      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00479                   ADDRESS OF IBGR-INTERNAL-TABS-TABLE.            ELXPMCAC
00480                                                                   ELXPMCAC
00481 ************************************************************      ELXPMCAC
00482 *                                                          *      ELXPMCAC
00483 *    ESTABLISH ADDRESSABLITY OF DIAGNOSIS TABULAR          *      ELXPMCAC
00484 *                                                          *      ELXPMCAC
00485 ************************************************************      ELXPMCAC
00486                                                                   ELXPMCAC
00487  1102-EST-ADDRS-OF-IDGD.                                          ELXPMCAC
00488      SET CIA-ELSIDGD-DDN TO TRUE.                                 ELXPMCAC
00489      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00490                   ADDRESS OF IDGD-INTERNAL-TABS-TABLE.            ELXPMCAC
00491                                                                   ELXPMCAC
00492 ************************************************************      ELXPMCAC
00493 *                                                          *      ELXPMCAC
00494 *    ESTABLISH ADDRESSABLITY OF PROVIDER NUMBER TABULAR    *      ELXPMCAC
00495 *                                                          *      ELXPMCAC
00496 ************************************************************      ELXPMCAC
00497                                                                   ELXPMCAC
00498  1103-EST-ADDRS-OF-IPGN.                                          ELXPMCAC
00499      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELXPMCAC
00500      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00501                   ADDRESS OF IPGN-INTERNAL-TABS-TABLE.            ELXPMCAC
00502                                                                   ELXPMCAC
00503 ************************************************************      ELXPMCAC
00504 *                                                          *      ELXPMCAC
00505 *    ESTABLISH ADDRESSABLITY OF PROCEDURE CODE TABULAR     *      ELXPMCAC
00506 *                                                          *      ELXPMCAC
00507 ************************************************************      ELXPMCAC
00508                                                                   ELXPMCAC
00509  1104-EST-ADDRS-OF-IPGP.                                          ELXPMCAC
00510      SET CIA-ELSIPGP-DDN TO TRUE.                                 ELXPMCAC
00511      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00512                   ADDRESS OF IPGP-INTERNAL-TABS-TABLE.            ELXPMCAC
00513                                                                   ELXPMCAC
00514 ************************************************************      ELXPMCAC
00515 *                                                          *      ELXPMCAC
00516 *    ESTABLISH ADDRESSABLITY OF PROVIDER TYPE TABULAR      *      ELXPMCAC
00517 *                                                          *      ELXPMCAC
00518 ************************************************************      ELXPMCAC
00519                                                                   ELXPMCAC
00520  1105-EST-ADDRS-OF-IPGT.                                          ELXPMCAC
00521      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELXPMCAC
00522      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00523                   ADDRESS OF IPGT-INTERNAL-TABS-TABLE.            ELXPMCAC
00524                                                                   ELXPMCAC
00525 ************************************************************      ELXPMCAC
00526 *                                                          *      ELXPMCAC
00527 *    ESTABLISH ADDRESSABLITY OF PROVIDER SPEC TABULAR      *      ELXPMCAC
00528 *                                                          *      ELXPMCAC
00529 ************************************************************      ELXPMCAC
00530                                                                   ELXPMCAC
00531  1106-EST-ADDRS-OF-IPGS.                                          ELXPMCAC
00532      SET CIA-ELSIPGS-DDN TO TRUE.                                 ELXPMCAC
00533      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCAC
00534                   ADDRESS OF IPGS-INTERNAL-TABS-TABLE.            ELXPMCAC
00535                                                                   ELXPMCAC
00536 /***********************************************************      ELXPMCAC
00537 *                                                          *      ELXPMCAC
00538 *    RECALCULATE PROVIDER BASED CONFIDENCE FACTORS         *      ELXPMCAC
00539 *                                                          *      ELXPMCAC
00540 ************************************************************      ELXPMCAC
00541                                                                   ELXPMCAC
00542  1110-RECALC-PROV-BASED-CFS.                                      ELXPMCAC
00543      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE NOT = NULL            ELXPMCAC
00544         PERFORM 1200-RECALC-PROV-NO-BASED-CF                      ELXPMCAC
00545         MOVE RETURN-CODE TO WS-RETURN-CODE                        ELXPMCAC
00546         IF WS-SUCCESSFUL-CALL                                     ELXPMCAC
00547             PERFORM 1250-COMPLETE-RECALC-PROV-CFS                 ELXPMCAC
00548         ELSE                                                      ELXPMCAC
00549             SET PMCI-BC-INTERNAL-ERROR TO TRUE                    ELXPMCAC
00550             MOVE +4010 TO PMCI-BLUE-CHIP-ERROR-CODE               ELXPMCAC
00551         END-IF                                                    ELXPMCAC
00552         IF WS-SUCCESSFUL-CALL                                     ELXPMCAC
00553             PERFORM 1251-COMPLETE-RECALC-SPEC-CFS                 ELXPMCAC
00554         ELSE                                                      ELXPMCAC
00555             SET PMCI-BC-INTERNAL-ERROR TO TRUE                    ELXPMCAC
00556             MOVE +4011 TO PMCI-BLUE-CHIP-ERROR-CODE               ELXPMCAC
00557         END-IF                                                    ELXPMCAC
00558      END-IF.                                                      ELXPMCAC
00559                                                                   ELXPMCAC
00560 ************************************************************      ELXPMCAC
00561 *                                                          *      ELXPMCAC
00562 *    RECALCULATE PROVIDER NUMBER BASED CONFIDENCE FACTOR   *      ELXPMCAC
00563 *                                                          *      ELXPMCAC
00564 ************************************************************      ELXPMCAC
00565                                                                   ELXPMCAC
00566  1200-RECALC-PROV-NO-BASED-CF.                                    ELXPMCAC
00567      SET PVNL-IDX TO 1.                                           ELXPMCAC
00568      MOVE PMCI-PROVIDER-NUMBER                                    ELXPMCAC
00569        TO PVNL-PRVDR-NBR (PVNL-IDX).                              ELXPMCAC
00570      SET IPGN-CF-CALC-MTCH TO TRUE.                               ELXPMCAC
00571      MOVE +1 TO PVNL-NBR-ENTRS.                                   ELXPMCAC
00572      CALL 'ELKIPGNF' USING IPGN-INTERNAL-TABS-TABLE               ELXPMCAC
00573                            PVNL-PRVDR-NBR-TBL.                    ELXPMCAC
00574      IF NOT WS-SUCCESSFUL-CALL                                    ELXPMCAC
00575         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00576         MOVE +4010 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAC
00577                                                                   ELXPMCAC
00578 ************************************************************      ELXPMCAC
00579 *                                                          *      ELXPMCAC
00580 * COMPLETE RECALCUALTING PROVIDER BASED CONFIDENCE FACTORS *      ELXPMCAC
00581 *                                                          *      ELXPMCAC
00582 ************************************************************      ELXPMCAC
00583                                                                   ELXPMCAC
00584  1250-COMPLETE-RECALC-PROV-CFS.                                   ELXPMCAC
00585      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE NOT = NULL            ELXPMCAC
00586         PERFORM 1300-RECALC-PROV-TYPE-BASED-CF.                   ELXPMCAC
00587      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00588         PERFORM 1400-COPY-PRV-CFS-TO-ATBL                         ELXPMCAC
00589             VARYING CSAC-X-IDX FROM 1 BY 1                        ELXPMCAC
00590                UNTIL CSAC-X-IDX > 5.                              ELXPMCAC
00591                                                                   ELXPMCAC
00592 ************************************************************      ELXPMCAC
00593 *                                                          *      ELXPMCAC
00594 * COMPLETE RECALC'ING SPECIALTY   BASED CONFIDENCE FACTORS *      ELXPMCAC
00595 *                                                          *      ELXPMCAC
00596 ************************************************************      ELXPMCAC
00597                                                                   ELXPMCAC
00598  1251-COMPLETE-RECALC-SPEC-CFS.                                   ELXPMCAC
00599      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE NOT = NULL            ELXPMCAC
00600         PERFORM 1301-RECALC-PROV-SPEC-BASED-CF.                   ELXPMCAC
00601      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00602         PERFORM 1400-COPY-PRV-CFS-TO-ATBL                         ELXPMCAC
00603             VARYING CSAC-X-IDX FROM 1 BY 1                        ELXPMCAC
00604                UNTIL CSAC-X-IDX > 4.                              ELXPMCAC
00605                                                                   ELXPMCAC
00606 ************************************************************      ELXPMCAC
00607 *                                                          *      ELXPMCAC
00608 *    RECALCULATE PROVIDER TYPE   BASED CONFIDENCE FACTOR   *      ELXPMCAC
00609 *                                                          *      ELXPMCAC
00610 ************************************************************      ELXPMCAC
00611                                                                   ELXPMCAC
00612  1300-RECALC-PROV-TYPE-BASED-CF.                                  ELXPMCAC
00613      SET PVNL-IDX TO 1.                                           ELXPMCAC
00614      MOVE PMCI-PROVIDER-TYPE                                      ELXPMCAC
00615        TO PVTL-PRVDR-TYP (PVTL-IDX).                              ELXPMCAC
00616      SET IPGT-CF-CALC-MTCH TO TRUE.                               ELXPMCAC
00617      MOVE +1 TO PVTL-NBR-ENTRS.                                   ELXPMCAC
00618      CALL 'ELKIPGTF' USING IPGT-INTERNAL-TABS-TABLE               ELXPMCAC
00619                            PVTL-PRVDR-TYP-TBL.                    ELXPMCAC
00620      IF NOT WS-SUCCESSFUL-CALL                                    ELXPMCAC
00621         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00622         MOVE +4011 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAC
00623                                                                   ELXPMCAC
00624 ************************************************************      ELXPMCAC
00625 *                                                          *      ELXPMCAC
00626 *    RECALCULATE PROVIDER SPEC   BASED CONFIDENCE FACTOR   *      ELXPMCAC
00627 *                                                          *      ELXPMCAC
00628 ************************************************************      ELXPMCAC
00629                                                                   ELXPMCAC
00630  1301-RECALC-PROV-SPEC-BASED-CF.                                  ELXPMCAC
00631      SET PVSL-IDX TO 1.                                           ELXPMCAC
00632      MOVE 0 TO PVSL-PRVDR-SPC (PVSL-IDX).                         ELXPMCAC
00633      MOVE PMCI-PROVIDER-SPECIALTY                                 ELXPMCAC
00634        TO PVSL-PRVDR-SPC (PVSL-IDX).                              ELXPMCAC
00635      SET IPGS-CF-CALC-MTCH TO TRUE.                               ELXPMCAC
00636      MOVE +1 TO PVSL-NBR-ENTRS.                                   ELXPMCAC
00637      CALL 'ELKIPGSF' USING IPGS-INTERNAL-TABS-TABLE               ELXPMCAC
00638                            PVSL-PRVDR-SPC-TBL.                    ELXPMCAC
00639      IF NOT WS-SUCCESSFUL-CALL                                    ELXPMCAC
00640         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00641         MOVE +4013 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCAC
00642                                                                   ELXPMCAC
00643 ************************************************************      ELXPMCAC
00644 *                                                          *      ELXPMCAC
00645 * COPY PROVIDER BASED CONFIDENCE FACTORS TO ATBL           *      ELXPMCAC
00646 *                                                          *      ELXPMCAC
00647 ************************************************************      ELXPMCAC
00648                                                                   ELXPMCAC
00649  1400-COPY-PRV-CFS-TO-ATBL.                                       ELXPMCAC
00650      IF CIA-RC-OK                                                 ELXPMCAC
00651         SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                     ELXPMCAC
00652                     TO CSAC-GC-TBL-PTR (CSAC-X-IDX)               ELXPMCAC
00653         IF ADDRESS OF ATBL-ACCUMULATOR-TABLE NOT = NULL           ELXPMCAC
00654             SET ATBL-MAX-IDX TO ATBL-TBL-CNT                      ELXPMCAC
00655             PERFORM 1410-GET-PV-CFS-FR-EACH-ATBL                  ELXPMCAC
00656                 VARYING ATBL-IDX FROM 1 BY 1                      ELXPMCAC
00657                    UNTIL ATBL-IDX > ATBL-MAX-IDX                  ELXPMCAC
00658         END-IF                                                    ELXPMCAC
00659      END-IF.                                                      ELXPMCAC
00660 ************************************************************      ELXPMCAC
00661 *                                                          *      ELXPMCAC
00662 * GET PROVIDER BASED CONFIDENCE FACTORS FOR EACH ENTRY IN  *      ELXPMCAC
00663 *   ATBL TABLE                                             *      ELXPMCAC
00664 *                                                          *      ELXPMCAC
00665 ************************************************************      ELXPMCAC
00666                                                                   ELXPMCAC
00667  1410-GET-PV-CFS-FR-EACH-ATBL.                                    ELXPMCAC
00668      IF ATBL-IPGN-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELXPMCAC
00669         MOVE WS-CF-ZERO TO ATBL-CF-OV-PRVDR-NBR (ATBL-IDX)        ELXPMCAC
00670      ELSE                                                         ELXPMCAC
00671         PERFORM 1510-LOOK-UP-PROV-NO-CF.                          ELXPMCAC
00672      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00673         PERFORM 1600-GET-PROV-TYPE-BASED-CF.                      ELXPMCAC
00674      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00675         PERFORM 1601-GET-PROV-SPEC-BASED-CF.                      ELXPMCAC
00676                                                                   ELXPMCAC
00677                                                                   ELXPMCAC
00678 ************************************************************      ELXPMCAC
00679 *                                                          *      ELXPMCAC
00680 *   LOOK UP PROVIDER NUMBER CONFIDENCE FACTOR              *      ELXPMCAC
00681 *                                                          *      ELXPMCAC
00682 ************************************************************      ELXPMCAC
00683                                                                   ELXPMCAC
00684  1510-LOOK-UP-PROV-NO-CF.                                         ELXPMCAC
00685      IF ADDRESS OF IPGN-INTERNAL-TABS-TABLE = NULL                ELXPMCAC
00686         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00687         MOVE +4010 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
00688      ELSE                                                         ELXPMCAC
00689         PERFORM 1520-LOOKUP-PV-NO-CF-IN-IPGN.                     ELXPMCAC
00690                                                                   ELXPMCAC
00691 ************************************************************      ELXPMCAC
00692 *                                                          *      ELXPMCAC
00693 *   LOOK UP PROVIDER NUMBER CONFIDENCE FACTOR IN IPGN      *      ELXPMCAC
00694 *        CONFIDENCE FACTOR TABLE                           *      ELXPMCAC
00695 *                                                          *      ELXPMCAC
00696 ************************************************************      ELXPMCAC
00697                                                                   ELXPMCAC
00698  1520-LOOKUP-PV-NO-CF-IN-IPGN.                                    ELXPMCAC
00699      SET WS-IPGN-SLOT-NO-NOT-FOUND TO TRUE.                       ELXPMCAC
00700      PERFORM 1530-SCAN-IPGN-CF-TBL-FR-SLOT                        ELXPMCAC
00701          VARYING IPGN-IDX FROM 1 BY 1                             ELXPMCAC
00702            UNTIL WS-IPGN-SLOT-NO-FOUND OR                         ELXPMCAC
00703                  IPGN-IDX > IPGN-TBL-CNT.                         ELXPMCAC
00704                                                                   ELXPMCAC
00705      IF WS-IPGN-SLOT-NO-NOT-FOUND SET                             ELXPMCAC
00706         WS-IPGN-SLOT-NO-NOT-IN-TABLE TO TRUE.                     ELXPMCAC
00707                                                                   ELXPMCAC
00708 ************************************************************      ELXPMCAC
00709 *                                                          *      ELXPMCAC
00710 *   SCAN IPGN CONFIDENCE FACTOR TABLE FOR SLOT NUMBER      *      ELXPMCAC
00711 *                                                          *      ELXPMCAC
00712 ************************************************************      ELXPMCAC
00713                                                                   ELXPMCAC
00714  1530-SCAN-IPGN-CF-TBL-FR-SLOT.                                   ELXPMCAC
00715      IF ATBL-IPGN-SLOT-NUMBER (ATBL-IDX) =                        ELXPMCAC
00716         IPGN-SLOT-NUMBER (IPGN-IDX)                               ELXPMCAC
00717         SET WS-IPGN-SLOT-NO-FOUND TO TRUE                         ELXPMCAC
00718         MOVE IPGN-CF-LIST-MTCH (IPGN-IDX)                         ELXPMCAC
00719           TO ATBL-CF-OV-PRVDR-NBR (ATBL-IDX).                     ELXPMCAC
00720                                                                   ELXPMCAC
00721 ************************************************************      ELXPMCAC
00722 *                                                          *      ELXPMCAC
00723 *   GET PROVIDER TYPE   BASED CONFIDENCE FACTOR            *      ELXPMCAC
00724 *                                                          *      ELXPMCAC
00725 ************************************************************      ELXPMCAC
00726                                                                   ELXPMCAC
00727  1600-GET-PROV-TYPE-BASED-CF.                                     ELXPMCAC
00728      IF ATBL-IPGT-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELXPMCAC
00729         MOVE WS-CF-ZERO TO ATBL-CF-OV-PRVDR-TYP (ATBL-IDX)        ELXPMCAC
00730      ELSE                                                         ELXPMCAC
00731         PERFORM 1610-LOOK-UP-PROV-TP-CF.                          ELXPMCAC
00732                                                                   ELXPMCAC
00733 ************************************************************      ELXPMCAC
00734 *                                                          *      ELXPMCAC
00735 *   GET PROVIDER SPEC   BASED CONFIDENCE FACTOR            *      ELXPMCAC
00736 *                                                          *      ELXPMCAC
00737 ************************************************************      ELXPMCAC
00738                                                                   ELXPMCAC
00739  1601-GET-PROV-SPEC-BASED-CF.                                     ELXPMCAC
00740      IF ATBL-IPGS-SLOT-NUMBER (ATBL-IDX) = ZERO                   ELXPMCAC
00741         MOVE WS-CF-ZERO TO ATBL-CF-OV-PRVDR-SPC (ATBL-IDX)        ELXPMCAC
00742      ELSE                                                         ELXPMCAC
00743         PERFORM 1611-LOOK-UP-PROV-SP-CF.                          ELXPMCAC
00744                                                                   ELXPMCAC
00745 ************************************************************      ELXPMCAC
00746 *                                                          *      ELXPMCAC
00747 *   LOOK UP PROVIDER TYPE   CONFIDENCE FACTOR              *      ELXPMCAC
00748 *                                                          *      ELXPMCAC
00749 ************************************************************      ELXPMCAC
00750                                                                   ELXPMCAC
00751  1611-LOOK-UP-PROV-SP-CF.                                         ELXPMCAC
00752      IF ADDRESS OF IPGS-INTERNAL-TABS-TABLE = NULL                ELXPMCAC
00753         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00754         MOVE +4013 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
00755      ELSE                                                         ELXPMCAC
00756         PERFORM 1621-LOOKUP-PV-SP-CF-IN-IPGS.                     ELXPMCAC
00757                                                                   ELXPMCAC
00758 ************************************************************      ELXPMCAC
00759 *                                                          *      ELXPMCAC
00760 *   LOOK UP PROVIDER TYPE   CONFIDENCE FACTOR              *      ELXPMCAC
00761 *                                                          *      ELXPMCAC
00762 ************************************************************      ELXPMCAC
00763                                                                   ELXPMCAC
00764  1610-LOOK-UP-PROV-TP-CF.                                         ELXPMCAC
00765      IF ADDRESS OF IPGT-INTERNAL-TABS-TABLE = NULL                ELXPMCAC
00766         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00767         MOVE +4011 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
00768      ELSE                                                         ELXPMCAC
00769         PERFORM 1620-LOOKUP-PV-TP-CF-IN-IPGT.                     ELXPMCAC
00770                                                                   ELXPMCAC
00771 ************************************************************      ELXPMCAC
00772 *                                                          *      ELXPMCAC
00773 *   LOOK UP PROVIDER TYPE   CONFIDENCE FACTOR IN IPGT      *      ELXPMCAC
00774 *        CONFIDENCE FACTOR TABLE                           *      ELXPMCAC
00775 *                                                          *      ELXPMCAC
00776 ************************************************************      ELXPMCAC
00777                                                                   ELXPMCAC
00778  1620-LOOKUP-PV-TP-CF-IN-IPGT.                                    ELXPMCAC
00779      SET WS-IPGT-SLOT-NO-NOT-FOUND TO TRUE.                       ELXPMCAC
00780      PERFORM 1630-SCAN-IPGT-CF-TBL-FR-SLOT                        ELXPMCAC
00781          VARYING IPGT-IDX FROM 1 BY 1                             ELXPMCAC
00782            UNTIL WS-IPGT-SLOT-NO-FOUND OR                         ELXPMCAC
00783                  IPGT-IDX > IPGT-TBL-CNT.                         ELXPMCAC
00784                                                                   ELXPMCAC
00785      IF WS-IPGT-SLOT-NO-NOT-FOUND SET                             ELXPMCAC
00786         WS-IPGT-SLOT-NO-NOT-IN-TABLE TO TRUE.                     ELXPMCAC
00787                                                                   ELXPMCAC
00788 ************************************************************      ELXPMCAC
00789 *                                                          *      ELXPMCAC
00790 *   LOOK UP PROVIDER SPEC   CONFIDENCE FACTOR IN IPGS      *      ELXPMCAC
00791 *        CONFIDENCE FACTOR TABLE                           *      ELXPMCAC
00792 *                                                          *      ELXPMCAC
00793 ************************************************************      ELXPMCAC
00794                                                                   ELXPMCAC
00795  1621-LOOKUP-PV-SP-CF-IN-IPGS.                                    ELXPMCAC
00796      SET WS-IPGS-SLOT-NO-NOT-FOUND TO TRUE.                       ELXPMCAC
00797      PERFORM 1631-SCAN-IPGS-CF-TBL-FR-SLOT                        ELXPMCAC
00798          VARYING IPGS-IDX FROM 1 BY 1                             ELXPMCAC
00799            UNTIL WS-IPGS-SLOT-NO-FOUND OR                         ELXPMCAC
00800                  IPGS-IDX > IPGS-TBL-CNT.                         ELXPMCAC
00801                                                                   ELXPMCAC
00802      IF WS-IPGS-SLOT-NO-NOT-FOUND SET                             ELXPMCAC
00803         WS-IPGS-SLOT-NO-NOT-IN-TABLE TO TRUE.                     ELXPMCAC
00804                                                                   ELXPMCAC
00805 ************************************************************      ELXPMCAC
00806 *                                                          *      ELXPMCAC
00807 *   SCAN IPGS CONFIDENCE FACTOR TABLE FOR SLOT NUMBER      *      ELXPMCAC
00808 *                                                          *      ELXPMCAC
00809 ************************************************************      ELXPMCAC
00810                                                                   ELXPMCAC
00811  1631-SCAN-IPGS-CF-TBL-FR-SLOT.                                   ELXPMCAC
00812      IF ATBL-IPGS-SLOT-NUMBER (ATBL-IDX) =                        ELXPMCAC
00813         IPGS-SLOT-NUMBER (IPGS-IDX)                               ELXPMCAC
00814         SET WS-IPGS-SLOT-NO-FOUND TO TRUE                         ELXPMCAC
00815         MOVE IPGS-CF-LIST-MTCH (IPGS-IDX)                         ELXPMCAC
00816           TO ATBL-CF-OV-PROV-SPEC (ATBL-IDX).                     ELXPMCAC
00817                                                                   ELXPMCAC
00818 ************************************************************      ELXPMCAC
00819 *                                                          *      ELXPMCAC
00820 *   SCAN IPGT CONFIDENCE FACTOR TABLE FOR SLOT NUMBER      *      ELXPMCAC
00821 *                                                          *      ELXPMCAC
00822 ************************************************************      ELXPMCAC
00823                                                                   ELXPMCAC
00824  1630-SCAN-IPGT-CF-TBL-FR-SLOT.                                   ELXPMCAC
00825      IF ATBL-IPGT-SLOT-NUMBER (ATBL-IDX) =                        ELXPMCAC
00826         IPGT-SLOT-NUMBER (IPGT-IDX)                               ELXPMCAC
00827         SET WS-IPGT-SLOT-NO-FOUND TO TRUE                         ELXPMCAC
00828         MOVE IPGT-CF-LIST-MTCH (IPGT-IDX)                         ELXPMCAC
00829           TO ATBL-CF-OV-PROV-TYPE (ATBL-IDX).                     ELXPMCAC
00830                                                                   ELXPMCAC
00831 ************************************************************      ELXPMCAC
00832 *                                                          *      ELXPMCAC
00833 *    PROCESS ALL ACCUMULATORS                              *      ELXPMCAC
00834 *                                                          *      ELXPMCAC
00835 ************************************************************      ELXPMCAC
00836                                                                   ELXPMCAC
00837  2000-PROCESS-ALL-ACCUMS.                                         ELXPMCAC
00838      PERFORM 2100-PROCESS-OVRLL-ACCUMS.                           ELXPMCAC
00839                                                                   ELXPMCAC
00840 ***********************************************************       ELXPMCAC
00841 *                         NOTE                            *       ELXPMCAC
00842 ***********************************************************       ELXPMCAC
00843 * AFTER THE OVERALL ACCUMS ARE PROCESSED, EXLPMCAP IS     *       ELXPMCAC
00844 * CALLED TO PROCESS THE COST CONTAINMENT ACCUMS, THEN     *       ELXPMCAC
00845 * ELXPMCBM IS CALLED TO PROCESS THE BENEFIT PROVISION     *       ELXPMCAC
00846 * ACCUMS FOR MAXIMUMS.  ELXPMCBC FOR COPAYS AND ELSPMCBI  *       ELXPMCAC
00847 * FOR OTHER COINSURANCE.                                  *       ELXPMCAC
00848 ***********************************************************       ELXPMCAC
00849      IF PMCI-BC-SUCCESSFUL                                        ELXPMCAC
00850         CALL 'ELXPMCAP' USING DFHCOMMAREA                         ELXPMCAC
00851                               PMCI-COMM-AREA                      ELXPMCAC
00852                               ATBL-ACCUMULATOR-TABLE              ELXPMCAC
00853                               CSAC-ACCUMULATOR-TABLE              ELXPMCAC
00854                               GROUP-SPECIFIC-RECORD               ELXPMCAC
00855         IF PMCI-BC-SUCCESSFUL                                     ELXPMCAC
00856            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCAC
00857            MOVE +4004 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCAC
00858            SET CIA-ELSPMCID-DDN TO TRUE                           ELXPMCAC
00859            CALL 'ELUSETAD' USING DFHCOMMAREA                      ELXPMCAC
00860                     ADDRESS OF NAES-INTERMEDIATE-DATA             ELXPMCAC
00861           IF CIA-RC-OK                                            ELXPMCAC
00862              SET PMCI-BC-SUCCESSFUL TO TRUE                       ELXPMCAC
00863              MOVE +0 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCAC
00864              CALL 'ELXPMCBM' USING PMCI-COMM-AREA                 ELXPMCAC
00865                                    NAES-INTERMEDIATE-DATA         ELXPMCAC
00866                                    CSAC-ACCUMULATOR-TABLE         ELXPMCAC
00867                                    IBGR-INTERNAL-TABS-TABLE       ELXPMCAC
00868              IF PMCI-BC-SUCCESSFUL                                ELXPMCAC
00869                 CALL 'ELXPMCBC' USING PMCI-COMM-AREA              ELXPMCAC
00870                                       NAES-INTERMEDIATE-DATA      ELXPMCAC
00871                                       CSAC-ACCUMULATOR-TABLE      ELXPMCAC
00872                                       IBGR-INTERNAL-TABS-TABLE    ELXPMCAC
00873                                  ACCDE-ATBL-ACCUMULATOR-TABLE     ELXPMCAC
00874                                                                   ELXPMCAC
00875               IF PMCI-BC-SUCCESSFUL                               ELXPMCAC
00876                  SET PMCI-ACCUM-PTR TO ADDRESS OF                 ELXPMCAC
00877                      ACCDE-ATBL-ACCUMULATOR-TABLE                 ELXPMCAC
00878                                                                   ELXPMCAC
00879                 IF PMCI-BC-SUCCESSFUL                             ELXPMCAC
00880                    CALL 'ELXPMCBI' USING PMCI-COMM-AREA           ELXPMCAC
00881                                        NAES-INTERMEDIATE-DATA     ELXPMCAC
00882                                        CSAC-ACCUMULATOR-TABLE     ELXPMCAC
00883                                        IBGR-INTERNAL-TABS-TABLE.  ELXPMCAC
00884 ************************************************************      ELXPMCAC
00885 *                                                          *      ELXPMCAC
00886 *    PROCESS CONTRACT OVERALL ACCUMS                       *      ELXPMCAC
00887 *                                                          *      ELXPMCAC
00888 ************************************************************      ELXPMCAC
00889                                                                   ELXPMCAC
00890  2100-PROCESS-OVRLL-ACCUMS.                                       ELXPMCAC
00891                                                                   ELXPMCAC
00892      IF ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE = NULL            ELXPMCAC
00893         PERFORM 5400-CREATE-ACCUM-CDE-TABLE                       ELXPMCAC
00894         MOVE ZERO TO AC-ATBL-TBL-CNT                              ELXPMCAC
00895      END-IF                                                       ELXPMCAC
00896                                                                   ELXPMCAC
00897      IF CSAC-ABM-GC-TBL-PTR NOT = NULL                            ELXPMCAC
00898         PERFORM 2110-FIND-OVRLL-CNTRCT-MAX                        ELXPMCAC
00899      ELSE                                                         ELXPMCAC
00900         SET PMCI-LM-NO-LIMIT TO TRUE.                             ELXPMCAC
00901                                                                   ELXPMCAC
00902      IF PMCI-BC-SUCCESSFUL AND                                    ELXPMCAC
00903         CSAC-ACL-GC-TBL-PTR NOT = NULL                            ELXPMCAC
00904         SET PROCESSING-ACL TO TRUE                                ELXPMCAC
00905         PERFORM 2120-FIND-OVRLL-CNTRCT-COINS                      ELXPMCAC
00906      ELSE SET PMCI-COINS-NONE TO TRUE.                            ELXPMCAC
00907      SET NOT-PROCESSING-ACL TO TRUE.                              ELXPMCAC
00908                                                                   ELXPMCAC
00909      IF PMCI-BC-SUCCESSFUL AND                                    ELXPMCAC
00910         CSAC-ADL-GC-TBL-PTR NOT = NULL                            ELXPMCAC
00911         PERFORM 2130-FIND-OVRLL-CNTRCT-DED                        ELXPMCAC
00912      ELSE SET PMCI-DEDI-NO-IND-DEDUCTIBLE TO TRUE                 ELXPMCAC
00913           SET PMCI-DEDF-NO-FAM-DEDUCTIBLE TO TRUE.                ELXPMCAC
00914                                                                   ELXPMCAC
00915      IF PMCI-BC-SUCCESSFUL AND                                    ELXPMCAC
00916         CSAC-AOL-GC-TBL-PTR NOT = NULL                            ELXPMCAC
00917         PERFORM 2160-FIND-OVRLL-CNTRCT-OPX                        ELXPMCAC
00918      ELSE SET OPX-NO-LIMIT TO TRUE.                               ELXPMCAC
00919                                                                   ELXPMCAC
00920                                                                   ELXPMCAC
00921 ************************************************************      ELXPMCAC
00922 ************************************************************      ELXPMCAC
00923 *                                                          *      ELXPMCAC
00924 *     OVERALL CONTRACT MAXIMUMS PROCESSING SECTION         *      ELXPMCAC
00925 *                                                          *      ELXPMCAC
00926 ************************************************************      ELXPMCAC
00927 ************************************************************      ELXPMCAC
00928                                                                   ELXPMCAC
00929                                                                   ELXPMCAC
00930 ************************************************************      ELXPMCAC
00931 *                                                          *      ELXPMCAC
00932 *    SEARCH FOR OVERALL CONTRACT MAXIMUMS                  *      ELXPMCAC
00933 *                                                          *      ELXPMCAC
00934 ************************************************************      ELXPMCAC
00935                                                                   ELXPMCAC
00936  2110-FIND-OVRLL-CNTRCT-MAX.                                      ELXPMCAC
00937      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAC
00938       TO CSAC-ABM-GC-TBL-PTR.                                     ELXPMCAC
00939                                                                   ELXPMCAC
00940      IF  ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULL                 ELXPMCAC
00941          SET PMCI-LM-NO-LIMIT TO TRUE                             ELXPMCAC
00942      ELSE                                                         ELXPMCAC
00943          PERFORM 2111-SCAN-ATBL-FOR-LFTM-MAX.                     ELXPMCAC
00944                                                                   ELXPMCAC
00945 ************************************************************      ELXPMCAC
00946 *                                                          *      ELXPMCAC
00947 *    SCAN ACCUMULATOR TABLE FOR LIFETIME MAXIMUMS          *      ELXPMCAC
00948 *                                                          *      ELXPMCAC
00949 ************************************************************      ELXPMCAC
00950                                                                   ELXPMCAC
00951  2111-SCAN-ATBL-FOR-LFTM-MAX.                                     ELXPMCAC
00952      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAC
00953      PERFORM 4000-MOVE-OVRLLS-TO-SPEC                             ELXPMCAC
00954       VARYING ATBL-IDX FROM 1 BY 1                                ELXPMCAC
00955         UNTIL ATBL-IDX > ATBL-MAX-IDX.                            ELXPMCAC
00956                                                                   ELXPMCAC
00957      IF PMCI-PROGRAM-IND > ZERO                                   ELXPMCAC
00958         PERFORM 3000-RECALC-OV-PER-CCP-FACTOR.                    ELXPMCAC
00959      SET WS-SUCCESSFUL-CALL TO TRUE.                              ELXPMCAC
00960      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAC
00961      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAC
00962      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
00963         MOVE WS-CF-FALSE TO WS-BS-TEST-CF                         ELXPMCAC
00964         MOVE WS-CF-FALSE TO WS-MM-TEST-CF                         ELXPMCAC
00965         MOVE ZEROS TO WS-BS-TEST-SUB                              ELXPMCAC
00966         MOVE ZEROS TO WS-MM-TEST-SUB                              ELXPMCAC
00967         MOVE CVG2-OV-THRSHLD-ABM TO WS-SAVE-THRESHOLD             ELXPMCAC
00968         PERFORM 2112-DTRMN-LFTM-MAX-CALC-PARMS                    ELXPMCAC
00969           VARYING ATBL-IDX FROM 1 BY 1 UNTIL                      ELXPMCAC
00970                   ATBL-IDX > ATBL-MAX-IDX                         ELXPMCAC
00971         IF WS-BS-TEST-SUB = ZERO AND                              ELXPMCAC
00972                    WS-MM-TEST-SUB = ZERO                          ELXPMCAC
00973             SET PMCI-LM-NO-LIMIT TO TRUE                          ELXPMCAC
00974         ELSE                                                      ELXPMCAC
00975             PERFORM 2117-SET-PMCI-LFTM-MAX-INFO                   ELXPMCAC
00976      ELSE                                                         ELXPMCAC
00977         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
00978         EVALUATE TRUE                                             ELXPMCAC
00979            WHEN WS-UNIDENTIFIED-PARAMETER                         ELXPMCAC
00980               MOVE +4007 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00981            WHEN WS-MISSING-PARAMETER                              ELXPMCAC
00982               MOVE +4008 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00983            WHEN WS-INTERNAL-ERROR                                 ELXPMCAC
00984               MOVE +4009 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
00985         END-EVALUATE                                              ELXPMCAC
00986      END-IF.                                                      ELXPMCAC
00987                                                                   ELXPMCAC
00988 ************************************************************      ELXPMCAC
00989 *                                                          *      ELXPMCAC
00990 *    DETERMINE PARAMETERS FOR LIFETIME MAXIMUM CALCULATION *      ELXPMCAC
00991 *                                                          *      ELXPMCAC
00992 ************************************************************      ELXPMCAC
00993                                                                   ELXPMCAC
00994  2112-DTRMN-LFTM-MAX-CALC-PARMS.                                  ELXPMCAC
00995      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAC
00996         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
00997            IF PMCI-INPATIENT                                      ELXPMCAC
00998               PERFORM 2113-CALC-MAX-INP-LFTM-INST-CF              ELXPMCAC
00999            ELSE                                                   ELXPMCAC
01000               PERFORM 2114-CALC-MAX-OUT-LFTM-INST-CF              ELXPMCAC
01001         ELSE                                                      ELXPMCAC
01002            IF PMCI-INPATIENT                                      ELXPMCAC
01003               PERFORM 2115-CALC-MAX-INP-LFTM-PROF-CF              ELXPMCAC
01004            ELSE                                                   ELXPMCAC
01005               PERFORM 2116-CALC-MAX-OUT-LFTM-PROF-CF.             ELXPMCAC
01006                                                                   ELXPMCAC
01007      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACE                        ELXPMCAC
01008         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
01009            IF PMCI-INPATIENT                                      ELXPMCAC
01010               PERFORM 2113M-CLC-MAX-INP-LFTM-INST-CF              ELXPMCAC
01011            ELSE                                                   ELXPMCAC
01012               PERFORM 2114M-CLC-MAX-OUT-LFTM-INST-CF              ELXPMCAC
01013         ELSE                                                      ELXPMCAC
01014            IF PMCI-INPATIENT                                      ELXPMCAC
01015               PERFORM 2115M-CLC-MAX-INP-LFTM-PROF-CF              ELXPMCAC
01016            ELSE                                                   ELXPMCAC
01017               PERFORM 2116M-CLC-MAX-OUT-LFTM-PROF-CF.             ELXPMCAC
01018                                                                   ELXPMCAC
01019                                                                   ELXPMCAC
01020 ************************************************************      ELXPMCAC
01021 *                                                          *      ELXPMCAC
01022 * CALCULATE MAXIMUM INPATIENT LIFETIME INST CF             *      ELXPMCAC
01023 *                                                          *      ELXPMCAC
01024 ************************************************************      ELXPMCAC
01025  2113-CALC-MAX-INP-LFTM-INST-CF.                                  ELXPMCAC
01026 *   CALCULATE FOR BASIC CONTRACTS FIRST                           ELXPMCAC
01027      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01028                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01029                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01030                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01031                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01032                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01033                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01034                                                                   ELXPMCAC
01035      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01036         WS-CF-TRUE OR                                             ELXPMCAC
01037         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01038         CONTINUE                                                  ELXPMCAC
01039      ELSE                                                         ELXPMCAC
01040         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01041                 WS-WT-LFTM                                        ELXPMCAC
01042         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01043                 WS-WT-INDVDL                                      ELXPMCAC
01044         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01045                 WS-WT-INST-BAS                                    ELXPMCAC
01046         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01047                 WS-WT-IP                                          ELXPMCAC
01048         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01049                 WS-WT-PLAN                                        ELXPMCAC
01050         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01051                 WS-WT-SP                                          ELXPMCAC
01052         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01053      END-IF.                                                      ELXPMCAC
01054                                                                   ELXPMCAC
01055      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01056                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01057         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01058         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01059                                                                   ELXPMCAC
01060  2113M-CLC-MAX-INP-LFTM-INST-CF.                                  ELXPMCAC
01061 *   CALCULATE FOR MAJOR MEDICAL NEXT                              ELXPMCAC
01062      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01063                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01064                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01065                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01066                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01067                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01068                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01069                                                                   ELXPMCAC
01070      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01071         WS-CF-TRUE OR                                             ELXPMCAC
01072         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01073         CONTINUE                                                  ELXPMCAC
01074      ELSE                                                         ELXPMCAC
01075         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01076                 WS-WT-LFTM                                        ELXPMCAC
01077         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01078                 WS-WT-INDVDL                                      ELXPMCAC
01079         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01080                 WS-WT-INST-SUP                                    ELXPMCAC
01081         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01082                 WS-WT-IP                                          ELXPMCAC
01083         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01084                 WS-WT-PLAN                                        ELXPMCAC
01085         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01086                 WS-WT-SP                                          ELXPMCAC
01087         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01088      END-IF.                                                      ELXPMCAC
01089      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01090                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01091         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01092         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01093                                                                   ELXPMCAC
01094 ************************************************************      ELXPMCAC
01095 *                                                          *      ELXPMCAC
01096 *    CALCULATE MAXIMUM INPATIENT LIFETIME INST CF          *      ELXPMCAC
01097 *                                                          *      ELXPMCAC
01098 ************************************************************      ELXPMCAC
01099  2114-CALC-MAX-OUT-LFTM-INST-CF.                                  ELXPMCAC
01100      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01101                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01102                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01103                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01104                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01105                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01106                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01107                                                                   ELXPMCAC
01108      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01109         WS-CF-TRUE OR                                             ELXPMCAC
01110         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01111         CONTINUE                                                  ELXPMCAC
01112      ELSE                                                         ELXPMCAC
01113         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01114                 WS-WT-LFTM                                        ELXPMCAC
01115         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01116                 WS-WT-INDVDL                                      ELXPMCAC
01117         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01118                 WS-WT-INST-BAS                                    ELXPMCAC
01119         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01120                 WS-WT-OP                                          ELXPMCAC
01121         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01122                 WS-WT-PLAN                                        ELXPMCAC
01123         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01124                 WS-WT-SP                                          ELXPMCAC
01125         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01126      END-IF.                                                      ELXPMCAC
01127                                                                   ELXPMCAC
01128      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01129                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01130         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01131         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01132                                                                   ELXPMCAC
01133  2114M-CLC-MAX-OUT-LFTM-INST-CF.                                  ELXPMCAC
01134 *   CALCULATE FOR MAJOR MEDICAL NEXT                              ELXPMCAC
01135      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01136                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01137                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01138                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01139                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01140                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01141                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01142                                                                   ELXPMCAC
01143      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01144         WS-CF-TRUE OR                                             ELXPMCAC
01145         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01146         CONTINUE                                                  ELXPMCAC
01147      ELSE                                                         ELXPMCAC
01148         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01149                 WS-WT-LFTM                                        ELXPMCAC
01150         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01151                 WS-WT-INDVDL                                      ELXPMCAC
01152         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01153                 WS-WT-INST-SUP                                    ELXPMCAC
01154         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01155                 WS-WT-OP                                          ELXPMCAC
01156         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01157                 WS-WT-PLAN                                        ELXPMCAC
01158         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01159                 WS-WT-SP                                          ELXPMCAC
01160         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01161      END-IF.                                                      ELXPMCAC
01162      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01163                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01164         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01165         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01166                                                                   ELXPMCAC
01167                                                                   ELXPMCAC
01168 ************************************************************      ELXPMCAC
01169 *                                                          *      ELXPMCAC
01170 *    CALCULATE MAXIMUM INPATIENT LIFETIME PROF CF          *      ELXPMCAC
01171 *                                                          *      ELXPMCAC
01172 ************************************************************      ELXPMCAC
01173  2115-CALC-MAX-INP-LFTM-PROF-CF.                                  ELXPMCAC
01174      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01175                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01176                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01177                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
01178                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01179                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01180                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01181                                                                   ELXPMCAC
01182      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01183         WS-CF-TRUE OR                                             ELXPMCAC
01184         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01185         CONTINUE                                                  ELXPMCAC
01186      ELSE                                                         ELXPMCAC
01187         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01188                 WS-WT-LFTM                                        ELXPMCAC
01189         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01190                 WS-WT-INDVDL                                      ELXPMCAC
01191         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01192                 WS-WT-INST-BAS                                    ELXPMCAC
01193         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01194                 WS-WT-OP                                          ELXPMCAC
01195         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01196                 WS-WT-PLAN                                        ELXPMCAC
01197         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01198                 WS-WT-SP                                          ELXPMCAC
01199         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01200      END-IF.                                                      ELXPMCAC
01201                                                                   ELXPMCAC
01202      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01203                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01204         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01205         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01206                                                                   ELXPMCAC
01207  2115M-CLC-MAX-INP-LFTM-PROF-CF.                                  ELXPMCAC
01208 *   CALCULATE FOR MAJOR MEDICAL NEXT                              ELXPMCAC
01209      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01210                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01211                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01212                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
01213                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01214                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01215                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01216                                                                   ELXPMCAC
01217      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01218         WS-CF-TRUE OR                                             ELXPMCAC
01219         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01220         CONTINUE                                                  ELXPMCAC
01221      ELSE                                                         ELXPMCAC
01222         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01223                 WS-WT-LFTM                                        ELXPMCAC
01224         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01225                 WS-WT-INDVDL                                      ELXPMCAC
01226         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCAC
01227                 WS-WT-INST-SUP                                    ELXPMCAC
01228         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01229                 WS-WT-OP                                          ELXPMCAC
01230         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01231                 WS-WT-PLAN                                        ELXPMCAC
01232         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01233                 WS-WT-SP                                          ELXPMCAC
01234         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01235      END-IF.                                                      ELXPMCAC
01236      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01237                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01238         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01239         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01240                                                                   ELXPMCAC
01241 ************************************************************      ELXPMCAC
01242 *                                                          *      ELXPMCAC
01243 *    CALCULATE MAXIMUM OUTPATIENT LIFETIME PROF CF         *      ELXPMCAC
01244 *                                                          *      ELXPMCAC
01245 ************************************************************      ELXPMCAC
01246  2116-CALC-MAX-OUT-LFTM-PROF-CF.                                  ELXPMCAC
01247      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01248                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01249                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01250                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
01251                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01252                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01253                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01254                                                                   ELXPMCAC
01255      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01256         WS-CF-TRUE OR                                             ELXPMCAC
01257         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01258         CONTINUE                                                  ELXPMCAC
01259      ELSE                                                         ELXPMCAC
01260         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01261                 WS-WT-LFTM                                        ELXPMCAC
01262         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01263                 WS-WT-INDVDL                                      ELXPMCAC
01264         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01265                 WS-WT-INST-BAS                                    ELXPMCAC
01266         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01267                 WS-WT-OP                                          ELXPMCAC
01268         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01269                 WS-WT-PLAN                                        ELXPMCAC
01270         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01271                 WS-WT-SP                                          ELXPMCAC
01272         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01273      END-IF.                                                      ELXPMCAC
01274                                                                   ELXPMCAC
01275      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01276                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01277         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01278         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01279                                                                   ELXPMCAC
01280  2116M-CLC-MAX-OUT-LFTM-PROF-CF.                                  ELXPMCAC
01281 *   CALCULATE FOR MAJOR MEDICAL NEXT                              ELXPMCAC
01282      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01283                            ATBL-CF-LFTM (ATBL-IDX)                ELXPMCAC
01284                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01285                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
01286                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01287                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01288                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01289                                                                   ELXPMCAC
01290      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01291         WS-CF-TRUE OR                                             ELXPMCAC
01292         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01293         CONTINUE                                                  ELXPMCAC
01294      ELSE                                                         ELXPMCAC
01295         COMPUTE WS-CF-1 = ATBL-CF-LFTM (ATBL-IDX) *               ELXPMCAC
01296                 WS-WT-LFTM                                        ELXPMCAC
01297         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01298                 WS-WT-INDVDL                                      ELXPMCAC
01299         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCAC
01300                 WS-WT-INST-SUP                                    ELXPMCAC
01301         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01302                 WS-WT-OP                                          ELXPMCAC
01303         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01304                 WS-WT-PLAN                                        ELXPMCAC
01305         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01306                 WS-WT-SP                                          ELXPMCAC
01307         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01308      END-IF.                                                      ELXPMCAC
01309      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01310                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01311         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01312         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01313                                                                   ELXPMCAC
01314 ************************************************************      ELXPMCAC
01315 *                                                          *      ELXPMCAC
01316 *    SET PMCI LIFETIME MAXIMUM INFORMATION                 *      ELXPMCAC
01317 *                                                          *      ELXPMCAC
01318 ************************************************************      ELXPMCAC
01319                                                                   ELXPMCAC
01320  2117-SET-PMCI-LFTM-MAX-INFO.                                     ELXPMCAC
01321      PERFORM 5300-SET-ATBL-SUBSCRIPT.                             ELXPMCAC
01322      IF ATBL-VALUE-QUALIFIER (ATBL-IDX) = '5' AND                 ELXPMCAC
01323         ATBL-COND-ALL-BIT (ATBL-IDX)    = '1'                     ELXPMCAC
01324           SET PMCI-LM-DOLLARS TO TRUE                             ELXPMCAC
01325           MOVE ATBL-VALUE-LIMIT (ATBL-IDX) TO PMCI-LIFETIME-MAX   ELXPMCAC
01326           MOVE WS-SAVE-COVER-TYPE TO PMCI-LFTM-MAX-FROM-IND       ELXPMCAC
01327           PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE                  ELXPMCAC
01328      ELSE                                                         ELXPMCAC
01329           SET PMCI-LM-CALL TO TRUE.                               ELXPMCAC
01330                                                                   ELXPMCAC
01331                                                                   ELXPMCAC
01332                                                                   ELXPMCAC
01333 ************************************************************      ELXPMCAC
01334 ************************************************************      ELXPMCAC
01335 *                                                          *      ELXPMCAC
01336 *     OVERALL CONTRACT COINSURANCE PROCESSING SECTION      *      ELXPMCAC
01337 *                                                          *      ELXPMCAC
01338 ************************************************************      ELXPMCAC
01339 ************************************************************      ELXPMCAC
01340                                                                   ELXPMCAC
01341                                                                   ELXPMCAC
01342 ************************************************************      ELXPMCAC
01343 *                                                          *      ELXPMCAC
01344 *    SEARCH FOR OVERALL CONTRACT COINSURANCE               *      ELXPMCAC
01345 *                                                          *      ELXPMCAC
01346 ************************************************************      ELXPMCAC
01347                                                                   ELXPMCAC
01348  2120-FIND-OVRLL-CNTRCT-COINS.                                    ELXPMCAC
01349      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAC
01350       TO CSAC-ACL-GC-TBL-PTR.                                     ELXPMCAC
01351                                                                   ELXPMCAC
01352      IF  ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULL                 ELXPMCAC
01353          SET PMCI-COINS-NONE TO TRUE                              ELXPMCAC
01354      ELSE                                                         ELXPMCAC
01355         PERFORM 2121-SCAN-ATBL-FOR-OVRLL-COINS.                   ELXPMCAC
01356                                                                   ELXPMCAC
01357 ************************************************************      ELXPMCAC
01358 *                                                          *      ELXPMCAC
01359 *    SCAN ACCUMULATOR TABLE FOR OVERALL COINSURANCE        *      ELXPMCAC
01360 *                                                          *      ELXPMCAC
01361 ************************************************************      ELXPMCAC
01362                                                                   ELXPMCAC
01363  2121-SCAN-ATBL-FOR-OVRLL-COINS.                                  ELXPMCAC
01364      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAC
01365      PERFORM 4000-MOVE-OVRLLS-TO-SPEC                             ELXPMCAC
01366       VARYING ATBL-IDX FROM 1 BY 1                                ELXPMCAC
01367         UNTIL ATBL-IDX > ATBL-MAX-IDX.                            ELXPMCAC
01368                                                                   ELXPMCAC
01369      IF PMCI-PROGRAM-IND > ZERO                                   ELXPMCAC
01370         PERFORM 3000-RECALC-OV-PER-CCP-FACTOR.                    ELXPMCAC
01371                                                                   ELXPMCAC
01372      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAC
01373      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAC
01374      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
01375         MOVE WS-CF-FALSE TO WS-BS-TEST-CF                         ELXPMCAC
01376         MOVE WS-CF-FALSE TO WS-MM-TEST-CF                         ELXPMCAC
01377         MOVE ZEROS TO WS-BS-TEST-SUB                              ELXPMCAC
01378         MOVE ZEROS TO WS-MM-TEST-SUB                              ELXPMCAC
01379         MOVE CVG2-OV-THRSHLD-ACL TO WS-SAVE-THRESHOLD             ELXPMCAC
01380         PERFORM 2122-DTRMN-PARMS-FOR-COINS                        ELXPMCAC
01381           VARYING ATBL-IDX FROM 1 BY 1 UNTIL                      ELXPMCAC
01382                   ATBL-IDX > ATBL-MAX-IDX                         ELXPMCAC
01383         IF WS-BS-TEST-SUB = ZERO AND                              ELXPMCAC
01384                    WS-MM-TEST-SUB = ZERO                          ELXPMCAC
01385             SET PMCI-COINS-NONE TO TRUE                           ELXPMCAC
01386         ELSE                                                      ELXPMCAC
01387             PERFORM 2127-SET-PMCI-OVRLL-COINS-INFO                ELXPMCAC
01388      ELSE                                                         ELXPMCAC
01389         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
01390         EVALUATE TRUE                                             ELXPMCAC
01391            WHEN WS-UNIDENTIFIED-PARAMETER                         ELXPMCAC
01392               MOVE +4007 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01393            WHEN WS-MISSING-PARAMETER                              ELXPMCAC
01394               MOVE +4008 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01395            WHEN WS-INTERNAL-ERROR                                 ELXPMCAC
01396               MOVE +4009 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01397         END-EVALUATE                                              ELXPMCAC
01398      END-IF.                                                      ELXPMCAC
01399                                                                   ELXPMCAC
01400                                                                   ELXPMCAC
01401 ************************************************************      ELXPMCAC
01402 *                                                          *      ELXPMCAC
01403 *    DETERMINE PARAMETERS FOR OVERALL COINSURANCE                 ELXPMCAC
01404 *         CALCULATION                                      *      ELXPMCAC
01405 *                                                          *      ELXPMCAC
01406 ************************************************************      ELXPMCAC
01407                                                                   ELXPMCAC
01408  2122-DTRMN-PARMS-FOR-COINS.                                      ELXPMCAC
01409                                                                   ELXPMCAC
01410      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAC
01411         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
01412            IF PMCI-INPATIENT                                      ELXPMCAC
01413               PERFORM 2123-CALC-INST-INP-OVRLL-CN-CF              ELXPMCAC
01414            ELSE                                                   ELXPMCAC
01415               PERFORM 2124-CALC-INST-OUT-OVRLL-CN-CF              ELXPMCAC
01416         ELSE                                                      ELXPMCAC
01417            IF PMCI-INPATIENT                                      ELXPMCAC
01418               PERFORM 2125-CALC-PROF-INP-OVRLL-CN-CF              ELXPMCAC
01419            ELSE                                                   ELXPMCAC
01420               PERFORM 2126-CALC-PROF-OUT-OVRLL-CN-CF.             ELXPMCAC
01421                                                                   ELXPMCAC
01422      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACE                        ELXPMCAC
01423         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
01424            IF PMCI-INPATIENT                                      ELXPMCAC
01425               PERFORM 2123M-CLC-INST-INP-OVRLL-CN-CF              ELXPMCAC
01426            ELSE                                                   ELXPMCAC
01427               PERFORM 2124M-CLC-INST-OUT-OVRLL-CN-CF              ELXPMCAC
01428         ELSE                                                      ELXPMCAC
01429            IF PMCI-INPATIENT                                      ELXPMCAC
01430               PERFORM 2125M-CLC-PROF-INP-OVRLL-CN-CF              ELXPMCAC
01431            ELSE                                                   ELXPMCAC
01432               PERFORM 2126M-CLC-PROF-OUT-OVRLL-CN-CF.             ELXPMCAC
01433                                                                   ELXPMCAC
01434 ************************************************************      ELXPMCAC
01435 *                                                          *      ELXPMCAC
01436 * CALCULATE INST INPATIENT OVERALL CONFIDENCE FACTOR       *      ELXPMCAC
01437 *                                                          *      ELXPMCAC
01438 ************************************************************      ELXPMCAC
01439                                                                   ELXPMCAC
01440  2123-CALC-INST-INP-OVRLL-CN-CF.                                  ELXPMCAC
01441 *   CALCULATE FOR BASIC CONTRACTS FIRST                           ELXPMCAC
01442      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01443                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01444                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01445                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01446                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01447                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01448                                                                   ELXPMCAC
01449      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01450         WS-CF-TRUE OR                                             ELXPMCAC
01451         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01452         CONTINUE                                                  ELXPMCAC
01453      ELSE                                                         ELXPMCAC
01454         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01455                 WS-WT-INDVDL                                      ELXPMCAC
01456         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01457                 WS-WT-INST-BAS                                    ELXPMCAC
01458         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01459                 WS-WT-IP                                          ELXPMCAC
01460         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01461                 WS-WT-PLAN                                        ELXPMCAC
01462         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01463                 WS-WT-SP                                          ELXPMCAC
01464         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01465      END-IF.                                                      ELXPMCAC
01466                                                                   ELXPMCAC
01467      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01468                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01469         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01470         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01471                                                                   ELXPMCAC
01472  2123M-CLC-INST-INP-OVRLL-CN-CF.                                  ELXPMCAC
01473      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01474                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01475                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01476                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01477                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01478                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01479                                                                   ELXPMCAC
01480      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01481         WS-CF-TRUE OR                                             ELXPMCAC
01482         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01483         CONTINUE                                                  ELXPMCAC
01484      ELSE                                                         ELXPMCAC
01485         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01486                 WS-WT-INDVDL                                      ELXPMCAC
01487         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01488                 WS-WT-INST-SUP                                    ELXPMCAC
01489         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01490                 WS-WT-IP                                          ELXPMCAC
01491         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01492                 WS-WT-PLAN                                        ELXPMCAC
01493         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01494                 WS-WT-SP                                          ELXPMCAC
01495         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01496      END-IF.                                                      ELXPMCAC
01497      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01498                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01499         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01500         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01501                                                                   ELXPMCAC
01502                                                                   ELXPMCAC
01503 *************************************************************     ELXPMCAC
01504 *                                                           *     ELXPMCAC
01505 * CALCULATE INST OUTPATIENT OVERALL COINS CONFIDENCE FACTOR *     ELXPMCAC
01506 *                                                           *     ELXPMCAC
01507 *************************************************************     ELXPMCAC
01508                                                                   ELXPMCAC
01509  2124-CALC-INST-OUT-OVRLL-CN-CF.                                  ELXPMCAC
01510      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01511                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01512                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01513                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01514                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01515                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01516                                                                   ELXPMCAC
01517      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01518         WS-CF-TRUE OR                                             ELXPMCAC
01519         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01520         CONTINUE                                                  ELXPMCAC
01521      ELSE                                                         ELXPMCAC
01522         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01523                 WS-WT-INDVDL                                      ELXPMCAC
01524         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01525                 WS-WT-INST-BAS                                    ELXPMCAC
01526         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01527                 WS-WT-OP                                          ELXPMCAC
01528         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01529                 WS-WT-PLAN                                        ELXPMCAC
01530         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01531                 WS-WT-SP                                          ELXPMCAC
01532         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01533      END-IF.                                                      ELXPMCAC
01534                                                                   ELXPMCAC
01535      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01536                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01537         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01538         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01539                                                                   ELXPMCAC
01540  2124M-CLC-INST-OUT-OVRLL-CN-CF.                                  ELXPMCAC
01541      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01542                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01543                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01544                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01545                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01546                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01547                                                                   ELXPMCAC
01548      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01549         WS-CF-TRUE OR                                             ELXPMCAC
01550         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01551         CONTINUE                                                  ELXPMCAC
01552      ELSE                                                         ELXPMCAC
01553         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01554                 WS-WT-INDVDL                                      ELXPMCAC
01555         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01556                 WS-WT-INST-SUP                                    ELXPMCAC
01557         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01558                 WS-WT-OP                                          ELXPMCAC
01559         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01560                 WS-WT-PLAN                                        ELXPMCAC
01561         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01562                 WS-WT-SP                                          ELXPMCAC
01563         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01564      END-IF.                                                      ELXPMCAC
01565      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01566                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01567         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01568         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01569                                                                   ELXPMCAC
01570 ************************************************************      ELXPMCAC
01571 *                                                          *      ELXPMCAC
01572 * CALCULATE PROF INPATIENT OVERALL COINS CONFIDENCE FACTOR *      ELXPMCAC
01573 *                                                          *      ELXPMCAC
01574 ************************************************************      ELXPMCAC
01575                                                                   ELXPMCAC
01576  2125-CALC-PROF-INP-OVRLL-CN-CF.                                  ELXPMCAC
01577      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01578                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01579                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
01580                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01581                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01582                                                                   ELXPMCAC
01583      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01584         WS-CF-TRUE OR                                             ELXPMCAC
01585         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01586         CONTINUE                                                  ELXPMCAC
01587      ELSE                                                         ELXPMCAC
01588         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
01589         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01590                 WS-WT-INDVDL                                      ELXPMCAC
01591         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01592                 WS-WT-PROF-BAS                                    ELXPMCAC
01593         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01594                 WS-WT-IP                                          ELXPMCAC
01595         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01596                 WS-WT-SP                                          ELXPMCAC
01597         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01598      END-IF.                                                      ELXPMCAC
01599                                                                   ELXPMCAC
01600      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01601                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01602         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01603         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01604                                                                   ELXPMCAC
01605  2125M-CLC-PROF-INP-OVRLL-CN-CF.                                  ELXPMCAC
01606      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01607                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01608                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
01609                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01610                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01611                                                                   ELXPMCAC
01612      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01613         WS-CF-TRUE OR                                             ELXPMCAC
01614         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01615         CONTINUE                                                  ELXPMCAC
01616      ELSE                                                         ELXPMCAC
01617         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
01618         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01619                 WS-WT-INDVDL                                      ELXPMCAC
01620         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01621                 WS-WT-PROF-BAS                                    ELXPMCAC
01622         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01623                 WS-WT-IP                                          ELXPMCAC
01624         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01625                 WS-WT-SP                                          ELXPMCAC
01626         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01627      END-IF.                                                      ELXPMCAC
01628                                                                   ELXPMCAC
01629      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01630                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01631         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01632         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01633                                                                   ELXPMCAC
01634 *************************************************************     ELXPMCAC
01635 *                                                           *     ELXPMCAC
01636 * CALCULATE PROF OUTPATIENT OVERALL COINS CONFIDENCE FACTOR *     ELXPMCAC
01637 *                                                           *     ELXPMCAC
01638 *************************************************************     ELXPMCAC
01639                                                                   ELXPMCAC
01640  2126-CALC-PROF-OUT-OVRLL-CN-CF.                                  ELXPMCAC
01641      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01642                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01643                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
01644                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01645                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01646                                                                   ELXPMCAC
01647      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01648         WS-CF-TRUE OR                                             ELXPMCAC
01649         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01650         CONTINUE                                                  ELXPMCAC
01651      ELSE                                                         ELXPMCAC
01652         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
01653         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01654                 WS-WT-INDVDL                                      ELXPMCAC
01655         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01656                 WS-WT-PROF-BAS                                    ELXPMCAC
01657         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01658                 WS-WT-OP                                          ELXPMCAC
01659         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01660                 WS-WT-SP                                          ELXPMCAC
01661         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01662      END-IF.                                                      ELXPMCAC
01663                                                                   ELXPMCAC
01664      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01665                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01666         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01667         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01668                                                                   ELXPMCAC
01669  2126M-CLC-PROF-OUT-OVRLL-CN-CF.                                  ELXPMCAC
01670      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01671                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01672                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
01673                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01674                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01675                                                                   ELXPMCAC
01676      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01677         WS-CF-TRUE OR                                             ELXPMCAC
01678         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
01679         CONTINUE                                                  ELXPMCAC
01680      ELSE                                                         ELXPMCAC
01681         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
01682         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01683                 WS-WT-INDVDL                                      ELXPMCAC
01684         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
01685                 WS-WT-PROF-BAS                                    ELXPMCAC
01686         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01687                 WS-WT-OP                                          ELXPMCAC
01688         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01689                 WS-WT-SP                                          ELXPMCAC
01690         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01691      END-IF.                                                      ELXPMCAC
01692                                                                   ELXPMCAC
01693                                                                   ELXPMCAC
01694      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01695                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01696         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01697         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01698                                                                   ELXPMCAC
01699 ************************************************************      ELXPMCAC
01700 *                                                          *      ELXPMCAC
01701 *    SET PMCI OVERALL COINSURANCE INFORMATION              *      ELXPMCAC
01702 *                                                          *      ELXPMCAC
01703 ************************************************************      ELXPMCAC
01704                                                                   ELXPMCAC
01705  2127-SET-PMCI-OVRLL-COINS-INFO.                                  ELXPMCAC
01706      PERFORM 5300-SET-ATBL-SUBSCRIPT.                             ELXPMCAC
01707      IF PMCI-PRV-CALL                                             ELXPMCAC
01708         IF WS-PRG-VAR-FOUND                                       ELXPMCAC
01709            SET PMCI-COINS-CALL TO TRUE                            ELXPMCAC
01710         ELSE                                                      ELXPMCAC
01711           SET PMCI-COINS-PERCENT TO TRUE                          ELXPMCAC
01712           MOVE WS-SAVE-COVER-TYPE TO PMCI-COINS-FROM-IND          ELXPMCAC
01713           COMPUTE   PMCI-COINS-PERCENT-VALUE =                    ELXPMCAC
01714                100 - ATBL-PERCENT-LEVEL (ATBL-IDX)                ELXPMCAC
01715           PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE                  ELXPMCAC
01716      ELSE                                                         ELXPMCAC
01717         IF ATBL-ASCEND-DESCEND-IND (ATBL-IDX) = '0' AND           ELXPMCAC
01718               ATBL-COND-ALL-BIT (ATBL-IDX)       = '1'            ELXPMCAC
01719            SET PMCI-COINS-PERCENT TO TRUE                         ELXPMCAC
01720            MOVE WS-SAVE-COVER-TYPE TO PMCI-COINS-FROM-IND         ELXPMCAC
01721            COMPUTE   PMCI-COINS-PERCENT-VALUE =                   ELXPMCAC
01722                100 - ATBL-PERCENT-LEVEL (ATBL-IDX)                ELXPMCAC
01723           PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE                  ELXPMCAC
01724         ELSE                                                      ELXPMCAC
01725           SET PMCI-COINS-CALL TO TRUE.                            ELXPMCAC
01726                                                                   ELXPMCAC
01727 ************************************************************      ELXPMCAC
01728 ************************************************************      ELXPMCAC
01729 *                                                          *      ELXPMCAC
01730 *     OVERALL CONTRACT DEDUCTIBLE PROCESSING SECTION       *      ELXPMCAC
01731 *                                                          *      ELXPMCAC
01732 ************************************************************      ELXPMCAC
01733 ************************************************************      ELXPMCAC
01734                                                                   ELXPMCAC
01735                                                                   ELXPMCAC
01736 ************************************************************      ELXPMCAC
01737 *                                                          *      ELXPMCAC
01738 *    SEARCH FOR OVERALL CONTRACT DEDUCTIBLE                *      ELXPMCAC
01739 *                                                          *      ELXPMCAC
01740 ************************************************************      ELXPMCAC
01741                                                                   ELXPMCAC
01742  2130-FIND-OVRLL-CNTRCT-DED.                                      ELXPMCAC
01743      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAC
01744       TO CSAC-ADL-GC-TBL-PTR.                                     ELXPMCAC
01745                                                                   ELXPMCAC
01746      IF  ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULL                 ELXPMCAC
01747         SET PMCI-DEDI-NO-IND-DEDUCTIBLE TO TRUE                   ELXPMCAC
01748         SET PMCI-DEDF-NO-FAM-DEDUCTIBLE TO TRUE                   ELXPMCAC
01749      ELSE                                                         ELXPMCAC
01750         PERFORM 2131-SRCH-ATBL-FOR-OV-DEDBL.                      ELXPMCAC
01751                                                                   ELXPMCAC
01752 ************************************************************      ELXPMCAC
01753 *                                                          *      ELXPMCAC
01754 *    SEARCH ACCUM TABLE FOR OVERALL DEDUCTIBLE             *      ELXPMCAC
01755 *                                                          *      ELXPMCAC
01756 ************************************************************      ELXPMCAC
01757  2131-SRCH-ATBL-FOR-OV-DEDBL.                                     ELXPMCAC
01758      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAC
01759      PERFORM 4000-MOVE-OVRLLS-TO-SPEC                             ELXPMCAC
01760       VARYING ATBL-IDX FROM 1 BY 1                                ELXPMCAC
01761         UNTIL ATBL-IDX > ATBL-MAX-IDX.                            ELXPMCAC
01762                                                                   ELXPMCAC
01763      IF PMCI-PROGRAM-IND > ZERO                                   ELXPMCAC
01764         PERFORM 3000-RECALC-OV-PER-CCP-FACTOR.                    ELXPMCAC
01765                                                                   ELXPMCAC
01766      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAC
01767      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAC
01768      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
01769         MOVE WS-CF-FALSE TO WS-BS-TEST-CF                         ELXPMCAC
01770         MOVE WS-CF-FALSE TO WS-MM-TEST-CF                         ELXPMCAC
01771         MOVE ZEROS TO WS-BS-TEST-SUB                              ELXPMCAC
01772         MOVE ZEROS TO WS-MM-TEST-SUB                              ELXPMCAC
01773         MOVE ZEROS TO WS-BS-INDVD-FOUND                           ELXPMCAC
01774         MOVE ZEROS TO WS-MM-INDVD-FOUND                           ELXPMCAC
01775         MOVE ZEROS TO WS-INDVD-DED-FOUND                          ELXPMCAC
01776         MOVE ZEROS TO WS-FAMILY-DED-FOUND                         ELXPMCAC
01777         MOVE ZEROS TO WS-BS-FML-FOUND                             ELXPMCAC
01778         MOVE ZEROS TO WS-MM-FML-FOUND                             ELXPMCAC
01779         PERFORM 2132-FIND-IND-OVRLL-DEDBL                         ELXPMCAC
01780         PERFORM 2133-FIND-FAM-OVRLL-DEDBL                         ELXPMCAC
01781      ELSE                                                         ELXPMCAC
01782         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
01783         EVALUATE TRUE                                             ELXPMCAC
01784            WHEN WS-UNIDENTIFIED-PARAMETER                         ELXPMCAC
01785               MOVE +4007 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01786            WHEN WS-MISSING-PARAMETER                              ELXPMCAC
01787               MOVE +4008 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01788            WHEN WS-INTERNAL-ERROR                                 ELXPMCAC
01789               MOVE +4009 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
01790         END-EVALUATE                                              ELXPMCAC
01791      END-IF.                                                      ELXPMCAC
01792                                                                   ELXPMCAC
01793                                                                   ELXPMCAC
01794 ************************************************************      ELXPMCAC
01795 *                                                          *      ELXPMCAC
01796 *    FIND INDIVIDUAL OVERALL DEDUCTIBLE                    *      ELXPMCAC
01797 *                                                          *      ELXPMCAC
01798 ************************************************************      ELXPMCAC
01799                                                                   ELXPMCAC
01800  2132-FIND-IND-OVRLL-DEDBL.                                       ELXPMCAC
01801      MOVE WS-CF-FALSE TO WS-BS-TEST-CF.                           ELXPMCAC
01802      MOVE WS-CF-FALSE TO WS-MM-TEST-CF.                           ELXPMCAC
01803      MOVE ZEROS TO WS-BS-TEST-SUB.                                ELXPMCAC
01804      MOVE ZEROS TO WS-MM-TEST-SUB.                                ELXPMCAC
01805      MOVE CVG2-OV-THRSHLD-ACL TO WS-SAVE-THRESHOLD.               ELXPMCAC
01806      PERFORM 2134-SCAN-FOR-IND-OVRLL-DEDBL                        ELXPMCAC
01807           VARYING ATBL-IDX FROM 1 BY 1 UNTIL                      ELXPMCAC
01808                   ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAC
01809      IF WS-BS-TEST-SUB = ZERO AND                                 ELXPMCAC
01810                   WS-MM-TEST-SUB = ZERO                           ELXPMCAC
01811         SET PMCI-DEDI-NO-IND-DEDUCTIBLE TO TRUE                   ELXPMCAC
01812      ELSE                                                         ELXPMCAC
01813         PERFORM 2140-SET-PMCI-IND-OV-DED-INFO.                    ELXPMCAC
01814                                                                   ELXPMCAC
01815 ************************************************************      ELXPMCAC
01816 *                                                          *      ELXPMCAC
01817 *    FIND FAMILY OVERALL DEDUCTIBLE                        *      ELXPMCAC
01818 *                                                          *      ELXPMCAC
01819 ************************************************************      ELXPMCAC
01820                                                                   ELXPMCAC
01821  2133-FIND-FAM-OVRLL-DEDBL.                                       ELXPMCAC
01822      MOVE WS-CF-FALSE TO WS-BS-TEST-CF.                           ELXPMCAC
01823      MOVE WS-CF-FALSE TO WS-MM-TEST-CF.                           ELXPMCAC
01824      MOVE ZEROS TO WS-BS-TEST-SUB.                                ELXPMCAC
01825      MOVE ZEROS TO WS-MM-TEST-SUB.                                ELXPMCAC
01826      MOVE CVG2-OV-THRSHLD-ACL TO WS-SAVE-THRESHOLD.               ELXPMCAC
01827      PERFORM 2141-SCAN-FOR-FAM-OVRLL-DEDBL                        ELXPMCAC
01828           VARYING ATBL-IDX FROM 1 BY 1 UNTIL                      ELXPMCAC
01829                   ATBL-IDX > ATBL-MAX-IDX.                        ELXPMCAC
01830      IF WS-BS-TEST-SUB = ZERO AND                                 ELXPMCAC
01831                   WS-MM-TEST-SUB = ZERO                           ELXPMCAC
01832          SET PMCI-DEDF-NO-FAM-DEDUCTIBLE TO TRUE                  ELXPMCAC
01833      ELSE                                                         ELXPMCAC
01834          PERFORM 2147-SET-PMCI-FAM-OV-DED-INFO.                   ELXPMCAC
01835                                                                   ELXPMCAC
01836 ************************************************************      ELXPMCAC
01837 *                                                          *      ELXPMCAC
01838 * SCAN ACCUMULATOR TABLE FOR INDIVIDUAL OVERALL DEDUCTIBLE *      ELXPMCAC
01839 *                                                          *      ELXPMCAC
01840 ************************************************************      ELXPMCAC
01841                                                                   ELXPMCAC
01842  2134-SCAN-FOR-IND-OVRLL-DEDBL.                                   ELXPMCAC
01843                                                                   ELXPMCAC
01844                                                                   ELXPMCAC
01845      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAC
01846         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
01847            IF PMCI-INPATIENT                                      ELXPMCAC
01848               PERFORM 2136-CALC-IND-INST-IP-OV-DB-CF              ELXPMCAC
01849            ELSE                                                   ELXPMCAC
01850               PERFORM 2137-CALC-IND-INST-OP-OV-DB-CF              ELXPMCAC
01851         ELSE                                                      ELXPMCAC
01852            IF PMCI-INPATIENT                                      ELXPMCAC
01853               PERFORM 2138-CALC-IND-PROF-IP-OV-DB-CF              ELXPMCAC
01854            ELSE                                                   ELXPMCAC
01855               PERFORM 2139-CALC-IND-PROF-OP-OV-DB-CF.             ELXPMCAC
01856                                                                   ELXPMCAC
01857      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACE                        ELXPMCAC
01858         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
01859            IF PMCI-INPATIENT                                      ELXPMCAC
01860               PERFORM 2136M-CLC-IND-INST-IP-OV-DB-CF              ELXPMCAC
01861            ELSE                                                   ELXPMCAC
01862               PERFORM 2137M-CLC-IND-INST-OP-OV-DB-CF              ELXPMCAC
01863         ELSE                                                      ELXPMCAC
01864            IF PMCI-INPATIENT                                      ELXPMCAC
01865               PERFORM 2138M-CLC-IND-PROF-IP-OV-DB-CF              ELXPMCAC
01866            ELSE                                                   ELXPMCAC
01867               PERFORM 2139M-CLC-IND-PROF-OP-OV-DB-CF.             ELXPMCAC
01868                                                                   ELXPMCAC
01869 ************************************************************      ELXPMCAC
01870 *                                                          *      ELXPMCAC
01871 * CALCULATE INDIVIDUAL INSTITUTIONAL INPATIENT OVERALL     *      ELXPMCAC
01872 *         DEDUCTIBLE CONFIDENCE FACTOR                     *      ELXPMCAC
01873 *                                                          *      ELXPMCAC
01874 ************************************************************      ELXPMCAC
01875                                                                   ELXPMCAC
01876  2136-CALC-IND-INST-IP-OV-DB-CF.                                  ELXPMCAC
01877      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01878                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01879                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01880                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01881                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01882                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01883                                                                   ELXPMCAC
01884      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01885         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
01886         CONTINUE                                                  ELXPMCAC
01887      ELSE                                                         ELXPMCAC
01888         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01889                 WS-WT-INDVDL                                      ELXPMCAC
01890         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01891                 WS-WT-INST-BAS                                    ELXPMCAC
01892         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01893                 WS-WT-IP                                          ELXPMCAC
01894         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01895                 WS-WT-PLAN                                        ELXPMCAC
01896         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01897                 WS-WT-SP                                          ELXPMCAC
01898         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01899      END-IF.                                                      ELXPMCAC
01900                                                                   ELXPMCAC
01901      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01902                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01903         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01904         ADD +1 TO WS-BS-INDVD-FOUND                               ELXPMCAC
01905         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01906                                                                   ELXPMCAC
01907  2136M-CLC-IND-INST-IP-OV-DB-CF.                                  ELXPMCAC
01908      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01909                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01910                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01911                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
01912                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01913                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01914                                                                   ELXPMCAC
01915      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01916         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
01917         CONTINUE                                                  ELXPMCAC
01918      ELSE                                                         ELXPMCAC
01919         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01920                 WS-WT-INDVDL                                      ELXPMCAC
01921         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01922                 WS-WT-INST-BAS                                    ELXPMCAC
01923         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
01924                 WS-WT-IP                                          ELXPMCAC
01925         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01926                 WS-WT-PLAN                                        ELXPMCAC
01927         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01928                 WS-WT-SP                                          ELXPMCAC
01929         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01930      END-IF.                                                      ELXPMCAC
01931                                                                   ELXPMCAC
01932      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
01933                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01934         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
01935         ADD +1 TO WS-MM-INDVD-FOUND                               ELXPMCAC
01936         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01937                                                                   ELXPMCAC
01938 ************************************************************      ELXPMCAC
01939 *                                                          *      ELXPMCAC
01940 * CALCULATE INDIVIDUAL INSTITUTIONAL OUTPATIENT OVERALL    *      ELXPMCAC
01941 *         DEDUCTIBEL CONFIDENCE FACTOR                     *      ELXPMCAC
01942 *                                                          *      ELXPMCAC
01943 ************************************************************      ELXPMCAC
01944                                                                   ELXPMCAC
01945  2137-CALC-IND-INST-OP-OV-DB-CF.                                  ELXPMCAC
01946      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01947                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
01948                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01949                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
01950                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01951                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01952                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01953                                                                   ELXPMCAC
01954      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01955         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
01956         CONTINUE                                                  ELXPMCAC
01957      ELSE                                                         ELXPMCAC
01958         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
01959                 WS-WT-LFTM                                        ELXPMCAC
01960         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01961                 WS-WT-INDVDL                                      ELXPMCAC
01962         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
01963                 WS-WT-INST-BAS                                    ELXPMCAC
01964         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01965                 WS-WT-OP                                          ELXPMCAC
01966         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
01967                 WS-WT-PLAN                                        ELXPMCAC
01968         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
01969                 WS-WT-SP                                          ELXPMCAC
01970         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
01971      END-IF.                                                      ELXPMCAC
01972                                                                   ELXPMCAC
01973      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
01974                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
01975         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
01976         ADD +1 TO WS-BS-INDVD-FOUND                               ELXPMCAC
01977         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
01978                                                                   ELXPMCAC
01979  2137M-CLC-IND-INST-OP-OV-DB-CF.                                  ELXPMCAC
01980      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
01981                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
01982                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
01983                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
01984                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
01985                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
01986                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
01987                                                                   ELXPMCAC
01988      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
01989         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
01990         CONTINUE                                                  ELXPMCAC
01991      ELSE                                                         ELXPMCAC
01992         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
01993                 WS-WT-LFTM                                        ELXPMCAC
01994         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
01995                 WS-WT-INDVDL                                      ELXPMCAC
01996         COMPUTE WS-CF-3 = ATBL-CF-INST-SUP (ATBL-IDX) *           ELXPMCAC
01997                 WS-WT-INST-BAS                                    ELXPMCAC
01998         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
01999                 WS-WT-OP                                          ELXPMCAC
02000         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02001                 WS-WT-PLAN                                        ELXPMCAC
02002         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02003                 WS-WT-SP                                          ELXPMCAC
02004         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02005      END-IF.                                                      ELXPMCAC
02006                                                                   ELXPMCAC
02007      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02008                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02009         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02010         ADD +1 TO WS-MM-INDVD-FOUND                               ELXPMCAC
02011         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02012                                                                   ELXPMCAC
02013 ************************************************************      ELXPMCAC
02014 *                                                          *      ELXPMCAC
02015 * CALCULATE INDIVIDUAL PROFESSIONAL   INPATIENT OVERALL    *      ELXPMCAC
02016 *         DEDUCTIBEL CONFIDENCE FACTOR                     *      ELXPMCAC
02017 *                                                          *      ELXPMCAC
02018 ************************************************************      ELXPMCAC
02019                                                                   ELXPMCAC
02020  2138-CALC-IND-PROF-IP-OV-DB-CF.                                  ELXPMCAC
02021      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02022                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02023                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
02024                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02025                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02026                                                                   ELXPMCAC
02027      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02028         WS-CF-TRUE OR                                             ELXPMCAC
02029         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02030         CONTINUE                                                  ELXPMCAC
02031      ELSE                                                         ELXPMCAC
02032         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02033         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02034                 WS-WT-INDVDL                                      ELXPMCAC
02035         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02036                 WS-WT-PROF-BAS                                    ELXPMCAC
02037         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02038                 WS-WT-IP                                          ELXPMCAC
02039         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02040                 WS-WT-SP                                          ELXPMCAC
02041         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02042      END-IF.                                                      ELXPMCAC
02043                                                                   ELXPMCAC
02044      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02045                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02046         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02047         ADD +1 TO WS-BS-INDVD-FOUND                               ELXPMCAC
02048         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02049                                                                   ELXPMCAC
02050  2138M-CLC-IND-PROF-IP-OV-DB-CF.                                  ELXPMCAC
02051      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02052                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02053                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02054                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
02055                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02056                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02057                                                                   ELXPMCAC
02058      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02059         WS-CF-TRUE OR                                             ELXPMCAC
02060         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02061         CONTINUE                                                  ELXPMCAC
02062      ELSE                                                         ELXPMCAC
02063         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02064         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02065                 WS-WT-LFTM                                        ELXPMCAC
02066         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02067                 WS-WT-INDVDL                                      ELXPMCAC
02068         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCAC
02069                 WS-WT-PROF-BAS                                    ELXPMCAC
02070         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02071                 WS-WT-IP                                          ELXPMCAC
02072         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02073                 WS-WT-SP                                          ELXPMCAC
02074         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02075      END-IF.                                                      ELXPMCAC
02076                                                                   ELXPMCAC
02077      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02078                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02079         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02080         ADD +1 TO WS-MM-INDVD-FOUND                               ELXPMCAC
02081         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02082                                                                   ELXPMCAC
02083 ************************************************************      ELXPMCAC
02084 *                                                          *      ELXPMCAC
02085 * CALCULATE INDIVIDUAL PROFESSIONAL  OUTPATIENT OVERALL    *      ELXPMCAC
02086 *         DEDUCTIBLE CONFIDENCE FACTOR                     *      ELXPMCAC
02087 *                                                          *      ELXPMCAC
02088 ************************************************************      ELXPMCAC
02089                                                                   ELXPMCAC
02090  2139-CALC-IND-PROF-OP-OV-DB-CF.                                  ELXPMCAC
02091      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02092                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02093                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
02094                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02095                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02096                                                                   ELXPMCAC
02097      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02098         WS-CF-TRUE OR                                             ELXPMCAC
02099         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02100         CONTINUE                                                  ELXPMCAC
02101      ELSE                                                         ELXPMCAC
02102         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02103         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02104                 WS-WT-INDVDL                                      ELXPMCAC
02105         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02106                 WS-WT-PROF-BAS                                    ELXPMCAC
02107         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02108                 WS-WT-OP                                          ELXPMCAC
02109         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02110                 WS-WT-SP                                          ELXPMCAC
02111         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02112      END-IF.                                                      ELXPMCAC
02113                                                                   ELXPMCAC
02114      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02115                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02116         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02117         ADD +1 TO WS-BS-INDVD-FOUND                               ELXPMCAC
02118         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02119                                                                   ELXPMCAC
02120  2139M-CLC-IND-PROF-OP-OV-DB-CF.                                  ELXPMCAC
02121      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02122                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02123                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
02124                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02125                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02126                                                                   ELXPMCAC
02127      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02128         WS-CF-TRUE OR                                             ELXPMCAC
02129         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02130         CONTINUE                                                  ELXPMCAC
02131      ELSE                                                         ELXPMCAC
02132         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02133         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02134                 WS-WT-INDVDL                                      ELXPMCAC
02135         COMPUTE WS-CF-3 = ATBL-CF-PROF-SUP (ATBL-IDX) *           ELXPMCAC
02136                 WS-WT-PROF-BAS                                    ELXPMCAC
02137         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02138                 WS-WT-OP                                          ELXPMCAC
02139         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02140                 WS-WT-SP                                          ELXPMCAC
02141         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02142      END-IF.                                                      ELXPMCAC
02143                                                                   ELXPMCAC
02144      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02145                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02146         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02147         ADD +1 TO WS-MM-INDVD-FOUND                               ELXPMCAC
02148         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02149                                                                   ELXPMCAC
02150                                                                   ELXPMCAC
02151 ************************************************************      ELXPMCAC
02152 *                                                          *      ELXPMCAC
02153 *    SET PMCI INDIVIDUAL OVERALL DEDUCTIBLE INFORMATION    *      ELXPMCAC
02154 *                                                          *      ELXPMCAC
02155 ************************************************************      ELXPMCAC
02156                                                                   ELXPMCAC
02157  2140-SET-PMCI-IND-OV-DED-INFO.                                   ELXPMCAC
02158      MOVE SPACES TO WS-SAVE-COVER-TYPE.                           ELXPMCAC
02159      IF WS-BS-TEST-SUB NOT EQUAL ZERO AND                         ELXPMCAC
02160                 WS-MM-TEST-SUB NOT EQUAL ZERO                     ELXPMCAC
02161         MOVE '+' TO WS-SAVE-COVER-TYPE                            ELXPMCAC
02162         MOVE WS-BS-INDVD-FOUND TO WS-INDVD-DED-FOUND              ELXPMCAC
02163         SET ATBL-IDX TO WS-BS-TEST-SUB                            ELXPMCAC
02164      ELSE                                                         ELXPMCAC
02165         IF WS-MM-TEST-SUB NOT ZERO                                ELXPMCAC
02166            MOVE '*' TO WS-SAVE-COVER-TYPE                         ELXPMCAC
02167            SET ATBL-IDX TO WS-MM-TEST-SUB                         ELXPMCAC
02168            MOVE WS-MM-INDVD-FOUND TO WS-INDVD-DED-FOUND           ELXPMCAC
02169         ELSE                                                      ELXPMCAC
02170            SET ATBL-IDX TO WS-BS-TEST-SUB                         ELXPMCAC
02171            MOVE WS-BS-INDVD-FOUND TO WS-INDVD-DED-FOUND.          ELXPMCAC
02172                                                                   ELXPMCAC
02173      IF WS-INDVD-DED-FOUND > 1                                    ELXPMCAC
02174         SET PMCI-DEDI-CALL TO TRUE                                ELXPMCAC
02175      ELSE                                                         ELXPMCAC
02176         IF PMCI-PRV-CALL                                          ELXPMCAC
02177            IF WS-PRG-VAR-FOUND                                    ELXPMCAC
02178               SET PMCI-DEDI-CALL TO TRUE                          ELXPMCAC
02179            ELSE                                                   ELXPMCAC
02180            PERFORM 2150-CHK-FOR-IND-DED-OTHR-SRC                  ELXPMCAC
02181         ELSE                                                      ELXPMCAC
02182            IF ATBL-COND-ALL-BIT (ATBL-IDX)    = '1'               ELXPMCAC
02183               EVALUATE ATBL-VALUE-QUALIFIER (ATBL-IDX)            ELXPMCAC
02184                 WHEN '5'                                          ELXPMCAC
02185                   PERFORM 2150-CHK-FOR-IND-DED-OTHR-SRC           ELXPMCAC
02186                 WHEN OTHER                                        ELXPMCAC
02187                   SET PMCI-DEDI-CALL TO TRUE                      ELXPMCAC
02188               END-EVALUATE                                        ELXPMCAC
02189            ELSE                                                   ELXPMCAC
02190               SET PMCI-DEDI-CALL TO TRUE                          ELXPMCAC
02191            END-IF                                                 ELXPMCAC
02192         END-IF                                                    ELXPMCAC
02193      END-IF.                                                      ELXPMCAC
02194                                                                   ELXPMCAC
02195 ************************************************************      ELXPMCAC
02196 *                                                          *      ELXPMCAC
02197 *   SCAN ACCUMULATOR TABLE FOR FAMILY OVERALL DEDUCTIBLE   *      ELXPMCAC
02198 *                                                          *      ELXPMCAC
02199 ************************************************************      ELXPMCAC
02200                                                                   ELXPMCAC
02201  2141-SCAN-FOR-FAM-OVRLL-DEDBL.                                   ELXPMCAC
02202                                                                   ELXPMCAC
02203      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAC
02204         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
02205            IF PMCI-INPATIENT                                      ELXPMCAC
02206               PERFORM 2143-CALC-FAM-INST-IP-OV-DB-CF              ELXPMCAC
02207            ELSE                                                   ELXPMCAC
02208               PERFORM 2144-CALC-FAM-INST-OP-OV-DB-CF              ELXPMCAC
02209         ELSE                                                      ELXPMCAC
02210            IF PMCI-INPATIENT                                      ELXPMCAC
02211               PERFORM 2145-CALC-FAM-PROF-IP-OV-DB-CF              ELXPMCAC
02212            ELSE                                                   ELXPMCAC
02213               PERFORM 2146-CALC-FAM-PROF-OP-OV-DB-CF.             ELXPMCAC
02214                                                                   ELXPMCAC
02215      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACE                        ELXPMCAC
02216         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
02217            IF PMCI-INPATIENT                                      ELXPMCAC
02218               PERFORM 2143M-CLC-FAM-INST-IP-OV-DB-CF              ELXPMCAC
02219            ELSE                                                   ELXPMCAC
02220               PERFORM 2144M-CLC-FAM-INST-OP-OV-DB-CF              ELXPMCAC
02221         ELSE                                                      ELXPMCAC
02222            IF PMCI-INPATIENT                                      ELXPMCAC
02223               PERFORM 2145M-CLC-FAM-PROF-IP-OV-DB-CF              ELXPMCAC
02224            ELSE                                                   ELXPMCAC
02225               PERFORM 2146M-CLC-FAM-PROF-OP-OV-DB-CF.             ELXPMCAC
02226                                                                   ELXPMCAC
02227 ************************************************************      ELXPMCAC
02228 *                                                          *      ELXPMCAC
02229 * CALCULATE FAMILY     INSTITUTIONAL INPATIENT OVERALL     *      ELXPMCAC
02230 *        DEDUCTIBLE CONFIDENCE FACTOR                      *      ELXPMCAC
02231 *                                                          *      ELXPMCAC
02232 ************************************************************      ELXPMCAC
02233                                                                   ELXPMCAC
02234  2143-CALC-FAM-INST-IP-OV-DB-CF.                                  ELXPMCAC
02235      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02236                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02237                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
02238                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02239                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02240                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02241                                                                   ELXPMCAC
02242      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02243         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02244         CONTINUE                                                  ELXPMCAC
02245      ELSE                                                         ELXPMCAC
02246         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02247                 WS-WT-FAM                                         ELXPMCAC
02248         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02249                 WS-WT-INST-BAS                                    ELXPMCAC
02250         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02251                 WS-WT-IP                                          ELXPMCAC
02252         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02253                 WS-WT-PLAN                                        ELXPMCAC
02254         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02255                 WS-WT-SP                                          ELXPMCAC
02256         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02257      END-IF.                                                      ELXPMCAC
02258                                                                   ELXPMCAC
02259                                                                   ELXPMCAC
02260      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02261                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02262         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02263         ADD +1 TO WS-BS-FML-FOUND                                 ELXPMCAC
02264         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02265                                                                   ELXPMCAC
02266  2143M-CLC-FAM-INST-IP-OV-DB-CF.                                  ELXPMCAC
02267      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02268                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02269                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
02270                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02271                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02272                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02273                                                                   ELXPMCAC
02274      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02275         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02276         CONTINUE                                                  ELXPMCAC
02277      ELSE                                                         ELXPMCAC
02278         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02279                 WS-WT-FAM                                         ELXPMCAC
02280         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02281                 WS-WT-INST-BAS                                    ELXPMCAC
02282         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02283                 WS-WT-IP                                          ELXPMCAC
02284         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02285                 WS-WT-PLAN                                        ELXPMCAC
02286         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02287                 WS-WT-SP                                          ELXPMCAC
02288         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02289      END-IF.                                                      ELXPMCAC
02290                                                                   ELXPMCAC
02291      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02292                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02293         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02294         ADD +1 TO WS-MM-FML-FOUND                                 ELXPMCAC
02295         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02296                                                                   ELXPMCAC
02297 ************************************************************      ELXPMCAC
02298 *                                                          *      ELXPMCAC
02299 * CALCULATE FAMILY     INSTITUTIONAL OUTPATIENT OVERALL    *      ELXPMCAC
02300 *        DEDUCTIBLE CONFIDENCE FACTOR                      *      ELXPMCAC
02301 *                                                          *      ELXPMCAC
02302 ************************************************************      ELXPMCAC
02303                                                                   ELXPMCAC
02304  2144-CALC-FAM-INST-OP-OV-DB-CF.                                  ELXPMCAC
02305      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02306                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02307                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
02308                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02309                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02310                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02311                                                                   ELXPMCAC
02312      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02313         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02314         CONTINUE                                                  ELXPMCAC
02315      ELSE                                                         ELXPMCAC
02316         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02317                 WS-WT-FAM                                         ELXPMCAC
02318         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02319                 WS-WT-INST-BAS                                    ELXPMCAC
02320         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02321                 WS-WT-IP                                          ELXPMCAC
02322         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02323                 WS-WT-PLAN                                        ELXPMCAC
02324         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02325                 WS-WT-SP                                          ELXPMCAC
02326         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02327      END-IF.                                                      ELXPMCAC
02328                                                                   ELXPMCAC
02329      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02330                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02331         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02332         ADD +1 TO WS-BS-FML-FOUND                                 ELXPMCAC
02333         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02334                                                                   ELXPMCAC
02335  2144M-CLC-FAM-INST-OP-OV-DB-CF.                                  ELXPMCAC
02336      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02337                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02338                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
02339                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02340                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02341                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02342                                                                   ELXPMCAC
02343      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02344         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02345         CONTINUE                                                  ELXPMCAC
02346      ELSE                                                         ELXPMCAC
02347         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02348                 WS-WT-FAM                                         ELXPMCAC
02349         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02350                 WS-WT-INST-BAS                                    ELXPMCAC
02351         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02352                 WS-WT-IP                                          ELXPMCAC
02353         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02354                 WS-WT-PLAN                                        ELXPMCAC
02355         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02356                 WS-WT-SP                                          ELXPMCAC
02357         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02358      END-IF.                                                      ELXPMCAC
02359                                                                   ELXPMCAC
02360      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02361                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02362         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02363         ADD +1 TO WS-MM-FML-FOUND                                 ELXPMCAC
02364         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02365                                                                   ELXPMCAC
02366 ************************************************************      ELXPMCAC
02367 *                                                          *      ELXPMCAC
02368 * CALCULATE FAMILY     PROFESSIONAL   INPATIENT OVERALL    *      ELXPMCAC
02369 *        DEDUCTIBLE CONFIDENCE FACTOR                      *      ELXPMCAC
02370 *                                                          *      ELXPMCAC
02371 ************************************************************      ELXPMCAC
02372                                                                   ELXPMCAC
02373  2145-CALC-FAM-PROF-IP-OV-DB-CF.                                  ELXPMCAC
02374      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02375                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02376                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
02377                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02378                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02379                                                                   ELXPMCAC
02380      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02381         WS-CF-TRUE OR                                             ELXPMCAC
02382         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02383         CONTINUE                                                  ELXPMCAC
02384      ELSE                                                         ELXPMCAC
02385         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02386         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02387                 WS-WT-FAM                                         ELXPMCAC
02388         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02389                 WS-WT-INST-BAS                                    ELXPMCAC
02390         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02391                 WS-WT-IP                                          ELXPMCAC
02392         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02393                 WS-WT-SP                                          ELXPMCAC
02394         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02395      END-IF.                                                      ELXPMCAC
02396                                                                   ELXPMCAC
02397      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02398                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02399         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02400         ADD +1 TO WS-BS-FML-FOUND                                 ELXPMCAC
02401         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02402                                                                   ELXPMCAC
02403  2145M-CLC-FAM-PROF-IP-OV-DB-CF.                                  ELXPMCAC
02404      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02405                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02406                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
02407                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02408                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02409                                                                   ELXPMCAC
02410      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02411         WS-CF-TRUE OR                                             ELXPMCAC
02412         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02413         CONTINUE                                                  ELXPMCAC
02414      ELSE                                                         ELXPMCAC
02415         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02416         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02417                 WS-WT-LFTM                                        ELXPMCAC
02418         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02419                 WS-WT-FAM                                         ELXPMCAC
02420         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02421                 WS-WT-INST-BAS                                    ELXPMCAC
02422         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02423                 WS-WT-IP                                          ELXPMCAC
02424         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02425                 WS-WT-SP                                          ELXPMCAC
02426         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02427      END-IF.                                                      ELXPMCAC
02428                                                                   ELXPMCAC
02429      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02430                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02431         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02432         ADD +1 TO WS-MM-FML-FOUND                                 ELXPMCAC
02433         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02434                                                                   ELXPMCAC
02435 ************************************************************      ELXPMCAC
02436 *                                                          *      ELXPMCAC
02437 * CALCULATE FAMILY     PROFESSIONAL  OUTPATIENT OVERALL    *      ELXPMCAC
02438 *        DEDUCTIBLE CONFIDENCE FACTOR                      *      ELXPMCAC
02439 *                                                          *      ELXPMCAC
02440 ************************************************************      ELXPMCAC
02441                                                                   ELXPMCAC
02442  2146-CALC-FAM-PROF-OP-OV-DB-CF.                                  ELXPMCAC
02443      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02444                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02445                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
02446                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02447                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02448                                                                   ELXPMCAC
02449      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02450         WS-CF-TRUE OR                                             ELXPMCAC
02451         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02452         CONTINUE                                                  ELXPMCAC
02453      ELSE                                                         ELXPMCAC
02454         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02455         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02456                 WS-WT-FAM                                         ELXPMCAC
02457         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02458                 WS-WT-INST-BAS                                    ELXPMCAC
02459         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02460                 WS-WT-IP                                          ELXPMCAC
02461         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02462                 WS-WT-SP                                          ELXPMCAC
02463         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02464      END-IF.                                                      ELXPMCAC
02465                                                                   ELXPMCAC
02466      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-BS-TEST-CF AND         ELXPMCAC
02467                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02468         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-BS-TEST-CF       ELXPMCAC
02469         ADD +1 TO WS-BS-FML-FOUND                                 ELXPMCAC
02470         SET WS-BS-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02471                                                                   ELXPMCAC
02472  2146M-CLC-FAM-PROF-OP-OV-DB-CF.                                  ELXPMCAC
02473      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02474                            ATBL-CF-FAM    (ATBL-IDX)              ELXPMCAC
02475                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
02476                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02477                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02478                                                                   ELXPMCAC
02479      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02480         WS-CF-TRUE OR                                             ELXPMCAC
02481         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02482         CONTINUE                                                  ELXPMCAC
02483      ELSE                                                         ELXPMCAC
02484         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02485         COMPUTE WS-CF-2 = ATBL-CF-FAM    (ATBL-IDX) *             ELXPMCAC
02486                 WS-WT-FAM                                         ELXPMCAC
02487         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02488                 WS-WT-INST-BAS                                    ELXPMCAC
02489         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02490                 WS-WT-IP                                          ELXPMCAC
02491         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02492                 WS-WT-SP                                          ELXPMCAC
02493         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02494      END-IF.                                                      ELXPMCAC
02495                                                                   ELXPMCAC
02496      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) > WS-MM-TEST-CF AND         ELXPMCAC
02497                 ATBL-CF-WORK-ENTRY (ATBL-IDX) >= WS-SAVE-THRESHOLDELXPMCAC
02498         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX) TO WS-MM-TEST-CF       ELXPMCAC
02499         ADD +1 TO WS-MM-FML-FOUND                                 ELXPMCAC
02500         SET WS-MM-TEST-SUB TO ATBL-IDX.                           ELXPMCAC
02501                                                                   ELXPMCAC
02502 ************************************************************      ELXPMCAC
02503 *                                                          *      ELXPMCAC
02504 *    SET PMCI FAMILY     OVERALL DEDUCTIBLE INFORMATION    *      ELXPMCAC
02505 *                                                          *      ELXPMCAC
02506 ************************************************************      ELXPMCAC
02507                                                                   ELXPMCAC
02508  2147-SET-PMCI-FAM-OV-DED-INFO.                                   ELXPMCAC
02509      MOVE SPACES TO WS-SAVE-COVER-TYPE.                           ELXPMCAC
02510      IF WS-BS-TEST-SUB NOT EQUAL ZERO AND                         ELXPMCAC
02511                 WS-MM-TEST-SUB NOT EQUAL ZERO                     ELXPMCAC
02512         MOVE '+' TO WS-SAVE-COVER-TYPE                            ELXPMCAC
02513         MOVE WS-BS-FML-FOUND TO WS-FAMILY-DED-FOUND               ELXPMCAC
02514         SET ATBL-IDX TO WS-BS-TEST-SUB                            ELXPMCAC
02515      ELSE                                                         ELXPMCAC
02516         IF WS-MM-TEST-SUB NOT ZERO                                ELXPMCAC
02517            MOVE '*' TO WS-SAVE-COVER-TYPE                         ELXPMCAC
02518            SET ATBL-IDX TO WS-MM-TEST-SUB                         ELXPMCAC
02519            MOVE WS-MM-FML-FOUND TO WS-FAMILY-DED-FOUND            ELXPMCAC
02520         ELSE                                                      ELXPMCAC
02521            SET ATBL-IDX TO WS-BS-TEST-SUB                         ELXPMCAC
02522            MOVE WS-BS-FML-FOUND TO WS-FAMILY-DED-FOUND.           ELXPMCAC
02523                                                                   ELXPMCAC
02524      IF WS-FAMILY-DED-FOUND > 1                                   ELXPMCAC
02525         SET PMCI-DEDF-CALL TO TRUE                                ELXPMCAC
02526      ELSE                                                         ELXPMCAC
02527         IF PMCI-PRV-CALL                                          ELXPMCAC
02528            IF WS-PRG-VAR-FOUND                                    ELXPMCAC
02529               SET PMCI-DEDF-CALL TO TRUE                          ELXPMCAC
02530            ELSE                                                   ELXPMCAC
02531            PERFORM 2152-CHK-FOR-FAM-DED-OTHR-SRC                  ELXPMCAC
02532         ELSE                                                      ELXPMCAC
02533            IF ATBL-COND-ALL-BIT (ATBL-IDX)    = '1'               ELXPMCAC
02534               EVALUATE ATBL-VALUE-QUALIFIER (ATBL-IDX)            ELXPMCAC
02535                 WHEN '5'                                          ELXPMCAC
02536                   PERFORM 2152-CHK-FOR-FAM-DED-OTHR-SRC           ELXPMCAC
02537                 WHEN '7'                                          ELXPMCAC
02538                   PERFORM 2153-SET-UP-NUM-IND-DISPLAY             ELXPMCAC
02539                 WHEN OTHER                                        ELXPMCAC
02540                   SET PMCI-DEDF-CALL TO TRUE                      ELXPMCAC
02541               END-EVALUATE                                        ELXPMCAC
02542            ELSE                                                   ELXPMCAC
02543               SET PMCI-DEDF-CALL TO TRUE                          ELXPMCAC
02544            END-IF                                                 ELXPMCAC
02545         END-IF                                                    ELXPMCAC
02546      END-IF.                                                      ELXPMCAC
02547                                                                   ELXPMCAC
02548                                                                   ELXPMCAC
02549 ************************************************************      ELXPMCAC
02550 *                                                          *      ELXPMCAC
02551 *    CHECK FOR INDIVIDUAL DEDUCTIBLE OTHER SOURCE          *      ELXPMCAC
02552 *                                                          *      ELXPMCAC
02553 *  BENEFIT-PERIOD = '0D' IS FOR CALENDAR YEAR,             *      ELXPMCAC
02554 *  BENEFIT-PERIOD = '0E' IS FOR CONTRACT YEAR: BOTH SHOULD *      ELXPMCAC
02555 *     BE HANDLED THE SAME WAY -- RGO.                      *      ELXPMCAC
02556 *                                                          *      ELXPMCAC
02557 ************************************************************      ELXPMCAC
02558                                                                   ELXPMCAC
02559  2150-CHK-FOR-IND-DED-OTHR-SRC.                                   ELXPMCAC
02560      IF ATBL-VALUE-LIMIT (ATBL-IDX) < ZERO                        ELXPMCAC
02561         PERFORM 2151-IND-OTR-SRC-IND-OVRLL-DED                    ELXPMCAC
02562      ELSE                                                         ELXPMCAC
02563         IF ATBL-BENEFIT-PERIOD (ATBL-IDX)  EQUAL '0D'             ELXPMCAC
02564            OR ATBL-BENEFIT-PERIOD (ATBL-IDX) EQUAL '0E'           ELXPMCAC
02565                SET PMCI-DEDI-DOLLARS TO TRUE                      ELXPMCAC
02566                MOVE WS-SAVE-COVER-TYPE TO PMCI-DEDI-FROM-IND      ELXPMCAC
02567                MOVE ATBL-VALUE-LIMIT (ATBL-IDX)                   ELXPMCAC
02568                             TO PMCI-DEDUCTIBLE-IND                ELXPMCAC
02569                PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE             ELXPMCAC
02570         ELSE                                                      ELXPMCAC
02571            SET PMCI-DEDI-CALL TO TRUE                             ELXPMCAC
02572         END-IF                                                    ELXPMCAC
02573      END-IF.                                                      ELXPMCAC
02574                                                                   ELXPMCAC
02575 ************************************************************      ELXPMCAC
02576 *                                                          *      ELXPMCAC
02577 * INDICATE OTHER SOURCE FOR INDIVIDUAL OVERALL DEDUCTIBLE  *      ELXPMCAC
02578 *                                                          *      ELXPMCAC
02579 ************************************************************      ELXPMCAC
02580                                                                   ELXPMCAC
02581  2151-IND-OTR-SRC-IND-OVRLL-DED.                                  ELXPMCAC
02582      IF GCG-DED-BASE-AMT-SOURCE-IND = '1'                         ELXPMCAC
02583         SET PMCI-DEDI-FROM-MEMBERSHIP TO TRUE                     ELXPMCAC
02584         MOVE WS-SAVE-COVER-TYPE TO PMCI-DEDI-FROM-IND             ELXPMCAC
02585      ELSE                                                         ELXPMCAC
02586         SET PMCI-DEDI-CALL TO TRUE.                               ELXPMCAC
02587                                                                   ELXPMCAC
02588 ************************************************************      ELXPMCAC
02589 *                                                          *      ELXPMCAC
02590 *    CHECK FOR FAMILY DEDUCTIBLE OTHER SOURCE              *      ELXPMCAC
02591 *                                                          *      ELXPMCAC
02592 *  BENEFIT-PERIOD = '0D' IS FOR CALENDAR YEAR,             *      ELXPMCAC
02593 *  BENEFIT-PERIOD = '0E' IS FOR CONTRACT YEAR: BOTH SHOULD *      ELXPMCAC
02594 *     BE HANDLED THE SAME WAY -- RGO.                      *      ELXPMCAC
02595 ************************************************************      ELXPMCAC
02596                                                                   ELXPMCAC
02597  2152-CHK-FOR-FAM-DED-OTHR-SRC.                                   ELXPMCAC
02598      IF ATBL-VALUE-LIMIT (ATBL-IDX) < ZERO                        ELXPMCAC
02599         PERFORM 2154-IND-OTR-SRC-FAM-OVRLL-DED                    ELXPMCAC
02600      ELSE                                                         ELXPMCAC
02601         IF ATBL-BENEFIT-PERIOD (ATBL-IDX) EQUAL '0D'              ELXPMCAC
02602            OR ATBL-BENEFIT-PERIOD (ATBL-IDX) EQUAL '0E'           ELXPMCAC
02603                SET PMCI-DEDF-DOLLARS TO TRUE                      ELXPMCAC
02604                MOVE WS-SAVE-COVER-TYPE TO PMCI-DEDF-FROM-IND      ELXPMCAC
02605                MOVE ATBL-VALUE-LIMIT (ATBL-IDX)                   ELXPMCAC
02606                            TO PMCI-DEDUCTIBLE-FAM                 ELXPMCAC
02607                PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE             ELXPMCAC
02608         ELSE                                                      ELXPMCAC
02609            SET PMCI-DEDF-CALL TO TRUE.                            ELXPMCAC
02610                                                                   ELXPMCAC
02611 ************************************************************      ELXPMCAC
02612 *                                                          *      ELXPMCAC
02613 *    SET UP NUMBER OF INDIVIDUALS DISPLAY                  *      ELXPMCAC
02614 *                                                          *      ELXPMCAC
02615 ************************************************************      ELXPMCAC
02616                                                                   ELXPMCAC
02617  2153-SET-UP-NUM-IND-DISPLAY.                                     ELXPMCAC
02618                                                                   ELXPMCAC
02619      SET PMCI-DEDF-NUMBER-IND TO TRUE.                            ELXPMCAC
02620      MOVE WS-SAVE-COVER-TYPE TO PMCI-DEDF-FROM-IND.               ELXPMCAC
02621      MOVE ATBL-VALUE-LIMIT-INTGR (ATBL-IDX)                       ELXPMCAC
02622                   TO PMCI-DEDUCTIBLE-FAM.                         ELXPMCAC
02623      PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE.                      ELXPMCAC
02624                                                                   ELXPMCAC
02625 ************************************************************      ELXPMCAC
02626 *                                                          *      ELXPMCAC
02627 *    INDICATE OTHER SOURCE FOR FAMILY OVERALL DEDUCTIBLE   *      ELXPMCAC
02628 *                                                          *      ELXPMCAC
02629 ************************************************************      ELXPMCAC
02630                                                                   ELXPMCAC
02631  2154-IND-OTR-SRC-FAM-OVRLL-DED.                                  ELXPMCAC
02632      IF GCG-DED-BASE-AMT-SOURCE-IND = '1'                         ELXPMCAC
02633         SET PMCI-DEDF-FROM-MEMBERSHIP TO TRUE                     ELXPMCAC
02634         MOVE WS-SAVE-COVER-TYPE TO PMCI-DEDF-FROM-IND             ELXPMCAC
02635      ELSE                                                         ELXPMCAC
02636         SET PMCI-DEDF-CALL TO TRUE.                               ELXPMCAC
02637                                                                   ELXPMCAC
02638                                                                   ELXPMCAC
02639 ************************************************************      ELXPMCAC
02640 ************************************************************      ELXPMCAC
02641 *                                                          *      ELXPMCAC
02642 *     OVERALL CONTRACT OUT OF POCKET PROCESSING SECTION    *      ELXPMCAC
02643 *                                                          *      ELXPMCAC
02644 ************************************************************      ELXPMCAC
02645 ************************************************************      ELXPMCAC
02646                                                                   ELXPMCAC
02647                                                                   ELXPMCAC
02648 ************************************************************      ELXPMCAC
02649 *                                                          *      ELXPMCAC
02650 *    SEARCH ACCUM TABLE FOR OVERALL CONTRACT OUT OF POCKET *      ELXPMCAC
02651 *                                                          *      ELXPMCAC
02652 ************************************************************      ELXPMCAC
02653                                                                   ELXPMCAC
02654  2160-FIND-OVRLL-CNTRCT-OPX.                                      ELXPMCAC
02655      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                        ELXPMCAC
02656       TO CSAC-AOL-GC-TBL-PTR.                                     ELXPMCAC
02657                                                                   ELXPMCAC
02658      IF  ADDRESS OF ATBL-ACCUMULATOR-TABLE = NULL                 ELXPMCAC
02659         SET OPX-NO-LIMIT TO TRUE                                  ELXPMCAC
02660      ELSE                                                         ELXPMCAC
02661         PERFORM 2161-SCAN-ATBL-FOR-OVERALL-OPX.                   ELXPMCAC
02662                                                                   ELXPMCAC
02663 ************************************************************      ELXPMCAC
02664 *                                                          *      ELXPMCAC
02665 *    SCAN ACCUMULATOR TABLE FOR OVERALL OUT OF POCKET      *      ELXPMCAC
02666 *                                                          *      ELXPMCAC
02667 ************************************************************      ELXPMCAC
02668                                                                   ELXPMCAC
02669  2161-SCAN-ATBL-FOR-OVERALL-OPX.                                  ELXPMCAC
02670      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAC
02671      PERFORM 4000-MOVE-OVRLLS-TO-SPEC                             ELXPMCAC
02672       VARYING ATBL-IDX                                            ELXPMCAC
02673         FROM 1 BY 1 UNTIL ATBL-IDX >                              ELXPMCAC
02674            ATBL-MAX-IDX.                                          ELXPMCAC
02675                                                                   ELXPMCAC
02676      IF PMCI-PROGRAM-IND > ZERO                                   ELXPMCAC
02677         PERFORM 3000-RECALC-OV-PER-CCP-FACTOR.                    ELXPMCAC
02678                                                                   ELXPMCAC
02679      CALL 'ELKSPCFF' USING ATBL-ACCUMULATOR-TABLE.                ELXPMCAC
02680      MOVE RETURN-CODE TO WS-RETURN-CODE.                          ELXPMCAC
02681      IF WS-SUCCESSFUL-CALL                                        ELXPMCAC
02682         MOVE WS-CF-FALSE TO WS-BS-TEST-CF                         ELXPMCAC
02683         MOVE WS-CF-FALSE TO WS-MM-TEST-CF                         ELXPMCAC
02684         MOVE ZEROS TO WS-BS-TEST-SUB                              ELXPMCAC
02685         MOVE ZEROS TO WS-MM-TEST-SUB                              ELXPMCAC
02686         MOVE ZEROS TO WS-OUT-OF-POCKET-FOUND                      ELXPMCAC
02687         MOVE ZEROS TO WS-BS-OPX-FOUND                             ELXPMCAC
02688         MOVE ZEROS TO WS-MM-OPX-FOUND                             ELXPMCAC
02689         MOVE CVG2-OV-THRSHLD-AOL TO WS-SAVE-THRESHOLD             ELXPMCAC
02690         PERFORM 2162-DTRMN-PARMS-FOR-OPX                          ELXPMCAC
02691           VARYING ATBL-IDX FROM 1 BY 1 UNTIL                      ELXPMCAC
02692                   ATBL-IDX > ATBL-MAX-IDX                         ELXPMCAC
02693         IF WS-BS-TEST-SUB = ZERO AND                              ELXPMCAC
02694                   WS-MM-TEST-SUB = ZERO                           ELXPMCAC
02695            SET OPX-NO-LIMIT TO TRUE                               ELXPMCAC
02696         ELSE                                                      ELXPMCAC
02697            PERFORM 2167-SET-PMCI-OVRL-OPX-INFO                    ELXPMCAC
02698      ELSE                                                         ELXPMCAC
02699         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
02700         EVALUATE TRUE                                             ELXPMCAC
02701            WHEN WS-UNIDENTIFIED-PARAMETER                         ELXPMCAC
02702               MOVE +4007 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
02703            WHEN WS-MISSING-PARAMETER                              ELXPMCAC
02704               MOVE +4008 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
02705            WHEN WS-INTERNAL-ERROR                                 ELXPMCAC
02706               MOVE +4009 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCAC
02707         END-EVALUATE                                              ELXPMCAC
02708      END-IF.                                                      ELXPMCAC
02709                                                                   ELXPMCAC
02710                                                                   ELXPMCAC
02711                                                                   ELXPMCAC
02712 ************************************************************      ELXPMCAC
02713 *                                                          *      ELXPMCAC
02714 *    DETERMINE PARAMETERS FOR OUT OF POCKET CALCULATION    *      ELXPMCAC
02715 *                                                          *      ELXPMCAC
02716 ************************************************************      ELXPMCAC
02717                                                                   ELXPMCAC
02718  2162-DTRMN-PARMS-FOR-OPX.                                        ELXPMCAC
02719      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACE                       ELXPMCAC
02720         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
02721            IF PMCI-INPATIENT                                      ELXPMCAC
02722               PERFORM 2163-CALC-ANL-INST-INP-OVRL-CF              ELXPMCAC
02723            ELSE                                                   ELXPMCAC
02724               PERFORM 2164-CALC-ANL-INST-OUT-OVRL-CF              ELXPMCAC
02725         ELSE                                                      ELXPMCAC
02726            IF PMCI-INPATIENT                                      ELXPMCAC
02727               PERFORM 2165-CALC-ANL-PROF-INP-OVRL-CF              ELXPMCAC
02728            ELSE                                                   ELXPMCAC
02729               PERFORM 2166-CALC-ANL-PROF-OUT-OVRL-CF.             ELXPMCAC
02730                                                                   ELXPMCAC
02731      IF PMCI-MM-CNTRCT-GRP NOT EQUAL SPACE                        ELXPMCAC
02732         IF PMCI-INSTITUTIONAL                                     ELXPMCAC
02733            IF PMCI-INPATIENT                                      ELXPMCAC
02734               PERFORM 2163M-CLC-ANL-INST-INP-OVRL-CF              ELXPMCAC
02735            ELSE                                                   ELXPMCAC
02736               PERFORM 2164M-CLC-ANL-INST-OUT-OVRL-CF              ELXPMCAC
02737         ELSE                                                      ELXPMCAC
02738            IF PMCI-INPATIENT                                      ELXPMCAC
02739               PERFORM 2165M-CLC-ANL-PROF-INP-OVRL-CF              ELXPMCAC
02740            ELSE                                                   ELXPMCAC
02741               PERFORM 2166M-CLC-ANL-PROF-OUT-OVRL-CF.             ELXPMCAC
02742                                                                   ELXPMCAC
02743                                                                   ELXPMCAC
02744 ************************************************************      ELXPMCAC
02745 *                                                          *      ELXPMCAC
02746 * CALCULATE OVERALL OPX  INST INPATIENT CONFIDENCE FACTOR  *      ELXPMCAC
02747 *                                                          *      ELXPMCAC
02748 ************************************************************      ELXPMCAC
02749                                                                   ELXPMCAC
02750  2163-CALC-ANL-INST-INP-OVRL-CF.                                  ELXPMCAC
02751      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02752                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02753                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02754                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
02755                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02756                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02757                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02758                                                                   ELXPMCAC
02759      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02760         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02761         CONTINUE                                                  ELXPMCAC
02762      ELSE                                                         ELXPMCAC
02763         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02764                 WS-WT-LFTM                                        ELXPMCAC
02765         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02766                 WS-WT-INDVDL                                      ELXPMCAC
02767         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02768                 WS-WT-INST-BAS                                    ELXPMCAC
02769         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02770                 WS-WT-IP                                          ELXPMCAC
02771         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02772                 WS-WT-PLAN                                        ELXPMCAC
02773         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02774                 WS-WT-SP                                          ELXPMCAC
02775         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02776      END-IF.                                                      ELXPMCAC
02777                                                                   ELXPMCAC
02778      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02779         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02780      AND                                                          ELXPMCAC
02781         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02782         WS-BS-TEST-CF                                             ELXPMCAC
02783      AND                                                          ELXPMCAC
02784         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
02785         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
02786           TO WS-BS-TEST-CF                                        ELXPMCAC
02787         SET WS-BS-TEST-SUB TO ATBL-IDX                            ELXPMCAC
02788         ADD +1 TO WS-BS-OPX-FOUND.                                ELXPMCAC
02789                                                                   ELXPMCAC
02790                                                                   ELXPMCAC
02791  2163M-CLC-ANL-INST-INP-OVRL-CF.                                  ELXPMCAC
02792      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02793                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02794                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02795                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
02796                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02797                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02798                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02799                                                                   ELXPMCAC
02800      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02801         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02802         CONTINUE                                                  ELXPMCAC
02803      ELSE                                                         ELXPMCAC
02804         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02805                 WS-WT-LFTM                                        ELXPMCAC
02806         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02807                 WS-WT-INDVDL                                      ELXPMCAC
02808         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02809                 WS-WT-INST-BAS                                    ELXPMCAC
02810         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02811                 WS-WT-IP                                          ELXPMCAC
02812         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02813                 WS-WT-PLAN                                        ELXPMCAC
02814         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02815                 WS-WT-SP                                          ELXPMCAC
02816         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02817      END-IF.                                                      ELXPMCAC
02818                                                                   ELXPMCAC
02819                                                                   ELXPMCAC
02820      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02821         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02822      AND                                                          ELXPMCAC
02823         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02824         WS-MM-TEST-CF                                             ELXPMCAC
02825      AND                                                          ELXPMCAC
02826         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
02827         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
02828           TO WS-MM-TEST-CF                                        ELXPMCAC
02829         SET WS-MM-TEST-SUB TO ATBL-IDX                            ELXPMCAC
02830         ADD +1 TO WS-MM-OPX-FOUND.                                ELXPMCAC
02831                                                                   ELXPMCAC
02832                                                                   ELXPMCAC
02833 ************************************************************      ELXPMCAC
02834 *                                                          *      ELXPMCAC
02835 * CALCULATE OVERALL  OPX INST OUTPATIENT CONFIDENCE FACTOR *      ELXPMCAC
02836 *                                                          *      ELXPMCAC
02837 ************************************************************      ELXPMCAC
02838                                                                   ELXPMCAC
02839  2164-CALC-ANL-INST-OUT-OVRL-CF.                                  ELXPMCAC
02840      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02841                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02842                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02843                            ATBL-CF-INST-BAS (ATBL-IDX)            ELXPMCAC
02844                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02845                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02846                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02847                                                                   ELXPMCAC
02848      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02849         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02850         CONTINUE                                                  ELXPMCAC
02851      ELSE                                                         ELXPMCAC
02852         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02853                 WS-WT-LFTM                                        ELXPMCAC
02854         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02855                 WS-WT-INDVDL                                      ELXPMCAC
02856         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02857                 WS-WT-INST-BAS                                    ELXPMCAC
02858         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02859                 WS-WT-OP                                          ELXPMCAC
02860         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02861                 WS-WT-PLAN                                        ELXPMCAC
02862         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02863                 WS-WT-SP                                          ELXPMCAC
02864         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02865      END-IF.                                                      ELXPMCAC
02866                                                                   ELXPMCAC
02867                                                                   ELXPMCAC
02868      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02869         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02870      AND                                                          ELXPMCAC
02871         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02872         WS-BS-TEST-CF                                             ELXPMCAC
02873      AND                                                          ELXPMCAC
02874         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
02875         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
02876           TO WS-BS-TEST-CF                                        ELXPMCAC
02877         SET WS-BS-TEST-SUB TO ATBL-IDX                            ELXPMCAC
02878         ADD +1 TO WS-BS-OPX-FOUND.                                ELXPMCAC
02879                                                                   ELXPMCAC
02880  2164M-CLC-ANL-INST-OUT-OVRL-CF.                                  ELXPMCAC
02881      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02882                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02883                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02884                            ATBL-CF-INST-SUP (ATBL-IDX)            ELXPMCAC
02885                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
02886                            ATBL-CF-PLAN (ATBL-IDX)                ELXPMCAC
02887                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02888                                                                   ELXPMCAC
02889      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02890         WS-CF-TRUE OR WS-CF-FALSE                                 ELXPMCAC
02891         CONTINUE                                                  ELXPMCAC
02892      ELSE                                                         ELXPMCAC
02893         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02894                 WS-WT-LFTM                                        ELXPMCAC
02895         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02896                 WS-WT-INDVDL                                      ELXPMCAC
02897         COMPUTE WS-CF-3 = ATBL-CF-INST-BAS (ATBL-IDX) *           ELXPMCAC
02898                 WS-WT-INST-BAS                                    ELXPMCAC
02899         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
02900                 WS-WT-OP                                          ELXPMCAC
02901         COMPUTE WS-CF-5 = ATBL-CF-PLAN (ATBL-IDX) *               ELXPMCAC
02902                 WS-WT-PLAN                                        ELXPMCAC
02903         COMPUTE WS-CF-6 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02904                 WS-WT-SP                                          ELXPMCAC
02905         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02906      END-IF.                                                      ELXPMCAC
02907                                                                   ELXPMCAC
02908                                                                   ELXPMCAC
02909      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02910         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02911      AND                                                          ELXPMCAC
02912         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02913         WS-MM-TEST-CF                                             ELXPMCAC
02914      AND                                                          ELXPMCAC
02915         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
02916         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
02917           TO WS-MM-TEST-CF                                        ELXPMCAC
02918         SET WS-MM-TEST-SUB TO ATBL-IDX                            ELXPMCAC
02919         ADD +1 TO WS-MM-OPX-FOUND.                                ELXPMCAC
02920                                                                   ELXPMCAC
02921 ************************************************************      ELXPMCAC
02922 *                                                          *      ELXPMCAC
02923 *    CALCULATE OVERALL OUT OF POCKET PROFESSIONAL          *      ELXPMCAC
02924 *      INPATIENT CONFIDENCE FACTOR                         *      ELXPMCAC
02925 *                                                          *      ELXPMCAC
02926 ************************************************************      ELXPMCAC
02927                                                                   ELXPMCAC
02928  2165-CALC-ANL-PROF-INP-OVRL-CF.                                  ELXPMCAC
02929      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02930                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02931                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02932                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
02933                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02934                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02935                                                                   ELXPMCAC
02936      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02937         WS-CF-TRUE OR                                             ELXPMCAC
02938         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02939         CONTINUE                                                  ELXPMCAC
02940      ELSE                                                         ELXPMCAC
02941         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02942         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02943                 WS-WT-LFTM                                        ELXPMCAC
02944         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02945                 WS-WT-INDVDL                                      ELXPMCAC
02946         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02947                 WS-WT-PROF-BAS                                    ELXPMCAC
02948         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02949                 WS-WT-IP                                          ELXPMCAC
02950         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02951                 WS-WT-SP                                          ELXPMCAC
02952         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02953      END-IF.                                                      ELXPMCAC
02954                                                                   ELXPMCAC
02955      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02956         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02957      AND                                                          ELXPMCAC
02958         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02959         WS-BS-TEST-CF                                             ELXPMCAC
02960      AND                                                          ELXPMCAC
02961         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
02962         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
02963           TO WS-BS-TEST-CF                                        ELXPMCAC
02964         SET WS-BS-TEST-SUB TO ATBL-IDX                            ELXPMCAC
02965         ADD +1 TO WS-BS-OPX-FOUND.                                ELXPMCAC
02966                                                                   ELXPMCAC
02967                                                                   ELXPMCAC
02968  2165M-CLC-ANL-PROF-INP-OVRL-CF.                                  ELXPMCAC
02969      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
02970                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
02971                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
02972                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
02973                            ATBL-CF-IP (ATBL-IDX)                  ELXPMCAC
02974                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
02975                                                                   ELXPMCAC
02976      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
02977         WS-CF-TRUE OR                                             ELXPMCAC
02978         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
02979         CONTINUE                                                  ELXPMCAC
02980      ELSE                                                         ELXPMCAC
02981         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
02982         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
02983                 WS-WT-LFTM                                        ELXPMCAC
02984         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
02985                 WS-WT-INDVDL                                      ELXPMCAC
02986         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
02987                 WS-WT-PROF-BAS                                    ELXPMCAC
02988         COMPUTE WS-CF-4 = ATBL-CF-IP (ATBL-IDX) *                 ELXPMCAC
02989                 WS-WT-IP                                          ELXPMCAC
02990         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
02991                 WS-WT-SP                                          ELXPMCAC
02992         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
02993      END-IF.                                                      ELXPMCAC
02994                                                                   ELXPMCAC
02995      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
02996         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
02997      AND                                                          ELXPMCAC
02998         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
02999         WS-MM-TEST-CF                                             ELXPMCAC
03000      AND                                                          ELXPMCAC
03001         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
03002         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
03003           TO WS-MM-TEST-CF                                        ELXPMCAC
03004         SET WS-MM-TEST-SUB TO ATBL-IDX                            ELXPMCAC
03005         ADD +1 TO WS-MM-OPX-FOUND.                                ELXPMCAC
03006                                                                   ELXPMCAC
03007                                                                   ELXPMCAC
03008 ************************************************************      ELXPMCAC
03009 *                                                          *      ELXPMCAC
03010 *    CALCULATE OVERALL OUT OF POCKET PROFESSIONAL          *      ELXPMCAC
03011 *     OUTPATIENT CONFIDENCE FACTOR                         *      ELXPMCAC
03012 *                                                          *      ELXPMCAC
03013 ************************************************************      ELXPMCAC
03014                                                                   ELXPMCAC
03015  2166-CALC-ANL-PROF-OUT-OVRL-CF.                                  ELXPMCAC
03016      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
03017                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
03018                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
03019                            ATBL-CF-PROF-BAS (ATBL-IDX)            ELXPMCAC
03020                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
03021                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
03022                                                                   ELXPMCAC
03023      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
03024         WS-CF-TRUE OR                                             ELXPMCAC
03025         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
03026         CONTINUE                                                  ELXPMCAC
03027      ELSE                                                         ELXPMCAC
03028         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
03029         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
03030                 WS-WT-LFTM                                        ELXPMCAC
03031         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
03032                 WS-WT-INDVDL                                      ELXPMCAC
03033         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
03034                 WS-WT-PROF-BAS                                    ELXPMCAC
03035         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
03036                 WS-WT-OP                                          ELXPMCAC
03037         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
03038                 WS-WT-SP                                          ELXPMCAC
03039         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
03040      END-IF.                                                      ELXPMCAC
03041                                                                   ELXPMCAC
03042      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
03043         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
03044      AND                                                          ELXPMCAC
03045         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
03046         WS-BS-TEST-CF                                             ELXPMCAC
03047      AND                                                          ELXPMCAC
03048         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
03049         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
03050           TO WS-BS-TEST-CF                                        ELXPMCAC
03051         SET WS-BS-TEST-SUB TO ATBL-IDX                            ELXPMCAC
03052         ADD +1 TO WS-BS-OPX-FOUND.                                ELXPMCAC
03053                                                                   ELXPMCAC
03054                                                                   ELXPMCAC
03055  2166M-CLC-ANL-PROF-OUT-OVRL-CF.                                  ELXPMCAC
03056      CALL 'ELKFLAND' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
03057                            ATBL-CF-ANL  (ATBL-IDX)                ELXPMCAC
03058                            ATBL-CF-INDVDL (ATBL-IDX)              ELXPMCAC
03059                            ATBL-CF-PROF-SUP (ATBL-IDX)            ELXPMCAC
03060                            ATBL-CF-OP (ATBL-IDX)                  ELXPMCAC
03061                            ATBL-CF-SP (ATBL-IDX).                 ELXPMCAC
03062                                                                   ELXPMCAC
03063      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) =                           ELXPMCAC
03064         WS-CF-TRUE OR                                             ELXPMCAC
03065         ATBL-CF-WORK-ENTRY (ATBL-IDX) < 0                         ELXPMCAC
03066         CONTINUE                                                  ELXPMCAC
03067      ELSE                                                         ELXPMCAC
03068         INITIALIZE WS-CONFIDENCE-FACTORS                          ELXPMCAC
03069         COMPUTE WS-CF-1 = ATBL-CF-ANL  (ATBL-IDX) *               ELXPMCAC
03070                 WS-WT-LFTM                                        ELXPMCAC
03071         COMPUTE WS-CF-2 = ATBL-CF-INDVDL (ATBL-IDX) *             ELXPMCAC
03072                 WS-WT-INDVDL                                      ELXPMCAC
03073         COMPUTE WS-CF-3 = ATBL-CF-PROF-BAS (ATBL-IDX) *           ELXPMCAC
03074                 WS-WT-PROF-BAS                                    ELXPMCAC
03075         COMPUTE WS-CF-4 = ATBL-CF-OP (ATBL-IDX) *                 ELXPMCAC
03076                 WS-WT-OP                                          ELXPMCAC
03077         COMPUTE WS-CF-5 = ATBL-CF-SP (ATBL-IDX) *                 ELXPMCAC
03078                 WS-WT-SP                                          ELXPMCAC
03079         PERFORM 5000-CALL-ELKFLCMB                                ELXPMCAC
03080      END-IF.                                                      ELXPMCAC
03081                                                                   ELXPMCAC
03082      IF ATBL-CF-WORK-ENTRY (ATBL-IDX) >                           ELXPMCAC
03083         CVG2-OV-THRSHLD-AOL                                       ELXPMCAC
03084      AND                                                          ELXPMCAC
03085         ATBL-CF-WORK-ENTRY (ATBL-IDX) >=                          ELXPMCAC
03086         WS-MM-TEST-CF                                             ELXPMCAC
03087      AND                                                          ELXPMCAC
03088         ATBL-PERCENT-LEVEL (ATBL-IDX) < +100                      ELXPMCAC
03089         MOVE ATBL-CF-WORK-ENTRY (ATBL-IDX)                        ELXPMCAC
03090           TO WS-MM-TEST-CF                                        ELXPMCAC
03091         SET WS-MM-TEST-SUB TO ATBL-IDX                            ELXPMCAC
03092         ADD +1 TO WS-MM-OPX-FOUND.                                ELXPMCAC
03093                                                                   ELXPMCAC
03094                                                                   ELXPMCAC
03095 ************************************************************      ELXPMCAC
03096 *                                                          *      ELXPMCAC
03097 *    SET PMCI OVERALL OUT OF POCKET INFORMATION            *      ELXPMCAC
03098 *                                                          *      ELXPMCAC
03099 ************************************************************      ELXPMCAC
03100                                                                   ELXPMCAC
03101  2167-SET-PMCI-OVRL-OPX-INFO.                                     ELXPMCAC
03102      MOVE SPACES TO WS-SAVE-COVER-TYPE.                           ELXPMCAC
03103      IF WS-BS-TEST-SUB NOT EQUAL ZERO AND                         ELXPMCAC
03104                 WS-MM-TEST-SUB NOT EQUAL ZERO                     ELXPMCAC
03105         MOVE '+' TO WS-SAVE-COVER-TYPE                            ELXPMCAC
03106         MOVE WS-BS-OPX-FOUND TO WS-OUT-OF-POCKET-FOUND            ELXPMCAC
03107         SET ATBL-IDX TO WS-BS-TEST-SUB                            ELXPMCAC
03108      ELSE                                                         ELXPMCAC
03109         IF WS-MM-TEST-SUB NOT ZERO                                ELXPMCAC
03110            MOVE '*' TO WS-SAVE-COVER-TYPE                         ELXPMCAC
03111            SET ATBL-IDX TO WS-MM-TEST-SUB                         ELXPMCAC
03112            MOVE WS-MM-OPX-FOUND TO WS-OUT-OF-POCKET-FOUND         ELXPMCAC
03113         ELSE                                                      ELXPMCAC
03114            SET ATBL-IDX TO WS-BS-TEST-SUB                         ELXPMCAC
03115            MOVE WS-BS-OPX-FOUND TO WS-OUT-OF-POCKET-FOUND.        ELXPMCAC
03116                                                                   ELXPMCAC
03117      IF PMCI-PRV-CALL                                             ELXPMCAC
03118         IF WS-PRG-VAR-FOUND                                       ELXPMCAC
03119            SET OPX-CALL TO TRUE                                   ELXPMCAC
03120         ELSE                                                      ELXPMCAC
03121            SET OPX-DOLLARS TO TRUE                                ELXPMCAC
03122            MOVE WS-SAVE-COVER-TYPE TO PMCI-OPX-FROM-IND           ELXPMCAC
03123            MOVE ATBL-VALUE-LIMIT (ATBL-IDX)                       ELXPMCAC
03124                             TO PMCI-OUT-OF-POCKET-EXPENSE         ELXPMCAC
03125            PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE                 ELXPMCAC
03126         END-IF                                                    ELXPMCAC
03127      ELSE                                                         ELXPMCAC
03128         PERFORM 2168-SET-PMCI-OVRL-OPX-VLS                        ELXPMCAC
03129      END-IF.                                                      ELXPMCAC
03130                                                                   ELXPMCAC
03131 ************************************************************      ELXPMCAC
03132 *                                                          *      ELXPMCAC
03133 *    SET PMCI OVERALL OUT OF POCKET VALUES                 *      ELXPMCAC
03134 *                                                          *      ELXPMCAC
03135 ************************************************************      ELXPMCAC
03136                                                                   ELXPMCAC
03137  2168-SET-PMCI-OVRL-OPX-VLS.                                      ELXPMCAC
03138      IF ATBL-COND-ALL-BIT (ATBL-IDX)       = '1'                  ELXPMCAC
03139      THEN                                                         ELXPMCAC
03140         IF ATBL-ASCEND-DESCEND-IND (ATBL-IDX) = '0'               ELXPMCAC
03141            SET OPX-DOLLARS TO TRUE                                ELXPMCAC
03142            MOVE WS-SAVE-COVER-TYPE TO PMCI-OPX-FROM-IND           ELXPMCAC
03143            MOVE ATBL-VALUE-LIMIT (ATBL-IDX)                       ELXPMCAC
03144              TO PMCI-OUT-OF-POCKET-EXPENSE                        ELXPMCAC
03145            PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE                 ELXPMCAC
03146         ELSE                                                      ELXPMCAC
03147            EVALUATE WS-OUT-OF-POCKET-FOUND                        ELXPMCAC
03148               WHEN 0                                              ELXPMCAC
03149                  SET OPX-NO-LIMIT TO TRUE                         ELXPMCAC
03150               WHEN 1                                              ELXPMCAC
03151                  SET OPX-DOLLARS TO TRUE                          ELXPMCAC
03152                  MOVE WS-SAVE-COVER-TYPE TO PMCI-OPX-FROM-IND     ELXPMCAC
03153                  MOVE ATBL-VALUE-LIMIT (ATBL-IDX)                 ELXPMCAC
03154                    TO PMCI-OUT-OF-POCKET-EXPENSE                  ELXPMCAC
03155                  PERFORM 5500-LOAD-ACCUM-CDE-ATBL-TABLE           ELXPMCAC
03156               WHEN OTHER                                          ELXPMCAC
03157                  SET OPX-CALL TO TRUE                             ELXPMCAC
03158         END-IF                                                    ELXPMCAC
03159      ELSE                                                         ELXPMCAC
03160         SET OPX-CALL TO TRUE                                      ELXPMCAC
03161      END-IF.                                                      ELXPMCAC
03162                                                                   ELXPMCAC
03163 ************************************************************      ELXPMCAC
03164 *                                                          *      ELXPMCAC
03165 *    RECOMPUTE OVERALL PER COST CONTAINMENT FACTOR FOR     *      ELXPMCAC
03166 *      PPO CONTRACT                                        *      ELXPMCAC
03167 *                                                          *      ELXPMCAC
03168 ************************************************************      ELXPMCAC
03169  3000-RECALC-OV-PER-CCP-FACTOR.                                   ELXPMCAC
03170      SET ATBL-MAX-IDX TO ATBL-TBL-CNT.                            ELXPMCAC
03171      IF PMCI-PRV-CALL OR PMCI-PRV-NONE                            ELXPMCAC
03172         CONTINUE                                                  ELXPMCAC
03173      ELSE                                                         ELXPMCAC
03174         PERFORM 3100-VERIFY-COST-CONTAINMENT                      ELXPMCAC
03175             VARYING ATBL-IDX FROM 1 BY 1                          ELXPMCAC
03176                  UNTIL ATBL-IDX > ATBL-MAX-IDX.                   ELXPMCAC
03177      SET WS-PRG-VAR-NOT-FOUND TO TRUE.                            ELXPMCAC
03178      PERFORM 3250-SET-PRG-CONFIDENCE-FACTOR                       ELXPMCAC
03179         VARYING ATBL-IDX FROM 1 BY 1                              ELXPMCAC
03180           UNTIL ATBL-IDX > ATBL-MAX-IDX.                          ELXPMCAC
03181 ************************************************************      ELXPMCAC
03182 *                                                          *      ELXPMCAC
03183 *        VERIFY COST CONTAINMENT PROGRAMS                  *      ELXPMCAC
03184 *                                                          *      ELXPMCAC
03185 ************************************************************      ELXPMCAC
03186  3100-VERIFY-COST-CONTAINMENT.                                    ELXPMCAC
03187                                                                   ELXPMCAC
03188      EVALUATE TRUE                                                ELXPMCAC
03189         WHEN PMCI-PRV-PPO-IN                                      ELXPMCAC
03190            PERFORM 3102-TEST-PPO-CSTCNMT-IN                       ELXPMCAC
03191         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCAC
03192            PERFORM 3102-TEST-PPO-CSTCNMT-OUT                      ELXPMCAC
03193         WHEN PMCI-PRV-BAE-IN                                      ELXPMCAC
03194            PERFORM 3108-TEST-BAE-CSTCNMT-IN                       ELXPMCAC
03195         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCAC
03196            PERFORM 3108-TEST-BAE-CSTCNMT-OUT                      ELXPMCAC
03197         WHEN PMCI-PRV-RPO-IN                                      ELXPMCAC
03198            PERFORM 3102-TEST-PPO-CSTCNMT-IN                       ELXPMCAC
03199         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCAC
03200            PERFORM 3104-TEST-RPO-CSTCNMT-OUT                      ELXPMCAC
03201         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCAC
03202            PERFORM 3104-TEST-RPO-CSTCNMT-IN                       ELXPMCAC
03203         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCAC
03204            PERFORM 3106-TEST-MCNP-CSTCNMT-IN                      ELXPMCAC
03205         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCAC
03206            PERFORM 3106-TEST-MCNP-CSTCNMT-IN                      ELXPMCAC
03207         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCAC
03208            PERFORM 3106-TEST-MCNP-CSTCNMT-OUT                     ELXPMCAC
03209         WHEN OTHER                                                ELXPMCAC
03210            CONTINUE                                               ELXPMCAC
03211      END-EVALUATE.                                                ELXPMCAC
03212 ************************************************************      ELXPMCAC
03213 *                                                          *      ELXPMCAC
03214 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCAC
03215 *          FOR IN NETWORK                                  *      ELXPMCAC
03216 * THIS INCLUDES LOGIC FOR BAE                              *      ELXPMCAC
03217 ************************************************************      ELXPMCAC
03218  3102-TEST-PPO-CSTCNMT-IN.                                        ELXPMCAC
03219                                                                   ELXPMCAC
03220      EVALUATE TRUE                                                ELXPMCAC
03221         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAC
03222             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCAC
03223         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAC
03224             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03225         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAC
03226             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03227         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03228             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03229             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCAC
03230         WHEN OTHER                                                ELXPMCAC
03231             CONTINUE                                              ELXPMCAC
03232      END-EVALUATE.                                                ELXPMCAC
03233                                                                   ELXPMCAC
03234 ************************************************************      ELXPMCAC
03235 *                                                          *      ELXPMCAC
03236 *     TEST FOR PPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCAC
03237 *          FOR OUT OF NETWORK                              *      ELXPMCAC
03238 ************************************************************      ELXPMCAC
03239  3102-TEST-PPO-CSTCNMT-OUT.                                       ELXPMCAC
03240                                                                   ELXPMCAC
03241      EVALUATE TRUE                                                ELXPMCAC
03242         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAC
03243             SET WS-PPO-INC-FOUND TO TRUE                          ELXPMCAC
03244         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAC
03245             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03246         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAC
03247             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03248         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03249             SET WS-PPO-PEN-FOUND TO TRUE                          ELXPMCAC
03250             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCAC
03251         WHEN OTHER                                                ELXPMCAC
03252             CONTINUE                                              ELXPMCAC
03253      END-EVALUATE.                                                ELXPMCAC
03254                                                                   ELXPMCAC
03255 ************************************************************      ELXPMCAC
03256 *                                                          *      ELXPMCAC
03257 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCAC
03258 *          FOR IN NETWORK                                  *      ELXPMCAC
03259 ************************************************************      ELXPMCAC
03260  3104-TEST-RPO-CSTCNMT-IN.                                        ELXPMCAC
03261                                                                   ELXPMCAC
03262      EVALUATE TRUE                                                ELXPMCAC
03263         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCAC
03264             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCAC
03265         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCAC
03266             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCAC
03267         WHEN OTHER                                                ELXPMCAC
03268             CONTINUE                                              ELXPMCAC
03269      END-EVALUATE.                                                ELXPMCAC
03270                                                                   ELXPMCAC
03271 ************************************************************      ELXPMCAC
03272 *                                                          *      ELXPMCAC
03273 *     TEST FOR RPO PENALTIES OR INCENTIVES FOR COST CONT.  *      ELXPMCAC
03274 *          FOR OUT OF NETWORK                              *      ELXPMCAC
03275 ************************************************************      ELXPMCAC
03276  3104-TEST-RPO-CSTCNMT-OUT.                                       ELXPMCAC
03277                                                                   ELXPMCAC
03278      EVALUATE TRUE                                                ELXPMCAC
03279         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCAC
03280             SET WS-RPO-INC-FOUND TO TRUE                          ELXPMCAC
03281         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCAC
03282             SET WS-RPO-PEN-FOUND TO TRUE                          ELXPMCAC
03283         WHEN OTHER                                                ELXPMCAC
03284             CONTINUE                                              ELXPMCAC
03285      END-EVALUATE.                                                ELXPMCAC
03286                                                                   ELXPMCAC
03287 ************************************************************      ELXPMCAC
03288 *                                                          *      ELXPMCAC
03289 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCAC
03290 *          FOR REFERRAL OR NO REFERRAL REQUIRED            *      ELXPMCAC
03291 ************************************************************      ELXPMCAC
03292  3106-TEST-MCNP-CSTCNMT-IN.                                       ELXPMCAC
03293                                                                   ELXPMCAC
03294      EVALUATE TRUE                                                ELXPMCAC
03295         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCAC
03296             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCAC
03297         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCAC
03298             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCAC
03299         WHEN OTHER                                                ELXPMCAC
03300             CONTINUE                                              ELXPMCAC
03301      END-EVALUATE.                                                ELXPMCAC
03302                                                                   ELXPMCAC
03303 ************************************************************      ELXPMCAC
03304 *                                                          *      ELXPMCAC
03305 *     TEST FOR MCNP PENALTIES OR INCENTIVES FOR COST CONT. *      ELXPMCAC
03306 *          FOR NON REFERRAL                                *      ELXPMCAC
03307 ************************************************************      ELXPMCAC
03308  3106-TEST-MCNP-CSTCNMT-OUT.                                      ELXPMCAC
03309                                                                   ELXPMCAC
03310      EVALUATE TRUE                                                ELXPMCAC
03311         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCAC
03312             SET WS-MCNP-INC-FOUND TO TRUE                         ELXPMCAC
03313         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCAC
03314             SET WS-MCNP-PEN-FOUND TO TRUE                         ELXPMCAC
03315         WHEN OTHER                                                ELXPMCAC
03316             CONTINUE                                              ELXPMCAC
03317      END-EVALUATE.                                                ELXPMCAC
03318                                                                   ELXPMCAC
03319 ************************************************************      ELXPMCAC
03320 *                                                          *      ELXPMCAC
03321 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT. *       ELXPMCAC
03322 *                                                          *      ELXPMCAC
03323 ************************************************************      ELXPMCAC
03324  3108-TEST-BAE-CSTCNMT-IN.                                        ELXPMCAC
03325      EVALUATE TRUE                                                ELXPMCAC
03326         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03327             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCAC
03328         WHEN OTHER                                                ELXPMCAC
03329             CONTINUE                                              ELXPMCAC
03330      END-EVALUATE.                                                ELXPMCAC
03331                                                                   ELXPMCAC
03332 ************************************************************      ELXPMCAC
03333 *                                                          *      ELXPMCAC
03334 *     TEST FOR BAE PENALTIES OR INCENTIVES FOR COST CONT. *       ELXPMCAC
03335 *                                                          *      ELXPMCAC
03336 ************************************************************      ELXPMCAC
03337  3108-TEST-BAE-CSTCNMT-OUT.                                       ELXPMCAC
03338      EVALUATE TRUE                                                ELXPMCAC
03339         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03340             SET WS-BAE-PEN-FOUND TO TRUE                          ELXPMCAC
03341         WHEN OTHER                                                ELXPMCAC
03342             CONTINUE                                              ELXPMCAC
03343      END-EVALUATE.                                                ELXPMCAC
03344                                                                   ELXPMCAC
03345 ************************************************************      ELXPMCAC
03346 *                                                          *      ELXPMCAC
03347 *        INITIALIZE SPECIAL CASE FACTORS                   *      ELXPMCAC
03348 * RGO 3/95 - ADD CPO                                       *      ELXPMCAC
03349 ************************************************************      ELXPMCAC
03350  3250-SET-PRG-CONFIDENCE-FACTOR.                                  ELXPMCAC
03351                                                                   ELXPMCAC
03352      MOVE ATBL-CF-OV-FCTRS (ATBL-IDX) TO                          ELXPMCAC
03353                    ATBL-CF-SP-FCTRS (ATBL-IDX).                   ELXPMCAC
03354      EVALUATE TRUE                                                ELXPMCAC
03355         WHEN PMCI-PRV-PPO-IN                                      ELXPMCAC
03356            PERFORM 3260-SCN-SP-CCP-IPPO                           ELXPMCAC
03357         WHEN PMCI-PRV-PPO-OUT                                     ELXPMCAC
03358            PERFORM 3261-SCN-SP-CCP-OPPO                           ELXPMCAC
03359         WHEN PMCI-PRV-BAE-IN                                      ELXPMCAC
03360            PERFORM 3290-SCN-SP-CCP-IBAE                           ELXPMCAC
03361         WHEN PMCI-PRV-BAE-OUT                                     ELXPMCAC
03362            PERFORM 3291-SCN-SP-CCP-OBAE                           ELXPMCAC
03363         WHEN PMCI-PRV-RPO-IN                                      ELXPMCAC
03364            PERFORM 3270-SCN-SP-CCP-IRPO                           ELXPMCAC
03365         WHEN PMCI-PRV-RPO-OUT                                     ELXPMCAC
03366            PERFORM 3271-SCN-SP-CCP-ORPO                           ELXPMCAC
03367         WHEN PMCI-PRV-RPO-IN-PPO                                  ELXPMCAC
03368            PERFORM 3270-SCN-SP-CCP-IRPO                           ELXPMCAC
03369         WHEN PMCI-PRV-MCNP-REFER                                  ELXPMCAC
03370            PERFORM 3280-SCN-SP-CCP-IMCNP                          ELXPMCAC
03371         WHEN PMCI-PRV-MCNP-IN                                     ELXPMCAC
03372            PERFORM 3280-SCN-SP-CCP-IMCNP                          ELXPMCAC
03373         WHEN PMCI-PRV-MCNP-OUT                                    ELXPMCAC
03374            PERFORM 3281-SCN-SP-CCP-OMCNP                          ELXPMCAC
03375         WHEN PMCI-PRV-CALL                                        ELXPMCAC
03376            PERFORM 3290-SCN-SP-CCP-CALL                           ELXPMCAC
03377 *                                                                 ELXPMCAC
03378         WHEN PMCI-PRV-CPO-MET                                     ELXPMCAC
03379            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCAC
03380               MOVE WS-CF-ZERO TO                                  ELXPMCAC
03381                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCAC
03382            ELSE                                                   ELXPMCAC
03383               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K1'          ELXPMCAC
03384                  MOVE WS-CF-TRUE TO                               ELXPMCAC
03385                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03386               ELSE                                                ELXPMCAC
03387                  MOVE WS-CF-FALSE TO                              ELXPMCAC
03388                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03389               END-IF                                              ELXPMCAC
03390            END-IF                                                 ELXPMCAC
03391                                                                   ELXPMCAC
03392         WHEN PMCI-PRV-CPO-PPO-MET                                 ELXPMCAC
03393            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCAC
03394               MOVE WS-CF-ZERO TO                                  ELXPMCAC
03395                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03396            ELSE                                                   ELXPMCAC
03397               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'KI'         ELXPMCAC
03398                     MOVE WS-CF-TRUE TO                            ELXPMCAC
03399                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03400               ELSE                                                ELXPMCAC
03401                  MOVE WS-CF-FALSE TO                              ELXPMCAC
03402                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03403               END-IF                                              ELXPMCAC
03404            END-IF                                                 ELXPMCAC
03405         WHEN PMCI-PRV-CPO-PPO-NOT-MET                             ELXPMCAC
03406            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCAC
03407               MOVE WS-CF-ZERO TO                                  ELXPMCAC
03408                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03409            ELSE                                                   ELXPMCAC
03410               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'K4'         ELXPMCAC
03411                     MOVE WS-CF-TRUE TO                            ELXPMCAC
03412                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03413               ELSE                                                ELXPMCAC
03414                  MOVE WS-CF-FALSE TO                              ELXPMCAC
03415                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03416               END-IF                                              ELXPMCAC
03417            END-IF                                                 ELXPMCAC
03418 * ***** COMMUNITY BLUE    *****************************           ELXPMCAC
03419         WHEN PMCI-PRV-CBL-IN                                      ELXPMCAC
03420            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCAC
03421               MOVE WS-CF-ZERO TO                                  ELXPMCAC
03422                          ATBL-CF-SP-CST-CNTNMT(ATBL-IDX)          ELXPMCAC
03423            ELSE                                                   ELXPMCAC
03424               IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J1'          ELXPMCAC
03425                  MOVE WS-CF-TRUE TO                               ELXPMCAC
03426                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03427               ELSE                                                ELXPMCAC
03428                  MOVE WS-CF-FALSE TO                              ELXPMCAC
03429                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03430               END-IF                                              ELXPMCAC
03431            END-IF                                                 ELXPMCAC
03432                                                                   ELXPMCAC
03433         WHEN PMCI-PRV-CBL-OUT                                     ELXPMCAC
03434            IF ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'             ELXPMCAC
03435               MOVE WS-CF-ZERO TO                                  ELXPMCAC
03436                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03437            ELSE                                                   ELXPMCAC
03438               IF  ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'J4'         ELXPMCAC
03439                     MOVE WS-CF-TRUE TO                            ELXPMCAC
03440                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03441               ELSE                                                ELXPMCAC
03442                  MOVE WS-CF-FALSE TO                              ELXPMCAC
03443                          ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)         ELXPMCAC
03444               END-IF                                              ELXPMCAC
03445            END-IF                                                 ELXPMCAC
03446         WHEN OTHER                                                ELXPMCAC
03447            CONTINUE                                               ELXPMCAC
03448      END-EVALUATE.                                                ELXPMCAC
03449                                                                   ELXPMCAC
03450 ************************************************************      ELXPMCAC
03451 *                                                          *      ELXPMCAC
03452 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR PPO   *      ELXPMCAC
03453 *                                                          *      ELXPMCAC
03454 ************************************************************      ELXPMCAC
03455  3260-SCN-SP-CCP-IPPO.                                            ELXPMCAC
03456                                                                   ELXPMCAC
03457      EVALUATE TRUE                                                ELXPMCAC
03458         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03459           IF WS-PPO-INC-FOUND                                     ELXPMCAC
03460             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03461           ELSE                                                    ELXPMCAC
03462             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03463           END-IF                                                  ELXPMCAC
03464         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAC
03465             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03466             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03467         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAC
03468             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03469             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03470         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03471             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03472             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03473         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAC
03474             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03475             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03476         WHEN OTHER                                                ELXPMCAC
03477             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03478      END-EVALUATE.                                                ELXPMCAC
03479 ************************************************************      ELXPMCAC
03480 *                                                          *      ELXPMCAC
03481 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR BAE   *      ELXPMCAC
03482 *                                                          *      ELXPMCAC
03483 ************************************************************      ELXPMCAC
03484  3290-SCN-SP-CCP-IBAE.                                            ELXPMCAC
03485                                                                   ELXPMCAC
03486      EVALUATE TRUE                                                ELXPMCAC
03487         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03488           IF WS-BAE-PEN-FOUND                                     ELXPMCAC
03489             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03490           END-IF                                                  ELXPMCAC
03491         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03492             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03493             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03494         WHEN OTHER                                                ELXPMCAC
03495             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03496      END-EVALUATE.                                                ELXPMCAC
03497                                                                   ELXPMCAC
03498 ************************************************************      ELXPMCAC
03499 *                                                          *      ELXPMCAC
03500 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-PPO   *      ELXPMCAC
03501 *                                                          *      ELXPMCAC
03502 ************************************************************      ELXPMCAC
03503  3261-SCN-SP-CCP-OPPO.                                            ELXPMCAC
03504                                                                   ELXPMCAC
03505      EVALUATE TRUE                                                ELXPMCAC
03506         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03507           IF WS-PPO-PEN-FOUND                                     ELXPMCAC
03508               MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)ELXPMCAC
03509              ELSE                                                 ELXPMCAC
03510               MOVE WS-CF-ZERO  TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)ELXPMCAC
03511              END-IF                                               ELXPMCAC
03512         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAC
03513             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03514             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03515         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAC
03516             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03517             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03518         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03519             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03520             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03521         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAC
03522             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03523             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03524         WHEN OTHER                                                ELXPMCAC
03525             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03526      END-EVALUATE.                                                ELXPMCAC
03527                                                                   ELXPMCAC
03528 ************************************************************      ELXPMCAC
03529 *                                                          *      ELXPMCAC
03530 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-BAE   *      ELXPMCAC
03531 *                                                          *      ELXPMCAC
03532 ************************************************************      ELXPMCAC
03533  3291-SCN-SP-CCP-OBAE.                                            ELXPMCAC
03534                                                                   ELXPMCAC
03535      EVALUATE TRUE                                                ELXPMCAC
03536         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03537           IF WS-BAE-PEN-FOUND                                     ELXPMCAC
03538 *         IF WS-PPO-PEN-FOUND                                     ELXPMCAC
03539               MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)ELXPMCAC
03540              ELSE                                                 ELXPMCAC
03541               MOVE WS-CF-ZERO  TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)ELXPMCAC
03542              END-IF                                               ELXPMCAC
03543         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03544             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03545             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03546         WHEN OTHER                                                ELXPMCAC
03547             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03548      END-EVALUATE.                                                ELXPMCAC
03549                                                                   ELXPMCAC
03550 ************************************************************      ELXPMCAC
03551 *                                                          *      ELXPMCAC
03552 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR RPO   *      ELXPMCAC
03553 *                                                          *      ELXPMCAC
03554 ************************************************************      ELXPMCAC
03555  3270-SCN-SP-CCP-IRPO.                                            ELXPMCAC
03556                                                                   ELXPMCAC
03557      EVALUATE TRUE                                                ELXPMCAC
03558         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03559           IF WS-RPO-INC-FOUND                                     ELXPMCAC
03560             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03561           ELSE                                                    ELXPMCAC
03562             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03563           END-IF                                                  ELXPMCAC
03564         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCAC
03565             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03566             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03567         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCAC
03568             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03569             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03570         WHEN OTHER                                                ELXPMCAC
03571             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03572      END-EVALUATE.                                                ELXPMCAC
03573                                                                   ELXPMCAC
03574 ************************************************************      ELXPMCAC
03575 *                                                          *      ELXPMCAC
03576 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-RPO   *      ELXPMCAC
03577 *                                                          *      ELXPMCAC
03578 ************************************************************      ELXPMCAC
03579  3271-SCN-SP-CCP-ORPO.                                            ELXPMCAC
03580                                                                   ELXPMCAC
03581      EVALUATE TRUE                                                ELXPMCAC
03582         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03583           IF WS-RPO-PEN-FOUND                                     ELXPMCAC
03584             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03585           ELSE                                                    ELXPMCAC
03586             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03587           END-IF                                                  ELXPMCAC
03588         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCAC
03589             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03590             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03591         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCAC
03592             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03593             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03594         WHEN OTHER                                                ELXPMCAC
03595             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03596      END-EVALUATE.                                                ELXPMCAC
03597                                                                   ELXPMCAC
03598 ************************************************************      ELXPMCAC
03599 *                                                          *      ELXPMCAC
03600 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR FOR MCNP  *      ELXPMCAC
03601 *                                                          *      ELXPMCAC
03602 ************************************************************      ELXPMCAC
03603  3280-SCN-SP-CCP-IMCNP.                                           ELXPMCAC
03604                                                                   ELXPMCAC
03605      EVALUATE TRUE                                                ELXPMCAC
03606         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03607           IF WS-MCNP-INC-FOUND                                    ELXPMCAC
03608             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03609           ELSE                                                    ELXPMCAC
03610             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03611           END-IF                                                  ELXPMCAC
03612         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCAC
03613             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03614             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03615         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCAC
03616             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03617             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03618         WHEN OTHER                                                ELXPMCAC
03619             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03620      END-EVALUATE.                                                ELXPMCAC
03621                                                                   ELXPMCAC
03622 ************************************************************      ELXPMCAC
03623 *                                                          *      ELXPMCAC
03624 *  SCAN SPECIAL CASE PER COST CONTAINMENT FACTOR NON-MCNP  *      ELXPMCAC
03625 *                                                          *      ELXPMCAC
03626 ************************************************************      ELXPMCAC
03627  3281-SCN-SP-CCP-OMCNP.                                           ELXPMCAC
03628                                                                   ELXPMCAC
03629      EVALUATE TRUE                                                ELXPMCAC
03630         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03631           IF WS-MCNP-PEN-FOUND                                    ELXPMCAC
03632             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03633           ELSE                                                    ELXPMCAC
03634             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03635           END-IF                                                  ELXPMCAC
03636         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCAC
03637             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03638             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03639         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCAC
03640             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03641             MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03642         WHEN OTHER                                                ELXPMCAC
03643             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03644      END-EVALUATE.                                                ELXPMCAC
03645                                                                   ELXPMCAC
03646 ************************************************************      ELXPMCAC
03647 *                                                          *      ELXPMCAC
03648 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PRG*      ELXPMCAC
03649 *                                                          *      ELXPMCAC
03650 ************************************************************      ELXPMCAC
03651 *3290-SCN-SP-CCP-CALL.                                            ELXPMCAC
03652 *                                                                 ELXPMCAC
03653 *    EVALUATE TRUE                                                ELXPMCAC
03654 *       WHEN PMCI-PRG-PPO-APPLIES                                 ELXPMCAC
03655 *           PERFORM 3291-SCN-SP-CCP-CALL-PPO                      ELXPMCAC
03656 *       WHEN PMCI-PRG-RPO-APPLIES                                 ELXPMCAC
03657 *           PERFORM 3292-SCN-SP-CCP-CALL-RPO                      ELXPMCAC
03658 *       WHEN PMCI-PRG-RPO-PPO-APPLIES                             ELXPMCAC
03659 *           PERFORM 3292-SCN-SP-CCP-CALL-RPO                      ELXPMCAC
03660 *       WHEN PMCI-PRG-MCNP-APPLIES                                ELXPMCAC
03661 *           PERFORM 3293-SCN-SP-CCP-CALL-MCNP                     ELXPMCAC
03662 *       WHEN OTHER                                                ELXPMCAC
03663 *           MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03664 *    END-EVALUATE.                                                ELXPMCAC
03665                                                                   ELXPMCAC
03666 ************************************************************      ELXPMCAC
03667 *                                                          *      ELXPMCAC
03668 *SCAN SPECIAL CASE PER COST CONTAINMENT FACT. UNCERTAIN PRG*      ELXPMCAC
03669 *                                                          *      ELXPMCAC
03670 ************************************************************      ELXPMCAC
03671  3290-SCN-SP-CCP-CALL.                                            ELXPMCAC
03672                                                                   ELXPMCAC
03673      EVALUATE TRUE                                                ELXPMCAC
03674         WHEN PMCI-PRG-PPO-APPLIES                                 ELXPMCAC
03675             PERFORM 3291-SCN-SP-CCP-CALL-PPO                      ELXPMCAC
03676         WHEN PMCI-PRG-RPO-APPLIES                                 ELXPMCAC
03677             PERFORM 3292-SCN-SP-CCP-CALL-RPO                      ELXPMCAC
03678         WHEN PMCI-PRG-RPO-PPO-APPLIES                             ELXPMCAC
03679             PERFORM 3292-SCN-SP-CCP-CALL-RPO                      ELXPMCAC
03680         WHEN PMCI-PRG-MCNP-APPLIES                                ELXPMCAC
03681             PERFORM 3293-SCN-SP-CCP-CALL-MCNP                     ELXPMCAC
03682         WHEN PMCI-PRG-BAE-APPLIES                                 ELXPMCAC
03683             PERFORM 3294-SCN-SP-CCP-CALL-BAE                      ELXPMCAC
03684         WHEN OTHER                                                ELXPMCAC
03685             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03686      END-EVALUATE.                                                ELXPMCAC
03687                                                                   ELXPMCAC
03688 ************************************************************      ELXPMCAC
03689 *                                                          *      ELXPMCAC
03690 *SET CALL FOR PPO FACTORS                                  *      ELXPMCAC
03691 *                                                          *      ELXPMCAC
03692 ************************************************************      ELXPMCAC
03693  3291-SCN-SP-CCP-CALL-PPO.                                        ELXPMCAC
03694                                                                   ELXPMCAC
03695      EVALUATE TRUE                                                ELXPMCAC
03696         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03697             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03698         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '91'              ELXPMCAC
03699             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03700             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03701         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '94'              ELXPMCAC
03702             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03703             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03704         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'G4'              ELXPMCAC
03705             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03706             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03707         WHEN OTHER                                                ELXPMCAC
03708             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03709      END-EVALUATE.                                                ELXPMCAC
03710 ************************************************************      ELXPMCAC
03711 *                                                          *      ELXPMCAC
03712 *SET CALL FOR BAREFACTORS                                  *      ELXPMCAC
03713 *                                                          *      ELXPMCAC
03714 ************************************************************      ELXPMCAC
03715  3294-SCN-SP-CCP-CALL-BAE.                                        ELXPMCAC
03716                                                                   ELXPMCAC
03717      EVALUATE TRUE                                                ELXPMCAC
03718         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'N4'              ELXPMCAC
03719             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03720             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03721         WHEN OTHER                                                ELXPMCAC
03722             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03723      END-EVALUATE.                                                ELXPMCAC
03724 ************************************************************      ELXPMCAC
03725 *                                                          *      ELXPMCAC
03726 *SET CALL FOR RPO FACTORS                                  *      ELXPMCAC
03727 *                                                          *      ELXPMCAC
03728 ************************************************************      ELXPMCAC
03729  3292-SCN-SP-CCP-CALL-RPO.                                        ELXPMCAC
03730                                                                   ELXPMCAC
03731      EVALUATE TRUE                                                ELXPMCAC
03732         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03733             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03734         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R1'              ELXPMCAC
03735             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03736             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03737         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'R4'              ELXPMCAC
03738             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03739             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03740         WHEN OTHER                                                ELXPMCAC
03741             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03742      END-EVALUATE.                                                ELXPMCAC
03743 ************************************************************      ELXPMCAC
03744 *                                                          *      ELXPMCAC
03745 *SET CALL FOR MCNP FACTORS                                 *      ELXPMCAC
03746 *                                                          *      ELXPMCAC
03747 ************************************************************      ELXPMCAC
03748  3293-SCN-SP-CCP-CALL-MCNP.                                       ELXPMCAC
03749                                                                   ELXPMCAC
03750      EVALUATE TRUE                                                ELXPMCAC
03751         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = '00'              ELXPMCAC
03752             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03753         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P8'              ELXPMCAC
03754             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03755             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03756         WHEN ATBL-COST-CONTAIN-IND (ATBL-IDX) = 'P9'              ELXPMCAC
03757             SET WS-PRG-VAR-FOUND TO TRUE                          ELXPMCAC
03758             MOVE WS-CF-ZERO TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)   ELXPMCAC
03759         WHEN OTHER                                                ELXPMCAC
03760             MOVE WS-CF-FALSE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX)  ELXPMCAC
03761      END-EVALUATE.                                                ELXPMCAC
03762      IF PMCI-REFERRAL-EXISTS                                      ELXPMCAC
03763         MOVE WS-CF-TRUE TO ATBL-CF-SP-CST-CNTNMT (ATBL-IDX).      ELXPMCAC
03764                                                                   ELXPMCAC
03765                                                                   ELXPMCAC
03766 ************************************************************      ELXPMCAC
03767 *                                                          *      ELXPMCAC
03768 *     MOVE OVERALLS TO SPECIAL CASE                        *      ELXPMCAC
03769 *                                                          *      ELXPMCAC
03770 ************************************************************      ELXPMCAC
03771                                                                   ELXPMCAC
03772  4000-MOVE-OVRLLS-TO-SPEC.                                        ELXPMCAC
03773       MOVE ATBL-CF-OV-FCTRS (ATBL-IDX)                            ELXPMCAC
03774         TO ATBL-CF-SP-FCTRS (ATBL-IDX).                           ELXPMCAC
03775                                                                   ELXPMCAC
03776 ************************************************************      ELXPMCAC
03777 *                                                          *      ELXPMCAC
03778 *    CALL ELKFLMB                                          *      ELXPMCAC
03779 *                                                          *      ELXPMCAC
03780 ************************************************************      ELXPMCAC
03781                                                                   ELXPMCAC
03782  5000-CALL-ELKFLCMB.                                              ELXPMCAC
03783      CALL 'ELKFLCMB' USING ATBL-CF-WORK-ENTRY (ATBL-IDX)          ELXPMCAC
03784                            WS-CF-1                                ELXPMCAC
03785                            WS-CF-2                                ELXPMCAC
03786                            WS-CF-3                                ELXPMCAC
03787                            WS-CF-4                                ELXPMCAC
03788                            WS-CF-5                                ELXPMCAC
03789                            WS-CF-6                                ELXPMCAC
03790      END-CALL.                                                    ELXPMCAC
03791 ************************************************************      ELXPMCAC
03792 *                                                          *      ELXPMCAC
03793 *    SET ATBL INDEX TO SAVE INDEX FOR SELECTED ACCUM       *      ELXPMCAC
03794 *                                                          *      ELXPMCAC
03795 ************************************************************      ELXPMCAC
03796                                                                   ELXPMCAC
03797  5300-SET-ATBL-SUBSCRIPT.                                         ELXPMCAC
03798      MOVE SPACES TO WS-SAVE-COVER-TYPE.                           ELXPMCAC
03799      IF WS-BS-TEST-SUB NOT EQUAL ZERO AND                         ELXPMCAC
03800                 WS-MM-TEST-SUB NOT EQUAL ZERO                     ELXPMCAC
03801         MOVE '+' TO WS-SAVE-COVER-TYPE                            ELXPMCAC
03802         SET ATBL-IDX TO WS-BS-TEST-SUB                            ELXPMCAC
03803      ELSE                                                         ELXPMCAC
03804         IF WS-MM-TEST-SUB NOT ZERO                                ELXPMCAC
03805            MOVE '*' TO WS-SAVE-COVER-TYPE                         ELXPMCAC
03806            SET ATBL-IDX TO WS-MM-TEST-SUB                         ELXPMCAC
03807         ELSE                                                      ELXPMCAC
03808            SET ATBL-IDX TO WS-BS-TEST-SUB.                        ELXPMCAC
03809                                                                   ELXPMCAC
03810 ******************************************************************ELXPMCAC
03811 *                                                                *ELXPMCAC
03812 *    CREATE ACCUM CDE TABLE FOR REAL MED PROCESSING              *ELXPMCAC
03813 *                                                                *ELXPMCAC
03814 ******************************************************************ELXPMCAC
03815  5400-CREATE-ACCUM-CDE-TABLE.                                     ELXPMCAC
03816                                                                   ELXPMCAC
03817      SET CIA-ELSRLMED-DDN TO TRUE.                                ELXPMCAC
03818      SET ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE   TO NULL.       ELXPMCAC
03819      CALL 'ELUSAVAD'                                              ELXPMCAC
03820         USING DFHCOMMAREA                                         ELXPMCAC
03821               ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE             ELXPMCAC
03822                                                                   ELXPMCAC
03823      CALL 'ELUSETAD'                                              ELXPMCAC
03824         USING DFHCOMMAREA                                         ELXPMCAC
03825               ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE             ELXPMCAC
03826                                                                   ELXPMCAC
03827      COMPUTE CIA-AREA-LEN =                                       ELXPMCAC
03828           LENGTH OF AC-ATBL-TBL-CNT                               ELXPMCAC
03829         + (CIA-MVO * LENGTH OF AC-ATBL-ACCUMULATOR).              ELXPMCAC
03830      SET CIA-STG-GETMAIN TO TRUE.                                 ELXPMCAC
03831                                                                   ELXPMCAC
03832      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA.                  ELXPMCAC
03833                                                                   ELXPMCAC
03834      IF NOT CIA-RC-OK                                             ELXPMCAC
03835         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCAC
03836         MOVE +4002 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCAC
03837         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCAC
03838         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELXPMCAC
03839         EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC              ELXPMCAC
03840      ELSE                                                         ELXPMCAC
03841         CALL 'ELUSETAD'                                           ELXPMCAC
03842            USING DFHCOMMAREA                                      ELXPMCAC
03843                  ADDRESS OF ACCDE-ATBL-ACCUMULATOR-TABLE          ELXPMCAC
03844         IF CIA-RC-PTR-NULL                                        ELXPMCAC
03845            SET SW-TRMNL-ERR TO TRUE                               ELXPMCAC
03846            MOVE +4002 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCAC
03847            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCAC
03848            SET CIA-AB-UNALLOC-AREA TO TRUE                        ELXPMCAC
03849            EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC           ELXPMCAC
03850         ELSE                                                      ELXPMCAC
03851            CONTINUE.                                              ELXPMCAC
03852                                                                   ELXPMCAC
03853                                                                   ELXPMCAC
03854 ******************************************************************ELXPMCAC
03855 *                                                                *ELXPMCAC
03856 *    LOAD   ACCUM CDE TABLE FOR REAL MED PROCESSING              *ELXPMCAC
03857 *                                                                *ELXPMCAC
03858 ******************************************************************ELXPMCAC
03859  5500-LOAD-ACCUM-CDE-ATBL-TABLE.                                  ELXPMCAC
03860                                                                   ELXPMCAC
03861      ADD 1 TO AC-ATBL-TBL-CNT                                     ELXPMCAC
03862      SET AC-ATBL-IDX TO AC-ATBL-TBL-CNT                           ELXPMCAC
03863                                                                   ELXPMCAC
03864                                                                   ELXPMCAC
03865      MOVE ATBL-ACCUM-DESC (ATBL-IDX) TO                           ELXPMCAC
03866           AC-ATBL-ACCUM-DESC (AC-ATBL-IDX)                        ELXPMCAC
03867                                                                   ELXPMCAC
03868      IF ATBL-PSEU-NBR-USING-IND (ATBL-IDX) = 'Y'                  ELXPMCAC
03869           MOVE ATBL-PSEUDO-GRP-NBR (ATBL-IDX) TO                  ELXPMCAC
03870                AC-ATBL-PSEUDO-GRP-NO  (AC-ATBL-IDX)               ELXPMCAC
03871           MOVE ATBL-PSEUDO-SECT-NBR (ATBL-IDX) TO                 ELXPMCAC
03872                AC-ATBL-PSEUDO-SEC-NO  (AC-ATBL-IDX)               ELXPMCAC
03873         ELSE                                                      ELXPMCAC
03874           MOVE ATBL-GRP-NBR (ATBL-IDX) TO                         ELXPMCAC
03875                AC-ATBL-PSEUDO-GRP-NO (AC-ATBL-IDX)                ELXPMCAC
03876           MOVE ATBL-SECT-NBR (ATBL-IDX) TO                        ELXPMCAC
03877                AC-ATBL-PSEUDO-SEC-NO (AC-ATBL-IDX)                ELXPMCAC
03878      END-IF                                                       ELXPMCAC
03879                                                                   ELXPMCAC
03880      MOVE ATBL-CON-FEAK-IND (ATBL-IDX)  TO                        ELXPMCAC
03881            AC-ATBL-CON-FEAK-IND (AC-ATBL-IDX)                     ELXPMCAC
03882      MOVE ATBL-CON-BGN-DT-MMDD (ATBL-IDX) TO                      ELXPMCAC
03883           AC-ATBL-CON-BGN-DT-MMDD (AC-ATBL-IDX)                   ELXPMCAC
03884                                                                   ELXPMCAC
03885      MOVE ATBL-BENEFIT-PERIOD (ATBL-IDX)  TO                      ELXPMCAC
03886           AC-ATBL-BENEFIT-PERIOD (AC-ATBL-IDX)                    ELXPMCAC
03887      MOVE ATBL-FAM-OR-INDIV (ATBL-IDX)  TO                        ELXPMCAC
03888           AC-ATBL-FAM-OR-INDIV (AC-ATBL-IDX)                      ELXPMCAC
03889      MOVE ATBL-L-O-B (ATBL-IDX)  TO                               ELXPMCAC
03890           AC-ATBL-L-O-B (AC-ATBL-IDX)                             ELXPMCAC
03891      MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-IDX)  TO                 ELXPMCAC
03892           AC-ATBL-INTERNAL-DESC (AC-ATBL-IDX)                     ELXPMCAC
03893      MOVE ATBL-SERVICE-GROUP (ATBL-IDX)  TO                       ELXPMCAC
03894           AC-ATBL-SERVICE-GROUP (AC-ATBL-IDX)                     ELXPMCAC
03895      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-IDX)  TO                  ELXPMCAC
03896           AC-ATBL-P-O-T (AC-ATBL-IDX)                             ELXPMCAC
03897      MOVE ATBL-CONDITION (ATBL-IDX)  TO                           ELXPMCAC
03898           AC-ATBL-CONDITION (AC-ATBL-IDX)                         ELXPMCAC
03899      MOVE ATBL-CO-PAY-IND (ATBL-IDX) TO                           ELXPMCAC
03900           AC-ATBL-CO-PAY-IND (AC-ATBL-IDX)                        ELXPMCAC
03901      MOVE ATBL-COST-CONTAIN-IND (ATBL-IDX) TO                     ELXPMCAC
03902           AC-ATBL-COST-CONT-IND (AC-ATBL-IDX)                     ELXPMCAC
03903      MOVE ATBL-AGE-LIMIT-FROM (ATBL-IDX) TO                       ELXPMCAC
03904           AC-ATBL-AGE-LIMIT-FROM (AC-ATBL-IDX)                    ELXPMCAC
03905      MOVE ATBL-AGE-LIMIT-TO (ATBL-IDX) TO                         ELXPMCAC
03906           AC-ATBL-AGE-LIMIT-TO (AC-ATBL-IDX)                      ELXPMCAC
03907      MOVE ATBL-AGE-QUAL-FROM (ATBL-IDX) TO                        ELXPMCAC
03908           AC-ATBL-AGE-QUAL-FROM (AC-ATBL-IDX)                     ELXPMCAC
03909      MOVE ATBL-AGE-QUAL-TO (ATBL-IDX) TO                          ELXPMCAC
03910           AC-ATBL-AGE-QUAL-TO (AC-ATBL-IDX)                       ELXPMCAC
03911                                                                   ELXPMCAC
03912 **THE FOLLOWING FIELDS APPLY ONLY TO SPECIFIC ACCUMS              ELXPMCAC
03913                                                                   ELXPMCAC
03914      IF ATBL-ACCUM-DESC (ATBL-IDX) =                              ELXPMCAC
03915                         '#ACL  ' OR '#ADL  ' OR '#ACP  '          ELXPMCAC
03916           MOVE ATBL-MANDATORY-IND (ATBL-IDX)  TO                  ELXPMCAC
03917               AC-ATBL-MANDATORY-IND (AC-ATBL-IDX)                 ELXPMCAC
03918         ELSE                                                      ELXPMCAC
03919           MOVE    SPACES                      TO                  ELXPMCAC
03920               AC-ATBL-MANDATORY-IND (AC-ATBL-IDX)                 ELXPMCAC
03921      END-IF                                                       ELXPMCAC
03922                                                                   ELXPMCAC
03923      IF ATBL-ACCUM-DESC (ATBL-IDX) = '#ACL  '                     ELXPMCAC
03924           MOVE ATBL-BISCEND-IND (ATBL-IDX)  TO                    ELXPMCAC
03925                AC-ATBL-BISCENDING-IND (AC-ATBL-IDX)               ELXPMCAC
03926         ELSE                                                      ELXPMCAC
03927           MOVE    SPACES                    TO                    ELXPMCAC
03928                AC-ATBL-BISCENDING-IND (AC-ATBL-IDX)               ELXPMCAC
03929      END-IF                                                       ELXPMCAC
03930                                                                   ELXPMCAC
03931                                                                   ELXPMCAC
03932      IF ATBL-ACCUM-DESC (ATBL-IDX) = '#ACL  ' OR '#AOL  '         ELXPMCAC
03933           MOVE ATBL-PERCENT-LEVEL (ATBL-IDX) TO                   ELXPMCAC
03934                AC-ATBL-PERCENT-LEVEL (AC-ATBL-IDX)                ELXPMCAC
03935         ELSE                                                      ELXPMCAC
03936           MOVE ZEROES TO                                          ELXPMCAC
03937                AC-ATBL-PERCENT-LEVEL (AC-ATBL-IDX)                ELXPMCAC
03938      END-IF                                                       ELXPMCAC
03939                                                                   ELXPMCAC
03940      MOVE ATBL-VALUE-QUALIFIER (ATBL-IDX) TO                      ELXPMCAC
03941           AC-ATBL-VALUE-QUALIFIER (AC-ATBL-IDX).                  ELXPMCAC
03942                                                                   ELXPMCAC
03943 ******************************************************************ELXPMCAC
03944 *                                                                *ELXPMCAC
03945 *    FREEMIAN CSAC POINTERS                                      *ELXPMCAC
03946 *                                                                *ELXPMCAC
03947 ******************************************************************ELXPMCAC
03948  9999-DO-FREEMAINS.                                               ELXPMCAC
03949      IF CSAC-ABM-GC-TBL-PTR = NULL                                ELXPMCAC
03950         CONTINUE                                                  ELXPMCAC
03951      ELSE                                                         ELXPMCAC
03952         EXEC CICS FREEMAIN DATAPOINTER(CSAC-ABM-GC-TBL-PTR)       ELXPMCAC
03953         END-EXEC                                                  ELXPMCAC
03954                                                                   ELXPMCAC
03955      END-IF.                                                      ELXPMCAC
03956      IF CSAC-ACL-GC-TBL-PTR = NULL                                ELXPMCAC
03957         CONTINUE                                                  ELXPMCAC
03958      ELSE                                                         ELXPMCAC
03959         EXEC CICS FREEMAIN DATAPOINTER(CSAC-ACL-GC-TBL-PTR)       ELXPMCAC
03960         END-EXEC                                                  ELXPMCAC
03961      END-IF.                                                      ELXPMCAC
03962      IF CSAC-ACP-GC-TBL-PTR = NULL                                ELXPMCAC
03963         CONTINUE                                                  ELXPMCAC
03964      ELSE                                                         ELXPMCAC
03965         EXEC CICS FREEMAIN DATAPOINTER(CSAC-ACP-GC-TBL-PTR)       ELXPMCAC
03966         END-EXEC                                                  ELXPMCAC
03967      END-IF.                                                      ELXPMCAC
03968      IF CSAC-ADL-GC-TBL-PTR = NULL                                ELXPMCAC
03969         CONTINUE                                                  ELXPMCAC
03970      ELSE                                                         ELXPMCAC
03971         EXEC CICS FREEMAIN DATAPOINTER(CSAC-ADL-GC-TBL-PTR)       ELXPMCAC
03972         END-EXEC                                                  ELXPMCAC
03973      END-IF.                                                      ELXPMCAC
03974      IF CSAC-AOL-GC-TBL-PTR = NULL                                ELXPMCAC
03975         CONTINUE                                                  ELXPMCAC
03976      ELSE                                                         ELXPMCAC
03977         EXEC CICS FREEMAIN DATAPOINTER(CSAC-AOL-GC-TBL-PTR)       ELXPMCAC
03978         END-EXEC                                                  ELXPMCAC
03979      END-IF.                                                      ELXPMCAC
03980                                                                   ELXPMCAC
