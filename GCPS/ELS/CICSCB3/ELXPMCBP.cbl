00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCBP
00003  PROGRAM-ID.         ELXPMCBP.                                       LV004
00004                                                                   ELXPMCBP
00005  AUTHOR.             ANNE KEFFER-KING.                            ELXPMCBP
00006                      COMPLETED AND REWRITTEN BY R. LUKETICH.      ELXPMCBP
00007                                                                   ELXPMCBP
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCBP
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCBP
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCBP
00011                      233 N. MICHIGAN AVE                          ELXPMCBP
00012                      CHICAGO, ILLINOIS 60601                      ELXPMCBP
00013                                                                   ELXPMCBP
00014  DATE-WRITTEN.       16-JUL-1992.                                 ELXPMCBP
00015                                                                   ELXPMCBP
00016  DATE-COMPILED.                                                   ELXPMCBP
00017                                                                   ELXPMCBP
00018  SECURITY.           COPYRIGHT 1992,                              ELXPMCBP
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCBP
00020      SKIP3                                                        ELXPMCBP
00021  TITLE 'NA/ES- BENEFIT PROVISION NA/ES PROGRAM         '.         ELXPMCBP
00022  ENVIRONMENT DIVISION.                                            ELXPMCBP
00023                                                                   ELXPMCBP
00024  CONFIGURATION SECTION.                                           ELXPMCBP
00025  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCBP
00026  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCBP
00027      EJECT                                                        ELXPMCBP
00028 ******************************************************************ELXPMCBP
00029 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCBP
00030 *    PROGRAM:    ELXPMCBP                                        *ELXPMCBP
00031 *    DATE:       16-JUL-1992                                     *ELXPMCBP
00032 *    AUTHOR:     ANNE KEFFER-KING                                *ELXPMCBP
00033 *    FUNCTION:                                                   *ELXPMCBP
00034 *      DETERMINE WHICH OF THE BENENFIT PROVISION BASED DATA      *ELXPMCBP
00035 *      REQUESTED IN THE NA/ES SYSTEM ARE COVERED/NOT COVERED OR  *ELXPMCBP
00036 *      ARE AMBIGUOUS.  GENERALLY, THE CONTRACT RECORD WILL BE    *ELXPMCBP
00037 *      SEARCHED FOR EACH REQUIRED BENEFIT PROVISION AND A        *ELXPMCBP
00038 *      THE STATUS FLAGGED.  A FEW OF THE PROVISIONS REQUIRE      *ELXPMCBP
00039 *      ADDITIONAL PROCESSING.  IN THOSE CASES THE SLOT NUMBER    *ELXPMCBP
00040 *      OF THE BENEFIT PROVISION WILL ALSO BE SAVED FOR FURTHER   *ELXPMCBP
00041 *      PROCESSING.                                               *ELXPMCBP
00042 *                                                                *ELXPMCBP
00043 *                                                                *ELXPMCBP
00044 ******************************************************************ELXPMCBP
00045 *                                                                *ELXPMCBP
00046 *                      MAINTENANCE HISTORY                       *ELXPMCBP
00047 *                                                                *ELXPMCBP
00048 * MOD      DATE     BY  DRPT                ACTION               *ELXPMCBP
00049 * ----- ----------- --- ----- ---------------------------------- *ELXPMCBP
00050 * 01.00 16-JUL-1992 AKK       CREATED                            *ELXPMCBP
00051 *                                                                *ELXPMCBP
00052 * 01.01 30-JUL-1992 AKK       ADDING LOGIC TO READ EACH PROVISION*ELXPMCBP
00053 *                             AND PVE                            *ELXPMCBP
00054 * 01.02 28-SEP-1992 AKK       ADDING LOGIC TO READ CLDR TAB WHEN *ELXPMCBP
00055 *                             IT IS CODED.  THIS IS FOR GPO +    *ELXPMCBP
00056 *                             IPO ONLY.                          *ELXPMCBP
00057 * 01.03 11-NOV-1992 JPB       ADDED LOGIC TO SET CONDITION       *ELXPMCBP
00058 *                             SWITCH TO NO IF THE CONDITION      *ELXPMCBP
00059 *                             DOES NOT EXIST.                    *ELXPMCBP
00060 * 01.04 22-FEB-1993 BAK       ADDED LOGIC TO HANDLE BOTH MEDICAL *ELXPMCBP
00061 *                             EQUIPMENT PURCHASE AND RENTAL--    *ELXPMCBP
00062 *                             DMRI B, DERO B, DMRI E AND DMRO E &*ELXPMCBP
00063 *                             DMEI B, DEMO B, DMEI E AND DMEO E. *ELXPMCBP
00064 *                             ALSO IF LIFE THREATENING HOURS ARE *ELXPMCBP
00065 *                             ZERO SET NOT APPLICABLE TO TRUE.   *ELXPMCBP
00066 * 01.05 02-MAR-1993 CGL       ADDED LOGIC TO HANDLE PROFESSIONAL *ELXPMCBP
00067 *                             OUTPATIENT DRUG AND ALCOHOL ABUSE. *ELXPMCBP
00068 * 01.06 05-MAR-1993 CGL       ADDED LOGIC TO CHECK CHC NO COVERAG*ELXPMCBP
00069 * 02.00 JUNE 7-1993 RGO       ISSR 13071.                        *ELXPMCBP
00070 *                             1. CHECK THE PROVISIONS OF MAJOR   *ELXPMCBP
00071 *                                MEDICAL/SUPPLEMENTAL CONTRACT.  *ELXPMCBP
00072 *                                THERE MUST AT LEAST BE A BASIC  *ELXPMCBP
00073 *                                OR SUPPLEMENTAL CONTRACT. BOTH  *ELXPMCBP
00074 *                                COULD EXIST.                    *ELXPMCBP
00075 *                             2. IN 7200, ADD THE WS-PASS-VALUE  *ELXPMCBP
00076 *                                ( 1 = BASIC, 2 = SUPPLEMENTAL)  *ELXPMCBP
00077 *                                TO THE CORRESPONDING DERIVATIVE *ELXPMCBP
00078 *                                INDICATOR VALUE.                *ELXPMCBP
00079 *                             3. TRANSLATE THE DERIVATIVE VALUE  *ELXPMCBP
00080 *                                TO A CHARACTER VALUE AND UPDATE *ELXPMCBP
00081 *                                THE PMCI-COMM-AREA.             *ELXPMCBP
00082 *                                                                *ELXPMCBP
00083 *                             4. DELETE THE MOVE OF SLOT.        *ELXPMCBP
00084 *                                                                *ELXPMCBP
00085 * 03.00 17-SEP-1993 BAK       ISSR 13071 -- PHASE 2 ADD DRB      *ELXPMCBP
00086 *                             CHECK FOR INST. INPATIENT AND SET  *ELXPMCBP
00087 *                             PMCI DRBF NDICATOR ACCORDINGLY.    *ELXPMCBP
00088 *                                                                *ELXPMCBP
00089 * 04.00 16-FEB-1995 RGO       SUPPLEMENTAL MEDICARE PROJECT.     *ELXPMCBP
00090 *                             ADDED THE FIELDS:                  *ELXPMCBP
00091 *                             HOME VISITS - PO                   *ELXPMCBP
00092 *                             PROSTHETICS - II, PI,IO, PO        *ELXPMCBP
00093 *                             MEDICAL SUPPLIES - II, PI,IO, PO   *ELXPMCBP
00094 *                             COPYLIB MEMBER ELSPMCID WAS CHANGED*ELXPMCBP
00095 *                             TO ADD THE CORRESPONDING BENIFIT   *ELXPMCBP
00096 *                             PROVISION.                         *ELXPMCBP
00097 *                                                                *ELXPMCBP
00098 * 04.01 01-APR-2003 AKK       MORE CHANGES DUE TO ENDEVOR        *ELXPMCBP
00099 *                                                                 ELXPMCBP
00100 * 04.02 24-JUN-2003 AKK       REGN'D FOR CHANGES TO CALLED PGMS. *ELXPMCBP
00101 * 05.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCBP
00102 *                                                                *ELXPMCBP
00103 * 05.01 09-DEC-2004 AKK S0C7 INTERTEST                           *ELXPMCBP
00104 *                                                                *ELXPMCBP
00105 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCBP
00106 *                                                                *ELXPMCBP
00107 ******************************************************************ELXPMCBP
00108      EJECT                                                        ELXPMCBP
00109  DATA DIVISION.                                                   ELXPMCBP
00110  WORKING-STORAGE SECTION.                                         ELXPMCBP
00111  01  WS-HDG                      PIC X(32)                        ELXPMCBP
00112           VALUE '******* WS STARTS HERE ******'.                  ELXPMCBP
00113                                                                   ELXPMCBP
00114  01  WS-PASS-VALUE               PIC S9(1) COMP-3 VALUE 0.        ELXPMCBP
00115  01  WS-DMEI-DRVD-CHR            PIC      X(01) VALUE SPACES.     ELXPMCBP
00116                                                                   ELXPMCBP
00117  01  WS-SWITCHES.                                                 ELXPMCBP
00118      02 WS-TERMINAL-SWITCH       PIC      X(01) VALUE 'N'.        ELXPMCBP
00119         88 SW-NO-TRMNL-ERR                       VALUE 'N'.       ELXPMCBP
00120         88 SW-TRMNL-ERR                          VALUE 'Y'.       ELXPMCBP
00121      02 WS-CLDR-SWITCH            PIC      X(01) VALUE 'N'.       ELXPMCBP
00122         88 SW-CLDR-NOT-FOUND                     VALUE 'N'.       ELXPMCBP
00123         88 SW-CLDR-FOUND                         VALUE 'Y'.       ELXPMCBP
00124      02 WS-DMR-PROCESSING        PIC      X(01) VALUE 'N'.        ELXPMCBP
00125         88 SW-DMR-NO                             VALUE 'N'.       ELXPMCBP
00126         88 SW-DMR-YES                            VALUE 'Y'.       ELXPMCBP
00127      02 WS-DME-PROCESSING        PIC      X(01) VALUE 'N'.        ELXPMCBP
00128         88 SW-DME-NO                             VALUE 'N'.       ELXPMCBP
00129         88 SW-DME-YES                            VALUE 'Y'.       ELXPMCBP
00130      02 WS-PVE-SWITCH            PIC      X(01) VALUE 'N'.        ELXPMCBP
00131         88 SW-PVE-NOT-FND                        VALUE 'N'.       ELXPMCBP
00132         88 SW-PVE-FND                            VALUE 'Y'.       ELXPMCBP
00133      02 WS-PVE-TABLE-SWITCH      PIC      X(01) VALUE 'N'.        ELXPMCBP
00134         88 SW-PVE-TBL-NOT-FND                    VALUE 'N'.       ELXPMCBP
00135         88 SW-PVE-TBL-FND                        VALUE 'Y'.       ELXPMCBP
00136      02 WS-PROVIDER-TYPE-SWITCH  PIC      X(01) VALUE 'N'.        ELXPMCBP
00137         88 SW-PRVDR-TYP-NOT-FND                  VALUE 'N'.       ELXPMCBP
00138         88 SW-PRVDR-TYP-FND                      VALUE 'Y'.       ELXPMCBP
00139      02 WS-BENEFIT-PROV-RESOLVED PIC      X(01) VALUE 'N'.        ELXPMCBP
00140         88 SW-BNFT-PRVSN-NOT-RSLVD               VALUE 'N'.       ELXPMCBP
00141         88 SW-BNFT-PRVSN-RSLVD                   VALUE 'Y'.       ELXPMCBP
00142                                                                   ELXPMCBP
00143  01  WS-MISC-CLDR-RESULTS.                                        ELXPMCBP
00144                                                                   ELXPMCBP
00145      02 WS-BSC-SAVE-ALC           PIC      X(01) VALUE '0'.       ELXPMCBP
00146         88 SW-BSC-ALC-YES                        VALUE '1'.       ELXPMCBP
00147         88 SW-BSC-ALC-NO                         VALUE '0'.       ELXPMCBP
00148         88 SW-BSC-ALC-CALL                       VALUE 'C'.       ELXPMCBP
00149                                                                   ELXPMCBP
00150      02 WS-BSC-SAVE-DRG           PIC      X(01) VALUE '0'.       ELXPMCBP
00151         88 SW-BSC-DRG-YES                        VALUE '1'.       ELXPMCBP
00152         88 SW-BSC-DRG-NO                         VALUE '0'.       ELXPMCBP
00153         88 SW-BSC-DRG-CALL                       VALUE 'C'.       ELXPMCBP
00154                                                                   ELXPMCBP
00155      02 WS-BSC-SAVE-PSY           PIC      X(01) VALUE '0'.       ELXPMCBP
00156         88 SW-BSC-PSY-YES                        VALUE '1'.       ELXPMCBP
00157         88 SW-BSC-PSY-NO                         VALUE '0'.       ELXPMCBP
00158         88 SW-BSC-PSY-CALL                       VALUE 'C'.       ELXPMCBP
00159                                                                   ELXPMCBP
00160      02 WS-MM-SAVE-ALC            PIC      X(01) VALUE '0'.       ELXPMCBP
00161         88 SW-MM-ALC-YES                         VALUE '1'.       ELXPMCBP
00162         88 SW-MM-ALC-NO                          VALUE '0'.       ELXPMCBP
00163         88 SW-MM-ALC-CALL                        VALUE 'C'.       ELXPMCBP
00164                                                                   ELXPMCBP
00165      02 WS-MM-SAVE-DRG            PIC      X(01) VALUE '0'.       ELXPMCBP
00166         88 SW-MM-DRG-YES                         VALUE '1'.       ELXPMCBP
00167         88 SW-MM-DRG-NO                          VALUE '0'.       ELXPMCBP
00168         88 SW-MM-DRG-CALL                        VALUE 'C'.       ELXPMCBP
00169                                                                   ELXPMCBP
00170      02 WS-MM-SAVE-PSY            PIC      X(01) VALUE '0'.       ELXPMCBP
00171         88 SW-MM-PSY-YES                         VALUE '1'.       ELXPMCBP
00172         88 SW-MM-PSY-NO                          VALUE '0'.       ELXPMCBP
00173         88 SW-MM-PSY-CALL                        VALUE 'C'.       ELXPMCBP
00174                                                                   ELXPMCBP
00175  01  WS-MISC.                                                     ELXPMCBP
00176      05 WS-CLDR-CF               COMP-1   VALUE +0.000000E+00.    ELXPMCBP
00177      05 WS-CF-CLDR-NO            COMP-1   VALUE -0.333300E+00.    ELXPMCBP
00178      05 WS-CF-CLDR-YES           COMP-1   VALUE +0.333300E+00.    ELXPMCBP
00179      05 WS-GCT-MAX-IDX           INDEX.                           ELXPMCBP
00180      05 WS-GCP-MAX-IDX           INDEX.                           ELXPMCBP
00181      05 WS-GBJ-MAX-IDX           INDEX.                           ELXPMCBP
00182      05 WS-CLDR                  PIC      X(06)                   ELXPMCBP
00183                                                  VALUE '#CLDR '.  ELXPMCBP
00184      05 WS-PVE                   PIC      X(06)                   ELXPMCBP
00185                                                  VALUE '#PVE  '.  ELXPMCBP
00186      05 WS-BP-TEST-ID            PIC      X(06).                  ELXPMCBP
00187         88 WS-BP-II-DMR-REN                      VALUE 'DMRI B'.  ELXPMCBP
00188         88 WS-BP-PI-DMR-REN                      VALUE 'DMRI E'.  ELXPMCBP
00189         88 WS-BP-IO-DMR-REN                      VALUE 'DMRO B'.  ELXPMCBP
00190         88 WS-BP-PO-DMR-REN                      VALUE 'DMRO E'.  ELXPMCBP
00191         88 WS-BP-II-DME-PUR                      VALUE 'DMEI B'.  ELXPMCBP
00192         88 WS-BP-PI-DME-PUR                      VALUE 'DMEI E'.  ELXPMCBP
00193         88 WS-BP-IO-DME-PUR                      VALUE 'DMEO B'.  ELXPMCBP
00194         88 WS-BP-PO-DME-PUR                      VALUE 'DMEO E'.  ELXPMCBP
00195         88 WS-BP-II-PVTA                         VALUE 'PVTA A'.  ELXPMCBP
00196         88 WS-BP-II-PVTR                         VALUE 'PVTR A'.  ELXPMCBP
00197                                                                   ELXPMCBP
00198                                                                   ELXPMCBP
00199                                                                   ELXPMCBP
00200  01  WS-RECORD-DATA-SAVES.                                        ELXPMCBP
00201                                                                   ELXPMCBP
00202      05 WS-PVTA-LOB-IND          PIC    X(01) VALUE SPACES.       ELXPMCBP
00203      05 WS-PVTR-LOB-IND          PIC    X(01) VALUE SPACES.       ELXPMCBP
00204      05 WS-PVTA-PRIC-METH        PIC    X(02) VALUE ZEROS.        ELXPMCBP
00205      05 WS-PVTR-PRIC-METH        PIC    X(02) VALUE ZEROS.        ELXPMCBP
00206      05 WS-PVTA-PRIC-METH-BSC    PIC    X(02) VALUE ZEROS.        ELXPMCBP
00207      05 WS-PVTR-PRIC-METH-BSC    PIC    X(02) VALUE ZEROS.        ELXPMCBP
00208      05 WS-PVTA-PRIC-METH-MM     PIC    X(02) VALUE ZEROS.        ELXPMCBP
00209      05 WS-PVTR-PRIC-METH-MM     PIC    X(02) VALUE ZEROS.        ELXPMCBP
00210                                                                   ELXPMCBP
00211                                                                   ELXPMCBP
00212      05 WS-DMR-CERT-REQ-IND-BSC  PIC      X(02) VALUE ZEROS.      ELXPMCBP
00213      05 WS-DME-CERT-REQ-IND-BSC  PIC      X(02) VALUE ZEROS.      ELXPMCBP
00214      05 WS-DMR-CERT-REQ-IND-MM   PIC      X(02) VALUE ZEROS.      ELXPMCBP
00215      05 WS-DME-CERT-REQ-IND-MM   PIC      X(02) VALUE ZEROS.      ELXPMCBP
00216      05 WS-DME-LOB-IND           PIC      X(01) VALUE SPACES.     ELXPMCBP
00217                                                                   ELXPMCBP
00218                                                                   ELXPMCBP
00219      05 WS-DAYS-BTWN-ACCD-EMRG    PIC  S999 COMP-3 VALUE ZEROS.   ELXPMCBP
00220      05 WS-DAYS-BTWN-MED-EMRG     PIC  S999 COMP-3 VALUE ZEROS.   ELXPMCBP
00221      05 WS-DAYS-BTWN-LIFE-THRT    PIC  S999 COMP-3 VALUE ZEROS.   ELXPMCBP
00222                                                                   ELXPMCBP
00223                                                                   ELXPMCBP
00224      05 WS-REN-CERT-REQUIRED-VALUE PIC X(02) VALUE ZEROS.         ELXPMCBP
00225          88  WS-REN-CERT-REQUIRED            VALUE 'A7' 'B9'      ELXPMCBP
00226                                                    '0I' '0L'      ELXPMCBP
00227                                                    '02'.          ELXPMCBP
00228          88  WS-NO-REN-CERT-REQUIRED         VALUE '00'.          ELXPMCBP
00229          88  WS-REN-CERT-CALL                VALUE 'CA'.          ELXPMCBP
00230                                                                   ELXPMCBP
00231      05 WS-PUR-CERT-REQUIRED-VALUE PIC X(02) VALUE ZEROS.         ELXPMCBP
00232          88  WS-PUR-CERT-REQUIRED            VALUE 'A7' 'B9'      ELXPMCBP
00233                                                    '0I' '0L'      ELXPMCBP
00234                                                    '02'.          ELXPMCBP
00235          88  WS-NO-PUR-CERT-REQUIRED         VALUE '00'.          ELXPMCBP
00236          88  WS-PUR-CERT-CALL                VALUE 'CA'.          ELXPMCBP
00237                                                                   ELXPMCBP
00238      05  WS-PPM-GROUPINGS            PIC X(02) VALUE ZEROS.       ELXPMCBP
00239          88  WS-BILLED-AMOUNT                VALUE '01'.          ELXPMCBP
00240          88  WS-PERCENT-BILLED-AMOUNT        VALUE '03'.          ELXPMCBP
00241          88  WS-MCSP                         VALUE '02' '34'.     ELXPMCBP
00242          88  WS-MCSP-PLUS-ADDN               VALUE '08' '35'.     ELXPMCBP
00243          88  WS-PERCENT-MCSP                 VALUE '12'.          ELXPMCBP
00244          88  WS-AV-SEMI-PRIV-ROOM            VALUE '10'.          ELXPMCBP
00245          88  WS-AV-SEMI-PRIV-ROOM-PLUS       VALUE '11'.          ELXPMCBP
00246          88  WS-PERCENT-AV-SEMI-PRIV-ROOM    VALUE '13'.          ELXPMCBP
00247          88  WS-FLAT-RATE                    VALUE '04'.          ELXPMCBP
00248          88  WS-FLAT-RATE-PLUS-ADDN          VALUE '14' '21'      ELXPMCBP
00249                                                    '22' '33'.     ELXPMCBP
00250          88  WS-PPM-CALL                     VALUE '00' '05' '06' ELXPMCBP
00251                                                    '07' '09' '15' ELXPMCBP
00252                                                    '16' '17' '18' ELXPMCBP
00253                                                    '19' '20' '23' ELXPMCBP
00254                                                    '24' '25' '26' ELXPMCBP
00255                                                    '27' '28' '29' ELXPMCBP
00256                                                    '30' '31' '32' ELXPMCBP
00257                                                    '36' '37' '38' ELXPMCBP
00258                                                    '39' '40' '41' ELXPMCBP
00259                                                    '42' '43' '44' ELXPMCBP
00260                                                    '45' '46' '47' ELXPMCBP
00261                                                    '48' '49' '50'.ELXPMCBP
00262 /                                                                 ELXPMCBP
00263  01  WS-PVE-TABLE.                                                ELXPMCBP
00264      02 WS-PVE-TBL-NBR-ENTRS     PIC S9(04) COMP.                 ELXPMCBP
00265      02 WS-PVE-TBL-DETAIL        OCCURS 50 TIMES                  ELXPMCBP
00266                                  INDEXED BY WS-PVE-IDX            ELXPMCBP
00267                                             WS-PVE-MAX-IDX.       ELXPMCBP
00268         03 WS-PVE-SLOT-NO        PIC S9(07).                      ELXPMCBP
00269         03 WS-PVE-RSLT           PIC      X(01).                  ELXPMCBP
00270            88 WS-PVE-PRVDR-TYP-ELGBL             VALUE 'Y'.       ELXPMCBP
00271            88 WS-PVE-PRVDR-TYP-NOT-ELGBL         VALUE 'N'.       ELXPMCBP
00272                                                                   ELXPMCBP
00273 /LIST OF MENTAL DIAGNOSIS CODES                                   ELXPMCBP
00274  COPY ELSDMNLC.                                                   ELXPMCBP
00275                                                                   ELXPMCBP
00276  COPY ELSALCLC.                                                   ELXPMCBP
00277                                                                   ELXPMCBP
00278  COPY ELSDRGLC.                                                   ELXPMCBP
00279                                                                   ELXPMCBP
00280 /CONFIDENCE FACTOR MASTER LIST                                    ELXPMCBP
00281  COPY ELSCFDBC.                                                   ELXPMCBP
00282                                                                   ELXPMCBP
00283  LINKAGE SECTION.                                                 ELXPMCBP
00284  01  DFHCOMMAREA.                                                 ELXPMCBP
00285  COPY ELSCOMMC.                                                   ELXPMCBP
00286 /ELS COMMON INTERFACE AREA                                        ELXPMCBP
00287  COPY ELSCIA2C.                                                   ELXPMCBP
00288 /PMCI RESULTS COMMON AREA                                         ELXPMCBP
00289  01   PMCI-COMM-AREA.                                             ELXPMCBP
00290  COPY PMCCOMM.                                                    ELXPMCBP
00291 /ELS KEY WORK AREA                                                ELXPMCBP
00292  COPY ELSKEYSC.                                                   ELXPMCBP
00293 /ELS INPUT OUTPUT PARAMETERS BLOCK                                ELXPMCBP
00294  COPY ELSIOPMC.                                                   ELXPMCBP
00295 /NAES INTERFACE WORK AREA                                         ELXPMCBP
00296  COPY ELSPMCID.                                                   ELXPMCBP
00297 /NAES INTERFACE BENEFIT PROVISION LIST OVERLAY                    ELXPMCBP
00298  COPY ELSPMCBP.                                                   ELXPMCBP
00299 /CONTRACT RECORD                                                  ELXPMCBP
00300  01 CONTRACT-RECORD.                                              ELXPMCBP
00301  COPY GCCONTRC.                                                   ELXPMCBP
00302 /CLDR TABULAR RECORD                                              ELXPMCBP
00303  01 CLDR-TABULAR-RECORD.                                          ELXPMCBP
00304  COPY GCTCLDRC.                                                   ELXPMCBP
00305 /BENEFIT PROVISION RECORD                                         ELXPMCBP
00306  01 BENEFIT-PROVISION-RECORD.                                     ELXPMCBP
00307  COPY GCBENPVC.                                                   ELXPMCBP
00308 /PROVIDER ELIGIBILTY TABULARE RECORD                              ELXPMCBP
00309  01 PROVIDER-ELIGIBILITY-RECORD.                                  ELXPMCBP
00310  COPY GCTPVEC.                                                    ELXPMCBP
00311                                                                   ELXPMCBP
00312 /***********************************************************      ELXPMCBP
00313 *                                                          *      ELXPMCBP
00314 *                    PROCEDURE DIVISION                    *      ELXPMCBP
00315 *                                                          *      ELXPMCBP
00316 ************************************************************      ELXPMCBP
00317                                                                   ELXPMCBP
00318  PROCEDURE DIVISION.                                              ELXPMCBP
00319                                                                   ELXPMCBP
00320 ************************************************************      ELXPMCBP
00321 *                                                          *      ELXPMCBP
00322 *    ELXPMCBP MAINLINE                                     *      ELXPMCBP
00323 *                                                          *      ELXPMCBP
00324 ************************************************************      ELXPMCBP
00325                                                                   ELXPMCBP
00326  0000-ELSPMCBP-MAINLINE.                                          ELXPMCBP
00327      SET SW-NO-TRMNL-ERR TO TRUE.                                 ELXPMCBP
00328      PERFORM 0100-ESTABLISH-ADDRESSABILITY.                       ELXPMCBP
00329      IF SW-NO-TRMNL-ERR                                           ELXPMCBP
00330         PERFORM 1000-PROCESS.                                     ELXPMCBP
00331      GOBACK.                                                      ELXPMCBP
00332                                                                   ELXPMCBP
00333 ************************************************************      ELXPMCBP
00334 *                                                          *      ELXPMCBP
00335 *    ESTABLISH ADDRESSABILITY TO THE ELS ENVIRONMENT       *      ELXPMCBP
00336 *                                                          *      ELXPMCBP
00337 *  ADDRESSABILITY TO THE BASIC AND SUPPLEMENTAL CONTRACTS  *      ELXPMCBP
00338 *  WILL BE DONE IN THE 2000,3000,4000 AND 5000 PROCS.      *      ELXPMCBP
00339 *                                            RGO 6/8/93    *      ELXPMCBP
00340 *                                                          *      ELXPMCBP
00341 ************************************************************      ELXPMCBP
00342                                                                   ELXPMCBP
00343  0100-ESTABLISH-ADDRESSABILITY.                                   ELXPMCBP
00344      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCBP
00345                  ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.        ELXPMCBP
00346      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCBP
00347      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
00348                   ADDRESS OF PMCI-COMM-AREA.                      ELXPMCBP
00349      IF CIA-RC-OK                                                 ELXPMCBP
00350         SET PMCI-BC-SUCCESSFUL                                    ELXPMCBP
00351             PMCI-BC-NO-ERROR                                      ELXPMCBP
00352          TO TRUE                                                  ELXPMCBP
00353         SET CIA-ELSPMCID-DDN TO TRUE                              ELXPMCBP
00354         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCBP
00355                         ADDRESS OF NAES-INTERMEDIATE-DATA         ELXPMCBP
00356         IF CIA-RC-OK                                              ELXPMCBP
00357            PERFORM 0110-ESTAB-RMNNG-ADRSBLTY                      ELXPMCBP
00358         ELSE                                                      ELXPMCBP
00359            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBP
00360            MOVE +3001 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBP
00361            SET SW-TRMNL-ERR TO TRUE                               ELXPMCBP
00362         END-IF                                                    ELXPMCBP
00363      ELSE                                                         ELXPMCBP
00364         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCBP
00365      END-IF.                                                      ELXPMCBP
00366                                                                   ELXPMCBP
00367 ************************************************************      ELXPMCBP
00368 *                                                          *      ELXPMCBP
00369 *    ESTABLISH REMAINING ADDRESSABLITY                     *      ELXPMCBP
00370 *                                                          *      ELXPMCBP
00371 *   THIS NEEDS TO BE DONE ONLY 1 TIME.         RGO 6/8/93  *      ELXPMCBP
00372 ************************************************************      ELXPMCBP
00373                                                                   ELXPMCBP
00374  0110-ESTAB-RMNNG-ADRSBLTY.                                       ELXPMCBP
00375      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELXPMCBP
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
00377                   ADDRESS OF KWA-FILE-KEY-WORK-AREA.              ELXPMCBP
00378      IF CIA-RC-PTR-NULL                                           ELXPMCBP
00379         SET CIA-STG-GETMAIN TO TRUE                               ELXPMCBP
00380         CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL       ELXPMCBP
00381         SET CIA-ELSKEYS-DDN TO TRUE                               ELXPMCBP
00382         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCBP
00383                               ADDRESS OF KWA-FILE-KEY-WORK-AREA.  ELXPMCBP
00384                                                                   ELXPMCBP
00385      SET CIA-GCBENPRV-DDN TO TRUE.                                ELXPMCBP
00386      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
00387                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELXPMCBP
00388      IF CIA-RC-PTR-NULL                                           ELXPMCBP
00389         SET CIA-STG-GETMAIN TO TRUE                               ELXPMCBP
00390         CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL       ELXPMCBP
00391         SET CIA-GCBENPRV-DDN TO TRUE                              ELXPMCBP
00392         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCBP
00393                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELXPMCBP
00394                                                                   ELXPMCBP
00395      SET CIA-GCTABULR-DDN TO TRUE.                                ELXPMCBP
00396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
00397                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELXPMCBP
00398      IF CIA-RC-PTR-NULL                                           ELXPMCBP
00399         SET CIA-STG-GETMAIN TO TRUE                               ELXPMCBP
00400         CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL       ELXPMCBP
00401         SET CIA-GCTABULR-DDN TO TRUE                              ELXPMCBP
00402         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCBP
00403                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELXPMCBP
00404                                                                   ELXPMCBP
00405 ************************************************************      ELXPMCBP
00406 *                                                          *      ELXPMCBP
00407 *    PROCESS BENEFIT PROVISION LISTS FOR RESULTS           *      ELXPMCBP
00408 *                                                          *      ELXPMCBP
00409 ************************************************************      ELXPMCBP
00410                                                                   ELXPMCBP
00411  1000-PROCESS.                                                    ELXPMCBP
00412      IF PMCI-BLUE-STORM-CALL                                      ELXPMCBP
00413         SET PMCI-PAT-SEX-FEMALE TO TRUE                           ELXPMCBP
00414      END-IF.                                                      ELXPMCBP
00415      EVALUATE TRUE                                                ELXPMCBP
00416      WHEN PMCI-INSTITUTIONAL                                      ELXPMCBP
00417         EVALUATE TRUE                                             ELXPMCBP
00418         WHEN PMCI-INPATIENT                                       ELXPMCBP
00419             SET ADDRESS OF BPLS-BNFT-PRVSN-LST-OVRLY              ELXPMCBP
00420              TO ADDRESS OF NAES-INSTITUTIONAL-IP-BEN-PROV         ELXPMCBP
00421             PERFORM 1100-PROCESS-BASIC-MM                         ELXPMCBP
00422             PERFORM 1200-CONVERT-DRVD-CHR                         ELXPMCBP
00423                VARYING BPLS-INDEX FROM 1 BY 1                     ELXPMCBP
00424                   UNTIL BPLS-INDEX > BPLS-MAX-INDEX               ELXPMCBP
00425             IF PMCI-BC-SUCCESSFUL                                 ELXPMCBP
00426                PERFORM 2100-DTRMN-II-RSLTS                        ELXPMCBP
00427             ELSE                                                  ELXPMCBP
00428                CONTINUE                                           ELXPMCBP
00429             END-IF                                                ELXPMCBP
00430                                                                   ELXPMCBP
00431         WHEN PMCI-OUTPATIENT                                      ELXPMCBP
00432                                                                   ELXPMCBP
00433             SET ADDRESS OF BPLS-BNFT-PRVSN-LST-OVRLY              ELXPMCBP
00434              TO ADDRESS OF NAES-INSTITUTIONAL-OP-BEN-PROV         ELXPMCBP
00435                                                                   ELXPMCBP
00436 *         RGO 6/8/93                                              ELXPMCBP
00437             PERFORM 1100-PROCESS-BASIC-MM                         ELXPMCBP
00438             PERFORM 1200-CONVERT-DRVD-CHR                         ELXPMCBP
00439                VARYING BPLS-INDEX FROM 1 BY 1                     ELXPMCBP
00440                   UNTIL BPLS-INDEX > BPLS-MAX-INDEX               ELXPMCBP
00441                                                                   ELXPMCBP
00442             IF PMCI-BC-SUCCESSFUL                                 ELXPMCBP
00443                PERFORM 3100-DTRMN-IO-RSLTS                        ELXPMCBP
00444             ELSE                                                  ELXPMCBP
00445                CONTINUE                                           ELXPMCBP
00446             END-IF                                                ELXPMCBP
00447                WHEN OTHER                                         ELXPMCBP
00448                   SET PMCI-BC-INVALID-DATA TO TRUE                ELXPMCBP
00449                   MOVE +3004 TO PMCI-BLUE-CHIP-ERROR-CODE         ELXPMCBP
00450                END-EVALUATE                                       ELXPMCBP
00451      WHEN PMCI-PROFESSIONAL                                       ELXPMCBP
00452         EVALUATE TRUE                                             ELXPMCBP
00453         WHEN PMCI-INPATIENT                                       ELXPMCBP
00454                                                                   ELXPMCBP
00455             SET ADDRESS OF BPLS-BNFT-PRVSN-LST-OVRLY              ELXPMCBP
00456              TO ADDRESS OF NAES-PROFESSIONAL-IP-BEN-PROV          ELXPMCBP
00457 *                                                                 ELXPMCBP
00458             PERFORM 1100-PROCESS-BASIC-MM                         ELXPMCBP
00459             PERFORM 1200-CONVERT-DRVD-CHR                         ELXPMCBP
00460                VARYING BPLS-INDEX FROM 1 BY 1                     ELXPMCBP
00461                   UNTIL BPLS-INDEX > BPLS-MAX-INDEX               ELXPMCBP
00462                                                                   ELXPMCBP
00463             IF PMCI-BC-SUCCESSFUL                                 ELXPMCBP
00464                PERFORM 4100-DTRMN-PI-RSLTS                        ELXPMCBP
00465             ELSE                                                  ELXPMCBP
00466                CONTINUE                                           ELXPMCBP
00467             END-IF                                                ELXPMCBP
00468         WHEN PMCI-OUTPATIENT                                      ELXPMCBP
00469                                                                   ELXPMCBP
00470             SET ADDRESS OF BPLS-BNFT-PRVSN-LST-OVRLY              ELXPMCBP
00471              TO ADDRESS OF NAES-PROFESSIONAL-OP-BEN-PROV          ELXPMCBP
00472 *                                                                 ELXPMCBP
00473             PERFORM 1100-PROCESS-BASIC-MM                         ELXPMCBP
00474             PERFORM 1200-CONVERT-DRVD-CHR                         ELXPMCBP
00475                VARYING BPLS-INDEX FROM 1 BY 1                     ELXPMCBP
00476                   UNTIL BPLS-INDEX > BPLS-MAX-INDEX               ELXPMCBP
00477             IF PMCI-BC-SUCCESSFUL                                 ELXPMCBP
00478                PERFORM 5100-DTRMN-PO-RSLTS                        ELXPMCBP
00479             ELSE                                                  ELXPMCBP
00480                CONTINUE                                           ELXPMCBP
00481             END-IF                                                ELXPMCBP
00482         WHEN OTHER                                                ELXPMCBP
00483            SET PMCI-BC-INVALID-DATA TO TRUE                       ELXPMCBP
00484            MOVE +3004 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBP
00485         END-EVALUATE                                              ELXPMCBP
00486      WHEN OTHER                                                   ELXPMCBP
00487         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
00488         MOVE +3003 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
00489      END-EVALUATE.                                                ELXPMCBP
00490                                                                   ELXPMCBP
00491 ************************************************************      ELXPMCBP
00492 ************************************************************      ELXPMCBP
00493                                                                   ELXPMCBP
00494  1100-PROCESS-BASIC-MM.                                           ELXPMCBP
00495                                                                   ELXPMCBP
00496      SET CIA-ELSCONIB-DDN TO TRUE                                 ELXPMCBP
00497      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
00498                            ADDRESS OF CONTRACT-RECORD             ELXPMCBP
00499      IF CIA-RC-OK                                                 ELXPMCBP
00500         MOVE 1 TO WS-PASS-VALUE                                   ELXPMCBP
00501         PERFORM 1150-FINISH-PROCESSING.                           ELXPMCBP
00502      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBP
00503         SET CIA-ELSCONIS-DDN TO TRUE                              ELXPMCBP
00504         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCBP
00505                                ADDRESS OF CONTRACT-RECORD         ELXPMCBP
00506         IF CIA-RC-OK                                              ELXPMCBP
00507            MOVE 2 TO WS-PASS-VALUE                                ELXPMCBP
00508            PERFORM 1150-FINISH-PROCESSING.                        ELXPMCBP
00509      IF WS-PASS-VALUE = 0                                         ELXPMCBP
00510         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
00511         MOVE +3002 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBP
00512 ************************************************************      ELXPMCBP
00513                                                                   ELXPMCBP
00514  1150-FINISH-PROCESSING.                                          ELXPMCBP
00515                                                                   ELXPMCBP
00516      IF WS-PASS-VALUE = 1                                         ELXPMCBP
00517         MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT TO                     ELXPMCBP
00518                      WS-DAYS-BTWN-ACCD-EMRG                       ELXPMCBP
00519         MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT TO                      ELXPMCBP
00520                      WS-DAYS-BTWN-MED-EMRG                        ELXPMCBP
00521         MOVE GCT-DAYS-BTW-LIFE-THREAT-TRMT TO                     ELXPMCBP
00522                      WS-DAYS-BTWN-LIFE-THRT.                      ELXPMCBP
00523                                                                   ELXPMCBP
00524      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBP
00525         SET GCT-INDEX TO GCT-COUNT-BEN-PROVN-POINTERS             ELXPMCBP
00526         SET WS-GCT-MAX-IDX TO GCT-INDEX                           ELXPMCBP
00527         PERFORM 7000-GET-BNFT-PRVNS.                              ELXPMCBP
00528      IF PMCI-BLUE-STORM-CALL                                      ELXPMCBP
00529         CONTINUE                                                  ELXPMCBP
00530      ELSE                                                         ELXPMCBP
00531      IF PMCI-PROFESSIONAL AND PMCI-OUTPATIENT                     ELXPMCBP
00532         PERFORM 6500-READ-CLDR-TABS.                              ELXPMCBP
00533                                                                   ELXPMCBP
00534 ************************************************************      ELXPMCBP
00535 * 1200-CONVERT-DRVD-CHR                                    *      ELXPMCBP
00536 *                                                          *      ELXPMCBP
00537 *  -- USING THE TABLE OVERLAY OF THE NAES RECORD,          *      ELXPMCBP
00538 *     CONVERT THE NUMERIC DERIVATION FIELD TO A            *      ELXPMCBP
00539 *     CHARATER FIELD.                                      *      ELXPMCBP
00540 *                                                          *      ELXPMCBP
00541 *   NUMERIC DRVD ==> CHAR DRVD   MEANING                   *      ELXPMCBP
00542 *     0  OR 1         BLANK      BASIC ONLY OR NOT COVERED.*      ELXPMCBP
00543 *        2            *          MAJOR MEDICAL ONLY.       *      ELXPMCBP
00544 *        3            +          BOTH BASIC AND MAJOR      *      ELXPMCBP
00545 *                                MEDICAL.                  *      ELXPMCBP
00546 *                                                          *      ELXPMCBP
00547 * NOTE: THIS 1200- PROC WAS CREATED NEW BY RGO 6/8/93.     *      ELXPMCBP
00548 ************************************************************      ELXPMCBP
00549                                                                   ELXPMCBP
00550  1200-CONVERT-DRVD-CHR.                                           ELXPMCBP
00551      IF BPLS-DRVD-IND (BPLS-INDEX) = 0                            ELXPMCBP
00552         OR                                                        ELXPMCBP
00553         BPLS-DRVD-IND (BPLS-INDEX) = 1                            ELXPMCBP
00554            MOVE ' ' TO BPLS-DRVD-CHR (BPLS-INDEX)                 ELXPMCBP
00555      ELSE                                                         ELXPMCBP
00556          IF BPLS-DRVD-IND (BPLS-INDEX) = 2                        ELXPMCBP
00557            MOVE '*' TO BPLS-DRVD-CHR (BPLS-INDEX)                 ELXPMCBP
00558          ELSE                                                     ELXPMCBP
00559              IF BPLS-DRVD-IND (BPLS-INDEX) = 3                    ELXPMCBP
00560                 MOVE '+' TO BPLS-DRVD-CHR (BPLS-INDEX)            ELXPMCBP
00561              ELSE                                                 ELXPMCBP
00562                  CONTINUE                                         ELXPMCBP
00563              END-IF                                               ELXPMCBP
00564          END-IF                                                   ELXPMCBP
00565      END-IF.                                                      ELXPMCBP
00566                                                                   ELXPMCBP
00567 ************************************************************      ELXPMCBP
00568 *                                                          *      ELXPMCBP
00569 *    GET INSTITUTIONAL INPATIENT BENEFIT RESULTS           *      ELXPMCBP
00570 *                                                          *      ELXPMCBP
00571 *                                             RGO 6/8/93   *      ELXPMCBP
00572 ************************************************************      ELXPMCBP
00573                                                                   ELXPMCBP
00574 *2000-GET-II-BENEFIT-PROV.                                        ELXPMCBP
00575                                                                   ELXPMCBP
00576 ************************************************************      ELXPMCBP
00577 *                                                          *      ELXPMCBP
00578 *    DETERMINE INSTITUTIONAL INPATIENT RESULTS             *      ELXPMCBP
00579 *                                                          *      ELXPMCBP
00580 ************************************************************      ELXPMCBP
00581                                                                   ELXPMCBP
00582  2100-DTRMN-II-RSLTS.                                             ELXPMCBP
00583                                                                   ELXPMCBP
00584      IF NAES-II-AMB-YES                                           ELXPMCBP
00585         SET AMBULANCE-YES TO TRUE                                 ELXPMCBP
00586         MOVE NAES-II-AMB-DRVD-CHR TO PMCI-AMB-FROM-IND            ELXPMCBP
00587      ELSE                                                         ELXPMCBP
00588         SET AMBULANCE-NO TO TRUE.                                 ELXPMCBP
00589      IF NAES-II-ARPI-YES                                          ELXPMCBP
00590         SET SUB-ABUSE-ALC-YES TO TRUE                             ELXPMCBP
00591         MOVE NAES-II-ARPI-DRVD-CHR TO PMCI-SBSTNCE-ABS-ALC-IND    ELXPMCBP
00592      ELSE                                                         ELXPMCBP
00593         SET SUB-ABUSE-ALC-NO TO TRUE.                             ELXPMCBP
00594      IF NAES-II-CHC-YES                                           ELXPMCBP
00595         MOVE NAES-II-CHC-DRVD-CHR TO PMCI-CHC-FROM-IND            ELXPMCBP
00596      ELSE                                                         ELXPMCBP
00597         SET CHC-NO-COVERAGE TO TRUE.                              ELXPMCBP
00598      IF NAES-II-DRB-YES                                           ELXPMCBP
00599         SET DRB-YES TO TRUE                                       ELXPMCBP
00600      ELSE                                                         ELXPMCBP
00601         SET DRB-NO TO TRUE                                        ELXPMCBP
00602      IF NAES-II-DRPI-YES                                          ELXPMCBP
00603         SET SUB-ABUSE-DRG-YES TO TRUE                             ELXPMCBP
00604         MOVE NAES-II-DRPI-DRVD-CHR TO PMCI-SBSTNCE-ABS-DRG-IND    ELXPMCBP
00605                                                                   ELXPMCBP
00606      ELSE                                                         ELXPMCBP
00607         SET SUB-ABUSE-DRG-NO TO TRUE.                             ELXPMCBP
00608      IF NAES-II-PSYI-YES                                          ELXPMCBP
00609         SET PSY-YES TO TRUE                                       ELXPMCBP
00610         MOVE NAES-II-PSYI-DRVD-CHR TO PMCI-PSYCH-FROM-IND         ELXPMCBP
00611      ELSE                                                         ELXPMCBP
00612         SET PSY-NO TO TRUE.                                       ELXPMCBP
00613      IF NAES-II-DMRI-YES                                          ELXPMCBP
00614         MOVE NAES-II-DMRI-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
00615         SET SW-DMR-YES TO TRUE.                                   ELXPMCBP
00616      IF NAES-II-DMEI-YES                                          ELXPMCBP
00617         SET SW-DME-YES TO TRUE                                    ELXPMCBP
00618         MOVE NAES-II-DMEI-DRVD-CHR TO WS-DME-LOB-IND.             ELXPMCBP
00619      IF SW-DME-YES OR SW-DMR-YES                                  ELXPMCBP
00620         PERFORM 6100-DTRMN-DME-DMR-RSLT                           ELXPMCBP
00621      ELSE                                                         ELXPMCBP
00622         SET DME-NO TO TRUE                                        ELXPMCBP
00623         SET DME-MD-CERT-NO TO TRUE.                               ELXPMCBP
00624      IF NAES-II-LABI-YES                                          ELXPMCBP
00625         SET PMCI-LAB-YES TO TRUE                                  ELXPMCBP
00626         MOVE NAES-II-LABI-DRVD-CHR TO PMCI-LAB-FROM-IND           ELXPMCBP
00627      ELSE                                                         ELXPMCBP
00628         SET PMCI-LAB-NO TO TRUE.                                  ELXPMCBP
00629      IF PMCI-PAT-SEX-FEMALE                                       ELXPMCBP
00630         PERFORM 2200-DTRMN-II-OB-RSLTS                            ELXPMCBP
00631      ELSE                                                         ELXPMCBP
00632         SET OB-NORM-NO                                            ELXPMCBP
00633             OB-COMP-NO TO TRUE.                                   ELXPMCBP
00634      IF NAES-II-FOTI-YES                                          ELXPMCBP
00635         SET PMCI-OCC-THRPY-COVERED TO TRUE                        ELXPMCBP
00636        MOVE NAES-II-FOTI-DRVD-CHR TO PMCI-OCCPTNL-THRPY-FROM-IND  ELXPMCBP
00637      ELSE                                                         ELXPMCBP
00638         SET PMCI-OCC-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
00639      IF NAES-II-PMTI-YES                                          ELXPMCBP
00640         SET PMCI-PHY-THRPY-COVERED TO TRUE                        ELXPMCBP
00641         MOVE NAES-II-PMTI-DRVD-CHR TO PMCI-PHYSCL-THRPY-FROM-IND  ELXPMCBP
00642      ELSE                                                         ELXPMCBP
00643         SET PMCI-PHY-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
00644      IF NAES-II-NRSI-YES                                          ELXPMCBP
00645         SET PMCI-PDN-COVERED TO TRUE                              ELXPMCBP
00646        MOVE NAES-II-NRSI-DRVD-CHR TO PMCI-PRVTE-DTY-NRS-FROM-IND  ELXPMCBP
00647      ELSE                                                         ELXPMCBP
00648         SET PMCI-PDN-NOT-COVERED TO TRUE.                         ELXPMCBP
00649      IF NAES-II-PVTA-YES OR NAES-II-PVTR-YES                      ELXPMCBP
00650         PERFORM 2110-DTRMN-II-PVT-RM-CNT-TYP                      ELXPMCBP
00651      ELSE                                                         ELXPMCBP
00652         SET PRRT-NOT-APPLICABLE TO TRUE.                          ELXPMCBP
00653      IF NAES-II-SPTI-YES                                          ELXPMCBP
00654         SET SPT-YES TO TRUE                                       ELXPMCBP
00655         MOVE NAES-II-SPTI-DRVD-CHR TO PMCI-SPCH-THRPY-FROM-IND    ELXPMCBP
00656      ELSE                                                         ELXPMCBP
00657         SET SPT-NO  TO TRUE.                                      ELXPMCBP
00658      IF NAES-II-XRYI-YES                                          ELXPMCBP
00659         SET XRAY-YES TO TRUE                                      ELXPMCBP
00660         MOVE NAES-II-XRYI-DRVD-CHR TO PMCI-XRAY-FROM-IND          ELXPMCBP
00661      ELSE                                                         ELXPMCBP
00662         SET XRAY-NO  TO TRUE.                                     ELXPMCBP
00663 * ADDED THE FOLLOWING:   RGO 2/16/95                              ELXPMCBP
00664      IF NAES-II-PRSI-YES                                          ELXPMCBP
00665         MOVE 'Y' TO PMCI-PROSTHETICS                              ELXPMCBP
00666      ELSE MOVE 'N' TO PMCI-PROSTHETICS.                           ELXPMCBP
00667                                                                   ELXPMCBP
00668      IF NAES-II-MSPI-YES                                          ELXPMCBP
00669         MOVE 'Y' TO PMCI-MED-SUPPLY                               ELXPMCBP
00670      ELSE MOVE 'N' TO PMCI-MED-SUPPLY.                            ELXPMCBP
00671                                                                   ELXPMCBP
00672 ************************************************************      ELXPMCBP
00673 *                                                          *      ELXPMCBP
00674 *    DETERMINE INSTITUTIONAL INPATIENT PVT RM RATE RESULT  *      ELXPMCBP
00675 *                                                          *      ELXPMCBP
00676 ************************************************************      ELXPMCBP
00677                                                                   ELXPMCBP
00678  2110-DTRMN-II-PVT-RM-CNT-TYP.                                    ELXPMCBP
00679                                                                   ELXPMCBP
00680      PERFORM 2115-DTRMN-PVTA.                                     ELXPMCBP
00681      PERFORM 2116-DTRMN-PVTR.                                     ELXPMCBP
00682      PERFORM 2120-DTRMN-II-PVT-RM-RT-RSLT.                        ELXPMCBP
00683                                                                   ELXPMCBP
00684 ************************************************************      ELXPMCBP
00685 *                                                          *      ELXPMCBP
00686 *    DETERMINE INSTITUTIONAL INPATIENT PVT RM RATE RESULT  *      ELXPMCBP
00687 *                                                          *      ELXPMCBP
00688 ************************************************************      ELXPMCBP
00689                                                                   ELXPMCBP
00690  2115-DTRMN-PVTA.                                                 ELXPMCBP
00691                                                                   ELXPMCBP
00692      IF WS-PVTA-PRIC-METH-BSC NOT EQUAL ZERO AND                  ELXPMCBP
00693                      WS-PVTA-PRIC-METH-MM NOT EQUAL ZERO          ELXPMCBP
00694         MOVE WS-PVTA-PRIC-METH-BSC TO WS-PVTA-PRIC-METH           ELXPMCBP
00695         MOVE '+' TO WS-PVTA-LOB-IND                               ELXPMCBP
00696      ELSE                                                         ELXPMCBP
00697      IF WS-PVTA-PRIC-METH-MM NOT EQUAL ZERO                       ELXPMCBP
00698         MOVE WS-PVTA-PRIC-METH-MM TO WS-PVTA-PRIC-METH            ELXPMCBP
00699         MOVE '*' TO WS-PVTA-LOB-IND                               ELXPMCBP
00700      ELSE                                                         ELXPMCBP
00701         MOVE WS-PVTA-PRIC-METH-BSC TO WS-PVTA-PRIC-METH           ELXPMCBP
00702         MOVE SPACES TO WS-PVTA-LOB-IND                            ELXPMCBP
00703      END-IF.                                                      ELXPMCBP
00704 ************************************************************      ELXPMCBP
00705 *                                                          *      ELXPMCBP
00706 *    DETERMINE INSTITUTIONAL INPATIENT PVT RM RATE RESULT  *      ELXPMCBP
00707 *                                                          *      ELXPMCBP
00708 ************************************************************      ELXPMCBP
00709                                                                   ELXPMCBP
00710  2116-DTRMN-PVTR.                                                 ELXPMCBP
00711                                                                   ELXPMCBP
00712      IF WS-PVTR-PRIC-METH-BSC NOT EQUAL ZERO AND                  ELXPMCBP
00713                      WS-PVTR-PRIC-METH-MM NOT EQUAL ZERO          ELXPMCBP
00714         MOVE WS-PVTR-PRIC-METH-BSC TO WS-PVTR-PRIC-METH           ELXPMCBP
00715         MOVE '+' TO WS-PVTR-LOB-IND                               ELXPMCBP
00716      ELSE                                                         ELXPMCBP
00717      IF WS-PVTR-PRIC-METH-BSC = ZERO                              ELXPMCBP
00718         MOVE WS-PVTR-PRIC-METH-MM TO WS-PVTR-PRIC-METH            ELXPMCBP
00719         MOVE '*' TO WS-PVTR-LOB-IND                               ELXPMCBP
00720      ELSE                                                         ELXPMCBP
00721         MOVE WS-PVTR-PRIC-METH-BSC TO WS-PVTR-PRIC-METH           ELXPMCBP
00722         MOVE SPACES TO WS-PVTR-LOB-IND                            ELXPMCBP
00723      END-IF.                                                      ELXPMCBP
00724                                                                   ELXPMCBP
00725                                                                   ELXPMCBP
00726 ************************************************************      ELXPMCBP
00727 *                                                          *      ELXPMCBP
00728 *    DETERMINE INSTITUTIONAL INPATIENT PVT RM RATE RESULT  *      ELXPMCBP
00729 *                                                          *      ELXPMCBP
00730 ************************************************************      ELXPMCBP
00731                                                                   ELXPMCBP
00732  2120-DTRMN-II-PVT-RM-RT-RSLT.                                    ELXPMCBP
00733      EVALUATE TRUE                                                ELXPMCBP
00734      WHEN PMCI-FAC-ROOM-ALL-PRIVATE                               ELXPMCBP
00735         EVALUATE TRUE                                             ELXPMCBP
00736         WHEN NAES-II-PVTA-YES                                     ELXPMCBP
00737            MOVE WS-PVTA-PRIC-METH TO WS-PPM-GROUPINGS             ELXPMCBP
00738            MOVE WS-PVTA-LOB-IND TO PMCI-PRVT-RM-RATE-IND          ELXPMCBP
00739            PERFORM 2130-SLCT-PVT-RM-RT-PPM                        ELXPMCBP
00740         WHEN NAES-II-PVTR-YES                                     ELXPMCBP
00741            MOVE WS-PVTR-PRIC-METH TO WS-PPM-GROUPINGS             ELXPMCBP
00742            MOVE WS-PVTR-LOB-IND TO PMCI-PRVT-RM-RATE-IND          ELXPMCBP
00743            PERFORM 2130-SLCT-PVT-RM-RT-PPM                        ELXPMCBP
00744         WHEN OTHER                                                ELXPMCBP
00745            SET PRRT-NOT-APPLICABLE TO TRUE                        ELXPMCBP
00746         END-EVALUATE                                              ELXPMCBP
00747      WHEN PMCI-FAC-ROOM-NOT-ALL-PRIVATE                           ELXPMCBP
00748         EVALUATE TRUE                                             ELXPMCBP
00749         WHEN NAES-II-PVTR-YES                                     ELXPMCBP
00750            MOVE WS-PVTR-PRIC-METH TO WS-PPM-GROUPINGS             ELXPMCBP
00751            MOVE WS-PVTR-LOB-IND TO PMCI-PRVT-RM-RATE-IND          ELXPMCBP
00752            PERFORM 2130-SLCT-PVT-RM-RT-PPM                        ELXPMCBP
00753         WHEN OTHER                                                ELXPMCBP
00754            SET PRRT-NOT-APPLICABLE TO TRUE                        ELXPMCBP
00755         END-EVALUATE                                              ELXPMCBP
00756      WHEN OTHER                                                   ELXPMCBP
00757         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
00758         MOVE +3009 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
00759      END-EVALUATE.                                                ELXPMCBP
00760                                                                   ELXPMCBP
00761 ************************************************************      ELXPMCBP
00762 *                                                          *      ELXPMCBP
00763 *    DETERMINE INSTITUTIONAL INPATIENT PVT RM RATE RESULT  *      ELXPMCBP
00764 *                                                          *      ELXPMCBP
00765 ************************************************************      ELXPMCBP
00766                                                                   ELXPMCBP
00767  2130-SLCT-PVT-RM-RT-PPM.                                         ELXPMCBP
00768      EVALUATE TRUE                                                ELXPMCBP
00769          WHEN WS-MCSP                                             ELXPMCBP
00770             SET PRRT-MCSP TO TRUE                                 ELXPMCBP
00771          WHEN WS-AV-SEMI-PRIV-ROOM                                ELXPMCBP
00772             SET PRRT-AVG-SEMI-PRIV TO TRUE                        ELXPMCBP
00773          WHEN WS-BILLED-AMOUNT                                    ELXPMCBP
00774             SET PRRT-BILLED-AMT TO TRUE                           ELXPMCBP
00775          WHEN WS-PERCENT-BILLED-AMOUNT                            ELXPMCBP
00776             SET PRRT-PERCENT-BILLED-AMT TO TRUE                   ELXPMCBP
00777          WHEN WS-MCSP-PLUS-ADDN                                   ELXPMCBP
00778             SET PRRT-MCSP-ADDL-AMT TO TRUE                        ELXPMCBP
00779          WHEN WS-PERCENT-MCSP                                     ELXPMCBP
00780             SET PRRT-PERCENT-MCSP TO TRUE                         ELXPMCBP
00781          WHEN WS-AV-SEMI-PRIV-ROOM-PLUS                           ELXPMCBP
00782             SET PRRT-AVG-SEMI-PRIV-PLUS TO TRUE                   ELXPMCBP
00783          WHEN WS-PERCENT-AV-SEMI-PRIV-ROOM                        ELXPMCBP
00784             SET PRRT-PERCENT-AVG-SEMI-PRIV TO TRUE                ELXPMCBP
00785          WHEN WS-FLAT-RATE                                        ELXPMCBP
00786             SET PRRT-FLAT-RATE TO TRUE                            ELXPMCBP
00787          WHEN WS-FLAT-RATE-PLUS-ADDN                              ELXPMCBP
00788             SET PRRT-FLAT-RATE-PLUS-ADDL TO TRUE                  ELXPMCBP
00789          WHEN OTHER                                               ELXPMCBP
00790             SET PRRT-CALL TO TRUE                                 ELXPMCBP
00791      END-EVALUATE.                                                ELXPMCBP
00792                                                                   ELXPMCBP
00793 ************************************************************      ELXPMCBP
00794 *                                                          *      ELXPMCBP
00795 *    DETERMINE INSTITUTIONAL INPATIENT OB RESULTS          *      ELXPMCBP
00796 *                                                          *      ELXPMCBP
00797 ************************************************************      ELXPMCBP
00798                                                                   ELXPMCBP
00799  2200-DTRMN-II-OB-RSLTS.                                          ELXPMCBP
00800      EVALUATE TRUE                                                ELXPMCBP
00801      WHEN PMCI-MEMBER                                             ELXPMCBP
00802                                                                   ELXPMCBP
00803                                                                   ELXPMCBP
00804         IF NAES-II-OBCM-YES                                       ELXPMCBP
00805            SET OB-COMP-YES TO TRUE                                ELXPMCBP
00806            MOVE NAES-II-OBCM-DRVD-CHR TO                          ELXPMCBP
00807                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
00808         ELSE                                                      ELXPMCBP
00809            SET OB-COMP-NO TO TRUE                                 ELXPMCBP
00810         END-IF                                                    ELXPMCBP
00811         IF NAES-II-OBNM-YES                                       ELXPMCBP
00812            SET OB-NORM-YES TO TRUE                                ELXPMCBP
00813            MOVE NAES-II-OBNM-DRVD-CHR TO                          ELXPMCBP
00814                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
00815         ELSE                                                      ELXPMCBP
00816            SET OB-NORM-NO TO TRUE                                 ELXPMCBP
00817         END-IF                                                    ELXPMCBP
00818                                                                   ELXPMCBP
00819      WHEN PMCI-SPOUSE                                             ELXPMCBP
00820                                                                   ELXPMCBP
00821         IF NAES-II-OBCS-YES                                       ELXPMCBP
00822            SET OB-COMP-YES TO TRUE                                ELXPMCBP
00823            MOVE NAES-II-OBCS-DRVD-CHR TO                          ELXPMCBP
00824                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
00825         ELSE                                                      ELXPMCBP
00826            SET OB-COMP-NO TO TRUE                                 ELXPMCBP
00827         END-IF                                                    ELXPMCBP
00828         IF NAES-II-OBNS-YES                                       ELXPMCBP
00829            SET OB-NORM-YES TO TRUE                                ELXPMCBP
00830            MOVE NAES-II-OBNS-DRVD-CHR TO                          ELXPMCBP
00831                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
00832         ELSE                                                      ELXPMCBP
00833            SET OB-NORM-NO TO TRUE                                 ELXPMCBP
00834         END-IF                                                    ELXPMCBP
00835                                                                   ELXPMCBP
00836      WHEN PMCI-DEPENDENT                                          ELXPMCBP
00837                                                                   ELXPMCBP
00838         IF NAES-II-OBCD-YES                                       ELXPMCBP
00839            SET OB-COMP-YES TO TRUE                                ELXPMCBP
00840            MOVE NAES-II-OBCD-DRVD-CHR TO                          ELXPMCBP
00841                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
00842         ELSE                                                      ELXPMCBP
00843            SET OB-COMP-NO TO TRUE                                 ELXPMCBP
00844         END-IF                                                    ELXPMCBP
00845         IF NAES-II-OBND-YES                                       ELXPMCBP
00846            SET OB-NORM-YES TO TRUE                                ELXPMCBP
00847            MOVE NAES-II-OBND-DRVD-CHR TO                          ELXPMCBP
00848                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
00849         ELSE                                                      ELXPMCBP
00850            SET OB-NORM-NO TO TRUE                                 ELXPMCBP
00851         END-IF                                                    ELXPMCBP
00852      WHEN OTHER                                                   ELXPMCBP
00853         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
00854         MOVE +3010 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
00855      END-EVALUATE.                                                ELXPMCBP
00856                                                                   ELXPMCBP
00857 ************************************************************      ELXPMCBP
00858 *                                                          *      ELXPMCBP
00859 *    GET INSTITUTIONAL OUTPATIENT BENEFIT RESULTS          *      ELXPMCBP
00860 *                                                          *      ELXPMCBP
00861 ************************************************************      ELXPMCBP
00862                                                                   ELXPMCBP
00863 *3000-GET-IO-BENEFIT-PROV.                                        ELXPMCBP
00864 ************************************************************      ELXPMCBP
00865 *                                                          *      ELXPMCBP
00866 *    DETERMINE INSTITUTIONAL OUTPATIENT BENEFIT RESULTS    *      ELXPMCBP
00867 *                                                          *      ELXPMCBP
00868 ************************************************************      ELXPMCBP
00869                                                                   ELXPMCBP
00870  3100-DTRMN-IO-RSLTS.                                             ELXPMCBP
00871                                                                   ELXPMCBP
00872      IF NAES-IO-AMB-YES                                           ELXPMCBP
00873         SET AMBULANCE-YES TO TRUE                                 ELXPMCBP
00874         MOVE NAES-IO-AMB-DRVD-CHR  TO PMCI-AMB-FROM-IND           ELXPMCBP
00875      ELSE                                                         ELXPMCBP
00876         SET AMBULANCE-NO TO TRUE.                                 ELXPMCBP
00877      IF NAES-IO-ARPO-YES                                          ELXPMCBP
00878         SET SUB-ABUSE-ALC-YES TO TRUE                             ELXPMCBP
00879         MOVE NAES-IO-ARPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-ALC-IND    ELXPMCBP
00880      ELSE                                                         ELXPMCBP
00881         SET SUB-ABUSE-ALC-NO TO TRUE.                             ELXPMCBP
00882      IF NAES-IO-CHC-YES                                           ELXPMCBP
00883         MOVE NAES-IO-CHC-DRVD-CHR TO PMCI-CHC-FROM-IND            ELXPMCBP
00884      ELSE                                                         ELXPMCBP
00885         SET CHC-NO-COVERAGE TO TRUE.                              ELXPMCBP
00886      IF NAES-IO-DRPO-YES                                          ELXPMCBP
00887         SET SUB-ABUSE-DRG-YES TO TRUE                             ELXPMCBP
00888         MOVE NAES-IO-DRPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-DRG-IND    ELXPMCBP
00889      ELSE                                                         ELXPMCBP
00890         SET SUB-ABUSE-DRG-NO TO TRUE.                             ELXPMCBP
00891      IF NAES-IO-PSYO-YES                                          ELXPMCBP
00892         SET PSY-YES TO TRUE                                       ELXPMCBP
00893         MOVE NAES-IO-PSYO-DRVD-CHR TO PMCI-PSYCH-FROM-IND         ELXPMCBP
00894      ELSE                                                         ELXPMCBP
00895         SET PSY-NO TO TRUE.                                       ELXPMCBP
00896      IF NAES-IO-DMRO-YES                                          ELXPMCBP
00897         MOVE NAES-IO-DMRO-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
00898         SET SW-DMR-YES TO TRUE.                                   ELXPMCBP
00899      IF NAES-IO-DMEO-YES                                          ELXPMCBP
00900         MOVE NAES-IO-DMEO-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
00901         SET SW-DME-YES TO TRUE.                                   ELXPMCBP
00902      IF SW-DME-YES OR SW-DMR-YES                                  ELXPMCBP
00903         PERFORM 6100-DTRMN-DME-DMR-RSLT                           ELXPMCBP
00904      ELSE                                                         ELXPMCBP
00905         SET DME-NO TO TRUE                                        ELXPMCBP
00906         SET DME-MD-CERT-NO TO TRUE.                               ELXPMCBP
00907      PERFORM 3120-DTRMN-IO-EMRGNCY-RSLTS.                         ELXPMCBP
00908      IF NAES-IO-LABO-YES                                          ELXPMCBP
00909         SET PMCI-LAB-YES TO TRUE                                  ELXPMCBP
00910         MOVE NAES-IO-LABO-DRVD-CHR TO PMCI-LAB-FROM-IND           ELXPMCBP
00911      ELSE                                                         ELXPMCBP
00912         SET PMCI-LAB-NO TO TRUE.                                  ELXPMCBP
00913      IF PMCI-PAT-SEX-FEMALE                                       ELXPMCBP
00914         PERFORM 3200-DTRMN-IO-OB-RSLTS                            ELXPMCBP
00915      ELSE                                                         ELXPMCBP
00916         SET OB-NORM-NO                                            ELXPMCBP
00917             OB-COMP-NO TO TRUE.                                   ELXPMCBP
00918      IF NAES-IO-FOTO-YES                                          ELXPMCBP
00919         SET PMCI-OCC-THRPY-COVERED TO TRUE                        ELXPMCBP
00920         MOVE NAES-IO-FOTO-DRVD-CHR TO PMCI-OCCPTNL-THRPY-FROM-IND ELXPMCBP
00921      ELSE                                                         ELXPMCBP
00922         SET PMCI-OCC-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
00923      IF NAES-IO-PMTO-YES                                          ELXPMCBP
00924         SET PMCI-PHY-THRPY-COVERED TO TRUE                        ELXPMCBP
00925         MOVE NAES-IO-PMTO-DRVD-CHR TO PMCI-PHYSCL-THRPY-FROM-IND  ELXPMCBP
00926      ELSE                                                         ELXPMCBP
00927         SET PMCI-PHY-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
00928      IF NAES-IO-NRSO-YES                                          ELXPMCBP
00929         SET PMCI-PDN-COVERED TO TRUE                              ELXPMCBP
00930         MOVE NAES-IO-NRSO-DRVD-CHR TO PMCI-PRVTE-DTY-NRS-FROM-IND ELXPMCBP
00931      ELSE                                                         ELXPMCBP
00932         SET PMCI-PDN-NOT-COVERED TO TRUE.                         ELXPMCBP
00933      IF NAES-IO-SPTO-YES                                          ELXPMCBP
00934         SET SPT-YES TO TRUE                                       ELXPMCBP
00935         MOVE NAES-IO-SPTO-DRVD-CHR TO PMCI-SPCH-THRPY-FROM-IND    ELXPMCBP
00936      ELSE                                                         ELXPMCBP
00937         SET SPT-NO  TO TRUE.                                      ELXPMCBP
00938      IF NAES-IO-XRYO-YES                                          ELXPMCBP
00939         SET XRAY-YES TO TRUE                                      ELXPMCBP
00940         MOVE NAES-IO-XRYO-DRVD-CHR TO PMCI-XRAY-FROM-IND          ELXPMCBP
00941      ELSE                                                         ELXPMCBP
00942         SET XRAY-NO  TO TRUE.                                     ELXPMCBP
00943                                                                   ELXPMCBP
00944 * ADDED THESE. RGO.                                               ELXPMCBP
00945      IF NAES-IO-PRSO-YES                                          ELXPMCBP
00946         MOVE 'Y' TO PMCI-PROSTHETICS                              ELXPMCBP
00947      ELSE                                                         ELXPMCBP
00948         MOVE 'Y' TO PMCI-PROSTHETICS.                             ELXPMCBP
00949                                                                   ELXPMCBP
00950      IF NAES-IO-MSPO-YES                                          ELXPMCBP
00951         MOVE 'Y' TO PMCI-MED-SUPPLY                               ELXPMCBP
00952      ELSE MOVE 'N' TO PMCI-MED-SUPPLY.                            ELXPMCBP
00953                                                                   ELXPMCBP
00954 ******************************************************************ELXPMCBP
00955 *                                                                *ELXPMCBP
00956 * DETERMINE INSTITUTIONAL OUTPATIENT EMERGENCY SERVICES RESULTS  *ELXPMCBP
00957 *                                                                *ELXPMCBP
00958 ******************************************************************ELXPMCBP
00959                                                                   ELXPMCBP
00960  3120-DTRMN-IO-EMRGNCY-RSLTS.                                     ELXPMCBP
00961      IF NAES-IO-EAER-YES                                          ELXPMCBP
00962         PERFORM 6210-DTRMN-EAC-RSLT                               ELXPMCBP
00963         IF NAES-IO-EMER-YES                                       ELXPMCBP
00964            PERFORM 6220-DTRMN-EMC-RSLT                            ELXPMCBP
00965            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
00966         ELSE                                                      ELXPMCBP
00967            SET EMC-NO-COVERAGE TO TRUE                            ELXPMCBP
00968            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
00969         END-IF                                                    ELXPMCBP
00970      ELSE                                                         ELXPMCBP
00971         SET EAC-NO-COVERAGE TO TRUE                               ELXPMCBP
00972         IF NAES-IO-EMER-YES                                       ELXPMCBP
00973            PERFORM 6220-DTRMN-EMC-RSLT                            ELXPMCBP
00974            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
00975         ELSE                                                      ELXPMCBP
00976            SET EMC-NO-COVERAGE TO TRUE                            ELXPMCBP
00977            SET ELT-NOT-APPLICABLE TO TRUE                         ELXPMCBP
00978         END-IF                                                    ELXPMCBP
00979      END-IF.                                                      ELXPMCBP
00980                                                                   ELXPMCBP
00981 ************************************************************      ELXPMCBP
00982 *                                                          *      ELXPMCBP
00983 *    DETERMINE INSTITUTIONAL OUTPATIENT OB RESULTS         *      ELXPMCBP
00984 *                                                          *      ELXPMCBP
00985 ************************************************************      ELXPMCBP
00986                                                                   ELXPMCBP
00987  3200-DTRMN-IO-OB-RSLTS.                                          ELXPMCBP
00988                                                                   ELXPMCBP
00989      EVALUATE TRUE                                                ELXPMCBP
00990      WHEN PMCI-MEMBER                                             ELXPMCBP
00991                                                                   ELXPMCBP
00992                                                                   ELXPMCBP
00993         IF NAES-IO-OBCM-YES                                       ELXPMCBP
00994            SET OB-COMP-YES TO TRUE                                ELXPMCBP
00995            MOVE NAES-IO-OBCM-DRVD-CHR TO                          ELXPMCBP
00996                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
00997         ELSE                                                      ELXPMCBP
00998            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
00999         END-IF                                                    ELXPMCBP
01000         IF NAES-IO-OBNM-YES                                       ELXPMCBP
01001            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01002            MOVE NAES-IO-OBNM-DRVD-CHR TO                          ELXPMCBP
01003                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
01004         ELSE                                                      ELXPMCBP
01005            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01006         END-IF                                                    ELXPMCBP
01007      WHEN PMCI-SPOUSE                                             ELXPMCBP
01008                                                                   ELXPMCBP
01009                                                                   ELXPMCBP
01010         IF NAES-IO-OBCS-YES                                       ELXPMCBP
01011            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01012            MOVE NAES-IO-OBCS-DRVD-CHR TO                          ELXPMCBP
01013                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
01014         ELSE                                                      ELXPMCBP
01015            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01016         END-IF                                                    ELXPMCBP
01017         IF NAES-IO-OBNS-YES                                       ELXPMCBP
01018            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01019            MOVE NAES-IO-OBNS-DRVD-CHR TO                          ELXPMCBP
01020                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
01021         ELSE                                                      ELXPMCBP
01022            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01023         END-IF                                                    ELXPMCBP
01024      WHEN PMCI-DEPENDENT                                          ELXPMCBP
01025                                                                   ELXPMCBP
01026         IF NAES-IO-OBCD-YES                                       ELXPMCBP
01027            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01028            MOVE NAES-IO-OBCD-DRVD-CHR TO                          ELXPMCBP
01029                         PMCI-OB-CMPLCTD-FROM-IND                  ELXPMCBP
01030         ELSE                                                      ELXPMCBP
01031            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01032         END-IF                                                    ELXPMCBP
01033         IF NAES-IO-OBND-YES                                       ELXPMCBP
01034            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01035            MOVE NAES-IO-OBND-DRVD-CHR TO                          ELXPMCBP
01036                         PMCI-OB-NORM-FROM-IND                     ELXPMCBP
01037         ELSE                                                      ELXPMCBP
01038            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01039         END-IF                                                    ELXPMCBP
01040      WHEN OTHER                                                   ELXPMCBP
01041         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
01042         MOVE +3010 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01043      END-EVALUATE.                                                ELXPMCBP
01044                                                                   ELXPMCBP
01045 ************************************************************      ELXPMCBP
01046 *                                                          *      ELXPMCBP
01047 *    GET PROFESSIONAL INPATIENT BENEFIT RESULTS            *      ELXPMCBP
01048 *                                                          *      ELXPMCBP
01049 ************************************************************      ELXPMCBP
01050                                                                   ELXPMCBP
01051 *4000-GET-PI-BENEFIT-PROV.                                        ELXPMCBP
01052 ************************************************************      ELXPMCBP
01053 *                                                          *      ELXPMCBP
01054 *    DETERMINE PROFESSIONAL INPATIENT BENEFIT RESULTS      *      ELXPMCBP
01055 *                                                          *      ELXPMCBP
01056 ************************************************************      ELXPMCBP
01057                                                                   ELXPMCBP
01058  4100-DTRMN-PI-RSLTS.                                             ELXPMCBP
01059                                                                   ELXPMCBP
01060                                                                   ELXPMCBP
01061      IF NAES-PI-AMB-YES                                           ELXPMCBP
01062         SET AMBULANCE-YES TO TRUE                                 ELXPMCBP
01063         MOVE NAES-PI-AMB-DRVD-CHR  TO PMCI-AMB-FROM-IND           ELXPMCBP
01064      ELSE                                                         ELXPMCBP
01065         SET AMBULANCE-NO  TO TRUE.                                ELXPMCBP
01066      IF NAES-PI-AHI-YES                                           ELXPMCBP
01067         SET SUB-ABUSE-ALC-YES TO TRUE                             ELXPMCBP
01068         MOVE NAES-PI-AHI-DRVD-CHR TO PMCI-SBSTNCE-ABS-ALC-IND     ELXPMCBP
01069      ELSE                                                         ELXPMCBP
01070         SET SUB-ABUSE-ALC-NO  TO TRUE.                            ELXPMCBP
01071      IF NAES-PI-CHCV-YES                                          ELXPMCBP
01072         MOVE NAES-PI-CHCV-DRVD-CHR TO PMCI-CHC-FROM-IND           ELXPMCBP
01073      ELSE                                                         ELXPMCBP
01074         SET CHC-NO-COVERAGE TO TRUE.                              ELXPMCBP
01075      IF NAES-PI-DRI-YES                                           ELXPMCBP
01076         SET SUB-ABUSE-DRG-YES TO TRUE                             ELXPMCBP
01077         MOVE NAES-PI-DRI-DRVD-CHR TO PMCI-SBSTNCE-ABS-DRG-IND     ELXPMCBP
01078      ELSE                                                         ELXPMCBP
01079         SET SUB-ABUSE-DRG-NO  TO TRUE.                            ELXPMCBP
01080      IF NAES-PI-MNI-YES                                           ELXPMCBP
01081         SET PSY-YES TO TRUE                                       ELXPMCBP
01082         MOVE NAES-PI-MNI-DRVD-CHR TO PMCI-PSYCH-FROM-IND          ELXPMCBP
01083      ELSE                                                         ELXPMCBP
01084         SET PSY-NO  TO TRUE.                                      ELXPMCBP
01085      IF NAES-PI-DMRI-YES                                          ELXPMCBP
01086         MOVE NAES-PI-DMRI-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
01087         SET SW-DMR-YES TO TRUE.                                   ELXPMCBP
01088      IF NAES-PI-DMEI-YES                                          ELXPMCBP
01089         MOVE NAES-PI-DMEI-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
01090         SET SW-DME-YES TO TRUE.                                   ELXPMCBP
01091      IF SW-DME-YES OR SW-DMR-YES                                  ELXPMCBP
01092         PERFORM 6100-DTRMN-DME-DMR-RSLT                           ELXPMCBP
01093      ELSE                                                         ELXPMCBP
01094         SET DME-NO TO TRUE                                        ELXPMCBP
01095         SET DME-MD-CERT-NO TO TRUE.                               ELXPMCBP
01096      IF NAES-PI-LABI-YES                                          ELXPMCBP
01097         SET PMCI-LAB-YES TO TRUE                                  ELXPMCBP
01098         MOVE NAES-PI-LABI-DRVD-CHR TO PMCI-LAB-FROM-IND           ELXPMCBP
01099      ELSE                                                         ELXPMCBP
01100         SET PMCI-LAB-NO  TO TRUE.                                 ELXPMCBP
01101      IF NAES-PI-ASOP-YES                                          ELXPMCBP
01102         SET PMCI-ASOP-YES TO TRUE                                 ELXPMCBP
01103         MOVE NAES-PI-ASOP-DRVD-CHR TO PMCI-ASOP-FROM-IND          ELXPMCBP
01104      ELSE                                                         ELXPMCBP
01105         SET PMCI-ASOP-NO  TO TRUE.                                ELXPMCBP
01106      IF PMCI-PAT-SEX-FEMALE                                       ELXPMCBP
01107         PERFORM 4200-DTRMN-PI-OB-RSLTS                            ELXPMCBP
01108      ELSE                                                         ELXPMCBP
01109         SET OB-NORM-NO                                            ELXPMCBP
01110             OB-COMP-NO TO TRUE.                                   ELXPMCBP
01111      IF NAES-PI-FOTI-YES                                          ELXPMCBP
01112         SET PMCI-OCC-THRPY-COVERED TO TRUE                        ELXPMCBP
01113        MOVE NAES-PI-FOTI-DRVD-CHR TO PMCI-OCCPTNL-THRPY-FROM-IND  ELXPMCBP
01114      ELSE                                                         ELXPMCBP
01115         SET PMCI-OCC-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
01116      IF NAES-PI-PMTI-YES                                          ELXPMCBP
01117         SET PMCI-PHY-THRPY-COVERED TO TRUE                        ELXPMCBP
01118         MOVE NAES-PI-PMTI-DRVD-CHR TO PMCI-PHYSCL-THRPY-FROM-IND  ELXPMCBP
01119      ELSE                                                         ELXPMCBP
01120         SET PMCI-PHY-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
01121      IF NAES-PI-NRSI-YES                                          ELXPMCBP
01122         SET PMCI-PDN-COVERED TO TRUE                              ELXPMCBP
01123        MOVE NAES-PI-NRSI-DRVD-CHR TO PMCI-PRVTE-DTY-NRS-FROM-IND  ELXPMCBP
01124      ELSE                                                         ELXPMCBP
01125         SET PMCI-PDN-NOT-COVERED TO TRUE.                         ELXPMCBP
01126      IF NAES-PI-SPTI-YES                                          ELXPMCBP
01127         SET SPT-YES TO TRUE                                       ELXPMCBP
01128         MOVE NAES-PI-SPTI-DRVD-CHR TO PMCI-SPCH-THRPY-FROM-IND    ELXPMCBP
01129      ELSE                                                         ELXPMCBP
01130         SET SPT-NO  TO TRUE.                                      ELXPMCBP
01131      IF NAES-PI-XRYI-YES                                          ELXPMCBP
01132         SET XRAY-YES TO TRUE                                      ELXPMCBP
01133         MOVE NAES-PI-XRYI-DRVD-CHR TO PMCI-XRAY-FROM-IND          ELXPMCBP
01134      ELSE                                                         ELXPMCBP
01135         SET XRAY-NO  TO TRUE.                                     ELXPMCBP
01136                                                                   ELXPMCBP
01137      IF NAES-PI-PRSI-YES                                          ELXPMCBP
01138         MOVE 'Y' TO PMCI-PROSTHETICS                              ELXPMCBP
01139      ELSE                                                         ELXPMCBP
01140         MOVE 'N' TO PMCI-PROSTHETICS.                             ELXPMCBP
01141                                                                   ELXPMCBP
01142      IF NAES-PI-MSPI-YES                                          ELXPMCBP
01143         MOVE 'Y' TO PMCI-MED-SUPPLY                               ELXPMCBP
01144      ELSE MOVE 'N' TO PMCI-MED-SUPPLY.                            ELXPMCBP
01145                                                                   ELXPMCBP
01146 ************************************************************      ELXPMCBP
01147 *                                                          *      ELXPMCBP
01148 *    DETERMINE PROFESSIONAL INPATIENT OB RESULTS           *      ELXPMCBP
01149 *                                                          *      ELXPMCBP
01150 ************************************************************      ELXPMCBP
01151                                                                   ELXPMCBP
01152  4200-DTRMN-PI-OB-RSLTS.                                          ELXPMCBP
01153      EVALUATE TRUE                                                ELXPMCBP
01154      WHEN PMCI-MEMBER                                             ELXPMCBP
01155                                                                   ELXPMCBP
01156         IF NAES-PI-OBCM-YES                                       ELXPMCBP
01157            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01158            MOVE NAES-PI-OBCM-DRVD-CHR TO                          ELXPMCBP
01159                           PMCI-OB-CMPLCTD-FROM-IND                ELXPMCBP
01160         ELSE                                                      ELXPMCBP
01161            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01162         END-IF                                                    ELXPMCBP
01163         IF NAES-PI-OBNM-YES                                       ELXPMCBP
01164            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01165            MOVE NAES-PI-OBNM-DRVD-CHR TO                          ELXPMCBP
01166                           PMCI-OB-NORM-FROM-IND                   ELXPMCBP
01167         ELSE                                                      ELXPMCBP
01168            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01169         END-IF                                                    ELXPMCBP
01170      WHEN PMCI-SPOUSE                                             ELXPMCBP
01171                                                                   ELXPMCBP
01172         IF NAES-PI-OBCS-YES                                       ELXPMCBP
01173            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01174            MOVE NAES-PI-OBCS-DRVD-CHR TO                          ELXPMCBP
01175                           PMCI-OB-CMPLCTD-FROM-IND                ELXPMCBP
01176         ELSE                                                      ELXPMCBP
01177            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01178         END-IF                                                    ELXPMCBP
01179         IF NAES-PI-OBNS-YES                                       ELXPMCBP
01180            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01181            MOVE NAES-PI-OBNS-DRVD-CHR TO                          ELXPMCBP
01182                           PMCI-OB-NORM-FROM-IND                   ELXPMCBP
01183         ELSE                                                      ELXPMCBP
01184            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01185         END-IF                                                    ELXPMCBP
01186      WHEN PMCI-DEPENDENT                                          ELXPMCBP
01187                                                                   ELXPMCBP
01188         IF NAES-PI-OBCD-YES                                       ELXPMCBP
01189            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01190            MOVE NAES-PI-OBCD-DRVD-CHR TO                          ELXPMCBP
01191                           PMCI-OB-CMPLCTD-FROM-IND                ELXPMCBP
01192            MOVE NAES-PI-OBND-DRVD-CHR TO                          ELXPMCBP
01193                           PMCI-OB-NORM-FROM-IND                   ELXPMCBP
01194         ELSE                                                      ELXPMCBP
01195            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01196         END-IF                                                    ELXPMCBP
01197         IF NAES-PI-OBND-YES                                       ELXPMCBP
01198            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01199         ELSE                                                      ELXPMCBP
01200            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01201         END-IF                                                    ELXPMCBP
01202      WHEN OTHER                                                   ELXPMCBP
01203         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
01204         MOVE +3010 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01205      END-EVALUATE.                                                ELXPMCBP
01206                                                                   ELXPMCBP
01207 ************************************************************      ELXPMCBP
01208 *                                                          *      ELXPMCBP
01209 *    GET PROFESSIONAL OUTPATIENT BENEFIT RESULTS           *      ELXPMCBP
01210 *                                                          *      ELXPMCBP
01211 ************************************************************      ELXPMCBP
01212                                                                   ELXPMCBP
01213 *5000-GET-PO-BENEFIT-PROV.                                        ELXPMCBP
01214                                                                   ELXPMCBP
01215 ************************************************************      ELXPMCBP
01216 *                                                          *      ELXPMCBP
01217 *    DETERMINE PROFESSIONAL OUTPATIENT BENEFIT RESULTS     *      ELXPMCBP
01218 *                                                          *      ELXPMCBP
01219 ************************************************************      ELXPMCBP
01220                                                                   ELXPMCBP
01221  5100-DTRMN-PO-RSLTS.                                             ELXPMCBP
01222                                                                   ELXPMCBP
01223      IF NAES-PO-AMB-YES                                           ELXPMCBP
01224         SET AMBULANCE-YES TO TRUE                                 ELXPMCBP
01225         MOVE NAES-PO-AMB-DRVD-CHR  TO PMCI-AMB-FROM-IND           ELXPMCBP
01226      ELSE                                                         ELXPMCBP
01227         SET AMBULANCE-NO  TO TRUE.                                ELXPMCBP
01228      IF NAES-PO-CHCV-YES                                          ELXPMCBP
01229         MOVE NAES-PO-CHCV-DRVD-CHR TO PMCI-CHC-FROM-IND           ELXPMCBP
01230      ELSE                                                         ELXPMCBP
01231         SET CHC-NO-COVERAGE TO TRUE.                              ELXPMCBP
01232      IF NAES-PO-GPO-YES OR NAES-PO-GPO-YES                        ELXPMCBP
01233         IF SW-CLDR-FOUND                                          ELXPMCBP
01234            PERFORM 5110-SAVE-BSC-MM-PSY                           ELXPMCBP
01235         ELSE                                                      ELXPMCBP
01236            PERFORM 5150-SET-PSY-DRVD                              ELXPMCBP
01237            SET PSY-YES TO TRUE                                    ELXPMCBP
01238            SET SUB-ABUSE-ALC-YES TO TRUE                          ELXPMCBP
01239            SET SUB-ABUSE-DRG-YES TO TRUE                          ELXPMCBP
01240      ELSE                                                         ELXPMCBP
01241         SET PSY-NO TO TRUE                                        ELXPMCBP
01242         SET SUB-ABUSE-ALC-NO TO TRUE                              ELXPMCBP
01243         SET SUB-ABUSE-DRG-NO TO TRUE.                             ELXPMCBP
01244      IF NAES-PO-DMRO-YES                                          ELXPMCBP
01245         MOVE NAES-PO-DMRO-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
01246         SET SW-DMR-YES TO TRUE.                                   ELXPMCBP
01247      IF NAES-PO-DMEO-YES                                          ELXPMCBP
01248         MOVE NAES-PO-DMEO-DRVD-CHR TO WS-DME-LOB-IND              ELXPMCBP
01249         SET SW-DME-YES TO TRUE.                                   ELXPMCBP
01250      IF SW-DME-YES OR SW-DMR-YES                                  ELXPMCBP
01251         PERFORM 6100-DTRMN-DME-DMR-RSLT                           ELXPMCBP
01252      ELSE                                                         ELXPMCBP
01253         SET DME-NO TO TRUE                                        ELXPMCBP
01254         SET DME-MD-CERT-NO TO TRUE.                               ELXPMCBP
01255      PERFORM 6200-DTRMN-PO-EMRGNCY-RSLTS.                         ELXPMCBP
01256      IF NAES-PO-LABO-YES                                          ELXPMCBP
01257         MOVE NAES-PO-LABO-DRVD-CHR TO PMCI-LAB-FROM-IND           ELXPMCBP
01258         SET PMCI-LAB-YES TO TRUE                                  ELXPMCBP
01259      ELSE                                                         ELXPMCBP
01260         SET PMCI-LAB-NO  TO TRUE.                                 ELXPMCBP
01261      IF NAES-PO-ASOP-YES                                          ELXPMCBP
01262         SET PMCI-ASOP-YES TO TRUE                                 ELXPMCBP
01263         MOVE NAES-PO-ASOP-DRVD-CHR TO PMCI-ASOP-FROM-IND          ELXPMCBP
01264      ELSE                                                         ELXPMCBP
01265         SET PMCI-ASOP-NO  TO TRUE.                                ELXPMCBP
01266      IF PMCI-PAT-SEX-FEMALE                                       ELXPMCBP
01267         PERFORM 6000-DTRMN-PO-OB-RSLTS                            ELXPMCBP
01268      ELSE                                                         ELXPMCBP
01269         SET OB-NORM-NO                                            ELXPMCBP
01270             OB-COMP-NO TO TRUE.                                   ELXPMCBP
01271      IF NAES-PO-FOTO-YES                                          ELXPMCBP
01272         SET PMCI-OCC-THRPY-COVERED TO TRUE                        ELXPMCBP
01273        MOVE NAES-PO-FOTO-DRVD-CHR TO PMCI-OCCPTNL-THRPY-FROM-IND  ELXPMCBP
01274      ELSE                                                         ELXPMCBP
01275         SET PMCI-OCC-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
01276      IF NAES-PO-PMTO-YES                                          ELXPMCBP
01277         SET PMCI-PHY-THRPY-COVERED TO TRUE                        ELXPMCBP
01278         MOVE NAES-PO-PMTO-DRVD-CHR TO PMCI-PHYSCL-THRPY-FROM-IND  ELXPMCBP
01279      ELSE                                                         ELXPMCBP
01280         SET PMCI-PHY-THRPY-NOT-COVERED TO TRUE.                   ELXPMCBP
01281      IF NAES-PO-NRSO-YES                                          ELXPMCBP
01282         SET PMCI-PDN-COVERED TO TRUE                              ELXPMCBP
01283        MOVE NAES-PO-NRSO-DRVD-CHR TO PMCI-PRVTE-DTY-NRS-FROM-IND  ELXPMCBP
01284      ELSE                                                         ELXPMCBP
01285         SET PMCI-PDN-NOT-COVERED TO TRUE.                         ELXPMCBP
01286      IF NAES-PO-OVIS-YES                                          ELXPMCBP
01287         SET PMCI-OFF-VISITS-COVERED TO TRUE                       ELXPMCBP
01288         MOVE NAES-PO-OVIS-DRVD-CHR TO PMCI-OFC-VSTS-FROM-IND      ELXPMCBP
01289      ELSE                                                         ELXPMCBP
01290         SET PMCI-OFF-VISITS-NOT-COVERED TO TRUE.                  ELXPMCBP
01291      IF NAES-PO-SPTO-YES                                          ELXPMCBP
01292         SET SPT-YES TO TRUE                                       ELXPMCBP
01293         MOVE NAES-PO-SPTO-DRVD-CHR TO PMCI-SPCH-THRPY-FROM-IND    ELXPMCBP
01294      ELSE                                                         ELXPMCBP
01295         SET SPT-NO  TO TRUE.                                      ELXPMCBP
01296      IF NAES-PO-XRYO-YES                                          ELXPMCBP
01297         SET XRAY-YES TO TRUE                                      ELXPMCBP
01298         MOVE NAES-PO-XRYO-DRVD-CHR TO PMCI-XRAY-FROM-IND          ELXPMCBP
01299      ELSE                                                         ELXPMCBP
01300         SET XRAY-NO  TO TRUE.                                     ELXPMCBP
01301      IF NAES-PO-PRSO-YES                                          ELXPMCBP
01302         MOVE 'Y' TO PMCI-PROSTHETICS                              ELXPMCBP
01303      ELSE                                                         ELXPMCBP
01304         MOVE 'N' TO PMCI-PROSTHETICS.                             ELXPMCBP
01305                                                                   ELXPMCBP
01306      IF NAES-PO-HVIO-YES                                          ELXPMCBP
01307         MOVE 'Y' TO PMCI-HOME-VISIT                               ELXPMCBP
01308      ELSE                                                         ELXPMCBP
01309         MOVE 'N' TO PMCI-HOME-VISIT.                              ELXPMCBP
01310                                                                   ELXPMCBP
01311                                                                   ELXPMCBP
01312      IF NAES-PO-MSPO-YES                                          ELXPMCBP
01313         MOVE 'Y' TO PMCI-MED-SUPPLY                               ELXPMCBP
01314      ELSE MOVE 'N' TO PMCI-MED-SUPPLY.                            ELXPMCBP
01315                                                                   ELXPMCBP
01316 ************************************************************      ELXPMCBP
01317 *                                                          *      ELXPMCBP
01318 * DETERMINE PROFESSIONAL OUTPATIENT PSYCH, ALCOHOL AND     *      ELXPMCBP
01319 * DRUG ABUSE RESULT- GPO  ONLY - BASIC OR MAJOR MEDICAL    *      ELXPMCBP
01320 *                                                          *      ELXPMCBP
01321 ************************************************************      ELXPMCBP
01322                                                                   ELXPMCBP
01323  5110-SAVE-BSC-MM-PSY.                                            ELXPMCBP
01324                                                                   ELXPMCBP
01325      PERFORM 5120-DERIVE-PSY.                                     ELXPMCBP
01326      PERFORM 5130-DERIVE-ALC.                                     ELXPMCBP
01327      PERFORM 5140-DERIVE-DRG.                                     ELXPMCBP
01328                                                                   ELXPMCBP
01329 ************************************************************      ELXPMCBP
01330 *                                                          *      ELXPMCBP
01331 * DERIVE PSY SETTING AND CONTRACT COVERAGE TYPE            *      ELXPMCBP
01332 *                                                          *      ELXPMCBP
01333 ************************************************************      ELXPMCBP
01334                                                                   ELXPMCBP
01335  5120-DERIVE-PSY.                                                 ELXPMCBP
01336                                                                   ELXPMCBP
01337      IF SW-BSC-PSY-YES AND SW-MM-PSY-YES                          ELXPMCBP
01338         MOVE '+' TO PMCI-PSYCH-FROM-IND                           ELXPMCBP
01339         MOVE WS-BSC-SAVE-PSY TO PMCI-PSYCHIATRIC                  ELXPMCBP
01340      ELSE                                                         ELXPMCBP
01341      IF SW-MM-PSY-YES                                             ELXPMCBP
01342         MOVE '*' TO PMCI-PSYCH-FROM-IND                           ELXPMCBP
01343         MOVE WS-MM-SAVE-PSY TO PMCI-PSYCHIATRIC                   ELXPMCBP
01344      ELSE                                                         ELXPMCBP
01345         MOVE SPACES TO PMCI-PSYCH-FROM-IND                        ELXPMCBP
01346         MOVE WS-BSC-SAVE-PSY TO PMCI-PSYCHIATRIC.                 ELXPMCBP
01347                                                                   ELXPMCBP
01348 ************************************************************      ELXPMCBP
01349 *                                                          *      ELXPMCBP
01350 * DERIVE ALC SETTING AND CONTRACT COVERAGE TYPE            *      ELXPMCBP
01351 *                                                          *      ELXPMCBP
01352 ************************************************************      ELXPMCBP
01353                                                                   ELXPMCBP
01354  5130-DERIVE-ALC.                                                 ELXPMCBP
01355                                                                   ELXPMCBP
01356      IF SW-BSC-ALC-YES AND SW-MM-ALC-YES                          ELXPMCBP
01357         MOVE '+' TO PMCI-SBSTNCE-ABS-ALC-IND                      ELXPMCBP
01358         MOVE WS-BSC-SAVE-ALC TO                                   ELXPMCBP
01359                           PMCI-SBSTNCE-ABUSE-ALC                  ELXPMCBP
01360      ELSE                                                         ELXPMCBP
01361      IF SW-MM-ALC-YES                                             ELXPMCBP
01362         MOVE '*' TO PMCI-SBSTNCE-ABS-ALC-IND                      ELXPMCBP
01363         MOVE WS-MM-SAVE-ALC TO                                    ELXPMCBP
01364                           PMCI-SBSTNCE-ABUSE-ALC                  ELXPMCBP
01365      ELSE                                                         ELXPMCBP
01366         MOVE SPACES TO PMCI-SBSTNCE-ABS-ALC-IND                   ELXPMCBP
01367         MOVE WS-BSC-SAVE-ALC TO                                   ELXPMCBP
01368                           PMCI-SBSTNCE-ABUSE-ALC.                 ELXPMCBP
01369                                                                   ELXPMCBP
01370 ************************************************************      ELXPMCBP
01371 *                                                          *      ELXPMCBP
01372 * DERIVE DRG SETTING AND CONTRACT COVERAGE TYPE            *      ELXPMCBP
01373 *                                                          *      ELXPMCBP
01374 ************************************************************      ELXPMCBP
01375                                                                   ELXPMCBP
01376  5140-DERIVE-DRG.                                                 ELXPMCBP
01377                                                                   ELXPMCBP
01378      IF SW-BSC-DRG-YES AND SW-MM-DRG-YES                          ELXPMCBP
01379         MOVE '+' TO PMCI-SBSTNCE-ABS-DRG-IND                      ELXPMCBP
01380         MOVE WS-BSC-SAVE-DRG TO                                   ELXPMCBP
01381                     PMCI-SUBSTNCE-ABUSE-DRUG                      ELXPMCBP
01382      ELSE                                                         ELXPMCBP
01383      IF SW-MM-DRG-YES                                             ELXPMCBP
01384         MOVE '*' TO PMCI-SBSTNCE-ABS-DRG-IND                      ELXPMCBP
01385         MOVE WS-MM-SAVE-DRG TO                                    ELXPMCBP
01386                     PMCI-SUBSTNCE-ABUSE-DRUG                      ELXPMCBP
01387      ELSE                                                         ELXPMCBP
01388         MOVE SPACES TO PMCI-SBSTNCE-ABS-DRG-IND                   ELXPMCBP
01389         MOVE WS-BSC-SAVE-DRG TO                                   ELXPMCBP
01390                     PMCI-SUBSTNCE-ABUSE-DRUG.                     ELXPMCBP
01391                                                                   ELXPMCBP
01392 ************************************************************      ELXPMCBP
01393 *                                                          *      ELXPMCBP
01394 * SET PSY ALCOHOL AND DRUG DRERIVED INDICATORS             *      ELXPMCBP
01395 *                                                          *      ELXPMCBP
01396 ************************************************************      ELXPMCBP
01397                                                                   ELXPMCBP
01398  5150-SET-PSY-DRVD.                                               ELXPMCBP
01399                                                                   ELXPMCBP
01400      IF NAES-PO-GPO-YES                                           ELXPMCBP
01401         MOVE NAES-PO-GPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-ALC-IND     ELXPMCBP
01402         MOVE NAES-PO-GPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-DRG-IND     ELXPMCBP
01403         MOVE NAES-PO-GPO-DRVD-CHR TO PMCI-PSYCH-FROM-IND          ELXPMCBP
01404      ELSE                                                         ELXPMCBP
01405         MOVE NAES-PO-IPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-ALC-IND     ELXPMCBP
01406         MOVE NAES-PO-IPO-DRVD-CHR TO PMCI-SBSTNCE-ABS-DRG-IND     ELXPMCBP
01407         MOVE NAES-PO-IPO-DRVD-CHR TO PMCI-PSYCH-FROM-IND.         ELXPMCBP
01408                                                                   ELXPMCBP
01409 ************************************************************      ELXPMCBP
01410 *                                                          *      ELXPMCBP
01411 *    DETERMINE PROFESSIONAL OUTPATIENT OB RESULTS          *      ELXPMCBP
01412 *                                                          *      ELXPMCBP
01413 ************************************************************      ELXPMCBP
01414                                                                   ELXPMCBP
01415  6000-DTRMN-PO-OB-RSLTS.                                          ELXPMCBP
01416                                                                   ELXPMCBP
01417      EVALUATE TRUE                                                ELXPMCBP
01418      WHEN PMCI-MEMBER                                             ELXPMCBP
01419         IF NAES-PO-OBCM-YES                                       ELXPMCBP
01420            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01421            MOVE NAES-PI-OBCM-DRVD-CHR TO PMCI-OB-CMPLCTD-FROM-IND ELXPMCBP
01422         ELSE                                                      ELXPMCBP
01423            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01424         END-IF                                                    ELXPMCBP
01425         IF NAES-PO-OBNM-YES                                       ELXPMCBP
01426            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01427            MOVE NAES-PI-OBNM-DRVD-CHR TO PMCI-OB-NORM-FROM-IND    ELXPMCBP
01428         ELSE                                                      ELXPMCBP
01429            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01430         END-IF                                                    ELXPMCBP
01431      WHEN PMCI-SPOUSE                                             ELXPMCBP
01432         IF NAES-PO-OBCS-YES                                       ELXPMCBP
01433            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01434            MOVE NAES-PI-OBCS-DRVD-CHR TO PMCI-OB-CMPLCTD-FROM-IND ELXPMCBP
01435         ELSE                                                      ELXPMCBP
01436            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01437         END-IF                                                    ELXPMCBP
01438         IF NAES-PO-OBNS-YES                                       ELXPMCBP
01439            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01440            MOVE NAES-PI-OBNS-DRVD-CHR TO PMCI-OB-NORM-FROM-IND    ELXPMCBP
01441         ELSE                                                      ELXPMCBP
01442            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01443         END-IF                                                    ELXPMCBP
01444      WHEN PMCI-DEPENDENT                                          ELXPMCBP
01445         IF NAES-PO-OBCD-YES                                       ELXPMCBP
01446            SET OB-COMP-YES TO TRUE                                ELXPMCBP
01447            MOVE NAES-PI-OBCD-DRVD-CHR TO PMCI-OB-CMPLCTD-FROM-IND ELXPMCBP
01448         ELSE                                                      ELXPMCBP
01449            SET OB-COMP-NO  TO TRUE                                ELXPMCBP
01450         END-IF                                                    ELXPMCBP
01451         IF NAES-PO-OBND-YES                                       ELXPMCBP
01452            SET OB-NORM-YES TO TRUE                                ELXPMCBP
01453            MOVE NAES-PI-OBND-DRVD-CHR TO PMCI-OB-NORM-FROM-IND    ELXPMCBP
01454         ELSE                                                      ELXPMCBP
01455            SET OB-NORM-NO  TO TRUE                                ELXPMCBP
01456         END-IF                                                    ELXPMCBP
01457      WHEN OTHER                                                   ELXPMCBP
01458         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCBP
01459         MOVE +3010 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01460      END-EVALUATE.                                                ELXPMCBP
01461                                                                   ELXPMCBP
01462 ************************************************************      ELXPMCBP
01463 *                                                          *      ELXPMCBP
01464 *    DETERMINE DURABLE MEDICAL EQUIPMENT RENTAL/PURCHASE   *      ELXPMCBP
01465 *                                                          *      ELXPMCBP
01466 ************************************************************      ELXPMCBP
01467  6100-DTRMN-DME-DMR-RSLT.                                         ELXPMCBP
01468                                                                   ELXPMCBP
01469                                                                   ELXPMCBP
01470      IF WS-DME-LOB-IND = '*'                                      ELXPMCBP
01471         MOVE WS-DMR-CERT-REQ-IND-MM TO                            ELXPMCBP
01472                           WS-REN-CERT-REQUIRED-VALUE              ELXPMCBP
01473         MOVE WS-DME-CERT-REQ-IND-MM TO                            ELXPMCBP
01474                           WS-PUR-CERT-REQUIRED-VALUE              ELXPMCBP
01475      ELSE                                                         ELXPMCBP
01476         MOVE WS-DMR-CERT-REQ-IND-BSC TO                           ELXPMCBP
01477                           WS-REN-CERT-REQUIRED-VALUE              ELXPMCBP
01478         MOVE WS-DME-CERT-REQ-IND-BSC TO                           ELXPMCBP
01479                           WS-PUR-CERT-REQUIRED-VALUE.             ELXPMCBP
01480                                                                   ELXPMCBP
01481      IF SW-DME-YES AND SW-DMR-NO                                  ELXPMCBP
01482         SET DME-PURCH-ONLY TO TRUE                                ELXPMCBP
01483         MOVE WS-DME-LOB-IND TO PMCI-DME-FROM-IND                  ELXPMCBP
01484         EVALUATE TRUE                                             ELXPMCBP
01485         WHEN WS-PUR-CERT-REQUIRED                                 ELXPMCBP
01486            SET DME-MD-CERT-PURCH TO TRUE                          ELXPMCBP
01487         WHEN WS-NO-PUR-CERT-REQUIRED                              ELXPMCBP
01488            SET DME-MD-CERT-NO TO TRUE                             ELXPMCBP
01489         WHEN OTHER                                                ELXPMCBP
01490            SET DME-MD-CERT-CALL TO TRUE                           ELXPMCBP
01491         END-EVALUATE.                                             ELXPMCBP
01492                                                                   ELXPMCBP
01493      IF SW-DME-NO AND SW-DMR-YES                                  ELXPMCBP
01494         SET DME-RENTAL-ONLY TO TRUE                               ELXPMCBP
01495         MOVE WS-DME-LOB-IND TO PMCI-DME-FROM-IND                  ELXPMCBP
01496         EVALUATE TRUE                                             ELXPMCBP
01497         WHEN WS-REN-CERT-REQUIRED                                 ELXPMCBP
01498            SET DME-MD-CERT-RENTAL TO TRUE                         ELXPMCBP
01499         WHEN WS-NO-REN-CERT-REQUIRED                              ELXPMCBP
01500            SET DME-MD-CERT-NO TO TRUE                             ELXPMCBP
01501         WHEN OTHER                                                ELXPMCBP
01502            SET DME-MD-CERT-CALL TO TRUE                           ELXPMCBP
01503         END-EVALUATE.                                             ELXPMCBP
01504                                                                   ELXPMCBP
01505      IF SW-DME-YES AND SW-DMR-YES                                 ELXPMCBP
01506         SET DME-RENT-AND-PURCH TO TRUE                            ELXPMCBP
01507         MOVE WS-DME-LOB-IND TO PMCI-DME-FROM-IND                  ELXPMCBP
01508         EVALUATE TRUE                                             ELXPMCBP
01509         WHEN WS-REN-CERT-REQUIRED AND WS-PUR-CERT-REQUIRED        ELXPMCBP
01510            SET DME-MD-CERT-RENT-AND-PURCH TO TRUE                 ELXPMCBP
01511         WHEN WS-REN-CERT-REQUIRED AND WS-NO-PUR-CERT-REQUIRED     ELXPMCBP
01512            SET DME-MD-CERT-RENTAL TO TRUE                         ELXPMCBP
01513         WHEN WS-NO-REN-CERT-REQUIRED AND WS-PUR-CERT-REQUIRED     ELXPMCBP
01514            SET DME-MD-CERT-PURCH TO TRUE                          ELXPMCBP
01515         WHEN WS-NO-REN-CERT-REQUIRED AND WS-NO-PUR-CERT-REQUIRED  ELXPMCBP
01516            SET DME-MD-CERT-NO TO TRUE                             ELXPMCBP
01517         WHEN OTHER                                                ELXPMCBP
01518            SET DME-MD-CERT-CALL TO TRUE                           ELXPMCBP
01519         END-EVALUATE.                                             ELXPMCBP
01520                                                                   ELXPMCBP
01521                                                                   ELXPMCBP
01522 ******************************************************************ELXPMCBP
01523 *                                                                *ELXPMCBP
01524 * DETERMINE INSTITUTIONAL OUTPATIENT EMERGENCY SERVICES RESULTS  *ELXPMCBP
01525 *                                                                *ELXPMCBP
01526 ******************************************************************ELXPMCBP
01527                                                                   ELXPMCBP
01528  6200-DTRMN-PO-EMRGNCY-RSLTS.                                     ELXPMCBP
01529      IF NAES-PO-EAC-YES                                           ELXPMCBP
01530         PERFORM 6210-DTRMN-EAC-RSLT                               ELXPMCBP
01531         IF NAES-PO-EMC-YES                                        ELXPMCBP
01532            PERFORM 6220-DTRMN-EMC-RSLT                            ELXPMCBP
01533            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
01534         ELSE                                                      ELXPMCBP
01535            SET EMC-NO-COVERAGE TO TRUE                            ELXPMCBP
01536            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
01537         END-IF                                                    ELXPMCBP
01538      ELSE                                                         ELXPMCBP
01539         SET EAC-NO-COVERAGE TO TRUE                               ELXPMCBP
01540         IF NAES-PO-EMC-YES                                        ELXPMCBP
01541            PERFORM 6220-DTRMN-EMC-RSLT                            ELXPMCBP
01542            PERFORM 6230-DTRMN-ELT-RSLT                            ELXPMCBP
01543         ELSE                                                      ELXPMCBP
01544            SET EMC-NO-COVERAGE TO TRUE                            ELXPMCBP
01545            SET ELT-NOT-APPLICABLE TO TRUE                         ELXPMCBP
01546         END-IF                                                    ELXPMCBP
01547      END-IF.                                                      ELXPMCBP
01548                                                                   ELXPMCBP
01549 ******************************************************************ELXPMCBP
01550 *                                                                *ELXPMCBP
01551 * DETERMINE INSTITUTIONAL INPATIENT EMERGENCY ACCIDENT RESULT    *ELXPMCBP
01552 *                                                                *ELXPMCBP
01553 ******************************************************************ELXPMCBP
01554                                                                   ELXPMCBP
01555  6210-DTRMN-EAC-RSLT.                                             ELXPMCBP
01556      EVALUATE WS-DAYS-BTWN-ACCD-EMRG                              ELXPMCBP
01557      WHEN ZERO                                                    ELXPMCBP
01558         SET EAC-NO-HOURS TO TRUE                                  ELXPMCBP
01559         MOVE ZERO TO PMCI-EMERGENCY-ACCIDENT-CARE                 ELXPMCBP
01560      WHEN +999                                                    ELXPMCBP
01561         SET EAC-NO-HOURS TO TRUE                                  ELXPMCBP
01562         MOVE ZERO TO PMCI-EMERGENCY-ACCIDENT-CARE                 ELXPMCBP
01563      WHEN OTHER                                                   ELXPMCBP
01564         SET EAC-HOURS TO TRUE                                     ELXPMCBP
01565         COMPUTE PMCI-EMERGENCY-ACCIDENT-CARE                      ELXPMCBP
01566                 = WS-DAYS-BTWN-ACCD-EMRG * 24                     ELXPMCBP
01567      END-EVALUATE.                                                ELXPMCBP
01568                                                                   ELXPMCBP
01569 ******************************************************************ELXPMCBP
01570 *                                                                *ELXPMCBP
01571 * DETERMINE INSTITUTIONAL INPATIENT EMERGENCY MEDICAL RESULT     *ELXPMCBP
01572 *                                                                *ELXPMCBP
01573 ******************************************************************ELXPMCBP
01574                                                                   ELXPMCBP
01575  6220-DTRMN-EMC-RSLT.                                             ELXPMCBP
01576      EVALUATE WS-DAYS-BTWN-MED-EMRG                               ELXPMCBP
01577      WHEN ZERO                                                    ELXPMCBP
01578         SET EMC-NO-HOURS TO TRUE                                  ELXPMCBP
01579         MOVE ZERO TO PMCI-EMERGENCY-MEDICAL-CARE                  ELXPMCBP
01580      WHEN +999                                                    ELXPMCBP
01581         SET EMC-NO-HOURS TO TRUE                                  ELXPMCBP
01582         MOVE ZERO TO PMCI-EMERGENCY-MEDICAL-CARE                  ELXPMCBP
01583      WHEN OTHER                                                   ELXPMCBP
01584         SET EMC-HOURS TO TRUE                                     ELXPMCBP
01585         COMPUTE PMCI-EMERGENCY-MEDICAL-CARE                       ELXPMCBP
01586                 = WS-DAYS-BTWN-MED-EMRG * 24                      ELXPMCBP
01587      END-EVALUATE.                                                ELXPMCBP
01588                                                                   ELXPMCBP
01589 ******************************************************************ELXPMCBP
01590 *                                                                *ELXPMCBP
01591 * DETERMINE INSTITUTIONAL INPATIENT LIFE-THREATENING RESULT      *ELXPMCBP
01592 *                                                                *ELXPMCBP
01593 ******************************************************************ELXPMCBP
01594                                                                   ELXPMCBP
01595  6230-DTRMN-ELT-RSLT.                                             ELXPMCBP
01596      EVALUATE WS-DAYS-BTWN-LIFE-THRT                              ELXPMCBP
01597      WHEN ZERO                                                    ELXPMCBP
01598         SET ELT-NOT-APPLICABLE TO TRUE                            ELXPMCBP
01599         MOVE ZERO TO PMCI-EMERGENCY-LIFE-THREAT                   ELXPMCBP
01600      WHEN +999                                                    ELXPMCBP
01601         SET ELT-NO-HOURS TO TRUE                                  ELXPMCBP
01602         MOVE ZERO TO PMCI-EMERGENCY-LIFE-THREAT                   ELXPMCBP
01603      WHEN OTHER                                                   ELXPMCBP
01604         SET ELT-HOURS TO TRUE                                     ELXPMCBP
01605         COMPUTE PMCI-EMERGENCY-LIFE-THREAT                        ELXPMCBP
01606                 = WS-DAYS-BTWN-LIFE-THRT * 24                     ELXPMCBP
01607      END-EVALUATE.                                                ELXPMCBP
01608                                                                   ELXPMCBP
01609 ******************************************************************ELXPMCBP
01610 *                                                                *ELXPMCBP
01611 * READ CLDR TABS FOR PSYCH - ALCOHOL AND DRUG                    *ELXPMCBP
01612 *                                                                *ELXPMCBP
01613 ******************************************************************ELXPMCBP
01614                                                                   ELXPMCBP
01615  6500-READ-CLDR-TABS.                                             ELXPMCBP
01616                                                                   ELXPMCBP
01617      IF NAES-PO-GPO-YES OR NAES-PO-IPO-YES                        ELXPMCBP
01618         PERFORM 6510-SRCH-CLDR-TABS.                              ELXPMCBP
01619                                                                   ELXPMCBP
01620 ******************************************************************ELXPMCBP
01621 *                                                                *ELXPMCBP
01622 *        SEARCH FOR CLDR TABULAR                                 *ELXPMCBP
01623 *                                                                *ELXPMCBP
01624 ******************************************************************ELXPMCBP
01625  6510-SRCH-CLDR-TABS.                                             ELXPMCBP
01626      SET GCT-TAB-INDEX TO 18.                                     ELXPMCBP
01627      SET WS-GCT-MAX-IDX TO GCT-TAB-INDEX.                         ELXPMCBP
01628      SET SW-CLDR-NOT-FOUND TO TRUE                                ELXPMCBP
01629      PERFORM VARYING GCT-TAB-INDEX                                ELXPMCBP
01630        FROM 1 BY 1 UNTIL                                          ELXPMCBP
01631             GCT-CON-TAB-ID (GCT-TAB-INDEX) > WS-CLDR              ELXPMCBP
01632                OR GCT-TAB-INDEX > WS-GCT-MAX-IDX                  ELXPMCBP
01633        IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = WS-CLDR                ELXPMCBP
01634           AND (GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZERO)           ELXPMCBP
01635            MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX) TO            ELXPMCBP
01636                                KWA-GCTABULR-KEY                   ELXPMCBP
01637            PERFORM 6520-READ-CLDR-TAB                             ELXPMCBP
01638            IF IOP-RC-OK                                           ELXPMCBP
01639               SET ADDRESS OF CLDR-TABULAR-RECORD                  ELXPMCBP
01640                         TO IOP-REC-PTR                            ELXPMCBP
01641               SET PMCI-BC-SUCCESSFUL TO TRUE                      ELXPMCBP
01642               SET SW-CLDR-FOUND TO TRUE                           ELXPMCBP
01643               PERFORM 6530-DTRMN-PSYCH-CVRG                       ELXPMCBP
01644           ELSE                                                    ELXPMCBP
01645               SET PMCI-BC-INTERNAL-ERROR TO TRUE                  ELXPMCBP
01646               MOVE +3012 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCBP
01647           END-IF                                                  ELXPMCBP
01648        ELSE                                                       ELXPMCBP
01649           CONTINUE                                                ELXPMCBP
01650        END-IF                                                     ELXPMCBP
01651      END-PERFORM.                                                 ELXPMCBP
01652                                                                   ELXPMCBP
01653 ******************************************************************ELXPMCBP
01654 *                                                                *ELXPMCBP
01655 *            READ CLDR TABULAR                                   *ELXPMCBP
01656 *                                                                *ELXPMCBP
01657 ******************************************************************ELXPMCBP
01658                                                                   ELXPMCBP
01659  6520-READ-CLDR-TAB.                                              ELXPMCBP
01660      SET CIA-GCTABULR-DDN TO TRUE.                                ELXPMCBP
01661      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
01662                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELXPMCBP
01663      IF CIA-RC-OK                                                 ELXPMCBP
01664         MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY                     ELXPMCBP
01665         SET IOP-RD TO TRUE                                        ELXPMCBP
01666         SET IOP-FCQ-NONE TO TRUE                                  ELXPMCBP
01667         SET IOP-KVQ-EQ TO TRUE                                    ELXPMCBP
01668         SET IOP-STG-MODE-LOCATE TO TRUE                           ELXPMCBP
01669         MOVE SPACES TO IOP-AIX-DDNAME                             ELXPMCBP
01670         CALL 'ELUIOPGM' USING DFHEIBLK                            ELXPMCBP
01671                               DFHCOMMAREA END-CALL.               ELXPMCBP
01672                                                                   ELXPMCBP
01673                                                                   ELXPMCBP
01674 ******************************************************************ELXPMCBP
01675 *                                                                *ELXPMCBP
01676 *       DETERMINE PSYCHIATRIC COVERAGE                           *ELXPMCBP
01677 *                                                                *ELXPMCBP
01678 * THE LARGER THE LIST MATCH CONFIDENCE FACTOR THE MORE THE       *ELXPMCBP
01679 * BENEFIT IS EXCLUDED.                                           *ELXPMCBP
01680 *                                                                *ELXPMCBP
01681 ******************************************************************ELXPMCBP
01682                                                                   ELXPMCBP
01683  6530-DTRMN-PSYCH-CVRG.                                           ELXPMCBP
01684      CALL 'ELKCLDRM' USING CLDR-TABULAR-RECORD                    ELXPMCBP
01685                            DMNL-DIAGNOSES-TABLE                   ELXPMCBP
01686                            CFDB-CNFDNC-FCTR-DATA-BLCK             ELXPMCBP
01687      IF RETURN-CODE = ZERO                                        ELXPMCBP
01688         MOVE CFDB-CF-CLDR-UNWGHTD-LST-MTCH TO WS-CLDR-CF          ELXPMCBP
01689         IF WS-CLDR-CF > WS-CF-CLDR-YES                            ELXPMCBP
01690            SET PSY-YES TO TRUE                                    ELXPMCBP
01691         ELSE                                                      ELXPMCBP
01692            IF WS-CLDR-CF < WS-CF-CLDR-NO                          ELXPMCBP
01693               SET PSY-NO TO TRUE                                  ELXPMCBP
01694            ELSE                                                   ELXPMCBP
01695               SET PSY-CALL TO TRUE                                ELXPMCBP
01696            END-IF                                                 ELXPMCBP
01697         END-IF                                                    ELXPMCBP
01698      END-IF.                                                      ELXPMCBP
01699      IF RETURN-CODE = ZERO                                        ELXPMCBP
01700         IF WS-PASS-VALUE = 1                                      ELXPMCBP
01701            MOVE PMCI-PSYCHIATRIC TO WS-BSC-SAVE-PSY               ELXPMCBP
01702         ELSE                                                      ELXPMCBP
01703            MOVE PMCI-PSYCHIATRIC TO WS-MM-SAVE-PSY                ELXPMCBP
01704      ELSE                                                         ELXPMCBP
01705         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01706         MOVE +3013 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBP
01707                                                                   ELXPMCBP
01708      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBP
01709         PERFORM 6540-DTRMN-ALCOHOL-CVRG.                          ELXPMCBP
01710                                                                   ELXPMCBP
01711                                                                   ELXPMCBP
01712 ******************************************************************ELXPMCBP
01713 *                                                                *ELXPMCBP
01714 *       DETERMINE ALCOHOL ABUSE COVERAGE                         *ELXPMCBP
01715 *                                                                *ELXPMCBP
01716 * THE LARGER THE LIST MATCH CONFIDENCE FACTOR THE MORE THE       *ELXPMCBP
01717 * BENEFIT IS EXCLUDED.                                           *ELXPMCBP
01718 ******************************************************************ELXPMCBP
01719                                                                   ELXPMCBP
01720  6540-DTRMN-ALCOHOL-CVRG.                                         ELXPMCBP
01721      CALL 'ELKCLDRM' USING CLDR-TABULAR-RECORD                    ELXPMCBP
01722                            ALCL-DIAGNOSES-TABLE                   ELXPMCBP
01723                            CFDB-CNFDNC-FCTR-DATA-BLCK             ELXPMCBP
01724      IF RETURN-CODE = ZERO                                        ELXPMCBP
01725         MOVE CFDB-CF-CLDR-UNWGHTD-LST-MTCH TO WS-CLDR-CF          ELXPMCBP
01726         IF WS-CLDR-CF > WS-CF-CLDR-YES                            ELXPMCBP
01727            SET SUB-ABUSE-ALC-YES TO TRUE                          ELXPMCBP
01728         ELSE                                                      ELXPMCBP
01729            IF WS-CLDR-CF < WS-CF-CLDR-NO                          ELXPMCBP
01730               SET SUB-ABUSE-ALC-NO TO TRUE                        ELXPMCBP
01731            ELSE                                                   ELXPMCBP
01732               SET SUB-ABUSE-ALC-CALL TO TRUE                      ELXPMCBP
01733            END-IF                                                 ELXPMCBP
01734         END-IF                                                    ELXPMCBP
01735      END-IF.                                                      ELXPMCBP
01736      IF RETURN-CODE = ZERO                                        ELXPMCBP
01737         IF WS-PASS-VALUE = 1                                      ELXPMCBP
01738            MOVE PMCI-SBSTNCE-ABUSE-ALC TO WS-BSC-SAVE-ALC         ELXPMCBP
01739         ELSE                                                      ELXPMCBP
01740            MOVE PMCI-SBSTNCE-ABUSE-ALC TO WS-MM-SAVE-ALC          ELXPMCBP
01741      ELSE                                                         ELXPMCBP
01742         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01743         MOVE +3014 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBP
01744                                                                   ELXPMCBP
01745      IF PMCI-BC-SUCCESSFUL                                        ELXPMCBP
01746         PERFORM 6550-DTRMN-ALCOHOL-CVRG.                          ELXPMCBP
01747                                                                   ELXPMCBP
01748 ******************************************************************ELXPMCBP
01749 *                                                                *ELXPMCBP
01750 *       DETERMINE DRUG ABUSE COVERAGE                            *ELXPMCBP
01751 *                                                                *ELXPMCBP
01752 * THE LARGER THE LIST MATCH CONFIDENCE FACTOR THE MORE THE       *ELXPMCBP
01753 * BENEFIT IS EXCLUDED.                                           *ELXPMCBP
01754 ******************************************************************ELXPMCBP
01755                                                                   ELXPMCBP
01756  6550-DTRMN-ALCOHOL-CVRG.                                         ELXPMCBP
01757      CALL 'ELKCLDRM' USING CLDR-TABULAR-RECORD                    ELXPMCBP
01758                            DRGL-DIAGNOSES-TABLE                   ELXPMCBP
01759                            CFDB-CNFDNC-FCTR-DATA-BLCK.            ELXPMCBP
01760      IF WS-CLDR-CF > WS-CF-CLDR-YES                               ELXPMCBP
01761          MOVE CFDB-CF-CLDR-UNWGHTD-LST-MTCH TO WS-CLDR-CF         ELXPMCBP
01762          SET SUB-ABUSE-DRG-YES TO TRUE                            ELXPMCBP
01763      ELSE                                                         ELXPMCBP
01764          IF WS-CLDR-CF < WS-CF-CLDR-NO                            ELXPMCBP
01765             SET SUB-ABUSE-DRG-NO TO TRUE                          ELXPMCBP
01766         ELSE                                                      ELXPMCBP
01767            SET SUB-ABUSE-DRG-CALL TO TRUE                         ELXPMCBP
01768         END-IF                                                    ELXPMCBP
01769      END-IF.                                                      ELXPMCBP
01770      IF RETURN-CODE = ZERO                                        ELXPMCBP
01771         IF WS-PASS-VALUE = 1                                      ELXPMCBP
01772            MOVE PMCI-SUBSTNCE-ABUSE-DRUG TO WS-BSC-SAVE-DRG       ELXPMCBP
01773         ELSE                                                      ELXPMCBP
01774            MOVE PMCI-SUBSTNCE-ABUSE-DRUG TO WS-MM-SAVE-DRG        ELXPMCBP
01775      ELSE                                                         ELXPMCBP
01776         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01777         MOVE +3015 TO PMCI-BLUE-CHIP-ERROR-CODE.                  ELXPMCBP
01778                                                                   ELXPMCBP
01779 ************************************************************      ELXPMCBP
01780 *                                                          *      ELXPMCBP
01781 *    SCAN CONTRACT FOR BENEFIT PROVISION RESULTS           *      ELXPMCBP
01782 *                                                          *      ELXPMCBP
01783 ************************************************************      ELXPMCBP
01784                                                                   ELXPMCBP
01785  7000-GET-BNFT-PRVNS.                                             ELXPMCBP
01786                                                                   ELXPMCBP
01787      IF WS-PASS-VALUE = 1 OR                                      ELXPMCBP
01788         WS-PASS-VALUE = 2 AND PMCI-BSC-CNTRCT-GRP = SPACES        ELXPMCBP
01789         MOVE ZERO TO WS-PVE-TBL-NBR-ENTRS                         ELXPMCBP
01790         SET WS-PVE-IDX                                            ELXPMCBP
01791             WS-PVE-MAX-IDX                                        ELXPMCBP
01792                   TO WS-PVE-TBL-NBR-ENTRS                         ELXPMCBP
01793         SET BPLS-MAX-INDEX TO BPLS-TABLE-MAX.                     ELXPMCBP
01794      SET GCT-INDEX TO 1.                                          ELXPMCBP
01795      PERFORM 7100-TAG-PROVISIONS                                  ELXPMCBP
01796         VARYING BPLS-INDEX FROM 1 BY 1                            ELXPMCBP
01797           UNTIL    BPLS-INDEX > BPLS-MAX-INDEX                    ELXPMCBP
01798                 OR NOT PMCI-BC-SUCCESSFUL.                        ELXPMCBP
01799                                                                   ELXPMCBP
01800 ************************************************************      ELXPMCBP
01801 *                                                          *      ELXPMCBP
01802 *    SCAN CONTRACT FOR EACH BENEFIT PROVISION              *      ELXPMCBP
01803 *                                                          *      ELXPMCBP
01804 ************************************************************      ELXPMCBP
01805                                                                   ELXPMCBP
01806  7100-TAG-PROVISIONS.                                             ELXPMCBP
01807      IF WS-PASS-VALUE = 1 OR                                      ELXPMCBP
01808         WS-PASS-VALUE = 2 AND PMCI-BSC-CNTRCT-GRP = SPACES        ELXPMCBP
01809         INITIALIZE BPLS-DRVD-IND (BPLS-INDEX)                     ELXPMCBP
01810         INITIALIZE BPLS-DRVD-CHR (BPLS-INDEX)                     ELXPMCBP
01811         SET BPLS-NO (BPLS-INDEX) TO TRUE.                         ELXPMCBP
01812      SET SW-BNFT-PRVSN-NOT-RSLVD TO TRUE.                         ELXPMCBP
01813                                                                   ELXPMCBP
01814      PERFORM WITH TEST AFTER                                      ELXPMCBP
01815         UNTIL    SW-BNFT-PRVSN-RSLVD                              ELXPMCBP
01816               OR NOT PMCI-BC-SUCCESSFUL                           ELXPMCBP
01817         EVALUATE TRUE                                             ELXPMCBP
01818         WHEN GCT-INDEX > WS-GCT-MAX-IDX                           ELXPMCBP
01819            SET BPLS-INDEX TO BPLS-MAX-INDEX                       ELXPMCBP
01820            SET SW-BNFT-PRVSN-RSLVD TO TRUE                        ELXPMCBP
01821         WHEN   BPLS-BENPROV (BPLS-INDEX)                          ELXPMCBP
01822              > GCT-BEN-PROVN-ID (GCT-INDEX)                       ELXPMCBP
01823            SET GCT-INDEX UP BY 1                                  ELXPMCBP
01824         WHEN   BPLS-BENPROV (BPLS-INDEX)                          ELXPMCBP
01825              = GCT-BEN-PROVN-ID (GCT-INDEX)                       ELXPMCBP
01826            MOVE BPLS-BENPROV (BPLS-INDEX) TO KWA-GCP-PROVN-ID     ELXPMCBP
01827            MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)                 ELXPMCBP
01828              TO KWA-GCP-PROVN-SLOT-NO                             ELXPMCBP
01829            IF PMCI-BLUE-STORM-CALL                                ELXPMCBP
01830               MOVE 'Y' TO BPLS-BP-SW (BPLS-INDEX)                 ELXPMCBP
01831               ADD WS-PASS-VALUE TO BPLS-DRVD-IND (BPLS-INDEX)     ELXPMCBP
01832            ELSE                                                   ELXPMCBP
01833               PERFORM 7200-READ-BNFT-PRVSN                        ELXPMCBP
01834            END-IF                                                 ELXPMCBP
01835            SET SW-BNFT-PRVSN-RSLVD TO TRUE                        ELXPMCBP
01836            SET GCT-INDEX UP BY 1                                  ELXPMCBP
01837         WHEN GCT-BEN-PROVN-ID (GCT-INDEX) = HIGH-VALUES           ELXPMCBP
01838            SET BPLS-INDEX TO BPLS-MAX-INDEX                       ELXPMCBP
01839            SET SW-BNFT-PRVSN-RSLVD TO TRUE                        ELXPMCBP
01840         WHEN OTHER                                                ELXPMCBP
01841            SET SW-BNFT-PRVSN-RSLVD TO TRUE                        ELXPMCBP
01842         END-EVALUATE                                              ELXPMCBP
01843         END-PERFORM.                                              ELXPMCBP
01844                                                                   ELXPMCBP
01845 ************************************************************      ELXPMCBP
01846 *                                                          *      ELXPMCBP
01847 *    READ CURRENT BENEFIT PROVISION RECORD                 *      ELXPMCBP
01848 *                                                          *      ELXPMCBP
01849 ************************************************************      ELXPMCBP
01850                                                                   ELXPMCBP
01851  7200-READ-BNFT-PRVSN.                                            ELXPMCBP
01852      SET CIA-GCBENPRV-DDN TO TRUE.                                ELXPMCBP
01853      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
01854                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.ELXPMCBP
01855      IF CIA-RC-OK                                                 ELXPMCBP
01856      THEN                                                         ELXPMCBP
01857         MOVE KWA-GCBENPRV-KEY TO IOP-FILE-KEY                     ELXPMCBP
01858         SET IOP-RD TO TRUE                                        ELXPMCBP
01859         SET IOP-FCQ-NONE TO TRUE                                  ELXPMCBP
01860         SET IOP-KVQ-EQ TO TRUE                                    ELXPMCBP
01861         SET IOP-STG-MODE-LOCATE TO TRUE                           ELXPMCBP
01862         MOVE SPACES TO IOP-AIX-DDNAME                             ELXPMCBP
01863         CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL       ELXPMCBP
01864         IF CIA-RC-OK                                              ELXPMCBP
01865            SET ADDRESS OF BENEFIT-PROVISION-RECORD TO IOP-REC-PTR ELXPMCBP
01866            PERFORM 7300-TEST-PRVDR-ELGBLTY                        ELXPMCBP
01867         ELSE                                                      ELXPMCBP
01868            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBP
01869            MOVE +3006 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBP
01870         END-IF                                                    ELXPMCBP
01871      ELSE                                                         ELXPMCBP
01872         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01873         MOVE +3005 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01874      END-IF.                                                      ELXPMCBP
01875                                                                   ELXPMCBP
01876 ************************************************************      ELXPMCBP
01877 *                                                          *      ELXPMCBP
01878 *    DETERMINE PROVIDER ELIGIBLITY FROM THE PVE TABULAR    *      ELXPMCBP
01879 *                                                          *      ELXPMCBP
01880 ************************************************************      ELXPMCBP
01881                                                                   ELXPMCBP
01882  7300-TEST-PRVDR-ELGBLTY.                                         ELXPMCBP
01883 * -- FIND PVE TABULAR SLOT NUMBER                                 ELXPMCBP
01884      SET GCP-INDEX TO GCP-COUNT-TAB-PROVN-POINTERS.               ELXPMCBP
01885      SET WS-GCP-MAX-IDX TO GCP-INDEX.                             ELXPMCBP
01886      SET SW-PVE-NOT-FND TO TRUE                                   ELXPMCBP
01887      MOVE WS-PVE TO KWA-PROVISION-ID.                             ELXPMCBP
01888      PERFORM WITH TEST BEFORE                                     ELXPMCBP
01889         VARYING GCP-INDEX FROM 1 BY 1                             ELXPMCBP
01890           UNTIL    GCP-INDEX > WS-GCP-MAX-IDX                     ELXPMCBP
01891                 OR SW-PVE-FND                                     ELXPMCBP
01892         IF GCP-BP-ID (GCP-INDEX) = WS-PVE                         ELXPMCBP
01893         THEN                                                      ELXPMCBP
01894            SET SW-PVE-FND TO TRUE                                 ELXPMCBP
01895            MOVE GCP-BP-SLOT-NO (GCP-INDEX)                        ELXPMCBP
01896              TO KWA-PROVISION-SLOT-NO                             ELXPMCBP
01897            PERFORM 7310-GET-PVE-RSLT                              ELXPMCBP
01898         END-IF                                                    ELXPMCBP
01899      END-PERFORM.                                                 ELXPMCBP
01900                                                                   ELXPMCBP
01901      IF SW-PVE-NOT-FND                                            ELXPMCBP
01902      THEN                                                         ELXPMCBP
01903 *    -- INDICATE INTERNAL ERROR - NO PVE ON BP                    ELXPMCBP
01904         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01905         MOVE +3006 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01906      END-IF.                                                      ELXPMCBP
01907                                                                   ELXPMCBP
01908 ************************************************************      ELXPMCBP
01909 *                                                          *      ELXPMCBP
01910 *    GET PROVIDER ELIGIBLITY RESULT FROM THE PVE TABULAR   *      ELXPMCBP
01911 *                                                          *      ELXPMCBP
01912 ************************************************************      ELXPMCBP
01913                                                                   ELXPMCBP
01914  7310-GET-PVE-RSLT.                                               ELXPMCBP
01915 * -- SCAN SAVE TABLE FOR SLOT NUMBER PREVIOUSLY USED              ELXPMCBP
01916      SET SW-PVE-TBL-NOT-FND TO TRUE.                              ELXPMCBP
01917      PERFORM WITH TEST BEFORE                                     ELXPMCBP
01918         VARYING WS-PVE-IDX FROM 1 BY 1                            ELXPMCBP
01919           UNTIL    WS-PVE-IDX > WS-PVE-MAX-IDX                    ELXPMCBP
01920                 OR SW-PVE-TBL-FND                                 ELXPMCBP
01921         IF WS-PVE-SLOT-NO (WS-PVE-IDX) = KWA-PROVISION-SLOT-NO    ELXPMCBP
01922            SET SW-PVE-TBL-FND TO TRUE                             ELXPMCBP
01923            IF WS-PVE-PRVDR-TYP-ELGBL (WS-PVE-IDX)                 ELXPMCBP
01924               MOVE 'Y' TO BPLS-BP-SW (BPLS-INDEX)                 ELXPMCBP
01925               ADD WS-PASS-VALUE TO BPLS-DRVD-IND (BPLS-INDEX)     ELXPMCBP
01926               PERFORM 7500-EXTRCT-ADTNL-INFRMTN                   ELXPMCBP
01927            END-IF                                                 ELXPMCBP
01928         END-IF                                                    ELXPMCBP
01929      END-PERFORM.                                                 ELXPMCBP
01930                                                                   ELXPMCBP
01931      IF SW-PVE-TBL-NOT-FND                                        ELXPMCBP
01932         PERFORM 7400-READ-TEST-PVE                                ELXPMCBP
01933         IF PMCI-BC-SUCCESSFUL                                     ELXPMCBP
01934            IF WS-PVE-PRVDR-TYP-ELGBL (WS-PVE-IDX)                 ELXPMCBP
01935               MOVE 'Y' TO BPLS-BP-SW (BPLS-INDEX)                 ELXPMCBP
01936               ADD WS-PASS-VALUE TO BPLS-DRVD-IND (BPLS-INDEX)     ELXPMCBP
01937               PERFORM 7500-EXTRCT-ADTNL-INFRMTN.                  ELXPMCBP
01938                                                                   ELXPMCBP
01939                                                                   ELXPMCBP
01940 ************************************************************      ELXPMCBP
01941 *                                                          *      ELXPMCBP
01942 *    READ NEW PROVIDER ELIGIBILITY TABULAR (PVE)           *      ELXPMCBP
01943 *                                                          *      ELXPMCBP
01944 ************************************************************      ELXPMCBP
01945                                                                   ELXPMCBP
01946  7400-READ-TEST-PVE.                                              ELXPMCBP
01947      SET CIA-GCTABULR-DDN TO TRUE.                                ELXPMCBP
01948      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCBP
01949                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.ELXPMCBP
01950      IF CIA-RC-OK                                                 ELXPMCBP
01951      THEN                                                         ELXPMCBP
01952         MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY                     ELXPMCBP
01953         SET IOP-RD TO TRUE                                        ELXPMCBP
01954         SET IOP-FCQ-NONE TO TRUE                                  ELXPMCBP
01955         SET IOP-KVQ-EQ TO TRUE                                    ELXPMCBP
01956         SET IOP-STG-MODE-LOCATE TO TRUE                           ELXPMCBP
01957         MOVE SPACES TO IOP-AIX-DDNAME                             ELXPMCBP
01958         CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL       ELXPMCBP
01959         IF CIA-RC-OK                                              ELXPMCBP
01960         THEN                                                      ELXPMCBP
01961            SET ADDRESS OF PROVIDER-ELIGIBILITY-RECORD             ELXPMCBP
01962             TO IOP-REC-PTR                                        ELXPMCBP
01963            PERFORM 7410-TEST-PVE                                  ELXPMCBP
01964         ELSE                                                      ELXPMCBP
01965            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCBP
01966            MOVE +3008 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCBP
01967      ELSE                                                         ELXPMCBP
01968         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCBP
01969         MOVE +3007 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCBP
01970      END-IF.                                                      ELXPMCBP
01971                                                                   ELXPMCBP
01972 ************************************************************      ELXPMCBP
01973 *                                                          *      ELXPMCBP
01974 *    TEST PROVIDER TYPE FOR ELIGIBILITY ON THIS PVE TAB    *      ELXPMCBP
01975 *                                                          *      ELXPMCBP
01976 ************************************************************      ELXPMCBP
01977                                                                   ELXPMCBP
01978  7410-TEST-PVE.                                                   ELXPMCBP
01979 * -- ADD NEW PVE SLOT NUMBER TO SAVE TABLE                        ELXPMCBP
01980      ADD 1 TO WS-PVE-TBL-NBR-ENTRS.                               ELXPMCBP
01981      SET WS-PVE-IDX                                               ELXPMCBP
01982          WS-PVE-MAX-IDX                                           ELXPMCBP
01983       TO WS-PVE-TBL-NBR-ENTRS.                                    ELXPMCBP
01984      MOVE KWA-PROVISION-SLOT-NO                                   ELXPMCBP
01985        TO WS-PVE-SLOT-NO (WS-PVE-MAX-IDX).                        ELXPMCBP
01986      SET WS-PVE-PRVDR-TYP-NOT-ELGBL (WS-PVE-MAX-IDX) TO TRUE.     ELXPMCBP
01987                                                                   ELXPMCBP
01988 * -- SCAN PVE FOR PROVIDER TYPE - INITIALIZE LOOP CONTROLS        ELXPMCBP
01989      SET GBJ-INDEX TO GBJ-ENTRY-COUNT.                            ELXPMCBP
01990      SET WS-GBJ-MAX-IDX TO GBJ-INDEX.                             ELXPMCBP
01991      SET SW-PRVDR-TYP-NOT-FND TO TRUE.                            ELXPMCBP
01992                                                                   ELXPMCBP
01993 * -- SCAN PVE FOR PROVIDER TYPE                                   ELXPMCBP
01994      PERFORM WITH TEST BEFORE                                     ELXPMCBP
01995         VARYING GBJ-INDEX FROM 1 BY 1                             ELXPMCBP
01996           UNTIL    GBJ-INDEX > WS-GBJ-MAX-IDX                     ELXPMCBP
01997                 OR SW-PRVDR-TYP-FND                               ELXPMCBP
01998                 OR   GBJ-PROVIDER-CODE (GBJ-INDEX)                ELXPMCBP
01999                    > PMCI-PROVIDER-TYPE                           ELXPMCBP
02000         IF GBJ-PROVIDER-CODE (GBJ-INDEX) = PMCI-PROVIDER-TYPE     ELXPMCBP
02001         THEN                                                      ELXPMCBP
02002            SET SW-PRVDR-TYP-FND TO TRUE                           ELXPMCBP
02003            SET WS-PVE-PRVDR-TYP-ELGBL (WS-PVE-MAX-IDX) TO TRUE    ELXPMCBP
02004         END-IF                                                    ELXPMCBP
02005      END-PERFORM.                                                 ELXPMCBP
02006                                                                   ELXPMCBP
02007 ************************************************************      ELXPMCBP
02008 *                                                          *      ELXPMCBP
02009 *    EXTRACT ADDITIONAL INFORMATION FOR SELECTED BPIDS     *      ELXPMCBP
02010 *                                                          *      ELXPMCBP
02011 ************************************************************      ELXPMCBP
02012                                                                   ELXPMCBP
02013  7500-EXTRCT-ADTNL-INFRMTN.                                       ELXPMCBP
02014                                                                   ELXPMCBP
02015      IF WS-PASS-VALUE = 1                                         ELXPMCBP
02016         PERFORM 7600-EXTRCT-ADTNL-INFRMTN-BSC                     ELXPMCBP
02017      ELSE                                                         ELXPMCBP
02018         PERFORM 7700-EXTRCT-ADTNL-INFRMTN-MM.                     ELXPMCBP
02019                                                                   ELXPMCBP
02020                                                                   ELXPMCBP
02021 ************************************************************      ELXPMCBP
02022 *                                                          *      ELXPMCBP
02023 *    EXTRACT ADDITIONAL INFORMATION FOR SELECTED BPIDS     *      ELXPMCBP
02024 *    FOR BASIC CONTRACTS ONLY                              *      ELXPMCBP
02025 *                                                          *      ELXPMCBP
02026 ************************************************************      ELXPMCBP
02027                                                                   ELXPMCBP
02028  7600-EXTRCT-ADTNL-INFRMTN-BSC.                                   ELXPMCBP
02029                                                                   ELXPMCBP
02030      MOVE BPLS-BENPROV (BPLS-INDEX) TO WS-BP-TEST-ID.             ELXPMCBP
02031                                                                   ELXPMCBP
02032      IF PMCI-INSTITUTIONAL                                        ELXPMCBP
02033      THEN                                                         ELXPMCBP
02034         IF PMCI-INPATIENT                                         ELXPMCBP
02035         THEN                                                      ELXPMCBP
02036            EVALUATE TRUE                                          ELXPMCBP
02037            WHEN WS-BP-II-DMR-REN                                  ELXPMCBP
02038               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02039                                  WS-DMR-CERT-REQ-IND-BSC          ELXPMCBP
02040            WHEN WS-BP-II-DME-PUR                                  ELXPMCBP
02041               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02042                                  WS-DME-CERT-REQ-IND-BSC          ELXPMCBP
02043            WHEN WS-BP-II-PVTA                                     ELXPMCBP
02044               MOVE GCP-PROVN-PRICING-METHD                        ELXPMCBP
02045                 TO WS-PVTA-PRIC-METH-BSC                          ELXPMCBP
02046            WHEN WS-BP-II-PVTR                                     ELXPMCBP
02047               MOVE GCP-PROVN-PRICING-METHD                        ELXPMCBP
02048                 TO WS-PVTR-PRIC-METH-BSC                          ELXPMCBP
02049            WHEN OTHER                                             ELXPMCBP
02050               CONTINUE                                            ELXPMCBP
02051            END-EVALUATE                                           ELXPMCBP
02052         ELSE                                                      ELXPMCBP
02053            EVALUATE TRUE                                          ELXPMCBP
02054            WHEN WS-BP-IO-DMR-REN                                  ELXPMCBP
02055               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02056                                   WS-DMR-CERT-REQ-IND-BSC         ELXPMCBP
02057            WHEN WS-BP-IO-DME-PUR                                  ELXPMCBP
02058               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02059                                  WS-DME-CERT-REQ-IND-BSC          ELXPMCBP
02060            WHEN OTHER                                             ELXPMCBP
02061               CONTINUE                                            ELXPMCBP
02062            END-EVALUATE                                           ELXPMCBP
02063         END-IF                                                    ELXPMCBP
02064      ELSE                                                         ELXPMCBP
02065         IF PMCI-INPATIENT                                         ELXPMCBP
02066         THEN                                                      ELXPMCBP
02067            EVALUATE TRUE                                          ELXPMCBP
02068            WHEN WS-BP-PI-DMR-REN                                  ELXPMCBP
02069               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02070                                      WS-DMR-CERT-REQ-IND-BSC      ELXPMCBP
02071            WHEN WS-BP-PI-DME-PUR                                  ELXPMCBP
02072               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02073                                      WS-DME-CERT-REQ-IND-BSC      ELXPMCBP
02074            WHEN OTHER                                             ELXPMCBP
02075               CONTINUE                                            ELXPMCBP
02076            END-EVALUATE                                           ELXPMCBP
02077         ELSE                                                      ELXPMCBP
02078            EVALUATE TRUE                                          ELXPMCBP
02079            WHEN WS-BP-PO-DMR-REN                                  ELXPMCBP
02080               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02081                                  WS-DMR-CERT-REQ-IND-BSC          ELXPMCBP
02082            WHEN WS-BP-PO-DME-PUR                                  ELXPMCBP
02083               MOVE GCP-CERTFN-REQRM-IND TO                        ELXPMCBP
02084                                  WS-DME-CERT-REQ-IND-BSC          ELXPMCBP
02085            WHEN OTHER                                             ELXPMCBP
02086               CONTINUE                                            ELXPMCBP
02087            END-EVALUATE                                           ELXPMCBP
02088         END-IF                                                    ELXPMCBP
02089      END-IF.                                                      ELXPMCBP
02090                                                                   ELXPMCBP
02091 ************************************************************      ELXPMCBP
02092 *                                                          *      ELXPMCBP
02093 *    EXTRACT ADDITIONAL INFORMATION FOR SELECTED BPIDS     *      ELXPMCBP
02094 *    FOR SUPPLEMENTAL MAJOR MEDICAL CONTRACTS ONLY         *      ELXPMCBP
02095 *                                                          *      ELXPMCBP
02096 ************************************************************      ELXPMCBP
02097                                                                   ELXPMCBP
02098  7700-EXTRCT-ADTNL-INFRMTN-MM.                                    ELXPMCBP
02099                                                                   ELXPMCBP
02100      MOVE BPLS-BENPROV (BPLS-INDEX) TO WS-BP-TEST-ID.             ELXPMCBP
02101                                                                   ELXPMCBP
02102      IF PMCI-INSTITUTIONAL                                        ELXPMCBP
02103      THEN                                                         ELXPMCBP
02104         IF PMCI-INPATIENT                                         ELXPMCBP
02105         THEN                                                      ELXPMCBP
02106            EVALUATE TRUE                                          ELXPMCBP
02107            WHEN WS-BP-II-DMR-REN                                  ELXPMCBP
02108               MOVE GCP-CERTFN-REQRM-IND TO WS-DMR-CERT-REQ-IND-MM ELXPMCBP
02109            WHEN WS-BP-II-DME-PUR                                  ELXPMCBP
02110               MOVE GCP-CERTFN-REQRM-IND TO WS-DME-CERT-REQ-IND-MM ELXPMCBP
02111            WHEN WS-BP-II-PVTA                                     ELXPMCBP
02112               MOVE GCP-PROVN-PRICING-METHD                        ELXPMCBP
02113                 TO WS-PVTA-PRIC-METH-MM                           ELXPMCBP
02114            WHEN WS-BP-II-PVTR                                     ELXPMCBP
02115               MOVE GCP-PROVN-PRICING-METHD                        ELXPMCBP
02116                 TO WS-PVTR-PRIC-METH-MM                           ELXPMCBP
02117            WHEN OTHER                                             ELXPMCBP
02118               CONTINUE                                            ELXPMCBP
02119            END-EVALUATE                                           ELXPMCBP
02120         ELSE                                                      ELXPMCBP
02121            EVALUATE TRUE                                          ELXPMCBP
02122            WHEN WS-BP-IO-DMR-REN                                  ELXPMCBP
02123               MOVE GCP-CERTFN-REQRM-IND TO WS-DMR-CERT-REQ-IND-MM ELXPMCBP
02124            WHEN WS-BP-IO-DME-PUR                                  ELXPMCBP
02125               MOVE GCP-CERTFN-REQRM-IND TO WS-DME-CERT-REQ-IND-MM ELXPMCBP
02126            WHEN OTHER                                             ELXPMCBP
02127               CONTINUE                                            ELXPMCBP
02128            END-EVALUATE                                           ELXPMCBP
02129         END-IF                                                    ELXPMCBP
02130      ELSE                                                         ELXPMCBP
02131         IF PMCI-INPATIENT                                         ELXPMCBP
02132         THEN                                                      ELXPMCBP
02133            EVALUATE TRUE                                          ELXPMCBP
02134            WHEN WS-BP-PI-DMR-REN                                  ELXPMCBP
02135               MOVE GCP-CERTFN-REQRM-IND TO WS-DMR-CERT-REQ-IND-MM ELXPMCBP
02136            WHEN WS-BP-PI-DME-PUR                                  ELXPMCBP
02137               MOVE GCP-CERTFN-REQRM-IND TO WS-DME-CERT-REQ-IND-MM ELXPMCBP
02138            WHEN OTHER                                             ELXPMCBP
02139               CONTINUE                                            ELXPMCBP
02140            END-EVALUATE                                           ELXPMCBP
02141         ELSE                                                      ELXPMCBP
02142            EVALUATE TRUE                                          ELXPMCBP
02143            WHEN WS-BP-PO-DMR-REN                                  ELXPMCBP
02144               MOVE GCP-CERTFN-REQRM-IND TO WS-DMR-CERT-REQ-IND-MM ELXPMCBP
02145            WHEN WS-BP-PO-DME-PUR                                  ELXPMCBP
02146               MOVE GCP-CERTFN-REQRM-IND TO WS-DME-CERT-REQ-IND-MM ELXPMCBP
02147            WHEN OTHER                                             ELXPMCBP
02148               CONTINUE                                            ELXPMCBP
02149            END-EVALUATE                                           ELXPMCBP
02150         END-IF                                                    ELXPMCBP
02151      END-IF.                                                      ELXPMCBP
02152                                                                   ELXPMCBP
02153 ************************************************************      ELXPMCBP
02154 *                                                          *      ELXPMCBP
02155 *    ELXPMCBP END OF PROGRAM                               *      ELXPMCBP
02156 *                                                          *      ELXPMCBP
02157 ************************************************************      ELXPMCBP
