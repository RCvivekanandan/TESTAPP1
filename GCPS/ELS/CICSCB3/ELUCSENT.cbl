00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUCSENT
00003  PROGRAM-ID.         ELUCSENT.                                       LV001
00004                                                                   ELUCSENT
00005  AUTHOR.             ANNE KEFFER-KING                             ELUCSENT
00006                      RICK BARILEAU.                               ELUCSENT
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSENT
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSENT
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSENT
00010                      233 N. MICHIGAN AVE                          ELUCSENT
00011                      CHICAGO, ILLINOIS 60601                      ELUCSENT
00012                                                                   ELUCSENT
00013  DATE-WRITTEN.       09-FEB-1988.                                 ELUCSENT
00014                                                                   ELUCSENT
00015  DATE-COMPILED.                                                   ELUCSENT
00016                                                                   ELUCSENT
00017  SECURITY.           COPYRIGHT 1986,                              ELUCSENT
00018                      HEALTH CARE SERVICE CORPORATION              ELUCSENT
00019      SKIP3                                                        ELUCSENT
00020  ENVIRONMENT DIVISION.                                            ELUCSENT
00021                                                                   ELUCSENT
00022  CONFIGURATION SECTION.                                           ELUCSENT
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSENT
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSENT
00025      EJECT                                                        ELUCSENT
00026 ******************************************************************ELUCSENT
00027 *                                                                *ELUCSENT
00028 *                      MAINTENANCE HISTORY                       *ELUCSENT
00029 *                                                                *ELUCSENT
00030 *  MOD     DATE     BY  DRPT                ACTION               *ELUCSENT
00031 * ----- ----------- --- ----- ---------------------------------- *ELUCSENT
00032 * 01.00 09-FEB-1988 AKK       CREATED                            *ELUCSENT
00033 *                                                                *ELUCSENT
00034 * 01.01 07-MAR-1988 REB       CLEANED UP CODE AND ADDED A LOT OF *ELUCSENT
00035 *                             LOGIC THAT WAS MISSING.            *ELUCSENT
00036 *                                                                *ELUCSENT
00037 * 01.02 18-MAR-1988 REB       FIXED THE LOGIC ANNE HAD FOR PPF   *ELUCSENT
00038 *                             HANDLING.                          *ELUCSENT
00039 *                                                                *ELUCSENT
00040 * 01.03 21-MAR-1988 REB       NINA NOW WANTS THE BEN-SCOPE-ID    *ELUCSENT
00041 *                             TRANSLATED INSTEAD OF CONT-SCOPE.  *ELUCSENT
00042 *                                                                *ELUCSENT
00043 * 01.04 24-MAR-1988 REB       USERS WOULD LIKE A MESSAGE STATING *ELUCSENT
00044 *                             THAT ADDITIONAL ACCUMULATORS ARE   *ELUCSENT
00045 *                             PRESENT WHEN MORE THAN ONE IS CODED*ELUCSENT
00046 *                             THIS WILL LIST THE PROVISIONS TOO. *ELUCSENT
00047 *                                                                *ELUCSENT
00048 * 01.05 28-MAR-1988 REB       VERBIAGE CHANGE TO ACCUMULATORS.   *ELUCSENT
00049 *                                                                *ELUCSENT
00050 * 01.06 29-MAR-1988 AKK       ADDED CODE TO SUPPRESS TIME RE-    *ELUCSENT
00051 *                             STRICTION SENTENCE FOR EMER AND    *ELUCSENT
00052 *                             EAER WHEN THEIR VALUES IN THE BEN- *ELUCSENT
00053 *                             EFIT PROVISION TABLE IS ZERO.      *ELUCSENT
00054 *                                                                *ELUCSENT
00055 * 01.07 04-APR-1988 REB       MOVED LOGIC FOR HANDLING TEXT FOR  *ELUCSENT
00056 *                             ADDITIONAL MAXIMUMS.               *ELUCSENT
00057 *                                                                *ELUCSENT
00058 * 01.08 04-APR-1988 AKK       CONSTRUCT PPM TYPE 5 SENTENCE HAD  *ELUCSENT
00059 *                             SLIGHT PROBLEM, FIXED IT.          *ELUCSENT
00060 *                                                                *ELUCSENT
00061 * 01.09 05-APR-1988 AKK       MADE CHANGE TO THE VERBIAGE OF THE *ELUCSENT
00062 *                             ASEND/DESEND PHRASE FOR ACL.       *ELUCSENT
00063 * 01.10 13-APR-1988 NAC       MODIFY TO COMPRESS AND UNSTRING ALL*ELUCSENT
00064 *                             ACCUM INFORMATION PER PROVISION    *ELUCSENT
00065 *                             GROUP.                             *ELUCSENT
00066 * 02.00 21-SEP-1988 NAC       MODIFY TO REFLECT ENHANCEMENTS MADE*ELUCSENT
00067 *                             TO STORAGE MANAGER.                *ELUCSENT
00068 * 02.01 23-NOV-1988 NAC       DISPLAY DOLLAR SIGN .              *ELUCSENT
00069 *                                                                *ELUCSENT
00070 * 02.02 18-JUL-1989 AKK       DESTRUCTED                         *ELUCSENT
00071 *                                                                *ELUCSENT
00072 * 02.03 19-JUL-1989 AKK       ADDED CODE TO HANDLE TEMP CHANGE   *ELUCSENT
00073 *                             TO PREVENT COINS TO PRINT ON UN-   *ELUCSENT
00074 *                             DESIRED SLOT NUMBER.               *ELUCSENT
00075 *                                                                *ELUCSENT
00076 * 02.04 13-OCT-1989 AKK       CHANGED CODE TO ACCOMODATE CHANGES *ELUCSENT
00077 *                             IN ELSCSBP AND ELSCSAD COPYBOOKS   *ELUCSENT
00078 *                             MOST OF THESE CHANGES ARE IN ACL.  *ELUCSENT
00079 *                             ABM 'Y' INDEX OCCURANCES INCREASED *ELUCSENT
00080 *                             TO 5.                              *ELUCSENT
00081 * 02.05 13-DEC-1989 AKK       INSERTED ADDITIONAL TEST FOR       *ELUCSENT
00082 *                             HANDLING 'SPECIAL-PPM-04'.  THE    *ELUCSENT
00083 *                             PLUS PERCENT SENTENCE SHOULD NOT   *ELUCSENT
00084 *                             PRINT UNLESS THE ADDITIONAL PERCENT*ELUCSENT
00085 *                             FIELD IS > 0.                      *ELUCSENT
00086 *02.06  12-APR-1990 AKK       ADDED HANDLING OF PPM 39 THRU 44   *ELUCSENT
00087 *                             AND ADDED TRANSLATION OF THE VALUE *ELUCSENT
00088 *                             QUALIFIER IN TYPE 2 A/D SENTENCE.  *ELUCSENT
00089 *02.07  17-APR-1990 AKK       ADDED CODE TO MOVE NAME AND PROVN- *ELUCSENT
00090 *                             PRICING-METHD TO ELSELOG TO BE     *ELUCSENT
00091 *                             PROCESSED WITH THE OTHER NOT FOUND *ELUCSENT
00092 *                             CODE VALUES.  THEY WILL BE TAGGED  *ELUCSENT
00093 *                             WITH ELUCSENT.                     *ELUCSENT
00094 *02.08  27-APR-1990 AKK       CORRECTED HANDLING OF NON-DOLLAR   *ELUCSENT
00095 *                             TYPES OF VALUE QUALIFIERS, I.E     *ELUCSENT
00096 *                             DAYS, VISITS ETC. ALSO CHANGED THE *ELUCSENT
00097 *                             PHRASING SLIGHTLY.                 *ELUCSENT
00098 *02.09  16-MAY-1991 GEM       ADDED CODE VALUE '46' TO PPM-TYPE-1*ELUCSENT
00099 *                                 (POINT OF SERVICE ALLOWANCE)   *ELUCSENT
00100 *02.09  30-MAY-1991 GEM       ADDED CODE VALUE '47' TO PPM-TYPE-1*ELUCSENT
00101 *                                 FOR 'N I GAS'.                 *ELUCSENT
00102 *02.10  14-OCT-1991 JPB       CHANGED NAME OF LG-C-S-PPM TO      *ELUCSENT
00103 *                                 LG-C-V-LOGIC.                  *ELUCSENT
00104 *02.11  28-MAY-1992 AKK       ADDED PPM TYPES 48 AND 49 ACES     *ELUCSENT
00105 *                                 FOR AMERITECH                  *ELUCSENT
00106 *02.12  21-OCT-1992 AKK       ADDED PPM TYPES 50.                *ELUCSENT
00107 *02.13   6-JUL-1993 JPB       ADDED PPM TYPE 51 SENTENCE AND     *ELUCSENT
00108 *                             LOGIC TO DISPLAY IT. ALSO ADDED    *ELUCSENT
00109 *                             33 TO PPM-TYPE-6 GROUP.            *ELUCSENT
00110 *02.14   8-NOV-1993 JPB       ADDED PPM TYPES 53-56 AND LOGIC TO *ELUCSENT
00111 *                             DISPLAY HARD-CODED PHRASES FROM    *ELUCSENT
00112 *                             WORKING-STORAGE.                   *ELUCSENT
00113 *02.15  21-APR-1994 AKK       ADDED PROVISION PRICING METHOD     *ELUCSENT
00114 *                             57, 58, 59 TO TYPE-1 SENTENCE.     *ELUCSENT
00115 *02.16  27-JUN-1994 AKK       ADDED PROVISION PRICING METHOD     *ELUCSENT
00116 *                             60, TO TYPE-1 SENTENCE.            *ELUCSENT
00117 *02.17  14-SEP-1994 AKK       ADDED PROVISION PRICING METHOD     *ELUCSENT
00118 *                             62.                                *ELUCSENT
00119 *                             HAVE NOT DONE 61 YET -- NOT AT     *ELUCSENT
00120 *                             PROD YET TO BE ADDED LATER.        *ELUCSENT
00121 *02.18  11-JUL-1995 AKK       ADDED PROVISION PRICING METHOD     *ELUCSENT
00122 *                             63.                                *ELUCSENT
00123 ******************************************************************ELUCSENT
00124                                                                   ELUCSENT
00125  DATA DIVISION.                                                   ELUCSENT
00126  WORKING-STORAGE SECTION.                                         ELUCSENT
00127  01  WS-SENTENCE-TABLE.                                           ELUCSENT
00128      03  WS-NBR-SENTENCES          PIC S9(04) COMP.               ELUCSENT
00129      03  WS-SENTENCES              OCCURS 15 TIMES                ELUCSENT
00130                                    INDEXED BY WS-Y-IDX            ELUCSENT
00131                                    PIC X(46).                     ELUCSENT
00132  01  WS-SWITCH.                                                   ELUCSENT
00133      05  WS-POINTER-SW             PIC X(01) VALUE SPACE.         ELUCSENT
00134          88  POINTER-FOUND                   VALUE 'Y'.           ELUCSENT
00135      05  WS-FIRST-PASS-SW          PIC X(01) VALUE SPACE.         ELUCSENT
00136          88  FIRST-PASS                      VALUE 'F'.           ELUCSENT
00137          88  ADDITIONAL-PASS                 VALUE 'A'.           ELUCSENT
00138      05  WS-SENTENCE-SW            PIC X(01) VALUE 'N'.           ELUCSENT
00139          88  TOPIC-SENTENCE-PRINTED          VALUE 'T'.           ELUCSENT
00140          88  NO-TOPIC-SENT-PRINTED           VALUE 'N'.           ELUCSENT
00141      05  WS-NOTDOLLAR-SW           PIC X(01) VALUE 'N'.           ELUCSENT
00142          88  NOT-DOLLARS                     VALUE 'D'.           ELUCSENT
00143          88  DOLLAR-AMT                      VALUE 'N'.           ELUCSENT
00144 *                                                                 ELUCSENT
00145  01  WS-WORK-AREAS.                                               ELUCSENT
00146      03  HOLD-IDX                  PIC S9(04) VALUE +0 COMP.      ELUCSENT
00147      03  WS-INDEX1                 PIC S9(04) VALUE +0 COMP.      ELUCSENT
00148      03  WS-ENTRY-SUB              PIC S9(04) VALUE +0 COMP.      ELUCSENT
00149      03  WS-SUBA                   PIC S9(04) VALUE +0 COMP.      ELUCSENT
00150      03  WS-SUBB                   PIC S9(04) VALUE +0 COMP.      ELUCSENT
00151      03  WS-CURRENT-IND            PIC X(02)  VALUE SPACES.       ELUCSENT
00152      03  WS-PREV-IND               PIC X(02)  VALUE SPACES.       ELUCSENT
00153      03  WS-DOLLAR-AMT             PIC $$9.99.                    ELUCSENT
00154      03  WS-PPF-VALUE-LIMIT        PIC 9(07)  VALUE ZEROS.        ELUCSENT
00155      03  WS-PPF-EDITED-LIMIT       REDEFINES WS-PPF-VALUE-LIMIT   ELUCSENT
00156                                    PIC Z(06)9.                    ELUCSENT
00157      03  WS-PPF-UNLIMITED-CK       REDEFINES WS-PPF-VALUE-LIMIT   ELUCSENT
00158                                    PIC X(07).                     ELUCSENT
00159          88  PPF-UNLIMITED-AMT                VALUE '9999999'.    ELUCSENT
00160 *                                                                 ELUCSENT
00161      03  WS-LIMIT-EDITED           PIC $$,$$$,$$9.99.             ELUCSENT
00162      03  WS-LIMIT-M                REDEFINES WS-LIMIT-EDITED      ELUCSENT
00163                                    PIC Z(11)V99.                  ELUCSENT
00164      03  WS-UNLIMITED-CK           REDEFINES WS-LIMIT-EDITED      ELUCSENT
00165                                    PIC X(13).                     ELUCSENT
00166          88  UNLIMITED-AMT                   VALUE                ELUCSENT
00167              '$9,999,999.00' '$9,999,999.99'.                     ELUCSENT
00168      03  WS-NONDOLLAR-UNLIMITED    PIC S9(07) COMP-3              ELUCSENT
00169                                              VALUE 9999999.       ELUCSENT
00170                                                                   ELUCSENT
00171      03  WS-LIMIT                  PIC Z(7)VZZ.                   ELUCSENT
00172 ***WS-LIMIT IS USED FORM NON NUMERIC VALUE QUALIFIERS LIKE DAYS   ELUCSENT
00173 ***VISITS ETC.                                                    ELUCSENT
00174 ***WS-VALUE-LIMIT-HOLD IS USED TO FIND OUT IF AN UNLIMITED VALUE  ELUCSENT
00175 ***HAS BEEN FOUND WHEN LOOKING AT A NON-DOLLAR VALUE VALUE        ELUCSENT
00176 ***QUALIFIER.  THE VALUE QUALIFIER IS DEFINED S9(7)V99 COMP-3     ELUCSENT
00177 ***WS-VALUE-HOLD-LMT IS ONLY CHECKING THE INTEGER PORTION OF      ELUCSENT
00178 ***VALUE QUALIFIER FOR 9'S BECAUSE OCCASIONALLY THE DECIMAL       ELUCSENT
00179 ***PORTIONS WILL BE 0'S NOT 9'S MAKING A CHECK FOR ALL 9'S        ELUCSENT
00180 ***DIFFICULT.                                                     ELUCSENT
00181      03  WS-VALUE-LMT-HOLD         PIC S9(07) COMP-3              ELUCSENT
00182                                              VALUE 0.             ELUCSENT
00183      03  WS-FLAT-AMT               PIC $$$,$$$.99.                ELUCSENT
00184      03  WS-EMERGENCY              PIC X(06).                     ELUCSENT
00185          88 EMERGENCY-PROVISIONS             VALUE 'EAER B'       ELUCSENT
00186                                              'EMER B' 'EAC  E'    ELUCSENT
00187                                              'EMC  E'.            ELUCSENT
00188          88 ACCIDENTAL-INJURY                VALUE 'EAER B'       ELUCSENT
00189                                                    'EAC  E'.      ELUCSENT
00190          88 MEDICAL-EMERGENCY                VALUE 'EMER B'       ELUCSENT
00191                                                    'EMC  E'.      ELUCSENT
00192 *                                                                 ELUCSENT
00193      03  WS-SUPPRESS-NBR           PIC ZZ9.                       ELUCSENT
00194 *                                                                 ELUCSENT
00195      03  WS-PERCENT-PHRASE.                                       ELUCSENT
00196          05  WS-PCT-VALUE          PIC ZZ9.                       ELUCSENT
00197          05  FILLER                PIC X(01)  VALUE '%'.          ELUCSENT
00198 *                                                                 ELUCSENT
00199      03  WS-PROVN-PRICING-METHD    PIC X(02)  VALUE SPACE.        ELUCSENT
00200          88  PPM-ZERO                         VALUE '00'.         ELUCSENT
00201          88  PPM-TYPE-1                       VALUE '18' '19'     ELUCSENT
00202                                               '20' '23' '24'      ELUCSENT
00203                                               '25' '26' '27'      ELUCSENT
00204                                               '28' '29' '30'      ELUCSENT
00205                                               '32' '36' '37'      ELUCSENT
00206                                               '38' '41' '46'      ELUCSENT
00207                                               '47' '48' '49'      ELUCSENT
00208                                               '50' '57' '58'      ELUCSENT
00209                                               '59' '60' '61'      ELUCSENT
00210                                               '62' '63' '64'      ELUCSENT
00211                                               '65' '66' '67'      ELUCSENT
00212                                               '68' '69'.          ELUCSENT
00213          88  PPM-TYPE-2                       VALUE '01' '02'     ELUCSENT
00214                                               '05' '10' '15'      ELUCSENT
00215                                               '31' '34' '39'      ELUCSENT
00216                                               '40' '42' '43'      ELUCSENT
00217                                               '44'.               ELUCSENT
00218          88  PPM-TYPE-3                       VALUE '03' '12'     ELUCSENT
00219                                               '13' '16' '17'.     ELUCSENT
00220          88  PPM-TYPE-4                       VALUE '08' '11'     ELUCSENT
00221                                               '35'.               ELUCSENT
00222          88  PPM-TYPE-5                       VALUE '04' '14'     ELUCSENT
00223                                               '21' '22'.          ELUCSENT
00224          88  PPM-TYPE-6                       VALUE '06' '07'     ELUCSENT
00225                                               '31' '33'.          ELUCSENT
00226          88  PPM-TYPE-09                      VALUE '09'.         ELUCSENT
00227          88  PPM-SPECIAL-04                   VALUE '14' '21'     ELUCSENT
00228                                               '22'.               ELUCSENT
00229          88  PPM-SPECIAL-06                   VALUE '06' '07'.    ELUCSENT
00230          88  PPM-21                           VALUE '21'.         ELUCSENT
00231          88  PPM-22                           VALUE '22'.         ELUCSENT
00232          88  PPM-33                           VALUE '33'.         ELUCSENT
00233          88  PPM-51                           VALUE '51'.         ELUCSENT
00234          88  PPM-53                           VALUE '53'.         ELUCSENT
00235          88  PPM-54                           VALUE '54'.         ELUCSENT
00236          88  PPM-55                           VALUE '55'.         ELUCSENT
00237          88  PPM-56                           VALUE '56'.         ELUCSENT
00238 *                                                                 ELUCSENT
00239 ***************************************************************** ELUCSENT
00240 *              HARD CODED SENTENCE AREA                         * ELUCSENT
00241 ***************************************************************** ELUCSENT
00242 *                                                                 ELUCSENT
00243  01  WS-SENTENCE-AREA.                                            ELUCSENT
00244      03  ERROR-MESSAGE              PIC X(46) VALUE               ELUCSENT
00245          'CONTRACT CODING ERROR'.                                 ELUCSENT
00246 *                                                                 ELUCSENT
00247      03  COVERED-SENTENCE           PIC X(46) VALUE 'COVERED'.    ELUCSENT
00248 *                                                                 ELUCSENT
00249      03  NOT-COVERED-SENTENCE       PIC X(46)                     ELUCSENT
00250      VALUE 'NOT COVERED'.                                         ELUCSENT
00251 *                                                                 ELUCSENT
00252      03  COVERED-ON-SUPP-SENTENCE   PIC X(46)                     ELUCSENT
00253      VALUE 'COVERED ON SUPPLEMENTAL'.                             ELUCSENT
00254 *                                                                 ELUCSENT
00255      03  CONTRACT-BENEFIT-SENTENCE  PIC X(46)                     ELUCSENT
00256      VALUE 'CONTRACT BENEFITS'.                                   ELUCSENT
00257 *                                                                 ELUCSENT
00258      03  UNLIMITED-PHRASE           PIC X(46) VALUE               ELUCSENT
00259      'TO UNLIMITED DOLLARS UP TO MAXIMUMS OR '.                   ELUCSENT
00260 *                                                                 ELUCSENT
00261      03  UNLIMITED-PHRASE-X         PIC X(46) VALUE               ELUCSENT
00262      'OUT-OF-POCKET LIMITS'.                                      ELUCSENT
00263 *                                                                 ELUCSENT
00264      03  PPM-PHRASE                 PIC X(46) VALUE               ELUCSENT
00265      'PROVISION PRICING METHOD '.                                 ELUCSENT
00266 *                                                                 ELUCSENT
00267      03  LESSER-OF-PHRASE           PIC X(10) VALUE               ELUCSENT
00268      'LESSER OF '.                                                ELUCSENT
00269 *                                                                 ELUCSENT
00270      03  MED-ACC-EMER-SENTENCE      PIC X(46) VALUE               ELUCSENT
00271      'BASED ON THE SUDDEN UNEXPECTED ONSET OF A '.                ELUCSENT
00272 *                                                                 ELUCSENT
00273      03  MED-ACC-EMER-SENTENCE-A     PIC X(46) VALUE              ELUCSENT
00274      'MEDICAL CONDITION REQUIRING IMMEDIATE MEDICAL '.            ELUCSENT
00275 *                                                                 ELUCSENT
00276      03  MED-ACC-EMER-SENTENCE-B     PIC X(46) VALUE              ELUCSENT
00277      'ATTENTION.'.                                                ELUCSENT
00278 *                                                                 ELUCSENT
00279      03  MEDICAL-EMER-TIME-PHRASE    PIC X(46) VALUE              ELUCSENT
00280      ' DAYS OF ONSET OF ILLNESS'.                                 ELUCSENT
00281 *                                                                 ELUCSENT
00282      03  ACCIDENTAL-EMER-TIME-PHRASE PIC X(46) VALUE              ELUCSENT
00283      ' DAYS OF AN ACCIDENT'.                                      ELUCSENT
00284 *                                                                 ELUCSENT
00285      03  UNDEFINED-SENTENCE         PIC X(35)  VALUE              ELUCSENT
00286      ' IS NOT DEFINED TO CONTRACT SUMMARY'.                       ELUCSENT
00287 *                                                                 ELUCSENT
00288      03  WS-NEED-MORE-AREA-MSG      PIC X(46)  VALUE              ELUCSENT
00289          'CONTACT ELS MORE SPACE NEEDED TO DISPLAY TEXT'.         ELUCSENT
00290 *                                                                 ELUCSENT
00291      03  WS-HOSP-MED-PYMT           PIC X(46)  VALUE              ELUCSENT
00292          'IN HOSPITAL MEDICAL PAYMENT '.                          ELUCSENT
00293 *                                                                 ELUCSENT
00294      03  WS-DED-DETERMINED-BY       PIC X(46)  VALUE              ELUCSENT
00295          'THE DEDUCTIBLE IS DETERMINED BY '.                      ELUCSENT
00296 *                                                                 ELUCSENT
00297      03  WS-PPO-SCHEDULE-PHRASE     PIC X(34)  VALUE              ELUCSENT
00298          ' OF PPO SCHEDULE OR BILLED AMOUNT.'.                    ELUCSENT
00299 *                                                                 ELUCSENT
00300      03  TOPIC-SENTENCE             PIC X(46)  VALUE              ELUCSENT
00301          'SEE COINSURANCE TOPIC FOR COINSURANCE INFO.'.           ELUCSENT
00302 *                                                                 ELUCSENT
00303      03  PPM-53-PHRASE1             PIC X(79)  VALUE              ELUCSENT
00304         'INDEMNITY SCHEDULE FOR SURGERY, BILLED AMOUNT FOR SURGERYELUCSENT
00305 -       ', BILLED AMOUNT FOR OT'.                                 ELUCSENT
00306      03  PPM-53-PHRASE2             PIC X(75)  VALUE              ELUCSENT
00307          'HER THAN SURGERY, UP TO MEDICARE REASONABLE (FOR ASSIGNMELUCSENT
00308 -        'ENT) OR MEDICARE '.                                     ELUCSENT
00309      03  PPM-53-PHRASE3             PIC X(38)  VALUE              ELUCSENT
00310          'MAXIMUM ALLOWANCE (FOR NON-ASSIGNMENT)'.                ELUCSENT
00311 *                                                                 ELUCSENT
00312      03  PPM-54-PHRASE1             PIC X(79)  VALUE              ELUCSENT
00313          '150% OF U & C, OPERATOR MUST DETERMINE PAYMENT IF 150% OELUCSENT
00314 -        'F U & C IS LESS THAN '.                                 ELUCSENT
00315 *                                                                 ELUCSENT
00316      03  PPM-54-PHRASE2             PIC X(14)  VALUE              ELUCSENT
00317          'BILLED AMOUNT '.                                        ELUCSENT
00318 *                                                                 ELUCSENT
00319      03  PPM-55-PHRASE              PIC X(35)  VALUE              ELUCSENT
00320          'U & C FOR PAR AND OUT-OF-AREA HOST '.                   ELUCSENT
00321 *                                                                 ELUCSENT
00322      03  PPM-56-PHRASE              PIC X(42)  VALUE              ELUCSENT
00323          'PPO SCHEDULE FOR PAR AND OUT-OF-AREA HOST '.            ELUCSENT
00324 *                                                                 ELUCSENT
00325  01  PROGRAM-CONSTANTS.                                           ELUCSENT
00326      03  PC-ABM                     PIC X(06)  VALUE '#ABM  '.    ELUCSENT
00327      03  PC-ADL                     PIC X(06)  VALUE '#ADL  '.    ELUCSENT
00328      03  PC-ACL                     PIC X(06)  VALUE '#ACL  '.    ELUCSENT
00329      03  PC-COLON                   PIC X(02)  VALUE ':'.         ELUCSENT
00330      03  PC-EMER-DAYS               PIC S9(03) VALUE +999  COMP-3.ELUCSENT
00331      03  PC-PPF                     PIC X(06)  VALUE '#PPF  '.    ELUCSENT
00332      03  PC-DOLLARS                 PIC X(01)  VALUE '5'.         ELUCSENT
00333 *                                                                 ELUCSENT
00334  LINKAGE SECTION.                                                 ELUCSENT
00335  01  DFHCOMMAREA.                                                 ELUCSENT
00336      COPY ELSCOMMC.                                               ELUCSENT
00337 /                                                                 ELUCSENT
00338      COPY ELSCIA2C.                                               ELUCSENT
00339 /                                                                 ELUCSENT
00340      COPY ELSELOGC.                                               ELUCSENT
00341 /                                                                 ELUCSENT
00342      COPY ELSSSCBC.                                               ELUCSENT
00343 /                                                                 ELUCSENT
00344      COPY ELSIOPMC.                                               ELUCSENT
00345 /                                                                 ELUCSENT
00346      COPY ELSKEYSC.                                               ELUCSENT
00347 /                                                                 ELUCSENT
00348      COPY ELSTCWAC.                                               ELUCSENT
00349 /                                                                 ELUCSENT
00350      COPY ELSCMIFC.                                               ELUCSENT
00351 /                                                                 ELUCSENT
00352      COPY ELSCMDSC.                                               ELUCSENT
00353 /    CONTRACT SUMMARY POINTER TABLE                               ELUCSENT
00354      COPY ELSCSPTC.                                               ELUCSENT
00355 /    CONTRACT SUMMARY BENEFIT PROVISION TABLE                     ELUCSENT
00356      COPY ELSCSBPC.                                               ELUCSENT
00357 /    CONTRACT SUMMARY ASCEND DESCEND TABLE                        ELUCSENT
00358      COPY ELSCSADC.                                               ELUCSENT
00359 /    CONTRACT SUMMARY SENTENCE TABLE                              ELUCSENT
00360      COPY ELSCSENC.                                               ELUCSENT
00361 /                                                                 ELUCSENT
00362  01  PAYMENT-FACTOR-RECORD.                                       ELUCSENT
00363      COPY GCTPPFC.                                                ELUCSENT
00364 /                                                                 ELUCSENT
00365  01  GROUP-SPECIFIC-RECORD.                                       ELUCSENT
00366      COPY GCGROUPC.                                               ELUCSENT
00367      EJECT                                                        ELUCSENT
00368  PROCEDURE DIVISION.                                              ELUCSENT
00369 ************************************************************      ELUCSENT
00370 *                                                          *      ELUCSENT
00371 *                    PROCEDURE DIVISION                    *      ELUCSENT
00372 *                                                          *      ELUCSENT
00373 ************************************************************      ELUCSENT
00374                                                                   ELUCSENT
00375                                                                   ELUCSENT
00376 ************************************************************      ELUCSENT
00377 *                                                          *      ELUCSENT
00378 *        CONTRACT SUMMARY SENTENCE UTILITY                 *      ELUCSENT
00379 *                                                          *      ELUCSENT
00380 ************************************************************      ELUCSENT
00381  CONTRACT-SUMMARY-SENTENCE-UTIL.                                  ELUCSENT
00382      PERFORM INITIALIZATION.                                      ELUCSENT
00383      PERFORM PROCESS-SENTENCE-UTILITY.                            ELUCSENT
00384      GOBACK.                                                      ELUCSENT
00385                                                                   ELUCSENT
00386                                                                   ELUCSENT
00387 ************************************************************      ELUCSENT
00388 *                                                          *      ELUCSENT
00389 *        INITIALIZATION                                    *      ELUCSENT
00390 *                                                          *      ELUCSENT
00391 ************************************************************      ELUCSENT
00392  INITIALIZATION.                                                  ELUCSENT
00393      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELUCSENT
00394      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELUCSENT
00395      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELUCSENT
00396      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELUCSENT
00397      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELUCSENT
00398      PERFORM ESTABLISH-ADDRESS-OF-CS-PTR.                         ELUCSENT
00399      PERFORM ESTABLISH-ADDRESS-OF-A-D.                            ELUCSENT
00400      PERFORM ESTABLISH-ADDRESS-OF-LOG-AREA.                       ELUCSENT
00401      PERFORM FIND-BENEFIT-PROVISION-TABLE-T                       ELUCSENT
00402          VARYING CSPT-IDX FROM +1 BY +1                           ELUCSENT
00403                  UNTIL   CSPT-IDX > CSPT-TBL-CNT                  ELUCSENT
00404                  OR      POINTER-FOUND.                           ELUCSENT
00405      IF NOT POINTER-FOUND                                         ELUCSENT
00406         SET CIA-AB-ARG-NOTFND TO TRUE                             ELUCSENT
00407         PERFORM SIGNAL-ABEND.                                     ELUCSENT
00408                                                                   ELUCSENT
00409                                                                   ELUCSENT
00410 ************************************************************      ELUCSENT
00411 *                                                          *      ELUCSENT
00412 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELUCSENT
00413 *                                                          *      ELUCSENT
00414 ************************************************************      ELUCSENT
00415  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELUCSENT
00416      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELUCSENT
00417      PERFORM ESTABLISH-ADDRESSABILITY-OF-CI.                      ELUCSENT
00418      PERFORM ESTABLISH-ADDRESSABILITY-OF-EL.                      ELUCSENT
00419                                                                   ELUCSENT
00420                                                                   ELUCSENT
00421 ************************************************************      ELUCSENT
00422 *                                                          *      ELUCSENT
00423 *        CHECK FOR VALID COMMAREA                          *      ELUCSENT
00424 *                                                          *      ELUCSENT
00425 ************************************************************      ELUCSENT
00426  CHECK-FOR-VALID-COMMAREA.                                        ELUCSENT
00427      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUCSENT
00428          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSENT
00429                                                                   ELUCSENT
00430                                                                   ELUCSENT
00431 ************************************************************      ELUCSENT
00432 *                                                          *      ELUCSENT
00433 *        SIGNAL INVALID COMMAREA                           *      ELUCSENT
00434 *                                                          *      ELUCSENT
00435 ************************************************************      ELUCSENT
00436  SIGNAL-INVALID-COMMAREA.                                         ELUCSENT
00437      EXEC CICS ABEND                                              ELUCSENT
00438                ABCODE ('EL01')                                    ELUCSENT
00439                END-EXEC.                                          ELUCSENT
00440                                                                   ELUCSENT
00441                                                                   ELUCSENT
00442 ************************************************************      ELUCSENT
00443 *                                                          *      ELUCSENT
00444 *        ESTABLISH ADDRESSABILITY OF CIA                   *      ELUCSENT
00445 *                                                          *      ELUCSENT
00446 ************************************************************      ELUCSENT
00447  ESTABLISH-ADDRESSABILITY-OF-CI.                                  ELUCSENT
00448      IF ECA-CIA-PTR = NULL                                        ELUCSENT
00449          PERFORM SIGNAL-INVALID-CIA.                              ELUCSENT
00450      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSENT
00451                      ADDRESS OF                                   ELUCSENT
00452          CIA-ELS-COMMON-INTERFACE-AREA.                           ELUCSENT
00453                                                                   ELUCSENT
00454                                                                   ELUCSENT
00455 ************************************************************      ELUCSENT
00456 *                                                          *      ELUCSENT
00457 *        SIGNAL INVALID CIA                                *      ELUCSENT
00458 *                                                          *      ELUCSENT
00459 ************************************************************      ELUCSENT
00460  SIGNAL-INVALID-CIA.                                              ELUCSENT
00461      EXEC CICS ABEND                                              ELUCSENT
00462                ABCODE ('EL02')                                    ELUCSENT
00463                END-EXEC.                                          ELUCSENT
00464                                                                   ELUCSENT
00465                                                                   ELUCSENT
00466 ************************************************************      ELUCSENT
00467 *                                                          *      ELUCSENT
00468 *        ESTABLISH ADDRESSABILITY OF ELSSSCB               *      ELUCSENT
00469 *                                                          *      ELUCSENT
00470 ************************************************************      ELUCSENT
00471  ESTABLISH-ADDRESSABILITY-OF-EL.                                  ELUCSENT
00472      SET  CIA-ELSSSCB-DDN   TO TRUE.                              ELUCSENT
00473      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00474                      ADDRESS OF                                   ELUCSENT
00475          SSB-SELECTOR-STATUS-CTL-BLK.                             ELUCSENT
00476      IF CIA-RC-PTR-NULL                                           ELUCSENT
00477          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00478                                                                   ELUCSENT
00479                                                                   ELUCSENT
00480 ************************************************************      ELUCSENT
00481 *                                                          *      ELUCSENT
00482 *        SIGNAL UNALLOCATED AREA ERROR                     *      ELUCSENT
00483 *                                                          *      ELUCSENT
00484 ************************************************************      ELUCSENT
00485  SIGNAL-UNALLOCATED-AREA-ERROR.                                   ELUCSENT
00486      SET  CIA-AB-UNALLOC-AREA  TO TRUE.                           ELUCSENT
00487      PERFORM SIGNAL-ABEND.                                        ELUCSENT
00488                                                                   ELUCSENT
00489                                                                   ELUCSENT
00490 ************************************************************      ELUCSENT
00491 *                                                          *      ELUCSENT
00492 *        SIGNAL ABEND                                      *      ELUCSENT
00493 *                                                          *      ELUCSENT
00494 ************************************************************      ELUCSENT
00495  SIGNAL-ABEND.                                                    ELUCSENT
00496      EXEC CICS ABEND                                              ELUCSENT
00497                ABCODE (CIA-ABCODE)                                ELUCSENT
00498                END-EXEC.                                          ELUCSENT
00499                                                                   ELUCSENT
00500                                                                   ELUCSENT
00501 ************************************************************      ELUCSENT
00502 *                                                          *      ELUCSENT
00503 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELUCSENT
00504 *                                                          *      ELUCSENT
00505 ************************************************************      ELUCSENT
00506  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELUCSENT
00507      SET  CIA-ELSGRPSP-DDN  TO TRUE.                              ELUCSENT
00508      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00509                      ADDRESS OF GROUP-SPECIFIC-RECORD.            ELUCSENT
00510      IF CIA-RC-PTR-NULL                                           ELUCSENT
00511          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00512                                                                   ELUCSENT
00513                                                                   ELUCSENT
00514 ************************************************************      ELUCSENT
00515 *                                                          *      ELUCSENT
00516 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELUCSENT
00517 *                                                          *      ELUCSENT
00518 ************************************************************      ELUCSENT
00519  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELUCSENT
00520      SET  CIA-ELSKEYS-DDN   TO TRUE.                              ELUCSENT
00521      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00522                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELUCSENT
00523      IF CIA-RC-PTR-NULL                                           ELUCSENT
00524          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00525                                                                   ELUCSENT
00526                                                                   ELUCSENT
00527 ************************************************************      ELUCSENT
00528 *                                                          *      ELUCSENT
00529 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELUCSENT
00530 *                                                          *      ELUCSENT
00531 ************************************************************      ELUCSENT
00532  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELUCSENT
00533      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELUCSENT
00534      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00535                      ADDRESS OF                                   ELUCSENT
00536          CMF-CODES-MANUAL-INTERFACE.                              ELUCSENT
00537      IF CIA-RC-PTR-NULL                                           ELUCSENT
00538          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00539                                                                   ELUCSENT
00540                                                                   ELUCSENT
00541 ************************************************************      ELUCSENT
00542 *                                                          *      ELUCSENT
00543 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION      *      ELUCSENT
00544 *                                                          *      ELUCSENT
00545 ************************************************************      ELUCSENT
00546  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELUCSENT
00547      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELUCSENT
00548      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00549                      ADDRESS OF                                   ELUCSENT
00550          TCAR-COMPRESSION-WORK-AREA.                              ELUCSENT
00551      IF CIA-RC-PTR-NULL                                           ELUCSENT
00552          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00553                                                                   ELUCSENT
00554                                                                   ELUCSENT
00555 ************************************************************      ELUCSENT
00556 *                                                          *      ELUCSENT
00557 *        ESTABLISH ADDRESS OF CS POINTER TABLE             *      ELUCSENT
00558 *                                                          *      ELUCSENT
00559 ************************************************************      ELUCSENT
00560  ESTABLISH-ADDRESS-OF-CS-PTR.                                     ELUCSENT
00561      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELUCSENT
00562      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00563                      ADDRESS OF CSPT-POINTER-LIST.                ELUCSENT
00564      IF CIA-RC-PTR-NULL                                           ELUCSENT
00565          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00566                                                                   ELUCSENT
00567 ************************************************************      ELUCSENT
00568 *                                                          *      ELUCSENT
00569 *        ESTABLISH ADDRESS OF ASEND DESCEND TABLE          *      ELUCSENT
00570 *A NULL POINTER WILL NOT CAUSE AN ABEND BECAUSE ELUCSACL   *      ELUCSENT
00571 *PROGRAM WILL NOT ESTABLISH THIS TABLE UNLESS AN ASCEND/DE-*      ELUCSENT
00572 *SEND SITUATION EXISTS.                                    *      ELUCSENT
00573 ************************************************************      ELUCSENT
00574  ESTABLISH-ADDRESS-OF-A-D.                                        ELUCSENT
00575      SET CIA-ELSCSADC-DDN TO TRUE.                                ELUCSENT
00576      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00577                      ADDRESS OF CSAD-ACL-TABLE.                   ELUCSENT
00578      IF CIA-RC-PTR-NULL                                           ELUCSENT
00579          CONTINUE.                                                ELUCSENT
00580                                                                   ELUCSENT
00581 ************************************************************      ELUCSENT
00582 *                                                          *      ELUCSENT
00583 *        ESTABLISH ADDRESS OF LOG AREA                     *      ELUCSENT
00584 *                                                          *      ELUCSENT
00585 *                                                          *      ELUCSENT
00586 ************************************************************      ELUCSENT
00587  ESTABLISH-ADDRESS-OF-LOG-AREA.                                   ELUCSENT
00588      SET CIA-ELSELOG-DDN TO TRUE.                                 ELUCSENT
00589      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00590                      ADDRESS OF LG-LOG-RECORD.                    ELUCSENT
00591      IF CIA-RC-PTR-NULL                                           ELUCSENT
00592          PERFORM ALLOCATE-LOG-REC-AREA                            ELUCSENT
00593      ELSE                                                         ELUCSENT
00594      IF NOT CIA-RC-OK                                             ELUCSENT
00595          PERFORM SIGNAL-LOGIC-ERROR.                              ELUCSENT
00596                                                                   ELUCSENT
00597 ************************************************************      ELUCSENT
00598 *                                                          *      ELUCSENT
00599 *        ALLOCATE-LOG-REC-AREA                             *      ELUCSENT
00600 *                                                          *      ELUCSENT
00601 ************************************************************      ELUCSENT
00602  ALLOCATE-LOG-REC-AREA.                                           ELUCSENT
00603      COMPUTE CIA-AREA-LEN = LENGTH OF LG-LOG-RECORD               ELUCSENT
00604      SET CIA-STG-GETMAIN TO TRUE.                                 ELUCSENT
00605      PERFORM LINK-TO-STORAGE-MANAGER.                             ELUCSENT
00606      SET CIA-ELSELOG-DDN TO TRUE.                                 ELUCSENT
00607      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00608                      ADDRESS OF LG-LOG-RECORD.                    ELUCSENT
00609                                                                   ELUCSENT
00610                                                                   ELUCSENT
00611 ************************************************************      ELUCSENT
00612 *                                                          *      ELUCSENT
00613 *        SIGNAL LOGIC ERROR                                *      ELUCSENT
00614 *                                                          *      ELUCSENT
00615 ************************************************************      ELUCSENT
00616  SIGNAL-LOGIC-ERROR.                                              ELUCSENT
00617      SET CIA-AB-UNDEF TO TRUE.                                    ELUCSENT
00618      PERFORM SIGNAL-ABEND.                                        ELUCSENT
00619                                                                   ELUCSENT
00620                                                                   ELUCSENT
00621 ************************************************************      ELUCSENT
00622 *                                                          *      ELUCSENT
00623 *        FIND BENEFIT PROVISION TABLE TO PROCESS           *      ELUCSENT
00624 *                                                          *      ELUCSENT
00625 ************************************************************      ELUCSENT
00626  FIND-BENEFIT-PROVISION-TABLE-T.                                  ELUCSENT
00627      IF PROCESS-SUBTOPIC (CSPT-IDX)                               ELUCSENT
00628          PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                  ELUCSENT
00629                                                                   ELUCSENT
00630                                                                   ELUCSENT
00631 ************************************************************      ELUCSENT
00632 *                                                          *      ELUCSENT
00633 *        ESTABLISH ADDRESSABILITY OF CS BENEFIT PROVISION T*      ELUCSENT
00634 *                                                          *      ELUCSENT
00635 ************************************************************      ELUCSENT
00636  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELUCSENT
00637      IF CSPT-BP-TBL-PTR (CSPT-IDX) = NULL                         ELUCSENT
00638          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR                    ELUCSENT
00639      ELSE                                                         ELUCSENT
00640          PERFORM ESTABLISH-ADDRESS-OF-CSBP.                       ELUCSENT
00641      SET POINTER-FOUND TO TRUE.                                   ELUCSENT
00642                                                                   ELUCSENT
00643                                                                   ELUCSENT
00644 ************************************************************      ELUCSENT
00645 *                                                          *      ELUCSENT
00646 *        ESTABLISH ADDRESS OF CSBP                         *      ELUCSENT
00647 *                                                          *      ELUCSENT
00648 ************************************************************      ELUCSENT
00649  ESTABLISH-ADDRESS-OF-CSBP.                                       ELUCSENT
00650      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE                  ELUCSENT
00651                                  TO CSPT-BP-TBL-PTR               ELUCSENT
00652          (CSPT-IDX).                                              ELUCSENT
00653                                                                   ELUCSENT
00654                                                                   ELUCSENT
00655 ************************************************************      ELUCSENT
00656 *                                                          *      ELUCSENT
00657 *        PROCESS SENTENCE UTILITY                          *      ELUCSENT
00658 *                                                          *      ELUCSENT
00659 ************************************************************      ELUCSENT
00660  PROCESS-SENTENCE-UTILITY.                                        ELUCSENT
00661      SET CSBP-X-IDX TO 1.                                         ELUCSENT
00662      PERFORM CONSTRUCT-SENTENCES-FOR-EACH-B                       ELUCSENT
00663          VARYING WS-SUBA FROM 1 BY 1                              ELUCSENT
00664                      UNTIL WS-SUBA > CSBP-TBL-CNT.                ELUCSENT
00665                                                                   ELUCSENT
00666                                                                   ELUCSENT
00667 ************************************************************      ELUCSENT
00668 *                                                          *      ELUCSENT
00669 *        CONSTRUCT SENTENCES FOR EACH BEN PROV ENTRY       *      ELUCSENT
00670 *                                                          *      ELUCSENT
00671 ************************************************************      ELUCSENT
00672  CONSTRUCT-SENTENCES-FOR-EACH-B.                                  ELUCSENT
00673      IF CSBP-PAYMENT-LVL (CSBP-X-IDX) = CSBP-X-IDX                ELUCSENT
00674          PERFORM DO-SENTENCE-CONSTRUCTION-FOR-E.                  ELUCSENT
00675      SET CSBP-X-IDX UP BY 1.                                      ELUCSENT
00676                                                                   ELUCSENT
00677                                                                   ELUCSENT
00678 ************************************************************      ELUCSENT
00679 *                                                          *      ELUCSENT
00680 *        DO SENTENCE CONSTRUCTION FOR EACH PAYMENT LEVEL GR*      ELUCSENT
00681 *                                                          *      ELUCSENT
00682 ************************************************************      ELUCSENT
00683  DO-SENTENCE-CONSTRUCTION-FOR-E.                                  ELUCSENT
00684      PERFORM INITIALIZE-COMPRESSION-AREA.                         ELUCSENT
00685      INITIALIZE WS-NBR-SENTENCES                                  ELUCSENT
00686                 WS-SENTENCE-TABLE                                 ELUCSENT
00687                 HOLD-IDX.                                         ELUCSENT
00688      SET WS-Y-IDX TO 1.                                           ELUCSENT
00689      IF CSBP-NO-PAYMENT-REQUESTED (CSBP-X-IDX)  OR                ELUCSENT
00690                   CSBP-NOT-COVERED (CSBP-X-IDX)                   ELUCSENT
00691          PERFORM CONSTRUCT-COVERAGE-ONLY-SENTEN                   ELUCSENT
00692      ELSE                                                         ELUCSENT
00693          PERFORM CONSTRUCT-PAYMENT-SENTENCES.                     ELUCSENT
00694      PERFORM CREATE-SENTENCE-TABLE-FOR-LINK.                      ELUCSENT
00695                                                                   ELUCSENT
00696                                                                   ELUCSENT
00697 ************************************************************      ELUCSENT
00698 *                                                          *      ELUCSENT
00699 *        INITIALIZE COMPRESSION AREA                       *      ELUCSENT
00700 *                                                          *      ELUCSENT
00701 ************************************************************      ELUCSENT
00702  INITIALIZE-COMPRESSION-AREA.                                     ELUCSENT
00703      INITIALIZE TCAR-FROM-AREA                                    ELUCSENT
00704                 TCAR-FROM-LENGTH                                  ELUCSENT
00705                 TCAR-FROM-SUB                                     ELUCSENT
00706                 TCAR-X.                                           ELUCSENT
00707                                                                   ELUCSENT
00708                                                                   ELUCSENT
00709 ************************************************************      ELUCSENT
00710 *                                                          *      ELUCSENT
00711 *        CREATE SENTENCE TABLE FOR LINKAGE                 *      ELUCSENT
00712 *                                                          *      ELUCSENT
00713 ************************************************************      ELUCSENT
00714  CREATE-SENTENCE-TABLE-FOR-LINK.                                  ELUCSENT
00715      SET CIA-ELSCSENC-DDN TO TRUE.                                ELUCSENT
00716      COMPUTE CIA-AREA-LEN = LENGTH OF CSEN-NBR-SENTENCES          ELUCSENT
00717          +                                                        ELUCSENT
00718              (WS-NBR-SENTENCES * LENGTH OF                        ELUCSENT
00719          CSEN-SENTENCES).                                         ELUCSENT
00720      SET CIA-STG-GETMAIN                TO TRUE.                  ELUCSENT
00721      PERFORM LINK-TO-STORAGE-MANAGER.                             ELUCSENT
00722      SET CIA-ELSCSENC-DDN TO TRUE.                                ELUCSENT
00723      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
00724                      ADDRESS OF CSEN-SENTENCE-TABLE.              ELUCSENT
00725      IF CIA-RC-PTR-NULL                                           ELUCSENT
00726          PERFORM SIGNAL-UNALLOCATED-AREA-ERROR.                   ELUCSENT
00727      MOVE WS-NBR-SENTENCES              TO                        ELUCSENT
00728          CSEN-NBR-SENTENCES.                                      ELUCSENT
00729      PERFORM DO-SENTENCE-MOVE-TO-LINKAGE.                         ELUCSENT
00730      SET CSBP-SENTENCE-PTR (CSBP-X-IDX) TO                        ELUCSENT
00731                      ADDRESS OF CSEN-SENTENCE-TABLE.              ELUCSENT
00732                                                                   ELUCSENT
00733                                                                   ELUCSENT
00734 ************************************************************      ELUCSENT
00735 *                                                          *      ELUCSENT
00736 *        LINK TO STORAGE MANAGER                           *      ELUCSENT
00737 *                                                          *      ELUCSENT
00738 ************************************************************      ELUCSENT
00739  LINK-TO-STORAGE-MANAGER.                                         ELUCSENT
00740      EXEC CICS LINK                                               ELUCSENT
00741                PROGRAM ('ELUSTGMG')                               ELUCSENT
00742                COMMAREA (DFHCOMMAREA)                             ELUCSENT
00743                END-EXEC.                                          ELUCSENT
00744                                                                   ELUCSENT
00745                                                                   ELUCSENT
00746 ************************************************************      ELUCSENT
00747 *                                                          *      ELUCSENT
00748 *        DO SENTENCE MOVE TO LINKAGE                       *      ELUCSENT
00749 *                                                          *      ELUCSENT
00750 ************************************************************      ELUCSENT
00751  DO-SENTENCE-MOVE-TO-LINKAGE.                                     ELUCSENT
00752      SET CSEN-Y-IDX        TO +1.                                 ELUCSENT
00753      PERFORM COMPLETE-SENTENCE-MOVE-TO-LINK                       ELUCSENT
00754          VARYING WS-Y-IDX FROM 1 BY 1                             ELUCSENT
00755                    UNTIL   WS-Y-IDX > WS-NBR-SENTENCES.           ELUCSENT
00756                                                                   ELUCSENT
00757                                                                   ELUCSENT
00758 ************************************************************      ELUCSENT
00759 *                                                          *      ELUCSENT
00760 *        COMPLETE SENTENCE MOVE TO LINKAGE                 *      ELUCSENT
00761 *                                                          *      ELUCSENT
00762 ************************************************************      ELUCSENT
00763  COMPLETE-SENTENCE-MOVE-TO-LINK.                                  ELUCSENT
00764      MOVE WS-SENTENCES (WS-Y-IDX) TO CSEN-SENTENCES               ELUCSENT
00765          (CSEN-Y-IDX).                                            ELUCSENT
00766      SET  CSEN-Y-IDX UP BY +1.                                    ELUCSENT
00767                                                                   ELUCSENT
00768                                                                   ELUCSENT
00769 ************************************************************      ELUCSENT
00770 *                                                          *      ELUCSENT
00771 *        CONSTRUCT COVERAGE ONLY SENTENCES                 *      ELUCSENT
00772 *                                                          *      ELUCSENT
00773 ************************************************************      ELUCSENT
00774  CONSTRUCT-COVERAGE-ONLY-SENTEN.                                  ELUCSENT
00775      IF CSBP-COVERED (CSBP-X-IDX)                                 ELUCSENT
00776          PERFORM CONSTRUCT-COVERED-SENTENCE                       ELUCSENT
00777      ELSE IF CSBP-NOT-COVERED (CSBP-X-IDX)                        ELUCSENT
00778          PERFORM CONSTRUCT-NOT-COVERED-SENTENCE                   ELUCSENT
00779      ELSE                                                         ELUCSENT
00780          PERFORM CONSTRUCT-COVERED-ON-SUPP-SENT.                  ELUCSENT
00781      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
00782                                                                   ELUCSENT
00783                                                                   ELUCSENT
00784 ************************************************************      ELUCSENT
00785 *                                                          *      ELUCSENT
00786 *        CONSTRUCT COVERED SENTENCE                        *      ELUCSENT
00787 *                                                          *      ELUCSENT
00788 ************************************************************      ELUCSENT
00789  CONSTRUCT-COVERED-SENTENCE.                                      ELUCSENT
00790      MOVE COVERED-SENTENCE TO WS-SENTENCES (WS-Y-IDX).            ELUCSENT
00791      ADD 1                 TO WS-NBR-SENTENCES.                   ELUCSENT
00792      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
00793                                                                   ELUCSENT
00794                                                                   ELUCSENT
00795 ************************************************************      ELUCSENT
00796 *                                                          *      ELUCSENT
00797 *        CONSTRUCT NOT COVERED SENTENCE                    *      ELUCSENT
00798 *                                                          *      ELUCSENT
00799 ************************************************************      ELUCSENT
00800  CONSTRUCT-NOT-COVERED-SENTENCE.                                  ELUCSENT
00801      MOVE NOT-COVERED-SENTENCE TO WS-SENTENCES                    ELUCSENT
00802          (WS-Y-IDX).                                              ELUCSENT
00803      ADD 1                     TO WS-NBR-SENTENCES.               ELUCSENT
00804      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
00805                                                                   ELUCSENT
00806                                                                   ELUCSENT
00807 ************************************************************      ELUCSENT
00808 *                                                          *      ELUCSENT
00809 *        CONSTRUCT COVERED ON SUPP SENTENCE                *      ELUCSENT
00810 *                                                          *      ELUCSENT
00811 ************************************************************      ELUCSENT
00812  CONSTRUCT-COVERED-ON-SUPP-SENT.                                  ELUCSENT
00813      MOVE COVERED-ON-SUPP-SENTENCE TO WS-SENTENCES (WS-Y-IDX).    ELUCSENT
00814      ADD 1                         TO WS-NBR-SENTENCES.           ELUCSENT
00815      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
00816                                                                   ELUCSENT
00817                                                                   ELUCSENT
00818 ************************************************************      ELUCSENT
00819 *                                                          *      ELUCSENT
00820 *        CONSTRUCT PAYMENT SENTENCES                       *      ELUCSENT
00821 *                                                          *      ELUCSENT
00822 ************************************************************      ELUCSENT
00823  CONSTRUCT-PAYMENT-SENTENCES.                                     ELUCSENT
00824      IF CSBP-COVERED (CSBP-X-IDX) OR                              ELUCSENT
00825                 CSBP-COVERED-ON-SUPP (CSBP-X-IDX)                 ELUCSENT
00826          PERFORM DO-PAYMENT-SENTENCE-CONSTRUCTI.                  ELUCSENT
00827                                                                   ELUCSENT
00828                                                                   ELUCSENT
00829 ************************************************************      ELUCSENT
00830 *                                                          *      ELUCSENT
00831 *        DO PAYMENT SENTENCE CONSTRUCTION                  *      ELUCSENT
00832 *                                                          *      ELUCSENT
00833 ************************************************************      ELUCSENT
00834  DO-PAYMENT-SENTENCE-CONSTRUCTI.                                  ELUCSENT
00835      IF CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) NOT = SPACES        ELUCSENT
00836          AND                                                      ELUCSENT
00837                LOW-VALUES                                         ELUCSENT
00838          PERFORM CONSTRUCT-COINSURANCE-SENTENCE.                  ELUCSENT
00839      PERFORM CONSTRUCT-MAXIMUM-SENTENCES                          ELUCSENT
00840          VARYING CSBP-Y-IDX FROM 1 BY 1                           ELUCSENT
00841                  UNTIL   CSBP-Y-IDX > 5.                          ELUCSENT
00842 *******************************************                       ELUCSENT
00843 * THIS IS DONE WHEN THE CSBP-Y-IDX IS TWO *                       ELUCSENT
00844 * BECAUSE WE KNOW WE HAVE GONE THROUGH    *                       ELUCSENT
00845 * BOTH OCCURENCES FOR ABM AND THE TEXT    *                       ELUCSENT
00846 * WILL SHOW UP ONLY ONCE.                 *                       ELUCSENT
00847 *******************************************                       ELUCSENT
00848      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
00849      PERFORM CONSTRUCT-DEDUCTIBLE-SENTENCES.                      ELUCSENT
00850      PERFORM CONSTRUCT-EXCEPTION-SENTENCES.                       ELUCSENT
00851      IF CSBP-CERTFN-REQRM-IND (CSBP-X-IDX) NOT = SPACES AND       ELUCSENT
00852          ZEROES                                                   ELUCSENT
00853          PERFORM CONSTRUCT-CERTIFICATION-REQUIR.                  ELUCSENT
00854      IF CSBP-COVERED-ON-SUPP (CSBP-X-IDX) AND                     ELUCSENT
00855                WS-Y-IDX < 16                                      ELUCSENT
00856          PERFORM CONSTRUCT-MAJOR-MEDICAL-PHRASE                   ELUCSENT
00857      ELSE IF WS-Y-IDX > 15                                        ELUCSENT
00858          PERFORM DISPLAY-MORE-SENTENCE-AREA-NEE.                  ELUCSENT
00859      SET NO-TOPIC-SENT-PRINTED TO TRUE.                           ELUCSENT
00860                                                                   ELUCSENT
00861                                                                   ELUCSENT
00862 ************************************************************      ELUCSENT
00863 *                                                          *      ELUCSENT
00864 *        CONSTRUCT COINSURANCE SENTENCES                   *      ELUCSENT
00865 *                                                          *      ELUCSENT
00866 ************************************************************      ELUCSENT
00867  CONSTRUCT-COINSURANCE-SENTENCE.                                  ELUCSENT
00868      IF CSBP-OB-STERILIZE-ST  AND                                 ELUCSENT
00869                 CSBP-FORMAT-W (CSBP-X-IDX)                        ELUCSENT
00870          PERFORM CONSTRUCT-FORMAT-W-SENTENCE                      ELUCSENT
00871      ELSE                                                         ELUCSENT
00872          PERFORM DO-COINSURANCE-SENTENCE-CONSTR.                  ELUCSENT
00873      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
00874                                                                   ELUCSENT
00875                                                                   ELUCSENT
00876 ************************************************************      ELUCSENT
00877 *                                                          *      ELUCSENT
00878 *        CONSTRUCT FORMAT W SENTENCE                       *      ELUCSENT
00879 *                                                          *      ELUCSENT
00880 ************************************************************      ELUCSENT
00881  CONSTRUCT-FORMAT-W-SENTENCE.                                     ELUCSENT
00882      MOVE CONTRACT-BENEFIT-SENTENCE TO WS-SENTENCES               ELUCSENT
00883          (WS-Y-IDX).                                              ELUCSENT
00884      ADD 1                          TO WS-NBR-SENTENCES.          ELUCSENT
00885      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
00886                                                                   ELUCSENT
00887                                                                   ELUCSENT
00888 ************************************************************      ELUCSENT
00889 *                                                          *      ELUCSENT
00890 *        CONSTRUCT MAXIMUM SENTENCES                       *      ELUCSENT
00891 *                                                          *      ELUCSENT
00892 ************************************************************      ELUCSENT
00893  CONSTRUCT-MAXIMUM-SENTENCES.                                     ELUCSENT
00894      IF CSBP-BAMA-BENEFIT-PERIOD (CSBP-X-IDX, CSBP-Y-IDX) NOT     ELUCSENT
00895          =                                                        ELUCSENT
00896                  SPACES AND ZEROS AND LOW-VALUES                  ELUCSENT
00897          PERFORM DO-MAXIMUM-SENTENCE-CONSTRUCTI.                  ELUCSENT
00898                                                                   ELUCSENT
00899                                                                   ELUCSENT
00900 ************************************************************      ELUCSENT
00901 *                                                          *      ELUCSENT
00902 *        CONSTRUCT DEDUCTIBLE SENTENCES                    *      ELUCSENT
00903 *                                                          *      ELUCSENT
00904 ************************************************************      ELUCSENT
00905  CONSTRUCT-DEDUCTIBLE-SENTENCES.                                  ELUCSENT
00906      IF ((NOT CSBP-OB-STERILIZE-ST)                               ELUCSENT
00907                           OR                                      ELUCSENT
00908                  (CSBP-OB-STERILIZE-ST AND NOT CSBP-FORMAT-W      ELUCSENT
00909          (CSBP-X-IDX)))                                           ELUCSENT
00910                          AND                                      ELUCSENT
00911                  (CSBP-DEDL-BENEFIT-PERIOD (CSBP-X-IDX) NOT =     ELUCSENT
00912          SPACES AND                                               ELUCSENT
00913                   ZEROS AND LOW-VALUES)                           ELUCSENT
00914          PERFORM DO-DEDUCTIBLE-SENTENCE-CONSTRU.                  ELUCSENT
00915                                                                   ELUCSENT
00916                                                                   ELUCSENT
00917 ************************************************************      ELUCSENT
00918 *                                                          *      ELUCSENT
00919 *        CONSTRUCT EXCEPTION SENTENCES                     *      ELUCSENT
00920 *                                                          *      ELUCSENT
00921 ************************************************************      ELUCSENT
00922  CONSTRUCT-EXCEPTION-SENTENCES.                                   ELUCSENT
00923      MOVE CSBP-BP-KEY (CSBP-X-IDX) TO WS-EMERGENCY.               ELUCSENT
00924      IF CSBP-OUTPATIENT-ST AND EMERGENCY-PROVISIONS               ELUCSENT
00925          PERFORM CONSTRUCT-TIME-RESTRICTION-SEN.                  ELUCSENT
00926                                                                   ELUCSENT
00927                                                                   ELUCSENT
00928 ************************************************************      ELUCSENT
00929 *                                                          *      ELUCSENT
00930 *        CONSTRUCT CERTIFICATION REQUIREMENT PHRASE        *      ELUCSENT
00931 *                                                          *      ELUCSENT
00932 ************************************************************      ELUCSENT
00933  CONSTRUCT-CERTIFICATION-REQUIR.                                  ELUCSENT
00934      MOVE 'BP'             TO CMF-RECORD-PREFIX.                  ELUCSENT
00935      MOVE CSBP-CERTFN-REQRM-IND (CSBP-X-IDX)                      ELUCSENT
00936                            TO CMF-CODE-VALUE.                     ELUCSENT
00937      MOVE 'CERTFN-REQRM-IND' TO                                   ELUCSENT
00938          CMF-ELEMENT-SYSTEM-NAME.                                 ELUCSENT
00939      PERFORM GET-TRANSLATION.                                     ELUCSENT
00940      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
00941                                                                   ELUCSENT
00942                                                                   ELUCSENT
00943 ************************************************************      ELUCSENT
00944 *                                                          *      ELUCSENT
00945 *        CONSTRUCT MAJOR MEDICAL PHRASE                    *      ELUCSENT
00946 *                                                          *      ELUCSENT
00947 ************************************************************      ELUCSENT
00948  CONSTRUCT-MAJOR-MEDICAL-PHRASE.                                  ELUCSENT
00949      MOVE COVERED-ON-SUPP-SENTENCE     TO WS-SENTENCES            ELUCSENT
00950          (WS-Y-IDX).                                              ELUCSENT
00951      ADD 1                             TO WS-NBR-SENTENCES.       ELUCSENT
00952      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
00953      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
00954                                                                   ELUCSENT
00955                                                                   ELUCSENT
00956 ************************************************************      ELUCSENT
00957 *                                                          *      ELUCSENT
00958 *        DO COINSURANCE SENTENCE CONSTRUCTION              *      ELUCSENT
00959 *                                                          *      ELUCSENT
00960 ************************************************************      ELUCSENT
00961  DO-COINSURANCE-SENTENCE-CONSTR.                                  ELUCSENT
00962      MOVE CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) TO                ELUCSENT
00963           WS-PROVN-PRICING-METHD.                                 ELUCSENT
00964      IF PPM-ZERO                                                  ELUCSENT
00965          PERFORM CONSTRUCT-ERROR-MESSAGE                          ELUCSENT
00966      ELSE IF CSBP-COINS-UNWANTED-SLOT (CSBP-X-IDX)                ELUCSENT
00967          PERFORM DO-UNWANTED-SLOT-ROUTINE                         ELUCSENT
00968      ELSE IF PPM-TYPE-1                                           ELUCSENT
00969          PERFORM CONSTRUCT-PPM-TYPE-1-SENTENCE                    ELUCSENT
00970      ELSE IF PPM-TYPE-2                                           ELUCSENT
00971          PERFORM CONSTRUCT-PPM-TYPE-2-SENTENCE                    ELUCSENT
00972      ELSE IF PPM-TYPE-3                                           ELUCSENT
00973          PERFORM CONSTRUCT-PPM-TYPE-3-SENTENCE                    ELUCSENT
00974      ELSE IF PPM-TYPE-4                                           ELUCSENT
00975          PERFORM CONSTRUCT-PPM-TYPE-4-SENTENCE                    ELUCSENT
00976      ELSE IF PPM-TYPE-5                                           ELUCSENT
00977          PERFORM CONSTRUCT-PPM-TYPE-5-SENTENCE                    ELUCSENT
00978      ELSE IF PPM-TYPE-6 AND (CSBP-BEN-SCOPE-ID (CSBP-X-IDX) NOT   ELUCSENT
00979          = SPACES                                                 ELUCSENT
00980                  AND ZEROS AND LOW-VALUES)                        ELUCSENT
00981          PERFORM CONSTRUCT-PPM-TYPE-6-SENTENCE                    ELUCSENT
00982      ELSE IF PPM-TYPE-09 AND CSBP-BP-PPF-SLOT (CSBP-X-IDX)  >     ELUCSENT
00983          +0                                                       ELUCSENT
00984          PERFORM CONSTRUCT-PPM-TYPE-09-SENTENCE                   ELUCSENT
00985      ELSE IF PPM-51                                               ELUCSENT
00986          PERFORM CONSTRUCT-PPM-TYPE-51-SENTENCE                   ELUCSENT
00987      ELSE IF PPM-53                                               ELUCSENT
00988          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
00989          MOVE PPM-53-PHRASE1 TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELUCSENT
00990          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
00991          MOVE PPM-53-PHRASE2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELUCSENT
00992          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
00993          MOVE PPM-53-PHRASE3 TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELUCSENT
00994      ELSE IF PPM-54                                               ELUCSENT
00995          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
00996          MOVE PPM-54-PHRASE1 TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELUCSENT
00997          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
00998          MOVE PPM-54-PHRASE2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELUCSENT
00999      ELSE IF PPM-55                                               ELUCSENT
01000          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
01001          MOVE PPM-55-PHRASE TO TCAR-FROM-LINE (TCAR-FROM-SUB)     ELUCSENT
01002      ELSE IF PPM-56                                               ELUCSENT
01003          ADD +1 TO TCAR-FROM-SUB                                  ELUCSENT
01004          MOVE PPM-56-PHRASE TO TCAR-FROM-LINE (TCAR-FROM-SUB)     ELUCSENT
01005      ELSE                                                         ELUCSENT
01006          PERFORM CONSTRUCT-UNDEFINED-SENTENCE.                    ELUCSENT
01007      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
01008                                                                   ELUCSENT
01009                                                                   ELUCSENT
01010 ************************************************************      ELUCSENT
01011 *                                                          *      ELUCSENT
01012 *        DO MAXIMUM SENTENCE CONSTRUCTION                  *      ELUCSENT
01013 *                                                          *      ELUCSENT
01014 ************************************************************      ELUCSENT
01015  DO-MAXIMUM-SENTENCE-CONSTRUCTI.                                  ELUCSENT
01016      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01017      MOVE ' MAXIMUMS '              TO TCAR-FROM-LINE             ELUCSENT
01018          (TCAR-FROM-SUB).                                         ELUCSENT
01019      IF CSBP-BAMA-VALUE-QUALIFIER (CSBP-X-IDX, CSBP-Y-IDX) NOT    ELUCSENT
01020          =                                                        ELUCSENT
01021                 SPACES AND ZEROS AND LOW-VALUES                   ELUCSENT
01022          PERFORM DISPLAY-MAXIMUM-PHRASE.                          ELUCSENT
01023                                                                   ELUCSENT
01024                                                                   ELUCSENT
01025 ************************************************************      ELUCSENT
01026 *                                                          *      ELUCSENT
01027 *        DO DEDUCTIBLE SENTENCE CONSTRUCTION               *      ELUCSENT
01028 *                                                          *      ELUCSENT
01029 ************************************************************      ELUCSENT
01030  DO-DEDUCTIBLE-SENTENCE-CONSTRU.                                  ELUCSENT
01031      IF CSBP-DEDL-VALUE-LIMIT (CSBP-X-IDX) < ZERO                 ELUCSENT
01032          PERFORM DISPLAY-DEDUCTIBLE-BASE-AMOUNT                   ELUCSENT
01033      ELSE                                                         ELUCSENT
01034          PERFORM DISPLAY-DEDUCTIBLE-INFORMATION.                  ELUCSENT
01035      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
01036                                                                   ELUCSENT
01037                                                                   ELUCSENT
01038 ************************************************************      ELUCSENT
01039 *                                                          *      ELUCSENT
01040 *        DISPLAY DEDUCTIBLE BASE AMOUNT FROM GROUP SPECIFIC*      ELUCSENT
01041 *                                                          *      ELUCSENT
01042 ************************************************************      ELUCSENT
01043  DISPLAY-DEDUCTIBLE-BASE-AMOUNT.                                  ELUCSENT
01044      IF GCG-DED-BASE-AMT-SOURCE-IND NOT = SPACES AND ZEROS        ELUCSENT
01045          AND                                                      ELUCSENT
01046                LOW-VALUES                                         ELUCSENT
01047         ADD +1                           TO TCAR-FROM-SUB         ELUCSENT
01048         MOVE WS-DED-DETERMINED-BY        TO TCAR-FROM-LINE        ELUCSENT
01049             (TCAR-FROM-SUB)                                       ELUCSENT
01050         MOVE GCG-DED-BASE-AMT-SOURCE-IND TO CMF-CODE-VALUE        ELUCSENT
01051         MOVE 'GROUP'                     TO CMF-RECORD-PREFIX     ELUCSENT
01052         MOVE 'DED-BASE-AMT-SOURCE-IND'                            ELUCSENT
01053                     TO CMF-ELEMENT-SYSTEM-NAME.                   ELUCSENT
01054         PERFORM GET-TRANSLATION.                                  ELUCSENT
01055                                                                   ELUCSENT
01056                                                                   ELUCSENT
01057 ************************************************************      ELUCSENT
01058 *                                                          *      ELUCSENT
01059 *        DISPLAY DEDUCTIBLE INFORMATION ON ACCUMULATOR     *      ELUCSENT
01060 *                                                          *      ELUCSENT
01061 ************************************************************      ELUCSENT
01062  DISPLAY-DEDUCTIBLE-INFORMATION.                                  ELUCSENT
01063      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01064      MOVE ' DEDUCTIBLE '            TO TCAR-FROM-LINE             ELUCSENT
01065          (TCAR-FROM-SUB).                                         ELUCSENT
01066      IF CSBP-DEDL-VALUE-QUALIFIER (CSBP-X-IDX) = '5'              ELUCSENT
01067          PERFORM CHECK-FOR-UNLIMITED-DEDUCTIBLE                   ELUCSENT
01068      ELSE IF CSBP-DEDL-VALUE-QUALIFIER (CSBP-X-IDX) NOT =         ELUCSENT
01069          SPACES AND                                               ELUCSENT
01070                 ZEROS AND LOW-VALUES                              ELUCSENT
01071          PERFORM DISPLAY-ADL-NUMERIC-FORMAT.                      ELUCSENT
01072      PERFORM CONSTRUCT-ADL-BENEFIT-PERIOD-P.                      ELUCSENT
01073                                                                   ELUCSENT
01074                                                                   ELUCSENT
01075 ************************************************************      ELUCSENT
01076 *                                                          *      ELUCSENT
01077 *        CHECK FOR UNLIMITED DEDUCTIBLE                    *      ELUCSENT
01078 *                                                          *      ELUCSENT
01079 ************************************************************      ELUCSENT
01080  CHECK-FOR-UNLIMITED-DEDUCTIBLE.                                  ELUCSENT
01081      MOVE CSBP-DEDL-VALUE-LIMIT (CSBP-X-IDX)                      ELUCSENT
01082                                     TO WS-LIMIT-EDITED.           ELUCSENT
01083      IF UNLIMITED-AMT                                             ELUCSENT
01084          PERFORM DISPLAY-UNLIMITED-PHRASE                         ELUCSENT
01085      ELSE                                                         ELUCSENT
01086          PERFORM DISPLAY-EDITED-DOLLAR-FORMAT.                    ELUCSENT
01087                                                                   ELUCSENT
01088                                                                   ELUCSENT
01089 ************************************************************      ELUCSENT
01090 *                                                          *      ELUCSENT
01091 *        CONSTRUCT TIME RESTRICTION SENTENCE               *      ELUCSENT
01092 *                                                          *      ELUCSENT
01093 ************************************************************      ELUCSENT
01094  CONSTRUCT-TIME-RESTRICTION-SEN.                                  ELUCSENT
01095      IF ACCIDENTAL-INJURY                                         ELUCSENT
01096          PERFORM CONSTRUCT-ACCIDENT-SENTENCE.                     ELUCSENT
01097      IF MEDICAL-EMERGENCY                                         ELUCSENT
01098          PERFORM CONSTRUCT-MEDICAL-SENTENCE.                      ELUCSENT
01099      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
01100                                                                   ELUCSENT
01101                                                                   ELUCSENT
01102 ************************************************************      ELUCSENT
01103 *                                                          *      ELUCSENT
01104 *        CONSTRUCT ERROR MESSAGE                           *      ELUCSENT
01105 *                                                          *      ELUCSENT
01106 ************************************************************      ELUCSENT
01107  CONSTRUCT-ERROR-MESSAGE.                                         ELUCSENT
01108      MOVE ERROR-MESSAGE         TO WS-SENTENCES                   ELUCSENT
01109          (WS-Y-IDX).                                              ELUCSENT
01110      ADD 1                      TO WS-NBR-SENTENCES.              ELUCSENT
01111      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
01112                                                                   ELUCSENT
01113                                                                   ELUCSENT
01114 ************************************************************      ELUCSENT
01115 *                                                          *      ELUCSENT
01116 *        CONSTRUCT PPM TYPE 1 SENTENCE                     *      ELUCSENT
01117 *                                                          *      ELUCSENT
01118 ************************************************************      ELUCSENT
01119  CONSTRUCT-PPM-TYPE-1-SENTENCE.                                   ELUCSENT
01120      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01121                                                                   ELUCSENT
01122 ************************************************************      ELUCSENT
01123 *                                                          *      ELUCSENT
01124 *        DO UNWANTED SLOT ROUTINE                          *      ELUCSENT
01125 *                                                          *      ELUCSENT
01126 ************************************************************      ELUCSENT
01127  DO-UNWANTED-SLOT-ROUTINE.                                        ELUCSENT
01128      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01129                                                                   ELUCSENT
01130 ************************************************************      ELUCSENT
01131 *                                                          *      ELUCSENT
01132 *        TRANSLATE PROVISION PRICING METHOD AND MOVE TO COM*      ELUCSENT
01133 *                                                          *      ELUCSENT
01134 ************************************************************      ELUCSENT
01135  TRANSLATE-PROVISION-PRICING-ME.                                  ELUCSENT
01136      MOVE 'BP'                           TO CMF-RECORD-PREFIX.    ELUCSENT
01137      MOVE 'PROVN-PRICING-METHD'          TO                       ELUCSENT
01138          CMF-ELEMENT-SYSTEM-NAME.                                 ELUCSENT
01139      MOVE CSBP-PROVN-PRICING-METHD (CSBP-X-IDX)                   ELUCSENT
01140                                          TO                       ELUCSENT
01141          CMF-CODE-VALUE.                                          ELUCSENT
01142      PERFORM GET-TRANSLATION.                                     ELUCSENT
01143                                                                   ELUCSENT
01144                                                                   ELUCSENT
01145 ************************************************************      ELUCSENT
01146 *                                                          *      ELUCSENT
01147 *        CONSTRUCT PPM TYPE 2 SENTENCE                     *      ELUCSENT
01148 *                                                          *      ELUCSENT
01149 ************************************************************      ELUCSENT
01150  CONSTRUCT-PPM-TYPE-2-SENTENCE.                                   ELUCSENT
01151      IF CSBP-COINS-AD-SUB (CSBP-X-IDX) = ZEROES                   ELUCSENT
01152          PERFORM CONSTRUCT-NORMAL-TYPE-2                          ELUCSENT
01153      ELSE                                                         ELUCSENT
01154         PERFORM CONSTRUCT-ASCEND-DESCEND-SITUA.                   ELUCSENT
01155                                                                   ELUCSENT
01156                                                                   ELUCSENT
01157 ************************************************************      ELUCSENT
01158 *                                                          *      ELUCSENT
01159 *        CONSTRUCT NORMAL TYPE 2                           *      ELUCSENT
01160 *                                                          *      ELUCSENT
01161 ************************************************************      ELUCSENT
01162  CONSTRUCT-NORMAL-TYPE-2.                                         ELUCSENT
01163      IF CSBP-COINS-PERCENT-LEVEL (CSBP-X-IDX) NOT =               ELUCSENT
01164          ZEROES                                                   ELUCSENT
01165          PERFORM INCLUDE-PERCENT-LEVEL.                           ELUCSENT
01166      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01167                                                                   ELUCSENT
01168                                                                   ELUCSENT
01169 ************************************************************      ELUCSENT
01170 *                                                          *      ELUCSENT
01171 *        INCLUDE PERCENT LEVEL                             *      ELUCSENT
01172 *                                                          *      ELUCSENT
01173 ************************************************************      ELUCSENT
01174  INCLUDE-PERCENT-LEVEL.                                           ELUCSENT
01175      MOVE CSBP-COINS-PERCENT-LEVEL (CSBP-X-IDX)                   ELUCSENT
01176                                    TO WS-PCT-VALUE.               ELUCSENT
01177      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01178      MOVE WS-PERCENT-PHRASE        TO TCAR-FROM-LINE              ELUCSENT
01179          (TCAR-FROM-SUB).                                         ELUCSENT
01180      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01181      MOVE ' OF '                   TO TCAR-FROM-LINE              ELUCSENT
01182          (TCAR-FROM-SUB).                                         ELUCSENT
01183                                                                   ELUCSENT
01184                                                                   ELUCSENT
01185 ************************************************************      ELUCSENT
01186 *                                                          *      ELUCSENT
01187 *        CONSTRUCT ASCEND DESCEND SITUATION                *      ELUCSENT
01188 *                                                          *      ELUCSENT
01189 ************************************************************      ELUCSENT
01190  CONSTRUCT-ASCEND-DESCEND-SITUA.                                  ELUCSENT
01191      SET CSAD-ACL-IDX TO CSBP-COINS-AD-SUB (CSBP-X-IDX).          ELUCSENT
01192      SET CSAD-ENTRY-IDX            TO +1.                         ELUCSENT
01193      MOVE CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-ENTRY-IDX)         ELUCSENT
01194                                    TO WS-PCT-VALUE.               ELUCSENT
01195      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01196      MOVE WS-PERCENT-PHRASE        TO TCAR-FROM-LINE              ELUCSENT
01197          (TCAR-FROM-SUB).                                         ELUCSENT
01198      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01199      MOVE ' OF '                   TO TCAR-FROM-LINE              ELUCSENT
01200          (TCAR-FROM-SUB).                                         ELUCSENT
01201      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01202      PERFORM DO-ASCEND-DESCEND-CONSTRUCTION.                      ELUCSENT
01203                                                                   ELUCSENT
01204                                                                   ELUCSENT
01205 ************************************************************      ELUCSENT
01206 *                                                          *      ELUCSENT
01207 *        CONSTRUCT PPM TYPE 3 SENTENCE                     *      ELUCSENT
01208 *                                                          *      ELUCSENT
01209 ************************************************************      ELUCSENT
01210  CONSTRUCT-PPM-TYPE-3-SENTENCE.                                   ELUCSENT
01211      MOVE CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX)              ELUCSENT
01212                        TO WS-PCT-VALUE.                           ELUCSENT
01213      ADD +1            TO TCAR-FROM-SUB.                          ELUCSENT
01214      MOVE WS-PERCENT-PHRASE TO TCAR-FROM-LINE                     ELUCSENT
01215          (TCAR-FROM-SUB).                                         ELUCSENT
01216      ADD +1            TO TCAR-FROM-SUB.                          ELUCSENT
01217      MOVE ' OF '       TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELUCSENT
01218      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01219                                                                   ELUCSENT
01220                                                                   ELUCSENT
01221 ************************************************************      ELUCSENT
01222 *                                                          *      ELUCSENT
01223 *        CONSTRUCT PPM TYPE 4 SENTENCE                     *      ELUCSENT
01224 *                                                          *      ELUCSENT
01225 ************************************************************      ELUCSENT
01226  CONSTRUCT-PPM-TYPE-4-SENTENCE.                                   ELUCSENT
01227      PERFORM TRANSLATE-PROVISION-PRICING-ME.                      ELUCSENT
01228      ADD +1             TO TCAR-FROM-SUB.                         ELUCSENT
01229      MOVE ' OF '        TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELUCSENT
01230      MOVE CSBP-ADDN-ALLOW-AMT-PER-DAY (CSBP-X-IDX)                ELUCSENT
01231                         TO WS-DOLLAR-AMT.                         ELUCSENT
01232      ADD +1             TO TCAR-FROM-SUB.                         ELUCSENT
01233      MOVE WS-DOLLAR-AMT TO TCAR-FROM-LINE                         ELUCSENT
01234          (TCAR-FROM-SUB).                                         ELUCSENT
01235                                                                   ELUCSENT
01236                                                                   ELUCSENT
01237 ************************************************************      ELUCSENT
01238 *                                                          *      ELUCSENT
01239 *        CONSTRUCT PPM TYPE 5 SENTENCE                     *      ELUCSENT
01240 *                                                          *      ELUCSENT
01241 ************************************************************      ELUCSENT
01242  CONSTRUCT-PPM-TYPE-5-SENTENCE.                                   ELUCSENT
01243      IF PPM-SPECIAL-04                                            ELUCSENT
01244          PERFORM CHANGE-PPM-VALUE-AND-SET-UP-PA                   ELUCSENT
01245      ELSE                                                         ELUCSENT
01246          PERFORM TRANSLATE-PROVISION-PRICING-ME.                  ELUCSENT
01247      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01248      MOVE ' OF '                  TO TCAR-FROM-LINE               ELUCSENT
01249          (TCAR-FROM-SUB).                                         ELUCSENT
01250      MOVE CSBP-FLAT-RATE-PDM-AMT (CSBP-X-IDX) TO WS-FLAT-AMT.     ELUCSENT
01251      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01252      MOVE WS-FLAT-AMT             TO TCAR-FROM-LINE               ELUCSENT
01253          (TCAR-FROM-SUB).                                         ELUCSENT
01254      IF PPM-SPECIAL-04  AND                                       ELUCSENT
01255          (CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX) > 0)         ELUCSENT
01256              PERFORM CONSTRUCT-PLUS-PERCENT-SENTENC.              ELUCSENT
01257      PERFORM CONSTRUCT-TYPE-5-COINS-SENTENC.                      ELUCSENT
01258                                                                   ELUCSENT
01259                                                                   ELUCSENT
01260 ************************************************************      ELUCSENT
01261 *                                                          *      ELUCSENT
01262 *        CONSTRUCT PPM TYPE 6 SENTENCE                     *      ELUCSENT
01263 *                                                          *      ELUCSENT
01264 ************************************************************      ELUCSENT
01265  CONSTRUCT-PPM-TYPE-6-SENTENCE.                                   ELUCSENT
01266      MOVE 'BPE'                          TO                       ELUCSENT
01267          CMF-RECORD-PREFIX.                                       ELUCSENT
01268      MOVE CSBP-BEN-SCOPE-ID (CSBP-X-IDX) TO CMF-CODE-VALUE.       ELUCSENT
01269      MOVE 'BEN-SCOPE-ID'                 TO                       ELUCSENT
01270          CMF-ELEMENT-SYSTEM-NAME.                                 ELUCSENT
01271      PERFORM GET-TRANSLATION.                                     ELUCSENT
01272      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01273      MOVE ' SCHEDULE '            TO TCAR-FROM-LINE               ELUCSENT
01274          (TCAR-FROM-SUB).                                         ELUCSENT
01275      IF PPM-SPECIAL-06  AND                                       ELUCSENT
01276                  CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX) >     ELUCSENT
01277          ZEROES                                                   ELUCSENT
01278          PERFORM CONSTRUCT-TYPE-06-SENTENCE                       ELUCSENT
01279      ELSE IF PPM-33                                               ELUCSENT
01280          PERFORM CONSTRUCT-TYPE-33-SENTENCE.                      ELUCSENT
01281                                                                   ELUCSENT
01282                                                                   ELUCSENT
01283 ************************************************************      ELUCSENT
01284 *                                                          *      ELUCSENT
01285 *        CONSTRUCT PPM TYPE 09 SENTENCE                    *      ELUCSENT
01286 *                                                          *      ELUCSENT
01287 ************************************************************      ELUCSENT
01288  CONSTRUCT-PPM-TYPE-09-SENTENCE.                                  ELUCSENT
01289      PERFORM READ-PPF-RECORD.                                     ELUCSENT
01290      PERFORM DO-PPM-TYPE-09-SENTENCE-CONSTR                       ELUCSENT
01291          VARYING GBB-INDEX FROM 1 BY 1                            ELUCSENT
01292                  UNTIL   GBB-INDEX = GBB-ENTRY-COUNT              ELUCSENT
01293                  OR      GBB-ENTRY (GBB-INDEX) = HIGH-VALUES      ELUCSENT
01294                  OR      TCAR-FROM-SUB > 19.                      ELUCSENT
01295      IF WS-CURRENT-IND NOT = SPACES                               ELUCSENT
01296          PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                  ELUCSENT
01297      INITIALIZE WS-CURRENT-IND                                    ELUCSENT
01298                 WS-PREV-IND.                                      ELUCSENT
01299                                                                   ELUCSENT
01300                                                                   ELUCSENT
01301 ************************************************************      ELUCSENT
01302 *                                                          *      ELUCSENT
01303 *        CONSTRUCT PPM TYPE 51 SENTENCE                    *      ELUCSENT
01304 *                                                          *      ELUCSENT
01305 ************************************************************      ELUCSENT
01306  CONSTRUCT-PPM-TYPE-51-SENTENCE.                                  ELUCSENT
01307      ADD +1            TO TCAR-FROM-SUB.                          ELUCSENT
01308      MOVE LESSER-OF-PHRASE                                        ELUCSENT
01309                        TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELUCSENT
01310      MOVE CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX)              ELUCSENT
01311                        TO WS-PCT-VALUE.                           ELUCSENT
01312      ADD +1            TO TCAR-FROM-SUB.                          ELUCSENT
01313      MOVE WS-PERCENT-PHRASE TO TCAR-FROM-LINE                     ELUCSENT
01314          (TCAR-FROM-SUB).                                         ELUCSENT
01315      ADD +1            TO TCAR-FROM-SUB.                          ELUCSENT
01316      MOVE WS-PPO-SCHEDULE-PHRASE                                  ELUCSENT
01317                        TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELUCSENT
01318                                                                   ELUCSENT
01319                                                                   ELUCSENT
01320 ************************************************************      ELUCSENT
01321 *                                                          *      ELUCSENT
01322 *        CONSTRUCT UNDEFINED SENTENCE                      *      ELUCSENT
01323 *                                                          *      ELUCSENT
01324 ************************************************************      ELUCSENT
01325  CONSTRUCT-UNDEFINED-SENTENCE.                                    ELUCSENT
01326      ADD +1                  TO TCAR-FROM-SUB.                    ELUCSENT
01327      MOVE PPM-PHRASE TO TCAR-FROM-LINE                            ELUCSENT
01328          (TCAR-FROM-SUB).                                         ELUCSENT
01329      ADD +1                  TO TCAR-FROM-SUB.                    ELUCSENT
01330      MOVE CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) TO                ELUCSENT
01331          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELUCSENT
01332      ADD +1                  TO TCAR-FROM-SUB.                    ELUCSENT
01333      MOVE UNDEFINED-SENTENCE TO TCAR-FROM-LINE                    ELUCSENT
01334          (TCAR-FROM-SUB).                                         ELUCSENT
01335      PERFORM WRITE-TO-LOGFILE.                                    ELUCSENT
01336 *                                                                 ELUCSENT
01337 ************************************************************      ELUCSENT
01338 *                                                          *      ELUCSENT
01339 *        WRITE-TO-LOGFILE                                  *      ELUCSENT
01340 *                                                          *      ELUCSENT
01341 ************************************************************      ELUCSENT
01342  WRITE-TO-LOGFILE.                                                ELUCSENT
01343      INITIALIZE LG-LOG-RECORD.                                    ELUCSENT
01344      MOVE 'ELUCSENT' TO LG-RECORD-PREFIX.                         ELUCSENT
01345      MOVE 'CSBP-PROVN-PRICING-METHD' TO LG-ELEMENT-NAME.          ELUCSENT
01346      MOVE CSBP-PROVN-PRICING-METHD (CSBP-X-IDX) TO                ELUCSENT
01347          LG-CODE-VALUE.                                           ELUCSENT
01348      SET LG-C-V-LOGIC TO TRUE.                                    ELUCSENT
01349      MOVE LENGTH OF LG-LOG-RECORD TO LG-LOG-LENGTH.               ELUCSENT
01350      CALL 'ELKLOG' USING DFHEIBLK                                 ELUCSENT
01351                          DFHCOMMAREA.                             ELUCSENT
01352                                                                   ELUCSENT
01353                                                                   ELUCSENT
01354 ************************************************************      ELUCSENT
01355 *                                                          *      ELUCSENT
01356 *        DISPLAY MAXIMUM PHRASE                            *      ELUCSENT
01357 *                                                          *      ELUCSENT
01358 ************************************************************      ELUCSENT
01359  DISPLAY-MAXIMUM-PHRASE.                                          ELUCSENT
01360      IF CSBP-BAMA-VALUE-QUALIFIER (CSBP-X-IDX, CSBP-Y-IDX) =      ELUCSENT
01361          '5'                                                      ELUCSENT
01362          PERFORM CHECK-FOR-UMLIMITED-MAXIMUM                      ELUCSENT
01363      ELSE                                                         ELUCSENT
01364          PERFORM DISPLAY-ABM-NUMERIC-FORMAT.                      ELUCSENT
01365      PERFORM CONSTRUCT-ABM-BENEFIT-PERIOD-P.                      ELUCSENT
01366      PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                      ELUCSENT
01367                                                                   ELUCSENT
01368                                                                   ELUCSENT
01369 ************************************************************      ELUCSENT
01370 *                                                          *      ELUCSENT
01371 *        CHECK FOR UMLIMITED MAXIMUM                       *      ELUCSENT
01372 *                                                          *      ELUCSENT
01373 ************************************************************      ELUCSENT
01374  CHECK-FOR-UMLIMITED-MAXIMUM.                                     ELUCSENT
01375      MOVE CSBP-BAMA-VALUE-LIMIT (CSBP-X-IDX,                      ELUCSENT
01376          CSBP-Y-IDX)                                              ELUCSENT
01377                                     TO WS-LIMIT-EDITED.           ELUCSENT
01378      IF UNLIMITED-AMT                                             ELUCSENT
01379          PERFORM DISPLAY-UNLIMITED-PHRASE                         ELUCSENT
01380      ELSE                                                         ELUCSENT
01381          PERFORM DISPLAY-EDITED-DOLLAR-FORMAT.                    ELUCSENT
01382                                                                   ELUCSENT
01383                                                                   ELUCSENT
01384 ************************************************************      ELUCSENT
01385 *                                                          *      ELUCSENT
01386 *        DISPLAY EDITED DOLLAR FORMAT                      *      ELUCSENT
01387 *                                                          *      ELUCSENT
01388 ************************************************************      ELUCSENT
01389  DISPLAY-EDITED-DOLLAR-FORMAT.                                    ELUCSENT
01390      ADD +1                           TO TCAR-FROM-SUB.           ELUCSENT
01391      MOVE WS-LIMIT-EDITED             TO TCAR-FROM-LINE           ELUCSENT
01392          (TCAR-FROM-SUB).                                         ELUCSENT
01393                                                                   ELUCSENT
01394                                                                   ELUCSENT
01395 ************************************************************      ELUCSENT
01396 *                                                          *      ELUCSENT
01397 *        DISPLAY ABM NUMERIC FORMAT                        *      ELUCSENT
01398 *                                                          *      ELUCSENT
01399 ************************************************************      ELUCSENT
01400  DISPLAY-ABM-NUMERIC-FORMAT.                                      ELUCSENT
01401      MOVE CSBP-BAMA-VALUE-LIMIT (CSBP-X-IDX, CSBP-Y-IDX)          ELUCSENT
01402                                       TO WS-LIMIT-M.              ELUCSENT
01403      ADD +1                           TO TCAR-FROM-SUB.           ELUCSENT
01404      MOVE WS-LIMIT-M                  TO TCAR-FROM-LINE           ELUCSENT
01405          (TCAR-FROM-SUB).                                         ELUCSENT
01406      MOVE PC-ABM                      TO CMF-RECORD-PREFIX.       ELUCSENT
01407      MOVE 'BAMA-VALUE-QUALIFIER'      TO CMF-ELEMENT-SYSTEM-NAME. ELUCSENT
01408      MOVE CSBP-BAMA-VALUE-QUALIFIER (CSBP-X-IDX, CSBP-Y-IDX)      ELUCSENT
01409                                       TO                          ELUCSENT
01410          CMF-CODE-VALUE.                                          ELUCSENT
01411      PERFORM GET-TRANSLATION.                                     ELUCSENT
01412                                                                   ELUCSENT
01413                                                                   ELUCSENT
01414 ************************************************************      ELUCSENT
01415 *                                                          *      ELUCSENT
01416 *        DISPLAY ADL NUMERIC FORMAT                        *      ELUCSENT
01417 *                                                          *      ELUCSENT
01418 ************************************************************      ELUCSENT
01419  DISPLAY-ADL-NUMERIC-FORMAT.                                      ELUCSENT
01420      MOVE CSBP-DEDL-VALUE-LIMIT (CSBP-X-IDX)                      ELUCSENT
01421                                    TO WS-LIMIT-M.                 ELUCSENT
01422      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01423      MOVE WS-LIMIT-M               TO TCAR-FROM-LINE              ELUCSENT
01424          (TCAR-FROM-SUB).                                         ELUCSENT
01425      MOVE PC-ADL                   TO CMF-RECORD-PREFIX.          ELUCSENT
01426      MOVE 'DEDL-VALUE-QUALIFIER'   TO CMF-ELEMENT-SYSTEM-NAME.    ELUCSENT
01427      MOVE CSBP-DEDL-VALUE-QUALIFIER (CSBP-X-IDX)                  ELUCSENT
01428                                    TO CMF-CODE-VALUE.             ELUCSENT
01429      PERFORM GET-TRANSLATION.                                     ELUCSENT
01430                                                                   ELUCSENT
01431                                                                   ELUCSENT
01432 ************************************************************      ELUCSENT
01433 *                                                          *      ELUCSENT
01434 *        CONSTRUCT ABM BENEFIT PERIOD PHRASE               *      ELUCSENT
01435 *                                                          *      ELUCSENT
01436 ************************************************************      ELUCSENT
01437  CONSTRUCT-ABM-BENEFIT-PERIOD-P.                                  ELUCSENT
01438      ADD +1                     TO TCAR-FROM-SUB.                 ELUCSENT
01439      MOVE ' PER '               TO TCAR-FROM-LINE                 ELUCSENT
01440          (TCAR-FROM-SUB).                                         ELUCSENT
01441      MOVE PC-ABM                TO CMF-RECORD-PREFIX.             ELUCSENT
01442      MOVE 'BAMA-BENEFIT-PERIOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELUCSENT
01443      MOVE CSBP-BAMA-BENEFIT-PERIOD (CSBP-X-IDX, CSBP-Y-IDX)       ELUCSENT
01444                                 TO CMF-CODE-VALUE.                ELUCSENT
01445      PERFORM GET-TRANSLATION.                                     ELUCSENT
01446                                                                   ELUCSENT
01447                                                                   ELUCSENT
01448 ************************************************************      ELUCSENT
01449 *                                                          *      ELUCSENT
01450 *        CONSTRUCT ADL BENEFIT PERIOD PHRASE               *      ELUCSENT
01451 *                                                          *      ELUCSENT
01452 ************************************************************      ELUCSENT
01453  CONSTRUCT-ADL-BENEFIT-PERIOD-P.                                  ELUCSENT
01454      ADD +1                           TO TCAR-FROM-SUB.           ELUCSENT
01455      MOVE ' PER '                     TO TCAR-FROM-LINE           ELUCSENT
01456          (TCAR-FROM-SUB).                                         ELUCSENT
01457      MOVE PC-ADL                      TO CMF-RECORD-PREFIX.       ELUCSENT
01458      MOVE 'DEDL-BENEFIT-PERIOD'       TO CMF-ELEMENT-SYSTEM-NAME. ELUCSENT
01459      MOVE CSBP-DEDL-BENEFIT-PERIOD (CSBP-X-IDX) TO                ELUCSENT
01460          CMF-CODE-VALUE.                                          ELUCSENT
01461      PERFORM GET-TRANSLATION.                                     ELUCSENT
01462                                                                   ELUCSENT
01463                                                                   ELUCSENT
01464 ************************************************************      ELUCSENT
01465 *                                                          *      ELUCSENT
01466 *        CONSTRUCT ACCIDENT SENTENCE                       *      ELUCSENT
01467 *                                                          *      ELUCSENT
01468 ************************************************************      ELUCSENT
01469  CONSTRUCT-ACCIDENT-SENTENCE.                                     ELUCSENT
01470      IF CSBP-DAYS-BTWN-ACCD-EMRG-TREAT (CSBP-X-IDX) =             ELUCSENT
01471                  PC-EMER-DAYS                                     ELUCSENT
01472          PERFORM CONSTRUCT-ACCIDENT-MEDICAL-SEN                   ELUCSENT
01473      ELSE IF CSBP-DAYS-BTWN-ACCD-EMRG-TREAT (CSBP-X-IDX) NOT      ELUCSENT
01474          =                                                        ELUCSENT
01475                  ZERO                                             ELUCSENT
01476          PERFORM CONSTRUCT-ACCIDENT-TIME-RESTRI.                  ELUCSENT
01477                                                                   ELUCSENT
01478                                                                   ELUCSENT
01479 ************************************************************      ELUCSENT
01480 *                                                          *      ELUCSENT
01481 *        CONSTRUCT ACCIDENT MEDICAL SENTENCE               *      ELUCSENT
01482 *                                                          *      ELUCSENT
01483 ************************************************************      ELUCSENT
01484  CONSTRUCT-ACCIDENT-MEDICAL-SEN.                                  ELUCSENT
01485      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01486      MOVE MED-ACC-EMER-SENTENCE      TO TCAR-FROM-LINE            ELUCSENT
01487          (TCAR-FROM-SUB).                                         ELUCSENT
01488      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01489      MOVE MED-ACC-EMER-SENTENCE-A    TO TCAR-FROM-LINE            ELUCSENT
01490          (TCAR-FROM-SUB).                                         ELUCSENT
01491      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01492      MOVE MED-ACC-EMER-SENTENCE-B    TO TCAR-FROM-LINE            ELUCSENT
01493          (TCAR-FROM-SUB).                                         ELUCSENT
01494                                                                   ELUCSENT
01495                                                                   ELUCSENT
01496 ************************************************************      ELUCSENT
01497 *                                                          *      ELUCSENT
01498 *        CONSTRUCT ACCIDENT TIME RESTRICTION SENTENCE      *      ELUCSENT
01499 *                                                          *      ELUCSENT
01500 ************************************************************      ELUCSENT
01501  CONSTRUCT-ACCIDENT-TIME-RESTRI.                                  ELUCSENT
01502      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01503      MOVE ' WITHIN '                 TO TCAR-FROM-LINE            ELUCSENT
01504          (TCAR-FROM-SUB).                                         ELUCSENT
01505      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01506      MOVE CSBP-DAYS-BTWN-ACCD-EMRG-TREAT (CSBP-X-IDX)             ELUCSENT
01507                                      TO                           ELUCSENT
01508          WS-SUPPRESS-NBR.                                         ELUCSENT
01509      MOVE WS-SUPPRESS-NBR            TO TCAR-FROM-LINE            ELUCSENT
01510          (TCAR-FROM-SUB).                                         ELUCSENT
01511      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01512      MOVE ' DAYS OF AN ACCIDENT '    TO TCAR-FROM-LINE            ELUCSENT
01513          (TCAR-FROM-SUB).                                         ELUCSENT
01514                                                                   ELUCSENT
01515                                                                   ELUCSENT
01516 ************************************************************      ELUCSENT
01517 *                                                          *      ELUCSENT
01518 *        CONSTRUCT MEDICAL SENTENCE                        *      ELUCSENT
01519 *                                                          *      ELUCSENT
01520 ************************************************************      ELUCSENT
01521  CONSTRUCT-MEDICAL-SENTENCE.                                      ELUCSENT
01522      IF CSBP-DAYS-BTWN-MED-EMRG-TREAT (CSBP-X-IDX) =              ELUCSENT
01523                  PC-EMER-DAYS                                     ELUCSENT
01524          PERFORM CONSTRUCT-ACCIDENT-MEDICAL-SEN                   ELUCSENT
01525      ELSE IF CSBP-DAYS-BTWN-MED-EMRG-TREAT (CSBP-X-IDX) NOT       ELUCSENT
01526          =                                                        ELUCSENT
01527                  ZERO                                             ELUCSENT
01528          PERFORM CONSTRUCT-MEDICAL-TIME-RESTRIC.                  ELUCSENT
01529                                                                   ELUCSENT
01530                                                                   ELUCSENT
01531 ************************************************************      ELUCSENT
01532 *                                                          *      ELUCSENT
01533 *        CONSTRUCT MEDICAL TIME RESTRICTION SENTENCE       *      ELUCSENT
01534 *                                                          *      ELUCSENT
01535 ************************************************************      ELUCSENT
01536  CONSTRUCT-MEDICAL-TIME-RESTRIC.                                  ELUCSENT
01537      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01538      MOVE ' WITHIN '                TO TCAR-FROM-LINE             ELUCSENT
01539          (TCAR-FROM-SUB).                                         ELUCSENT
01540      MOVE CSBP-DAYS-BTWN-MED-EMRG-TREAT (CSBP-X-IDX)              ELUCSENT
01541                                     TO WS-SUPPRESS-NBR.           ELUCSENT
01542      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01543      MOVE WS-SUPPRESS-NBR           TO TCAR-FROM-LINE             ELUCSENT
01544          (TCAR-FROM-SUB).                                         ELUCSENT
01545      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01546      MOVE ' DAYS OF ONSET OF ILLNESS '                            ELUCSENT
01547                                     TO TCAR-FROM-LINE             ELUCSENT
01548          (TCAR-FROM-SUB).                                         ELUCSENT
01549                                                                   ELUCSENT
01550                                                                   ELUCSENT
01551 ************************************************************      ELUCSENT
01552 *                                                          *      ELUCSENT
01553 *        CONSTRUCT TYPE 06 SENTENCE                        *      ELUCSENT
01554 *                                                          *      ELUCSENT
01555 ************************************************************      ELUCSENT
01556  CONSTRUCT-TYPE-06-SENTENCE.                                      ELUCSENT
01557      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01558      MOVE ' PLUS '                  TO TCAR-FROM-LINE             ELUCSENT
01559          (TCAR-FROM-SUB).                                         ELUCSENT
01560      MOVE CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX)              ELUCSENT
01561                                     TO WS-PCT-VALUE.              ELUCSENT
01562      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01563      MOVE WS-PERCENT-PHRASE         TO TCAR-FROM-LINE             ELUCSENT
01564          (TCAR-FROM-SUB).                                         ELUCSENT
01565                                                                   ELUCSENT
01566                                                                   ELUCSENT
01567 ************************************************************      ELUCSENT
01568 *                                                          *      ELUCSENT
01569 *        CONSTRUCT TYPE 33 SENTENCE                        *      ELUCSENT
01570 *                                                          *      ELUCSENT
01571 ************************************************************      ELUCSENT
01572  CONSTRUCT-TYPE-33-SENTENCE.                                      ELUCSENT
01573      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01574      MOVE ' PLUS '                  TO TCAR-FROM-LINE             ELUCSENT
01575          (TCAR-FROM-SUB).                                         ELUCSENT
01576      MOVE CSBP-VARIABLE-INDEMNITY-PRCNT (CSBP-X-IDX)              ELUCSENT
01577                                     TO WS-PCT-VALUE.              ELUCSENT
01578      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01579      MOVE WS-PERCENT-PHRASE         TO TCAR-FROM-LINE             ELUCSENT
01580          (TCAR-FROM-SUB).                                         ELUCSENT
01581      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01582      MOVE ' THEN APPLY '            TO TCAR-FROM-LINE             ELUCSENT
01583          (TCAR-FROM-SUB).                                         ELUCSENT
01584      MOVE CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX)              ELUCSENT
01585                                     TO WS-PCT-VALUE.              ELUCSENT
01586      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01587      MOVE WS-PERCENT-PHRASE         TO TCAR-FROM-LINE             ELUCSENT
01588          (TCAR-FROM-SUB).                                         ELUCSENT
01589                                                                   ELUCSENT
01590                                                                   ELUCSENT
01591 ************************************************************      ELUCSENT
01592 *                                                          *      ELUCSENT
01593 *        CONSTRUCT PLUS PERCENT SENTENCE                   *      ELUCSENT
01594 *                                                          *      ELUCSENT
01595 ************************************************************      ELUCSENT
01596  CONSTRUCT-PLUS-PERCENT-SENTENC.                                  ELUCSENT
01597      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01598      MOVE ' PLUS '                  TO TCAR-FROM-LINE             ELUCSENT
01599          (TCAR-FROM-SUB).                                         ELUCSENT
01600      MOVE CSBP-ADDITIONAL-PRICING-PRCNT (CSBP-X-IDX)              ELUCSENT
01601                                     TO WS-PCT-VALUE.              ELUCSENT
01602      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01603      MOVE WS-PERCENT-PHRASE         TO TCAR-FROM-LINE             ELUCSENT
01604          (TCAR-FROM-SUB).                                         ELUCSENT
01605      ADD +1                         TO TCAR-FROM-SUB.             ELUCSENT
01606      MOVE ' ON REMAINDER '          TO TCAR-FROM-LINE             ELUCSENT
01607          (TCAR-FROM-SUB).                                         ELUCSENT
01608      IF PPM-21                                                    ELUCSENT
01609          PERFORM CONSTRUCT-PPM-VALUE-21-SENTENC                   ELUCSENT
01610      ELSE IF PPM-22                                               ELUCSENT
01611          PERFORM CONSTRUCT-PPM-VALUE-22-SENTENC.                  ELUCSENT
01612                                                                   ELUCSENT
01613                                                                   ELUCSENT
01614 ************************************************************      ELUCSENT
01615 *                                                          *      ELUCSENT
01616 *        CHANGE PPM VALUE AND SET UP PARMS                 *      ELUCSENT
01617 *                                                          *      ELUCSENT
01618 ************************************************************      ELUCSENT
01619  CHANGE-PPM-VALUE-AND-SET-UP-PA.                                  ELUCSENT
01620      MOVE 'BP'                      TO                            ELUCSENT
01621          CMF-RECORD-PREFIX.                                       ELUCSENT
01622      MOVE 'PROVN-PRICING-METHD'     TO CMF-ELEMENT-SYSTEM-NAME.   ELUCSENT
01623      MOVE '04'                      TO CMF-CODE-VALUE.            ELUCSENT
01624      PERFORM GET-TRANSLATION.                                     ELUCSENT
01625                                                                   ELUCSENT
01626                                                                   ELUCSENT
01627 ************************************************************      ELUCSENT
01628 *                                                          *      ELUCSENT
01629 *        CONSTRUCT TYPE 5 COINS SENTENCE                   *      ELUCSENT
01630 *                                                          *      ELUCSENT
01631 ************************************************************      ELUCSENT
01632  CONSTRUCT-TYPE-5-COINS-SENTENC.                                  ELUCSENT
01633      IF CSBP-COINS-AD-SUB (CSBP-X-IDX) NOT = ZEROES               ELUCSENT
01634          PERFORM DO-TYPE-5-ASCEND-DESCEND-SITUA                   ELUCSENT
01635      ELSE                                                         ELUCSENT
01636          PERFORM DO-TYPE-5-NORMAL-PHRASE.                         ELUCSENT
01637                                                                   ELUCSENT
01638                                                                   ELUCSENT
01639 ************************************************************      ELUCSENT
01640 *                                                          *      ELUCSENT
01641 *        DO TYPE 5 ASCEND DESCEND SITUATION                *      ELUCSENT
01642 *                                                          *      ELUCSENT
01643 ************************************************************      ELUCSENT
01644  DO-TYPE-5-ASCEND-DESCEND-SITUA.                                  ELUCSENT
01645      SET CSAD-ACL-IDX TO CSBP-COINS-AD-SUB (CSBP-X-IDX).          ELUCSENT
01646      SET CSAD-ENTRY-IDX                  TO 1.                    ELUCSENT
01647      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01648      MOVE PC-COLON                 TO TCAR-FROM-LINE              ELUCSENT
01649          (TCAR-FROM-SUB).                                         ELUCSENT
01650      MOVE CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-ENTRY-IDX)         ELUCSENT
01651                                    TO WS-PCT-VALUE.               ELUCSENT
01652      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01653      MOVE WS-PERCENT-PHRASE        TO TCAR-FROM-LINE              ELUCSENT
01654          (TCAR-FROM-SUB).                                         ELUCSENT
01655      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01656      MOVE ' COINSURANCE '          TO TCAR-FROM-LINE              ELUCSENT
01657          (TCAR-FROM-SUB).                                         ELUCSENT
01658      PERFORM DISPLAY-ASCEND-DESCEND-SENTENC                       ELUCSENT
01659          VARYING WS-ENTRY-SUB FROM 2 BY 1                         ELUCSENT
01660                  UNTIL   WS-ENTRY-SUB >                           ELUCSENT
01661                        CSAD-ENTRIES-USED (CSAD-ACL-IDX).          ELUCSENT
01662                                                                   ELUCSENT
01663                                                                   ELUCSENT
01664 ************************************************************      ELUCSENT
01665 *                                                          *      ELUCSENT
01666 *        DO TYPE 5 NORMAL PHRASE                           *      ELUCSENT
01667 *                                                          *      ELUCSENT
01668 ************************************************************      ELUCSENT
01669  DO-TYPE-5-NORMAL-PHRASE.                                         ELUCSENT
01670      IF CSBP-COINS-PERCENT-LEVEL (CSBP-X-IDX) > 0                 ELUCSENT
01671         ADD +1                       TO TCAR-FROM-SUB             ELUCSENT
01672         MOVE PC-COLON                TO TCAR-FROM-LINE            ELUCSENT
01673             (TCAR-FROM-SUB)                                       ELUCSENT
01674         MOVE CSBP-COINS-PERCENT-LEVEL (CSBP-X-IDX)                ELUCSENT
01675                                      TO WS-PCT-VALUE              ELUCSENT
01676         ADD +1                       TO TCAR-FROM-SUB             ELUCSENT
01677         MOVE WS-PERCENT-PHRASE       TO TCAR-FROM-LINE            ELUCSENT
01678             (TCAR-FROM-SUB)                                       ELUCSENT
01679         ADD +1                       TO TCAR-FROM-SUB             ELUCSENT
01680         MOVE ' COINSURANCE '         TO TCAR-FROM-LINE            ELUCSENT
01681             (TCAR-FROM-SUB).                                      ELUCSENT
01682                                                                   ELUCSENT
01683                                                                   ELUCSENT
01684 ************************************************************      ELUCSENT
01685 *                                                          *      ELUCSENT
01686 *        CONSTRUCT PPM VALUE 21 SENTENCE                   *      ELUCSENT
01687 *                                                          *      ELUCSENT
01688 ************************************************************      ELUCSENT
01689  CONSTRUCT-PPM-VALUE-21-SENTENC.                                  ELUCSENT
01690      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01691      MOVE ' UP TO ASP RATE '      TO TCAR-FROM-LINE               ELUCSENT
01692          (TCAR-FROM-SUB).                                         ELUCSENT
01693                                                                   ELUCSENT
01694                                                                   ELUCSENT
01695 ************************************************************      ELUCSENT
01696 *                                                          *      ELUCSENT
01697 *        CONSTRUCT PPM VALUE 22 SENTENCE                   *      ELUCSENT
01698 *                                                          *      ELUCSENT
01699 ************************************************************      ELUCSENT
01700  CONSTRUCT-PPM-VALUE-22-SENTENC.                                  ELUCSENT
01701      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01702      MOVE ' UP TO MCSP RATE '     TO TCAR-FROM-LINE               ELUCSENT
01703          (TCAR-FROM-SUB).                                         ELUCSENT
01704                                                                   ELUCSENT
01705                                                                   ELUCSENT
01706 ************************************************************      ELUCSENT
01707 *                                                          *      ELUCSENT
01708 *        DO ASCEND DESCEND CONSTRUCTION                    *      ELUCSENT
01709 *                                                          *      ELUCSENT
01710 ************************************************************      ELUCSENT
01711  DO-ASCEND-DESCEND-CONSTRUCTION.                                  ELUCSENT
01712      IF CSBP-COINS-VALUE-QUALIFIER (CSBP-X-IDX) = PC-DOLLARS      ELUCSENT
01713         MOVE CSAD-ACL-VAL-LMT (CSAD-ACL-IDX, CSAD-ENTRY-IDX)      ELUCSENT
01714                                    TO WS-LIMIT-EDITED             ELUCSENT
01715      ELSE                                                         ELUCSENT
01716         MOVE CSAD-ACL-VAL-LMT (CSAD-ACL-IDX, CSAD-ENTRY-IDX)      ELUCSENT
01717                                    TO WS-LIMIT                    ELUCSENT
01718         SET NOT-DOLLARS TO TRUE.                                  ELUCSENT
01719      IF NOT-DOLLARS                                               ELUCSENT
01720          PERFORM DISPLAY-NONDOLLAR-LIMIT-TRANSL                   ELUCSENT
01721          PERFORM DO-TRANS-VALUE-QUALIFIER.                        ELUCSENT
01722      IF UNLIMITED-AMT                                             ELUCSENT
01723           PERFORM DISPLAY-UNLIMITED-PHRASE                        ELUCSENT
01724      ELSE                                                         ELUCSENT
01725         IF DOLLAR-AMT                                             ELUCSENT
01726            PERFORM DISPLAY-EDITED-DOLLAR-FORMAT-F                 ELUCSENT
01727         ELSE                                                      ELUCSENT
01728            CONTINUE.                                              ELUCSENT
01729      PERFORM DISPLAY-ASCEND-DESCEND-SENTENC                       ELUCSENT
01730          VARYING WS-ENTRY-SUB FROM 2 BY 1                         ELUCSENT
01731                  UNTIL   WS-ENTRY-SUB >                           ELUCSENT
01732                         CSAD-ENTRIES-USED (CSAD-ACL-IDX).         ELUCSENT
01733      SET DOLLAR-AMT TO TRUE.                                      ELUCSENT
01734                                                                   ELUCSENT
01735 ************************************************************      ELUCSENT
01736 *                                                          *      ELUCSENT
01737 *        DISPLAY ASCEND DESCEND SENTENCE                   *      ELUCSENT
01738 *                                                          *      ELUCSENT
01739 ************************************************************      ELUCSENT
01740  DISPLAY-ASCEND-DESCEND-SENTENC.                                  ELUCSENT
01741      SET CSAD-ENTRY-IDX TO WS-ENTRY-SUB.                          ELUCSENT
01742      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01743      MOVE ' THEN CHANGES TO '     TO TCAR-FROM-LINE               ELUCSENT
01744          (TCAR-FROM-SUB).                                         ELUCSENT
01745      MOVE CSAD-ACL-PCT-LVL (CSAD-ACL-IDX, CSAD-ENTRY-IDX)         ELUCSENT
01746                                   TO WS-PCT-VALUE.                ELUCSENT
01747      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01748      MOVE WS-PERCENT-PHRASE       TO TCAR-FROM-LINE               ELUCSENT
01749          (TCAR-FROM-SUB).                                         ELUCSENT
01750      PERFORM DETERMINE-TYPE-VALUE-QUALIFIER.                      ELUCSENT
01751      IF NOT-DOLLARS                                               ELUCSENT
01752          PERFORM DISPLAY-NONDOLLAR-LIMIT-TRANSL                   ELUCSENT
01753          PERFORM DO-TRANS-VALUE-QUALIFIER.                        ELUCSENT
01754      IF UNLIMITED-AMT AND DOLLAR-AMT                              ELUCSENT
01755          PERFORM DISPLAY-UNLIMITED-PHRASE                         ELUCSENT
01756      ELSE                                                         ELUCSENT
01757          IF DOLLAR-AMT                                            ELUCSENT
01758             PERFORM DISPLAY-EDITED-DOLLAR-FORMAT-F                ELUCSENT
01759          END-IF                                                   ELUCSENT
01760      END-IF.                                                      ELUCSENT
01761                                                                   ELUCSENT
01762 ************************************************************      ELUCSENT
01763 *                                                          *      ELUCSENT
01764 *        DETERMINE TYPE VALUE QUALIFIER                    *      ELUCSENT
01765 *                                                          *      ELUCSENT
01766 ************************************************************      ELUCSENT
01767  DETERMINE-TYPE-VALUE-QUALIFIER.                                  ELUCSENT
01768      IF CSBP-COINS-VALUE-QUALIFIER (CSBP-X-IDX) = PC-DOLLARS      ELUCSENT
01769         MOVE CSAD-ACL-VAL-LMT (CSAD-ACL-IDX, CSAD-ENTRY-IDX)      ELUCSENT
01770                                    TO WS-LIMIT-EDITED             ELUCSENT
01771      ELSE                                                         ELUCSENT
01772         MOVE CSAD-ACL-VAL-LMT (CSAD-ACL-IDX, CSAD-ENTRY-IDX)      ELUCSENT
01773            TO WS-VALUE-LMT-HOLD                                   ELUCSENT
01774         IF WS-VALUE-LMT-HOLD = WS-NONDOLLAR-UNLIMITED             ELUCSENT
01775             SET NOT-DOLLARS TO TRUE                               ELUCSENT
01776         ELSE                                                      ELUCSENT
01777             MOVE CSAD-ACL-VAL-LMT (CSAD-ACL-IDX, CSAD-ENTRY-IDX)  ELUCSENT
01778                                        TO WS-LIMIT                ELUCSENT
01779             SET NOT-DOLLARS TO TRUE                               ELUCSENT
01780         END-IF                                                    ELUCSENT
01781      END-IF.                                                      ELUCSENT
01782 ************************************************************      ELUCSENT
01783 *                                                          *      ELUCSENT
01784 *        DISPLAY NONDOLLAR LIMIT TRANSLATION               *      ELUCSENT
01785 *                                                          *      ELUCSENT
01786 ************************************************************      ELUCSENT
01787  DISPLAY-NONDOLLAR-LIMIT-TRANSL.                                  ELUCSENT
01788      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01789      MOVE ' FOR '                 TO TCAR-FROM-LINE               ELUCSENT
01790          (TCAR-FROM-SUB).                                         ELUCSENT
01791      IF WS-VALUE-LMT-HOLD = WS-NONDOLLAR-UNLIMITED                ELUCSENT
01792         MOVE ' FOR UNLIMITED '     TO TCAR-FROM-LINE              ELUCSENT
01793             (TCAR-FROM-SUB)                                       ELUCSENT
01794      ELSE                                                         ELUCSENT
01795         ADD +1                       TO TCAR-FROM-SUB             ELUCSENT
01796         MOVE WS-LIMIT               TO TCAR-FROM-LINE             ELUCSENT
01797             (TCAR-FROM-SUB)                                       ELUCSENT
01798      END-IF.                                                      ELUCSENT
01799                                                                   ELUCSENT
01800 ************************************************************      ELUCSENT
01801 *                                                          *      ELUCSENT
01802 *        DISPLAY EDITED DOLLAR FORMAT FOR ACL              *      ELUCSENT
01803 *                                                          *      ELUCSENT
01804 ************************************************************      ELUCSENT
01805  DISPLAY-EDITED-DOLLAR-FORMAT-F.                                  ELUCSENT
01806      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01807      MOVE ' TO '                  TO TCAR-FROM-LINE               ELUCSENT
01808          (TCAR-FROM-SUB).                                         ELUCSENT
01809      PERFORM DISPLAY-EDITED-DOLLAR-FORMAT.                        ELUCSENT
01810                                                                   ELUCSENT
01811                                                                   ELUCSENT
01812 ************************************************************      ELUCSENT
01813 *                                                          *      ELUCSENT
01814 *        DISPLAY UNLIMITED PHRASE                          *      ELUCSENT
01815 *                                                          *      ELUCSENT
01816 ************************************************************      ELUCSENT
01817  DISPLAY-UNLIMITED-PHRASE.                                        ELUCSENT
01818      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01819      MOVE UNLIMITED-PHRASE        TO TCAR-FROM-LINE               ELUCSENT
01820          (TCAR-FROM-SUB).                                         ELUCSENT
01821      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
01822      MOVE UNLIMITED-PHRASE-X      TO TCAR-FROM-LINE               ELUCSENT
01823          (TCAR-FROM-SUB).                                         ELUCSENT
01824                                                                   ELUCSENT
01825                                                                   ELUCSENT
01826 ************************************************************      ELUCSENT
01827 *                                                          *      ELUCSENT
01828 *        GET TRANSLATION                                   *      ELUCSENT
01829 *                                                          *      ELUCSENT
01830 ************************************************************      ELUCSENT
01831  GET-TRANSLATION.                                                 ELUCSENT
01832      PERFORM LINK-TO-CODES-MANUAL.                                ELUCSENT
01833      PERFORM MOVE-CODES-MANUAL-TEXT                               ELUCSENT
01834          VARYING CMF-DESCR-IDX FROM 1 BY 1                        ELUCSENT
01835                   UNTIL CMF-DESCR-IDX >                           ELUCSENT
01836              CMF-NBR-DESCR-LINES.                                 ELUCSENT
01837                                                                   ELUCSENT
01838                                                                   ELUCSENT
01839 ************************************************************      ELUCSENT
01840 *                                                          *      ELUCSENT
01841 *        COMPRESS UNSTRING AND LOAD WS TABLE               *      ELUCSENT
01842 *                                                          *      ELUCSENT
01843 ************************************************************      ELUCSENT
01844  COMPRESS-UNSTRING-AND-LOAD-WSX.                                  ELUCSENT
01845      PERFORM DO-COMPRESSION-ROUTINE.                              ELUCSENT
01846      PERFORM DO-UNSTRING.                                         ELUCSENT
01847      SET HOLD-IDX TO WS-Y-IDX.                                    ELUCSENT
01848      COMPUTE HOLD-IDX = HOLD-IDX + (TCAR-OUTPUT-FIELDS-USED -     ELUCSENT
01849          1).                                                      ELUCSENT
01850      PERFORM LOAD-WS-SENTENCE-TABLE                               ELUCSENT
01851          VARYING WS-Y-IDX FROM WS-Y-IDX BY 1                      ELUCSENT
01852                  UNTIL   WS-Y-IDX > HOLD-IDX                      ELUCSENT
01853                  OR      WS-Y-IDX > +15.                          ELUCSENT
01854      IF WS-Y-IDX < +15                                            ELUCSENT
01855                 AND (CSBP-COINS-UNWANTED-SLOT (CSBP-X-IDX))       ELUCSENT
01856                 AND (NO-TOPIC-SENT-PRINTED)                       ELUCSENT
01857         PERFORM DO-SEE-TOPIC-SENTENCE                             ELUCSENT
01858      ELSE                                                         ELUCSENT
01859         IF WS-Y-IDX > +15                                         ELUCSENT
01860             PERFORM DISPLAY-MORE-SENTENCE-AREA-NEE.               ELUCSENT
01861      PERFORM INITIALIZE-COMPRESSION-AREA.                         ELUCSENT
01862                                                                   ELUCSENT
01863                                                                   ELUCSENT
01864 ***********************************************************       ELUCSENT
01865 *                                                         *       ELUCSENT
01866 *     DO SEE TOPIC SENTENCE                               *       ELUCSENT
01867 *                                                         *       ELUCSENT
01868 ***********************************************************       ELUCSENT
01869  DO-SEE-TOPIC-SENTENCE.                                           ELUCSENT
01870      MOVE TOPIC-SENTENCE TO WS-SENTENCES (WS-Y-IDX).              ELUCSENT
01871      ADD +1 TO WS-NBR-SENTENCES.                                  ELUCSENT
01872      SET WS-Y-IDX UP BY 1.                                        ELUCSENT
01873      SET TOPIC-SENTENCE-PRINTED TO TRUE.                          ELUCSENT
01874 *                                                                 ELUCSENT
01875 ************************************************************      ELUCSENT
01876 *                                                          *      ELUCSENT
01877 *        DISPLAY MORE SENTENCE AREA NEEDED                 *      ELUCSENT
01878 *                                                          *      ELUCSENT
01879 ************************************************************      ELUCSENT
01880  DISPLAY-MORE-SENTENCE-AREA-NEE.                                  ELUCSENT
01881 *************************************************                 ELUCSENT
01882 ** THIS MESSAGE WILL OVER LAY THE LAST LINE TO **                 ELUCSENT
01883 ** INDICATE THAT MORE AREA IS NEEDED TO INSERT **                 ELUCSENT
01884 ** THE TEXT THAT HAS BEEN CONSTRUCTED. ==> REB **                 ELUCSENT
01885 *************************************************                 ELUCSENT
01886      MOVE WS-NEED-MORE-AREA-MSG  TO WS-SENTENCES (15).            ELUCSENT
01887                                                                   ELUCSENT
01888                                                                   ELUCSENT
01889 ************************************************************      ELUCSENT
01890 *                                                          *      ELUCSENT
01891 *        LINK TO CODES MANUAL                              *      ELUCSENT
01892 *                                                          *      ELUCSENT
01893 ************************************************************      ELUCSENT
01894  LINK-TO-CODES-MANUAL.                                            ELUCSENT
01895      EXEC CICS LINK                                               ELUCSENT
01896                PROGRAM ('ELUCMIF')                                ELUCSENT
01897                COMMAREA (DFHCOMMAREA)                             ELUCSENT
01898                END-EXEC.                                          ELUCSENT
01899      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUCSENT
01900      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
01901                      ADDRESS OF CMF-DESCR.                        ELUCSENT
01902                                                                   ELUCSENT
01903                                                                   ELUCSENT
01904 ************************************************************      ELUCSENT
01905 *                                                          *      ELUCSENT
01906 *        MOVE CODES MANUAL TEXT                            *      ELUCSENT
01907 *                                                          *      ELUCSENT
01908 ************************************************************      ELUCSENT
01909  MOVE-CODES-MANUAL-TEXT.                                          ELUCSENT
01910      ADD +1                          TO TCAR-FROM-SUB.            ELUCSENT
01911      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELUCSENT
01912                                      TO  TCAR-FROM-LINE           ELUCSENT
01913          (TCAR-FROM-SUB).                                         ELUCSENT
01914                                                                   ELUCSENT
01915                                                                   ELUCSENT
01916 ************************************************************      ELUCSENT
01917 *                                                          *      ELUCSENT
01918 *        DO COMPRESSION ROUTINE                            *      ELUCSENT
01919 *                                                          *      ELUCSENT
01920 ************************************************************      ELUCSENT
01921  DO-COMPRESSION-ROUTINE.                                          ELUCSENT
01922      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELUCSENT
01923                                                                   ELUCSENT
01924                                                                   ELUCSENT
01925 ************************************************************      ELUCSENT
01926 *                                                          *      ELUCSENT
01927 *        DO UNSTRING                                       *      ELUCSENT
01928 *                                                          *      ELUCSENT
01929 ************************************************************      ELUCSENT
01930  DO-UNSTRING.                                                     ELUCSENT
01931      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELUCSENT
01932      MOVE +46 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELUCSENT
01933      MOVE +46 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELUCSENT
01934      MOVE +46 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELUCSENT
01935      MOVE +46 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELUCSENT
01936      MOVE +46 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELUCSENT
01937      MOVE +46 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELUCSENT
01938      MOVE +46 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELUCSENT
01939      MOVE +46 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELUCSENT
01940      MOVE +46 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELUCSENT
01941      MOVE +46 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELUCSENT
01942      MOVE +46 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELUCSENT
01943      MOVE +46 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELUCSENT
01944      MOVE +46 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELUCSENT
01945      MOVE +46 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELUCSENT
01946      MOVE +46 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELUCSENT
01947      MOVE +46 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELUCSENT
01948      MOVE +46 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELUCSENT
01949      MOVE +46 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELUCSENT
01950      MOVE +46 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELUCSENT
01951      MOVE +46 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELUCSENT
01952      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELUCSENT
01953                                                                   ELUCSENT
01954                                                                   ELUCSENT
01955 ************************************************************      ELUCSENT
01956 *                                                          *      ELUCSENT
01957 *        LOAD WS SENTENCE TABLE                            *      ELUCSENT
01958 *                                                          *      ELUCSENT
01959 ************************************************************      ELUCSENT
01960  LOAD-WS-SENTENCE-TABLE.                                          ELUCSENT
01961      ADD +1                      TO TCAR-X.                       ELUCSENT
01962      MOVE TCAR-OPF-DATA (TCAR-X) TO WS-SENTENCES (WS-Y-IDX).      ELUCSENT
01963      ADD +1                      TO WS-NBR-SENTENCES.             ELUCSENT
01964                                                                   ELUCSENT
01965                                                                   ELUCSENT
01966 ************************************************************      ELUCSENT
01967 *                                                          *      ELUCSENT
01968 *        DO PPM TYPE 09 SENTENCE CONSTRUCTION              *      ELUCSENT
01969 *                                                          *      ELUCSENT
01970 ************************************************************      ELUCSENT
01971  DO-PPM-TYPE-09-SENTENCE-CONSTR.                                  ELUCSENT
01972      IF GBB-PAYMT-FACTOR-IND (GBB-INDEX) NOT = SPACES AND ZEROS   ELUCSENT
01973          AND                                                      ELUCSENT
01974                LOW-VALUES                                         ELUCSENT
01975          PERFORM PROCESS-PPM-TYPE-09-TEXT.                        ELUCSENT
01976                                                                   ELUCSENT
01977                                                                   ELUCSENT
01978 ************************************************************      ELUCSENT
01979 *                                                          *      ELUCSENT
01980 *        PROCESS PPM TYPE 09 TEXT                          *      ELUCSENT
01981 *                                                          *      ELUCSENT
01982 ************************************************************      ELUCSENT
01983  PROCESS-PPM-TYPE-09-TEXT.                                        ELUCSENT
01984      MOVE GBB-PAYMT-FACTOR-IND (GBB-INDEX) TO                     ELUCSENT
01985          WS-CURRENT-IND.                                          ELUCSENT
01986      IF WS-CURRENT-IND NOT = WS-PREV-IND                          ELUCSENT
01987          PERFORM DO-MEDICAL-PAYMENT-LITERAL-MOV                   ELUCSENT
01988      ELSE                                                         ELUCSENT
01989          PERFORM DO-THEN-LITERAL-MOVE.                            ELUCSENT
01990      PERFORM SET-UP-PPF-VALUE-MOVE.                               ELUCSENT
01991      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
01992      MOVE ' DOLLARS FOR '          TO TCAR-FROM-LINE              ELUCSENT
01993          (TCAR-FROM-SUB).                                         ELUCSENT
01994      PERFORM DO-END-OF-PPM-09-SENTENCE.                           ELUCSENT
01995      MOVE WS-CURRENT-IND           TO WS-PREV-IND.                ELUCSENT
01996                                                                   ELUCSENT
01997                                                                   ELUCSENT
01998 ************************************************************      ELUCSENT
01999 *                                                          *      ELUCSENT
02000 *        DO MEDICAL PAYMENT LITERAL MOVE                   *      ELUCSENT
02001 *                                                          *      ELUCSENT
02002 ************************************************************      ELUCSENT
02003  DO-MEDICAL-PAYMENT-LITERAL-MOV.                                  ELUCSENT
02004      IF WS-PREV-IND NOT = SPACES                                  ELUCSENT
02005          PERFORM COMPRESS-UNSTRING-AND-LOAD-WSX.                  ELUCSENT
02006      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
02007      MOVE WS-HOSP-MED-PYMT         TO TCAR-FROM-LINE              ELUCSENT
02008          (TCAR-FROM-SUB).                                         ELUCSENT
02009                                                                   ELUCSENT
02010                                                                   ELUCSENT
02011 ************************************************************      ELUCSENT
02012 *                                                          *      ELUCSENT
02013 *        DO THEN LITERAL MOVE                              *      ELUCSENT
02014 *                                                          *      ELUCSENT
02015 ************************************************************      ELUCSENT
02016  DO-THEN-LITERAL-MOVE.                                            ELUCSENT
02017      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
02018      MOVE ' THEN '                 TO TCAR-FROM-LINE              ELUCSENT
02019          (TCAR-FROM-SUB).                                         ELUCSENT
02020                                                                   ELUCSENT
02021                                                                   ELUCSENT
02022 ************************************************************      ELUCSENT
02023 *                                                          *      ELUCSENT
02024 *        SET UP PPF VALUE MOVE                             *      ELUCSENT
02025 *                                                          *      ELUCSENT
02026 ************************************************************      ELUCSENT
02027  SET-UP-PPF-VALUE-MOVE.                                           ELUCSENT
02028      MOVE GBB-PAYMT-FACTOR-VALUE (GBB-INDEX) TO WS-DOLLAR-AMT.    ELUCSENT
02029      ADD +1                        TO TCAR-FROM-SUB.              ELUCSENT
02030      MOVE WS-DOLLAR-AMT            TO TCAR-FROM-LINE              ELUCSENT
02031          (TCAR-FROM-SUB).                                         ELUCSENT
02032                                                                   ELUCSENT
02033                                                                   ELUCSENT
02034 ************************************************************      ELUCSENT
02035 *                                                          *      ELUCSENT
02036 *        DO END OF PPM 09 SENTENCE                         *      ELUCSENT
02037 *                                                          *      ELUCSENT
02038 ************************************************************      ELUCSENT
02039  DO-END-OF-PPM-09-SENTENCE.                                       ELUCSENT
02040      INITIALIZE WS-PPF-VALUE-LIMIT.                               ELUCSENT
02041      MOVE GBB-PAYMT-FACTOR-LIMIT (GBB-INDEX) TO                   ELUCSENT
02042          WS-PPF-VALUE-LIMIT.                                      ELUCSENT
02043      IF PPF-UNLIMITED-AMT                                         ELUCSENT
02044          PERFORM DO-REMAINING-LITERAL-MOVE                        ELUCSENT
02045      ELSE                                                         ELUCSENT
02046          PERFORM DO-FIRST-NEXT-LITERAL-MOVE.                      ELUCSENT
02047      IF GBB-PAYMT-FACTOR-IND (GBB-INDEX) = '01'                   ELUCSENT
02048          PERFORM DO-DAYS-LITERAL-MOVE                             ELUCSENT
02049      ELSE                                                         ELUCSENT
02050          PERFORM DO-VISITS-LITERAL-MOVE.                          ELUCSENT
02051                                                                   ELUCSENT
02052                                                                   ELUCSENT
02053 ************************************************************      ELUCSENT
02054 *                                                          *      ELUCSENT
02055 *        DO REMAINING LITERAL MOVE                         *      ELUCSENT
02056 *                                                          *      ELUCSENT
02057 ************************************************************      ELUCSENT
02058  DO-REMAINING-LITERAL-MOVE.                                       ELUCSENT
02059      ADD +1                      TO TCAR-FROM-SUB.                ELUCSENT
02060      MOVE ' REMAINING '          TO TCAR-FROM-LINE                ELUCSENT
02061          (TCAR-FROM-SUB).                                         ELUCSENT
02062                                                                   ELUCSENT
02063                                                                   ELUCSENT
02064 ************************************************************      ELUCSENT
02065 *                                                          *      ELUCSENT
02066 *        DO FIRST NEXT LITERAL MOVE                        *      ELUCSENT
02067 *                                                          *      ELUCSENT
02068 ************************************************************      ELUCSENT
02069  DO-FIRST-NEXT-LITERAL-MOVE.                                      ELUCSENT
02070      IF WS-CURRENT-IND NOT = WS-PREV-IND                          ELUCSENT
02071          PERFORM MOVE-FIRST-LITERAL                               ELUCSENT
02072      ELSE                                                         ELUCSENT
02073          PERFORM MOVE-NEXT-LITERAL.                               ELUCSENT
02074      MOVE GBB-PAYMT-FACTOR-LIMIT (GBB-INDEX) TO                   ELUCSENT
02075          WS-PPF-EDITED-LIMIT.                                     ELUCSENT
02076      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
02077      MOVE WS-PPF-EDITED-LIMIT     TO TCAR-FROM-LINE               ELUCSENT
02078          (TCAR-FROM-SUB).                                         ELUCSENT
02079                                                                   ELUCSENT
02080                                                                   ELUCSENT
02081 ************************************************************      ELUCSENT
02082 *                                                          *      ELUCSENT
02083 *        MOVE FIRST LITERAL                                *      ELUCSENT
02084 *                                                          *      ELUCSENT
02085 ************************************************************      ELUCSENT
02086  MOVE-FIRST-LITERAL.                                              ELUCSENT
02087      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
02088      MOVE ' FIRST '               TO TCAR-FROM-LINE               ELUCSENT
02089          (TCAR-FROM-SUB).                                         ELUCSENT
02090                                                                   ELUCSENT
02091                                                                   ELUCSENT
02092 ************************************************************      ELUCSENT
02093 *                                                          *      ELUCSENT
02094 *        MOVE NEXT LITERAL                                 *      ELUCSENT
02095 *                                                          *      ELUCSENT
02096 ************************************************************      ELUCSENT
02097  MOVE-NEXT-LITERAL.                                               ELUCSENT
02098      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
02099      MOVE ' NEXT '                TO TCAR-FROM-LINE               ELUCSENT
02100          (TCAR-FROM-SUB).                                         ELUCSENT
02101                                                                   ELUCSENT
02102                                                                   ELUCSENT
02103 ************************************************************      ELUCSENT
02104 *                                                          *      ELUCSENT
02105 *        DO DAYS LITERAL MOVE                              *      ELUCSENT
02106 *                                                          *      ELUCSENT
02107 ************************************************************      ELUCSENT
02108  DO-DAYS-LITERAL-MOVE.                                            ELUCSENT
02109      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
02110      MOVE ' DAYS '                TO TCAR-FROM-LINE               ELUCSENT
02111          (TCAR-FROM-SUB).                                         ELUCSENT
02112                                                                   ELUCSENT
02113                                                                   ELUCSENT
02114 ************************************************************      ELUCSENT
02115 *                                                          *      ELUCSENT
02116 *        DO VISITS LITERAL MOVE                            *      ELUCSENT
02117 *                                                          *      ELUCSENT
02118 ************************************************************      ELUCSENT
02119  DO-VISITS-LITERAL-MOVE.                                          ELUCSENT
02120      ADD +1                       TO TCAR-FROM-SUB.               ELUCSENT
02121      MOVE ' VISITS '              TO TCAR-FROM-LINE               ELUCSENT
02122          (TCAR-FROM-SUB).                                         ELUCSENT
02123                                                                   ELUCSENT
02124                                                                   ELUCSENT
02125 ************************************************************      ELUCSENT
02126 *                                                          *      ELUCSENT
02127 *        READ PPF RECORD                                   *      ELUCSENT
02128 *                                                          *      ELUCSENT
02129 ************************************************************      ELUCSENT
02130  READ-PPF-RECORD.                                                 ELUCSENT
02131      SET CIA-GCTABULR-DDN TO TRUE.                                ELUCSENT
02132      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSENT
02133           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELUCSENT
02134      SET IOP-RD                              TO TRUE.             ELUCSENT
02135      SET IOP-FCQ-NONE                        TO TRUE.             ELUCSENT
02136      SET IOP-STG-MODE-MOVE                   TO TRUE.             ELUCSENT
02137      MOVE PC-PPF                             TO                   ELUCSENT
02138          KWA-PROVISION-ID.                                        ELUCSENT
02139      MOVE CSBP-BP-PPF-SLOT (CSBP-X-IDX)      TO                   ELUCSENT
02140          KWA-PROVISION-SLOT-NO.                                   ELUCSENT
02141      MOVE KWA-GCTABULR-KEY                   TO IOP-FILE-KEY.     ELUCSENT
02142      PERFORM LINK-TO-I-O-PGM.                                     ELUCSENT
02143      IF IOP-RC-OK                                                 ELUCSENT
02144          PERFORM ESTABLISH-ADDRESSABILITY-OF-PP                   ELUCSENT
02145      ELSE                                                         ELUCSENT
02146          PERFORM SIGNAL-CONTRACT-CODING-ERROR.                    ELUCSENT
02147                                                                   ELUCSENT
02148                                                                   ELUCSENT
02149 ************************************************************      ELUCSENT
02150 *                                                          *      ELUCSENT
02151 *        LINK TO I O PGM                                   *      ELUCSENT
02152 *                                                          *      ELUCSENT
02153 ************************************************************      ELUCSENT
02154  LINK-TO-I-O-PGM.                                                 ELUCSENT
02155      EXEC CICS LINK                                               ELUCSENT
02156                PROGRAM ('ELUIOPGM')                               ELUCSENT
02157                COMMAREA (DFHCOMMAREA)                             ELUCSENT
02158         END-EXEC.                                                 ELUCSENT
02159                                                                   ELUCSENT
02160                                                                   ELUCSENT
02161 ************************************************************      ELUCSENT
02162 *                                                          *      ELUCSENT
02163 *        ESTABLISH ADDRESSABILITY OF PPF RECORD AREA       *      ELUCSENT
02164 *                                                          *      ELUCSENT
02165 ************************************************************      ELUCSENT
02166  ESTABLISH-ADDRESSABILITY-OF-PP.                                  ELUCSENT
02167      SET ADDRESS OF PAYMENT-FACTOR-RECORD TO                      ELUCSENT
02168          IOP-REC-PTR.                                             ELUCSENT
02169      SET IOP-REC-PTR                      TO NULLS.               ELUCSENT
02170                                                                   ELUCSENT
02171                                                                   ELUCSENT
02172 ************************************************************      ELUCSENT
02173 *                                                          *      ELUCSENT
02174 *        SIGNAL CONTRACT CODING ERROR                      *      ELUCSENT
02175 *                                                          *      ELUCSENT
02176 ************************************************************      ELUCSENT
02177  SIGNAL-CONTRACT-CODING-ERROR.                                    ELUCSENT
02178      SET CIA-AB-NOTFND-GCBENPRV TO TRUE.                          ELUCSENT
02179      EXEC CICS ABEND                                              ELUCSENT
02180                ABCODE (CIA-ABCODE)                                ELUCSENT
02181         END-EXEC.                                                 ELUCSENT
02182                                                                   ELUCSENT
02183 ************************************************************      ELUCSENT
02184 *                                                          *      ELUCSENT
02185 *  DO TRANSLATION OF VALUE QUALIFIER                       *      ELUCSENT
02186 *                                                          *      ELUCSENT
02187 ************************************************************      ELUCSENT
02188  DO-TRANS-VALUE-QUALIFIER.                                        ELUCSENT
02189      MOVE PC-ACL                   TO CMF-RECORD-PREFIX.          ELUCSENT
02190      MOVE 'COINS-VALUE-QUALIFIER'   TO CMF-ELEMENT-SYSTEM-NAME.   ELUCSENT
02191      MOVE CSBP-COINS-VALUE-QUALIFIER (CSBP-X-IDX)                 ELUCSENT
02192                                    TO CMF-CODE-VALUE.             ELUCSENT
02193      PERFORM GET-TRANSLATION.                                     ELUCSENT
02194                                                                   ELUCSENT
